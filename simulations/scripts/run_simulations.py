"""Run the reproducible three-simulation workflow.

The workflow is independent of the caller's working directory.  Results and
figures are generated in a staging tree and promoted only after every active
artifact, source hash, and manifest entry has been produced.  Documents are
deliberately not inputs to this workflow.
"""

from __future__ import annotations

import hashlib
import importlib.metadata
import json
import math
import os
import platform
import random
import shutil
import subprocess
import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent


def _find_repo_root(start: Path) -> Path:
    for candidate in (start, *start.parents):
        if (candidate / "DESCRIPTION").is_file() and (
            candidate / "inst" / "python"
        ).is_dir():
            return candidate
    raise RuntimeError(f"Could not locate repository root from {start}")


REPO = _find_repo_root(SCRIPT_DIR)
SIMULATIONS_DIR = REPO / "simulations"
RESULTS_DIR = SIMULATIONS_DIR / "results"
FIGURES_DIR = SIMULATIONS_DIR / "figures"
MANIFEST_PATH = SIMULATIONS_DIR / "manifest.json"
sys.path.insert(0, str(REPO / "inst" / "python"))

from dyadic.causal import CausalWrapper  # noqa: E402
from dyadic.engine import DyadicEngine  # noqa: E402
from dyadic.identification import IdentificationWrapper  # noqa: E402
from dyadic.profiles import CausalProfileBuilder  # noqa: E402
from registry.builder import ComponentRegistryBuilder  # noqa: E402
from simulation.suite import SimulationSuite  # noqa: E402
from state.completions import CompletionIndex  # noqa: E402
from state.expander import ModelStateExpander  # noqa: E402
from state.tensor import StateTensor  # noqa: E402

SEED = 42

# ── Viridis-based palette and white style (Phase 5) ───────────────────────────
_VIRIDIS = matplotlib.colormaps["viridis"]
PALETTE = {
    "similarity_rate": _VIRIDIS(0.10),  # dark purple/blue
    "mas_compatible": _VIRIDIS(0.52),  # teal/green
    "identified_compatible": _VIRIDIS(0.83),  # yellow/green (needs dark edge)
    "baseline": "#9E9E9E",  # neutral gray
    "post": "#5F6368",  # neutral gray
    "zero": "#6E6E6E",
    "noise": "#B4AEA4",
    "cluster_mainstream": _VIRIDIS(0.18),
    "cluster_ghost": _VIRIDIS(0.58),
    "cluster_fragmented": _VIRIDIS(0.88),
    "tick": "#3D3D3D",
}
IDENTIFIED_EDGE = "#2B2B2B"  # dark edge for the yellow Viridis value


def _style() -> None:
    plt.rcParams.update(
        {
            "font.family": "serif",
            "font.serif": ["DejaVu Serif"],
            "axes.spines.top": False,
            "axes.spines.right": False,
            "axes.labelcolor": "#2B2B2B",
            "text.color": "#2B2B2B",
            "xtick.color": "#3D3D3D",
            "ytick.color": "#3D3D3D",
            "figure.facecolor": "white",
            "axes.facecolor": "white",
            "savefig.facecolor": "white",
            "savefig.transparent": False,
            "axes.titleweight": "bold",
            "axes.titlesize": 12,
        }
    )


def _dump(path: Path, value) -> None:
    def clean(item):
        if isinstance(item, dict):
            return {str(key): clean(val) for key, val in item.items()}
        if isinstance(item, (list, tuple)):
            return [clean(val) for val in item]
        if isinstance(item, (np.integer, np.floating)):
            item = item.item()
        if isinstance(item, float) and not np.isfinite(item):
            return None
        return item

    path.write_text(
        json.dumps(clean(value), indent=2, allow_nan=False), encoding="utf-8"
    )


def _dataframe_records(frame: pd.DataFrame) -> list[dict]:
    """Convert registry frames to API records without pandas NaN sentinels."""
    records = frame.to_dict(orient="records")
    for record in records:
        for key, value in record.items():
            try:
                missing = bool(pd.isna(value))
            except (TypeError, ValueError):
                missing = False
            if missing:
                record[key] = None
    return records


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    digest.update(path.read_bytes())
    return digest.hexdigest()


# ── shared design helpers ────────────────────────────────────────────────────
def _edge_id(registry, source: str, target: str, direction: str = "->") -> str:
    rows = registry.data[
        (registry.data["type"] == "edge")
        & (registry.data["source"] == source)
        & (registry.data["target"] == target)
        & (registry.data["direction"] == direction)
    ]
    if len(rows) != 1:
        raise RuntimeError(f"Expected one component for {source} {direction} {target}")
    return str(rows.iloc[0]["comp_id"])


def _seed_claims(registry, timing: dict[str, int], edge_status: dict[str, str]):
    claims = []
    for _, component in registry.data.iterrows():
        if component["type"] == "node":
            status = "present"
            component_timing = timing[str(component["source"])]
        else:
            status = edge_status.get(str(component["comp_id"]), "non-causal")
            component_timing = None
        claims.append(
            {
                "model_id": "seed_theory",
                "comp_id": str(component["comp_id"]),
                "status": status,
                "timing": component_timing,
            }
        )
    return claims


def _expand_seeded_universe(
    registry,
    timing,
    edge_status,
    edge_statuses=("causal", "non-causal"),
):
    return ModelStateExpander.expand(
        registry,
        mode="exhaustive",
        seed_claims=_seed_claims(registry, timing, edge_status),
        node_timing=timing,
        max_models=10000,
        edge_statuses=list(edge_statuses),
        node_policy="all-present",
    )


# ── Simulation 1: Consensus Illusion (1A and 1B, dual metrics) ────────────────
NEIGHBORHOODS = [
    ("Focused", 1),
    ("Moderate", 2),
    ("Broad", 3),
    ("Full", 6),
]
EXPECTED_NEIGHBORHOODS = {
    "Focused": {"blocks": 7, "models": 21, "dyads": 420},
    "Moderate": {"blocks": 22, "models": 66, "dyads": 4290},
    "Broad": {"blocks": 42, "models": 126, "dyads": 15750},
    "Full": {"blocks": 64, "models": 192, "dyads": 36672},
}
SAMPLE_BLOCKS = 20
REPLICATES = 100
RELIABILITY_GATE_REQUIRED = 75  # plan predeclared gate (out of 100)
RELIABILITY_JOINT_GATE_REQUIRED = 70

# This is historical provenance only.  It is deliberately embedded in the
# workflow rather than discovered from whatever happens to be in the output
# directory, and it never participates in an acceptance decision.
CONSENSUS_BASELINE_PROVENANCE = {
    "status": "historical_provenance_only",
    "use_for_acceptance": False,
    "captured_at": "pre-0.2.0 rerun",
    "definition": (
        "Historical identified_compatible values were computed before equal "
        "conditioning-node-set equality became part of the metric."
    ),
    "historical_primary_rates": {
        "1A mas_compatibility_rate": [0.028571, 0.016783, 0.01219, 0.010471],
        "1B identified_compatibility_rate": [0.1, 0.107692, 0.109333, 0.109948],
    },
}


def _build_consensus_multiverse(metric: str):
    suite = SimulationSuite(SEED)
    registry, fixed_edges, variable_edges, edge_ids, node_timing, design = (
        suite._build_consensus_illusion_design(metric)
    )
    states = suite._generate_consensus_illusion_states(
        registry, fixed_edges, variable_edges, edge_ids, node_timing
    )
    completion_blocks = [
        [
            f"R{2 * context + 1:04d}",
            f"R{2 * context + 2:04d}",
            f"P{context + 1:04d}",
        ]
        for context in range(64)
    ]
    block_distances = [
        {"model_ids": block, "distance": context.bit_count()}
        for context, block in enumerate(completion_blocks)
    ]
    seed_status = {
        edge_ids[edge]: "causal" if edge in fixed_edges else "non-causal"
        for edge in fixed_edges + variable_edges
    }
    return (
        registry,
        states,
        completion_blocks,
        block_distances,
        {
            "design": design,
            "metric": metric,
            "seed_model_id": "R0001",
            "seed_edge_status": seed_status,
            "fixed_edges": fixed_edges,
            "focal_edge": variable_edges[0],
            "context_edges": variable_edges[1:],
            "node_timing": node_timing,
            "directed_only_registry": True,
            "resolved_model_count": 128,
            "partial_theory_count": 64,
            "augmented_multiverse_count": 192,
            "completion_block_count": 64,
        },
    )


def _three_metric_subset(model_ids, dyads):
    selected = set(model_ids)
    selected_dyads = [
        dyad
        for dyad in dyads
        if dyad["ego_id"] in selected and dyad["alter_id"] in selected
    ]
    for metric in ("mas_compatible", "identified_compatible"):
        if any(dyad[metric] is None for dyad in selected_dyads):
            raise RuntimeError(
                f"Consensus subset is not completion-closed for {metric}"
            )
    similarity = float(np.mean([dyad["similarity_rate"] for dyad in selected_dyads]))
    mas = float(np.mean([float(dyad["mas_compatible"]) for dyad in selected_dyads]))
    identified = float(
        np.mean([float(dyad["identified_compatible"]) for dyad in selected_dyads])
    )
    return {
        "n_models": len(selected),
        "n_dyads": len(selected_dyads),
        "mean_similarity_rate": round(similarity, 6),
        "mas_compatibility_rate": round(mas, 6),
        "identified_compatibility_rate": round(identified, 6),
        "similarity_minus_mas": round(similarity - mas, 6),
        "similarity_minus_identified": round(similarity - identified, 6),
    }


def _consensus_neighborhood_model_ids(block_distances, max_departures):
    return sorted(
        model_id
        for block in block_distances
        if block["distance"] <= max_departures
        for model_id in block["model_ids"]
    )


def _sample_completion_blocks(completion_blocks, n_blocks, seed):
    selected_blocks = random.Random(seed).sample(completion_blocks, n_blocks)
    return sorted(model_id for block in selected_blocks for model_id in block)


def _assert_consensus_invariants(
    label, registry, states, model_ids, block_distances, dyads, profiles
):
    index = CompletionIndex(
        StateTensor.from_records(registry, states, model_ids=model_ids),
        registry,
    )
    resolved = sum(1 for m in model_ids if index.is_resolved(m))
    partial = len(model_ids) - resolved
    if (resolved, partial, len(model_ids)) != (128, 64, 192):
        raise RuntimeError(
            f"{label} model counts wrong: {resolved}/{partial}/{len(model_ids)}"
        )
    if len(block_distances) != 64:
        raise RuntimeError(f"{label} completion block count is not 64")

    # Verify declared presence and the robust completion-level node sets used by
    # the actual causal profiles, including partial theories.
    declared_sets = set()
    node_sets = {profile.identification_nodes for profile in profiles.values()}
    engine = DyadicEngine()
    state = StateTensor.from_records(registry, states, model_ids=model_ids)
    for model_id in model_ids:
        spec = engine._dag_spec_for_model(
            model_id, state, registry, exposure="X1", outcome="Y"
        )
        declared_sets.add(frozenset(spec["declared_nodes"]))
    if len(declared_sets) != 1:
        raise RuntimeError(
            f"{label} declared node sets are not identical within the design"
        )
    if len(node_sets) != 1 or None in node_sets:
        raise RuntimeError(
            f"{label} normalized identification_node sets are not identical or "
            "some are unavailable"
        )
    if label == "1B":
        expected_nodes = frozenset({"X2", "X3", "X4", "X5", "X6"})
        if node_sets != {expected_nodes}:
            raise RuntimeError(
                f"{label} complete-conditioning node set is wrong: {node_sets}"
            )
        mas_sets = {
            frozenset(frozenset(item) for item in (profile.mas or []))
            for profile in profiles.values()
        }
        if mas_sets != {frozenset({frozenset({"X2"})})}:
            raise RuntimeError(f"{label} MAS sets are not shared {{X2}}: {mas_sets}")
        expected_identified = {
            "R0001": True,
            "R0002": False,
            "R0003": True,
            "P0001": False,
        }
        observed_identified = {
            model_id: profiles[model_id].identified for model_id in expected_identified
        }
        if observed_identified != expected_identified:
            raise RuntimeError(
                f"{label} focal/context identification outcomes are wrong: "
                f"{observed_identified}"
            )
        focal_off_on = CausalProfileBuilder.compare(
            profiles["R0001"], profiles["R0002"]
        )
        context_only = CausalProfileBuilder.compare(
            profiles["R0001"], profiles["R0003"]
        )
        if focal_off_on["identified_compatible"] is not False:
            raise RuntimeError(f"{label} focal on/off identification did not fail")
        if context_only["identified_compatible"] is not True:
            raise RuntimeError(f"{label} context-only identification changed")
        if focal_off_on["mas_compatible"] is not True:
            raise RuntimeError(f"{label} focal on/off MAS compatibility changed")
    node_filtered = [
        dyad
        for dyad in dyads
        if dyad.get("identified_ego") is True
        and dyad.get("identified_alter") is True
        and dyad.get("identified_compatible") is not True
    ]
    if node_filtered:
        raise RuntimeError(
            f"{label} node-set equality filtered {len(node_filtered)} jointly "
            "identified dyads"
        )

    for name, max_d in NEIGHBORHOODS:
        ids = _consensus_neighborhood_model_ids(block_distances, max_d)
        exp = EXPECTED_NEIGHBORHOODS[name]
        if len(ids) != exp["models"]:
            raise RuntimeError(
                f"{label} {name} model count {len(ids)} != {exp['models']}"
            )
        subset = _three_metric_subset(ids, dyads)
        if subset["n_dyads"] != exp["dyads"]:
            raise RuntimeError(
                f"{label} {name} dyad count {subset['n_dyads']} != {exp['dyads']}"
            )
        for metric in ("mas_compatible", "identified_compatible"):
            bad = [d for d in dyads if d.get(metric) is None]
            if bad:
                raise RuntimeError(f"{label} has unavailable {metric} dyads")


def _run_consensus_design(design_metric):
    registry, states, completion_blocks, block_distances, generation = (
        _build_consensus_multiverse(design_metric)
    )
    label = "1A" if design_metric == "mas_compatible" else "1B"
    model_ids = sorted({str(record["model_id"]) for record in states})
    state = StateTensor.from_records(registry, states, model_ids=model_ids)

    # Build all causal profiles once with both wrappers so the three rates share
    # exactly the same model population, completions, and dyad denominator.
    engine = DyadicEngine()
    causal_wrapper = CausalWrapper()
    identification_wrapper = IdentificationWrapper()
    profiles = engine._build_causal_profiles(
        state,
        registry,
        mode="full",
        causal_wrapper=causal_wrapper,
        identification_wrapper=identification_wrapper,
        exposure="X1",
        outcome="Y",
    )
    dyads = [
        engine.compare(
            ego,
            alter,
            state,
            registry,
            mode="full",
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
            exposure="X1",
            outcome="Y",
            _causal_profiles=profiles,
        )
        for ego in model_ids
        for alter in model_ids
        if ego != alter
    ]
    _assert_consensus_invariants(
        label, registry, states, model_ids, block_distances, dyads, profiles
    )

    census = _three_metric_subset(model_ids, dyads)

    summary_rows = []
    for name, max_departures in NEIGHBORHOODS:
        ids = _consensus_neighborhood_model_ids(block_distances, max_departures)
        metrics = _three_metric_subset(ids, dyads)
        summary_rows.append(
            {
                "design_label": label,
                "design": generation["design"],
                "neighborhood": name,
                "max_context_departures": max_departures,
                "completion_blocks": len(ids) // 3,
                "block_count": len(ids) // 3,
                "models": len(ids),
                "ordered_dyads": metrics["n_dyads"],
                **metrics,
            }
        )

    displayed_sample_ids = _sample_completion_blocks(
        completion_blocks, SAMPLE_BLOCKS, SEED
    )
    displayed_sample = _three_metric_subset(displayed_sample_ids, dyads)

    reliability_rows = []
    for replicate in range(1, REPLICATES + 1):
        sample_seed = 1000 + replicate
        sampled_ids = _sample_completion_blocks(
            completion_blocks, SAMPLE_BLOCKS, sample_seed
        )
        metrics = _three_metric_subset(sampled_ids, dyads)
        reliability_rows.append(
            {
                "design_label": label,
                "metric": design_metric,
                "replicate": replicate,
                "sample_seed": sample_seed,
                **metrics,
                "similarity_error": round(
                    metrics["mean_similarity_rate"] - census["mean_similarity_rate"], 6
                ),
                "mas_error": round(
                    metrics["mas_compatibility_rate"]
                    - census["mas_compatibility_rate"],
                    6,
                ),
                "identified_error": round(
                    metrics["identified_compatibility_rate"]
                    - census["identified_compatibility_rate"],
                    6,
                ),
                "similarity_minus_mas_error": round(
                    metrics["similarity_minus_mas"] - census["similarity_minus_mas"], 6
                ),
                "similarity_minus_identified_error": round(
                    metrics["similarity_minus_identified"]
                    - census["similarity_minus_identified"],
                    6,
                ),
            }
        )

    sampling_summary_rows = [
        {
            "design_label": label,
            "analysis": "full census",
            "sample_blocks": len(completion_blocks),
            **census,
        },
        {
            "design_label": label,
            "analysis": "sample (seed 42)",
            "sample_blocks": SAMPLE_BLOCKS,
            **displayed_sample,
        },
    ]

    for metric in ("mas_compatibility_rate", "identified_compatibility_rate"):
        range_ok = all(0.0 <= row[metric] <= 1.0 for row in summary_rows)
        if not range_ok:
            raise RuntimeError(f"{label} metric {metric} out of [0,1]")

    result = {
        "scenario": "consensus_illusion",
        "design_label": label,
        "compatibility_metric": design_metric,
        "construction": generation,
        "completion_blocks": completion_blocks,
        "completion_block_distances": block_distances,
        "neighborhood_results": summary_rows,
        "displayed_sample_model_ids": displayed_sample_ids,
        "census": census,
        "displayed_sample": displayed_sample,
        "baseline_comparison": {
            "provenance_artifact": "results/simulation_1_consensus_baseline.json",
            "used_for_acceptance": False,
            "reason": "Historical baseline is retained as explicit provenance only.",
        },
        "sampling_design": {
            "sample_blocks": SAMPLE_BLOCKS,
            "models_per_block": 3,
            "models_per_sample": SAMPLE_BLOCKS * 3,
            "repeated_samples": REPLICATES,
            "without_replacement": True,
            "completion_closed": True,
        },
        "sampling_reliability": reliability_rows,
    }
    return (
        result,
        _dataframe_records(registry.data),
        states,
        pd.DataFrame(summary_rows),
        pd.DataFrame(sampling_summary_rows),
        pd.DataFrame(reliability_rows),
    )


def _consensus_combined_dataframe(summary_a, summary_b):
    a = summary_a.set_index("neighborhood")
    b = summary_b.set_index("neighborhood")
    if not list(a.index) == list(b.index):
        raise RuntimeError("1A and 1B neighborhood ordering differs")
    if (a["models"] != b["models"]).any():
        raise RuntimeError("1A and 1B model denominators differ; refuse one count")
    if (a["ordered_dyads"] != b["ordered_dyads"]).any():
        raise RuntimeError("1A and 1B dyad denominators differ")
    rows = []
    for neighborhood in a.index:
        models = int(a.loc[neighborhood, "models"])
        rows.append(
            {
                "neighborhood": neighborhood,
                "models": models,
                "1A_mean_similarity": a.loc[neighborhood, "mean_similarity_rate"],
                "1A_mas_compatibility": a.loc[neighborhood, "mas_compatibility_rate"],
                "1A_identified_compatibility": a.loc[
                    neighborhood, "identified_compatibility_rate"
                ],
                "1B_mean_similarity": b.loc[neighborhood, "mean_similarity_rate"],
                "1B_mas_compatibility": b.loc[neighborhood, "mas_compatibility_rate"],
                "1B_identified_compatibility": b.loc[
                    neighborhood, "identified_compatibility_rate"
                ],
            }
        )
    return pd.DataFrame(rows)


# ── Phase 3: Uncertainty Crux (similarity + MAS only) ─────────────────────────
def _build_crux_multiverse():
    timing = {
        "X2": 1,
        "X1": 2,
        "X4": 3,
        "X3": 4,
        "Y": 5,
    }
    nodes = [
        {
            "name": name,
            "timing": value,
            "description": {
                "X1": "Exposure",
                "X2": "Observed baseline variable",
                "X3": "Later mediator",
                "X4": "Early post-exposure mechanism",
                "Y": "Outcome",
            }[name],
            "observed": True,
        }
        for name, value in timing.items()
    ]
    all_forward_edges = [
        (source, target)
        for source in timing
        for target in timing
        if timing[source] < timing[target]
    ]
    uncertain_edges = [
        ("X2", "X1"),
        ("X2", "X3"),
        ("X1", "X4"),
        ("X4", "X3"),
    ]
    fixed_edges = [edge for edge in all_forward_edges if edge not in uncertain_edges]
    allowed = [(*edge, "->") for edge in all_forward_edges]
    registry = ComponentRegistryBuilder.from_nodes(
        nodes,
        respect_timing=True,
        include_bidirectional=False,
        constraints=[
            {
                "source": source,
                "target": target,
                "direction": direction,
                "rule": "allow",
            }
            for source, target, direction in allowed
        ],
        exposure="X1",
        outcome="Y",
    )
    edges = {
        (source, target): _edge_id(registry, source, target)
        for source, target, _ in allowed
    }
    registry_edges = {
        (str(row["source"]), str(row["target"]))
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows()
    }
    if registry_edges != set(all_forward_edges):
        raise RuntimeError("Crux registry does not contain every forward edge")
    fixed_ids = {edges[edge] for edge in fixed_edges}
    registry.data.loc[registry.data["comp_id"].isin(fixed_ids), "fixed_status"] = (
        "causal"
    )

    seed_status = {edges[edge]: "causal" for edge in fixed_edges}
    seed_status.update({edges[edge]: "unknown" for edge in uncertain_edges})
    universe = _expand_seeded_universe(
        registry,
        timing,
        seed_status,
        edge_statuses=("causal", "unknown", "non-causal"),
    )

    by_model = {}
    for record in universe:
        by_model.setdefault(str(record["model_id"]), []).append(record)
    selected_ids = sorted(by_model)
    states = universe
    if len(selected_ids) != 3 ** len(uncertain_edges):
        raise RuntimeError(
            "Crux expansion did not produce the complete four-edge multiverse"
        )

    state = StateTensor.from_records(registry, states, model_ids=selected_ids)
    completion_index = CompletionIndex(state, registry)
    if any(
        not completion_index.diagnostics(model_id).completion_coverage_complete
        for model_id in selected_ids
    ):
        raise RuntimeError("Selected crux multiverse is not completion-closed")
    signatures = {
        completion_index.semantic_signature(model_id) for model_id in selected_ids
    }
    if len(signatures) != len(selected_ids):
        raise RuntimeError(
            "Selected crux multiverse contains duplicate semantic states"
        )

    edge_ids = {edges[edge] for edge in fixed_edges + uncertain_edges}
    resolved_models = sum(
        all(
            row["status"] != "unknown"
            for row in by_model[model_id]
            if row["comp_id"] in edge_ids
        )
        for model_id in selected_ids
    )
    if len(selected_ids) != 81:
        raise RuntimeError(f"Crux multiverse is not 81 models: {len(selected_ids)}")

    return (
        registry,
        states,
        {
            "seed_model_id": "seed_theory",
            "seed_edge_status": seed_status,
            "node_timing": timing,
            "all_forward_edges": all_forward_edges,
            "fixed_edges": fixed_edges,
            "uncertain_edges": uncertain_edges,
            "possible_forward_edges": len(all_forward_edges),
            "excluded_forward_edges": [],
            "expanded_universe_models": len(by_model),
            "candidate_space_models": 3 ** len(uncertain_edges),
            "selected_models": len(selected_ids),
            "resolved_models": resolved_models,
            "partial_models": len(selected_ids) - resolved_models,
            "unique_semantic_states": len(signatures),
            "completion_closed": True,
            "crux_mode": "marginal",
            "models_retained": len(selected_ids),
            "dyads_retained": len(selected_ids) * (len(selected_ids) - 1),
            "baseline_ordered_dyads": len(selected_ids) * (len(selected_ids) - 1),
        },
    )


def _run_crux():
    registry, states, generation = _build_crux_multiverse()
    n_models = len({record["model_id"] for record in states})
    if generation["baseline_ordered_dyads"] != 6480:
        raise RuntimeError("Crux baseline dyad count is not 6,480")
    runs = []
    rows = []
    metrics = ("similarity_rate", "mas_compatible")
    registry_records = _dataframe_records(registry.data)
    for record in registry_records:
        if isinstance(record.get("fixed_status"), float) and math.isnan(
            record["fixed_status"]
        ):
            record["fixed_status"] = None
    for metric in metrics:
        causal = metric == "mas_compatible"
        result = SimulationSuite(SEED).run_scenario(
            "crux_of_certainty",
            registry_data=registry_records,
            state_data=states,
            compatibility_metric=metric,
            exposure="X1" if causal else None,
            outcome="Y" if causal else None,
            crux_mode="marginal",
            enforce_thresholds=False,
            include_plot_data=False,
        )
        runs.append({"generation": generation, "metric": metric, "run": result})
        summary = result["results"]
        rankings = result["artifacts"]["rankings"]
        if len(rankings) != 4:
            raise RuntimeError(f"Expected four {metric} crux rankings")
        # Marginal crux preserves every model and every ordered dyad.
        expected_retained = (81, 6480)
        if (
            summary["models_retained"],
            summary["dyads_retained"],
        ) != expected_retained:
            raise RuntimeError(f"Unexpected marginal {metric} retained size")
        if summary["crux_mode"] != "marginal":
            raise RuntimeError(
                f"Expected marginal crux mode, got {summary['crux_mode']}"
            )
        top = rankings[0]
        if metric == "similarity_rate":
            structural_values = {ranking["delta_u"] for ranking in rankings}
            if len(structural_values) != 1 or not 0.005 <= top["delta_u"] <= 0.006:
                raise RuntimeError("Structural marginal result lost exchangeability")
        elif metric == "mas_compatible":
            if (
                (top["source"], top["target"]) != ("X2", "X1")
                or top["delta_u"] < 0.33
                or top["delta_u_causal"] != top["delta_u_non_causal"]
            ):
                raise RuntimeError("MAS crux no longer has the expected leverage")
        for ranking in rankings:
            rows.append(
                {
                    "n_models": n_models,
                    "resolved_models": generation["resolved_models"],
                    "partial_models": generation["partial_models"],
                    "metric": metric,
                    "rank": ranking["rank"],
                    "component_id": ranking["component_id"],
                    "claim": f'{ranking["source"]} -> {ranking["target"]}',
                    "delta_u": ranking["delta_u"],
                    "delta_u_causal": ranking["delta_u_causal"],
                    "delta_u_non_causal": ranking["delta_u_non_causal"],
                    "best_resolution": ranking["best_resolution"],
                    "models_changed_causal": ranking["models_changed_causal"],
                    "models_changed_non_causal": ranking["models_changed_non_causal"],
                    "mapping_coverage_causal": ranking["mapping_coverage_causal"],
                    "mapping_coverage_non_causal": ranking[
                        "mapping_coverage_non_causal"
                    ],
                    "baseline": summary["baseline_compatibility"],
                    "post_resolution": summary["post_resolution_compatibility"],
                    "phase_transition": summary["phase_transition_score"],
                    "crux_component": summary["lynchpin_component_id"],
                    "n_dyads": summary["n_dyads"],
                    "crux_mode": summary["crux_mode"],
                }
            )

    # MAS Delta-U must equal the difference between the baseline and the
    # post-resolution compatibility of the MAS crux.
    mas_rows = [row for row in rows if row["metric"] == "mas_compatible"]
    if mas_rows:
        mas_top = min(mas_rows, key=lambda r: r["rank"])
        mas_delta = round(mas_top["post_resolution"] - mas_top["baseline"], 6)
        if abs(mas_delta - mas_top["delta_u"]) > 1e-6:
            raise RuntimeError(
                "MAS Delta-U mismatch: " f"{mas_delta} != {mas_top['delta_u']}"
            )
    return runs, pd.DataFrame(rows)


# ── Phase 4: Ghost Discovery (redesigned) ─────────────────────────────────────
GHOST_NODE_ORDER = ["X2", "X3", "X1", "X4", "X5", "X6", "X7", "Y"]
GHOST_TIMING = {name: idx + 1 for idx, name in enumerate(GHOST_NODE_ORDER)}
GHOST_EXPOSURE = "X1"
GHOST_OUTCOME = "Y"
GHOST_REFERENCE_MODEL = "M0001"
GHOST_FAMILY_SIZES = {"mainstream": 30, "ghost": 12, "heterogeneous": 158}
GHOST_TOTAL_MODELS = sum(GHOST_FAMILY_SIZES.values())
GHOST_SEEDED_FAMILIES = ("mainstream", "ghost")
# Primary/reference setting for the seed-42 validation/acceptance run, the
# 100 reliability replicates, the secondary min_samples=10 diagnostic, and
# the per-cluster qualification audit. The seed-42 sweep is
# (strict 0.20, figure 0.35, reference 0.50): 0.20 is the strict setting
# intended to under-recover planted ghosts; 0.35 is the Figure C panel source;
# 0.50 is the primary/reference setting and carries the seed-42
# validation acceptance gate, the reliability replicates, the secondary
# diagnostic, and the per-cluster qualification audit (0.50 is now also the
# highest swept radius, so the qualification scope is named after the
# reference configuration). The figure eps=0.35 run additionally receives
# its own per-cluster qualification audit on the same seed-42 dual dyads,
# stored in the results JSON as the top-level
# ``figure_cluster_qualification`` object (scope
# ``seed42_figure_eps_qualification``) with no separate CSV artifact.
GHOST_PRIMARY_EPS = 0.50
GHOST_PRIMARY_MIN_SAMPLES = 4
GHOST_EPS_SWEEP = (0.20, 0.35, 0.50)
GHOST_SECONDARY_MIN_SAMPLES = 10
GHOST_INTERNAL_THRESHOLD = 0.60
GHOST_WITHIN_MAS_THRESHOLD = 0.60
GHOST_WITHIN_IDENTIFIED_THRESHOLD = 0.60
GHOST_REFERENCE_SIMILARITY_THRESHOLD = 0.50
GHOST_REFERENCE_MAS_THRESHOLD = 0.50
GHOST_IDENTIFIED_HIGH_THRESHOLD = 0.60
GHOST_IDENTIFIED_THRESHOLD_NOTE = (
    "Simulation 3 requires strict within-cluster identified_compatible >0.60; "
    "this is a design-specific qualification gate and does not change the "
    "generic GhostDetector defaults."
)
FLIP_PROBABILITY = 0.02
HETEROGENEOUS_CAUSAL_PROBABILITY = 0.50
DUPLICATE_POLICY = "reject and redraw from same family RNG stream"
FIXED_CAUSAL_EDGES = {("X1", "Y")}
PROTECTED_EDGES = {
    ("X2", "X3"): {"mainstream": "causal", "ghost": "non-causal"},
    ("X2", "X1"): {"mainstream": "non-causal", "ghost": "causal"},
    ("X3", "X1"): {"mainstream": "causal", "ghost": "causal"},
    ("X2", "Y"): {"mainstream": "causal", "ghost": "causal"},
    ("X3", "Y"): {"mainstream": "causal", "ghost": "causal"},
    ("X1", "Y"): {"mainstream": "causal", "ghost": "causal"},
}


def _ghost_forward_edges():
    return [
        (source, target)
        for si, source in enumerate(GHOST_NODE_ORDER)
        for target in GHOST_NODE_ORDER[si + 1 :]
    ]


def _ghost_prototype(mainstream: bool) -> set[tuple[str, str]]:
    edges = _ghost_forward_edges()
    causal = set()
    for edge in edges:
        source = edge[0]
        if edge in PROTECTED_EDGES:
            status = PROTECTED_EDGES[edge]["ghost" if not mainstream else "mainstream"]
            if status == "causal":
                causal.add(edge)
            continue
        if mainstream:
            sequential = [
                ("X2", "X3"),
                ("X3", "X1"),
                ("X1", "X4"),
                ("X4", "X5"),
                ("X5", "X6"),
                ("X6", "X7"),
                ("X7", "Y"),
            ]
            if edge in sequential:
                causal.add(edge)
                continue
            if edge[1] == "Y" and edge[0] != "Y":
                causal.add(edge)
                continue
        else:
            # Ghost prototype: edges from X2 and X3 to later nodes, plus X1 -> Y.
            if source in {"X2", "X3"} and edge[1] in {
                "X1",
                "X4",
                "X5",
                "X6",
                "X7",
                "Y",
            }:
                causal.add(edge)
                continue
            if edge == ("X1", "Y"):
                causal.add(edge)
    return causal


def _assert_ghost_prototype_contract():
    edges = _ghost_forward_edges()
    if len(edges) != 28:
        raise RuntimeError(f"Ghost timing-forward edges != 28: {len(edges)}")
    pm = _ghost_prototype(True)
    pg = _ghost_prototype(False)
    if len(pm) != 13 or len(pg) != 13:
        raise RuntimeError(
            f"Prototype causal counts wrong: P_M={len(pm)} P_G={len(pg)}"
        )
    shared_causal = pm & pg
    if len(shared_causal) != 4:
        raise RuntimeError(f"Shared causal prototypes != 4: {len(shared_causal)}")
    disagreements = pm.symmetric_difference(pg)
    if len(disagreements) != 18:
        raise RuntimeError(f"Prototype disagreements != 18: {len(disagreements)}")
    shared_noncausal = set(edges) - (pm | pg)
    if len(shared_noncausal) != 6:
        raise RuntimeError(f"Shared non-causal edges != 6: {len(shared_noncausal)}")
    node_count = len(GHOST_NODE_ORDER)
    numerator = node_count + len(shared_causal) + len(shared_noncausal)
    denominator = numerator + 2 * (len(pm - pg) + len(pg - pm))
    if (numerator, denominator) != (18, 54):
        raise RuntimeError(
            f"Prototype similarity fraction wrong: {numerator}/{denominator}"
        )
    if numerator / denominator != 1 / 3:
        raise RuntimeError("Prototype structural similarity is not 1/3")


def _ghost_registry():
    nodes = [
        {
            "name": name,
            "timing": GHOST_TIMING[name],
            "description": "Outcome" if name == "Y" else name,
        }
        for name in GHOST_NODE_ORDER
    ]
    candidate_edges = _ghost_forward_edges()
    registry = ComponentRegistryBuilder.from_nodes(
        nodes,
        respect_timing=True,
        include_bidirectional=False,
        constraints=[
            {"source": s, "target": t, "direction": "->", "rule": "allow"}
            for s, t in candidate_edges
        ],
        exposure=GHOST_EXPOSURE,
        outcome=GHOST_OUTCOME,
    )
    edge_rows = registry.data[registry.data["type"] == "edge"]
    registry_edges = {
        (str(row["source"]), str(row["target"])) for _, row in edge_rows.iterrows()
    }
    if registry_edges != set(candidate_edges):
        raise RuntimeError("Ghost registry does not contain every forward edge")
    return registry, candidate_edges


def _ghost_prototype_signature(registry, candidate_edges, prototype_causal):
    states = []
    edge_by_id = {
        str(row["comp_id"]): (str(row["source"]), str(row["target"]))
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows()
    }
    for _, component in registry.data.iterrows():
        comp_id = str(component["comp_id"])
        is_node = component["type"] == "node"
        if is_node:
            status = "present"
            timing = GHOST_TIMING[str(component["source"])]
        else:
            edge = edge_by_id[comp_id]
            status = "causal" if edge in prototype_causal else "non-causal"
            timing = None
        states.append(
            {
                "model_id": "prototype",
                "comp_id": comp_id,
                "status": status,
                "timing": timing,
            }
        )
    return states


def _validate_ghost_prototype_mas(registry, candidate_edges):
    from dyadic.profiles import normalize_mas

    causal_wrapper = CausalWrapper()
    mainstream = _ghost_prototype(True)
    ghost = _ghost_prototype(False)
    specs = {}
    for name, prototype in (("mainstream", mainstream), ("ghost", ghost)):
        states = _ghost_prototype_signature(registry, candidate_edges, prototype)
        state = StateTensor.from_records(registry, states, model_ids=["prototype"])
        spec = DyadicEngine()._dag_spec_for_model(
            "prototype", state, registry, exposure=GHOST_EXPOSURE, outcome=GHOST_OUTCOME
        )
        mas = causal_wrapper.compute_adjustment_sets(spec)
        specs[name] = normalize_mas(mas)
    main_sets = {frozenset(s) for s in specs["mainstream"]}
    ghost_sets = {frozenset(s) for s in specs["ghost"]}
    if frozenset() in main_sets or frozenset() in ghost_sets:
        raise RuntimeError(
            "Prototype adjustment sets include the empty set unexpectedly"
        )
    if main_sets & ghost_sets:
        raise RuntimeError(
            "Mainstream and ghost prototypes share a minimal adjustment set"
        )
    if main_sets != {frozenset({"X3"})}:
        raise RuntimeError(f"Mainstream prototype MAS not {{X3}}: {main_sets}")
    if ghost_sets != {frozenset({"X2", "X3"})}:
        raise RuntimeError(f"Ghost prototype MAS not {{X2, X3}}: {ghost_sets}")
    return {
        "mainstream_mas": _serialize_mas(specs["mainstream"]),
        "ghost_mas": _serialize_mas(specs["ghost"]),
    }


def _serialize_mas(mas):
    sets = (
        {frozenset(item) for item in mas}
        if not isinstance(next(iter(mas)), frozenset)
        else set(mas)
    )
    return [
        sorted(item)
        for item in sorted(sets, key=lambda value: (len(value), tuple(sorted(value))))
    ]


def _build_ghost_multiverse(seed: int = SEED):
    _assert_ghost_prototype_contract()
    registry, candidate_edges = _ghost_registry()
    prototype_mainstream = _ghost_prototype(True)
    prototype_ghost = _ghost_prototype(False)
    edge_by_id = {
        str(row["comp_id"]): (str(row["source"]), str(row["target"]))
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows()
    }
    rng = random.Random(seed)
    states = []
    family_by_model = {}
    used_signatures = set()
    model_index = 1

    for family, count in GHOST_FAMILY_SIZES.items():
        prototype = prototype_mainstream if family == "mainstream" else prototype_ghost
        for _ in range(count):
            while True:
                signature = []
                for edge in candidate_edges:
                    if edge in FIXED_CAUSAL_EDGES:
                        causal = True
                    elif family == "heterogeneous":
                        causal = rng.random() < HETEROGENEOUS_CAUSAL_PROBABILITY
                    else:
                        causal = edge in prototype
                        if edge in PROTECTED_EDGES:
                            protected = PROTECTED_EDGES[edge][family]
                            causal = protected == "causal"
                        elif rng.random() < FLIP_PROBABILITY:
                            causal = not causal
                    signature.append("causal" if causal else "non-causal")
                signature_tuple = tuple(signature)
                if signature_tuple not in used_signatures:
                    used_signatures.add(signature_tuple)
                    break
            model_id = f"M{model_index:04d}"
            model_index += 1
            family_by_model[model_id] = family
            status_by_edge = dict(zip(candidate_edges, signature_tuple))
            for _, component in registry.data.iterrows():
                comp_id = str(component["comp_id"])
                is_node = component["type"] == "node"
                states.append(
                    {
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": (
                            "present"
                            if is_node
                            else status_by_edge[edge_by_id[comp_id]]
                        ),
                        "timing": (
                            GHOST_TIMING[str(component["source"])] if is_node else None
                        ),
                    }
                )

    reference_model_id = GHOST_REFERENCE_MODEL
    family_counts = _assert_ghost_generation_contract(
        registry,
        states,
        family_by_model,
        used_signatures,
        reference_model_id,
    )
    seeded_family_total = sum(family_counts[family] for family in GHOST_SEEDED_FAMILIES)
    prototype_edges = {
        "mainstream": [f"{s}->{t}" for s, t in sorted(prototype_mainstream)],
        "ghost": [f"{s}->{t}" for s, t in sorted(prototype_ghost)],
    }
    design = {
        "seed": seed,
        "nodes": len(GHOST_NODE_ORDER),
        "node_order": GHOST_NODE_ORDER,
        "timing": GHOST_TIMING,
        "exposure": GHOST_EXPOSURE,
        "outcome": GHOST_OUTCOME,
        "timing_admissible_edges": len(candidate_edges),
        "resolved_model_space": 2 ** len(candidate_edges),
        "sample_models": GHOST_TOTAL_MODELS,
        "total_models": GHOST_TOTAL_MODELS,
        "family_sizes": dict(GHOST_FAMILY_SIZES),
        "family_counts": family_counts,
        "seeded_family_names": list(GHOST_SEEDED_FAMILIES),
        "seeded_family_total": seeded_family_total,
        "heterogeneous_model_count": family_counts["heterogeneous"],
        "heterogeneous_to_seeded_ratio": round(
            family_counts["heterogeneous"] / seeded_family_total, 6
        ),
        "within_family_flip_probability": FLIP_PROBABILITY,
        "heterogeneous_causal_probability": HETEROGENEOUS_CAUSAL_PROBABILITY,
        "fixed_causal_edges": [f"{s}->{t}" for s, t in sorted(FIXED_CAUSAL_EDGES)],
        "heterogeneous_random_flip_excluded_edges": [
            f"{s}->{t}" for s, t in sorted(FIXED_CAUSAL_EDGES)
        ],
        "protected_edges": {
            f"{s}->{t}": status for (s, t), status in PROTECTED_EDGES.items()
        },
        "prototype_disagreements": len(
            prototype_mainstream.symmetric_difference(prototype_ghost)
        ),
        "prototype_shared_causal": len(prototype_mainstream & prototype_ghost),
        "prototype_shared_noncausal": len(
            set(candidate_edges) - (prototype_mainstream | prototype_ghost)
        ),
        "prototype_similarity": 1 / 3,
        "prototype_edges": prototype_edges,
        "duplicate_policy": DUPLICATE_POLICY,
        "unique_models": len(used_signatures),
        "unique_model_count": len(used_signatures),
        "family_by_model": family_by_model,
        "reference_model_id": reference_model_id,
        "reference_model_family": family_by_model[reference_model_id],
        "design_hash": _design_hash(),
    }
    return registry, states, design


def _design_hash():
    mainstream = _ghost_prototype(True)
    ghost = _ghost_prototype(False)
    payload = {
        "node_order": GHOST_NODE_ORDER,
        "timing": GHOST_TIMING,
        "exposure": GHOST_EXPOSURE,
        "outcome": GHOST_OUTCOME,
        "family_sizes": GHOST_FAMILY_SIZES,
        "flip_probability": FLIP_PROBABILITY,
        "heterogeneous_causal_probability": HETEROGENEOUS_CAUSAL_PROBABILITY,
        "fixed_causal_edges": [f"{s}->{t}" for s, t in sorted(FIXED_CAUSAL_EDGES)],
        "protected_edges": {f"{s}->{t}": v for (s, t), v in PROTECTED_EDGES.items()},
        "candidate_edges": [f"{s}->{t}" for s, t in _ghost_forward_edges()],
        "prototype_edges": {
            "mainstream": [f"{s}->{t}" for s, t in sorted(mainstream)],
            "ghost": [f"{s}->{t}" for s, t in sorted(ghost)],
        },
        "duplicate_policy": DUPLICATE_POLICY,
    }
    return hashlib.sha256(json.dumps(payload, sort_keys=True).encode()).hexdigest()


def _ghost_family_counts(family_by_model):
    return {
        family: sum(1 for value in family_by_model.values() if value == family)
        for family in GHOST_FAMILY_SIZES
    }


def _assert_ghost_generation_contract(
    registry, states, family_by_model, used_signatures, reference_model_id
):
    """Assert population invariants without pinning any signature values."""
    if GHOST_TOTAL_MODELS != 200:
        raise RuntimeError(
            "Ghost family sizes must derive a total of exactly 200 models: "
            f"{GHOST_TOTAL_MODELS}"
        )
    family_counts = _ghost_family_counts(family_by_model)
    if set(family_by_model.values()) != set(GHOST_FAMILY_SIZES):
        raise RuntimeError(
            "Ghost generation produced an unexpected family label set: "
            f"{sorted(set(family_by_model.values()))}"
        )
    if family_counts != GHOST_FAMILY_SIZES:
        raise RuntimeError(
            "Ghost generation family counts are wrong: "
            f"{family_counts} != {GHOST_FAMILY_SIZES}"
        )

    generated_model_ids = set(family_by_model)
    if len(generated_model_ids) != GHOST_TOTAL_MODELS:
        raise RuntimeError(
            "Ghost generation did not produce exactly "
            f"{GHOST_TOTAL_MODELS} model IDs: {len(generated_model_ids)}"
        )
    if len(used_signatures) != GHOST_TOTAL_MODELS:
        raise RuntimeError(
            "Ghost generation did not produce unique model signatures: "
            f"{len(used_signatures)} unique signatures"
        )

    state_model_ids = {str(record["model_id"]) for record in states}
    if state_model_ids != generated_model_ids:
        raise RuntimeError("Ghost state records and generated model IDs differ")
    if len(states) != GHOST_TOTAL_MODELS * len(registry.data):
        raise RuntimeError("Ghost generation did not materialize every model component")

    edge_ids = {
        (str(row["source"]), str(row["target"])): str(row["comp_id"])
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows()
    }
    fixed_ids = {edge_ids[edge] for edge in FIXED_CAUSAL_EDGES}
    fixed_statuses = {
        (str(record["model_id"]), str(record["comp_id"])): record["status"]
        for record in states
        if str(record["comp_id"]) in fixed_ids
    }
    if len(fixed_statuses) != GHOST_TOTAL_MODELS * len(fixed_ids) or any(
        status != "causal" for status in fixed_statuses.values()
    ):
        raise RuntimeError(
            "Ghost generation did not keep every fixed causal edge causal in "
            "every generated model"
        )

    if reference_model_id not in generated_model_ids:
        raise RuntimeError(f"Ghost reference model is missing: {reference_model_id}")
    if family_by_model[reference_model_id] != "mainstream":
        raise RuntimeError(
            "Ghost reference model is not a mainstream model: "
            f"{reference_model_id} -> {family_by_model[reference_model_id]}"
        )
    return family_counts


def _ghost_assignment_audit(assignments, family_by_model, primary_cluster_id):
    """Separate planted family membership from DBSCAN's unassigned noise."""
    assignment_ids = [str(a["model_id"]) for a in assignments]
    if len(assignment_ids) != len(set(assignment_ids)):
        raise RuntimeError("Ghost cluster assignments contain duplicate model IDs")
    if set(assignment_ids) != set(family_by_model):
        raise RuntimeError(
            "Ghost cluster assignments do not cover the generated population"
        )
    assignment_by_model = {
        str(assignment["model_id"]): assignment.get("cluster_id")
        for assignment in assignments
    }

    def composition(model_ids):
        return {
            family: sum(
                1 for model_id in model_ids if family_by_model[model_id] == family
            )
            for family in GHOST_FAMILY_SIZES
        }

    primary_members = {
        model_id
        for model_id, cluster_id in assignment_by_model.items()
        if primary_cluster_id is not None and cluster_id == primary_cluster_id
    }
    noise_members = {
        model_id
        for model_id, cluster_id in assignment_by_model.items()
        if cluster_id is None
    }
    family_counts = _ghost_family_counts(family_by_model)
    primary_composition = composition(primary_members)
    noise_composition = composition(noise_members)
    false_positive_count = len(primary_members) - primary_composition["ghost"]
    seeded_family_total = sum(family_counts[family] for family in GHOST_SEEDED_FAMILIES)
    heterogeneous_to_seeded_ratio = (
        family_counts["heterogeneous"] / seeded_family_total
        if seeded_family_total
        else None
    )
    return {
        "family_counts": family_counts,
        "seeded_family_total": seeded_family_total,
        "heterogeneous_to_seeded_ratio": (
            round(heterogeneous_to_seeded_ratio, 6)
            if heterogeneous_to_seeded_ratio is not None
            else None
        ),
        "selected_primary_cluster_id": primary_cluster_id,
        "selected_primary_cluster_family_composition": primary_composition,
        "primary_cluster_family_composition": primary_composition,
        "selected_primary_cluster_size": len(primary_members),
        "primary_cluster_size": len(primary_members),
        "selected_primary_cluster_ghost_count": primary_composition["ghost"],
        "selected_primary_cluster_mainstream_count": primary_composition["mainstream"],
        "selected_primary_cluster_heterogeneous_count": primary_composition[
            "heterogeneous"
        ],
        "selected_primary_cluster_false_positive_count": false_positive_count,
        "primary_cluster_false_positive_count": false_positive_count,
        "dbscan_unassigned_noise_count": len(noise_members),
        "dbscan_unassigned_noise_family_composition": noise_composition,
        "dbscan_unassigned_mainstream_count": noise_composition["mainstream"],
        "dbscan_unassigned_ghost_count": noise_composition["ghost"],
        "dbscan_unassigned_heterogeneous_count": noise_composition["heterogeneous"],
        "clustered_heterogeneous_count": (
            family_counts["heterogeneous"] - noise_composition["heterogeneous"]
        ),
    }


def _ghost_audit_columns(design, audit):
    """Flatten generated audit data for the backwards-compatible CSV schema."""
    family_counts = audit.get("family_counts") or design.get("family_counts", {})
    primary_composition = audit.get(
        "selected_primary_cluster_family_composition",
        {family: 0 for family in GHOST_FAMILY_SIZES},
    )
    noise_composition = audit.get(
        "dbscan_unassigned_noise_family_composition",
        {family: 0 for family in GHOST_FAMILY_SIZES},
    )
    return {
        "family_counts": json.dumps(family_counts, sort_keys=True),
        "mainstream_family_count": family_counts.get("mainstream", 0),
        "ghost_family_count": family_counts.get("ghost", 0),
        "heterogeneous_family_count": family_counts.get("heterogeneous", 0),
        "seeded_family_total": audit.get("seeded_family_total"),
        "heterogeneous_to_seeded_ratio": audit.get("heterogeneous_to_seeded_ratio"),
        "selected_primary_cluster_id": audit.get("selected_primary_cluster_id"),
        "selected_primary_cluster_family_composition": json.dumps(
            primary_composition, sort_keys=True
        ),
        "primary_cluster_family_composition": json.dumps(
            primary_composition, sort_keys=True
        ),
        "selected_primary_cluster_size": audit.get("selected_primary_cluster_size", 0),
        "selected_primary_cluster_ghost_count": primary_composition.get("ghost", 0),
        "selected_primary_cluster_mainstream_count": primary_composition.get(
            "mainstream", 0
        ),
        "selected_primary_cluster_heterogeneous_count": primary_composition.get(
            "heterogeneous", 0
        ),
        "selected_primary_cluster_false_positive_count": audit.get(
            "selected_primary_cluster_false_positive_count", 0
        ),
        "primary_cluster_false_positive_count": audit.get(
            "primary_cluster_false_positive_count", 0
        ),
        "dbscan_unassigned_noise_count": audit.get("dbscan_unassigned_noise_count", 0),
        "dbscan_unassigned_noise_family_composition": json.dumps(
            noise_composition, sort_keys=True
        ),
        "dbscan_unassigned_mainstream_count": noise_composition.get("mainstream", 0),
        "dbscan_unassigned_ghost_count": noise_composition.get("ghost", 0),
        "dbscan_unassigned_heterogeneous_count": noise_composition.get(
            "heterogeneous", 0
        ),
        "clustered_heterogeneous_count": audit.get("clustered_heterogeneous_count", 0),
    }


def _select_primary_ghost(contrast):
    ghost_rows = [row for row in contrast if row.get("label") == "ghost"]
    if not ghost_rows:
        return None
    ghost_rows.sort(
        key=lambda r: (
            -float(r.get("internal_compatibility", 0.0)),
            str(r.get("cluster_id")),
        )
    )
    return ghost_rows[0]


def _select_mainstream_cluster(contrast, assignments, reference_model_id):
    ref_cluster = None
    for a in assignments:
        if a["model_id"] == reference_model_id:
            ref_cluster = a.get("cluster_id")
            break
    for row in contrast:
        if row.get("label") == "mainstream" and row.get("cluster_id") == ref_cluster:
            return row
    return None


def _cluster_membership(assignments, cluster_id):
    return {a["model_id"] for a in assignments if a.get("cluster_id") == cluster_id}


def _aggregate_compatibility(dyads, ego_ids, alter_ids):
    """Aggregate MAS and identified compatibility over ordered dyads.

    The authoritative denominator is the expected set of ordered non-self
    pairs built from ``ego_ids x alter_ids`` (self comparisons are excluded
    whenever the two sets overlap).  Duplicate dyad records for an expected
    pair are detected and rejected, wholly absent expected pairs are counted
    as unavailable, and a rate is reported only when every expected pair is
    present and carries the metric.  Comparable/unavailable counts plus the
    expected/present pair counts keep availability fully auditable.
    """
    expected_pairs = {
        (ego_id, alter_id)
        for ego_id in ego_ids
        for alter_id in alter_ids
        if ego_id != alter_id
    }
    seen_pairs = set()
    selected = []
    duplicates = []
    for dyad in dyads:
        pair = (dyad["ego_id"], dyad["alter_id"])
        if pair not in expected_pairs:
            continue
        if pair in seen_pairs:
            duplicates.append(pair)
            continue
        seen_pairs.add(pair)
        selected.append(dyad)
    if duplicates:
        raise RuntimeError(
            "Duplicate ordered dyad records in compatibility aggregation: "
            f"{sorted(duplicates)[:10]}{' ...' if len(duplicates) > 10 else ''} "
            f"({len(duplicates)} duplicate(s))"
        )
    n_expected = len(expected_pairs)
    n_present = len(selected)

    def metric_stats(metric):
        comparable = [d for d in selected if d.get(metric) is not None]
        n_unavailable = (n_expected - n_present) + (n_present - len(comparable))
        if n_expected and n_present == n_expected and len(comparable) == n_present:
            rate = round(float(np.mean([float(d[metric]) for d in comparable])), 6)
        else:
            rate = None
        return {
            "n_expected": n_expected,
            "n_present": n_present,
            "n_comparable": len(comparable),
            "n_unavailable": n_unavailable,
            "rate": rate,
        }

    return {
        "n_dyads": n_present,
        "n_expected": n_expected,
        "n_present": n_present,
        "mas_compatible": metric_stats("mas_compatible"),
        "identified_compatible": metric_stats("identified_compatible"),
    }


def _aggregate_mas(dyads, ego_ids, alter_ids):
    """Backwards-compatible MAS-only aggregation for the reliability runs."""
    stats = _aggregate_compatibility(dyads, ego_ids, alter_ids)
    mas = stats["mas_compatible"]
    return {
        "n_dyads": stats["n_dyads"],
        "n_comparable": mas["n_comparable"],
        "n_unavailable": mas["n_unavailable"],
        "mas_compatibility_rate": mas["rate"],
    }


def _pool_metric_stats(*stats):
    """Pool ordered-pair metric summaries without hiding unavailable pairs."""
    expected = sum(int(item.get("n_expected", 0)) for item in stats)
    present = sum(int(item.get("n_present", 0)) for item in stats)
    comparable = sum(int(item.get("n_comparable", 0)) for item in stats)
    unavailable = sum(int(item.get("n_unavailable", 0)) for item in stats)
    weighted = [
        (float(item["rate"]), int(item["n_comparable"]))
        for item in stats
        if item.get("rate") is not None and int(item.get("n_comparable", 0)) > 0
    ]
    rate = (
        round(
            sum(value * count for value, count in weighted)
            / sum(count for _, count in weighted),
            6,
        )
        if unavailable == 0 and comparable > 0 and weighted
        else None
    )
    return {
        "n_expected": expected,
        "n_present": present,
        "n_comparable": comparable,
        "n_unavailable": unavailable,
        "rate": rate,
    }


def _reference_cluster_id(assignments, reference_model_id):
    """Return the non-noise cluster containing the independent reference model."""
    for assignment in assignments:
        if (
            assignment.get("model_id") == reference_model_id
            and assignment.get("cluster_id") is not None
        ):
            return assignment.get("cluster_id")
    return None


def _simulation_3_cluster_classification(
    contrast,
    assignments,
    dual_dyads,
    reference_model_id,
    *,
    require_identified=True,
):
    """Apply the Simulation 3 strict classification contract.

    The reference group is derived from assignments alone, before any contrast
    label is inspected.  A common strict gate requires structural similarity,
    within-cluster MAS, and within-cluster complete-conditioning
    identification all to be strictly greater than 0.60, with no unavailable
    required comparisons.  A common-gate cluster is mainstream when its
    structural similarity to M0001 is strictly greater than 0.50.  It is a
    ghost only when that similarity is strictly below 0.50 and its pooled MAS
    compatibility with the reference group is strictly below 0.50.

    Clusters that are structurally weak are labelled ``fragmented``; clusters
    that are structurally coherent but fail the common or contrast gate are
    labelled ``unqualified``.  These labels are descriptive and do not alter
    the generic GhostDetector defaults.
    """
    reference_group_id = _reference_cluster_id(assignments, reference_model_id)
    assignment_map = {
        assignment["model_id"]: assignment.get("cluster_id")
        for assignment in assignments
    }
    cluster_members = {}
    for model_id, cluster_id in assignment_map.items():
        if cluster_id is not None:
            cluster_members.setdefault(cluster_id, set()).add(model_id)
    reference_group_members = cluster_members.get(reference_group_id, set())

    classified = []
    for original in sorted(contrast, key=lambda row: str(row["cluster_id"])):
        row = dict(original)
        cluster_id = row["cluster_id"]
        members = cluster_members.get(cluster_id, set())
        within = _aggregate_compatibility(dual_dyads, members, members)
        within_mas = within["mas_compatible"]
        within_identified = within["identified_compatible"]

        to_reference_group = _aggregate_compatibility(
            dual_dyads, members, reference_group_members
        )
        from_reference_group = _aggregate_compatibility(
            dual_dyads, reference_group_members, members
        )
        reference_group_mas = _pool_metric_stats(
            to_reference_group["mas_compatible"],
            from_reference_group["mas_compatible"],
        )

        internal_similarity = _finite_or_none(row.get("internal_compatibility"))
        reference_similarity = _finite_or_none(row.get("prior_compatibility"))
        common_unavailable = (
            within_mas["n_unavailable"] + within_identified["n_unavailable"]
        )
        identified_gate = (
            within_identified["rate"] is not None
            and within_identified["rate"] > GHOST_WITHIN_IDENTIFIED_THRESHOLD
            and within_identified["n_unavailable"] == 0
        )
        common_gate = (
            internal_similarity is not None
            and internal_similarity > GHOST_INTERNAL_THRESHOLD
            and within_mas["rate"] is not None
            and within_mas["rate"] > GHOST_WITHIN_MAS_THRESHOLD
            and (identified_gate if require_identified else True)
            and within_mas["n_unavailable"] == 0
            and (common_unavailable == 0 if require_identified else True)
        )
        reference_mas_available = bool(
            reference_group_id is not None
            and reference_group_members
            and reference_group_mas["n_unavailable"] == 0
            and reference_group_mas["rate"] is not None
        )
        if (
            internal_similarity is None
            or internal_similarity <= GHOST_INTERNAL_THRESHOLD
        ):
            label = "fragmented"
        elif not common_gate:
            label = "unqualified"
        elif not reference_group_members:
            label = "unqualified"
        elif reference_similarity is not None and (
            reference_similarity > GHOST_REFERENCE_SIMILARITY_THRESHOLD
        ):
            label = "mainstream"
        elif (
            reference_similarity is not None
            and reference_similarity < GHOST_REFERENCE_SIMILARITY_THRESHOLD
            and reference_mas_available
            and reference_group_mas["rate"] < GHOST_REFERENCE_MAS_THRESHOLD
        ):
            label = "ghost"
        else:
            # Strict equality at either contrast boundary, missing reference
            # evidence, and any other non-qualifying case do not pass.
            label = "unqualified"

        row.update(
            {
                "label": label,
                "reference_group_cluster_id": reference_group_id,
                "reference_group_size": len(reference_group_members),
                "within_mas_rate": within_mas["rate"],
                "within_mas_comparable_dyads": within_mas["n_comparable"],
                "within_mas_unavailable_dyads": within_mas["n_unavailable"],
                "within_identified_rate": within_identified["rate"],
                "within_identified_comparable_dyads": within_identified["n_comparable"],
                "within_identified_unavailable_dyads": within_identified[
                    "n_unavailable"
                ],
                "to_reference_group_mas_rate": to_reference_group["mas_compatible"][
                    "rate"
                ],
                "to_reference_group_mas_comparable_dyads": to_reference_group[
                    "mas_compatible"
                ]["n_comparable"],
                "to_reference_group_mas_unavailable_dyads": to_reference_group[
                    "mas_compatible"
                ]["n_unavailable"],
                "from_reference_group_mas_rate": from_reference_group["mas_compatible"][
                    "rate"
                ],
                "from_reference_group_mas_comparable_dyads": from_reference_group[
                    "mas_compatible"
                ]["n_comparable"],
                "from_reference_group_mas_unavailable_dyads": from_reference_group[
                    "mas_compatible"
                ]["n_unavailable"],
                "reference_group_mas_rate": reference_group_mas["rate"],
                "reference_group_mas_comparable_dyads": reference_group_mas[
                    "n_comparable"
                ],
                "reference_group_mas_unavailable_dyads": reference_group_mas[
                    "n_unavailable"
                ],
                "common_gate_passed": common_gate,
                "reference_group_mas_lt_0_50": (
                    reference_mas_available
                    and reference_group_mas["rate"] < GHOST_REFERENCE_MAS_THRESHOLD
                ),
                "qualifies_as_mainstream": label == "mainstream",
                "qualifies_as_ghost": label == "ghost",
            }
        )
        classified.append(row)
    return classified, reference_group_id, reference_group_members


def _compare_causal_pairs(
    state,
    registry,
    model_ids,
    *,
    causal_wrapper,
    identification_wrapper=None,
    exposure,
    outcome,
):
    """Compare causal profiles for ordered non-self pairs.

    ``identified_compatible`` is computed by the native complete-conditioning
    predicate in ``CausalProfileBuilder``.  The legacy general-ID wrapper is
    intentionally not supplied: Simulation 3's common gate must use the same
    identified metric as the package.
    """
    engine = DyadicEngine()
    model_ids = sorted(model_ids)
    for model_id in model_ids:
        engine._validate_acyclic(model_id, state, registry)

    profiles = engine._build_causal_profiles(
        state,
        registry,
        mode="full",
        causal_wrapper=causal_wrapper,
        identification_wrapper=identification_wrapper,
        exposure=exposure,
        outcome=outcome,
    )
    return [
        {
            "ego_id": ego_id,
            "alter_id": alter_id,
            **CausalProfileBuilder.compare(profiles[ego_id], profiles[alter_id]),
        }
        for ego_id in model_ids
        for alter_id in model_ids
        if ego_id != alter_id
    ]


def _compare_mas_pairs(
    state, registry, model_ids, *, causal_wrapper, exposure, outcome
):
    """MAS-only comparison used by the 100 reliability replicates."""
    return _compare_causal_pairs(
        state,
        registry,
        model_ids,
        causal_wrapper=causal_wrapper,
        identification_wrapper=None,
        exposure=exposure,
        outcome=outcome,
    )


def _ghost_recovery(assignments, family_by_model, primary_cluster_id):
    predicted = {
        a["model_id"]
        for a in assignments
        if primary_cluster_id is not None and a.get("cluster_id") == primary_cluster_id
    }
    expected = {model_id for model_id, fam in family_by_model.items() if fam == "ghost"}
    recovered = predicted & expected
    precision = len(recovered) / len(predicted) if predicted else 0.0
    recall = len(recovered) / len(expected) if expected else 0.0
    return {
        "expected_ghost_models": len(expected),
        "predicted_ghost_models": len(predicted),
        "recovered_ghost_models": len(recovered),
        "ghost_precision": round(precision, 6),
        "ghost_recall": round(recall, 6),
        "exact_recovery": predicted == expected,
    }


def _validate_ghost_clusters(
    registry,
    states,
    design,
    contrast,
    assignments,
    mas_dyads,
    *,
    require_identified=True,
):
    classified, reference_group_id, reference_group_members = (
        _simulation_3_cluster_classification(
            contrast,
            assignments,
            mas_dyads,
            design["reference_model_id"],
            require_identified=require_identified,
        )
    )
    primary = _select_primary_ghost(classified)
    reference_model_id = design["reference_model_id"]
    mainstream = _select_mainstream_cluster(classified, assignments, reference_model_id)
    family_by_model = design["family_by_model"]
    # The reference group is defined by membership, not by the detector's
    # mainstream label.  Keep the label-dependent row for reporting, but use
    # the independent reference group for all cross-cluster diagnostics.
    mainstream_members = set(reference_group_members)
    mainstream_cluster_contains_reference = (
        mainstream is not None
        and mainstream.get("cluster_id") is not None
        and reference_model_id in mainstream_members
    )
    mainstream_cluster_size = len(mainstream_members)
    audit = _ghost_assignment_audit(
        assignments,
        family_by_model,
        primary.get("cluster_id") if primary is not None else None,
    )
    recovery = _ghost_recovery(
        assignments,
        family_by_model,
        primary.get("cluster_id") if primary is not None else None,
    )

    if primary is None:
        gate = {
            "ghost_cluster_found": False,
            "mainstream_cluster_contains_reference": mainstream_cluster_contains_reference,
            "passed": False,
        }
        return {
            "primary_ghost": None,
            "mainstream": mainstream,
            "classified_contrast": classified,
            "reference_group_cluster_id": reference_group_id,
            "reference_group_members": sorted(reference_group_members),
            "mainstream_members": sorted(mainstream_members),
            "mainstream_cluster_contains_reference": (
                mainstream_cluster_contains_reference
            ),
            "mainstream_cluster_size": mainstream_cluster_size,
            "recovery": recovery,
            "mas_validation": None,
            "audit": audit,
            "acceptance_gate": gate,
        }

    ghost_members = _cluster_membership(assignments, primary["cluster_id"])
    reference = reference_group_members

    within_ghost = _aggregate_mas(mas_dyads, ghost_members, ghost_members)
    ghost_to_reference = _aggregate_mas(mas_dyads, ghost_members, reference)
    reference_to_ghost = _aggregate_mas(mas_dyads, reference, ghost_members)
    ghost_to_mainstream = _aggregate_mas(mas_dyads, ghost_members, mainstream_members)
    mainstream_to_ghost = _aggregate_mas(mas_dyads, mainstream_members, ghost_members)
    within_mainstream = _aggregate_mas(
        mas_dyads, mainstream_members, mainstream_members
    )

    within_ghost_rate = within_ghost["mas_compatibility_rate"]
    ref_to_ghost_rate = ghost_to_reference["mas_compatibility_rate"]
    primary_classification = next(
        row for row in classified if row["cluster_id"] == primary["cluster_id"]
    )

    checks = {
        "structural_ghost_found": primary_classification["qualifies_as_ghost"],
        "mainstream_cluster_contains_reference": mainstream_cluster_contains_reference,
        "ghost_cluster_at_least_two_models": len(ghost_members) >= 2,
        "no_unavailable_mas_dyads": all(
            stat["n_unavailable"] == 0
            for stat in [
                within_ghost,
                ghost_to_reference,
                reference_to_ghost,
                ghost_to_mainstream,
                mainstream_to_ghost,
                within_mainstream,
            ]
        ),
        "no_unavailable_identified_dyads": (
            all(
                primary_classification[column] == 0
                for column in (
                    "within_identified_unavailable_dyads",
                    "reference_group_mas_unavailable_dyads",
                )
            )
            if require_identified
            else True
        ),
        "within_ghost_mas_gt_0_60": (
            within_ghost_rate is not None
            and within_ghost_rate > GHOST_WITHIN_MAS_THRESHOLD
        ),
        "within_ghost_identified_gt_0_60": (
            (
                primary_classification["within_identified_rate"] is not None
                and primary_classification["within_identified_rate"]
                > GHOST_WITHIN_IDENTIFIED_THRESHOLD
            )
            if require_identified
            else True
        ),
        "ghost_to_reference_mas_lt_0_50": (
            ref_to_ghost_rate is not None
            and ref_to_ghost_rate < GHOST_REFERENCE_MAS_THRESHOLD
        ),
    }
    passed = all(checks.values())
    return {
        "primary_ghost": primary,
        "mainstream": mainstream,
        "classified_contrast": classified,
        "reference_group_cluster_id": reference_group_id,
        "reference_group_members": sorted(reference_group_members),
        "ghost_members": sorted(ghost_members),
        "mainstream_members": sorted(mainstream_members),
        "mainstream_cluster_contains_reference": (
            mainstream_cluster_contains_reference
        ),
        "mainstream_cluster_size": mainstream_cluster_size,
        "recovery": recovery,
        "audit": audit,
        "mas_validation": {
            "within_ghost": within_ghost,
            "ghost_to_reference": ghost_to_reference,
            "reference_to_ghost": reference_to_ghost,
            "ghost_to_mainstream": ghost_to_mainstream,
            "mainstream_to_ghost": mainstream_to_ghost,
            "within_mainstream": within_mainstream,
            "within_ghost_identified_rate": primary_classification[
                "within_identified_rate"
            ],
            "reference_group_mas_rate": primary_classification[
                "reference_group_mas_rate"
            ],
        },
        "acceptance_gate": {"checks": checks, "passed": bool(passed)},
    }


def _finite_or_none(value):
    try:
        number = float(value)
    except (TypeError, ValueError):
        return None
    return number if math.isfinite(number) else None


def _qualify_ghost_clusters(
    registry,
    states,
    design,
    contrast,
    assignments,
    dual_dyads,
    *,
    eps,
    min_samples,
    scope,
):
    """Build one strict qualification record per non-noise cluster.

    The caller supplies the run's eps, min_samples, and a scope label, so the
    audit runs on whichever configuration's artifacts are passed.  The
    classification itself is shared with the seed-42 acceptance audit: the
    common gate is strict ``> 0.60`` for structural, MAS, and native
    complete-conditioning identification compatibility, with no unavailable
    required comparisons.  The reference group is the cluster containing
    ``reference_model_id`` independently of its detector label.  Mainstream
    and ghost labels then use strict ``> 0.50`` and ``< 0.50`` contrast rules,
    respectively; a ghost also requires pooled MAS compatibility with the
    reference group ``< 0.50``.

    DBSCAN noise is deliberately excluded; its count and family composition
    are returned alongside the records so the exclusion stays auditable.
    """
    family_by_model = design["family_by_model"]
    reference_model_id = design["reference_model_id"]
    classified, reference_group_id, reference_group_members = (
        _simulation_3_cluster_classification(
            contrast,
            assignments,
            dual_dyads,
            reference_model_id,
        )
    )
    # Cross-cluster diagnostic columns use the reference group even if that
    # group is ultimately labelled unqualified; the label is not its identity.
    mainstream_members = set(reference_group_members)

    def composition(members):
        return {
            family: sum(1 for member in members if family_by_model[member] == family)
            for family in GHOST_FAMILY_SIZES
        }

    def metric_columns(prefix, comparison):
        stats = comparison
        mas = stats["mas_compatible"]
        identified = stats["identified_compatible"]
        return {
            f"{prefix}_mas_rate": mas["rate"],
            f"{prefix}_mas_comparable_dyads": mas["n_comparable"],
            f"{prefix}_mas_unavailable_dyads": mas["n_unavailable"],
            f"{prefix}_identified_rate": identified["rate"],
            f"{prefix}_identified_comparable_dyads": identified["n_comparable"],
            f"{prefix}_identified_unavailable_dyads": identified["n_unavailable"],
        }

    records = []
    for row in sorted(classified, key=lambda r: str(r["cluster_id"])):
        cluster_id = row["cluster_id"]
        members = _cluster_membership(assignments, cluster_id)
        counts = composition(members)
        internal_similarity = _finite_or_none(row.get("internal_compatibility"))
        reference_similarity = _finite_or_none(row.get("prior_compatibility"))
        internal_coherent = (
            internal_similarity is not None
            and internal_similarity > GHOST_INTERNAL_THRESHOLD
        )
        reference_distinct = (
            reference_similarity is not None
            and reference_similarity < GHOST_REFERENCE_SIMILARITY_THRESHOLD
        )

        comparisons = {
            "within": _aggregate_compatibility(dual_dyads, members, members),
            "to_reference": _aggregate_compatibility(
                dual_dyads, members, reference_group_members
            ),
            "from_reference": _aggregate_compatibility(
                dual_dyads, reference_group_members, members
            ),
            "to_mainstream": _aggregate_compatibility(
                dual_dyads, members, mainstream_members
            ),
            "from_mainstream": _aggregate_compatibility(
                dual_dyads, mainstream_members, members
            ),
        }

        within_mas = comparisons["within"]["mas_compatible"]
        within_identified = comparisons["within"]["identified_compatible"]
        high_mas = (
            within_mas["rate"] is not None
            and within_mas["rate"] > GHOST_WITHIN_MAS_THRESHOLD
        )
        high_identified = (
            within_identified["rate"] is not None
            and within_identified["rate"] > GHOST_WITHIN_IDENTIFIED_THRESHOLD
        )
        common_gate = bool(row.get("common_gate_passed"))
        reference_group_mas = _pool_metric_stats(
            comparisons["to_reference"]["mas_compatible"],
            comparisons["from_reference"]["mas_compatible"],
        )
        unavailable_required = (
            within_mas["n_unavailable"]
            + within_identified["n_unavailable"]
            + reference_group_mas["n_unavailable"]
        )
        qualifies = bool(row.get("qualifies_as_ghost"))
        qualifies_mainstream = bool(row.get("qualifies_as_mainstream"))
        records.append(
            {
                "eps": eps,
                "min_samples": min_samples,
                "cluster_id": cluster_id,
                "label": row.get("label"),
                "model_count": len(members),
                "mainstream_count": counts["mainstream"],
                "ghost_count": counts["ghost"],
                "heterogeneous_count": counts["heterogeneous"],
                "family_composition": json.dumps(counts, sort_keys=True),
                "internal_similarity": internal_similarity,
                "reference_similarity": reference_similarity,
                "internal_coherent": internal_coherent,
                "reference_distinct": reference_distinct,
                "reference_group_cluster_id": reference_group_id,
                "reference_group_size": len(reference_group_members),
                **metric_columns("within", comparisons["within"]),
                **metric_columns("to_reference", comparisons["to_reference"]),
                **metric_columns("from_reference", comparisons["from_reference"]),
                **metric_columns("to_mainstream", comparisons["to_mainstream"]),
                **metric_columns("from_mainstream", comparisons["from_mainstream"]),
                "high_mas": high_mas,
                "high_identified": high_identified,
                "common_gate_passed": common_gate,
                "required_unavailable_dyads": unavailable_required,
                "reference_group_mas_rate": reference_group_mas["rate"],
                "reference_group_mas_comparable_dyads": reference_group_mas[
                    "n_comparable"
                ],
                "reference_group_mas_unavailable_dyads": reference_group_mas[
                    "n_unavailable"
                ],
                "qualifies_as_mainstream": qualifies_mainstream,
                "qualifies_as_ghost": qualifies,
            }
        )

    noise_members = {a["model_id"] for a in assignments if a.get("cluster_id") is None}
    noise_family_composition = {
        family: sum(1 for member in noise_members if family_by_model[member] == family)
        for family in GHOST_FAMILY_SIZES
    }
    return {
        "eps": eps,
        "min_samples": min_samples,
        "scope": scope,
        "reference_model_id": reference_model_id,
        "reference_group_cluster_id": reference_group_id,
        "reference_group_size": len(reference_group_members),
        "excluded_noise_count": len(noise_members),
        "excluded_noise_family_composition": noise_family_composition,
        "qualification_thresholds": {
            "internal_similarity_strictly_above": GHOST_INTERNAL_THRESHOLD,
            "within_mas_strictly_above": GHOST_WITHIN_MAS_THRESHOLD,
            "within_identified_strictly_above": GHOST_WITHIN_IDENTIFIED_THRESHOLD,
            "reference_similarity_mainstream_strictly_above": (
                GHOST_REFERENCE_SIMILARITY_THRESHOLD
            ),
            "reference_similarity_ghost_strictly_below": (
                GHOST_REFERENCE_SIMILARITY_THRESHOLD
            ),
            "reference_group_mas_ghost_strictly_below": GHOST_REFERENCE_MAS_THRESHOLD,
            "required_unavailable_dyads": 0,
        },
        "identified_high_threshold": GHOST_WITHIN_IDENTIFIED_THRESHOLD,
        "identified_threshold_note": GHOST_IDENTIFIED_THRESHOLD_NOTE,
        "records": records,
    }


def _qualification_summary(qualification):
    """Compact, data-driven summary of a per-cluster qualification object.

    The qualifying cluster ids are read from the qualification records
    themselves (never hard-coded); ``non_noise_cluster_count`` is the number
    of records, one per actual non-noise cluster of the audited run.  The
    summary is recorded in the design metadata and in the top-level
    ``simulation_3`` metadata block for the figure-radius qualification.
    """
    records = qualification["records"]
    qualifying_ids = [
        str(record["cluster_id"])
        for record in records
        if record.get("qualifies_as_ghost") is True
    ]
    return {
        "eps": qualification["eps"],
        "min_samples": qualification["min_samples"],
        "non_noise_cluster_count": len(records),
        "qualifying_cluster_count": len(qualifying_ids),
        "qualifying_cluster_ids": qualifying_ids,
        "excluded_noise_count": qualification["excluded_noise_count"],
    }


def _ghost_reliability_summary(reliability_df: pd.DataFrame, provenance: str) -> dict:
    """Recompute the reliability summary and predeclared gates.

    ``provenance`` is recorded verbatim; the Simulation 3 driver supplies only
    ``"computed"`` because stale reliability artifacts are not accepted.
    """
    if provenance != "computed":
        raise RuntimeError(
            "Simulation 3 reliability provenance must be 'computed', got "
            f"{provenance!r}"
        )
    required_columns = {
        "within_ghost_identified",
        "within_ghost_identified_comparable_dyads",
        "within_ghost_identified_unavailable_dyads",
    }
    missing_columns = sorted(required_columns - set(reliability_df.columns))
    if missing_columns:
        raise RuntimeError(
            "Simulation 3 reliability evidence is missing identified metric "
            f"columns: {missing_columns}"
        )
    structural_detections = int(reliability_df["ghost_found"].sum())
    joint_passes = int(reliability_df["gate_passed"].sum())
    detected = reliability_df[reliability_df["ghost_found"]]
    reliability_summary = {
        "replicates": REPLICATES,
        "sample_seeds": [1000 + replicate for replicate in range(1, REPLICATES + 1)],
        "analysis_role": "primary_eps_reliability",
        "eps": GHOST_PRIMARY_EPS,
        "min_samples": GHOST_PRIMARY_MIN_SAMPLES,
        "reliability_provenance": provenance,
        "gate_metrics": [
            "similarity_rate",
            "mas_compatible",
            "identified_compatible",
        ],
        "structural_ghost_detections": structural_detections,
        "structural_detection_rate": round(structural_detections / REPLICATES, 6),
        "structural_detection_gate": {
            "required": RELIABILITY_GATE_REQUIRED,
            "passed": structural_detections >= RELIABILITY_GATE_REQUIRED,
        },
        "joint_gate_passes": joint_passes,
        "joint_gate_pass_rate": round(joint_passes / REPLICATES, 6),
        "joint_gate": {
            "required": RELIABILITY_JOINT_GATE_REQUIRED,
            "passed": joint_passes >= RELIABILITY_JOINT_GATE_REQUIRED,
        },
        "mean_precision": round(float(reliability_df["ghost_precision"].mean()), 6),
        "mean_recall": round(float(reliability_df["ghost_recall"].mean()), 6),
        "exact_recovery_rate": round(float(reliability_df["exact_recovery"].mean()), 6),
        "conditional_mean_within_ghost_mas": (
            round(float(detected["within_ghost_mas"].dropna().mean()), 6)
            if not detected["within_ghost_mas"].dropna().empty
            else None
        ),
        "conditional_mean_within_ghost_identified": (
            round(float(detected["within_ghost_identified"].dropna().mean()), 6)
            if not detected["within_ghost_identified"].dropna().empty
            else None
        ),
        "fraction_detected_ghosts_identified_above_0_60": (
            round(
                float(
                    (
                        detected["within_ghost_identified"]
                        > GHOST_WITHIN_IDENTIFIED_THRESHOLD
                    ).mean()
                ),
                6,
            )
            if not detected["within_ghost_identified"].dropna().empty
            else None
        ),
        "fraction_detected_ghosts_mas_above_0_60": (
            round(
                float(
                    (detected["within_ghost_mas"] > GHOST_WITHIN_MAS_THRESHOLD).mean()
                ),
                6,
            )
            if not detected["within_ghost_mas"].dropna().empty
            else None
        ),
        "conditional_mean_ghost_to_reference_mas": (
            round(float(detected["ghost_to_reference_mas"].dropna().mean()), 6)
            if not detected["ghost_to_reference_mas"].dropna().empty
            else None
        ),
        "conditional_mean_ghost_to_mainstream_mas": (
            round(float(detected["ghost_to_mainstream_mas"].dropna().mean()), 6)
            if not detected["ghost_to_mainstream_mas"].dropna().empty
            else None
        ),
    }
    if not reliability_summary["structural_detection_gate"]["passed"]:
        raise RuntimeError(
            "Simulation 3 structural reliability gate failed: "
            f"{structural_detections}/{REPLICATES}"
        )
    if not reliability_summary["joint_gate"]["passed"]:
        raise RuntimeError(
            "Simulation 3 joint reliability gate failed: "
            f"{joint_passes}/{REPLICATES}"
        )
    return reliability_summary


def _apply_simulation_3_classification(
    result, design, comparison_dyads, *, require_identified=True
):
    """Replace detector labels with the explicit Simulation 3 labels."""
    artifacts = result["artifacts"]
    if artifacts.get("prior_model_id") != design["reference_model_id"]:
        raise RuntimeError(
            "Simulation 3 contrast prior must be the independent reference "
            f"model {design['reference_model_id']}"
        )
    classified, reference_group_id, reference_group_members = (
        _simulation_3_cluster_classification(
            artifacts["contrast_analysis"],
            artifacts["cluster_assignments"],
            comparison_dyads,
            design["reference_model_id"],
            require_identified=require_identified,
        )
    )
    artifacts["contrast_analysis"] = classified
    ghost_clusters = [row for row in classified if row["label"] == "ghost"]
    mainstream_clusters = [row for row in classified if row["label"] == "mainstream"]
    summary = result["results"]
    summary["ghost_cluster_found"] = bool(ghost_clusters)
    summary["ghost_clusters"] = ghost_clusters
    summary["mainstream_cluster"] = (
        mainstream_clusters[0] if mainstream_clusters else None
    )
    summary["total_ghost_models"] = sum(
        int(row.get("model_count", 0)) for row in ghost_clusters
    )
    summary["top_ghost_cluster"] = (
        max(ghost_clusters, key=lambda row: row["internal_compatibility"])
        if ghost_clusters
        else None
    )
    return classified, reference_group_id, reference_group_members


def _run_ghost() -> tuple:
    """Run the seed-42 Simulation 3 sweep and fresh reliability study.

    Reliability is always recomputed for the current generator and metric
    implementation.  There is deliberately no preserved-artifact input path:
    an old CSV cannot silently become evidence for a changed design.
    """
    configurations = [
        {
            "configuration": "strict",
            "eps": GHOST_EPS_SWEEP[0],
            "min_samples": GHOST_PRIMARY_MIN_SAMPLES,
            "analysis_role": "primary_eps_sensitivity",
            "sensitivity_axis": "eps",
        },
        {
            "configuration": "figure",
            "eps": GHOST_EPS_SWEEP[1],
            "min_samples": GHOST_PRIMARY_MIN_SAMPLES,
            "analysis_role": "primary_eps_sensitivity",
            "sensitivity_axis": "eps",
        },
        {
            "configuration": "reference",
            "eps": GHOST_EPS_SWEEP[2],
            "min_samples": GHOST_PRIMARY_MIN_SAMPLES,
            "analysis_role": "primary_eps_sensitivity",
            "sensitivity_axis": "eps",
        },
    ]
    registry, states, design = _build_ghost_multiverse(SEED)
    prototype_mas = _validate_ghost_prototype_mas(registry, _ghost_forward_edges())
    model_ids = sorted({str(r["model_id"]) for r in states})
    state = StateTensor.from_records(registry, states, model_ids=model_ids)

    # Discovery uses structural similarity only. Causal validation for the
    # seed-42 and reliability runs uses the native complete-conditioning
    # identified metric as well as MAS; no legacy general-ID wrapper is
    # involved. Reliability gates therefore cover structural, MAS, and
    # identified compatibility together.
    dual_dyads = _compare_causal_pairs(
        state,
        registry,
        model_ids,
        causal_wrapper=CausalWrapper(causal_backend="native"),
        identification_wrapper=None,
        exposure=GHOST_EXPOSURE,
        outcome=GHOST_OUTCOME,
    )

    runs = []
    rows = []
    seed42_validation = None
    for config in configurations:
        result = SimulationSuite(SEED).run_scenario(
            "ghost_discovery",
            registry_data=_dataframe_records(registry.data),
            state_data=states,
            eps=config["eps"],
            min_samples=config["min_samples"],
            compatibility_metric="similarity_rate",
            enforce_thresholds=False,
            include_plot_data=config["configuration"] == "figure",
        )
        artifacts = result["artifacts"]
        _apply_simulation_3_classification(result, design, dual_dyads)
        contrast = artifacts["contrast_analysis"]
        assignments = artifacts["cluster_assignments"]
        summary = result["results"]
        validation = _validate_ghost_clusters(
            registry, states, design, contrast, assignments, dual_dyads
        )
        runs.append(
            {
                "generation": design,
                "configuration": config,
                "metric": "similarity_rate",
                "recovery": validation["recovery"],
                "validation": validation,
                "run": result,
            }
        )
        recovery = validation["recovery"]
        top = validation.get("primary_ghost") or {}
        mas_val = validation.get("mas_validation") or {}
        within_ghost = mas_val.get("within_ghost") or {}
        ghost_to_ref = mas_val.get("ghost_to_reference") or {}
        ghost_to_main = mas_val.get("ghost_to_mainstream") or {}
        rows.append(
            {
                **config,
                "metric": "similarity_rate",
                "clusters": summary["clusters_detected"],
                "noise_models": summary["noise_count"],
                "ghost_found": summary["ghost_cluster_found"],
                "ghost_models": summary["total_ghost_models"],
                "top_internal_similarity": top.get("internal_compatibility"),
                "top_reference_similarity": top.get("prior_compatibility"),
                "top_internal_mas_compatibility": within_ghost.get(
                    "mas_compatibility_rate"
                ),
                "top_reference_mas_compatibility": ghost_to_ref.get(
                    "mas_compatibility_rate"
                ),
                "top_ghost_to_mainstream_mas_compatibility": ghost_to_main.get(
                    "mas_compatibility_rate"
                ),
                "mas_comparable_dyads": within_ghost.get("n_comparable", 0)
                + ghost_to_ref.get("n_comparable", 0)
                + ghost_to_main.get("n_comparable", 0),
                "mas_unavailable_dyads": within_ghost.get("n_unavailable", 0)
                + ghost_to_ref.get("n_unavailable", 0)
                + ghost_to_main.get("n_unavailable", 0),
                "profile_dimensions": len(model_ids),  # aligned profiles
                **recovery,
                **_ghost_audit_columns(design, validation.get("audit", {})),
            }
        )
        # The seed-42 validation and its acceptance gate come from the
        # primary/reference eps=0.50 run, not the strict eps=0.20 run or the
        # figure eps=0.35 run.  The figure run is the Figure C panel source;
        # the reference run also carries the per-cluster
        # qualification audit below.
        if config["configuration"] == "reference":
            seed42_validation = validation

    seed42_gate = (seed42_validation or {}).get("acceptance_gate", {})
    if not seed42_gate.get("passed", False):
        raise RuntimeError(
            "Seed-42 Simulation 3 acceptance gate failed; reliability run not started: "
            f"{seed42_gate}"
        )

    # Seed-42 strict-setting showcase invariant.  At the strict eps=0.20
    # radius the selected primary recovers 11 of the 12 planted ghost models,
    # so the showcase claim is explicit under-recovery: fewer than the 12
    # planted ghost models recovered by the selected primary
    # (equivalently recall < 1).  The total number of structurally
    # ghost-labeled models is recorded for audit but is NOT required to stay
    # below the planted count, because heterogeneous false-positive ghost
    # clusters can legitimately push that total above 12.  This is a seed-42
    # showcase invariant for the strict row only; it is NOT part of the
    # primary/reference eps=0.50 acceptance gate and NOT part of the
    # 100-replicate reliability gates.
    strict_row = next(row for row in rows if row["configuration"] == "strict")
    strict_total = strict_row["ghost_models"]
    strict_recovered = strict_row["recovered_ghost_models"]
    strict_recall = strict_row["ghost_recall"]
    strict_showcase = {
        "configuration": strict_row["configuration"],
        "analysis_role": strict_row["analysis_role"],
        "eps": strict_row["eps"],
        "min_samples": strict_row["min_samples"],
        "ghost_labeled_clusters": strict_row["clusters"],
        "unassigned_noise_models": strict_row["noise_models"],
        "ghost_labeled_cluster_total_models": strict_total,  # audit only
        "selected_primary_predicted_ghost_models": strict_row["predicted_ghost_models"],
        "selected_primary_recovered_ghost_models": strict_recovered,
        "selected_primary_ghost_precision": strict_row["ghost_precision"],
        "selected_primary_ghost_recall": strict_recall,
        "planted_ghost_models": GHOST_FAMILY_SIZES["ghost"],
        "selected_primary_recovered_lt_12": (
            strict_recovered < GHOST_FAMILY_SIZES["ghost"]
        ),
        "recall_lt_1": strict_recall < 1.0,
        "passed": (
            strict_recovered < GHOST_FAMILY_SIZES["ghost"] and strict_recall < 1.0
        ),
    }
    if not strict_showcase["passed"]:
        raise RuntimeError(
            "Seed-42 Simulation 3 strict-setting showcase invariant failed; "
            "the strict eps=0.20 radius must under-recover the planted ghost "
            "family via the selected primary (recovered < 12 and recall < 1): "
            f"{strict_showcase}"
        )

    # Secondary diagnostic: hold the reference epsilon fixed and vary DBSCAN's
    # min_samples.  This is deliberately recorded outside the three primary
    # epsilon-sensitivity rows and cannot affect either predeclared gate.
    secondary_config = {
        "configuration": "min_samples_10",
        "eps": GHOST_PRIMARY_EPS,
        "min_samples": GHOST_SECONDARY_MIN_SAMPLES,
        "analysis_role": "secondary_min_samples_sensitivity",
        "sensitivity_axis": "min_samples",
    }
    secondary_result = SimulationSuite(SEED).run_scenario(
        "ghost_discovery",
        registry_data=_dataframe_records(registry.data),
        state_data=states,
        eps=secondary_config["eps"],
        min_samples=secondary_config["min_samples"],
        compatibility_metric="similarity_rate",
        enforce_thresholds=False,
        include_plot_data=False,
    )
    _apply_simulation_3_classification(secondary_result, design, dual_dyads)
    secondary_validation = _validate_ghost_clusters(
        registry,
        states,
        design,
        secondary_result["artifacts"]["contrast_analysis"],
        secondary_result["artifacts"]["cluster_assignments"],
        dual_dyads,
    )
    runs.append(
        {
            **secondary_config,
            "seed": SEED,
            "controls_primary_gate": False,
            "controls_reliability_gate": False,
            "metric": "similarity_rate",
            "recovery": secondary_validation["recovery"],
            "validation": secondary_validation,
            # Keep the secondary diagnostic compact while retaining its result
            # values; the primary runs continue to carry full plot artifacts.
            "run": {"results": secondary_result["results"]},
        }
    )

    # Seed-42 per-cluster qualification at the reference eps (0.50, now the
    # highest swept radius): one record per non-noise cluster scored on
    # structural, MAS, and identified criteria.  It is the reference run's own
    # artifact: its excluded-noise audit is self-contained and does not reuse
    # the acceptance-gate audit.
    reference_run = next(
        run for run in runs if run["configuration"]["configuration"] == "reference"
    )
    reference_artifacts = reference_run["run"]["artifacts"]
    qualification = _qualify_ghost_clusters(
        registry,
        states,
        design,
        reference_artifacts["contrast_analysis"],
        reference_artifacts["cluster_assignments"],
        dual_dyads,
        eps=GHOST_EPS_SWEEP[2],
        min_samples=GHOST_PRIMARY_MIN_SAMPLES,
        scope="seed42_reference_eps_qualification",
    )
    qualification_df = pd.DataFrame(qualification["records"])

    # Seed-42 per-cluster qualification at the figure eps (0.35): the same
    # audit run on the figure configuration's own artifacts with the same
    # seed-42 dual dyads.  It is stored in the results JSON as the top-level
    # ``figure_cluster_qualification`` object; unlike the reference
    # qualification it has no separate CSV artifact.  The verifier derives the
    # observed qualifying count and ids from the actual records; no cluster
    # result is hard-coded here.
    figure_run = next(
        run for run in runs if run["configuration"]["configuration"] == "figure"
    )
    figure_artifacts = figure_run["run"]["artifacts"]
    figure_qualification = _qualify_ghost_clusters(
        registry,
        states,
        design,
        figure_artifacts["contrast_analysis"],
        figure_artifacts["cluster_assignments"],
        dual_dyads,
        eps=GHOST_EPS_SWEEP[1],
        min_samples=GHOST_PRIMARY_MIN_SAMPLES,
        scope="seed42_figure_eps_qualification",
    )

    # Run all 100 reliability seeds at the primary/reference eps=0.50 setting.
    # Each replicate carries structural, MAS, and identified compatibility
    # metrics through the same causal dyad comparison used by seed 42.
    reliability_rows = []
    for replicate in range(1, REPLICATES + 1):
        sample_seed = 1000 + replicate
        rep_registry, rep_states, rep_design = _build_ghost_multiverse(sample_seed)
        rep_model_ids = sorted({str(r["model_id"]) for r in rep_states})
        rep_state = StateTensor.from_records(
            rep_registry, rep_states, model_ids=rep_model_ids
        )
        rep_dual_dyads = _compare_causal_pairs(
            rep_state,
            rep_registry,
            rep_model_ids,
            causal_wrapper=CausalWrapper(causal_backend="native"),
            exposure=GHOST_EXPOSURE,
            outcome=GHOST_OUTCOME,
        )
        result = SimulationSuite(sample_seed).run_scenario(
            "ghost_discovery",
            registry_data=_dataframe_records(rep_registry.data),
            state_data=rep_states,
            eps=GHOST_PRIMARY_EPS,
            min_samples=GHOST_PRIMARY_MIN_SAMPLES,
            compatibility_metric="similarity_rate",
            enforce_thresholds=False,
            include_plot_data=False,
        )
        summary = result["results"]
        _apply_simulation_3_classification(
            result,
            rep_design,
            rep_dual_dyads,
        )
        contrast = result["artifacts"]["contrast_analysis"]
        assignments = result["artifacts"]["cluster_assignments"]
        validation = _validate_ghost_clusters(
            rep_registry,
            rep_states,
            rep_design,
            contrast,
            assignments,
            rep_dual_dyads,
        )
        recovery = validation["recovery"]
        mas_val = validation.get("mas_validation")
        primary = validation.get("primary_ghost")
        identified_rate = (
            primary.get("within_identified_rate") if primary is not None else None
        )
        identified_comparable = (
            primary.get("within_identified_comparable_dyads")
            if primary is not None
            else None
        )
        identified_unavailable = (
            primary.get("within_identified_unavailable_dyads")
            if primary is not None
            else None
        )
        reliability_rows.append(
            {
                "replicate": replicate,
                "sample_seed": sample_seed,
                "ghost_found": summary["ghost_cluster_found"],
                "clusters": summary["clusters_detected"],
                "noise_models": summary["noise_count"],
                "ghost_precision": recovery["ghost_precision"],
                "ghost_recall": recovery["ghost_recall"],
                "exact_recovery": recovery["exact_recovery"],
                "within_ghost_mas": (
                    mas_val["within_ghost"]["mas_compatibility_rate"]
                    if mas_val
                    else None
                ),
                "ghost_to_reference_mas": (
                    mas_val["ghost_to_reference"]["mas_compatibility_rate"]
                    if mas_val
                    else None
                ),
                "ghost_to_mainstream_mas": (
                    mas_val["ghost_to_mainstream"]["mas_compatibility_rate"]
                    if mas_val
                    else None
                ),
                "reference_to_ghost_mas": (
                    mas_val["reference_to_ghost"]["mas_compatibility_rate"]
                    if mas_val
                    else None
                ),
                "mainstream_to_ghost_mas": (
                    mas_val["mainstream_to_ghost"]["mas_compatibility_rate"]
                    if mas_val
                    else None
                ),
                "within_mainstream_mas": (
                    mas_val["within_mainstream"]["mas_compatibility_rate"]
                    if mas_val
                    else None
                ),
                "within_ghost_mas_comparable_dyads": (
                    mas_val["within_ghost"]["n_comparable"] if mas_val else None
                ),
                "within_ghost_mas_unavailable_dyads": (
                    mas_val["within_ghost"]["n_unavailable"] if mas_val else None
                ),
                "within_ghost_identified": identified_rate,
                "within_ghost_identified_comparable_dyads": identified_comparable,
                "within_ghost_identified_unavailable_dyads": identified_unavailable,
                "ghost_to_reference_mas_comparable_dyads": (
                    mas_val["ghost_to_reference"]["n_comparable"] if mas_val else None
                ),
                "ghost_to_reference_mas_unavailable_dyads": (
                    mas_val["ghost_to_reference"]["n_unavailable"] if mas_val else None
                ),
                "ghost_to_mainstream_mas_comparable_dyads": (
                    mas_val["ghost_to_mainstream"]["n_comparable"] if mas_val else None
                ),
                "ghost_to_mainstream_mas_unavailable_dyads": (
                    mas_val["ghost_to_mainstream"]["n_unavailable"] if mas_val else None
                ),
                "reference_to_ghost_mas_comparable_dyads": (
                    mas_val["reference_to_ghost"]["n_comparable"] if mas_val else None
                ),
                "reference_to_ghost_mas_unavailable_dyads": (
                    mas_val["reference_to_ghost"]["n_unavailable"] if mas_val else None
                ),
                "mainstream_to_ghost_mas_comparable_dyads": (
                    mas_val["mainstream_to_ghost"]["n_comparable"] if mas_val else None
                ),
                "mainstream_to_ghost_mas_unavailable_dyads": (
                    mas_val["mainstream_to_ghost"]["n_unavailable"] if mas_val else None
                ),
                "within_mainstream_mas_comparable_dyads": (
                    mas_val["within_mainstream"]["n_comparable"] if mas_val else None
                ),
                "within_mainstream_mas_unavailable_dyads": (
                    mas_val["within_mainstream"]["n_unavailable"] if mas_val else None
                ),
                "analysis_role": "primary_eps_reliability",
                "sensitivity_axis": "eps",
                "eps": GHOST_PRIMARY_EPS,
                "min_samples": GHOST_PRIMARY_MIN_SAMPLES,
                **_ghost_audit_columns(rep_design, validation.get("audit", {})),
                "gate_passed": validation["acceptance_gate"]["passed"],
                "mainstream_cluster_contains_reference": validation[
                    "mainstream_cluster_contains_reference"
                ],
                "mainstream_cluster_size": validation["mainstream_cluster_size"],
            }
        )
    reliability_df = pd.DataFrame(reliability_rows)
    reliability_provenance = "computed"

    # Reliability summary with predeclared gates.
    reliability_summary = _ghost_reliability_summary(
        reliability_df, reliability_provenance
    )
    runs.append({"reliability_summary": reliability_summary})
    metadata_design = {
        **{k: v for k, v in design.items() if k != "family_by_model"},
        "prototype_mas_contract": prototype_mas,
        "design_hash": design["design_hash"],
        "primary_eps": GHOST_PRIMARY_EPS,
        "primary_min_samples": GHOST_PRIMARY_MIN_SAMPLES,
        "eps_sweep": list(GHOST_EPS_SWEEP),
        # Machine-readable provenance is always "computed": stale reliability
        # artifacts are never accepted as evidence for this design.
        "reliability_provenance": reliability_provenance,
        "identified_compatibility_threshold": {
            "value": GHOST_IDENTIFIED_HIGH_THRESHOLD,
            "note": GHOST_IDENTIFIED_THRESHOLD_NOTE,
        },
        "figure_cluster_qualification_summary": _qualification_summary(
            figure_qualification
        ),
    }
    return (
        runs,
        pd.DataFrame(rows),
        reliability_df,
        metadata_design,
        reliability_summary,
        seed42_validation,
        qualification_df,
        qualification,
        figure_qualification,
        strict_showcase,
    )


def _ghost_secondary_summary(runs):
    secondary = next(
        (
            item
            for item in runs
            if item.get("analysis_role") == "secondary_min_samples_sensitivity"
        ),
        None,
    )
    if secondary is None:
        raise RuntimeError("Missing seed-42 min_samples=10 sensitivity diagnostic")
    result = secondary.get("run", {}).get("results", {})
    validation = secondary.get("validation", {})
    return {
        "analysis_role": secondary["analysis_role"],
        "sensitivity_axis": secondary["sensitivity_axis"],
        "configuration": secondary["configuration"],
        "seed": secondary["seed"],
        "eps": secondary["eps"],
        "min_samples": secondary["min_samples"],
        "controls_primary_gate": secondary["controls_primary_gate"],
        "controls_reliability_gate": secondary["controls_reliability_gate"],
        "results": result,
        "recovery": secondary["recovery"],
        "audit": validation.get("audit"),
        "acceptance_gate": validation.get("acceptance_gate"),
    }


# ── Figures 2 and 3 ───────────────────────────────────────────────────────────
def _figure_crux(summary: pd.DataFrame) -> None:
    fig, axes = plt.subplots(1, 2, figsize=(11.5, 4.4), constrained_layout=True)
    claims = list(summary["claim"].drop_duplicates())
    x = np.arange(len(claims))
    width = 0.38
    metric_labels = {
        "similarity_rate": "Structural similarity",
        "mas_compatible": "MAS compatibility",
    }
    metrics = ["similarity_rate", "mas_compatible"]
    for index, metric in enumerate(metrics):
        subset = summary[summary.metric == metric].set_index("claim").loc[claims]
        offset = (index - 0.5) * width
        color = PALETTE[metric]
        axes[0].bar(
            x + offset,
            subset.delta_u,
            width,
            color=color,
            label=metric_labels[metric],
        )
    axes[0].set_xticks(x, claims, rotation=20, ha="right")
    axes[0].set_ylabel("Delta-U")
    axes[0].set_title(
        "Criterion-specific return from resolving a claim\n"
        "(structural result is a four-way tie)"
    )
    axes[0].legend(frameon=False, fontsize=8)

    top_rows = summary[summary["rank"] == 1].set_index("metric")
    x_metric = np.arange(len(metrics))
    axes[1].bar(
        x_metric - width / 2,
        top_rows.loc[metrics, "baseline"],
        width,
        color=PALETTE["baseline"],
        label="Full multiverse",
    )
    axes[1].bar(
        x_metric + width / 2,
        top_rows.loc[metrics, "post_resolution"],
        width,
        color=PALETTE["post"],
        label="Best post-resolution value",
    )
    axes[1].set_xticks(x_metric, [metric_labels[m] for m in metrics])
    axes[1].set_ylim(0, 1.02)
    axes[1].set_ylabel("Mean pairwise score")
    axes[1].set_title("Compatibility before and after resolution")
    axes[1].legend(frameon=False, fontsize=8)
    fig.savefig(FIGURES_DIR / "figure_2_phase_transition.png", dpi=320)
    fig.savefig(FIGURES_DIR / "figure_2_phase_transition.svg")
    plt.close(fig)


# Figure C UMAP encoding (redesigned): marker shape denotes the
# structural/contrast label -- mainstream circle, ghost diamond, fragmented
# upward triangle, noise x -- while color denotes the DBSCAN cluster identity
# (noise is not a cluster and stays gray).  The legend shows only the four
# shape meanings in a neutral dark marker color, with no cluster colors and no
# per-cluster entries.  Structural labels are contrast labels, not planted
# family labels.
FIGURE_SHAPE_BY_LABEL = {
    "mainstream": "o",
    "ghost": "D",
    "fragmented": "^",
    "unqualified": "v",
    "noise": "x",
}
FIGURE_LEGEND_LABELS = ("mainstream", "ghost", "fragmented", "unqualified", "noise")
FIGURE_LEGEND_COLOR = "#2B2B2B"


def _figure_cluster_palette(cluster_ids):
    """Distinct categorical tab20 colors in the stable natural cluster order.

    One color per actual non-noise DBSCAN cluster id, assigned in sorted
    natural ordering, so the mapping is stable across
    reruns.  tab20 is colorblind reasonable and contains none of the
    verifier-banned colors.
    """
    return {
        cid: matplotlib.colormaps["tab20"](index)
        for index, cid in enumerate(sorted(cluster_ids))
    }


def _figure_ghost(summary: pd.DataFrame, runs: list[dict]) -> None:
    fig, axes = plt.subplots(1, 2, figsize=(11.5, 4.8), constrained_layout=True)
    axes[0].plot(
        summary.eps,
        summary.clusters,
        marker="o",
        linewidth=2,
        color=PALETTE["similarity_rate"],
        label="Clusters",
    )
    axes[0].plot(
        summary.eps,
        summary.noise_models,
        marker="s",
        linewidth=2,
        color=PALETTE["baseline"],
        label="Unassigned models",
    )
    axes[0].plot(
        summary.eps,
        summary.selected_primary_cluster_size,
        marker="D",
        linewidth=2,
        color=PALETTE["mas_compatible"],
        label="Models in ghost cluster",
    )
    for eps, cluster_size in zip(summary.eps, summary.selected_primary_cluster_size):
        axes[0].annotate(
            str(int(cluster_size)),
            (eps, cluster_size),
            xytext=(0, 5),
            textcoords="offset points",
            ha="center",
            va="bottom",
            fontsize=7,
        )
    axes[0].set_xlabel("Neighbourhood radius (epsilon)")
    axes[0].set_ylabel("Number of models")
    axes[0].set_title("Sensitivity to the neighbourhood radius")
    axes[0].legend(frameon=False, fontsize=8)

    # UMAP panel: the figure-config eps=0.35 run from the primary sweep.
    # Selection is by the sweep configuration role, and the title derives from
    # the row's own eps and cluster counts, so the panel tracks eps=0.35
    # without depending on a primary/reference name.  The title is kept to a
    # single concise line so it renders unclipped at any sweep radius.
    #
    # Encoding (Figure C redesign): color encodes the DBSCAN cluster identity
    # -- each actual non-noise cluster gets its own categorical color in the
    # stable natural cluster-id ordering (tab20, colorblind
    # reasonable, none verifier-banned) -- while marker shape encodes the
    # structural/contrast label: mainstream circle, ghost diamond, fragmented
    # upward triangle, noise x.  Noise is not a cluster and keeps the neutral
    # noise gray.  The legend is manual, shows only the four shape meanings in
    # a neutral dark marker color, and carries no cluster colors and no
    # per-cluster entries.
    figure_run = next(
        item for item in runs if item["configuration"]["configuration"] == "figure"
    )
    figure_row = summary[summary["configuration"] == "figure"].iloc[0]
    figure_artifacts = figure_run["run"]["artifacts"]
    embedding = figure_artifacts["embedding_2d"]
    assignments = {
        row["model_id"]: row.get("cluster_id") or "noise"
        for row in figure_artifacts["cluster_assignments"]
    }
    contrast = {
        row["cluster_id"]: row["label"] for row in figure_artifacts["contrast_analysis"]
    }
    model_ids = embedding["model_ids"]
    cluster_ids = sorted({cid for cid in assignments.values() if cid != "noise"})
    cluster_colors = _figure_cluster_palette(cluster_ids)
    x_all = np.asarray(embedding["x"])
    y_all = np.asarray(embedding["y"])
    for cluster_id in cluster_ids:
        label = contrast.get(cluster_id)
        if label not in FIGURE_SHAPE_BY_LABEL:
            raise RuntimeError(
                f"Figure-eps cluster {cluster_id} has an unexpected "
                f"structural label: {label!r}"
            )
        mask = np.array([assignments[mid] == cluster_id for mid in model_ids])
        axes[1].scatter(
            x_all[mask],
            y_all[mask],
            s=34,
            alpha=0.85,
            color=cluster_colors[cluster_id],
            marker=FIGURE_SHAPE_BY_LABEL[label],
            edgecolor="#2B2B2B",
            linewidth=0.3,
        )
    noise_mask = np.array([assignments[mid] == "noise" for mid in model_ids])
    if noise_mask.any():
        # The x marker is unfilled, so it takes the marker color directly (no
        # edgecolor is meaningful for it).
        axes[1].scatter(
            x_all[noise_mask],
            y_all[noise_mask],
            s=34,
            alpha=0.85,
            color=PALETTE["noise"],
            marker=FIGURE_SHAPE_BY_LABEL["noise"],
        )
    if GHOST_REFERENCE_MODEL not in model_ids:
        raise RuntimeError(
            f"Figure-eps embedding is missing reference model {GHOST_REFERENCE_MODEL}"
        )
    reference_index = model_ids.index(GHOST_REFERENCE_MODEL)
    axes[1].scatter(
        [x_all[reference_index]],
        [y_all[reference_index]],
        s=120,
        marker="*",
        color="#FFFFFF",
        edgecolor="#2B2B2B",
        linewidth=1.0,
        label="Reference theory",
        zorder=5,
    )
    axes[1].set_xlabel("UMAP dimension 1")
    axes[1].set_ylabel("UMAP dimension 2")
    axes[1].set_title(
        f"Structural clustering at eps = {figure_row['eps']:.2f} "
        f"({int(figure_row['clusters'])} clusters)",
        pad=10,
    )
    legend_handles = [
        plt.Line2D(
            [0],
            [0],
            marker=FIGURE_SHAPE_BY_LABEL[label],
            linestyle="None",
            color=FIGURE_LEGEND_COLOR,
            markerfacecolor=FIGURE_LEGEND_COLOR,
            markersize=7,
            label=label,
        )
        for label in FIGURE_LEGEND_LABELS
    ]
    axes[1].legend(
        handles=legend_handles
        + [
            plt.Line2D(
                [0],
                [0],
                marker="*",
                linestyle="None",
                color="#2B2B2B",
                markerfacecolor="#FFFFFF",
                markersize=9,
                label="Reference theory",
            )
        ],
        frameon=False,
        fontsize=8,
    )

    fig.savefig(FIGURES_DIR / "figure_3_ghost_discovery.png", dpi=600)
    fig.savefig(FIGURES_DIR / "figure_3_ghost_discovery.svg")
    plt.close(fig)


# ── transactional staging and manifest ────────────────────────────────────────
RESULT_ARTIFACTS = [
    "simulation_1a_consensus_results.json",
    "simulation_1a_consensus_summary.csv",
    "simulation_1a_consensus_sampling_summary.csv",
    "simulation_1a_consensus_sampling_reliability.csv",
    "simulation_1b_consensus_results.json",
    "simulation_1b_consensus_summary.csv",
    "simulation_1b_consensus_sampling_summary.csv",
    "simulation_1b_consensus_sampling_reliability.csv",
    "simulation_1_consensus_summary.csv",
    "simulation_1_consensus_baseline.json",
    "simulation_2_crux_results.json",
    "simulation_2_crux_summary.csv",
    "simulation_3_ghost_results.json",
    "simulation_3_ghost_summary.csv",
    "simulation_3_ghost_reliability.csv",
    "simulation_3_ghost_cluster_qualification.csv",
]
FIGURE_ARTIFACTS = [
    "figure_2_phase_transition.png",
    "figure_2_phase_transition.svg",
    "figure_3_ghost_discovery.png",
    "figure_3_ghost_discovery.svg",
]
METADATA_ARTIFACT = "results/simulation_metadata.json"
EXPECTED_ARTIFACTS = [
    *(f"results/{name}" for name in RESULT_ARTIFACTS),
    METADATA_ARTIFACT,
    *(f"figures/{name}" for name in FIGURE_ARTIFACTS),
]
MANIFEST_ARTIFACT = "manifest.json"
OBSOLETE_ARTIFACTS = [
    # Previous manuscript names are removed if a local checkout still has
    # them.  None of these names is generated by the active workflow.
    "results/simulation_A1_results.json",
    "results/simulation_A1_summary.csv",
    "results/simulation_A1_sampling_summary.csv",
    "results/simulation_A1_sampling_reliability.csv",
    "results/simulation_A2_results.json",
    "results/simulation_A2_summary.csv",
    "results/simulation_A2_sampling_summary.csv",
    "results/simulation_A2_sampling_reliability.csv",
    "results/simulation_A_precision_summary.csv",
    "results/simulation_A_baseline.json",
    "results/simulation_B_crux_results.json",
    "results/simulation_B_crux_summary.csv",
    "results/simulation_C_ghost_results.json",
    "results/simulation_C_ghost_summary.csv",
    "results/simulation_C_ghost_reliability.csv",
    "results/simulation_C_ghost_cluster_qualification.csv",
    "results/simulation_D_sampling_results.json",
    "results/simulation_D_sampling_summary.csv",
    "results/simulation_D_sampling_reliability.csv",
    "figures/fig_A_metric_contrast.png",
    "figures/fig_A_metric_contrast.svg",
    "figures/fig_B_phase_transition.png",
    "figures/fig_B_phase_transition.svg",
    "figures/fig_C_ghost_discovery.png",
    "figures/fig_C_ghost_discovery.svg",
    "figures/fig_D_sampling_reliability.png",
    "figures/fig_D_sampling_reliability.svg",
]


def _package_versions(extra=()):
    names = (
        "torch",
        "fastapi",
        "rpy2",
        "pandas",
        "matplotlib",
        "umap-learn",
        "scikit-learn",
        "numpy",
        "scipy",
        "numba",
        "pynndescent",
        *extra,
    )
    versions = {}
    for name in names:
        try:
            versions[name] = importlib.metadata.version(name)
        except importlib.metadata.PackageNotFoundError:
            versions[name] = None
    return versions


def _r_versions():
    info = {}
    try:
        out = subprocess.run(
            [
                "Rscript",
                "-e",
                'cat(R.version.string,"\\n"); '
                'for (p in c("dagitty","causaleffect")) cat(p,":",as.character(packageVersion(p)),"\\n")',
            ],
            capture_output=True,
            text=True,
            timeout=30,
        )
        info["raw"] = out.stdout.strip()
    except Exception as exc:  # noqa: BLE001
        info["error"] = repr(exc)
    return info


def _git_status():
    info = {}
    try:
        info["commit"] = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            cwd=REPO,
        ).stdout.strip()
        dirty = subprocess.run(
            ["git", "status", "--porcelain"],
            capture_output=True,
            text=True,
            cwd=REPO,
        ).stdout.strip()
        info["dirty"] = bool(dirty)
        info["changed_file_count"] = len([line for line in dirty.splitlines() if line])
    except Exception as exc:  # noqa: BLE001
        info["error"] = repr(exc)
    return info


def _relative_repo_path(path: Path) -> str:
    """Return a portable source path relative to the repository root."""
    return path.resolve().relative_to(REPO).as_posix()


_SOURCE_EXCLUDED_PARTS = frozenset(
    {
        "test",
        "tests",
        "__pycache__",
        ".pytest_cache",
        ".mypy_cache",
        ".ruff_cache",
        ".tox",
        ".nox",
    }
)


def _is_production_python(path: Path, python_root: Path) -> bool:
    relative = path.relative_to(python_root)
    directory_parts = {part.lower() for part in relative.parts[:-1]}
    filename = path.name.lower()
    return not (
        directory_parts & _SOURCE_EXCLUDED_PARTS
        or filename in {"test.py", "tests.py"}
        or filename.startswith("test_")
        or filename.endswith("_test.py")
    )


def _workflow_source_paths() -> list[Path]:
    python_root = REPO / "inst" / "python"
    candidates = [
        path
        for path in python_root.rglob("*.py")
        if _is_production_python(path, python_root)
    ]
    candidates.extend(
        [
            REPO / "simulations" / "scripts" / "run_simulations.py",
            REPO / "simulations" / "scripts" / "verify_outputs.py",
            REPO / "simulations" / "requirements.in",
            REPO / "simulations" / "requirements.lock.txt",
        ]
    )
    by_key = {}
    for path in candidates:
        if not path.is_file():
            raise RuntimeError(f"Missing source inventory file: {path}")
        key = _relative_repo_path(path)
        if key in by_key:
            raise RuntimeError(f"Duplicate source inventory path: {key}")
        by_key[key] = path
    return [by_key[key] for key in sorted(by_key)]


def main() -> None:
    _style()
    staging = SIMULATIONS_DIR / ".staging"
    if staging.exists():
        shutil.rmtree(staging)
    stage_results = staging / "results"
    stage_figures = staging / "figures"
    stage_results.mkdir(parents=True)
    stage_figures.mkdir()

    _dump(
        stage_results / "simulation_1_consensus_baseline.json",
        CONSENSUS_BASELINE_PROVENANCE,
    )

    run_a, _reg_a, _states_a, summary_a, sampling_a, rel_a = _run_consensus_design(
        "mas_compatible"
    )
    run_b, _reg_b, _states_b, summary_b, sampling_b, rel_b = _run_consensus_design(
        "identified_compatible"
    )
    combined = _consensus_combined_dataframe(summary_a, summary_b)
    _dump(stage_results / "simulation_1a_consensus_results.json", run_a)
    _dump(stage_results / "simulation_1b_consensus_results.json", run_b)
    summary_a.to_csv(stage_results / "simulation_1a_consensus_summary.csv", index=False)
    summary_b.to_csv(stage_results / "simulation_1b_consensus_summary.csv", index=False)
    sampling_a.to_csv(
        stage_results / "simulation_1a_consensus_sampling_summary.csv", index=False
    )
    sampling_b.to_csv(
        stage_results / "simulation_1b_consensus_sampling_summary.csv", index=False
    )
    rel_a.to_csv(
        stage_results / "simulation_1a_consensus_sampling_reliability.csv", index=False
    )
    rel_b.to_csv(
        stage_results / "simulation_1b_consensus_sampling_reliability.csv", index=False
    )
    combined.to_csv(stage_results / "simulation_1_consensus_summary.csv", index=False)

    crux_runs, crux_summary = _run_crux()
    _dump(stage_results / "simulation_2_crux_results.json", crux_runs)
    crux_summary.to_csv(stage_results / "simulation_2_crux_summary.csv", index=False)

    (
        ghost_runs,
        ghost_summary,
        ghost_reliability,
        ghost_design,
        ghost_reliability_summary,
        seed42_validation,
        ghost_qualification_df,
        ghost_qualification,
        ghost_figure_qualification,
        ghost_strict_showcase,
    ) = _run_ghost()
    ghost_secondary = _ghost_secondary_summary(ghost_runs)
    _dump(
        stage_results / "simulation_3_ghost_results.json",
        {
            "runs": ghost_runs,
            "design": ghost_design,
            "reliability_summary": ghost_reliability_summary,
            "seed42_validation": seed42_validation,
            "secondary_sensitivity": ghost_secondary,
            "cluster_qualification": ghost_qualification,
            "figure_cluster_qualification": ghost_figure_qualification,
            "strict_showcase": ghost_strict_showcase,
        },
    )
    ghost_summary.to_csv(stage_results / "simulation_3_ghost_summary.csv", index=False)
    ghost_reliability.to_csv(
        stage_results / "simulation_3_ghost_reliability.csv", index=False
    )
    ghost_qualification_df.to_csv(
        stage_results / "simulation_3_ghost_cluster_qualification.csv", index=False
    )

    # Figures are written to their staged directory.  No figure is generated
    # for Simulation 1; the active workflow retains only Figures 2 and 3.
    original_figures_dir = FIGURES_DIR
    globals()["FIGURES_DIR"] = stage_figures
    try:
        _figure_crux(crux_summary)
        _figure_ghost(ghost_summary, ghost_runs)
    finally:
        globals()["FIGURES_DIR"] = original_figures_dir

    # Metadata
    artifact_hashes = {}
    for relative in EXPECTED_ARTIFACTS:
        if relative == METADATA_ARTIFACT:
            continue
        path = staging / relative
        if not path.exists():
            raise RuntimeError(f"Missing staged artifact: {relative}")
        artifact_hashes[relative] = _sha256(path)
    source_paths = _workflow_source_paths()
    source_keys = [_relative_repo_path(path) for path in source_paths]
    if source_keys != sorted(source_keys) or len(source_keys) != len(set(source_keys)):
        raise RuntimeError("Source inventory must be unique and sorted")
    source_hashes = {key: _sha256(path) for key, path in zip(source_keys, source_paths)}
    metadata = {
        "workflow": "three_simulation_reproducible",
        "active_simulations": ["consensus_illusion", "crux", "ghost"],
        "active_figures": [
            "figure_2_phase_transition",
            "figure_3_ghost_discovery",
        ],
        "manifest": MANIFEST_ARTIFACT,
        "baseline_provenance": "results/simulation_1_consensus_baseline.json",
        "seed": SEED,
        "construction": "seeded theories -> timing-directed registries -> stratified or exhaustive finite multiverses",
        "threshold_policy": "disabled; fixed configurations; no acceptance retries",
        "identified_compatibility_semantic_version": "0.3.0",
        "identified_compatibility_definition": (
            "both models satisfy fixed-direct exposure -> outcome "
            "complete-conditioning d-separation after removing only the "
            "mandatory direct edge, conditioning on every other declared "
            "present node, and have exactly equal conditioning node sets"
        ),
        "simulation_3": {
            "exposure": GHOST_EXPOSURE,
            "outcome": GHOST_OUTCOME,
            "node_order": GHOST_NODE_ORDER,
            "prototype_edges": ghost_design.get("prototype_edges"),
            "prototype_mas_contract": ghost_design.get("prototype_mas_contract"),
            "family_sizes": ghost_design.get("family_sizes"),
            "family_counts": ghost_design.get("family_counts"),
            "total_models": ghost_design.get("total_models"),
            "unique_models": ghost_design.get("unique_models"),
            "unique_model_count": ghost_design.get("unique_model_count"),
            "seeded_family_names": ghost_design.get("seeded_family_names"),
            "seeded_family_total": ghost_design.get("seeded_family_total"),
            "heterogeneous_model_count": ghost_design.get("heterogeneous_model_count"),
            "heterogeneous_to_seeded_ratio": ghost_design.get(
                "heterogeneous_to_seeded_ratio"
            ),
            "reference_model_id": ghost_design.get("reference_model_id"),
            "reference_model_family": ghost_design.get("reference_model_family"),
            "fixed_causal_edges": ghost_design.get("fixed_causal_edges"),
            "primary_eps": GHOST_PRIMARY_EPS,
            "primary_min_samples": GHOST_PRIMARY_MIN_SAMPLES,
            "eps_sweep": list(GHOST_EPS_SWEEP),
            "reliability_seeds": ghost_reliability_summary.get("sample_seeds"),
            "reliability_provenance": ghost_design.get("reliability_provenance"),
            "protected_edges": {
                f"{s}->{t}": v for (s, t), v in PROTECTED_EDGES.items()
            },
            "perturbation_probability": FLIP_PROBABILITY,
            "heterogeneous_causal_probability": HETEROGENEOUS_CAUSAL_PROBABILITY,
            "duplicate_policy": DUPLICATE_POLICY,
            "design_hash": ghost_design.get("design_hash"),
            "seed42_gate_passed": (seed42_validation or {})
            .get("acceptance_gate", {})
            .get("passed"),
            "seed42_acceptance_gate": (seed42_validation or {}).get("acceptance_gate"),
            "seed42_audit": (seed42_validation or {}).get("audit"),
            "strict_showcase": ghost_strict_showcase,
            "secondary_sensitivity": ghost_secondary,
            "figure_cluster_qualification_summary": ghost_design.get(
                "figure_cluster_qualification_summary"
            ),
            "qualification_thresholds": ghost_qualification.get(
                "qualification_thresholds"
            ),
            "identified_compatibility_threshold": {
                "value": GHOST_IDENTIFIED_HIGH_THRESHOLD,
                "note": GHOST_IDENTIFIED_THRESHOLD_NOTE,
            },
        },
        "reliability_gates": {
            "structural_detection": ghost_reliability_summary[
                "structural_detection_gate"
            ],
            "joint": ghost_reliability_summary["joint_gate"],
            "metrics": ghost_reliability_summary["gate_metrics"],
        },
        "python": platform.python_version(),
        "platform": platform.platform(),
        "ld_preload": os.environ.get("LD_PRELOAD"),
        "packages": _package_versions(),
        "r": _r_versions(),
        "git": _git_status(),
        "source_hashes": source_hashes,
        "artifact_hashes": artifact_hashes,
    }
    _dump(stage_results / "simulation_metadata.json", metadata)

    # Validate the staged tree and generate a path-portable manifest.  The
    # manifest includes the metadata hash, while metadata intentionally omits
    # its own hash to avoid a circular digest.
    all_artifact_hashes = {
        relative: _sha256(staging / relative) for relative in EXPECTED_ARTIFACTS
    }
    manifest = {
        "version": 1,
        "workflow": "three_simulation_reproducible",
        "artifacts": all_artifact_hashes,
        "sources": source_hashes,
        "metadata": {
            "path": METADATA_ARTIFACT,
            "seed": SEED,
            "active_simulations": ["consensus_illusion", "crux", "ghost"],
            "active_figures": [
                "figure_2_phase_transition",
                "figure_3_ghost_discovery",
            ],
        },
    }
    _dump(staging / MANIFEST_ARTIFACT, manifest)
    expected_staged = set(EXPECTED_ARTIFACTS) | {MANIFEST_ARTIFACT}
    produced = {
        path.relative_to(staging).as_posix()
        for path in staging.rglob("*")
        if path.is_file()
    }
    if produced != expected_staged:
        raise RuntimeError(
            "Staging contains an unexpected artifact set: "
            f"missing={sorted(expected_staged - produced)}, "
            f"extra={sorted(produced - expected_staged)}"
        )

    # Promote with rollback. Individual renames are atomic on this filesystem;
    # the backup restores the prior complete set if any replacement fails.
    backup = SIMULATIONS_DIR / ".backup"
    if backup.exists():
        shutil.rmtree(backup)
    backup.mkdir(parents=True)
    target_names = set(produced) | set(OBSOLETE_ARTIFACTS)
    previously_present = {
        relative for relative in target_names if (SIMULATIONS_DIR / relative).exists()
    }
    for relative in previously_present:
        backup_path = backup / relative
        backup_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(SIMULATIONS_DIR / relative, backup_path)
    try:
        for relative in sorted(produced):
            target = SIMULATIONS_DIR / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            os.replace(staging / relative, target)
        for relative in OBSOLETE_ARTIFACTS:
            old = SIMULATIONS_DIR / relative
            if old.exists():
                old.unlink()
    except BaseException:
        for relative in target_names - previously_present:
            target = SIMULATIONS_DIR / relative
            if target.exists():
                target.unlink()
        for relative in previously_present:
            target = SIMULATIONS_DIR / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            os.replace(backup / relative, target)
        raise
    finally:
        shutil.rmtree(backup, ignore_errors=True)
        shutil.rmtree(staging, ignore_errors=True)


if __name__ == "__main__":
    main()
