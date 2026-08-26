"""Verify the active reproducible simulation artifacts.

The verifier is independent of the caller's working directory and does not
read manuscript, Markdown, or DOCX files.  All paths in metadata and the
manifest are relative to ``simulations/``.
"""

from __future__ import annotations

import json
import hashlib
import math
import sys
from pathlib import Path

import pandas as pd
from PIL import Image

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
HERE = RESULTS_DIR

ERRORS: list[str] = []
FIGURES = [
    "figure_2_phase_transition",
    "figure_3_ghost_discovery",
]
EXPECTED_ARTIFACTS = {
    *(
        f"results/{name}"
        for name in (
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
            "simulation_metadata.json",
        )
    ),
    *(f"figures/{base}.{ext}" for base in FIGURES for ext in ("png", "svg")),
}


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


def _source_inventory_paths() -> list[Path]:
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


SOURCE_PATHS = {_relative_repo_path(path): path for path in _source_inventory_paths()}
SIMULATION_3_FAMILY_SIZES = {
    "mainstream": 30,
    "ghost": 12,
    "heterogeneous": 158,
}
SIMULATION_3_TOTAL_MODELS = sum(SIMULATION_3_FAMILY_SIZES.values())
SIMULATION_3_SEEDED_FAMILY_TOTAL = sum(
    SIMULATION_3_FAMILY_SIZES[family] for family in ("mainstream", "ghost")
)
SIMULATION_3_HETEROGENEOUS_TO_SEEDED_RATIO = round(
    SIMULATION_3_FAMILY_SIZES["heterogeneous"] / SIMULATION_3_SEEDED_FAMILY_TOTAL,
    6,
)
SIMULATION_3_PRIMARY_EPS = 0.5
SIMULATION_3_EPS_SWEEP = (0.2, 0.35, 0.5)
SIMULATION_3_STRICT_EPS = SIMULATION_3_EPS_SWEEP[0]
SIMULATION_3_FIGURE_EPS = SIMULATION_3_EPS_SWEEP[1]
SIMULATION_3_REFERENCE_EPS = SIMULATION_3_EPS_SWEEP[2]
# The seed-42 validation and its acceptance gate run on the primary/reference
# eps (0.50), which is now also the highest swept radius; the per-cluster
# qualification audit runs on the same reference eps (0.50) and carries its
# own self-contained excluded-noise audit.
SIMULATION_3_QUALIFICATION_EPS = SIMULATION_3_REFERENCE_EPS
# The strict eps=0.20 seed-42 showcase requires the selected primary ghost
# cluster to under-recover the planted ghost family: fewer than the 12
# planted ghost models recovered by the selected primary (recall < 1).  The
# total number of structurally ghost-labeled models is recorded for audit
# but is NOT required to stay below the planted count, because heterogeneous
# false-positive ghost clusters can legitimately push that total above 12.
# The showcase is separate from the primary eps=0.50 acceptance gate and
# from the 100-replicate reliability gates.
SIMULATION_3_PLANTED_GHOST_COUNT = SIMULATION_3_FAMILY_SIZES["ghost"]
SIMULATION_3_CONFIGURATIONS = {"strict", "figure", "reference"}
SIMULATION_3_INTERNAL_THRESHOLD = 0.60
SIMULATION_3_WITHIN_MAS_THRESHOLD = 0.60
SIMULATION_3_WITHIN_IDENTIFIED_THRESHOLD = 0.60
SIMULATION_3_REFERENCE_SIMILARITY_THRESHOLD = 0.50
SIMULATION_3_REFERENCE_MAS_THRESHOLD = 0.50
SIMULATION_3_IDENTIFIED_HIGH_THRESHOLD = SIMULATION_3_WITHIN_IDENTIFIED_THRESHOLD
SIMULATION_3_RELIABILITY_GATE_REQUIRED = 75
SIMULATION_3_JOINT_GATE_REQUIRED = 70
SIMULATION_3_GATE_METRICS = (
    "similarity_rate",
    "mas_compatible",
    "identified_compatible",
)
SIMULATION_1B_DESIGN = "forced_conditioning_collider"
SIMULATION_1B_FIXED_EDGES = {
    ("X2", "X1"),
    ("X2", "Y"),
    ("X1", "Y"),
    ("Y", "X6"),
}
SIMULATION_1B_FOCAL_EDGE = ("X1", "X6")
SIMULATION_1B_CONTEXT_EDGES = {
    ("X3", "X4"),
    ("X3", "X5"),
    ("X4", "X5"),
    ("X3", "Y"),
    ("X4", "Y"),
    ("X5", "Y"),
}
SIMULATION_1B_NODE_TIMING = {
    "X2": 1,
    "X1": 2,
    "X3": 3,
    "X4": 4,
    "X5": 5,
    "Y": 6,
    "X6": 7,
}
SIMULATION_3_MAS_COMPARISONS = (
    "within_ghost",
    "ghost_to_reference",
    "ghost_to_mainstream",
    "reference_to_ghost",
    "mainstream_to_ghost",
    "within_mainstream",
)
SIMULATION_3_UNAVAILABLE_MAS_COLUMNS = tuple(
    f"{comparison}_mas_unavailable_dyads" for comparison in SIMULATION_3_MAS_COMPARISONS
)
SIMULATION_3_MAS_RATE_COLUMNS = tuple(
    f"{comparison}_mas" for comparison in SIMULATION_3_MAS_COMPARISONS
)
SIMULATION_3_COMPARABLE_MAS_COLUMNS = tuple(
    f"{comparison}_mas_comparable_dyads" for comparison in SIMULATION_3_MAS_COMPARISONS
)
SIMULATION_3_MAS_EVIDENCE_COLUMNS = (
    *SIMULATION_3_MAS_RATE_COLUMNS,
    *SIMULATION_3_COMPARABLE_MAS_COLUMNS,
    *SIMULATION_3_UNAVAILABLE_MAS_COLUMNS,
)
SIMULATION_3_GATE_COLUMNS = {
    "ghost_found",
    "ghost_precision",
    "ghost_recall",
    "selected_primary_cluster_size",
    "mainstream_cluster_contains_reference",
    "mainstream_cluster_size",
    "within_ghost_mas",
    "within_ghost_identified",
    "within_ghost_identified_comparable_dyads",
    "within_ghost_identified_unavailable_dyads",
    "ghost_to_reference_mas",
    "ghost_to_mainstream_mas",
    "ghost_to_mainstream_mas_comparable_dyads",
    "mainstream_to_ghost_mas_comparable_dyads",
    "gate_passed",
    *SIMULATION_3_MAS_EVIDENCE_COLUMNS,
}
SIMULATION_3_AUDIT_COLUMNS = {
    "family_counts",
    "mainstream_family_count",
    "ghost_family_count",
    "heterogeneous_family_count",
    "seeded_family_total",
    "heterogeneous_to_seeded_ratio",
    "selected_primary_cluster_family_composition",
    "selected_primary_cluster_false_positive_count",
    "dbscan_unassigned_noise_family_composition",
    "dbscan_unassigned_noise_count",
    "dbscan_unassigned_heterogeneous_count",
}
QUALIFICATION_REQUIRED_COLUMNS = {
    "eps",
    "min_samples",
    "cluster_id",
    "label",
    "model_count",
    "mainstream_count",
    "ghost_count",
    "heterogeneous_count",
    "internal_similarity",
    "reference_similarity",
    "internal_coherent",
    "reference_distinct",
    "reference_group_cluster_id",
    "reference_group_size",
    "within_mas_rate",
    "within_mas_comparable_dyads",
    "within_mas_unavailable_dyads",
    "within_identified_rate",
    "within_identified_comparable_dyads",
    "within_identified_unavailable_dyads",
    "high_mas",
    "high_identified",
    "common_gate_passed",
    "required_unavailable_dyads",
    "reference_group_mas_rate",
    "reference_group_mas_comparable_dyads",
    "reference_group_mas_unavailable_dyads",
    "qualifies_as_mainstream",
    "qualifies_as_ghost",
}
QUALIFICATION_UNAVAILABLE_COLUMNS = (
    "within_mas_unavailable_dyads",
    "within_identified_unavailable_dyads",
    "to_reference_mas_unavailable_dyads",
    "to_reference_identified_unavailable_dyads",
    "from_reference_mas_unavailable_dyads",
    "from_reference_identified_unavailable_dyads",
    "to_mainstream_mas_unavailable_dyads",
    "to_mainstream_identified_unavailable_dyads",
    "from_mainstream_mas_unavailable_dyads",
    "from_mainstream_identified_unavailable_dyads",
    "reference_group_mas_unavailable_dyads",
)
QUALIFICATION_METRIC_COLUMN_PREFIXES = (
    "within",
    "to_reference",
    "from_reference",
    "to_mainstream",
    "from_mainstream",
)


def fail(message: str) -> None:
    ERRORS.append(message)


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _ghost_ratio_matches(value) -> bool:
    try:
        return float(value) == SIMULATION_3_HETEROGENEOUS_TO_SEEDED_RATIO
    except (TypeError, ValueError):
        return False


def _verify_ghost_audit(audit, label: str) -> None:
    if not isinstance(audit, dict):
        fail(f"{label} is missing the generated Simulation 3 audit object")
        return
    required = {
        "family_counts",
        "seeded_family_total",
        "heterogeneous_to_seeded_ratio",
        "selected_primary_cluster_family_composition",
        "selected_primary_cluster_false_positive_count",
        "dbscan_unassigned_noise_family_composition",
        "dbscan_unassigned_noise_count",
        "dbscan_unassigned_heterogeneous_count",
    }
    missing = required - set(audit)
    if missing:
        fail(f"{label} is missing Simulation 3 audit fields: {sorted(missing)}")
        return

    if audit["family_counts"] != SIMULATION_3_FAMILY_SIZES:
        fail(f"{label} family counts are wrong: {audit['family_counts']}")
    if audit["seeded_family_total"] != SIMULATION_3_SEEDED_FAMILY_TOTAL:
        fail(f"{label} seeded family total is wrong: {audit['seeded_family_total']}")
    if not _ghost_ratio_matches(audit["heterogeneous_to_seeded_ratio"]):
        fail(
            f"{label} heterogeneous-to-seeded ratio is wrong: "
            f"{audit['heterogeneous_to_seeded_ratio']}"
        )

    primary = audit["selected_primary_cluster_family_composition"]
    if not isinstance(primary, dict) or set(primary) != set(SIMULATION_3_FAMILY_SIZES):
        fail(f"{label} primary cluster family composition is malformed: {primary}")
        return
    primary_size = audit.get("selected_primary_cluster_size")
    if primary_size is not None and primary_size != sum(primary.values()):
        fail(f"{label} primary cluster size does not match its family composition")
    false_positive_count = audit["selected_primary_cluster_false_positive_count"]
    expected_false_positives = primary["mainstream"] + primary["heterogeneous"]
    if false_positive_count != expected_false_positives:
        fail(
            f"{label} primary cluster false-positive count is wrong: "
            f"{false_positive_count} != {expected_false_positives}"
        )
    if audit.get("primary_cluster_family_composition") != primary:
        fail(f"{label} primary cluster audit aliases disagree")
    if audit.get("primary_cluster_false_positive_count") != false_positive_count:
        fail(f"{label} primary cluster false-positive audit aliases disagree")

    noise = audit["dbscan_unassigned_noise_family_composition"]
    if not isinstance(noise, dict) or set(noise) != set(SIMULATION_3_FAMILY_SIZES):
        fail(f"{label} DBSCAN noise family composition is malformed: {noise}")
        return
    noise_count = audit["dbscan_unassigned_noise_count"]
    if noise_count != sum(noise.values()):
        fail(f"{label} DBSCAN noise count does not match its family composition")
    if audit["dbscan_unassigned_heterogeneous_count"] != noise["heterogeneous"]:
        fail(f"{label} DBSCAN heterogeneous-noise audit is inconsistent")
    if audit.get("clustered_heterogeneous_count") is not None and (
        audit["clustered_heterogeneous_count"] + noise["heterogeneous"]
        != SIMULATION_3_FAMILY_SIZES["heterogeneous"]
    ):
        fail(f"{label} heterogeneous planted/noise audit does not balance")


def _parse_mapping(value):
    try:
        parsed = json.loads(value)
    except (TypeError, json.JSONDecodeError):
        return None
    return parsed if isinstance(parsed, dict) else None


def _finite_number(value):
    try:
        if pd.isna(value):
            return None
        number = float(value)
    except (TypeError, ValueError, OverflowError):
        return None
    return number if math.isfinite(number) else None


def _strict_bool(value):
    if pd.api.types.is_bool(value):
        return bool(value)
    if isinstance(value, str):
        normalized = value.strip().lower()
        if normalized == "true":
            return True
        if normalized == "false":
            return False
    return None


def _zero_number(value):
    number = _finite_number(value)
    return number is not None and number == 0


def _at_least(value, threshold):
    number = _finite_number(value)
    return number is not None and number >= threshold


def _missing_value(value):
    try:
        return bool(pd.isna(value))
    except (TypeError, ValueError):
        return False


def _nonnegative_integer(value):
    number = _finite_number(value)
    if number is None or not number.is_integer() or number < 0:
        return None
    return int(number)


def _simulation_3_expected_mas_dyads(row):
    ghost_size = _nonnegative_integer(row.get("selected_primary_cluster_size"))
    mainstream_size = _nonnegative_integer(row.get("mainstream_cluster_size"))
    if ghost_size is None or mainstream_size is None:
        return None
    return {
        "within_ghost": ghost_size * (ghost_size - 1),
        # Simulation 3 compares the selected ghost cluster with the full
        # reference group (the non-noise cluster containing M0001), not just
        # the singleton reference model.
        "ghost_to_reference": ghost_size * mainstream_size,
        "reference_to_ghost": ghost_size * mainstream_size,
        "ghost_to_mainstream": ghost_size * mainstream_size,
        "mainstream_to_ghost": ghost_size * mainstream_size,
        "within_mainstream": mainstream_size * (mainstream_size - 1),
    }


def _simulation_3_mas_denominator_errors(row, ghost_found):
    expected = _simulation_3_expected_mas_dyads(row)
    if expected is None:
        return [
            "selected_primary_cluster_size and mainstream_cluster_size must be "
            "finite non-negative integers for dyad denominator validation"
        ]

    errors = []
    for comparison, expected_count in expected.items():
        comparable_column = f"{comparison}_mas_comparable_dyads"
        unavailable_column = f"{comparison}_mas_unavailable_dyads"
        comparable_raw = row.get(comparable_column)
        unavailable_raw = row.get(unavailable_column)

        # An undetected ghost has no selected ghost cluster, so the driver may
        # legitimately leave all MAS validation fields unavailable.  A partial
        # pair of missing counts is still malformed.
        if (
            ghost_found is False
            and _missing_value(comparable_raw)
            and _missing_value(unavailable_raw)
        ):
            continue

        comparable = _nonnegative_integer(comparable_raw)
        unavailable = _nonnegative_integer(unavailable_raw)
        if comparable is None:
            errors.append(f"{comparable_column} must be a finite non-negative integer")
        if unavailable is None:
            errors.append(f"{unavailable_column} must be a finite non-negative integer")
        if comparable is not None and unavailable is not None:
            observed_count = comparable + unavailable
            if observed_count != expected_count:
                errors.append(
                    f"{comparison} MAS dyads total {observed_count} differs from "
                    f"expected {expected_count} from selected cluster sizes"
                )
    return errors


def _simulation_3_reliability_row_errors(row):
    errors = []
    ghost_found = _strict_bool(row.get("ghost_found"))
    if ghost_found is None:
        errors.append("ghost_found is missing or invalid; expected True or False")

    mainstream_contains_reference = _strict_bool(
        row.get("mainstream_cluster_contains_reference")
    )
    if mainstream_contains_reference is None:
        errors.append(
            "mainstream_cluster_contains_reference is missing or invalid; "
            "expected True or False"
        )

    for column in ("ghost_precision", "ghost_recall"):
        if _finite_number(row.get(column)) is None:
            errors.append(f"{column} must be finite")

    for column in ("selected_primary_cluster_size", "mainstream_cluster_size"):
        if _nonnegative_integer(row.get(column)) is None:
            errors.append(f"{column} must be a finite non-negative integer")
    mainstream_size = _nonnegative_integer(row.get("mainstream_cluster_size"))
    if mainstream_contains_reference is True and mainstream_size == 0:
        errors.append(
            "mainstream_cluster_size must be at least one when the reference "
            "is reported in the mainstream cluster"
        )

    # MAS rates are required for detected ghosts.  For a legitimately
    # undetected ghost, the validation stage does not select a ghost cluster,
    # so its conditional MAS rates may be NaN/missing.
    for column in SIMULATION_3_MAS_RATE_COLUMNS:
        value = row.get(column)
        if _finite_number(value) is None and not (
            ghost_found is False and _missing_value(value)
        ):
            errors.append(
                f"{column} must be finite unless the ghost is legitimately "
                "undetected"
            )

    identified_rate = row.get("within_ghost_identified")
    if _finite_number(identified_rate) is None and not (
        ghost_found is False and _missing_value(identified_rate)
    ):
        errors.append(
            "within_ghost_identified must be finite unless the ghost is "
            "legitimately undetected"
        )
    expected_within = _simulation_3_expected_mas_dyads(row)
    if expected_within is not None:
        comparable = _nonnegative_integer(
            row.get("within_ghost_identified_comparable_dyads")
        )
        unavailable = _nonnegative_integer(
            row.get("within_ghost_identified_unavailable_dyads")
        )
        if (
            ghost_found is False
            and _missing_value(row.get("within_ghost_identified_comparable_dyads"))
            and _missing_value(row.get("within_ghost_identified_unavailable_dyads"))
        ):
            pass
        elif comparable is None or unavailable is None:
            errors.append("identified dyad counts must be finite non-negative integers")
        elif comparable + unavailable != expected_within["within_ghost"]:
            errors.append(
                "within_ghost identified dyad denominator differs from expected "
                f"{expected_within['within_ghost']}"
            )

    errors.extend(_simulation_3_mas_denominator_errors(row, ghost_found is True))
    return errors


def _recompute_simulation_3_gate(row):
    within_ghost = _finite_number(row.get("within_ghost_mas"))
    within_ghost_identified = _finite_number(row.get("within_ghost_identified"))
    ghost_to_mainstream = _finite_number(row.get("ghost_to_mainstream_mas"))
    ghost_to_reference = _finite_number(row.get("ghost_to_reference_mas"))
    mainstream_contains_reference = _strict_bool(
        row.get("mainstream_cluster_contains_reference")
    )
    ghost_found = _strict_bool(row.get("ghost_found"))
    checks = {
        "structural_ghost_found": ghost_found is True,
        "mainstream_cluster_contains_reference": (
            mainstream_contains_reference is True
        ),
        "ghost_cluster_at_least_two_models": _at_least(
            row.get("selected_primary_cluster_size"), 2
        ),
        "no_unavailable_mas_dyads": all(
            _zero_number(row.get(column))
            for column in SIMULATION_3_UNAVAILABLE_MAS_COLUMNS
        ),
        "no_unavailable_identified_dyads": _zero_number(
            row.get("within_ghost_identified_unavailable_dyads")
        ),
        "mas_dyad_denominators_match_cluster_sizes": not bool(
            _simulation_3_mas_denominator_errors(row, ghost_found is True)
        ),
        "within_ghost_mas_gt_0_60": (
            within_ghost is not None
            and within_ghost > SIMULATION_3_WITHIN_MAS_THRESHOLD
        ),
        "within_ghost_identified_gt_0_60": (
            within_ghost_identified is not None
            and within_ghost_identified > SIMULATION_3_WITHIN_IDENTIFIED_THRESHOLD
        ),
        "ghost_to_mainstream_mas_lt_0_50": (
            ghost_to_mainstream is not None
            and ghost_to_mainstream < SIMULATION_3_REFERENCE_MAS_THRESHOLD
        ),
        "ghost_to_reference_mas_lt_0_50": (
            ghost_to_reference is not None
            and ghost_to_reference < SIMULATION_3_REFERENCE_MAS_THRESHOLD
        ),
    }
    return checks, all(checks.values())


def _verify_simulation_3_reliability_gates(frame):
    missing = SIMULATION_3_GATE_COLUMNS - set(frame.columns)
    if missing:
        fail(
            "Simulation 3 reliability is missing gate evidence columns: "
            f"{sorted(missing)}"
        )
        return (
            pd.Series(False, index=frame.index, dtype=bool),
            pd.Series(False, index=frame.index, dtype=bool),
        )

    recomputed = []
    ghost_found = []
    for index, row in frame.iterrows():
        row_label = f"Simulation 3 reliability row {index + 1}"
        for error in _simulation_3_reliability_row_errors(row):
            fail(f"{row_label} {error}")
        _, passed = _recompute_simulation_3_gate(row)
        recomputed.append(passed)
        ghost_value = _strict_bool(row.get("ghost_found"))
        ghost_found.append(ghost_value is True)
        observed = _strict_bool(row.get("gate_passed"))
        if observed is None:
            fail(f"{row_label} gate_passed is missing or invalid")
        elif passed != observed:
            fail(
                f"{row_label} recomputed acceptance gate differs from "
                f"gate_passed: {passed} != {observed}"
            )

    return (
        pd.Series(recomputed, index=frame.index, dtype=bool),
        pd.Series(ghost_found, index=frame.index, dtype=bool),
    )


def _integer_values(series):
    values = []
    for value in series:
        number = _finite_number(value)
        if number is None or not number.is_integer():
            return None
        values.append(int(number))
    return values


def _rounded_mean(values):
    numeric = values.map(_finite_number).dropna()
    if numeric.empty:
        return None
    return round(float(numeric.mean()), 6)


def _pooled_qualification_rate(row, prefixes, metric):
    weighted = []
    for prefix in prefixes:
        rate = _finite_number(row.get(f"{prefix}_{metric}_rate"))
        comparable = _nonnegative_integer(
            row.get(f"{prefix}_{metric}_comparable_dyads")
        )
        unavailable = _nonnegative_integer(
            row.get(f"{prefix}_{metric}_unavailable_dyads")
        )
        if rate is None or comparable is None or unavailable is None:
            return None
        if unavailable != 0:
            return None
        if comparable > 0:
            weighted.append((rate, comparable))
    if not weighted:
        return None
    return round(
        sum(rate * count for rate, count in weighted)
        / sum(count for _, count in weighted),
        6,
    )


def _conditional_rounded_mean(frame, mask, column, label):
    if column not in frame.columns:
        fail(f"{label} is missing {column}")
        return None
    return _rounded_mean(frame.loc[mask, column])


def _compare_rounded_summary_stat(summary, key, expected, label):
    if key not in summary:
        fail(f"{label} is missing {key}")
        return
    actual = summary[key]
    if expected is None:
        if actual is not None:
            fail(f"{label} {key} differs from CSV: {actual} != None")
        return
    actual_number = _finite_number(actual)
    if actual_number is None or round(actual_number, 6) != expected:
        fail(f"{label} {key} differs from CSV: {actual} != {expected:.6f}")


def _compare_reliability_gate_status(summary, key, required, passed, label):
    gate = summary.get(key)
    if not isinstance(gate, dict):
        fail(f"{label} is missing {key}")
        return
    if gate.get("required") != required:
        fail(
            f"{label} {key}.required differs from the declared requirement: "
            f"{gate.get('required')} != {required}"
        )
    observed = _strict_bool(gate.get("passed"))
    if observed is None or observed != passed:
        fail(
            f"{label} {key}.passed differs from CSV: "
            f"{gate.get('passed')} != {passed}"
        )


def _verify_simulation_3_reliability_summary(
    summary, reliability, recomputed_gates, ghost_found, label
):
    if not isinstance(summary, dict):
        fail(f"{label} is missing or malformed")
        return

    replicate_count = len(reliability)
    seed_values = (
        _integer_values(reliability["sample_seed"])
        if "sample_seed" in reliability.columns
        else None
    )
    if seed_values is None:
        fail(f"{label} sample seed set cannot be derived from CSV")
        expected_seeds = None
    else:
        expected_seeds = sorted(seed_values)

    if summary.get("replicates") != replicate_count:
        fail(
            f"{label} replicates differ from CSV: "
            f"{summary.get('replicates')} != {replicate_count}"
        )
    if expected_seeds is not None and summary.get("sample_seeds") != expected_seeds:
        fail(f"{label} sample_seeds differ from CSV")

    structural_detections = int(ghost_found.sum())
    structural_rate = (
        round(structural_detections / replicate_count, 6) if replicate_count else None
    )
    joint_passes = int(recomputed_gates.sum())
    joint_rate = round(joint_passes / replicate_count, 6) if replicate_count else None
    _compare_rounded_summary_stat(
        summary, "structural_ghost_detections", structural_detections, label
    )
    _compare_rounded_summary_stat(
        summary, "structural_detection_rate", structural_rate, label
    )
    _compare_rounded_summary_stat(summary, "joint_gate_passes", joint_passes, label)
    _compare_rounded_summary_stat(summary, "joint_gate_pass_rate", joint_rate, label)

    precision = (
        _rounded_mean(reliability["ghost_precision"])
        if "ghost_precision" in reliability.columns
        else None
    )
    recall = (
        _rounded_mean(reliability["ghost_recall"])
        if "ghost_recall" in reliability.columns
        else None
    )
    exact_values = (
        [_strict_bool(value) for value in reliability["exact_recovery"]]
        if "exact_recovery" in reliability.columns
        else None
    )
    if exact_values is None or any(value is None for value in exact_values):
        fail(f"{label} exact_recovery is missing or invalid in CSV")
        exact_recovery_rate = None
    else:
        exact_recovery_rate = round(
            float(pd.Series(exact_values, dtype=bool).mean()), 6
        )
    _compare_rounded_summary_stat(summary, "mean_precision", precision, label)
    _compare_rounded_summary_stat(summary, "mean_recall", recall, label)
    _compare_rounded_summary_stat(
        summary, "exact_recovery_rate", exact_recovery_rate, label
    )

    within_ghost = _conditional_rounded_mean(
        reliability, ghost_found, "within_ghost_mas", label
    )
    ghost_to_reference = _conditional_rounded_mean(
        reliability, ghost_found, "ghost_to_reference_mas", label
    )
    ghost_to_mainstream = _conditional_rounded_mean(
        reliability, ghost_found, "ghost_to_mainstream_mas", label
    )
    _compare_rounded_summary_stat(
        summary, "conditional_mean_within_ghost_mas", within_ghost, label
    )
    _compare_rounded_summary_stat(
        summary,
        "fraction_detected_ghosts_mas_above_0_60",
        (
            round(
                float(
                    (
                        reliability.loc[ghost_found, "within_ghost_mas"].map(
                            _finite_number
                        )
                        > SIMULATION_3_WITHIN_MAS_THRESHOLD
                    ).mean()
                ),
                6,
            )
            if "within_ghost_mas" in reliability.columns
            and not reliability.loc[ghost_found, "within_ghost_mas"]
            .map(_finite_number)
            .dropna()
            .empty
            else None
        ),
        label,
    )
    _compare_rounded_summary_stat(
        summary,
        "conditional_mean_ghost_to_reference_mas",
        ghost_to_reference,
        label,
    )
    _compare_rounded_summary_stat(
        summary,
        "conditional_mean_ghost_to_mainstream_mas",
        ghost_to_mainstream,
        label,
    )

    within_ghost_identified = _conditional_rounded_mean(
        reliability, ghost_found, "within_ghost_identified", label
    )
    _compare_rounded_summary_stat(
        summary,
        "conditional_mean_within_ghost_identified",
        within_ghost_identified,
        label,
    )
    _compare_rounded_summary_stat(
        summary,
        "fraction_detected_ghosts_identified_above_0_60",
        (
            round(
                float(
                    (
                        reliability.loc[ghost_found, "within_ghost_identified"].map(
                            _finite_number
                        )
                        > SIMULATION_3_WITHIN_IDENTIFIED_THRESHOLD
                    ).mean()
                ),
                6,
            )
            if "within_ghost_identified" in reliability.columns
            and not reliability.loc[ghost_found, "within_ghost_identified"]
            .map(_finite_number)
            .dropna()
            .empty
            else None
        ),
        label,
    )

    gate_metrics = summary.get("gate_metrics")
    if gate_metrics != list(SIMULATION_3_GATE_METRICS):
        fail(
            f"{label} gate_metrics must cover structural, MAS, and identified "
            f"compatibility: {gate_metrics!r}"
        )
    for index, row in reliability.iterrows():
        if not bool(ghost_found.iloc[index]):
            continue
        expected = _simulation_3_expected_mas_dyads(row).get("within_ghost")
        comparable = _nonnegative_integer(
            row.get("within_ghost_identified_comparable_dyads")
        )
        unavailable = _nonnegative_integer(
            row.get("within_ghost_identified_unavailable_dyads")
        )
        if comparable is None or unavailable is None:
            fail(
                f"{label} row {index + 1} identified dyad counts must be finite "
                "non-negative integers"
            )
        elif comparable + unavailable != expected:
            fail(
                f"{label} row {index + 1} identified dyad denominator differs "
                f"from expected {expected}: {comparable + unavailable}"
            )

    _compare_reliability_gate_status(
        summary,
        "structural_detection_gate",
        SIMULATION_3_RELIABILITY_GATE_REQUIRED,
        structural_detections >= SIMULATION_3_RELIABILITY_GATE_REQUIRED,
        label,
    )
    _compare_reliability_gate_status(
        summary,
        "joint_gate",
        SIMULATION_3_JOINT_GATE_REQUIRED,
        joint_passes >= SIMULATION_3_JOINT_GATE_REQUIRED,
        label,
    )


def _verify_ghost_audit_columns(frame, label: str) -> None:
    missing = SIMULATION_3_AUDIT_COLUMNS - set(frame.columns)
    if missing:
        fail(f"{label} missing Simulation 3 audit columns: {sorted(missing)}")
        return
    for index, row in frame.iterrows():
        row_label = f"{label} row {index + 1}"
        family_counts = _parse_mapping(row["family_counts"])
        primary = _parse_mapping(row["selected_primary_cluster_family_composition"])
        noise = _parse_mapping(row["dbscan_unassigned_noise_family_composition"])
        audit = {
            "family_counts": family_counts,
            "seeded_family_total": row["seeded_family_total"],
            "heterogeneous_to_seeded_ratio": row["heterogeneous_to_seeded_ratio"],
            "selected_primary_cluster_family_composition": primary,
            "selected_primary_cluster_size": row.get("selected_primary_cluster_size"),
            "selected_primary_cluster_false_positive_count": row[
                "selected_primary_cluster_false_positive_count"
            ],
            "primary_cluster_family_composition": _parse_mapping(
                row.get("primary_cluster_family_composition")
            ),
            "primary_cluster_false_positive_count": row.get(
                "primary_cluster_false_positive_count"
            ),
            "dbscan_unassigned_noise_family_composition": noise,
            "dbscan_unassigned_noise_count": row["dbscan_unassigned_noise_count"],
            "dbscan_unassigned_heterogeneous_count": row[
                "dbscan_unassigned_heterogeneous_count"
            ],
            "clustered_heterogeneous_count": row.get("clustered_heterogeneous_count"),
        }
        _verify_ghost_audit(audit, row_label)
        for column, family in (
            ("mainstream_family_count", "mainstream"),
            ("ghost_family_count", "ghost"),
            ("heterogeneous_family_count", "heterogeneous"),
        ):
            if row[column] != SIMULATION_3_FAMILY_SIZES[family]:
                fail(f"{row_label} {column} is inconsistent with family_counts")
        if row["noise_models"] != row["dbscan_unassigned_noise_count"]:
            fail(f"{row_label} DBSCAN noise count differs from noise_models")


def _derive_eps_clusters(data, label, *, configuration):
    """Derive the actual non-noise clusters of one seed-42 sweep run.

    Cluster ids, members, family composition, contrast labels, and
    internal/reference similarity are read from the selected configuration's
    own ``cluster_assignments``, ``generation.family_by_model``, and
    ``contrast_analysis`` inside ``simulation_3_ghost_results.json``.  Nothing
    is hard-coded; the mainstream cluster is derived as the cluster containing
    the reference model (M0001).  The reference eps (0.50) is also the highest
    swept radius, so it is the qualification run; the figure eps (0.35) run is
    audited the same way against its own artifacts.
    """
    runs = data.get("runs") or []
    selected = next(
        (
            item
            for item in runs
            if isinstance(item, dict)
            and isinstance(item.get("configuration"), dict)
            and item["configuration"].get("configuration") == configuration
        ),
        None,
    )
    if selected is None:
        fail(f"{label} cannot find the {configuration}-eps run for cross-linking")
        return None
    generation = selected.get("generation") or {}
    family_by_model = generation.get("family_by_model") or {}
    reference_model_id = generation.get("reference_model_id")
    artifacts = (selected.get("run") or {}).get("artifacts") or {}
    assignments = artifacts.get("cluster_assignments") or []
    contrast = artifacts.get("contrast_analysis") or []

    cluster_by_model = {}
    for assignment in assignments:
        cluster_by_model[str(assignment.get("model_id"))] = assignment.get("cluster_id")
    clusters = {}
    for cluster_id in sorted(
        {cid for cid in cluster_by_model.values() if cid is not None}
    ):
        cluster_id = str(cluster_id)
        members = {
            model_id
            for model_id, assigned in cluster_by_model.items()
            if assigned == cluster_id
        }
        composition = {
            family: sum(
                1 for model_id in members if family_by_model.get(model_id) == family
            )
            for family in SIMULATION_3_FAMILY_SIZES
        }
        contrast_row = next(
            (row for row in contrast if str(row.get("cluster_id")) == cluster_id), {}
        )
        clusters[cluster_id] = {
            "members": members,
            "model_count": len(members),
            "family_composition": composition,
            "label": contrast_row.get("label"),
            "internal_similarity": _finite_number(
                contrast_row.get("internal_compatibility")
            ),
            "reference_similarity": _finite_number(
                contrast_row.get("prior_compatibility")
            ),
        }
    mainstream_cluster_id = None
    for cluster_id, info in clusters.items():
        if reference_model_id in info["members"]:
            mainstream_cluster_id = cluster_id
            break
    return {
        "configuration": configuration,
        "eps": (selected.get("configuration") or {}).get("eps"),
        "reference_model_id": reference_model_id,
        "mainstream_cluster_id": mainstream_cluster_id,
        "clusters": clusters,
    }


def _qualification_denominator_errors(
    row,
    model_count,
    reference_group_size,
    reference_assigned,
    contains_reference,
    is_mainstream_cluster,
):
    """Validate qualification dyad denominators against actual membership.

    The within-cluster denominator must be ``n*(n-1)`` with zero unavailable
    dyads; the reference comparisons use the full reference group (the
    non-noise cluster containing M0001), and the mainstream comparisons use
    that same independently derived group size.  Self-pairs are excluded when
    the row itself is the reference group.
    """
    errors = []
    if model_count is None:
        return [
            "model_count must be a finite non-negative integer for "
            "denominator validation"
        ]
    within_expected = model_count * (model_count - 1)
    for metric in ("mas", "identified"):
        comparable = _nonnegative_integer(row.get(f"within_{metric}_comparable_dyads"))
        unavailable = _nonnegative_integer(
            row.get(f"within_{metric}_unavailable_dyads")
        )
        if comparable is None or unavailable is None:
            errors.append(
                f"within_{metric} comparable/unavailable dyads must be finite "
                "non-negative integers"
            )
            continue
        if comparable + unavailable != within_expected:
            errors.append(
                f"within_{metric} dyads total {comparable + unavailable} differs "
                f"from expected {within_expected} (n*(n-1))"
            )
        if unavailable != 0:
            errors.append(
                f"within_{metric} unavailable dyads must be zero for the "
                "qualification rates"
            )
    if reference_assigned:
        reference_expected = model_count * reference_group_size
        if contains_reference:
            reference_expected -= model_count
        for prefix in ("to_reference", "from_reference"):
            for metric in ("mas", "identified"):
                comparable = _nonnegative_integer(
                    row.get(f"{prefix}_{metric}_comparable_dyads")
                )
                unavailable = _nonnegative_integer(
                    row.get(f"{prefix}_{metric}_unavailable_dyads")
                )
                if comparable is None or unavailable is None:
                    errors.append(
                        f"{prefix}_{metric} comparable/unavailable dyads must be "
                        "finite non-negative integers"
                    )
                    continue
                if comparable + unavailable != reference_expected:
                    errors.append(
                        f"{prefix}_{metric} dyads total "
                        f"{comparable + unavailable} differs from expected "
                        f"{reference_expected} using the full reference group "
                        "and excluding any self-pair"
                    )
    if reference_group_size is not None:
        mainstream_expected = model_count * reference_group_size
        if is_mainstream_cluster:
            mainstream_expected -= model_count
        for prefix, expected_count in (
            ("to_mainstream", mainstream_expected),
            ("from_mainstream", mainstream_expected),
        ):
            for metric in ("mas", "identified"):
                comparable = _nonnegative_integer(
                    row.get(f"{prefix}_{metric}_comparable_dyads")
                )
                unavailable = _nonnegative_integer(
                    row.get(f"{prefix}_{metric}_unavailable_dyads")
                )
                if comparable is None or unavailable is None:
                    errors.append(
                        f"{prefix}_{metric} comparable/unavailable dyads must be "
                        "finite non-negative integers"
                    )
                    continue
                if comparable + unavailable != expected_count:
                    errors.append(
                        f"{prefix}_{metric} dyads total "
                        f"{comparable + unavailable} differs from expected "
                        f"{expected_count} using the actual reference-group "
                        f"size {reference_group_size}"
                    )
    return errors


def _verify_ghost_cluster_qualification(
    frame,
    summary,
    data,
    label,
    *,
    configuration="reference",
    eps=SIMULATION_3_QUALIFICATION_EPS,
    scope="seed42_reference_eps_qualification",
    qualification_key="cluster_qualification",
):
    """Verify a seed-42 per-cluster qualification artifact end to end.

    One qualification record is required per actual non-noise cluster of the
    selected configuration's run.  The reference eps (0.50) qualification --
    the reference eps is also the highest swept radius -- is cross-linked to
    the reference run's cluster assignments, generation family membership,
    and contrast analysis and is checked both in its CSV (``frame``) and in
    the ``cluster_qualification`` JSON object.  The figure eps (0.35)
    qualification runs the same checks on the JSON-only top-level
    ``figure_cluster_qualification`` object (``frame`` is None and the
    records come from the JSON object itself); no CSV exists for it.
    Everything is derived from the selected run's own artifacts (never
    hard-coded).  Each record's label, model count, family composition,
    internal/reference similarities, and reference-group metadata must agree
    exactly with the derived values.  The verifier recomputes every strict
    gate from the numeric columns, requires zero unavailable required dyads,
    and validates denominators against actual membership.  Qualification is
    decided purely by those independently recomputed criteria, regardless of
    planted family composition or cluster id.
    """
    qualification = data.get(qualification_key)
    if not isinstance(qualification, dict):
        fail(f"{label} JSON is missing the {qualification_key} object")
        return

    if frame is not None:
        missing = QUALIFICATION_REQUIRED_COLUMNS - set(frame.columns)
        if missing:
            fail(f"{label} is missing qualification columns: {sorted(missing)}")
            return
        missing_unavailable = set(QUALIFICATION_UNAVAILABLE_COLUMNS) - set(
            frame.columns
        )
        if missing_unavailable:
            fail(
                f"{label} is missing availability columns: "
                f"{sorted(missing_unavailable)}"
            )
            return
        if frame.empty:
            fail(f"{label} contains no cluster qualification records")
            return
        rows = list(frame.iterrows())
    else:
        json_records = qualification.get("records")
        if not isinstance(json_records, list) or not json_records:
            fail(f"{label} JSON qualification records are missing or empty")
            return
        for record in json_records:
            missing_keys = QUALIFICATION_REQUIRED_COLUMNS - set(record)
            if missing_keys:
                fail(
                    f"{label} JSON qualification record is missing fields: "
                    f"{sorted(missing_keys)}"
                )
                return
        rows = list(enumerate(json_records))

    derived = _derive_eps_clusters(data, label, configuration=configuration)
    if derived is None:
        return
    derived_clusters = derived["clusters"]
    reference_model_id = derived["reference_model_id"]
    mainstream_cluster_id = derived["mainstream_cluster_id"]
    reference_group_size = (
        derived_clusters[mainstream_cluster_id]["model_count"]
        if mainstream_cluster_id is not None
        else None
    )
    reference_assigned = any(
        reference_model_id in info["members"] for info in derived_clusters.values()
    )

    record_ids = {str(row["cluster_id"]) for _, row in rows}
    if record_ids != set(derived_clusters):
        fail(
            f"{label} qualification records do not match the actual "
            f"{configuration}-eps non-noise clusters: "
            f"records={sorted(record_ids)} derived={sorted(derived_clusters)}"
        )
        return

    for index, row in rows:
        row_label = f"{label} row {index + 1}"
        cluster_id = str(row["cluster_id"])
        if row["eps"] != eps:
            fail(f"{row_label} eps is not the {configuration} eps {eps}")
        if _nonnegative_integer(row["min_samples"]) != 4:
            fail(f"{row_label} min_samples is not 4")

        # Cross-link: every qualification record must agree exactly with the
        # actual selected run's assignments, family membership, and contrast
        # values.
        derived_info = derived_clusters[cluster_id]
        model_count = _nonnegative_integer(row["model_count"])
        if model_count != derived_info["model_count"]:
            fail(
                f"{row_label} model_count differs from the actual "
                f"{configuration}-eps cluster: {model_count} != "
                f"{derived_info['model_count']}"
            )
        for family in SIMULATION_3_FAMILY_SIZES:
            csv_count = _nonnegative_integer(row[f"{family}_count"])
            if csv_count != derived_info["family_composition"][family]:
                fail(
                    f"{row_label} {family}_count differs from the actual "
                    f"{configuration}-eps family composition: "
                    f"{csv_count} != {derived_info['family_composition'][family]}"
                )
        if str(row["label"]) != str(derived_info["label"]):
            fail(
                f"{row_label} label differs from the actual {configuration}-eps "
                f"contrast label: {row['label']} != {derived_info['label']}"
            )
        if str(row.get("reference_group_cluster_id")) != str(mainstream_cluster_id):
            fail(
                f"{row_label} reference_group_cluster_id differs from the "
                f"derived reference group: {row.get('reference_group_cluster_id')} "
                f"!= {mainstream_cluster_id}"
            )
        if _nonnegative_integer(row.get("reference_group_size")) != (
            reference_group_size or 0
        ):
            fail(
                f"{row_label} reference_group_size differs from the derived "
                f"reference group: {row.get('reference_group_size')} != "
                f"{reference_group_size or 0}"
            )
        internal_similarity = _finite_number(row.get("internal_similarity"))
        reference_similarity = _finite_number(row.get("reference_similarity"))
        if (
            internal_similarity is None
            or derived_info["internal_similarity"] is None
            or abs(internal_similarity - derived_info["internal_similarity"]) > 1e-6
        ):
            fail(
                f"{row_label} internal_similarity differs from the actual "
                f"{configuration}-eps contrast internal_compatibility: "
                f"{internal_similarity} != {derived_info['internal_similarity']}"
            )
        if (
            reference_similarity is None
            or derived_info["reference_similarity"] is None
            or abs(reference_similarity - derived_info["reference_similarity"]) > 1e-6
        ):
            fail(
                f"{row_label} reference_similarity differs from the actual "
                f"{configuration}-eps contrast prior_compatibility: "
                f"{reference_similarity} != {derived_info['reference_similarity']}"
            )

        # Recomputed substantive booleans from the numeric columns; each must
        # match the stored boolean before the overall flag is recomputed.
        within_mas_rate = _finite_number(row.get("within_mas_rate"))
        within_identified_rate = _finite_number(row.get("within_identified_rate"))
        recomputed = {
            "internal_coherent": (
                internal_similarity is not None
                and internal_similarity > SIMULATION_3_INTERNAL_THRESHOLD
            ),
            "reference_distinct": (
                reference_similarity is not None
                and reference_similarity < SIMULATION_3_REFERENCE_SIMILARITY_THRESHOLD
            ),
            "high_mas": (
                within_mas_rate is not None
                and within_mas_rate > SIMULATION_3_WITHIN_MAS_THRESHOLD
            ),
            "high_identified": (
                within_identified_rate is not None
                and within_identified_rate > SIMULATION_3_WITHIN_IDENTIFIED_THRESHOLD
            ),
        }
        common_gate = (
            recomputed["internal_coherent"]
            and recomputed["high_mas"]
            and recomputed["high_identified"]
            and _zero_number(row.get("within_mas_unavailable_dyads"))
            and _zero_number(row.get("within_identified_unavailable_dyads"))
        )
        reference_group_mas = _pooled_qualification_rate(
            row, ("to_reference", "from_reference"), "mas"
        )
        recomputed["common_gate_passed"] = common_gate
        reference_group_available = reference_group_size is not None and (
            reference_group_size > 0
        )
        recomputed["qualifies_as_mainstream"] = (
            common_gate
            and reference_group_available
            and reference_similarity is not None
            and reference_similarity > SIMULATION_3_REFERENCE_SIMILARITY_THRESHOLD
        )
        recomputed["qualifies_as_ghost"] = (
            common_gate
            and recomputed["reference_distinct"]
            and reference_group_mas is not None
            and reference_group_mas < SIMULATION_3_REFERENCE_MAS_THRESHOLD
            and _zero_number(row.get("reference_group_mas_unavailable_dyads"))
        )
        stored_reference_group_mas = _finite_number(row.get("reference_group_mas_rate"))
        if stored_reference_group_mas != reference_group_mas:
            fail(
                f"{row_label} reference_group_mas_rate differs from the pooled "
                f"reference comparisons: {stored_reference_group_mas} != "
                f"{reference_group_mas}"
            )
        stored = {}
        for column in (
            "internal_coherent",
            "reference_distinct",
            "high_mas",
            "high_identified",
            "common_gate_passed",
            "qualifies_as_mainstream",
            "qualifies_as_ghost",
        ):
            stored[column] = _strict_bool(row.get(column))
            if stored[column] is None:
                fail(
                    f"{row_label} {column} is missing or invalid; "
                    "expected True or False"
                )
        for column, recomputed_value in recomputed.items():
            if stored[column] is not None and stored[column] != recomputed_value:
                fail(
                    f"{row_label} stored {column} differs from the recomputed "
                    f"value: {stored[column]} != {recomputed_value}"
                )

        # Zero unavailable seed-42 causal dyads across every reported
        # within/cross comparison.
        for column in QUALIFICATION_UNAVAILABLE_COLUMNS:
            if not _zero_number(row.get(column)):
                fail(
                    f"{row_label} {column} must be zero; every seed-42 causal "
                    "dyad behind the qualification rates must be available"
                )
        if not _zero_number(row.get("required_unavailable_dyads")):
            fail(
                f"{row_label} required_unavailable_dyads must be zero; "
                "required qualification comparisons must be available"
            )

        # Dyad denominators must match actual membership: within n*(n-1),
        # and reference/mainstream comparisons use the full reference group.
        for error in _qualification_denominator_errors(
            row,
            model_count,
            reference_group_size,
            reference_assigned,
            reference_model_id in derived_info["members"],
            cluster_id == mainstream_cluster_id,
        ):
            fail(f"{row_label} {error}")

        for column in (
            "common_gate_passed",
            "qualifies_as_mainstream",
            "qualifies_as_ghost",
        ):
            if stored[column] is not None and stored[column] != recomputed[column]:
                fail(
                    f"{row_label} stored {column} differs from the recomputed "
                    f"value: {stored[column]} != {recomputed[column]}"
                )
    # Planted family membership remains available in every record as an audit
    # field, but it does not decide qualification or require a particular
    # cluster count/id.  This keeps heterogeneous false positives and DBSCAN
    # fragmentation observable without hard-coding a result.

    # The qualification object is the authoritative audit source: it is the
    # selected run's own artifact.  Its excluded-noise audit is self-contained
    # and must never be inferred from the acceptance-gate seed-42 audit (the
    # gate audit and the qualification audit may exclude different model
    # sets).
    if qualification.get("eps") != eps or qualification.get("min_samples") != 4:
        fail(f"{label} JSON qualification configuration is wrong")
    if qualification.get("scope") != scope:
        fail(
            f"{label} JSON qualification scope is unexpected: "
            f"{qualification.get('scope')}"
        )
    if (
        qualification.get("identified_high_threshold")
        != SIMULATION_3_IDENTIFIED_HIGH_THRESHOLD
    ):
        fail(f"{label} JSON qualification identified threshold is wrong")
    note = qualification.get("identified_threshold_note")
    if not isinstance(note, str) or "generic GhostDetector defaults" not in note:
        fail(
            f"{label} JSON qualification identified threshold note is missing "
            "or unclear"
        )
    thresholds = qualification.get("qualification_thresholds")
    if not isinstance(thresholds, dict):
        fail(f"{label} JSON qualification thresholds are missing")
    else:
        expected_thresholds = {
            "internal_similarity_strictly_above": SIMULATION_3_INTERNAL_THRESHOLD,
            "within_mas_strictly_above": SIMULATION_3_WITHIN_MAS_THRESHOLD,
            "within_identified_strictly_above": (
                SIMULATION_3_WITHIN_IDENTIFIED_THRESHOLD
            ),
            "reference_similarity_mainstream_strictly_above": (
                SIMULATION_3_REFERENCE_SIMILARITY_THRESHOLD
            ),
            "reference_similarity_ghost_strictly_below": (
                SIMULATION_3_REFERENCE_SIMILARITY_THRESHOLD
            ),
            "reference_group_mas_ghost_strictly_below": (
                SIMULATION_3_REFERENCE_MAS_THRESHOLD
            ),
            "required_unavailable_dyads": 0,
        }
        for key, expected in expected_thresholds.items():
            if thresholds.get(key) != expected:
                fail(
                    f"{label} qualification threshold {key} is wrong: "
                    f"{thresholds.get(key)} != {expected}"
                )

    # The selected run's excluded-noise family composition must be present,
    # its total must equal the declared excluded_noise_count, and both must
    # agree with that run's primary summary row.  No zero-noise assumption is
    # made; every value is read from the qualification artifact.
    excluded_noise = qualification.get("excluded_noise_family_composition")
    if not isinstance(excluded_noise, dict) or set(excluded_noise) != set(
        SIMULATION_3_FAMILY_SIZES
    ):
        fail(
            f"{label} excluded noise family composition is malformed: "
            f"{excluded_noise}"
        )
        return
    excluded_noise_count = _nonnegative_integer(
        qualification.get("excluded_noise_count")
    )
    if excluded_noise_count is None or (
        sum(excluded_noise.values()) != excluded_noise_count
    ):
        fail(
            f"{label} excluded noise family composition total "
            f"{sum(excluded_noise.values())} does not equal excluded_noise_count "
            f"{excluded_noise_count}"
        )
        return
    selected_row = summary[summary["configuration"] == configuration]
    if selected_row.empty:
        fail(
            f"{label} cannot find the {configuration}-eps primary row for the "
            "noise audit"
        )
    elif excluded_noise_count != _nonnegative_integer(
        selected_row.iloc[0]["noise_models"]
    ):
        fail(
            f"{label} excluded_noise_count differs from the {configuration}-eps "
            "summary noise count"
        )

    # Family counts must balance: non-noise clusters plus the selected run's
    # excluded noise cover the planted families exactly.
    totals = {family: 0 for family in SIMULATION_3_FAMILY_SIZES}
    for _, row in rows:
        for family in SIMULATION_3_FAMILY_SIZES:
            totals[family] += _nonnegative_integer(row[f"{family}_count"]) or 0
    for family in SIMULATION_3_FAMILY_SIZES:
        observed = totals[family] + int(excluded_noise.get(family, 0))
        if observed != SIMULATION_3_FAMILY_SIZES[family]:
            fail(
                f"{label} {family} family counts do not balance across the "
                f"non-noise clusters plus excluded noise: {observed} != "
                f"{SIMULATION_3_FAMILY_SIZES[family]}"
            )

    json_records = qualification.get("records")
    if not isinstance(json_records, list) or not json_records:
        fail(f"{label} JSON qualification records are missing or empty")
        json_records = []
    if frame is not None:
        csv_by_id = {str(row["cluster_id"]): row for _, row in rows}
        json_by_id = {
            str(record.get("cluster_id")): record
            for record in json_records
            if isinstance(record, dict)
        }
        if set(csv_by_id) != set(json_by_id):
            fail(f"{label} JSON qualification cluster ids differ from the CSV")
        else:
            for cluster_id, csv_row in csv_by_id.items():
                json_record = json_by_id[cluster_id]
                for field in ("model_count", "ghost_count", "qualifies_as_ghost"):
                    if csv_row.get(field) != json_record.get(field):
                        fail(
                            f"{label} JSON qualification record for {cluster_id} "
                            f"differs from the CSV on {field}"
                        )


def _expected_figure_qualification_summary(qualification):
    """The compact summary the driver must record for the figure qualification.

    Derived purely from the figure_cluster_qualification records themselves
    (never hard-coded): eps, non-noise cluster count, qualifying count/ids,
    and excluded noise count.
    """
    records = qualification.get("records") or []
    qualifying_ids = [
        str(record.get("cluster_id"))
        for record in records
        if record.get("qualifies_as_ghost") is True
    ]
    return {
        "eps": SIMULATION_3_FIGURE_EPS,
        "min_samples": 4,
        "non_noise_cluster_count": len(records),
        "qualifying_cluster_count": len(qualifying_ids),
        "qualifying_cluster_ids": qualifying_ids,
        "excluded_noise_count": qualification.get("excluded_noise_count"),
    }


def _edge_set(value):
    if not isinstance(value, list):
        return None
    edges = set()
    for edge in value:
        if (
            not isinstance(edge, (list, tuple))
            or len(edge) != 2
            or not all(isinstance(node, str) for node in edge)
        ):
            return None
        edges.add(tuple(edge))
    return edges


def _edge_tuple(value):
    if (
        not isinstance(value, (list, tuple))
        or len(value) != 2
        or not all(isinstance(node, str) for node in value)
    ):
        return None
    return tuple(value)


def _verify_simulation_1b_construction() -> None:
    path = RESULTS_DIR / "simulation_1b_consensus_results.json"
    if not path.exists():
        fail("Missing simulation_1b_consensus_results.json")
        return
    try:
        payload = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as exc:
        fail(f"Simulation 1B results JSON is unreadable: {exc}")
        return
    if not isinstance(payload, dict):
        fail("Simulation 1B results JSON must contain an object")
        return

    if payload.get("design_label") != "1B":
        fail(
            f"Simulation 1B results have wrong design_label: {payload.get('design_label')}"
        )
    if payload.get("compatibility_metric") != "identified_compatible":
        fail(
            "Simulation 1B results must use identified_compatible, found "
            f"{payload.get('compatibility_metric')}"
        )
    construction = payload.get("construction")
    if not isinstance(construction, dict):
        fail("Simulation 1B results are missing the construction object")
        return

    if construction.get("design") != SIMULATION_1B_DESIGN:
        fail(
            "Simulation 1B construction design is wrong: "
            f"{construction.get('design')} != {SIMULATION_1B_DESIGN}"
        )
    if _edge_set(construction.get("fixed_edges")) != SIMULATION_1B_FIXED_EDGES:
        fail(
            "Simulation 1B fixed edges are wrong: " f"{construction.get('fixed_edges')}"
        )
    if _edge_tuple(construction.get("focal_edge")) != SIMULATION_1B_FOCAL_EDGE:
        fail("Simulation 1B focal edge is wrong: " f"{construction.get('focal_edge')}")
    if _edge_set(construction.get("context_edges")) != SIMULATION_1B_CONTEXT_EDGES:
        fail(
            "Simulation 1B context edges are wrong: "
            f"{construction.get('context_edges')}"
        )
    if construction.get("node_timing") != SIMULATION_1B_NODE_TIMING:
        fail(
            "Simulation 1B node timing is wrong: " f"{construction.get('node_timing')}"
        )
    if construction.get("directed_only_registry") is not True:
        fail("Simulation 1B registry must be directed-only")

    expected_counts = {
        "resolved_model_count": 128,
        "partial_theory_count": 64,
        "augmented_multiverse_count": 192,
        "completion_block_count": 64,
    }
    for field, expected in expected_counts.items():
        observed = _nonnegative_integer(construction.get(field))
        if observed != expected:
            fail(
                f"Simulation 1B construction {field} is wrong: "
                f"{observed} != {expected}"
            )

    blocks = payload.get("completion_blocks")
    if not isinstance(blocks, list) or len(blocks) != 64:
        fail("Simulation 1B completion_blocks must contain exactly 64 blocks")
    elif any(not isinstance(block, list) or len(block) != 3 for block in blocks):
        fail("Simulation 1B completion blocks must each contain three model IDs")


def verify_simulation_1():
    _verify_simulation_1b_construction()
    combined = RESULTS_DIR / "simulation_1_consensus_summary.csv"
    if not combined.exists():
        fail("Missing simulation_1_consensus_summary.csv")
        return
    frame = pd.read_csv(combined)
    if len(frame) != 4:
        fail(f"Combined summary has {len(frame)} rows, expected 4")

    required_cols = {
        "neighborhood",
        "1A_mean_similarity",
        "1A_mas_compatibility",
        "1A_identified_compatibility",
        "1B_mean_similarity",
        "1B_mas_compatibility",
        "1B_identified_compatibility",
    }
    missing = required_cols - set(frame.columns)
    if missing:
        fail(f"Combined summary missing columns: {missing}")

    for label in ("1A", "1B"):
        metrics = (
            f"{label}_mean_similarity",
            f"{label}_mas_compatibility",
            f"{label}_identified_compatibility",
        )
        for col in metrics:
            if col not in frame.columns or frame[col].isna().any():
                fail(f"{label} column {col} missing or has NA values")
        reliability = (
            RESULTS_DIR
            / f"simulation_{'1a' if label == '1A' else '1b'}_consensus_sampling_reliability.csv"
        )
        if not reliability.exists() or len(pd.read_csv(reliability)) != 100:
            fail(f"{label} sampling reliability does not contain 100 replicates")


def verify_simulation_2():
    crux = RESULTS_DIR / "simulation_2_crux_summary.csv"
    if not crux.exists():
        fail("Missing simulation_2_crux_summary.csv")
        return
    frame = pd.read_csv(crux)
    metrics = set(frame["metric"].unique())
    if "identified_compatible" in metrics:
        fail("Simulation 2 ID metric should be removed but appears in CSV")
    if set(frame["claim"].unique()) != {
        "X2 -> X1",
        "X2 -> X3",
        "X1 -> X4",
        "X4 -> X3",
    }:
        fail(f"Unexpected Simulation 2 claims: {sorted(set(frame['claim'].unique()))}")
    structural = frame[frame.metric == "similarity_rate"]
    structural_deltas = set(structural["delta_u"])
    if len(structural_deltas) != 1:
        fail(f"Structural Delta-U is not a perfect tie: {structural_deltas}")
    mas = frame[frame.metric == "mas_compatible"]
    mas_top = mas[mas["rank"] == 1]
    if mas_top["claim"].iloc[0] != "X2 -> X1":
        fail(f"MAS top crux is not X2 -> X1: {mas_top['claim'].iloc[0]}")
    if mas_top["delta_u"].iloc[0] < 0.33:
        fail(f"MAS Delta-U too low: {mas_top['delta_u'].iloc[0]}")
    if mas_top["delta_u_causal"].iloc[0] != mas_top["delta_u_non_causal"].iloc[0]:
        fail("MAS crux branches differ under marginal semantics")
    if set(frame["crux_mode"].unique()) != {"marginal"}:
        fail(f"Unexpected Simulation 2 crux modes: {set(frame['crux_mode'].unique())}")
    if any(frame["models_changed_causal"] < 1):
        fail("Simulation 2 marginal rows report no changed models")
    if any(frame["mapping_coverage_causal"] != 1.0):
        fail("Simulation 2 marginal rows report incomplete mapping coverage")

    results = (RESULTS_DIR / "simulation_2_crux_results.json").read_text().lower()
    svg = (FIGURES_DIR / "figure_2_phase_transition.svg").read_text().lower()
    forbidden_terms = (
        "identified_compatible",
        "identified compatibility",
        "identification compatibility",
        "general identification",
    )
    for term in forbidden_terms:
        if term in results or term in svg:
            fail(f"Simulation 2 generated artifacts contain ID label: {term}")


def verify_simulation_3():
    ghost_csv = RESULTS_DIR / "simulation_3_ghost_summary.csv"
    ghost_json = RESULTS_DIR / "simulation_3_ghost_results.json"
    if not ghost_csv.exists():
        fail("Missing simulation_3_ghost_summary.csv")
        return
    frame = pd.read_csv(ghost_csv)
    if len(frame) != 3:
        fail(f"Simulation 3 primary summary has {len(frame)} rows, expected 3")
    _verify_ghost_audit_columns(frame, "Simulation 3 primary summary")
    for col in (
        "top_internal_mas_compatibility",
        "top_reference_mas_compatibility",
        "top_ghost_to_mainstream_mas_compatibility",
    ):
        if col not in frame.columns:
            fail(f"Ghost CSV missing MAS validation column: {col}")
    if "analysis_role" not in frame.columns:
        fail("Simulation 3 summary is missing analysis_role")
    elif set(frame["analysis_role"]) != {"primary_eps_sensitivity"}:
        fail(
            "Simulation 3 summary contains a non-primary row; "
            f"roles={set(frame['analysis_role'])}"
        )
    if "min_samples" not in frame.columns:
        fail("Simulation 3 summary is missing min_samples")
    elif set(frame["min_samples"]) != {4}:
        fail("Simulation 3 primary epsilon rows do not all use min_samples=4")
    if "eps" not in frame.columns:
        fail("Simulation 3 summary is missing eps")
    elif set(frame["eps"]) != set(SIMULATION_3_EPS_SWEEP):
        fail(f"Simulation 3 epsilon sweep is wrong: {set(frame['eps'])}")
    if "configuration" not in frame.columns:
        fail("Simulation 3 summary is missing configuration")
    elif set(frame["configuration"]) != SIMULATION_3_CONFIGURATIONS:
        fail(
            "Simulation 3 summary has unexpected configuration identifiers: "
            f"{sorted(set(frame['configuration']))}"
        )

    # Strict-setting seed-42 showcase: the selected primary of the strict
    # eps=0.20 run must under-recover the planted ghost family (recovered < 12
    # and recall < 1).  Recomputed from the strict summary row (never from the
    # recorded showcase object) and required as a condition of the simulation
    # being a valid showcase of strict under-recovery.  The total number of
    # structurally ghost-labeled models is recorded for audit but is NOT
    # required to stay below the planted count: heterogeneous false-positive
    # ghost clusters can legitimately push that total above 12.  This is
    # deliberately NOT part of the primary/reference acceptance gate.
    strict_rows = frame[frame["configuration"] == "strict"]
    if strict_rows.empty:
        fail("Simulation 3 summary is missing the strict row for the showcase")
    else:
        strict_row = strict_rows.iloc[0]
        if strict_row["eps"] != SIMULATION_3_STRICT_EPS:
            fail(
                "Simulation 3 strict row does not use " f"eps={SIMULATION_3_STRICT_EPS}"
            )
        strict_total = _nonnegative_integer(strict_row.get("ghost_models"))
        strict_recovered = _nonnegative_integer(
            strict_row.get("recovered_ghost_models")
        )
        strict_recall = _finite_number(strict_row.get("ghost_recall"))
        if strict_total is None:
            fail(
                "Strict showcase ghost_models must be a finite non-negative "
                "integer (recorded for audit; not required below the planted "
                "count)"
            )
        if strict_recovered is None:
            fail(
                "Strict showcase recovered_ghost_models must be a finite "
                "non-negative integer"
            )
        elif strict_recovered >= SIMULATION_3_PLANTED_GHOST_COUNT:
            fail(
                "Strict showcase selected primary must recover fewer than "
                f"{SIMULATION_3_PLANTED_GHOST_COUNT} planted ghost models, "
                f"found {strict_recovered}"
            )
        if strict_recall is None or strict_recall >= 1.0:
            fail(
                "Strict showcase selected primary ghost recall must be < 1, "
                f"found {strict_recall}"
            )

    reliability_path = RESULTS_DIR / "simulation_3_ghost_reliability.csv"
    if not reliability_path.exists():
        fail("Missing simulation_3_ghost_reliability.csv")
        reliability = None
    else:
        reliability = pd.read_csv(reliability_path)
        if len(reliability) != 100:
            fail(f"Ghost reliability has {len(reliability)} rows, expected 100")
        expected_seeds = set(range(1001, 1101))
        if "sample_seed" not in reliability.columns:
            fail("Ghost reliability is missing sample_seed")
        else:
            seed_values = _integer_values(reliability["sample_seed"])
            if seed_values is None or set(seed_values) != expected_seeds:
                fail(
                    "Ghost reliability does not use the exact seed set 1001-1100: "
                    f"{sorted(set(seed_values or []))}"
                )
        _verify_ghost_audit_columns(reliability, "Simulation 3 reliability")
        if "analysis_role" not in reliability.columns:
            fail("Simulation 3 reliability is missing analysis_role")
        elif set(reliability["analysis_role"]) != {"primary_eps_reliability"}:
            fail("Simulation 3 reliability has an unexpected analysis role")
        if "sensitivity_axis" not in reliability.columns:
            fail("Simulation 3 reliability is missing sensitivity_axis")
        elif set(reliability["sensitivity_axis"]) != {"eps"}:
            fail("Simulation 3 reliability has an unexpected sensitivity axis")
        if "eps" not in reliability.columns:
            fail("Simulation 3 reliability is missing eps")
        elif set(reliability["eps"]) != {SIMULATION_3_PRIMARY_EPS}:
            fail(
                "Simulation 3 reliability does not use primary "
                f"eps={SIMULATION_3_PRIMARY_EPS}"
            )
        if "min_samples" not in reliability.columns:
            fail("Simulation 3 reliability is missing min_samples")
        elif set(reliability["min_samples"]) != {4}:
            fail("Simulation 3 reliability does not use primary min_samples=4")

    # MAS query fields should be in JSON
    if not ghost_json.exists():
        fail("Missing simulation_3_ghost_results.json")
        return

    data = json.loads(ghost_json.read_text())
    design = data.get("design", {})
    if design.get("exposure") != "X1":
        fail(f"Ghost query exposure is not X1: {design.get('exposure')}")
    if design.get("outcome") != "Y":
        fail(f"Ghost query outcome is not Y: {design.get('outcome')}")
    if design.get("family_sizes") != SIMULATION_3_FAMILY_SIZES:
        fail(f"Ghost design family sizes are wrong: {design.get('family_sizes')}")
    if design.get("family_counts") != SIMULATION_3_FAMILY_SIZES:
        fail(f"Ghost design family counts are wrong: {design.get('family_counts')}")
    if design.get("total_models") != SIMULATION_3_TOTAL_MODELS:
        fail(f"Ghost design total is not 200: {design.get('total_models')}")
    if design.get("sample_models") != SIMULATION_3_TOTAL_MODELS:
        fail(f"Ghost design sample_models is not 200: {design.get('sample_models')}")
    for key in ("unique_models", "unique_model_count"):
        if design.get(key) != SIMULATION_3_TOTAL_MODELS:
            fail(f"Ghost design {key} is not 200: {design.get(key)}")
    if design.get("seeded_family_total") != SIMULATION_3_SEEDED_FAMILY_TOTAL:
        fail(
            f"Ghost design seeded family total is wrong: {design.get('seeded_family_total')}"
        )
    if not _ghost_ratio_matches(design.get("heterogeneous_to_seeded_ratio")):
        fail(
            "Ghost design heterogeneous-to-seeded ratio is wrong: "
            f"{design.get('heterogeneous_to_seeded_ratio')}"
        )
    if design.get("reference_model_id") != "M0001":
        fail(f"Ghost reference model is not M0001: {design.get('reference_model_id')}")
    if design.get("reference_model_family") != "mainstream":
        fail(
            "Ghost reference model is not mainstream: "
            f"{design.get('reference_model_family')}"
        )
    if design.get("primary_eps") != SIMULATION_3_PRIMARY_EPS:
        fail(
            f"Ghost primary epsilon is not {SIMULATION_3_PRIMARY_EPS}: "
            f"{design.get('primary_eps')}"
        )
    if design.get("primary_min_samples") != 4:
        fail(
            "Ghost primary min_samples is not 4: "
            f"{design.get('primary_min_samples')}"
        )
    if design.get("eps_sweep") != list(SIMULATION_3_EPS_SWEEP):
        fail(f"Ghost epsilon sweep is wrong: {design.get('eps_sweep')}")

    runs = data.get("runs") or []
    primary_runs = [
        item
        for item in runs
        if isinstance(item.get("configuration"), dict)
        and item["configuration"].get("analysis_role") == "primary_eps_sensitivity"
    ]
    if len(primary_runs) != 3:
        fail(f"Simulation 3 JSON has {len(primary_runs)} primary runs, expected 3")
    reference_run = next(
        (
            item
            for item in primary_runs
            if (item.get("configuration") or {}).get("configuration") == "reference"
        ),
        None,
    )
    if reference_run is None:
        fail("Simulation 3 JSON is missing the primary reference-eps run")
    else:
        generation = reference_run.get("generation") or {}
        family_by_model = generation.get("family_by_model") or {}
        generated_counts = {
            family: sum(1 for value in family_by_model.values() if value == family)
            for family in SIMULATION_3_FAMILY_SIZES
        }
        if generated_counts != SIMULATION_3_FAMILY_SIZES:
            fail(f"Generated family membership counts are wrong: {generated_counts}")
        if len(family_by_model) != SIMULATION_3_TOTAL_MODELS:
            fail("Generated family membership is not unique across 200 models")
        if family_by_model.get("M0001") != "mainstream":
            fail("Generated reference M0001 is not a mainstream model")
        state_data = (
            reference_run.get("run", {}).get("artifacts", {}).get("state_data", [])
        )
        if len({str(row.get("model_id")) for row in state_data}) != (
            SIMULATION_3_TOTAL_MODELS
        ):
            fail("Generated state data does not contain 200 unique models")
        registry_data = (
            reference_run.get("run", {}).get("artifacts", {}).get("registry_data", [])
        )
        fixed_edge_ids = {
            str(row.get("comp_id"))
            for row in registry_data
            if row.get("type") == "edge"
            and row.get("source") == "X1"
            and row.get("target") == "Y"
        }
        if len(fixed_edge_ids) != 1:
            fail("Generated registry is missing the unique fixed X1->Y edge")
        fixed_edge_id = next(iter(fixed_edge_ids), None)
        edge_ids = sorted(
            str(row.get("comp_id"))
            for row in registry_data
            if row.get("type") == "edge"
        )
        signatures = {}
        for row in state_data:
            model_id = str(row.get("model_id"))
            comp_id = str(row.get("comp_id"))
            if comp_id in edge_ids:
                signatures.setdefault(model_id, {})[comp_id] = row.get("status")
        if set(signatures) != set(family_by_model) or any(
            set(values) != set(edge_ids) for values in signatures.values()
        ):
            fail("Generated state data does not cover every edge for every model")
        elif fixed_edge_id is not None and any(
            signatures[model_id][fixed_edge_id] != "causal" for model_id in signatures
        ):
            fail("Generated state data flips the fixed X1->Y causal edge")
        elif (
            len(
                {
                    tuple(signatures[model_id][comp_id] for comp_id in edge_ids)
                    for model_id in sorted(signatures)
                }
            )
            != SIMULATION_3_TOTAL_MODELS
        ):
            fail("Generated model edge signatures are not unique across 200 models")
        _verify_ghost_audit(
            (data.get("seed42_validation") or {}).get("audit"),
            "Seed-42 ghost audit",
        )

    validation = data.get("seed42_validation") or {}
    gate = validation.get("acceptance_gate") or {}
    checks = gate.get("checks") or {}
    if not checks or not gate.get("passed", False) or not all(checks.values()):
        fail(f"Seed-42 ghost acceptance gate did not pass: {gate}")

    # The machine-readable strict showcase record must exist and must agree
    # with the strict summary row recomputed above (eps, totals, recall, and
    # the boolean checks).  The ghost-labeled total is cross-checked against
    # the summary row for audit but is not required to stay below the planted
    # count; only the selected-primary under-recovery invariants are required.
    strict_showcase = data.get("strict_showcase")
    if not isinstance(strict_showcase, dict):
        fail("Simulation 3 JSON is missing the strict_showcase record")
    else:
        if (
            strict_showcase.get("eps") != SIMULATION_3_STRICT_EPS
            or strict_showcase.get("min_samples") != 4
        ):
            fail("Simulation 3 strict_showcase configuration is wrong")
        if not strict_rows.empty:
            strict_row = strict_rows.iloc[0]
            recorded_total = strict_showcase.get("ghost_labeled_cluster_total_models")
            recorded_recovered = strict_showcase.get(
                "selected_primary_recovered_ghost_models"
            )
            recorded_recall = _finite_number(
                strict_showcase.get("selected_primary_ghost_recall")
            )
            if recorded_total != _nonnegative_integer(strict_row.get("ghost_models")):
                fail(
                    "Simulation 3 strict_showcase ghost-labeled total differs "
                    "from the strict summary row"
                )
            if recorded_recovered != _nonnegative_integer(
                strict_row.get("recovered_ghost_models")
            ):
                fail(
                    "Simulation 3 strict_showcase recovered count differs from "
                    "the strict summary row"
                )
            if recorded_recall is None or recorded_recall != _finite_number(
                strict_row.get("ghost_recall")
            ):
                fail(
                    "Simulation 3 strict_showcase recall differs from the "
                    "strict summary row"
                )
        if strict_showcase.get("passed") is not True:
            fail(
                "Simulation 3 strict_showcase.passed is not True: "
                f"{strict_showcase.get('passed')}"
            )
        for key in ("selected_primary_recovered_lt_12", "recall_lt_1"):
            if strict_showcase.get(key) is not True:
                fail(f"Simulation 3 strict_showcase.{key} is not True")

    secondary = data.get("secondary_sensitivity")
    if not isinstance(secondary, dict):
        fail("Simulation 3 is missing the secondary min_samples=10 diagnostic")
    else:
        if secondary.get("analysis_role") != "secondary_min_samples_sensitivity":
            fail("Simulation 3 secondary diagnostic has the wrong analysis role")
        if (
            secondary.get("seed") != 42
            or secondary.get("eps") != SIMULATION_3_PRIMARY_EPS
        ):
            fail(
                "Simulation 3 secondary diagnostic is not seed 42 at "
                f"primary eps={SIMULATION_3_PRIMARY_EPS}"
            )
        if secondary.get("min_samples") != 10:
            fail("Simulation 3 secondary diagnostic does not use min_samples=10")
        if secondary.get("controls_primary_gate") is not False:
            fail("Secondary min_samples diagnostic controls the primary gate")
        if secondary.get("controls_reliability_gate") is not False:
            fail("Secondary min_samples diagnostic controls the reliability gate")
        _verify_ghost_audit(secondary.get("audit"), "Secondary min_samples=10 audit")

    qualification_path = RESULTS_DIR / "simulation_3_ghost_cluster_qualification.csv"
    if not qualification_path.exists():
        fail("Missing simulation_3_ghost_cluster_qualification.csv")
    else:
        _verify_ghost_cluster_qualification(
            pd.read_csv(qualification_path),
            frame,
            data,
            "Seed-42 ghost cluster qualification",
        )

    # The figure-radius eps=0.35 qualification lives in the results JSON only
    # (no CSV): one record per actual non-noise figure cluster, each judged
    # purely on its independently recomputed criteria (never on planted family
    # membership).
    figure_qualification = data.get("figure_cluster_qualification")
    if not isinstance(figure_qualification, dict):
        fail(
            "Simulation 3 JSON is missing the top-level "
            "figure_cluster_qualification object"
        )
    else:
        _verify_ghost_cluster_qualification(
            None,
            frame,
            data,
            "Seed-42 figure-eps ghost cluster qualification",
            configuration="figure",
            eps=SIMULATION_3_FIGURE_EPS,
            scope="seed42_figure_eps_qualification",
            qualification_key="figure_cluster_qualification",
        )

    summary = data.get("reliability_summary") or {}
    if not isinstance(summary, dict):
        fail("Ghost reliability summary is malformed")
        summary = {}
    if summary.get("replicates") != 100:
        fail("Ghost reliability summary does not declare 100 replicates")
    if summary.get("sample_seeds") != list(range(1001, 1101)):
        fail("Ghost reliability summary does not declare seeds 1001-1100")
    if summary.get("analysis_role") != "primary_eps_reliability":
        fail("Ghost reliability summary has an unexpected analysis role")
    if (
        summary.get("eps") != SIMULATION_3_PRIMARY_EPS
        or summary.get("min_samples") != 4
    ):
        fail("Ghost reliability summary has the wrong primary configuration")
    # Reliability is always recomputed for the current generator and metric;
    # preserved CSVs are not accepted as evidence for a changed design.
    if summary.get("reliability_provenance") != "computed":
        fail(
            "Ghost reliability summary provenance must be 'computed': "
            f"{summary.get('reliability_provenance')!r}"
        )
    structural_gate = summary.get("structural_detection_gate")
    if not isinstance(structural_gate, dict) or not structural_gate.get("passed"):
        fail("Ghost structural reliability summary gate failed")
    joint_gate = summary.get("joint_gate")
    if not isinstance(joint_gate, dict) or not joint_gate.get("passed"):
        fail("Ghost joint reliability summary gate failed")
    if reliability is not None:
        recomputed_gates, ghost_found = _verify_simulation_3_reliability_gates(
            reliability
        )
        structural_detections = int(ghost_found.sum())
        joint_passes = int(recomputed_gates.sum())
        if structural_detections < SIMULATION_3_RELIABILITY_GATE_REQUIRED:
            fail("Ghost structural reliability gate did not reach 75/100")
        if joint_passes < SIMULATION_3_JOINT_GATE_REQUIRED:
            fail("Ghost joint reliability gate did not reach 70/100")
        required_reliability = {
            "within_ghost_mas",
            "ghost_to_reference_mas",
            "reference_to_ghost_mas",
            "ghost_to_mainstream_mas",
            "mainstream_to_ghost_mas",
            "within_mainstream_mas",
        }
        for metric in required_reliability:
            for suffix in ("", "_comparable_dyads", "_unavailable_dyads"):
                column = f"{metric}{suffix}"
                if column not in reliability.columns:
                    fail(f"Ghost reliability missing MAS audit column: {column}")

        _verify_simulation_3_reliability_summary(
            summary,
            reliability,
            recomputed_gates,
            ghost_found,
            "Ghost reliability summary",
        )
        for run_index, run in enumerate(runs):
            if isinstance(run, dict) and "reliability_summary" in run:
                _verify_simulation_3_reliability_summary(
                    run["reliability_summary"],
                    reliability,
                    recomputed_gates,
                    ghost_found,
                    f"Simulation 3 run {run_index + 1} reliability summary",
                )


def verify_no_old_artifacts():
    obsolete = [
        "simulation_D_sampling_results.json",
        "simulation_D_sampling_summary.csv",
        "simulation_D_sampling_reliability.csv",
        "simulation_A1_results.json",
        "simulation_A2_results.json",
        "simulation_A_precision_summary.csv",
        "simulation_A_baseline.json",
        "simulation_B_crux_results.json",
        "simulation_B_crux_summary.csv",
        "simulation_C_ghost_results.json",
        "simulation_C_ghost_summary.csv",
        "simulation_C_ghost_reliability.csv",
        "simulation_C_ghost_cluster_qualification.csv",
        "simulation_A1_mas_results.json",
        "simulation_A1_mas_summary.csv",
        "simulation_A1_mas_sampling_summary.csv",
        "simulation_A1_mas_sampling_reliability.csv",
        "simulation_A2_id_results.json",
        "simulation_A2_id_summary.csv",
        "simulation_A2_id_sampling_summary.csv",
        "simulation_A2_id_sampling_reliability.csv",
    ]
    for name in obsolete:
        if (HERE / name).exists():
            fail(f"Obsolete artifact still present: {name}")
    obsolete_figures = (
        "fig_A_metric_contrast.png",
        "fig_A_metric_contrast.svg",
        "fig_D_sampling_reliability.png",
        "fig_D_sampling_reliability.svg",
        "fig_B_phase_transition.png",
        "fig_B_phase_transition.svg",
        "fig_C_ghost_discovery.png",
        "fig_C_ghost_discovery.svg",
    )
    for name in obsolete_figures:
        if (FIGURES_DIR / name).exists():
            fail(f"Obsolete figure artifact still present: {name}")


def verify_figures():
    for base in FIGURES:
        for ext in (".png", ".svg"):
            path = FIGURES_DIR / f"{base}{ext}"
            if not path.exists():
                fail(f"Missing figure: {path.name}")

    # Check no cream background in SVGs.
    for base in FIGURES:
        svg_path = FIGURES_DIR / f"{base}.svg"
        if svg_path.exists():
            text = svg_path.read_text()
            for cream in ("#FBF8F2", "#fbf8f2", "#FBF8f2", "#fbf8F2"):
                if cream in text:
                    fail(f"Cream color {cream} found in {svg_path.name}")

    # Check for old non-Viridis manuscript colors.
    old_colors = ("#355C7D", "#A44A3F", "#4F7C66", "#8C5E58", "#D8A657")
    for base in FIGURES:
        svg_path = FIGURES_DIR / f"{base}.svg"
        if svg_path.exists():
            text = svg_path.read_text()
            for color in old_colors:
                if color in text:
                    fail(f"Old palette color {color} found in {svg_path.name}")

    required_colors = {"#482475", "#1f958b", "#ffffff"}
    svg_text = "".join(
        (FIGURES_DIR / f"{base}.svg").read_text().lower() for base in FIGURES
    )
    for color in required_colors:
        if color not in svg_text:
            fail(f"Required Viridis/white color {color} absent from SVG figures")

    for base in FIGURES:
        png_path = FIGURES_DIR / f"{base}.png"
        with Image.open(png_path) as image:
            rgba = image.convert("RGBA")
            width, height = rgba.size
            alpha_extrema = rgba.getchannel("A").getextrema()
            if alpha_extrema != (255, 255):
                fail(f"PNG is not fully opaque: {png_path.name}")
            samples = [
                (1, 1),
                (width - 2, 1),
                (1, height - 2),
                (width - 2, height - 2),
                (width // 2, height - 2),
            ]
            if any(rgba.getpixel(point)[:3] != (255, 255, 255) for point in samples):
                fail(f"PNG background samples are not white: {png_path.name}")

    # Figure 3: the UMAP legend is shape-only, while the third panel
    # displays the data-driven reference contrast and strict .50/.60 lines.
    # Cluster ids may appear as dynamic annotations in the contrast panel; no
    # cluster id is hard-coded by this verifier.
    fig_c = FIGURES_DIR / "figure_3_ghost_discovery.svg"
    if fig_c.exists():
        svg_text = fig_c.read_text().lower()
        for label in ("mainstream", "ghost", "fragmented", "unqualified", "noise"):
            if f"<!-- {label} -->" not in svg_text:
                fail(f"Figure 3 SVG legend is missing the {label} shape label")
        results_path = RESULTS_DIR / "simulation_3_ghost_results.json"
        if not results_path.exists():
            fail(
                "Figure 3 title cannot be verified without "
                "simulation_3_ghost_results.json"
            )
        else:
            results = json.loads(results_path.read_text())
            figure_run = next(
                (
                    item
                    for item in (results.get("runs") or [])
                    if isinstance(item.get("configuration"), dict)
                    and item["configuration"].get("configuration") == "figure"
                ),
                None,
            )
            if figure_run is None:
                fail("Figure 3 title cannot be verified: missing figure-eps run")
            else:
                config = figure_run["configuration"]
                clusters = (
                    figure_run.get("run", {})
                    .get("results", {})
                    .get("clusters_detected")
                )
                if clusters is None:
                    fail(
                        "Figure 3 title cannot be verified: the figure run has no "
                        "cluster count"
                    )
                else:
                    expected_title = (
                        f"eps = {float(config['eps']):.2f} "
                        f"({int(clusters)} clusters)"
                    )
                    if expected_title not in svg_text:
                        fail(
                            "Figure 3 SVG title is not the data-driven "
                            f"{expected_title!r}"
                        )


def _verify_simulation_3_prototype_mas_contract(
    contract, results_design, label: str
) -> None:
    """Require and validate the Simulation 3 prototype MAS contract.

    The metadata block must declare the mainstream prototype's minimal
    adjustment set [[X3]] and the ghost prototype's [[X2, X3]], and the
    declaration must agree with the contract recorded in the results design.
    """
    if (
        not isinstance(contract, dict)
        or contract.get("mainstream_mas") != [["X3"]]
        or contract.get("ghost_mas") != [["X2", "X3"]]
    ):
        fail(
            f"{label} prototype MAS contract must be the mainstream [[X3]] "
            f"and ghost [[X2, X3]] minimal adjustment sets: {contract!r}"
        )
        return
    if results_design is not None:
        results_contract = (results_design or {}).get("prototype_mas_contract")
        if results_contract != contract:
            fail(
                f"{label} prototype MAS contract differs from the results "
                f"design contract: {results_contract!r}"
            )


def verify_metadata():
    path = RESULTS_DIR / "simulation_metadata.json"
    if not path.exists():
        fail("Missing simulation_metadata.json")
        return
    data = json.loads(path.read_text())
    for key in (
        "workflow",
        "active_simulations",
        "active_figures",
        "manifest",
        "baseline_provenance",
        "identified_compatibility_semantic_version",
        "simulation_3",
        "source_hashes",
        "artifact_hashes",
        "git",
    ):
        if key not in data:
            fail(f"Metadata missing key: {key}")
    sim3 = data.get("simulation_3") or {}
    if sim3.get("exposure") != "X1" or sim3.get("outcome") != "Y":
        fail("Metadata Simulation 3 query mismatch")
    for key in (
        "prototype_edges",
        "prototype_mas_contract",
        "family_sizes",
        "family_counts",
        "total_models",
        "unique_models",
        "unique_model_count",
        "seeded_family_names",
        "seeded_family_total",
        "heterogeneous_model_count",
        "heterogeneous_to_seeded_ratio",
        "reference_model_id",
        "reference_model_family",
        "primary_eps",
        "primary_min_samples",
        "eps_sweep",
        "reliability_seeds",
        "reliability_provenance",
        "seed42_audit",
        "strict_showcase",
        "protected_edges",
        "perturbation_probability",
        "heterogeneous_causal_probability",
        "duplicate_policy",
        "design_hash",
    ):
        if key not in sim3:
            fail(f"Metadata Simulation 3 design missing {key}")
    if sim3.get("perturbation_probability") != 0.02:
        fail("Metadata Simulation 3 perturbation probability is not 0.02")
    if sim3.get("heterogeneous_causal_probability") != 0.5:
        fail("Metadata Simulation 3 heterogeneous probability is not 0.5")
    if sim3.get("family_sizes") != SIMULATION_3_FAMILY_SIZES:
        fail("Metadata Simulation 3 family sizes are not 30/12/158")
    if sim3.get("family_counts") != SIMULATION_3_FAMILY_SIZES:
        fail("Metadata Simulation 3 family counts are not 30/12/158")
    if sim3.get("total_models") != SIMULATION_3_TOTAL_MODELS:
        fail("Metadata Simulation 3 total model count is not 200")
    if sim3.get("unique_models") != SIMULATION_3_TOTAL_MODELS:
        fail("Metadata Simulation 3 unique model count is not 200")
    if sim3.get("unique_model_count") != SIMULATION_3_TOTAL_MODELS:
        fail("Metadata Simulation 3 unique_model_count is not 200")
    if sim3.get("seeded_family_names") != ["mainstream", "ghost"]:
        fail("Metadata Simulation 3 seeded family names are wrong")
    if sim3.get("seeded_family_total") != SIMULATION_3_SEEDED_FAMILY_TOTAL:
        fail("Metadata Simulation 3 seeded family total is wrong")
    if (
        sim3.get("heterogeneous_model_count")
        != SIMULATION_3_FAMILY_SIZES["heterogeneous"]
    ):
        fail("Metadata Simulation 3 heterogeneous model count is wrong")
    if (
        sim3.get("heterogeneous_to_seeded_ratio")
        != SIMULATION_3_HETEROGENEOUS_TO_SEEDED_RATIO
    ):
        fail("Metadata Simulation 3 heterogeneous-to-seeded ratio is wrong")
    if sim3.get("reference_model_id") != "M0001":
        fail("Metadata Simulation 3 reference model is not M0001")
    if sim3.get("reference_model_family") != "mainstream":
        fail("Metadata Simulation 3 reference model is not mainstream")
    if (
        sim3.get("primary_eps") != SIMULATION_3_PRIMARY_EPS
        or sim3.get("primary_min_samples") != 4
    ):
        fail("Metadata Simulation 3 primary clustering settings are wrong")
    if sim3.get("eps_sweep") != list(SIMULATION_3_EPS_SWEEP):
        fail("Metadata Simulation 3 epsilon sweep is wrong")
    results_path = RESULTS_DIR / "simulation_3_ghost_results.json"
    results_data = None
    results_design = None
    if results_path.exists():
        results_data = json.loads(results_path.read_text())
        results_design = results_data.get("design") or {}
    _verify_simulation_3_prototype_mas_contract(
        sim3.get("prototype_mas_contract"), results_design, "Metadata Simulation 3"
    )
    if sim3.get("reliability_seeds") != list(range(1001, 1101)):
        fail("Metadata Simulation 3 reliability seeds are not exactly 1001-1100")
    # Reliability must be freshly computed for the current generator and
    # metric implementation; stale CSVs are not accepted as evidence.
    if sim3.get("reliability_provenance") != "computed":
        fail(
            "Metadata Simulation 3 reliability provenance must be 'computed': "
            f"unexpected: {sim3.get('reliability_provenance')!r}"
        )
    _verify_ghost_audit(sim3.get("seed42_audit"), "Metadata Seed-42 ghost audit")
    strict_showcase = sim3.get("strict_showcase")
    if not isinstance(strict_showcase, dict):
        fail("Metadata Simulation 3 strict_showcase is missing or malformed")
    else:
        if (
            strict_showcase.get("eps") != SIMULATION_3_STRICT_EPS
            or strict_showcase.get("min_samples") != 4
        ):
            fail("Metadata Simulation 3 strict_showcase configuration is wrong")
        if strict_showcase.get("passed") is not True:
            fail(
                "Metadata Simulation 3 strict_showcase.passed is not True: "
                f"{strict_showcase.get('passed')}"
            )
        for key in ("selected_primary_recovered_lt_12", "recall_lt_1"):
            if strict_showcase.get(key) is not True:
                fail(f"Metadata Simulation 3 strict_showcase.{key} is not True")
    threshold = sim3.get("identified_compatibility_threshold") or {}
    if (
        threshold.get("value") != SIMULATION_3_IDENTIFIED_HIGH_THRESHOLD
        or not isinstance(threshold.get("note"), str)
        or "generic GhostDetector defaults" not in threshold.get("note", "")
    ):
        fail(
            "Metadata Simulation 3 identified-compatibility threshold must "
            "document the design-specific gate without changing generic "
            "GhostDetector defaults"
        )
    fixed_edges = sim3.get("fixed_causal_edges")
    if fixed_edges != ["X1->Y"]:
        fail(f"Metadata Simulation 3 fixed causal edges are wrong: {fixed_edges}")
    qualification_thresholds = sim3.get("qualification_thresholds")
    expected_qualification_thresholds = {
        "internal_similarity_strictly_above": SIMULATION_3_INTERNAL_THRESHOLD,
        "within_mas_strictly_above": SIMULATION_3_WITHIN_MAS_THRESHOLD,
        "within_identified_strictly_above": (SIMULATION_3_WITHIN_IDENTIFIED_THRESHOLD),
        "reference_similarity_mainstream_strictly_above": (
            SIMULATION_3_REFERENCE_SIMILARITY_THRESHOLD
        ),
        "reference_similarity_ghost_strictly_below": (
            SIMULATION_3_REFERENCE_SIMILARITY_THRESHOLD
        ),
        "reference_group_mas_ghost_strictly_below": (
            SIMULATION_3_REFERENCE_MAS_THRESHOLD
        ),
        "required_unavailable_dyads": 0,
    }
    if qualification_thresholds != expected_qualification_thresholds:
        fail(
            "Metadata Simulation 3 qualification thresholds are wrong: "
            f"{qualification_thresholds} != {expected_qualification_thresholds}"
        )
    secondary = sim3.get("secondary_sensitivity") or {}
    if (
        secondary.get("analysis_role") != "secondary_min_samples_sensitivity"
        or secondary.get("seed") != 42
        or secondary.get("eps") != SIMULATION_3_PRIMARY_EPS
        or secondary.get("min_samples") != 10
        or secondary.get("controls_primary_gate") is not False
        or secondary.get("controls_reliability_gate") is not False
    ):
        fail("Metadata Simulation 3 secondary sensitivity diagnostic is wrong")

    # The figure-radius eps=0.35 qualification summary recorded in the
    # metadata must match the top-level figure_cluster_qualification object in
    # the results JSON exactly (eps, non-noise cluster count, qualifying
    # count/ids, excluded noise count), and the same summary must appear in
    # the results design block.
    figure_summary = sim3.get("figure_cluster_qualification_summary")
    figure_qualification = (
        results_data.get("figure_cluster_qualification")
        if results_data is not None
        else None
    )
    if not isinstance(figure_qualification, dict):
        fail(
            "Metadata Simulation 3 cannot verify the figure qualification "
            "summary without the figure_cluster_qualification object"
        )
    elif not isinstance(figure_summary, dict):
        fail("Metadata Simulation 3 is missing figure_cluster_qualification_summary")
    else:
        expected_summary = _expected_figure_qualification_summary(figure_qualification)
        if figure_summary != expected_summary:
            fail(
                "Metadata Simulation 3 figure_cluster_qualification_summary "
                f"differs from the JSON object: {figure_summary} != "
                f"{expected_summary}"
            )
        if results_design.get("figure_cluster_qualification_summary") != figure_summary:
            fail(
                "Metadata Simulation 3 figure qualification summary differs "
                "from the results design summary"
            )

    source_hashes = data.get("source_hashes") or {}
    expected_source_keys = list(SOURCE_PATHS)
    if list(source_hashes) != expected_source_keys:
        fail("Metadata source hash inventory is incomplete, unexpected, or unsorted")
    for path_text, expected in source_hashes.items():
        if Path(path_text).is_absolute() or "docs/internal" in path_text:
            fail(f"Metadata source path is not portable: {path_text}")
        path = SOURCE_PATHS.get(path_text)
        if path is None:
            fail(
                f"Metadata source hash path is not in the source inventory: {path_text}"
            )
            continue
        if not path.exists() or digest(path) != expected:
            fail(f"Metadata source hash mismatch: {path_text}")

    artifact_hashes = data.get("artifact_hashes") or {}
    expected_metadata_artifacts = EXPECTED_ARTIFACTS - {
        "results/simulation_metadata.json"
    }
    if set(artifact_hashes) != expected_metadata_artifacts:
        fail("Metadata artifact hash inventory is incomplete or unexpected")
    for name, expected in artifact_hashes.items():
        if Path(name).is_absolute() or "docs/internal" in name:
            fail(f"Metadata artifact path is not portable: {name}")
        path = SIMULATIONS_DIR / name
        if not path.exists() or digest(path) != expected:
            fail(f"Metadata artifact hash mismatch: {name}")


def verify_manifest():
    path = MANIFEST_PATH
    if not path.exists():
        fail("Missing manifest.json")
        return
    data = json.loads(path.read_text())
    for key in ("version", "workflow", "sources", "artifacts", "metadata"):
        if key not in data:
            fail(f"Manifest missing section: {key}")
    if any(key in data for key in ("markdown", "docx", "documents")):
        fail("Manifest must not contain document hashes or document sections")

    manifest_sources = data.get("sources") or {}
    expected_source_keys = list(SOURCE_PATHS)
    if list(manifest_sources) != expected_source_keys:
        fail("Manifest source inventory is incomplete, unexpected, or unsorted")
    if set(data.get("artifacts", {})) != EXPECTED_ARTIFACTS:
        fail("Manifest artifact inventory is incomplete or unexpected")
    for name, expected in manifest_sources.items():
        if Path(name).is_absolute() or "docs/internal" in name:
            fail(f"Manifest source path is not portable: {name}")
        path = SOURCE_PATHS.get(name)
        if path is None or not path.exists() or digest(path) != expected:
            fail(f"Manifest source hash mismatch: {name}")
    for name, expected in data.get("artifacts", {}).items():
        if Path(name).is_absolute() or "docs/internal" in name:
            fail(f"Manifest artifact path is not portable: {name}")
        path = SIMULATIONS_DIR / name
        if not path.exists() or digest(path) != expected:
            fail(f"Manifest artifact hash mismatch: {name}")
    actual_files = {
        path.relative_to(SIMULATIONS_DIR).as_posix()
        for directory in (RESULTS_DIR, FIGURES_DIR)
        for path in directory.rglob("*")
        if path.is_file()
    }
    expected_files = set(EXPECTED_ARTIFACTS)
    if actual_files != expected_files:
        fail(
            "Simulation results/figures contain an unexpected artifact set: "
            f"missing={sorted(expected_files - actual_files)}, "
            f"extra={sorted(actual_files - expected_files)}"
        )
    metadata = data.get("metadata") or {}
    if metadata.get("path") != "results/simulation_metadata.json":
        fail("Manifest metadata path is not relative to simulations/")
    if metadata.get("seed") != 42:
        fail("Manifest seed is not deterministic seed 42")
    if metadata.get("active_simulations") != [
        "consensus_illusion",
        "crux",
        "ghost",
    ]:
        fail("Manifest active simulation inventory is wrong")
    if metadata.get("active_figures") != [
        "figure_2_phase_transition",
        "figure_3_ghost_discovery",
    ]:
        fail("Manifest active figure inventory is wrong")
    metadata_path = SIMULATIONS_DIR / "results/simulation_metadata.json"
    if metadata_path.exists():
        metadata_data = json.loads(metadata_path.read_text())
        manifest_artifacts_without_metadata = {
            key: value
            for key, value in data.get("artifacts", {}).items()
            if key != "results/simulation_metadata.json"
        }
        metadata_sources = metadata_data.get("source_hashes") or {}
        if (
            list(metadata_sources) != list(manifest_sources)
            or metadata_sources != manifest_sources
        ):
            fail("Manifest source hashes differ from simulation metadata")
        if metadata_data.get("artifact_hashes") != manifest_artifacts_without_metadata:
            fail("Manifest artifact hashes differ from simulation metadata")


def main() -> int:
    verify_simulation_1()
    verify_simulation_2()
    verify_simulation_3()
    verify_no_old_artifacts()
    verify_figures()
    verify_metadata()
    verify_manifest()
    if ERRORS:
        for error in ERRORS:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1
    print("All verifier checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
