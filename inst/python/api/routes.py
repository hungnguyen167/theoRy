import logging
import math
import signal
import os
import asyncio

import numpy as np
from fastapi import APIRouter, HTTPException
from fastapi.responses import JSONResponse
from api.models import (
    BuildRegistryRequest,
    ClustersRequest,
    DeltaURequest,
    DyadMatrixRequest,
    ExpandModelStatesRequest,
    SimulateRequest,
    SymbolicUniverseRequest,
    SymbolicQueryRequest,
    SymbolicCompareRequest,
    SymbolicDeltaURequest,
    SymbolicSimulateRequest,
)
from registry.builder import ComponentRegistryBuilder
from registry.loader import RegistryLoader
from registry.schema import RegistryError
from state.expander import ModelStateExpander
from state.tensor import StateError, StateTensor
from dyadic.engine import DyadicEngine, DyadicError
from dyadic.causal import CausalWrapper, CausalError
from dyadic.identification import IdentificationWrapper, IdentificationError
from dyadic.hybrid import HybridDyadicEngine
from api.session import (
    update_latest_dyad_context,
    get_latest_dyad_context,
    update_latest_symbolic_context,
)
from simulation.delta_u import DeltaUEngine, DeltaUError
from simulation.suite import SimulationSuite, SimulationError, SimulationInputError
from clustering.engine import ClusteringEngine, ClusteringError
from clustering.ghost import GhostDetector, GhostError
from symbolic.universe import build_symbolic_universe as sym_build_universe
from symbolic.constraints import (
    constraints_from_edge_statuses as sym_edge_constraints,
    constraints_from_dag_spec as sym_dag_constraints,
    node_absence_constraints as sym_absence_constraints,
    fixed_edge_constraints as sym_fixed_constraints,
)
from symbolic.engine import SymbolicCompatibilityEngine
from symbolic.delta_u import SymbolicDeltaUEngine
from symbolic.simulation import SymbolicSimulationEngine
from symbolic.classes import build_query_classes as sym_build_query_classes

router = APIRouter()

engine = DyadicEngine()
hybrid = HybridDyadicEngine(engine)
logger = logging.getLogger("api.routes")


def _sanitize_null(value):
    """Replace NaN/None with JSON-safe null."""
    if value is None:
        return None
    if isinstance(value, float):
        if np.isnan(value) or math.isnan(value):
            return None
    return value


def _sanitize_record(record: dict) -> dict:
    """Replace all NaN values in a record dict with None."""
    return {k: _sanitize_null(v) for k, v in record.items()}


def _json_mass(value):
    if isinstance(value, int) and abs(value) > 10**15:
        return str(value)
    return value


@router.get("/health")
async def health():
    return {"status": "success", "data": {"status": "healthy", "version": "0.1.0"}}


@router.post("/component-registry")
async def build_component_registry_endpoint(request: BuildRegistryRequest):
    """Build a component registry from node / timing specifications."""
    try:
        nodes = [n.model_dump() for n in request.nodes]
        constraints = (
            [c.model_dump() for c in request.constraints]
            if request.constraints
            else None
        )
        registry = ComponentRegistryBuilder.from_nodes(
            nodes,
            respect_timing=request.respect_timing,
            include_bidirectional=request.include_bidirectional,
            constraints=constraints,
            exposure=request.exposure,
            outcome=request.outcome,
        )
        clean_records = []
        for r in registry.data.to_dict(orient="records"):
            rec = _sanitize_record(r)
            if not rec.get("description"):
                rec["description"] = rec.get("source", "")
            clean_records.append(rec)
        return {
            "status": "success",
            "data": {
                "registry_data": clean_records,
                "summary": registry.summary(),
            },
        }
    except RegistryError as e:
        raise HTTPException(
            status_code=400,
            detail={"code": "INVALID_NODES", "message": str(e)},
        )


@router.post("/model-states")
async def expand_model_states_endpoint(request: ExpandModelStatesRequest):
    """Expand a component registry into model-state records."""
    try:
        registry_records = [r.model_dump() for r in request.registry_data]
        registry = RegistryLoader.from_records(registry_records)
    except RegistryError as e:
        raise HTTPException(
            status_code=400,
            detail={"code": "INVALID_REGISTRY", "message": str(e)},
        )

    seed_claims = (
        [s.model_dump() for s in request.seed_claims] if request.seed_claims else None
    )

    exposure = request.exposure
    outcome = request.outcome

    if (exposure is None) != (outcome is None):
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_CAUSAL_TARGET",
                "message": "Both or neither of exposure and outcome must be provided",
            },
        )

    if exposure is not None and outcome is not None:
        node_names = set(
            registry.data[registry.data["type"] == "node"]["source"].tolist()
        )
        if exposure == outcome:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_CAUSAL_TARGET",
                    "message": "Exposure and outcome must be distinct nodes",
                },
            )
        invalid = [n for n in (exposure, outcome) if n not in node_names]
        if invalid:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_CAUSAL_TARGET",
                    "message": f"Invalid exposure/outcome node(s): {invalid}",
                },
            )

    try:
        state_records = ModelStateExpander.expand(
            registry,
            mode=request.mode,
            seed_claims=seed_claims,
            node_timing=request.node_timing,
            timing_options=request.timing_options,
            optional_nodes=request.optional_nodes,
            max_models=request.max_models,
            n_models=request.n_models,
            seed=request.seed,
            edge_statuses=request.edge_statuses,
            bidirected_statuses=request.bidirected_statuses,
            node_policy=request.node_policy,
            allow_large=request.allow_large,
            exposure=exposure,
            outcome=outcome,
        )
        model_ids = sorted({r["model_id"] for r in state_records})
        seeded_model_ids = sorted(
            {r["model_id"] for r in state_records if r.get("seeded")}
        )
        return {
            "status": "success",
            "data": {
                "state_data": state_records,
                "model_count": len(model_ids),
                "component_count": len(registry.data),
                "seeded_model_ids": seeded_model_ids,
                "pruning_report": state_records.pruning_report,
            },
        }
    except StateError as e:
        raise HTTPException(
            status_code=400,
            detail={"code": "EXPANSION_ERROR", "message": str(e)},
        )


@router.post("/dyad-matrix")
async def dyad_matrix(request: DyadMatrixRequest):
    if not request.registry_data:
        raise HTTPException(
            status_code=400,
            detail={
                "code": "EMPTY_REGISTRY",
                "message": "Registry must contain at least one component",
            },
        )

    try:
        registry_records = [r.model_dump() for r in request.registry_data]
        registry = RegistryLoader.from_records(registry_records)
    except RegistryError as e:
        raise HTTPException(
            status_code=400,
            detail={"code": "INVALID_REGISTRY", "message": str(e)},
        )

    try:
        state_records = [r.model_dump() for r in request.state_data]
        state = StateTensor.from_records(registry, state_records, request.model_ids)
    except Exception as e:
        raise HTTPException(
            status_code=400,
            detail={"code": "INVALID_STATE", "message": str(e)},
        )

    exposure = request.exposure
    outcome = request.outcome

    if (exposure is None) != (outcome is None):
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_CAUSAL_TARGET",
                "message": "Both or neither of exposure and outcome must be provided",
            },
        )

    if exposure is not None and outcome is not None:
        node_names = set(
            registry.data[registry.data["type"] == "node"]["source"].tolist()
        )
        if exposure == outcome:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_CAUSAL_TARGET",
                    "message": "Exposure and outcome must be distinct nodes",
                },
            )
        invalid = [n for n in (exposure, outcome) if n not in node_names]
        if invalid:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_CAUSAL_TARGET",
                    "message": f"Invalid exposure/outcome node(s): {invalid}",
                },
            )

    causal_wrapper = None
    identification_wrapper = None
    if request.mode in ("full", "two-stage"):
        try:
            causal_wrapper = CausalWrapper(causal_backend=request.causal_backend)
        except CausalError as e:
            raise HTTPException(
                status_code=400,
                detail={"code": "CAUSAL_ERROR", "message": str(e)},
            )
        if exposure is not None and outcome is not None:
            try:
                identification_wrapper = IdentificationWrapper(
                    causal_backend=request.causal_backend
                )
            except IdentificationError as e:
                raise HTTPException(
                    status_code=400,
                    detail={"code": "IDENTIFICATION_ERROR", "message": str(e)},
                )

    try:
        if request.mode == "basic":
            dyads = engine.compare_pairs(
                state,
                registry,
                mode="basic",
                exposure=exposure,
                outcome=outcome,
            )
        elif request.mode == "full":
            dyads = engine.compare_pairs(
                state,
                registry,
                mode="full",
                causal_wrapper=causal_wrapper,
                identification_wrapper=identification_wrapper,
                exposure=exposure,
                outcome=outcome,
            )
        elif request.mode == "single-ref":
            if request.reference_id is None:
                raise HTTPException(
                    status_code=422,
                    detail={
                        "code": "INVALID_REFERENCE",
                        "message": "reference_id is required for mode='single-ref'",
                    },
                )
            try:
                dyads = hybrid.compare_single_ref(
                    request.reference_id,
                    state,
                    registry,
                    mode="basic",
                    exposure=exposure,
                    outcome=outcome,
                )
            except ValueError as e:
                raise HTTPException(
                    status_code=400,
                    detail={"code": "INVALID_REFERENCE", "message": str(e)},
                )
        elif request.mode == "two-stage":
            top_k = request.top_k
            if top_k is None or top_k <= 0:
                raise HTTPException(
                    status_code=422,
                    detail={
                        "code": "INVALID_TOP_K",
                        "message": "top_k must be positive for mode='two-stage'",
                    },
                )
            two_stage_result = hybrid.compare_two_stage(
                state,
                registry,
                top_k=top_k,
                causal_wrapper=causal_wrapper,
                identification_wrapper=identification_wrapper,
                exposure=exposure,
                outcome=outcome,
            )
        else:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_MODE",
                    "message": f"Unknown mode: {request.mode}",
                },
            )
    except DyadicError as e:
        logger.error(f"Dyadic engine error: {e}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail={"code": "INTERNAL_ERROR", "message": str(e)},
        )
    except CausalError as e:
        logger.error(f"Causal error: {e}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail={"code": "CAUSAL_ERROR", "message": str(e)},
        )
    except IdentificationError as e:
        logger.error(f"Identification error: {e}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail={"code": "IDENTIFICATION_ERROR", "message": str(e)},
        )
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Unexpected dyad-matrix error: {e}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail={
                "code": "INTERNAL_ERROR",
                "message": "An unexpected error occurred",
            },
        )

    if request.mode == "two-stage":
        cached_dyads = two_stage_result["heatmap_dyads"]
        response_data = {
            "model_count": two_stage_result["heatmap_summary"]["model_count"],
            "dyad_count": two_stage_result["heatmap_summary"]["dyad_count"],
            "mode": request.mode,
            "top_k": two_stage_result["heatmap_summary"]["top_k"],
            "heatmap_summary": two_stage_result["heatmap_summary"],
            "detailed_comparisons": two_stage_result["detailed_comparisons"],
        }
    else:
        response_data = {
            "dyads": dyads,
            "model_count": len(state.model_ids),
            "dyad_count": len(dyads),
            "mode": request.mode,
        }
        if request.mode == "single-ref":
            response_data["reference_id"] = request.reference_id
        cached_dyads = dyads

    if request.mode in ("full", "two-stage"):
        response_data["causal_backend"] = request.causal_backend
        if exposure:
            response_data["exposure"] = exposure
        if outcome:
            response_data["outcome"] = outcome

    update_latest_dyad_context(
        registry=registry,
        state=state,
        dyads=cached_dyads,
        mode=request.mode,
        exposure=exposure,
        outcome=outcome,
    )

    return {"status": "success", "data": response_data}


@router.post("/delta-u")
async def delta_u(request: DeltaURequest):
    if request.registry_data is not None or request.state_data is not None:
        if request.registry_data is None or request.state_data is None:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_DELTA_U_CONTEXT",
                    "message": "registry_data and state_data must be provided together",
                },
            )
        try:
            registry_records = [r.model_dump() for r in request.registry_data]
            registry = RegistryLoader.from_records(registry_records)
            state_records = [r.model_dump() for r in request.state_data]
            state = StateTensor.from_records(
                registry,
                state_records,
                request.model_ids,
            )
        except Exception as e:
            raise HTTPException(
                status_code=422,
                detail={"code": "INVALID_DELTA_U_CONTEXT", "message": str(e)},
            )
        dyads = request.dyads or []
        exposure = request.exposure
        outcome = request.outcome
    else:
        context = get_latest_dyad_context()
        if context is None:
            raise HTTPException(
                status_code=400,
                detail={
                    "code": "NO_DYADS",
                    "message": "Run dyad-matrix computation first",
                },
            )
        registry = context.registry
        state = context.state
        dyads = context.dyads
        exposure = request.exposure or context.exposure
        outcome = request.outcome or context.outcome

    if not dyads:
        raise HTTPException(
            status_code=400,
            detail={
                "code": "NO_DYADS",
                "message": "Dyad records are required for Delta-U",
            },
        )

    compatibility_metric = request.compatibility_metric

    if compatibility_metric not in (
        "similarity_rate",
        "mas_compatible",
        "identified_compatible",
    ):
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_COMPATIBILITY_METRIC",
                "message": "compatibility_metric must be one of: "
                "similarity_rate, mas_compatible, identified_compatible",
            },
        )

    if request.device == "cuda":
        import torch

        if not torch.cuda.is_available():
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "CUDA_UNAVAILABLE",
                    "message": "CUDA requested but not available",
                },
            )

    if (exposure is None) != (outcome is None):
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_CAUSAL_TARGET",
                "message": "Both or neither of exposure and outcome must be provided",
            },
        )
    if exposure is not None and outcome is not None:
        node_names = set(
            registry.data[registry.data["type"] == "node"]["source"].tolist()
        )
        if exposure == outcome:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_CAUSAL_TARGET",
                    "message": "Exposure and outcome must be distinct nodes",
                },
            )
        invalid = [n for n in (exposure, outcome) if n not in node_names]
        if invalid:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_CAUSAL_TARGET",
                    "message": f"Invalid exposure/outcome node(s): {invalid}",
                },
            )

    if request.mode == "two-stage":
        heatmap_threshold = (
            request.heatmap_threshold if request.heatmap_threshold is not None else 0.1
        )
        if not 0.0 <= heatmap_threshold <= 1.0:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "INVALID_HEATMAP_THRESHOLD",
                    "message": "heatmap_threshold must be between 0 and 1",
                },
            )
    else:
        heatmap_threshold = request.heatmap_threshold

    if request.top_k <= 0:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_TOP_K",
                "message": "top_k must be positive",
            },
        )

    causal_wrapper = None
    identification_wrapper = None
    requires_causal = compatibility_metric in (
        "mas_compatible",
        "identified_compatible",
    )
    if requires_causal and (exposure is None or outcome is None):
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_CAUSAL_TARGET",
                "message": (
                    f"compatibility_metric {compatibility_metric!r} requires both "
                    "exposure and outcome in the request or stored dyad context"
                ),
            },
        )
    if requires_causal:
        try:
            causal_wrapper = CausalWrapper()
        except CausalError as e:
            raise HTTPException(
                status_code=400,
                detail={"code": "CAUSAL_ERROR", "message": str(e)},
            )
        if compatibility_metric == "identified_compatible":
            try:
                from dyadic.identification import IdentificationWrapper

                identification_wrapper = IdentificationWrapper()
            except Exception as e:
                raise HTTPException(
                    status_code=400,
                    detail={
                        "code": "IDENTIFICATION_ERROR",
                        "message": f"Failed to initialize identification: {e}",
                    },
                )

    delta_engine = DeltaUEngine(
        dyadic_engine=engine,
        causal_wrapper=causal_wrapper,
        compatibility_metric=compatibility_metric,
        device=request.device,
        use_tensor_engine=request.use_tensor_engine,
        exposure=exposure,
        outcome=outcome,
        identification_wrapper=identification_wrapper,
        model_ids=list(state.model_ids),
        crux_mode=request.crux_mode,
    )
    computation_mode = request.mode

    requires_causal = compatibility_metric in (
        "mas_compatible",
        "identified_compatible",
    )
    try:
        if len(dyads) != len(state.model_ids) * (len(state.model_ids) - 1):
            dyads = engine.compare_pairs(
                state,
                registry,
                mode="full" if requires_causal else "basic",
                causal_wrapper=causal_wrapper,
                identification_wrapper=identification_wrapper,
                exposure=exposure,
                outcome=outcome,
            )

        if request.crux_mode == "global":
            rankings = delta_engine.rank_lynchpins(
                state,
                dyads,
                registry,
                top_k=request.top_k,
                mode=request.mode,
                heatmap_threshold=heatmap_threshold,
            )
            response_data = {
                "rankings": rankings,
                "component_count": len(state.component_ids),
                "crux_mode": "global",
                "computation_mode": "global",
            }
        elif request.component_id is not None:
            if request.component_id not in state.component_index:
                raise HTTPException(
                    status_code=422,
                    detail={
                        "code": "COMPONENT_NOT_FOUND",
                        "message": (
                            f"Component {request.component_id} "
                            f"not found in registry"
                        ),
                    },
                )
            single = delta_engine.compute_delta_u(
                request.component_id,
                state,
                dyads,
                registry,
            )
            response_data = {
                "result": single,
                "computation_mode": computation_mode,
            }
        else:
            rankings = delta_engine.rank_lynchpins(
                state,
                dyads,
                registry,
                top_k=request.top_k,
                mode=request.mode,
                heatmap_threshold=heatmap_threshold,
            )
            response_data = {
                "rankings": rankings,
                "component_count": len(state.component_ids),
                "computation_mode": computation_mode,
            }

            if request.synergistic_set_size is not None:
                synergy_search = request.synergistic_search or "greedy"
                synergy_beam = request.synergistic_beam_width or 5
                if request.synergistic_set_size < 2:
                    raise HTTPException(
                        status_code=422,
                        detail={
                            "code": "INVALID_SYNERGY_REQUEST",
                            "message": "synergistic_set_size must be at least 2",
                        },
                    )
                if synergy_beam <= 0:
                    raise HTTPException(
                        status_code=422,
                        detail={
                            "code": "INVALID_SYNERGY_REQUEST",
                            "message": "synergistic_beam_width must be positive",
                        },
                    )
                synergistic = delta_engine.compute_synergistic_sets(
                    state,
                    dyads,
                    registry,
                    set_size=request.synergistic_set_size,
                    top_n=request.top_k,
                    search_strategy=synergy_search,
                    beam_width=synergy_beam,
                )
                response_data["synergistic_sets"] = synergistic

    except DeltaUError as e:
        raise HTTPException(
            status_code=422,
            detail={"code": "DELTA_U_ERROR", "message": str(e)},
        )
    except CausalError as e:
        raise HTTPException(
            status_code=500,
            detail={"code": "CAUSAL_DELTA_U_ERROR", "message": str(e)},
        )
    except DyadicError as e:
        raise HTTPException(
            status_code=422,
            detail={"code": "DYAD_ERROR", "message": str(e)},
        )
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Unexpected delta-u error: {e}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail={
                "code": "INTERNAL_ERROR",
                "message": "An unexpected error occurred",
            },
        )

    response_data["compatibility_metric"] = compatibility_metric
    response_data["crux_mode"] = request.crux_mode
    if exposure is not None:
        response_data["exposure"] = exposure
    if outcome is not None:
        response_data["outcome"] = outcome
    response_data["device"] = request.device
    response_data["used_tensor_engine"] = delta_engine.used_tensor_engine

    return {"status": "success", "data": response_data}


@router.post("/clusters")
async def clusters(request: ClustersRequest):
    """Detect ghost clusters via compatibility profile clustering."""
    dyads = request.dyads
    model_ids = request.model_ids
    context = None

    if dyads is None:
        context = get_latest_dyad_context()
        if context is None:
            raise HTTPException(
                status_code=400,
                detail={
                    "code": "NO_DYADS",
                    "message": "Run dyad-matrix computation first",
                },
            )
        dyads = context.dyads
        if model_ids is None:
            model_ids = list(context.state.model_ids)

    if not dyads:
        raise HTTPException(
            status_code=400,
            detail={
                "code": "NO_DYADS",
                "message": "Dyad records are required for clustering",
            },
        )

    if model_ids is None:
        model_ids = sorted(
            {d["ego_id"] for d in dyads} | {d["alter_id"] for d in dyads}
        )

    if request.eps <= 0:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_EPS",
                "message": "eps must be positive",
            },
        )

    if request.min_samples < 2:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_MIN_SAMPLES",
                "message": "min_samples must be at least 2",
            },
        )

    if request.umap_components not in (2, 3):
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_UMAP_COMPONENTS",
                "message": "umap_components must be 2 or 3",
            },
        )

    if request.umap_n_neighbors < 2:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_UMAP_N_NEIGHBORS",
                "message": "umap_n_neighbors must be at least 2",
            },
        )

    if not 0 <= request.umap_min_dist <= 1:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_UMAP_MIN_DIST",
                "message": "umap_min_dist must be between 0 and 1",
            },
        )

    if not 0 <= request.internal_threshold <= 1:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_INTERNAL_THRESHOLD",
                "message": "internal_threshold must be between 0 and 1",
            },
        )

    if not 0 <= request.prior_threshold <= 1:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_PRIOR_THRESHOLD",
                "message": "prior_threshold must be between 0 and 1",
            },
        )

    if not request.score_field:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_SCORE_FIELD",
                "message": "score_field must be a non-empty string",
            },
        )

    if request.score_field in ("mas_compatible", "identified_compatible"):
        if request.dyads is not None and all(request.score_field in d for d in dyads):
            pass
        elif request.exposure is None or request.outcome is None:
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "MISSING_EXPOSURE_OUTCOME",
                    "message": (
                        f"score_field '{request.score_field}' requires "
                        "exposure and outcome to be specified"
                    ),
                },
            )
        if context is not None and request.dyads is None:
            node_names = set(
                context.registry.data[context.registry.data["type"] == "node"][
                    "source"
                ].tolist()
            )
            if request.exposure == request.outcome:
                raise HTTPException(
                    status_code=422,
                    detail={
                        "code": "INVALID_CAUSAL_TARGET",
                        "message": "Exposure and outcome must be distinct nodes",
                    },
                )
            invalid = [
                n for n in (request.exposure, request.outcome) if n not in node_names
            ]
            if invalid:
                raise HTTPException(
                    status_code=422,
                    detail={
                        "code": "INVALID_CAUSAL_TARGET",
                        "message": f"Invalid exposure/outcome node(s): {invalid}",
                    },
                )
            try:
                dyads = engine.compare_pairs(
                    context.state,
                    context.registry,
                    model_ids,
                    mode="full",
                    causal_wrapper=CausalWrapper(),
                    identification_wrapper=(
                        IdentificationWrapper()
                        if request.score_field == "identified_compatible"
                        else None
                    ),
                    exposure=request.exposure,
                    outcome=request.outcome,
                )
            except CausalError as e:
                raise HTTPException(
                    status_code=500,
                    detail={"code": "CAUSAL_ERROR", "message": str(e)},
                )
        elif any(request.score_field not in d for d in dyads):
            raise HTTPException(
                status_code=422,
                detail={
                    "code": "MISSING_CAUSAL_SCORE_FIELD",
                    "message": (
                        f"dyads must include '{request.score_field}' when using "
                        "a causal score field"
                    ),
                },
            )

    if len(model_ids) < 2:
        raise HTTPException(
            status_code=400,
            detail={
                "code": "INSUFFICIENT_MODELS",
                "message": "At least 2 models required for clustering",
            },
        )

    if request.prior_model_id is not None and request.prior_model_id not in model_ids:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "MODEL_NOT_FOUND",
                "message": f"Model {request.prior_model_id} not found",
            },
        )

    try:
        clustering_engine = ClusteringEngine(
            umap_components=request.umap_components,
            umap_n_neighbors=request.umap_n_neighbors,
            umap_min_dist=request.umap_min_dist,
            umap_metric=request.umap_metric,
            eps=request.eps,
            min_samples=request.min_samples,
            random_state=request.random_state,
            score_field=request.score_field,
        )
        result = clustering_engine.detect_clusters(dyads, model_ids)
    except ClusteringError as e:
        raise HTTPException(
            status_code=422,
            detail={"code": "CLUSTERING_ERROR", "message": str(e)},
        )

    response_data = {
        "cluster_assignments": result["cluster_assignments"],
        "cluster_summaries": result["cluster_summaries"],
        "embedding_2d": result["embedding_2d"],
        "model_count": result["model_count"],
        "cluster_count": result["cluster_count"],
        "noise_count": result["noise_count"],
        "umap_components": request.umap_components,
        "eps": request.eps,
        "min_samples": request.min_samples,
        "score_field": request.score_field,
        "metric_unique_values": result["metric_unique_values"],
        "all_pairs_compatible": result["all_pairs_compatible"],
        "all_pairs_incompatible": result["all_pairs_incompatible"],
        "profile_variance": result["profile_variance"],
        "degenerate_metric": result["degenerate_metric"],
    }

    ghost_clusters = []
    if request.prior_model_id is not None:
        try:
            detector = GhostDetector(
                internal_threshold=request.internal_threshold,
                prior_threshold=request.prior_threshold,
                score_field=request.score_field,
            )
            contrast_results = detector.contrast(
                result["cluster_summaries"],
                result["cluster_assignments"],
                request.prior_model_id,
                dyads,
                model_ids,
            )
            ghost_summary = detector.get_ghost_summary(contrast_results)
            ghost_clusters = ghost_summary["ghost_clusters"]
            response_data["prior_model_id"] = request.prior_model_id
            response_data["ghost_count"] = len(ghost_clusters)
        except GhostError as e:
            raise HTTPException(
                status_code=422,
                detail={"code": "GHOST_ERROR", "message": str(e)},
            )

    response_data["ghost_clusters"] = ghost_clusters

    return {"status": "success", "data": response_data}


@router.post("/simulate")
async def simulate(request: SimulateRequest):
    is_seeded = request.registry_data is not None and request.state_data is not None

    try:
        suite = SimulationSuite(random_state=request.random_state)
        enforce_thresholds = request.enforce_thresholds
        if enforce_thresholds is None:
            enforce_thresholds = not is_seeded

        common = dict(
            n_models=request.n_models,
            n_components=request.n_components,
            compatibility_metric=request.compatibility_metric,
            enforce_thresholds=enforce_thresholds,
            exposure=request.exposure,
            outcome=request.outcome,
            include_bidirectional=request.include_bidirectional,
        )

        if is_seeded:
            registry_records = [r.model_dump() for r in request.registry_data]
            state_records = [r.model_dump() for r in request.state_data]
            common.update(
                registry_data=registry_records,
                state_data=state_records,
                sample_n=request.sample_n,
            )

        if request.include_plot_data:
            common.update(
                include_plot_data=True,
                plot_sample_n=request.plot_sample_n,
                pair_sample_n=request.pair_sample_n,
            )

        if request.scenario in ("lynchpin_of_certainty", "crux_of_certainty"):
            common.update(
                crux_mode=request.crux_mode,
                global_status=request.global_status,
            )

        if request.scenario == "illusion_of_precision":
            result = suite.run_scenario(
                scenario=request.scenario,
                **common,
            )
        elif request.scenario in ("lynchpin_of_certainty", "crux_of_certainty"):
            result = suite.run_scenario(
                scenario=request.scenario,
                **common,
                n_zones=request.n_zones,
                noise_fraction=request.noise_fraction,
            )
        elif request.scenario == "ghost_discovery":
            result = suite.run_scenario(
                scenario=request.scenario,
                **common,
                mainstream_fraction=request.mainstream_fraction,
                ghost_fraction=request.ghost_fraction,
                eps=request.eps,
                min_samples=request.min_samples,
                internal_threshold=request.internal_threshold,
                prior_threshold=request.prior_threshold,
                divergent_fraction=request.divergent_fraction,
            )
        else:
            result = suite.run_scenario(scenario=request.scenario, **common)
    except SimulationInputError as e:
        return JSONResponse(
            status_code=422,
            content={
                "status": "error",
                "code": "INVALID_SIMULATION_INPUT",
                "message": str(e),
            },
        )
    except DeltaUError as e:
        return JSONResponse(
            status_code=422,
            content={
                "status": "error",
                "code": "DELTA_U_ERROR",
                "message": str(e),
            },
        )
    except SimulationError as e:
        return JSONResponse(
            status_code=500,
            content={
                "status": "error",
                "code": "SIMULATION_ERROR",
                "message": str(e),
            },
        )

    return {
        "status": "success",
        "data": {
            "scenario": result["scenario"],
            "n_models": result["n_models"],
            "n_components": result["n_components"],
            "results": result["results"],
            "artifacts": result["artifacts"],
        },
    }


# --- Symbolic endpoints --------------------------------------------------------


def _universe_from_request(request: SymbolicUniverseRequest):
    if request.registry_data is not None:
        import pandas as pd

        recs = [r.model_dump() for r in request.registry_data]
        reg = pd.DataFrame(recs)
        universe = sym_build_universe(
            registry=reg,
            exposure=request.exposure,
            outcome=request.outcome,
        )
    elif request.nodes is not None:
        node_names = []
        timing: dict[str, int | None] = {}
        for n in request.nodes:
            if isinstance(n, str):
                node_names.append(n)
                timing[n] = None
            else:
                node_names.append(n.name)
                timing[n.name] = n.timing
        if (
            request.preferred_model is not None
            and request.preferred_model.timing is not None
        ):
            for k, v in request.preferred_model.timing.items():
                timing[k] = v
        universe = sym_build_universe(
            nodes=node_names,
            timing=timing,
            exposure=request.exposure or "",
            outcome=request.outcome or "",
        )
    else:
        raise HTTPException(
            status_code=400,
            detail={
                "code": "INVALID_UNIVERSE",
                "message": "registry_data or nodes required",
            },
        )

    if universe.fixed_causal_edges:
        if request.constraints is not None:
            for c_record in request.constraints:
                c = c_record if isinstance(c_record, dict) else c_record.model_dump()
                if c.get("status") == "non-causal":
                    edge = universe.comp_to_edge.get(c.get("comp_id"))
                    if edge and edge in universe.fixed_causal_edges:
                        raise HTTPException(
                            status_code=422,
                            detail={
                                "code": "FIXED_EDGE_CONFLICT",
                                "message": (
                                    f"Cannot set fixed edge {edge[0]} -> {edge[1]} "
                                    f"to non-causal"
                                ),
                            },
                        )

        if request.absent_nodes is not None:
            for src, tgt in universe.fixed_causal_edges:
                if src in request.absent_nodes or tgt in request.absent_nodes:
                    raise HTTPException(
                        status_code=422,
                        detail={
                            "code": "FIXED_EDGE_CONFLICT",
                            "message": (
                                f"Cannot mark node '{src}' or '{tgt}' as absent: "
                                f"fixed edge {src} -> {tgt} requires both endpoints"
                            ),
                        },
                    )

    return universe


def _constraints_from_request(request: SymbolicUniverseRequest, universe):
    terms = []
    fixed = sym_fixed_constraints(universe)
    if not isinstance(fixed, type(True)) or fixed is not True:
        terms.append(fixed)
    if request.preferred_model is not None:
        pm = request.preferred_model
        dag_dict = {
            "nodes": [n if isinstance(n, str) else n.name for n in pm.nodes],
            "edges": pm.edges,
            "exposure": pm.exposure,
            "outcome": pm.outcome,
        }
        terms.append(
            sym_dag_constraints(
                dag_dict, universe, unmentioned_edges=pm.unmentioned_edges
            )
        )
    if request.constraints is not None:
        edge_records = [c.model_dump() for c in request.constraints]
        terms.append(sym_edge_constraints(edge_records, universe))
    if request.absent_nodes is not None:
        terms.append(sym_absence_constraints(universe, set(request.absent_nodes)))
    if not terms:
        from symbolic.formula import TRUE

        return TRUE
    if len(terms) == 1:
        return terms[0]
    from symbolic.formula import And

    return And(*terms)


@router.post("/symbolic/universe")
async def symbolic_universe(request: SymbolicUniverseRequest):
    universe = _universe_from_request(request)
    constraints = _constraints_from_request(request, universe)
    update_latest_symbolic_context(universe=universe, constraints=constraints)
    return {
        "status": "success",
        "data": {
            "nodes": list(universe.nodes),
            "exposure": universe.exposure,
            "outcome": universe.outcome,
            "edge_count": universe.edge_count,
            "edge_variables": [
                {"source": s, "target": t, "name": ev.name, "comp_id": ev.comp_id}
                for (s, t), ev in universe.edge_vars.items()
            ],
        },
    }


@router.post("/symbolic/query-classes")
async def symbolic_query_classes(request: SymbolicQueryRequest):
    universe = _universe_from_request(request)
    constraints = _constraints_from_request(request, universe)
    result = sym_build_query_classes(
        universe,
        constraints,
        mode=request.mode,
        n_samples=request.n_samples,
        fallback=request.fallback,
        signature_policy=request.signature_policy,
        max_signature_atoms=request.max_signature_atoms,
        max_path_len=request.max_path_len,
        max_paths=request.max_paths,
        max_compile_seconds=request.max_compile_seconds,
        max_count_seconds=request.max_count_seconds,
        max_bdd_nodes=request.max_bdd_nodes,
    )
    class_list = []
    for c in result.classes:
        class_list.append(
            {
                "class_id": c.class_id,
                "mass": _json_mass(c.mass),
                "proportion": round(c.proportion, 6),
                "adjustment_identifiable": c.atom_values.get(
                    "adjustment_identifiable", False
                ),
                "empty_adjustment_valid": c.atom_values.get(
                    "empty_adjustment_valid", False
                ),
                "signature": c.signature,
                "atom_values": c.atom_values,
            }
        )
    return {
        "status": "success",
        "data": {
            "mode": result.mode,
            "exact": result.exact,
            "edge_variable_count": result.edge_variable_count,
            "candidate_adjustment_set_count": result.candidate_adjustment_set_count,
            "signature_atom_count": result.signature_atom_count,
            "total_mass": _json_mass(result.total_mass),
            "classes": class_list,
            "warnings": result.warnings,
        },
    }


@router.post("/symbolic/compare")
async def symbolic_compare(request: SymbolicCompareRequest):
    universe = _universe_from_request(request)
    constraints = _constraints_from_request(request, universe)
    engine = SymbolicCompatibilityEngine()

    theory_a_dag = {
        "nodes": [n if isinstance(n, str) else n.name for n in request.theory_a.nodes],
        "edges": request.theory_a.edges,
        "exposure": request.theory_a.exposure,
        "outcome": request.theory_a.outcome,
    }
    a_constraints = sym_dag_constraints(
        theory_a_dag, universe, unmentioned_edges=request.theory_a.unmentioned_edges
    )

    fixed = sym_fixed_constraints(universe)
    from symbolic.formula import And as _And
    from symbolic.formula import TRUE as _TRUE

    if not isinstance(fixed, type(True)) or fixed is not _TRUE:
        a_constraints = _And(fixed, a_constraints)

    if request.theory_b is not None:
        theory_b_dag = {
            "nodes": [
                n if isinstance(n, str) else n.name for n in request.theory_b.nodes
            ],
            "edges": request.theory_b.edges,
            "exposure": request.theory_b.exposure,
            "outcome": request.theory_b.outcome,
        }
        b_constraints = sym_dag_constraints(
            theory_b_dag, universe, unmentioned_edges=request.theory_b.unmentioned_edges
        )
        if not isinstance(fixed, type(True)) or fixed is not _TRUE:
            b_constraints = _And(fixed, b_constraints)
        comparison = engine.compare_theories(universe, a_constraints, b_constraints)
        return {
            "status": "success",
            "data": {
                "adjustment_identifiable_compatible": comparison[
                    "adjustment_identifiable_compatible"
                ],
                "a_signature": _sig_to_dict(comparison["a_signature"]),
                "b_signature": _sig_to_dict(comparison["b_signature"]),
            },
        }

    comparison = engine.compare_preferred_to_multiverse(
        universe, a_constraints, constraints
    )
    class_list = []
    for c in comparison["classes"]:
        class_list.append(
            {
                "class_id": c.class_id,
                "mass": c.mass,
                "adjustment_identifiable": c.adjustment_identifiable,
            }
        )
    return {
        "status": "success",
        "data": {
            "preferred_adjustment_identifiable": comparison.get(
                "preferred_adjustment_identifiable"
            ),
            "classes": class_list,
        },
    }


def _sig_to_dict(sig):
    return {
        "adjustment_identifiable": sig.adjustment_identifiable,
        "possibly_adjustment_identifiable": sig.possibly_adjustment_identifiable,
        "necessarily_adjustment_identifiable": sig.necessarily_adjustment_identifiable,
        "adjustment_identifiable_mass": sig.adjustment_identifiable_mass,
        "empty_adjustment_valid": sig.empty_adjustment_valid,
    }


@router.post("/symbolic/delta-u")
async def symbolic_delta_u(request: SymbolicDeltaURequest):
    universe = _universe_from_request(request)
    constraints = _constraints_from_request(request, universe)
    engine = SymbolicDeltaUEngine()
    results = engine.compute_delta_u(
        universe,
        constraints,
        top_k=request.top_k,
        mode=request.mode,
        n_samples=request.n_samples,
        fallback=request.fallback,
        signature_policy=request.signature_policy,
        max_signature_atoms=request.max_signature_atoms,
    )
    result_mode = results[0]["mode"] if results else f"symbolic_{request.mode}"
    exact = (
        all(r.get("exact", False) for r in results)
        if results
        else request.mode == "full"
    )
    return {
        "status": "success",
        "data": {
            "mode": result_mode,
            "exact": exact,
            "results": results,
        },
    }


@router.post("/symbolic/simulate")
async def symbolic_simulate(request: SymbolicSimulateRequest):
    engine = SymbolicSimulationEngine()
    if request.scenario == "illusion_of_precision":
        result = engine.run_illusion_of_precision(
            n_shared_edges=request.n_shared_edges,
            n_critical_unknown=request.n_critical_unknown,
            seed=request.seed,
            mode=request.mode,
            n_samples=request.n_samples,
            fallback=request.fallback,
            signature_policy=request.signature_policy,
            template_size=request.template_size,
            max_signature_atoms=request.max_signature_atoms,
        )
    elif request.scenario == "lynchpin_of_certainty":
        result = engine.run_lynchpin_of_certainty(
            n_zones=request.n_zones,
            n_edges_per_zone=request.n_edges_per_zone,
            seed=request.seed,
            mode=request.mode,
            n_samples=request.n_samples,
            fallback=request.fallback,
            signature_policy=request.signature_policy,
            template_size=request.template_size,
            max_signature_atoms=request.max_signature_atoms,
        )
    elif request.scenario == "ghost_discovery":
        result = engine.run_ghost_discovery(
            n_mainstream=request.n_mainstream,
            n_ghost=request.n_ghost,
            n_noise=request.n_noise,
            seed=request.seed,
            mode=request.mode,
            n_samples=request.n_samples,
            fallback=request.fallback,
            signature_policy=request.signature_policy,
            template_size=request.template_size,
            max_signature_atoms=request.max_signature_atoms,
        )
    else:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_SCENARIO",
                "message": f"Unknown scenario: {request.scenario}",
            },
        )

    return {
        "status": "success",
        "data": result,
    }


@router.post("/shutdown")
async def shutdown():
    """Gracefully shut down the theory engine."""
    loop = asyncio.get_running_loop()
    loop.call_later(0.3, lambda: os.kill(os.getpid(), signal.SIGTERM))
    return {"status": "success", "data": {"message": "Shutting down..."}}
