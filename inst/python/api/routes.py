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
)
from registry.builder import ComponentRegistryBuilder
from registry.loader import RegistryLoader
from registry.schema import RegistryError
from state.expander import ModelStateExpander
from state.tensor import StateError, StateTensor
from dyadic.engine import DyadicEngine, DyadicError
from dyadic.causal import CausalWrapper, CausalError
from dyadic.hybrid import HybridDyadicEngine
from api.session import update_latest_dyad_context, get_latest_dyad_context
from simulation.delta_u import DeltaUEngine, DeltaUError
from simulation.suite import SimulationSuite, SimulationError
from clustering.engine import ClusteringEngine, ClusteringError
from clustering.ghost import GhostDetector, GhostError

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

    try:
        state_records = ModelStateExpander.expand(
            registry,
            mode=request.mode,
            seed_claims=seed_claims,
            node_timing=request.node_timing,
            max_models=request.max_models,
            n_models=request.n_models,
            seed=request.seed,
            edge_statuses=request.edge_statuses,
        )
        model_ids = sorted({r["model_id"] for r in state_records})
        return {
            "status": "success",
            "data": {
                "state_data": state_records,
                "model_count": len(model_ids),
                "component_count": len(registry.data),
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
    if request.mode in ("full", "two-stage"):
        try:
            causal_wrapper = CausalWrapper()
        except CausalError as e:
            raise HTTPException(
                status_code=400,
                detail={"code": "CAUSAL_ERROR", "message": str(e)},
            )

    try:
        if request.mode == "basic":
            dyads = engine.compare_pairs(
                state, registry, mode="basic",
                exposure=exposure, outcome=outcome,
            )
        elif request.mode == "full":
            dyads = engine.compare_pairs(
                state, registry, mode="full", causal_wrapper=causal_wrapper,
                exposure=exposure, outcome=outcome,
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
                    request.reference_id, state, registry, mode="basic",
                    exposure=exposure, outcome=outcome,
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
                state, registry, top_k=top_k,
                causal_wrapper=causal_wrapper,
                exposure=exposure, outcome=outcome,
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
                registry, state_records, request.model_ids,
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

    scoring = request.scoring

    if scoring not in ("structural", "causal", "hybrid"):
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_SCORING",
                "message": "scoring must be structural, causal, or hybrid",
            },
        )

    sw = request.structural_weight
    cw = request.causal_weight

    if not 0.0 <= sw <= 1.0 or not 0.0 <= cw <= 1.0:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_SCORING_WEIGHTS",
                "message": "structural_weight and causal_weight must be in [0, 1]",
            },
        )

    if scoring == "hybrid" and sw + cw <= 0.0:
        raise HTTPException(
            status_code=422,
            detail={
                "code": "INVALID_SCORING_WEIGHTS",
                "message": "At least one scoring weight must be positive in hybrid mode",
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

    causal_metrics = (
        list(request.causal_metrics)
        if request.causal_metrics is not None
        else None
    )
    if scoring in ("causal", "hybrid") and causal_metrics is None:
        causal_metrics = ["mas_compatible", "full_compatible"]

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
            request.heatmap_threshold
            if request.heatmap_threshold is not None
            else 0.1
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
    if scoring in ("causal", "hybrid"):
        try:
            causal_wrapper = CausalWrapper()
        except CausalError as e:
            raise HTTPException(
                status_code=400,
                detail={"code": "CAUSAL_ERROR", "message": str(e)},
            )

    delta_engine = DeltaUEngine(
        dyadic_engine=engine,
        causal_wrapper=causal_wrapper,
        scoring=scoring,
        structural_weight=sw,
        causal_weight=cw,
        causal_metrics=causal_metrics,
        device=request.device,
        use_tensor_engine=request.use_tensor_engine,
        exposure=exposure,
        outcome=outcome,
    )
    computation_mode = request.mode

    if len(dyads) != len(state.model_ids) * (len(state.model_ids) - 1):
        dyads = engine.compare_pairs(
            state, registry, mode="basic",
        )

    try:
        if request.component_id is not None:
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
                request.component_id, state, dyads, registry,
            )
            response_data = {
                "result": single,
                "computation_mode": computation_mode,
            }
        else:
            rankings = delta_engine.rank_lynchpins(
                state, dyads, registry,
                top_k=request.top_k, mode=request.mode,
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
                    state, dyads, registry,
                    set_size=request.synergistic_set_size,
                    top_n=request.top_k,
                    search_strategy=synergy_search,
                    beam_width=synergy_beam,
                )
                response_data["synergistic_sets"] = synergistic

    except DeltaUError as e:
        raise HTTPException(
            status_code=500,
            detail={"code": "DELTA_U_ERROR", "message": str(e)},
        )
    except CausalError as e:
        raise HTTPException(
            status_code=500,
            detail={"code": "CAUSAL_DELTA_U_ERROR", "message": str(e)},
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

    response_data["scoring"] = scoring
    response_data["structural_weight"] = sw
    response_data["causal_weight"] = cw
    if causal_metrics is not None:
        response_data["causal_metrics"] = causal_metrics
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
        model_ids = sorted({
            d["ego_id"] for d in dyads
        } | {
            d["alter_id"] for d in dyads
        })

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
        engine = ClusteringEngine(
            umap_components=request.umap_components,
            umap_n_neighbors=request.umap_n_neighbors,
            umap_min_dist=request.umap_min_dist,
            umap_metric=request.umap_metric,
            eps=request.eps,
            min_samples=request.min_samples,
            random_state=request.random_state,
            score_field=request.score_field,
        )
        result = engine.detect_clusters(dyads, model_ids)
    except ClusteringError as e:
        raise HTTPException(
            status_code=500,
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
                status_code=500,
                detail={"code": "GHOST_ERROR", "message": str(e)},
            )

    response_data["ghost_clusters"] = ghost_clusters

    return {"status": "success", "data": response_data}


@router.post("/simulate")
async def simulate(request: SimulateRequest):
    try:
        suite = SimulationSuite(random_state=request.random_state)
        result = suite.run_scenario(
            scenario=request.scenario,
            n_models=request.n_models,
            n_components=request.n_components,
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


@router.post("/shutdown")
async def shutdown():
    """Gracefully shut down the theory engine."""
    loop = asyncio.get_running_loop()
    loop.call_later(0.3, lambda: os.kill(os.getpid(), signal.SIGTERM))
    return {"status": "success", "data": {"message": "Shutting down..."}}
