from __future__ import annotations

import logging

from dyadic.engine import DyadicEngine
from registry.schema import ComponentRegistry
from state.tensor import StateTensor

logger = logging.getLogger(__name__)


class HybridDyadicEngine:
    def __init__(self, base_engine: DyadicEngine | None = None):
        self._base_engine = base_engine or DyadicEngine()

    def compare_batch(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
        *,
        mode: str = "basic",
        causal_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        return self._base_engine.compare_pairs(
            state, registry, model_ids=model_ids,
            mode=mode, causal_wrapper=causal_wrapper,
            exposure=exposure, outcome=outcome,
        )

    def compare_chunked(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
        *,
        mode: str = "basic",
        chunk_size_mb: int = 400,
        causal_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        if model_ids is None:
            model_ids = state.model_ids
        model_ids = sorted(model_ids)
        n = len(model_ids)
        total_pairs = n * (n - 1)

        bytes_per_dyad = 2048
        pairs_per_chunk = max(
            1, int((chunk_size_mb * 1024 * 1024) // bytes_per_dyad)
        )
        total_chunks = max(
            1, (total_pairs + pairs_per_chunk - 1) // pairs_per_chunk
        )

        results: list[dict] = []
        pair_count = 0

        for i in range(n):
            for j in range(n):
                if i == j:
                    continue
                if pair_count > 0 and pair_count % pairs_per_chunk == 0:
                    chunk_n = pair_count // pairs_per_chunk
                    pct = min(100.0, 100.0 * pair_count / total_pairs)
                    logger.info(
                        "Chunk %d/%d complete - %.0f%% done",
                        chunk_n, total_chunks, pct,
                    )

                result = self._base_engine.compare(
                    model_ids[i], model_ids[j], state, registry,
                    mode=mode, causal_wrapper=causal_wrapper,
                    exposure=exposure, outcome=outcome,
                )
                results.append(result)
                pair_count += 1

        if total_chunks > 0:
            logger.info(
                "Chunk %d/%d complete - 100%% done",
                total_chunks, total_chunks,
            )

        return results

    def compare_single_ref(
        self,
        reference_id: str,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
        *,
        mode: str = "basic",
        causal_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        if model_ids is None:
            model_ids = state.model_ids

        if reference_id not in model_ids:
            raise ValueError(
                f"Reference model {reference_id} not found"
            )

        other_ids = [m for m in model_ids if m != reference_id]
        results: list[dict] = []
        for other in other_ids:
            result = self._base_engine.compare(
                reference_id, other, state, registry,
                mode=mode, causal_wrapper=causal_wrapper,
                exposure=exposure, outcome=outcome,
            )
            results.append(result)
        return results

    def compare_two_stage(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        top_k: int = 100,
        model_ids: list[str] | None = None,
        *,
        causal_wrapper=None,
        identification_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> dict:
        if top_k <= 0:
            raise ValueError("top_k must be positive")

        basic_dyads = self._base_engine.compare_pairs(
            state, registry, model_ids=model_ids, mode="basic",
        )

        basic_dyads.sort(key=lambda d: d["similarity_rate"], reverse=True)

        actual_k = min(top_k, len(basic_dyads))
        detailed: list[dict] = []
        for i in range(actual_k):
            d = basic_dyads[i]
            if causal_wrapper is None:
                result = d
            else:
                result = self._base_engine.compare(
                    d["ego_id"], d["alter_id"], state, registry,
                    mode="full", causal_wrapper=causal_wrapper,
                    identification_wrapper=identification_wrapper,
                    exposure=exposure, outcome=outcome,
                )
            detailed.append(result)

        all_ids = model_ids if model_ids else state.model_ids

        return {
            "heatmap_summary": {
                "model_count": len(all_ids),
                "dyad_count": len(basic_dyads),
                "top_k": actual_k,
            },
            "heatmap_dyads": basic_dyads,
            "detailed_comparisons": detailed,
        }
