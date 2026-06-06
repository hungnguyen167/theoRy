"""Ghost cluster detection via contrast analysis against user prior."""

from __future__ import annotations

import logging
from typing import Any

logger = logging.getLogger(__name__)


class GhostError(Exception):
    """Raised when ghost detection operations fail."""
    pass


class GhostDetector:
    """Identify ghost clusters by contrasting against a user prior model."""

    def __init__(
        self,
        internal_threshold: float = 0.6,
        prior_threshold: float = 0.4,
        score_field: str = "similarity_rate",
    ):
        """Initialize ghost detector.

        Args:
            internal_threshold: Minimum internal_compatibility for a cluster
                to be considered internally consistent.
            prior_threshold: Minimum prior_compatibility for a cluster to be
                considered mainstream (aligned with prior).
            score_field: Dyad field used as the compatibility score.
        """
        if not 0.0 <= internal_threshold <= 1.0:
            raise GhostError("internal_threshold must be between 0 and 1")
        if not 0.0 <= prior_threshold <= 1.0:
            raise GhostError("prior_threshold must be between 0 and 1")
        if not score_field:
            raise GhostError("score_field must be a non-empty string")

        self._internal_threshold = internal_threshold
        self._prior_threshold = prior_threshold
        self._score_field = score_field

    def contrast(
        self,
        cluster_summaries: list[dict[str, Any]],
        cluster_assignments: list[dict[str, Any]],
        prior_model_id: str,
        dyads: list[dict[str, Any]],
        model_ids: list[str],
    ) -> list[dict[str, Any]]:
        """Perform contrast analysis between clusters and prior model.

        Args:
            cluster_summaries: List of cluster summary dicts.
            cluster_assignments: List of model-to-cluster assignments.
            prior_model_id: ID of the user's prior model.
            dyads: Dyad compatibility records.
            model_ids: List of all model IDs.

        Returns:
            List of contrast result dicts with labels and metrics.
        """
        if prior_model_id not in model_ids:
            raise GhostError(
                f"Prior model {prior_model_id!r} not found in model IDs"
            )

        dyad_lookup = {}
        for d in dyads:
            key = (d["ego_id"], d["alter_id"])
            dyad_lookup[key] = self._dyad_score(d)

        results = []
        for summary in cluster_summaries:
            cluster_id = summary["cluster_id"]
            cluster_models = [
                a["model_id"]
                for a in cluster_assignments
                if a["cluster_id"] == cluster_id
            ]

            scores = []
            for mid in cluster_models:
                fwd = dyad_lookup.get((prior_model_id, mid))
                rev = dyad_lookup.get((mid, prior_model_id))
                if fwd is not None:
                    scores.append(fwd)
                if rev is not None:
                    scores.append(rev)

            prior_compatibility = round(
                sum(scores) / len(scores) if scores else 0.0, 6
            )
            prior_distance = round(1.0 - prior_compatibility, 6)
            internal = summary["internal_compatibility"]

            if internal < self._internal_threshold:
                label = "fragmented"
            elif prior_compatibility < self._prior_threshold:
                label = "ghost"
            else:
                label = "mainstream"

            representative = self._representative_models(
                cluster_models, dyad_lookup, top_k=3
            )

            results.append({
                "cluster_id": cluster_id,
                "model_count": summary["model_count"],
                "internal_compatibility": internal,
                "prior_compatibility": prior_compatibility,
                "prior_distance": prior_distance,
                "label": label,
                "representative_models": representative,
            })

        return results

    def _dyad_score(self, dyad: dict[str, Any]) -> float:
        """Extract a numeric compatibility score from a dyad record."""
        value = dyad.get(self._score_field, dyad.get("similarity_rate", 0.0))
        if value is None:
            return 0.0
        if isinstance(value, bool):
            return 1.0 if value else 0.0
        try:
            return float(value)
        except (TypeError, ValueError):
            return 0.0

    def _representative_models(
        self,
        cluster_models: list[str],
        dyad_lookup: dict[tuple[str, str], float],
        top_k: int = 3,
    ) -> list[str]:
        """Select top-K representative models by centrality.

        Args:
            cluster_models: List of model IDs in the cluster.
            dyad_lookup: Dict mapping (ego, alter) to similarity_rate.
            top_k: Number of representative models to select.

        Returns:
            List of representative model IDs.
        """
        if len(cluster_models) <= top_k:
            return sorted(cluster_models)

        centrality = {}
        for mid in cluster_models:
            scores = []
            for other in cluster_models:
                if other == mid:
                    continue
                fwd = dyad_lookup.get((mid, other))
                rev = dyad_lookup.get((other, mid))
                if fwd is not None:
                    scores.append(fwd)
                if rev is not None:
                    scores.append(rev)
            centrality[mid] = sum(scores) / len(scores) if scores else 0.0

        ranked = sorted(centrality, key=lambda m: (-centrality[m], m))
        return ranked[:top_k]

    def get_ghost_summary(
        self,
        contrast_results: list[dict[str, Any]],
    ) -> dict[str, Any]:
        """Generate summary of ghost clusters from contrast results.

        Args:
            contrast_results: List of contrast result dicts.

        Returns:
            Dict with ghost_clusters, total_ghost_models, top_ghost_cluster.
        """
        ghosts = [r for r in contrast_results if r["label"] == "ghost"]
        total_ghost_models = sum(g["model_count"] for g in ghosts)

        if not ghosts:
            logger.info(
                "No ghost clusters detected - all zones align with user prior"
            )

        top_ghost = None
        if ghosts:
            top_ghost = max(ghosts, key=lambda g: g["internal_compatibility"])

        return {
            "ghost_clusters": ghosts,
            "total_ghost_models": total_ghost_models,
            "top_ghost_cluster": top_ghost,
        }
