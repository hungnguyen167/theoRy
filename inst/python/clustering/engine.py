"""Compatibility profile clustering engine for ghost cluster detection."""

from __future__ import annotations

import logging
from typing import Any

import numpy as np
import umap
from sklearn.cluster import DBSCAN

logger = logging.getLogger(__name__)


class ClusteringError(Exception):
    """Raised when clustering operations fail."""
    pass


class ClusteringEngine:
    """Build compatibility profiles and detect clusters using UMAP + DBSCAN."""

    def __init__(
        self,
        umap_components: int = 2,
        umap_n_neighbors: int = 15,
        umap_min_dist: float = 0.1,
        umap_metric: str = "euclidean",
        eps: float = 0.5,
        min_samples: int = 5,
        random_state: int | None = 42,
        score_field: str = "similarity_rate",
    ):
        """Initialize clustering engine.

        Args:
            umap_components: Number of dimensions for UMAP reduction (2 or 3).
            umap_n_neighbors: UMAP n_neighbors parameter.
            umap_min_dist: UMAP min_dist parameter.
            umap_metric: Distance metric for UMAP.
            eps: DBSCAN eps parameter.
            min_samples: DBSCAN min_samples parameter.
            random_state: Random seed for reproducibility.
            score_field: Dyad field used as the compatibility score.
        """
        if umap_components not in (2, 3):
            raise ClusteringError("umap_components must be 2 or 3")
        if eps <= 0:
            raise ClusteringError("eps must be positive")
        if min_samples < 2:
            raise ClusteringError("min_samples must be at least 2")
        if umap_n_neighbors < 2:
            raise ClusteringError("umap_n_neighbors must be at least 2")
        if not 0.0 <= umap_min_dist <= 1.0:
            raise ClusteringError("umap_min_dist must be between 0 and 1")
        if not score_field:
            raise ClusteringError("score_field must be a non-empty string")

        self._umap_components = umap_components
        self._umap_n_neighbors = umap_n_neighbors
        self._umap_min_dist = umap_min_dist
        self._umap_metric = umap_metric
        self._eps = eps
        self._min_samples = min_samples
        self._random_state = random_state
        self._score_field = score_field

    def build_profiles(
        self,
        dyads: list[dict[str, Any]],
        model_ids: list[str] | None = None,
    ) -> tuple[np.ndarray, list[str]]:
        """Build compatibility profile vectors from dyad records.

        For each model m_i, creates a profile vector V_i of length N-1
        containing the configured compatibility score between m_i and every
        other model.

        Args:
            dyads: List of dyad records with ego_id, alter_id, and score field.
            model_ids: Optional list of model IDs. If None, extracted from dyads.

        Returns:
            Tuple of (profile_matrix, model_ids) where profile_matrix has
            shape (N, N-1).
        """
        if model_ids is None:
            model_ids = sorted({
                d["ego_id"] for d in dyads
            } | {
                d["alter_id"] for d in dyads
            })
        else:
            model_ids = sorted(model_ids)

        n = len(model_ids)
        if n < 2:
            raise ClusteringError("At least 2 models required for clustering")

        id_to_idx = {mid: i for i, mid in enumerate(model_ids)}
        profile_matrix = np.zeros((n, n - 1), dtype=np.float64)

        for dyad in dyads:
            ego = dyad["ego_id"]
            alter = dyad["alter_id"]

            if ego not in id_to_idx or alter not in id_to_idx:
                continue

            ego_idx = id_to_idx[ego]
            alter_idx = id_to_idx[alter]

            col = alter_idx if alter_idx < ego_idx else alter_idx - 1
            profile_matrix[ego_idx, col] = self._dyad_score(dyad)

        return profile_matrix, model_ids

    def _dyad_score(self, dyad: dict[str, Any]) -> float:
        """Extract a numeric compatibility score from a dyad record."""
        value = dyad.get(self._score_field, dyad.get("similarity_rate", 0.0))
        if value is None:
            return 0.0
        if isinstance(value, bool):
            return 1.0 if value else 0.0
        try:
            score = float(value)
        except (TypeError, ValueError):
            return 0.0
        if np.isnan(score):
            return 0.0
        return score

    def _build_aligned_profiles(
        self,
        dyads: list[dict[str, Any]],
        model_ids: list[str],
    ) -> np.ndarray:
        """Build N x N model-aligned profiles for distance-based clustering."""
        n = len(model_ids)
        id_to_idx = {mid: i for i, mid in enumerate(model_ids)}
        profiles = np.zeros((n, n), dtype=np.float64)
        np.fill_diagonal(profiles, 1.0)

        for dyad in dyads:
            ego = dyad["ego_id"]
            alter = dyad["alter_id"]
            if ego not in id_to_idx or alter not in id_to_idx:
                continue
            profiles[id_to_idx[ego], id_to_idx[alter]] = self._dyad_score(dyad)

        return profiles

    def _reduce_umap(self, profiles: np.ndarray) -> np.ndarray:
        """Reduce dimensionality using UMAP.

        Args:
            profiles: Profile matrix of shape (N, N-1).

        Returns:
            Embedding of shape (N, umap_components).
        """
        n_samples = len(profiles)

        if n_samples <= self._umap_components + 1:
            padded = np.zeros(
                (n_samples, self._umap_components), dtype=np.float64
            )
            cols = min(profiles.shape[1], self._umap_components)
            padded[:, :cols] = profiles[:, :cols]
            return padded

        n_neighbors = min(self._umap_n_neighbors, n_samples - 1)

        if n_neighbors < 2:
            n_neighbors = 2

        reducer = umap.UMAP(
            n_components=self._umap_components,
            n_neighbors=n_neighbors,
            min_dist=self._umap_min_dist,
            metric=self._umap_metric,
            random_state=self._random_state,
        )

        return reducer.fit_transform(profiles)

    def _cluster_dbscan(self, embedding: np.ndarray) -> np.ndarray:
        """Cluster embedding using DBSCAN.

        Args:
            embedding: Reduced embedding of shape (N, umap_components).

        Returns:
            Array of cluster labels (-1 for noise).
        """
        clusterer = DBSCAN(eps=self._eps, min_samples=self._min_samples)
        return clusterer.fit_predict(embedding)

    def _compute_cluster_summaries(
        self,
        labels: np.ndarray,
        embedding: np.ndarray,
        model_ids: list[str],
        dyads: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """Compute summary statistics for each cluster.

        Args:
            labels: DBSCAN labels (-1 for noise).
            embedding: UMAP embedding.
            model_ids: List of model IDs.
            dyads: Dyad records for computing internal compatibility.

        Returns:
            List of cluster summary dicts.
        """
        unique_labels = sorted(set(labels))
        unique_labels = [lbl for lbl in unique_labels if lbl != -1]

        if not unique_labels:
            return []

        dyad_lookup = {}
        for d in dyads:
            key = (d["ego_id"], d["alter_id"])
            dyad_lookup[key] = self._dyad_score(d)

        summaries = []
        for lbl in unique_labels:
            cluster_models = [
                model_ids[i] for i, label in enumerate(labels) if label == lbl
            ]
            cluster_indices = [i for i, label in enumerate(labels) if label == lbl]

            centroid = embedding[cluster_indices].mean(axis=0).tolist()

            scores = []
            for m1 in cluster_models:
                for m2 in cluster_models:
                    if m1 == m2:
                        continue
                    score = dyad_lookup.get((m1, m2), 0.0)
                    scores.append(score)

            internal_compat = (
                sum(scores) / len(scores) if scores else 0.0
            )

            cluster_id = f"Cluster_{lbl + 1:02d}"

            summaries.append({
                "cluster_id": cluster_id,
                "model_count": len(cluster_models),
                "internal_compatibility": round(internal_compat, 6),
                "centroid": [round(c, 6) for c in centroid],
            })

        return summaries

    def detect_clusters(
        self,
        dyads: list[dict[str, Any]],
        model_ids: list[str] | None = None,
    ) -> dict[str, Any]:
        """Execute full clustering pipeline.

        Args:
            dyads: Dyad compatibility records.
            model_ids: Optional list of model IDs.

        Returns:
            Dict with cluster_assignments, cluster_summaries, embedding_2d,
            model_count, cluster_count, noise_count.
        """
        profiles, model_ids = self.build_profiles(dyads, model_ids)
        n = len(model_ids)

        if n < self._min_samples:
            logger.warning(
                "Model count below min_samples - no clusters detected"
            )
            assignments = [
                {"model_id": mid, "cluster_id": None} for mid in model_ids
            ]
            embedding = self._reduce_umap(profiles)
            embedding_2d = {
                "model_ids": model_ids,
                "x": embedding[:, 0].tolist(),
                "y": embedding[:, 1].tolist(),
            }
            if self._umap_components >= 3:
                embedding_2d["z"] = embedding[:, 2].tolist()
            return {
                "cluster_assignments": assignments,
                "cluster_summaries": [],
                "embedding_2d": embedding_2d,
                "model_count": n,
                "cluster_count": 0,
                "noise_count": n,
            }

        embedding_profiles = self._build_aligned_profiles(dyads, model_ids)
        embedding = self._reduce_umap(embedding_profiles)
        embedding_2d = {
            "model_ids": model_ids,
            "x": embedding[:, 0].tolist(),
            "y": embedding[:, 1].tolist(),
        }
        if self._umap_components >= 3:
            embedding_2d["z"] = embedding[:, 2].tolist()
        labels = self._cluster_dbscan(embedding)

        assignments = []
        for i, (mid, lbl) in enumerate(zip(model_ids, labels)):
            if lbl == -1:
                assignments.append({"model_id": mid, "cluster_id": None})
            else:
                cluster_id = f"Cluster_{lbl + 1:02d}"
                assignments.append({"model_id": mid, "cluster_id": cluster_id})

        summaries = self._compute_cluster_summaries(
            labels, embedding, model_ids, dyads
        )

        noise_count = sum(1 for lbl in labels if lbl == -1)
        cluster_count = len(summaries)

        return {
            "cluster_assignments": assignments,
            "cluster_summaries": summaries,
            "embedding_2d": embedding_2d,
            "model_count": n,
            "cluster_count": cluster_count,
            "noise_count": noise_count,
        }
