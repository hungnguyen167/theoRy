"""Tests for clustering engine and ghost detection."""

import numpy as np
import pytest

from clustering.engine import ClusteringEngine, ClusteringError
from clustering.ghost import GhostDetector, GhostError


def _make_dyads(model_ids, similarity_map):
    """Helper to create dyad records from a similarity map."""
    dyads = []
    for ego in model_ids:
        for alter in model_ids:
            if ego == alter:
                continue
            score = similarity_map.get((ego, alter), 0.0)
            dyads.append(
                {
                    "dyad_id": f"{ego}__{alter}",
                    "ego_id": ego,
                    "alter_id": alter,
                    "similarity_rate": score,
                }
            )
    return dyads


class TestBuildProfiles:
    def test_build_profiles_shape_and_values(self):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = _make_dyads(
            model_ids,
            {
                ("M0001", "M0002"): 0.8,
                ("M0002", "M0001"): 0.8,
                ("M0001", "M0003"): 0.5,
                ("M0003", "M0001"): 0.5,
                ("M0002", "M0003"): 0.3,
                ("M0003", "M0002"): 0.3,
            },
        )

        engine = ClusteringEngine()
        profiles, ids = engine.build_profiles(dyads)

        assert profiles.shape == (3, 2)
        assert ids == ["M0001", "M0002", "M0003"]
        assert profiles[0, 0] == 0.8
        assert profiles[0, 1] == 0.5
        assert profiles[1, 0] == 0.8
        assert profiles[1, 1] == 0.3

    def test_build_profiles_requires_complete_directed_dyads(self):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = [
            {
                "dyad_id": "M0001__M0002",
                "ego_id": "M0001",
                "alter_id": "M0002",
                "similarity_rate": 0.9,
            },
        ]

        engine = ClusteringEngine()
        with pytest.raises(ClusteringError, match="missing 5 directed pair"):
            engine.build_profiles(dyads, model_ids)

    def test_build_profiles_uses_configured_score_field(self):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = _make_dyads(model_ids, {})
        for dyad in dyads:
            pair = (dyad["ego_id"], dyad["alter_id"])
            dyad["mas_compatible"] = pair == ("M0001", "M0002")

        engine = ClusteringEngine(score_field="mas_compatible")
        profiles, _ = engine.build_profiles(dyads, model_ids)

        assert profiles[0, 0] == 1.0
        assert profiles[0, 1] == 0.0

    @pytest.mark.parametrize("value", [None, np.nan, "not-a-number"])
    def test_build_profiles_rejects_unavailable_selected_scores(self, value):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = _make_dyads(model_ids, {})
        for dyad in dyads:
            dyad["mas_compatible"] = True
        dyads[0]["mas_compatible"] = value
        dyads[1]["mas_compatible"] = value

        engine = ClusteringEngine(score_field="mas_compatible")
        with pytest.raises(
            ClusteringError,
            match="mas_compatible.*unavailable for 2 dyad",
        ):
            engine.build_profiles(dyads, model_ids)

    def test_build_profiles_does_not_fallback_to_similarity_rate(self):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = _make_dyads(model_ids, {})

        engine = ClusteringEngine(score_field="identified_compatible")
        with pytest.raises(
            ClusteringError,
            match="identified_compatible.*unavailable for 6 dyad",
        ):
            engine.build_profiles(dyads, model_ids)

    @pytest.mark.parametrize("score_field", ["", "full", "compatible"])
    def test_invalid_score_field_raises(self, score_field):
        with pytest.raises(ClusteringError, match="score_field must be one of"):
            ClusteringEngine(score_field=score_field)

    def test_build_profiles_deterministic_model_order(self):
        model_ids = ["M0003", "M0001", "M0002"]
        dyads = _make_dyads(model_ids, {})

        engine = ClusteringEngine()
        _, ids = engine.build_profiles(dyads)

        assert ids == ["M0001", "M0002", "M0003"]

    def test_build_profiles_below_2_models_raises(self):
        engine = ClusteringEngine()
        with pytest.raises(ClusteringError, match="At least 2 models"):
            engine.build_profiles([], ["M0001"])


class TestUMAPReduction:
    def test_umap_reduction_output_shape(self):
        model_ids = [f"M{i:04d}" for i in range(1, 11)]
        dyads = _make_dyads(model_ids, {})

        engine = ClusteringEngine(umap_components=2, random_state=42)
        profiles, _ = engine.build_profiles(dyads, model_ids)
        embedding = engine._reduce_umap(profiles)

        assert embedding.shape == (10, 2)

    def test_umap_reduction_small_n_clamps_neighbors(self):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = _make_dyads(model_ids, {})

        engine = ClusteringEngine(umap_n_neighbors=15, random_state=42)
        profiles, _ = engine.build_profiles(dyads, model_ids)
        embedding = engine._reduce_umap(profiles)

        assert embedding.shape == (3, 2)


class TestDBSCANClustering:
    def test_dbscan_clustering_assigns_cluster_labels(self):
        embedding = np.array(
            [
                [0.0, 0.0],
                [0.1, 0.1],
                [0.2, 0.2],
                [5.0, 5.0],
                [5.1, 5.1],
                [5.2, 5.2],
            ]
        )

        engine = ClusteringEngine(eps=0.5, min_samples=2)
        labels = engine._cluster_dbscan(embedding)

        assert len(labels) == 6
        assert len(set(labels)) >= 2

    def test_dbscan_noise_labeled_as_null(self):
        embedding = np.array(
            [
                [0.0, 0.0],
                [0.1, 0.1],
                [10.0, 10.0],
            ]
        )

        engine = ClusteringEngine(eps=0.5, min_samples=3)
        labels = engine._cluster_dbscan(embedding)

        assert -1 in labels

    def test_dbscan_cluster_id_format(self):
        embedding = np.array(
            [
                [0.0, 0.0],
                [0.1, 0.1],
                [0.2, 0.2],
            ]
        )

        engine = ClusteringEngine(eps=1.0, min_samples=2)
        labels = engine._cluster_dbscan(embedding)

        for lbl in labels:
            if lbl != -1:
                cluster_id = f"Cluster_{lbl + 1:02d}"
                assert cluster_id.startswith("Cluster_")
                assert len(cluster_id) == 10


class TestDetectClusters:
    def test_detect_clusters_full_pipeline(self):
        model_ids = [f"M{i:04d}" for i in range(1, 11)]
        similarity_map = {}
        for i in range(1, 6):
            for j in range(1, 6):
                if i != j:
                    similarity_map[(f"M{i:04d}", f"M{j:04d}")] = 0.9
        for i in range(6, 11):
            for j in range(6, 11):
                if i != j:
                    similarity_map[(f"M{i:04d}", f"M{j:04d}")] = 0.9

        dyads = _make_dyads(model_ids, similarity_map)

        engine = ClusteringEngine(eps=0.5, min_samples=3, random_state=42)
        result = engine.detect_clusters(dyads, model_ids)

        assert "cluster_assignments" in result
        assert "cluster_summaries" in result
        assert "embedding_2d" in result
        assert result["model_count"] == 10
        assert result["score_field"] == "similarity_rate"
        assert result["metric_unique_values"] == [0.0, 0.9]
        assert result["all_pairs_compatible"] is False
        assert result["all_pairs_incompatible"] is False
        assert result["profile_variance"] > 0
        assert result["degenerate_metric"] is False

    def test_detect_clusters_degenerate_metric_skips_umap(self, monkeypatch):
        model_ids = [f"M{i:04d}" for i in range(1, 6)]
        dyads = _make_dyads(model_ids, {})
        engine = ClusteringEngine(min_samples=2, random_state=42)
        monkeypatch.setattr(
            engine,
            "_reduce_umap",
            lambda profiles: pytest.fail("UMAP must not run for degenerate data"),
        )

        result = engine.detect_clusters(dyads, model_ids)

        assert result["cluster_count"] == 0
        assert result["noise_count"] == 5
        assert result["metric_unique_values"] == [0.0]
        assert result["all_pairs_compatible"] is False
        assert result["all_pairs_incompatible"] is True
        assert result["profile_variance"] == 0.0
        assert result["degenerate_metric"] is True
        assert result["score_field"] == "similarity_rate"
        assert result["embedding_2d"]["x"] == [0.0] * 5
        assert result["embedding_2d"]["y"] == [0.0] * 5
        assert all(a["cluster_id"] is None for a in result["cluster_assignments"])

    def test_detect_clusters_all_compatible_diagnostics(self):
        model_ids = ["M0001", "M0002", "M0003"]
        similarity_map = {
            (ego, alter): 1.0
            for ego in model_ids
            for alter in model_ids
            if ego != alter
        }
        dyads = _make_dyads(model_ids, similarity_map)

        result = ClusteringEngine(min_samples=2).detect_clusters(dyads, model_ids)

        assert result["metric_unique_values"] == [1.0]
        assert result["all_pairs_compatible"] is True
        assert result["all_pairs_incompatible"] is False
        assert result["degenerate_metric"] is True

    def test_detect_clusters_small_n_all_noise(self):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = _make_dyads(model_ids, {})

        engine = ClusteringEngine(min_samples=5, random_state=42)
        result = engine.detect_clusters(dyads, model_ids)

        assert result["cluster_count"] == 0
        assert result["noise_count"] == 3
        assert all(a["cluster_id"] is None for a in result["cluster_assignments"])

    def test_detect_clusters_below_2_models_raises(self):
        engine = ClusteringEngine()
        with pytest.raises(ClusteringError, match="At least 2 models"):
            engine.detect_clusters([], ["M0001"])

    def test_cluster_summaries_internal_compatibility(self):
        model_ids = ["M0001", "M0002", "M0003"]
        similarity_map = {
            ("M0001", "M0002"): 0.8,
            ("M0002", "M0001"): 0.8,
            ("M0001", "M0003"): 0.5,
            ("M0003", "M0001"): 0.5,
            ("M0002", "M0003"): 0.3,
            ("M0003", "M0002"): 0.3,
        }
        dyads = _make_dyads(model_ids, similarity_map)

        engine = ClusteringEngine(eps=10.0, min_samples=2, random_state=42)
        result = engine.detect_clusters(dyads, model_ids)

        if result["cluster_summaries"]:
            summary = result["cluster_summaries"][0]
            assert "internal_compatibility" in summary
            assert 0.0 <= summary["internal_compatibility"] <= 1.0

    def test_cluster_summaries_centroid_matches_embedding(self):
        model_ids = [f"M{i:04d}" for i in range(1, 6)]
        dyads = _make_dyads(model_ids, {})

        engine = ClusteringEngine(eps=10.0, min_samples=2, random_state=42)
        result = engine.detect_clusters(dyads, model_ids)

        if result["cluster_summaries"]:
            summary = result["cluster_summaries"][0]
            assert "centroid" in summary
            assert len(summary["centroid"]) == 2


class TestGhostDetection:
    def test_ghost_identification_labels_ghost_cluster(self):
        summaries = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 5,
                "internal_compatibility": 0.85,
                "centroid": [0.0, 0.0],
            }
        ]
        assignments = [
            {"model_id": f"M{i:04d}", "cluster_id": "Cluster_01"} for i in range(1, 6)
        ]
        model_ids = ["M0001"] + [f"M{i:04d}" for i in range(1, 6)]

        similarity_map = {}
        for i in range(1, 6):
            similarity_map[("M0001", f"M{i:04d}")] = 0.2
            similarity_map[(f"M{i:04d}", "M0001")] = 0.2

        dyads = _make_dyads(model_ids, similarity_map)

        detector = GhostDetector(internal_threshold=0.6, prior_threshold=0.4)
        results = detector.contrast(summaries, assignments, "M0001", dyads, model_ids)

        assert len(results) == 1
        assert results[0]["label"] == "ghost"
        assert results[0]["prior_compatibility"] < 0.4

    def test_ghost_identification_labels_mainstream_cluster(self):
        summaries = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 3,
                "internal_compatibility": 0.8,
                "centroid": [0.0, 0.0],
            }
        ]
        assignments = [
            {"model_id": f"M{i:04d}", "cluster_id": "Cluster_01"} for i in range(1, 4)
        ]
        model_ids = ["M0001"] + [f"M{i:04d}" for i in range(1, 4)]

        similarity_map = {}
        for i in range(1, 4):
            similarity_map[("M0001", f"M{i:04d}")] = 0.75
            similarity_map[(f"M{i:04d}", "M0001")] = 0.75

        dyads = _make_dyads(model_ids, similarity_map)

        detector = GhostDetector()
        results = detector.contrast(summaries, assignments, "M0001", dyads, model_ids)

        assert results[0]["label"] == "mainstream"

    def test_ghost_identification_labels_fragmented_cluster(self):
        summaries = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 3,
                "internal_compatibility": 0.3,
                "centroid": [0.0, 0.0],
            }
        ]
        assignments = [
            {"model_id": f"M{i:04d}", "cluster_id": "Cluster_01"} for i in range(1, 4)
        ]
        model_ids = ["M0001"] + [f"M{i:04d}" for i in range(1, 4)]

        similarity_map = {}
        for i in range(1, 4):
            similarity_map[("M0001", f"M{i:04d}")] = 0.25
            similarity_map[(f"M{i:04d}", "M0001")] = 0.25

        dyads = _make_dyads(model_ids, similarity_map)

        detector = GhostDetector()
        results = detector.contrast(summaries, assignments, "M0001", dyads, model_ids)

        assert results[0]["label"] == "fragmented"

    def test_fragmented_label_takes_precedence_over_high_prior(self):
        summaries = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 3,
                "internal_compatibility": 0.3,
                "centroid": [0.0, 0.0],
            }
        ]
        assignments = [
            {"model_id": f"M{i:04d}", "cluster_id": "Cluster_01"} for i in range(1, 4)
        ]
        model_ids = ["M0001"] + [f"M{i:04d}" for i in range(1, 4)]

        similarity_map = {}
        for i in range(1, 4):
            similarity_map[("M0001", f"M{i:04d}")] = 0.9
            similarity_map[(f"M{i:04d}", "M0001")] = 0.9

        dyads = _make_dyads(model_ids, similarity_map)

        detector = GhostDetector()
        results = detector.contrast(summaries, assignments, "M0001", dyads, model_ids)

        assert results[0]["label"] == "fragmented"

    def test_ghost_summary_returns_ghost_clusters(self):
        contrast_results = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 5,
                "internal_compatibility": 0.85,
                "prior_compatibility": 0.2,
                "prior_distance": 0.8,
                "label": "ghost",
                "representative_models": ["M0002", "M0003", "M0004"],
            },
            {
                "cluster_id": "Cluster_02",
                "model_count": 3,
                "internal_compatibility": 0.8,
                "prior_compatibility": 0.75,
                "prior_distance": 0.25,
                "label": "mainstream",
                "representative_models": ["M0010", "M0011"],
            },
        ]

        detector = GhostDetector()
        summary = detector.get_ghost_summary(contrast_results)

        assert len(summary["ghost_clusters"]) == 1
        assert summary["total_ghost_models"] == 5
        assert summary["top_ghost_cluster"]["cluster_id"] == "Cluster_01"

    def test_ghost_summary_no_ghosts_returns_empty(self):
        contrast_results = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 3,
                "internal_compatibility": 0.8,
                "prior_compatibility": 0.75,
                "prior_distance": 0.25,
                "label": "mainstream",
                "representative_models": ["M0001", "M0002"],
            }
        ]

        detector = GhostDetector()
        summary = detector.get_ghost_summary(contrast_results)

        assert len(summary["ghost_clusters"]) == 0
        assert summary["total_ghost_models"] == 0
        assert summary["top_ghost_cluster"] is None

    def test_ghost_summary_top_ghost_cluster(self):
        contrast_results = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 5,
                "internal_compatibility": 0.7,
                "prior_compatibility": 0.2,
                "prior_distance": 0.8,
                "label": "ghost",
                "representative_models": [],
            },
            {
                "cluster_id": "Cluster_02",
                "model_count": 3,
                "internal_compatibility": 0.9,
                "prior_compatibility": 0.1,
                "prior_distance": 0.9,
                "label": "ghost",
                "representative_models": [],
            },
        ]

        detector = GhostDetector()
        summary = detector.get_ghost_summary(contrast_results)

        assert summary["top_ghost_cluster"]["cluster_id"] == "Cluster_02"

    def test_contrast_invalid_prior_model_raises(self):
        detector = GhostDetector()
        with pytest.raises(GhostError, match="not found"):
            detector.contrast([], [], "INVALID", [], ["M0001"])

    def test_representative_models_by_centrality(self):
        cluster_models = ["M0001", "M0002", "M0003", "M0004", "M0005"]
        dyad_lookup = {
            ("M0001", "M0002"): 0.9,
            ("M0002", "M0001"): 0.9,
            ("M0001", "M0003"): 0.8,
            ("M0003", "M0001"): 0.8,
            ("M0002", "M0003"): 0.7,
            ("M0003", "M0002"): 0.7,
        }

        detector = GhostDetector()
        reps = detector._representative_models(cluster_models, dyad_lookup, top_k=3)

        assert len(reps) == 3
        assert "M0001" in reps
        assert "M0002" in reps

    def test_representative_models_small_cluster(self):
        cluster_models = ["M0001", "M0002"]
        dyad_lookup = {}

        detector = GhostDetector()
        reps = detector._representative_models(cluster_models, dyad_lookup, top_k=3)

        assert reps == ["M0001", "M0002"]

    def test_prior_compatibility_uses_both_directions(self):
        summaries = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 2,
                "internal_compatibility": 0.8,
                "centroid": [0.0, 0.0],
            }
        ]
        assignments = [
            {"model_id": "M0002", "cluster_id": "Cluster_01"},
            {"model_id": "M0003", "cluster_id": "Cluster_01"},
        ]
        model_ids = ["M0001", "M0002", "M0003"]

        dyads = _make_dyads(
            model_ids,
            {
                ("M0001", "M0002"): 0.6,
                ("M0002", "M0001"): 0.8,
                ("M0001", "M0003"): 0.4,
                ("M0003", "M0001"): 0.2,
            },
        )

        detector = GhostDetector()
        results = detector.contrast(summaries, assignments, "M0001", dyads, model_ids)

        expected = (0.6 + 0.8 + 0.4 + 0.2) / 4
        assert abs(results[0]["prior_compatibility"] - expected) < 1e-6

    def test_custom_thresholds_change_labels(self):
        summaries = [
            {
                "cluster_id": "Cluster_01",
                "model_count": 3,
                "internal_compatibility": 0.5,
                "centroid": [0.0, 0.0],
            }
        ]
        assignments = [
            {"model_id": f"M{i:04d}", "cluster_id": "Cluster_01"} for i in range(1, 4)
        ]
        model_ids = ["M0001"] + [f"M{i:04d}" for i in range(1, 4)]

        similarity_map = {}
        for i in range(1, 4):
            similarity_map[("M0001", f"M{i:04d}")] = 0.35
            similarity_map[(f"M{i:04d}", "M0001")] = 0.35

        dyads = _make_dyads(model_ids, similarity_map)

        detector_low = GhostDetector(internal_threshold=0.4, prior_threshold=0.4)
        results_low = detector_low.contrast(
            summaries, assignments, "M0001", dyads, model_ids
        )
        assert results_low[0]["label"] == "ghost"

        detector_high = GhostDetector(internal_threshold=0.6, prior_threshold=0.4)
        results_high = detector_high.contrast(
            summaries, assignments, "M0001", dyads, model_ids
        )
        assert results_high[0]["label"] == "fragmented"

    @pytest.mark.parametrize("score_field", ["", "full", "compatible"])
    def test_invalid_score_field_raises(self, score_field):
        with pytest.raises(GhostError, match="score_field must be one of"):
            GhostDetector(score_field=score_field)

    def test_contrast_rejects_unavailable_selected_scores(self):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = _make_dyads(model_ids, {})
        for dyad in dyads:
            dyad["identified_compatible"] = True
        dyads[0]["identified_compatible"] = None
        dyads[1]["identified_compatible"] = np.nan
        dyads[2]["identified_compatible"] = "unknown"

        detector = GhostDetector(score_field="identified_compatible")
        with pytest.raises(
            GhostError,
            match="identified_compatible.*unavailable for 3 dyad",
        ):
            detector.contrast([], [], "M0001", dyads, model_ids)

    def test_contrast_requires_complete_directed_dyads(self):
        model_ids = ["M0001", "M0002", "M0003"]
        dyads = _make_dyads(model_ids, {})[:-1]

        detector = GhostDetector()
        with pytest.raises(GhostError, match="missing 1 directed pair"):
            detector.contrast([], [], "M0001", dyads, model_ids)
