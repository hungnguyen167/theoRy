from __future__ import annotations

import torch


_VALID_SCORING = frozenset({"structural", "causal", "hybrid"})
_DEFAULT_CAUSAL_METRICS: list[str] = ["mas_compatible", "full_compatible"]


def metric_to_score(value: bool | None) -> float:
    if value is True:
        return 1.0
    if value is False:
        return 0.0
    return 0.5


class CompatibilityScorer:
    def __init__(
        self,
        scoring: str = "structural",
        structural_weight: float = 0.5,
        causal_weight: float = 0.5,
        causal_metrics: list[str] | None = None,
    ) -> None:
        if scoring not in _VALID_SCORING:
            raise ValueError(
                f"scoring must be one of {sorted(_VALID_SCORING)}, got {scoring!r}"
            )
        if not 0.0 <= structural_weight <= 1.0:
            raise ValueError("structural_weight must be in [0, 1]")
        if not 0.0 <= causal_weight <= 1.0:
            raise ValueError("causal_weight must be in [0, 1]")
        if scoring == "hybrid" and structural_weight + causal_weight <= 0.0:
            raise ValueError(
                "At least one scoring weight must be positive in hybrid mode"
            )

        self.scoring = scoring
        self.structural_weight = structural_weight
        self.causal_weight = causal_weight

        if causal_metrics is None and scoring != "structural":
            self.causal_metrics = list(_DEFAULT_CAUSAL_METRICS)
        else:
            self.causal_metrics = causal_metrics or []

        self._normalized_structural: float
        self._normalized_causal: float
        if scoring == "hybrid":
            total = structural_weight + causal_weight
            self._normalized_structural = structural_weight / total
            self._normalized_causal = causal_weight / total
        else:
            self._normalized_structural = 1.0
            self._normalized_causal = 1.0

    def requires_causal(self) -> bool:
        return self.scoring in ("causal", "hybrid")

    def score_dyads(self, dyads: list[dict]) -> torch.Tensor:
        if self.scoring == "structural":
            scores = torch.tensor(
                [d.get("similarity_rate", 0.0) for d in dyads],
                dtype=torch.float32,
            )
            return scores

        causal_scores = self._compute_causal_scores(dyads)

        if self.scoring == "causal":
            return causal_scores

        structural_scores = torch.tensor(
            [d.get("similarity_rate", 0.0) for d in dyads],
            dtype=torch.float32,
        )
        return (
            self._normalized_structural * structural_scores
            + self._normalized_causal * causal_scores
        )

    def _compute_causal_scores(self, dyads: list[dict]) -> torch.Tensor:
        if not self.causal_metrics:
            return torch.zeros(len(dyads), dtype=torch.float32)

        scores = torch.zeros(len(dyads), dtype=torch.float32)
        for i, dyad in enumerate(dyads):
            values = []
            for metric in self.causal_metrics:
                val = dyad.get(metric)
                values.append(metric_to_score(val))
            scores[i] = sum(values) / len(values) if values else 0.5
        return scores
