from __future__ import annotations

import math

import torch

_VALID_COMPATIBILITY_METRICS = frozenset(
    {
        "similarity_rate",
        "mas_compatible",
        "identified_compatible",
    }
)


class CompatibilityScorer:
    def __init__(
        self,
        compatibility_metric: str = "similarity_rate",
    ) -> None:
        if compatibility_metric not in _VALID_COMPATIBILITY_METRICS:
            raise ValueError(
                f"compatibility_metric must be one of "
                f"{sorted(_VALID_COMPATIBILITY_METRICS)}, "
                f"got {compatibility_metric!r}"
            )

        self.compatibility_metric = compatibility_metric

    def requires_causal(self) -> bool:
        return self.compatibility_metric in ("mas_compatible", "identified_compatible")

    def score_dyads(self, dyads: list[dict]) -> torch.Tensor:
        metric = self.compatibility_metric
        scores: list[float] = []

        for i, dyad in enumerate(dyads):
            val = dyad.get(metric)
            if val is None:
                unavailable = sum(1 for d in dyads if d.get(metric) is None)
                raise ValueError(
                    f"Compatibility metric {metric!r} is unavailable "
                    f"for {unavailable} dyad(s). "
                    f"This may indicate missing exposure/outcome or "
                    f"incomplete completion coverage."
                )
            if isinstance(val, bool):
                scores.append(1.0 if val else 0.0)
            else:
                try:
                    numeric = float(val)
                except (TypeError, ValueError, OverflowError) as exc:
                    raise ValueError(
                        f"Compatibility metric {metric!r} must contain finite "
                        f"numeric values; dyad {i} has {val!r}."
                    ) from exc
                if not math.isfinite(numeric):
                    raise ValueError(
                        f"Compatibility metric {metric!r} must contain finite "
                        f"numeric values; dyad {i} has {val!r}."
                    )
                scores.append(numeric)

        return torch.tensor(scores, dtype=torch.float32)
