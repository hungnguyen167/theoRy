from pydantic import BaseModel, field_validator
from typing import Literal


class ComponentRecord(BaseModel):
    comp_id: str
    type: Literal["node", "edge"]
    source: str
    target: str | None = None
    direction: Literal["->", "<->"] | None = None
    description: str


class StateRecord(BaseModel):
    model_id: str
    comp_id: str
    status: Literal["causal", "unknown", "non-causal"]
    timing: int | None = None


class DyadMatrixRequest(BaseModel):
    registry_data: list[ComponentRecord]
    state_data: list[StateRecord]
    model_ids: list[str] | None = None
    mode: Literal["basic", "full", "single-ref", "two-stage"] = "basic"
    reference_id: str | None = None
    top_k: int | None = None
    exposure: str | None = None
    outcome: str | None = None


class DyadMatrixResponse(BaseModel):
    dyad_matrix: list
    model_count: int


class ApiSuccessResponse(BaseModel):
    status: Literal["success"] = "success"
    data: dict


class ApiErrorResponse(BaseModel):
    status: Literal["error"] = "error"
    code: str
    message: str


# --- Story 1.2A / 1.3A models ------------------------------------------------


class NodeSpec(BaseModel):
    name: str
    timing: int | None = None
    description: str | None = None


class EdgeConstraint(BaseModel):
    source: str
    target: str
    direction: Literal["->", "<->"] = "->"
    rule: Literal["allow", "forbid", "require"]


class BuildRegistryRequest(BaseModel):
    nodes: list[NodeSpec]
    respect_timing: bool = True
    include_bidirectional: bool = False
    constraints: list[EdgeConstraint] | None = None


class ExpandModelStatesRequest(BaseModel):
    registry_data: list[ComponentRecord]
    mode: Literal["seeded", "exhaustive", "sampled"] = "seeded"
    seed_claims: list[StateRecord] | None = None
    node_timing: dict[str, int] | None = None
    max_models: int = 10000
    n_models: int | None = None
    seed: int | None = None
    edge_statuses: list[str] | None = None


# --- Story 3.3 Delta-U request model ------------------------------------------


class DeltaURequest(BaseModel):
    registry_data: list[ComponentRecord] | None = None
    state_data: list[StateRecord] | None = None
    model_ids: list[str] | None = None
    dyads: list[dict] | None = None
    component_id: str | None = None
    top_k: int = 10
    mode: Literal["exhaustive", "two-stage"] = "exhaustive"
    heatmap_threshold: float | None = None
    synergistic_set_size: int | None = None
    synergistic_search: Literal["greedy", "beam"] | None = None
    synergistic_beam_width: int | None = None

    scoring: Literal["structural", "causal", "hybrid"] = "structural"
    causal_weight: float = 0.5
    structural_weight: float = 0.5
    causal_metrics: list[Literal["mas_compatible", "full_compatible"]] | None = None
    device: Literal["auto", "cpu", "cuda"] = "auto"
    use_tensor_engine: bool = True
    exposure: str | None = None
    outcome: str | None = None


# --- Story 4.3 Clusters request model -----------------------------------------


class ClustersRequest(BaseModel):
    registry_data: list[ComponentRecord] | None = None
    state_data: list[StateRecord] | None = None
    model_ids: list[str] | None = None
    dyads: list[dict] | None = None
    prior_model_id: str | None = None
    eps: float = 0.5
    min_samples: int = 5
    umap_components: int = 2
    umap_n_neighbors: int = 15
    umap_min_dist: float = 0.1
    umap_metric: str = "euclidean"
    random_state: int | None = 42
    internal_threshold: float = 0.6
    prior_threshold: float = 0.4
    score_field: str = "similarity_rate"


# --- Story 5.4 Simulate request model -----------------------------------------


class SimulateRequest(BaseModel):
    scenario: Literal[
        "illusion_of_precision",
        "lynchpin_of_certainty",
        "ghost_discovery",
    ]
    n_models: int = 100
    n_components: int = 50
    random_state: int | None = 42

    @field_validator("n_models")
    @classmethod
    def validate_n_models(cls, value: int) -> int:
        if value < 10:
            raise ValueError("n_models must be at least 10")
        return value

    @field_validator("n_components")
    @classmethod
    def validate_n_components(cls, value: int) -> int:
        if value < 5:
            raise ValueError("n_components must be at least 5")
        return value
