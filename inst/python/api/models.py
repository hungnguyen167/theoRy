from pydantic import BaseModel, ConfigDict, model_validator
from typing import Literal

CompatibilityMetric = Literal[
    "similarity_rate", "mas_compatible", "identified_compatible"
]


class ComponentRecord(BaseModel):
    model_config = ConfigDict(extra="allow")

    comp_id: str
    type: Literal["node", "edge"]
    source: str
    target: str | None = None
    direction: Literal["->", "<->"] | None = None
    description: str
    fixed_status: Literal["causal"] | None = None
    observed: bool = True


class StateRecord(BaseModel):
    model_config = ConfigDict(extra="allow")

    model_id: str
    comp_id: str
    status: Literal["present", "absent", "causal", "unknown", "non-causal"]
    timing: int | None = None
    seeded: bool | None = None


class DyadMatrixRequest(BaseModel):
    registry_data: list[ComponentRecord]
    state_data: list[StateRecord]
    model_ids: list[str] | None = None
    mode: Literal["basic", "full", "single-ref", "two-stage"] = "basic"
    reference_id: str | None = None
    top_k: int | None = None
    exposure: str | None = None
    outcome: str | None = None
    causal_backend: Literal["auto", "native", "r"] = "r"


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
    timing_options: list[int] | None = None
    description: str | None = None
    observed: bool = True


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
    exposure: str
    outcome: str

    @model_validator(mode="after")
    def validate_exposure_outcome(self):
        node_names = {n.name for n in self.nodes}
        if self.exposure == self.outcome:
            raise ValueError("Exposure and outcome must be distinct nodes")
        if self.exposure not in node_names:
            raise ValueError(f"Exposure '{self.exposure}' is not in the node list")
        if self.outcome not in node_names:
            raise ValueError(f"Outcome '{self.outcome}' is not in the node list")
        return self


class ExpandModelStatesRequest(BaseModel):
    registry_data: list[ComponentRecord]
    mode: Literal["sampled", "exhaustive"] = "sampled"
    seed_claims: list[StateRecord] | None = None
    node_timing: dict[str, int] | None = None
    timing_options: dict[str, list[int]] | None = None
    optional_nodes: list[str] | None = None
    max_models: int = 10000
    n_models: int | None = None
    seed: int | None = None
    edge_statuses: list[str] | None = None
    bidirected_statuses: list[Literal["present", "absent"]] | None = None
    node_policy: Literal["all-present", "vary"] = "all-present"
    allow_large: bool = False
    exposure: str | None = None
    outcome: str | None = None


# --- Story 3.3 Delta-U request model ------------------------------------------


class DeltaURequest(BaseModel):
    model_config = ConfigDict(extra="forbid")

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

    compatibility_metric: CompatibilityMetric = "similarity_rate"
    crux_mode: Literal["marginal", "global"] = "marginal"
    global_status: Literal["causal", "non-causal"] | None = None
    device: Literal["auto", "cpu", "cuda"] = "auto"
    use_tensor_engine: bool = True
    exposure: str | None = None
    outcome: str | None = None

    @model_validator(mode="after")
    def validate_metric_query(self):
        if (self.exposure is None) != (self.outcome is None):
            raise ValueError("Both or neither of exposure and outcome must be provided")
        has_explicit_context = (
            self.registry_data is not None or self.state_data is not None
        )
        if (
            self.compatibility_metric != "similarity_rate"
            and has_explicit_context
            and self.exposure is None
        ):
            raise ValueError(
                f"compatibility_metric '{self.compatibility_metric}' requires "
                "both exposure and outcome with an explicit analysis context"
            )
        return self

    @model_validator(mode="after")
    def validate_crux_request(self):
        if self.crux_mode == "global":
            if self.component_id is not None:
                raise ValueError("global crux does not accept component_id")
            if self.mode == "two-stage":
                raise ValueError("global crux does not support two-stage mode")
            if self.synergistic_set_size is not None:
                raise ValueError("global crux does not support synergistic sets")
        else:
            if self.global_status is not None:
                raise ValueError("global_status is only valid with crux_mode='global'")
        return self


# --- Story 4.3 Clusters request model -----------------------------------------


class ClustersRequest(BaseModel):
    model_config = ConfigDict(extra="forbid")

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
    score_field: CompatibilityMetric = "similarity_rate"
    exposure: str | None = None
    outcome: str | None = None


# --- Story 5.4 Simulate request model -----------------------------------------


class SimulateRequest(BaseModel):
    model_config = ConfigDict(extra="forbid")

    scenario: Literal[
        "illusion_of_precision",
        "lynchpin_of_certainty",
        "crux_of_certainty",
        "ghost_discovery",
    ]
    n_models: int = 100
    n_components: int = 50
    random_state: int | None = 42

    # Seeded-mode fields
    registry_data: list[ComponentRecord] | None = None
    state_data: list[StateRecord] | None = None
    sample_n: int | None = None

    # Scenario B: lynchpin_of_certainty
    n_zones: int | None = None
    noise_fraction: float = 0.10

    # Scenario C: ghost_discovery
    mainstream_fraction: float = 0.70
    ghost_fraction: float = 0.20
    eps: float = 0.5
    min_samples: int | None = None
    internal_threshold: float = 0.6
    prior_threshold: float = 0.4
    divergent_fraction: float | None = None

    # Plot-data diagnostics
    include_plot_data: bool = False
    plot_sample_n: int | None = 200
    pair_sample_n: int | None = 5000

    # All scenarios
    enforce_thresholds: bool | None = None

    # Causal query (for full-compatibility metrics)
    exposure: str | None = None
    outcome: str | None = None

    # Which compatibility metric drives the scenario
    compatibility_metric: CompatibilityMetric = "similarity_rate"

    # Crux semantics (only used by lynchpin/crux scenarios)
    crux_mode: Literal["marginal", "global"] = "marginal"
    global_status: Literal["causal", "non-causal"] | None = None

    # Retained for validation of clients using the former simulation option.
    include_bidirectional: bool = False

    @model_validator(mode="after")
    def validate_simulate_request(self):
        has_registry = self.registry_data is not None
        has_states = self.state_data is not None
        is_seeded = has_registry or has_states
        is_crux_scenario = self.scenario in (
            "lynchpin_of_certainty",
            "crux_of_certainty",
        )

        if self.include_bidirectional:
            raise ValueError(
                "Simulations support directed components only; "
                "include_bidirectional must be false"
            )

        if not is_crux_scenario and (
            self.crux_mode != "marginal" or self.global_status is not None
        ):
            raise ValueError(
                "crux_mode and global_status only apply to "
                "lynchpin_of_certainty and crux_of_certainty scenarios"
            )
        if is_crux_scenario and self.crux_mode == "global":
            if self.global_status is None:
                raise ValueError(
                    "crux_mode='global' requires global_status "
                    "('causal' or 'non-causal')"
                )
        elif is_crux_scenario and self.global_status is not None:
            raise ValueError("global_status is only valid with crux_mode='global'")

        if self.registry_data is not None and any(
            row.direction == "<->" for row in self.registry_data
        ):
            raise ValueError(
                "Seeded simulations support directed components only; "
                "registry_data contains a bidirected component"
            )

        if (self.exposure is None) != (self.outcome is None):
            raise ValueError("Both or neither of exposure and outcome must be provided")
        generated_illusion = self.scenario == "illusion_of_precision" and not is_seeded
        if (
            self.scenario == "illusion_of_precision"
            and self.compatibility_metric == "similarity_rate"
        ):
            raise ValueError(
                "illusion_of_precision requires compatibility_metric "
                "'mas_compatible' or 'identified_compatible'"
            )
        if (
            self.compatibility_metric != "similarity_rate"
            and (self.exposure is None or self.outcome is None)
            and not generated_illusion
        ):
            raise ValueError(
                f"compatibility_metric '{self.compatibility_metric}' requires "
                "both exposure and outcome"
            )

        if is_seeded:
            if not has_registry or not has_states:
                raise ValueError(
                    "Seeded simulation requires both registry_data and state_data. "
                    "Dyad matrices are not accepted as simulation seeds."
                )
            if self.registry_data is not None and len(self.registry_data) == 0:
                raise ValueError("registry_data must not be empty in seeded mode")
            if self.state_data is not None and len(self.state_data) == 0:
                raise ValueError("state_data must not be empty in seeded mode")
            if self.sample_n is not None and self.sample_n < 1:
                raise ValueError("sample_n must be a positive integer")
            if self.sample_n is not None and self.sample_n < 2:
                raise ValueError("sample_n must be at least 2 for simulation")
        else:
            if self.sample_n is not None:
                raise ValueError(
                    "sample_n is only used when registry_data and state_data are supplied"
                )
            if self.n_models < 10:
                raise ValueError("n_models must be at least 10")
            if self.n_components < 5:
                raise ValueError("n_components must be at least 5")

        if self.plot_sample_n is not None and self.plot_sample_n < 1:
            raise ValueError("plot_sample_n must be a positive integer")
        if self.pair_sample_n is not None and self.pair_sample_n < 1:
            raise ValueError("pair_sample_n must be a positive integer")

        return self


# --- Symbolic API models -------------------------------------------------------


class DagSpec(BaseModel):
    nodes: list[NodeSpec] | list[str]
    edges: list[tuple[str, str]]
    exposure: str
    outcome: str
    timing: dict[str, int | None] | None = None
    unmentioned_edges: Literal["non-causal", "unknown"] = "non-causal"


class EdgeStatus(BaseModel):
    comp_id: str
    status: Literal["causal", "non-causal", "unknown"]


class SymbolicUniverseRequest(BaseModel):
    registry_data: list[ComponentRecord] | None = None
    nodes: list[NodeSpec] | list[str] | None = None
    exposure: str | None = None
    outcome: str | None = None
    preferred_model: DagSpec | None = None
    constraints: list[EdgeStatus] | None = None
    absent_nodes: list[str] | None = None

    @model_validator(mode="after")
    def validate_exposure_outcome(self):
        if (self.exposure is None) != (self.outcome is None):
            raise ValueError("Both or neither of exposure and outcome must be provided")
        if self.exposure is None:
            return self

        if self.exposure == self.outcome:
            raise ValueError("Exposure and outcome must be distinct nodes")

        if self.nodes is not None:
            node_names = {
                node if isinstance(node, str) else node.name for node in self.nodes
            }
        elif self.registry_data is not None:
            node_names = {
                record.source for record in self.registry_data if record.type == "node"
            }
        else:
            return self

        invalid_targets = {self.exposure, self.outcome} - node_names
        if invalid_targets:
            raise ValueError(
                "Exposure/outcome must be supplied node names: "
                + ", ".join(sorted(invalid_targets))
            )
        return self


class SymbolicQueryRequest(SymbolicUniverseRequest):
    mode: Literal["full", "sampled"] = "full"
    n_samples: int = 5000
    fallback: Literal["sampled", "error"] = "sampled"
    max_compile_seconds: int = 60
    max_count_seconds: int = 60
    max_bdd_nodes: int | None = None
    signature_policy: Literal["paper_v1", "minimal"] = "paper_v1"
    max_signature_atoms: int = 16
    max_path_len: int = 8
    max_paths: int | None = None
    structural_similarity: Literal["projected", "expected", "sampled"] = "projected"


class SymbolicCompareRequest(SymbolicUniverseRequest):
    theory_a: DagSpec
    theory_b: DagSpec | None = None
    mode: Literal["full", "sampled"] = "full"


class SymbolicDeltaURequest(SymbolicUniverseRequest):
    mode: Literal["full", "sampled"] = "full"
    fallback: Literal["sampled", "error"] = "sampled"
    top_k: int = 10
    n_samples: int = 5000
    signature_policy: Literal["paper_v1", "minimal"] = "paper_v1"
    max_signature_atoms: int = 16


class SymbolicSimulateRequest(BaseModel):
    scenario: Literal[
        "illusion_of_precision",
        "lynchpin_of_certainty",
        "crux_of_certainty",
        "ghost_discovery",
    ]
    mode: Literal["full", "sampled"] = "full"
    fallback: Literal["sampled", "error"] = "sampled"
    n_samples: int = 5000
    signature_policy: Literal["paper_v1", "minimal"] = "paper_v1"
    max_signature_atoms: int = 8
    template_size: Literal["paper_small", "paper_13"] = "paper_small"
    seed: int = 42
    n_shared_edges: int = 6
    n_critical_unknown: int = 2
    n_zones: int = 3
    n_edges_per_zone: int = 3
    n_mainstream: int = 40
    n_ghost: int = 10
    n_noise: int = 10
