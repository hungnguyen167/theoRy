devtools::load_all()
library(theoRy)

# fire up the package
start_theory_engine()

# Generate a registry with two X variables and one Y and no priors
df_registry <- build_component_registry(
    n_x = 2,
    time_orders = list(X1 = c(1), X2 = c(1), Y = c(2)),
    post_y = NULL,
    forced_edges = list(),
    forbidden_edges = list(),
    confounded_pairs = list(c("X1", "X2")),
    optional_nodes = character(0)
)

# generate multiverse
df_states <- expand_model_states(df_registry)

# generate metadata
df_metadata <- build_dyad_matrix(df_registry, df_states)
