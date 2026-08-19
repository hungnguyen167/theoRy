devtools::load_all()
library(theoRy)
start_theory_engine()

# Create multiverse, direct method
# users can also do step-by-step with build_component_registry(), expand_model_states(),
# and build_dyad_matrix()

result <- analyze_theory(
    nodes = c("X1", "X2", "X3", "Y"),
    timing = c(2, 2, 1, 3),
    exposure = "X1",
    outcome = "Y",
    mode = "exhaustive",
    constraints = list(list(source = "X3", target = "X2", direction = "->", rule = "require"),
                       list(source = "X2", target = "Y", direction = "->", rule = "require")),
    include_bidirectional = TRUE,
    causal_backend = "native"
)

# plot figure A1
plot_dag_models(result$registry, result$states, plot_all = T,
                width     = 1200,
                height    = 600,
                save_path = here::here("vignettes/fig1a/"))


result$summary

# Make Table 1
tbl1 <- result$summary

parsed <- do.call(rbind, strsplit(tbl1, ":\\s*"))

t1df <- data.frame(
    Metric = parsed[, 1],
    Value = parsed[, 2],
    stringsAsFactors = FALSE
)



write.csv(t1df, "vignettes/Tbl1.csv", row.names = FALSE)

