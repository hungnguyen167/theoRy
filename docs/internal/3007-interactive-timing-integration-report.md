# Interactive Timing Integration Report

## Delivered

- Added `build_component_registry_interactive()` and
  `analyze_theory(input_mode = "interactive")`. The questionnaire collects
  arbitrary node names, a required focal exposure/outcome pair, timing,
  directed constraints, possible latent confounding, optional nodes, and final
  confirmation.
- Kept the programmatic registry builder. Exposure and outcome are now
  required, distinct, have one fixed time, and force `exposure -> outcome` to
  be causal in every generated model.
- Added `timing_options`, `time_points`, and `optional_nodes`. Expansion now
  enumerates one allowed time per present node, prunes inconsistent required
  paths, returns a pruning report, and applies one global projected-model cap.
- Made selected bidirected pairs possible confounding candidates with
  `present`/`absent` states rather than calling them causal.
- Added `causal_backend = "auto" | "native" | "r"`. NetworkX handles the
  supported backdoor subset; Dagitty/CausalEffect remains the optional broader
  identification path.
- Replaced shell-based engine startup and port killing with a managed
  `processx` lifecycle that is portable across Linux, macOS, and Windows.
  `stop_theory_engine(stop_external = TRUE)` can explicitly and gracefully
  stop a compatible externally managed engine over HTTP.

## Compatibility Notes

- Existing scripted builders continue to work when they already supply
  exposure, outcome, and fixed timing. Calls without exposure/outcome now fail
  explicitly rather than inferring `X1` and `Y`.
- `node_policy` remains available. `optional_nodes` is the narrower preferred
  subset control and takes precedence when supplied.
- Parquet persists the registry table, not R attributes. Callers reloading a
  timing-uncertain registry must supply its timing options and focal metadata.

## Verification

- Full Python suite passed: 574 passed, 4 skipped.
- Full R test suite passed: 363 expectations, with four existing deprecation
  warnings.
- The portable launcher was checked for managed startup, readiness, graceful
  shutdown, failed-start logs, and externally managed engine protection. An
  opt-in external HTTP shutdown was also exercised from a session without the
  process handle.
- A live R-to-Python native-causal flow with timing uncertainty, an optional
  node, and possible confounding generated 37 models and 1,332 dyads.
