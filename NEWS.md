# theoRy NEWS

## theoRy 0.2.0

### Breaking changes

- `identified_compatible` now targets a fixed direct effect rather than a
  general total-effect ID query. Causal queries require exactly one registry
  `exposure -> outcome` edge with `fixed_status = "causal"`, causal and
  applicable in every queried model.
  - For each resolved model, remove only that mandatory direct edge and test
    native d-separation given every other declared present node. Mediators,
    confounders, colliders, latent nodes, and bidirected paths remain part of
    the specified graph semantics.
  - For a partial model, compute the node set from node presence before edge
    completion. Identification is true only with complete nonempty completion
    coverage and all valid descendants true; any false descendant is false,
    while incomplete or empty coverage is unavailable.
  - The legacy `IdentificationWrapper`/causaleffect path is not used to
    determine this metric; it remains available for compatibility elsewhere.
  - The dyadic truth table is now: unavailable if either identification
    status is unavailable; `FALSE` if either model is not identified;
    unavailable if either relevant node set is unavailable; otherwise the
    two relevant node sets must be exactly equal.

The coupled R package (`DESCRIPTION`) and Python backend
(`inst/python/pyproject.toml`) versions advance from `0.1.0` to `0.2.0`.

## theoRy 0.1.0

Initial modern dyadic API with `similarity_rate`, `mas_compatible`, and the
original `identified_compatible` (joint identification only).
