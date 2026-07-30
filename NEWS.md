# theoRy NEWS

## theoRy 0.2.0

### Breaking changes

- `identified_compatible` now uses a stricter compatibility contract. Two
  models are identified-compatible only when **both** independently identify
  the same total-effect query **and** their relevant declared node sets are
  exactly equal after removing robust directed-path intermediates.
  - For a resolved model, the relevant set is all declared present nodes
    (observed **and** latent) minus nodes that lie on at least one directed
    exposure-outcome path in the declared directed graph; bidirected edges
    never make a node a directed-path intermediate.
  - For a partial model, a node is removed only when it is a directed-path
    intermediate in **every** valid represented completion (the robust
    intersection rule), so an uncertain possible mediator is retained rather
    than ignored. Incomplete completion coverage, an empty descendant set,
    or any unavailable descendant relevant set returns unavailable.
  - Identification itself continues to be computed by R `causaleffect` over
    the observed latent-projected ADMG; only cross-model comparability uses
    the declared (pre-projection) node set.
  - The dyadic truth table is now: unavailable if either identification
    status is unavailable; `FALSE` if either model is not identified;
    unavailable if either relevant node set is unavailable; otherwise the
    two relevant node sets must be exactly equal.

The coupled R package (`DESCRIPTION`) and Python backend
(`inst/python/pyproject.toml`) versions advance from `0.1.0` to `0.2.0`.

## theoRy 0.1.0

Initial modern dyadic API with `similarity_rate`, `mas_compatible`, and the
original `identified_compatible` (joint identification only).