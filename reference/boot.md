# Inference for `selector` via bootstrapping

Inference for `selector` via bootstrapping

Bootstrapping selection process

## Usage

``` r
infer_boot(
  object,
  data,
  inference_target = c("selections", "all"),
  pad = c("zero_side", "none", "all"),
  conf.level = 0.95,
  type = c("paired", "residual"),
  B = 250,
  n_cores = 4,
  ...
)

boot(
  object,
  data,
  B,
  inference_target = c("selections", "all"),
  pad = c("zero_side", "none", "all"),
  conf.level,
  n_cores,
  ...
)
```

## Arguments

- object:

  a `selector` object

- data:

  data must be passed to infer

- inference_target:

  is inference requested on all or selected only

- pad:

  how to pad the bootstrap percentile intervals, which otherwise
  collapse toward `[0, 0]` for small nonzero coefficients the selector
  rarely picks. Within each replicate a non-selected coefficient is 0.

  - `"zero_side"` (default): an interval that contains 0 has each
    endpoint pushed out to at least `-delta` / `delta`; intervals that
    exclude 0 are unchanged. `[-delta, delta]` is a region of
    "reasonably confident null" that shrinks with `n`.

  - `"none"`: the raw percentile interval ("confident nulls").

  - `"all"`: every interval is widened by `delta` on both sides.

  The radius `delta` is set from the selector (scaled metric) and
  returned in the `null_radius` column of
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html):

  - [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md):
    `gamma * lambda` for MCP/SCAD with `"zero_side"`, `lambda` with
    `"all"`.

  - [`select_glmnet()`](https://petersonr.github.io/selectInferToolkit/reference/select_glmnet.md)
    and lasso
    [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md):
    `lambda`. The lasso's bias does not vanish for large effects, so
    `"zero_side"` warns; `"all"` is the uniformly valid choice.

  - [`select_stepwise_ic()`](https://petersonr.github.io/selectInferToolkit/reference/select_stepwise_ic.md):
    `sqrt(k)` times the full-model standard error, with `k = 2` (AIC) or
    `log(n)` (BIC). Requires `n > p + 1`.

  - [`select_full_model()`](https://petersonr.github.io/selectInferToolkit/reference/select_full_model.md)
    /
    [`select_null_model()`](https://petersonr.github.io/selectInferToolkit/reference/select_null_model.md):
    0, so no padding.

  Coverage guarantees are for the gaussian family in an orthonormal
  design; other families warn.

- conf.level:

  .95 by default

- type:

  what type of bootstrap (currently only `paired` supported)

- B:

  The number of bootstrap replicates.

- n_cores:

  number of cores for parallel computation

- ...:

  any additional arguments to that can be passed to fitting engine

## Value

`inferrer` s3 class with things like...

an `inferrer` object
