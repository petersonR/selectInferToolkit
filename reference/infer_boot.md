# Inference for `selector` via bootstrapping

Inference for `selector` via bootstrapping

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
  cluster = NULL,
  ...
)
```

## Arguments

- object:

  a `selector` object

- data:

  the data used to fit the selector

- inference_target:

  is inference requested on all or selected only

- pad:

  how to pad the bootstrap percentile intervals, which otherwise
  collapse toward `[0, 0]` for small nonzero coefficients the selector
  rarely picks. Within each replicate a non-selected coefficient is 0.

  - `"zero_side"` (default): an interval that contains 0 has each
    endpoint pushed out to at least `-delta` / `delta`; intervals that
    exclude 0 are unchanged. `[-delta, delta]` is a region of
    "reasonably confident null", which tends to shrink as `n` grows.

  - `"none"`: the raw percentile interval ("confident nulls").

  - `"all"`: every interval is widened by `delta` on both sides.

  The radius `delta` is set from the selector (scaled metric) and
  returned in the `null_radius` column of
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html). The column
  is left out when no interval could be padded (`pad = "none"`, or a
  radius of 0):

  - [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md):
    `gamma * lambda_eff` for MCP/SCAD with `"zero_side"`, `lambda_eff`
    with `"all"` (see
    [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md)
    resampling below).

  - [`select_glmnet()`](https://petersonr.github.io/selectInferToolkit/reference/select_glmnet.md):
    `lambda`; lasso
    [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md):
    `lambda_eff`. The lasso's bias does not vanish for large effects, so
    `"zero_side"` warns. `"all"`, which also widens intervals that
    exclude 0, is the alternative to consider.

  - [`select_stepwise_ic()`](https://petersonr.github.io/selectInferToolkit/reference/select_stepwise_ic.md):
    `sqrt(k)` times the full-model standard error, with `k = 2` (AIC) or
    `log(n)` (BIC). Requires `n > p + 1`.

  - [`select_full_model()`](https://petersonr.github.io/selectInferToolkit/reference/select_full_model.md)
    /
    [`select_null_model()`](https://petersonr.github.io/selectInferToolkit/reference/select_null_model.md):
    0, so no padding.

  The radii are motivated by the gaussian family in an orthonormal
  design; other families warn.

  For
  [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md),
  every resample is refit at `lambda_eff`, the smallest penalty giving
  the selected model (see
  [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md)),
  rather than at the cross-validated penalty, which can sit near the
  smallest selected effect rather than the noise level (see GitHub issue
  \#34). So `estimate` can differ from `coef` for a coefficient in MCP's
  shrinkage zone. `lambda_eff` is reported in the inferrer's meta.

  A resample whose path ends before `lambda_eff` (a binomial fit that
  saturates, e.g. under separation, or one that reaches a `dfmax` passed
  to
  [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md))
  is fit at the smallest penalty it reached, which shrinks it more;
  their number is reported in a warning and as `n_short_path` in the
  meta.

  Warnings from fitting the resamples (e.g. from
  [`glm()`](https://rdrr.io/r/stats/glm.html) or `ncvreg()`) are
  collected and given once each, with the number of resamples that
  raised them, with or without a cluster. `lambda_eff` is motivated by
  MCP and SCAD; with the lasso it is used too, with a warning when it is
  below the cross-validated penalty.

- conf.level:

  .95 by default

- type:

  the bootstrap type. Only `"paired"` (resampling rows) is implemented;
  `"residual"` is reserved and currently an error.

- B:

  number of bootstrap resamples

- cluster:

  `NULL` (default) to fit the resamples sequentially, or a cluster from
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  to fit them in parallel. Setting up and stopping the cluster is left
  to you, so one cluster can serve several calls. Each resample draws
  from its own L'Ecuyer-CMRG stream
  ([`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html),
  as with `doRNG` or `future.apply`'s `future.seed = TRUE`), started
  from your session's random number state, so for a given
  [`set.seed()`](https://rdrr.io/r/base/Random.html) the results are the
  same with or without a cluster and with any number of workers. The
  session's generator is left as it was, with its stream advanced by one
  draw, and each worker's random number state is restored when
  `infer_boot()` returns. Each worker must have selectInferToolkit
  installed (a fork cluster from
  [`parallel::makeForkCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  shares the calling session's instead). The progress bar is shown
  either way; see
  [`pbapply::pboptions()`](https://peter.solymos.org/pbapply/reference/pboptions.html)
  to turn it off.

- ...:

  not used. Any argument passed here is an error, so that a misspelled
  or removed argument (such as `debias`) can't be silently ignored.

## Value

an `inferrer` object; see
[`tidy.inferrer()`](https://petersonr.github.io/selectInferToolkit/reference/inferrer-methods.md).
Its inferences include `prop_selected` (the proportion of resamples in
which each term was selected) and, when intervals are padded,
`null_radius` (the padding radius; see `pad`).

## Examples

``` r
set.seed(1)
sel <- select_ncvreg(mpg ~ ., data = mtcars)
set.seed(2)
inf <- infer_boot(sel, data = mtcars, B = 50)
#> Bootstrapping with B = 50 replicates...
tidy(inf)
#> # A tibble: 11 × 8
#>    term        selected  coef estimate ci_low ci_high prop_selected null_radius
#>    <chr>          <dbl> <dbl>    <dbl>  <dbl>   <dbl>         <dbl>       <dbl>
#>  1 (Intercept)        1 20.1    20.1    19.0    20.9           1          NA   
#>  2 cyl                0  0      NA      NA      NA            NA           1.02
#>  3 disp               0  0      NA      NA      NA            NA           1.02
#>  4 hp                 0  0      NA      NA      NA            NA           1.02
#>  5 drat               0  0      NA      NA      NA            NA           1.02
#>  6 wt                 1 -3.83   -3.38   -6.15    1.02          0.9         1.02
#>  7 qsec               1  2.19    1.35   -1.02    4.56          0.58        1.02
#>  8 vs                 0  0      NA      NA      NA            NA           1.02
#>  9 am                 1  1.46    0.588  -1.02    2.42          0.4         1.02
#> 10 gear               0  0      NA      NA      NA            NA           1.02
#> 11 carb               0  0      NA      NA      NA            NA           1.02

# \donttest{
# In parallel: make the cluster once, reuse it, and stop it when done
cl <- parallel::makeCluster(2)
set.seed(2)
inf_par <- infer_boot(sel, data = mtcars, B = 50, cluster = cl)
#> Bootstrapping with B = 50 replicates...
parallel::stopCluster(cl)
all.equal(tidy(inf), tidy(inf_par)) # same resamples, same results
#> [1] TRUE
# }
```
