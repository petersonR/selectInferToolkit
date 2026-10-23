# Methods for `inferrer` objects

The objects returned by the `infer_*` functions support
[`tidy()`](https://generics.r-lib.org/reference/tidy.html), which
returns the inferences as a tibble with one row per candidate term, and
[`print()`](https://rdrr.io/r/base/print.html), which summarizes them.

## Usage

``` r
# S3 method for class 'inferrer'
tidy(x, scale_coef = TRUE, ...)

# S3 method for class 'inferrer'
print(x, ...)
```

## Arguments

- x:

  an `inferrer`

- scale_coef:

  if `TRUE` (default), estimates are per standard deviation of each
  predictor; if `FALSE`, per unit. See "Scale of the coefficients".

- ...:

  additional parameters (not yet used)

## Value

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns a
tibble with one row per candidate term (plus the intercept):

- `term`, `selected`, `coef`: as in
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) for the
  `selector` (see
  [`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md)).
  `coef` is the selector's own estimate.

- `estimate`: the inference method's estimate, which can differ from
  `coef`: a refit of the selected model for
  [`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer_upsi.md),
  the selective-inference estimate for
  [`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer_selective.md),
  and the mean over resamples for
  [`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md).

- `ci_low`, `ci_high`: the confidence interval at the inferrer's
  `conf.level`.

- `p_value`: from
  [`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer_upsi.md),
  [`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer_selective.md)
  and
  [`infer_pipe()`](https://petersonr.github.io/selectInferToolkit/reference/infer_pipe.md).

- `prop_selected`: from
  [`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md),
  the proportion of resamples in which the term was selected.

- `null_radius`: from
  [`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md),
  the padding radius (see its `pad` argument).

How non-selected terms are filled in depends on the method's
`nonselection` (see
[`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer_upsi.md))
or, for
[`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md),
`inference_target`.

[`print()`](https://rdrr.io/r/base/print.html) returns, invisibly, a
summary of the CI lengths.

## Scale of the coefficients

Coefficients, estimates and intervals are on the same metric as the
selector's; see "Scale of the coefficients" in
[`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md).
By default (`scale_coef = TRUE`) they are per standard deviation of each
design column; with `scale_coef = FALSE` they are per unit. The
intercept is never unscaled.

Intervals and `null_radius` are converted along with the estimates.
