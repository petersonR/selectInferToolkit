# MCP, SCAD or lasso selection via `ncvreg`

A wrapper for
[`ncvreg::cv.ncvreg()`](https://pbreheny.github.io/ncvreg/reference/cv.ncvreg.html).
The penalty is ncvreg's default, MCP, unless `penalty = "SCAD"` or
`"lasso"` is passed through `...`. Cross-validation is random, so set a
seed for reproducibility. Predictors are standardized first; see
[`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md).

## Usage

``` r
select_ncvreg(
  formula,
  data,
  family = c("gaussian", "binomial", "poisson"),
  lambda = c("best", "compact"),
  fitted_selector = NULL,
  ...
)
```

## Arguments

- formula:

  a model formula, as for [`lm()`](https://rdrr.io/r/stats/lm.html). It
  may include interactions (`x * z`) and in-formula transformations
  (`log(x)`, `poly(x, 2)`); factor, logical and character predictors are
  treatment-coded. Ordered factors and missing values are not supported.
  See
  [`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md)
  for how the columns are standardized and named.

- data:

  a data frame

- family:

  outcome distributional family

- lambda:

  `"best"` (default) or `"compact"`, both chosen by cross-validation
  (see Details)

- fitted_selector:

  a previously fit `selector`, used by
  [`reselect()`](https://petersonr.github.io/selectInferToolkit/reference/reselect.md)

- ...:

  additional arguments passed to
  [`ncvreg::cv.ncvreg()`](https://pbreheny.github.io/ncvreg/reference/cv.ncvreg.html),
  e.g. `penalty`, `gamma`, `alpha`, `nfolds`

## Value

a `selector` wrapping the `cv.ncvreg` fit; see
[`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md).
Its default inference method is
[`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md).

## Details

With `lambda = "best"` the penalty is `lambda.min`, the value minimizing
cross-validated error; with `"compact"` it is `lambda.1se`, the largest
value whose cross-validated error is within one standard error of the
minimum, which gives a sparser model.

Cross-validated error is nearly flat over the stretch of the path where
the selected model doesn't change, so the chosen penalty can sit at the
top of that stretch, near the smallest selected effect rather than the
noise level (see GitHub issue \#34). The selector also stores
`lambda_eff`, the smallest penalty on the path next to the chosen one
that gives the same selected model (in `attr(x, "meta")`). Predictions
and [`coef()`](https://rdrr.io/r/stats/coef.html) use the chosen
penalty;
[`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md)
refits its resamples at `lambda_eff` and pads with it.
