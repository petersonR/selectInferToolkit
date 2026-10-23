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

  a formula

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
