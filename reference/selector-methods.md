# Methods for `selector` objects

The objects returned by the `select_*` functions support:

- [`predict()`](https://rdrr.io/r/stats/predict.html): predictions on
  new data, using the selected model.

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html): a tibble
  with one row per candidate term.

- [`coef()`](https://rdrr.io/r/stats/coef.html): the selected
  coefficients (including the intercept).

- [`print()`](https://rdrr.io/r/base/print.html): a short summary of the
  selection.

## Usage

``` r
# S3 method for class 'selector'
predict(object, newdata, ...)

# S3 method for class 'selector'
tidy(x, scale_coef = TRUE, ...)

# S3 method for class 'selector'
print(x, ...)

# S3 method for class 'selector'
coef(object, use_native = FALSE, ...)
```

## Arguments

- object, x:

  a `selector`

- newdata:

  a data frame to predict on

- ...:

  for [`coef()`](https://rdrr.io/r/stats/coef.html), passed to the
  native `coef` method when `use_native = TRUE`; otherwise not used

- scale_coef:

  if `TRUE` (default), coefficients are per standard deviation of each
  predictor; if `FALSE`, per unit. See "Scale of the coefficients".

- use_native:

  if `TRUE`, call the underlying fit's own `coef` method (e.g.
  `coef.cv.glmnet`) instead

## Value

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html): a tibble
  with columns `term` (every candidate term, plus the intercept),
  `selected` (1 if the selector kept the term, else 0; the intercept is
  always 1) and `coef` (the selector's estimate, e.g. the penalized
  estimate for
  [`select_glmnet()`](https://petersonr.github.io/selectInferToolkit/reference/select_glmnet.md)
  or
  [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md);
  0 when not selected).

- [`coef()`](https://rdrr.io/r/stats/coef.html): a named vector of the
  selected coefficients, including the intercept, always on the scaled
  metric.

- [`predict()`](https://rdrr.io/r/stats/predict.html): a vector of
  predictions for `newdata`.

## Scale of the coefficients

Before selection, every `select_*` function standardizes the predictors
with a `recipes` recipe: factors are dummy-coded, then all numeric
predictors (dummies included) are centered and divided by their standard
deviation. The outcome is not transformed. So, by default
(`scale_coef = TRUE`), a coefficient is the change in the outcome per
standard deviation of that predictor. With `scale_coef = FALSE`, slopes
are divided by the predictor's standard deviation, giving the usual
per-unit change. (A factor kept whole by
`select_stepwise_ic(select_factors_together = TRUE)` is not scaled, so
it is the same either way.)

The intercept is never unscaled: because the predictors are centered, it
is the predicted outcome (on the link scale) at the predictor means, not
the intercept [`lm()`](https://rdrr.io/r/stats/lm.html) would report on
the raw data.
