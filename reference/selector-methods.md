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

Before selection, every `select_*` function builds the model matrix as
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) would
(factors, logicals and character predictors are treatment-coded, so the
columns are named e.g. `g3mid`, `am1`, `fTRUE`), then centers each
column and divides it by its standard deviation, dummies included. The
outcome is not transformed. So, by default (`scale_coef = TRUE`), a
coefficient is the change in the outcome per standard deviation of that
column. With `scale_coef = FALSE`, slopes are divided by the column's
standard deviation, giving the usual per-unit change. The centers and
standard deviations come from the data the selector was first fit to,
and are reused by
[`reselect()`](https://petersonr.github.io/selectInferToolkit/reference/reselect.md)
and [`predict()`](https://rdrr.io/r/stats/predict.html).

The intercept is never unscaled: it is the predicted outcome (on the
link scale) at the means of the predictors, not the intercept
[`lm()`](https://rdrr.io/r/stats/lm.html) would report on the raw data.
([`select_stepwise_ic()`](https://petersonr.github.io/selectInferToolkit/reference/select_stepwise_ic.md)
leaves dummies uncentered, so there it is the prediction at each
factor's reference level.)

Ordered factors are not supported (convert them to numeric scores or
unordered factors first), nor are missing values in the outcome or
predictors.
