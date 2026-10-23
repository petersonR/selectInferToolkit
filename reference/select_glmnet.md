# Lasso or elastic net selection via `glmnet`

A wrapper for
[`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
(or
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
at a fixed `lambda`). Cross-validation is random, so set a seed for
reproducibility. Predictors are standardized first; see
[`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md).

## Usage

``` r
select_glmnet(
  formula,
  data,
  family = c("gaussian", "binomial", "poisson"),
  lambda = c("best", "compact"),
  fitted_selector = NULL,
  alpha = 1,
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
  (see Details), or a single number giving a fixed penalty.

- fitted_selector:

  a previously fit `selector`, used by
  [`reselect()`](https://petersonr.github.io/selectInferToolkit/reference/reselect.md)

- alpha:

  the elastic net mixing parameter, passed to `glmnet`: 1 (default) is
  the lasso

- ...:

  additional arguments passed to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  or
  [`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html),
  e.g. `nfolds`, `penalty.factor`

## Value

a `selector` wrapping the `cv.glmnet` (or `glmnet`) fit; see
[`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md).
Its default inference method is
[`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer_selective.md).

## Details

With `lambda = "best"` the penalty is `lambda.min`, the value minimizing
cross-validated error; with `"compact"` it is `lambda.1se`, the largest
value whose cross-validated error is within one standard error of the
minimum, which gives a sparser model.

Only the lasso (`alpha = 1`) is supported by
[`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer_selective.md).
