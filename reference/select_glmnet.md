# Fit an (relaxed) lasso or elastic net penalized regression via glmnet

A wrapper for `glmnet` and `cv.glmnet`. It runs CV by default so
remember to set your seed for reproducibility.

## Usage

``` r
select_glmnet(
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

  data set

- family:

  outcome distributional family

- lambda:

  can be `best`, `compact` (which use CV), or a numeric vector. See
  details.

- fitted_selector:

  a previously fit `selector`, used for resampling

- ...:

  Additional arguments that can be passed to `glmnet`, e.g. `alpha`

## Value

A `selector` object wrapping `glmnet` containing:

- beta:

  a tibble containing term names and coefficients

- std:

  Was desing matrix standadrized

- penalty:

  penalty used (lasso or MCP)

- lambda:

  are the coefficeint associated with "lambda.min" or "lambda.1se"

- lambda.select:

  numeric value of selected lambda

- fold:

  Which fold each observation belongs to. By default the observations
  are randomly assigned.

- x:

  the model dataframe used

- y:

  repsonse used

- alpha:

  selected alpha for model fitting
