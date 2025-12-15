# Stepwise forward/backward/bidirectional selection with AIC/BIC

This function implements forward/backward/bidirectional stepwise
regression, for use in the `selectInferToolkit` package

## Usage

``` r
select_stepwise_ic(
  formula,
  data,
  family = c("gaussian", "binomial"),
  select_factors_together = FALSE,
  penalty = c("AIC", "BIC"),
  direction = c("forward", "backward", "both"),
  trace = 0,
  fitted_selector = NULL,
  ...
)
```

## Arguments

- formula:

  a formula.

- data:

  data set

- family:

  outcome distributional family

- select_factors_together:

  should categorical variables be jointly selected?

- penalty:

  AIC, BIC

- direction:

  the mode of step wise search, can be one of "both", "backward", or
  "forward", with a default of "forward"

- trace:

  passed to MASS::stepAIC

- fitted_selector:

  a previously fit `selector`, used for resampling

- ...:

  Additional arguments

## Value

A `selector` object
