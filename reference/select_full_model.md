# Select a full model

This function simply attempts a full model and returns a `selector`
object as a useful comparison.

## Usage

``` r
select_full_model(
  formula,
  data,
  family = c("gaussian", "binomial", "poisson"),
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

- fitted_selector:

  a previously fit `selector`, used for resampling

- ...:

  Additional arguments

## Value

A `selector` object
