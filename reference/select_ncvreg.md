# Fit an MCP- or SCAD-penalized regression via `ncvreg`

A wrapper for `ncvreg` and `cv.ncvreg`. It runs CV by default so
remember to set your seed for reproducibility.

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

  data set

- family:

  outcome distributional family

- lambda:

  can be `best`, `compact` (which use CV), or a numeric vector. See
  details.

- fitted_selector:

  a previously fit `selector`, used for resampling

- ...:

  Additional arguments that can be passed to `ncvreg`, e.g. `penalty`,
  `alpha`

## Value

A `selector` object wrapping `ncvreg`
