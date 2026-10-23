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

- fitted_selector:

  a previously fit `selector`, used by
  [`reselect()`](https://petersonr.github.io/selectInferToolkit/reference/reselect.md)

- ...:

  Additional arguments

## Value

A `selector` object
