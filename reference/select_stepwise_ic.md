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
  criterion = c("deviance", "cp"),
  sigma = NULL,
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

- criterion:

  which form of the information criterion to minimize. `"deviance"`
  (default) mirrors
  [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html) `"cp"`
  is the Mallows-Cp form, mirrors `selectiveInference`

  The two may disagree, in which case the results returned by
  [`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer.md)
  has to fall back from passing `type = "aic"` to selectiveInference to
  `type = "active"`, which may have minor inferential consequences as it
  doesn't account for uncertainty in the number of selected features,
  proceeding as though the number of steps taken by the algorithm was
  pre-ordained. The effect of this seems to be minor.

  Requires `family = "gaussian"`, `direction = "forward"` and
  `select_factors_together = FALSE`.

- sigma:

  residual standard deviation defining the `criterion = "cp"` penalty.
  Ignored when `criterion = "deviance"`. When `NULL` the same rule
  [`selectiveInference::fsInf()`](https://rdrr.io/pkg/selectiveInference/man/fsInf.html)
  uses is applied.

- trace:

  passed to MASS::stepAIC

- fitted_selector:

  a previously fit `selector`, used for resampling

- ...:

  Additional arguments

## Value

A `selector` object
