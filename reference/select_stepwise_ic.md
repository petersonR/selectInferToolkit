# Stepwise selection with AIC or BIC

Forward, backward or bidirectional stepwise selection via
[`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html).
Predictors are standardized first; see
[`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md).

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

  outcome distributional family: `"gaussian"` or `"binomial"`
  (`"poisson"` is not yet supported)

- select_factors_together:

  should the dummy columns of a factor enter or leave the model
  together?

- penalty:

  `"AIC"` (default) or `"BIC"`

- direction:

  the search direction: `"forward"` (default), `"backward"` or `"both"`

- criterion:

  which form of the information criterion to minimize:

  - `"deviance"` (default): `n * log(RSS / n) + k * df`, as in
    [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html).

  - `"cp"`: the Mallows-Cp form `RSS + k * sigma^2 * df`, which is what
    `selectiveInference` conditions on.

  Here `k = 2` for AIC and `log(n)` for BIC.

  The two can stop at different steps. When they do,
  [`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer_selective.md)
  falls back from conditioning on the IC stopping rule (`type = "aic"`)
  to conditioning on a fixed number of steps (`type = "active"`), which
  treats the number of selected terms as fixed in advance; the effect of
  this seems to be minor. `"cp"` avoids the fallback. It requires
  `family = "gaussian"`, `direction = "forward"` and
  `select_factors_together = FALSE`.

- sigma:

  residual standard deviation defining the `criterion = "cp"` penalty.
  Ignored when `criterion = "deviance"`. When `NULL` the same rule
  [`selectiveInference::fsInf()`](https://rdrr.io/pkg/selectiveInference/man/fsInf.html)
  uses is applied.

- trace:

  passed to
  [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html)

- fitted_selector:

  a previously fit `selector`, used by
  [`reselect()`](https://petersonr.github.io/selectInferToolkit/reference/reselect.md)

- ...:

  additional arguments passed to
  [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html)

## Value

a `selector`; see
[`tidy.selector()`](https://petersonr.github.io/selectInferToolkit/reference/selector-methods.md).
Its default inference method is
[`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md).
