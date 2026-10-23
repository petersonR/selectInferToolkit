# Selective inference

Inference that conditions on the selection event, via the
`selectiveInference` package:
[`selectiveInference::fixedLassoInf()`](https://rdrr.io/pkg/selectiveInference/man/fixedLassoInf.html)
for lasso
[`select_glmnet()`](https://petersonr.github.io/selectInferToolkit/reference/select_glmnet.md)
selectors (`alpha = 1`), and
[`selectiveInference::fsInf()`](https://rdrr.io/pkg/selectiveInference/man/fsInf.html)
for forward
[`select_stepwise_ic()`](https://petersonr.github.io/selectInferToolkit/reference/select_stepwise_ic.md)
selectors (gaussian family only).

## Usage

``` r
infer_selective(
  object,
  data,
  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
  conf.level = 0.95,
  use_cv_sigma = FALSE,
  sigma = NULL,
  on_mismatch = c("warn-fall-back", "silent-fall-back", "stop"),
  ...
)
```

## Arguments

- object:

  a `selector` object

- data:

  the data used to fit the selector

- nonselection:

  how to handle variables the selector did not select; one of
  `"ignored"`, `"confident_nulls"` or `"uncertain_nulls"`. See
  [`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer_upsi.md)
  for what each does; here, too, `"uncertain_nulls"` intervals are not
  adjusted for selection.

- conf.level:

  .95 by default

- use_cv_sigma:

  estimate Sigma via CV (if FALSE, uses SI defaults)

- sigma:

  optional known or pre-computed residual standard deviation. When
  supplied it is passed straight through to the `selectiveInference`
  function and no sigma is estimated. The automatic estimate used when
  `p > n/2` calls
  [`selectiveInference::estimateSigma()`](https://rdrr.io/pkg/selectiveInference/man/estimateSigma.html)
  (for `glmnet`), which runs cross-validation, thus important to
  [`set.seed()`](https://rdrr.io/r/base/Random.html).

- on_mismatch:

  what to report when the selector's stopping point and
  `selectiveInference`'s disagree. For `stepwise_ic` selectors the two
  minimize **slightly** different criteria and can disagree. Inference
  falls back to the model the selector returned, by conditioning on a
  fixed number of steps; this argument controls how loudly that is
  reported:

  - `"warn-fall-back"` (default) warns and points at
    `select_stepwise_ic(criterion = "cp")`, which makes the two rules
    agree.

  - `"silent-fall-back"` falls back quietly.

  - `"stop"` errors instead.

  The fallback does not account for uncertainty in how many variables
  were selected, proceeding as though the number of steps had been
  pre-ordained; the effect of this appears to be minor.

  For `glmnet` selectors, inference always conditions on a refit of the
  lasso at the selected lambda with a tight convergence tolerance
  (`thresh = 1e-14`), since glmnet's default tolerance can leave the
  active set or signs slightly off on correlated designs, which makes
  intervals undercover. If the refit's active set or signs differ from
  the selector's, the same three options control whether this is
  reported with a warning, silently, or as an error.

- ...:

  arguments passed to `selectiveInference` function(s)

## Value

an `inferrer` object; see
[`tidy.inferrer()`](https://petersonr.github.io/selectInferToolkit/reference/inferrer-methods.md)
