# selectInferToolkit 0.4.3

## New features

- New vignette, "Available methods in selectInferToolkit Package", documenting
  which selection method / inference method / tuning option combinations are
  currently supported. The vignette probes every combination when it is built,
  so its compatibility matrix and flow diagrams reflect the package as of the
  build rather than a hand-maintained table.

- `select_glmnet()` gains an explicit `alpha` argument. It was previously
  reachable only through `...`, and is now recorded on the fitted selector.

- `infer_selective()` gains a `sigma` argument for supplying a known or
  pre-computed residual standard deviation. Since the automatic estimate calls
  `selectiveInference::estimateSigma()`, which runs its own unseeded
  cross-validation, this is the way to get reproducible intervals without
  relying on `set.seed()`.

## Bug fixes

- `select_glmnet()` now carries `alpha` through re-selection. Previously
  `reselect()` fell back to the default `alpha = 1`, so every bootstrap
  replicate in `infer_boot()` refit a lasso even when the original selector was
  a ridge or elastic net fit.

- `select_glmnet()` now replays the arguments originally passed through `...`
  on re-selection. `reselect()` calls the selector with only `data` and
  `fitted_selector`, so `penalty.factor`, `weights`, `nlambda`, `intercept` and
  friends were silently dropped and every `infer_boot()` replicate fit a plain
  unweighted model rather than the estimator the user selected with.

- `select_glmnet()` accepts a numeric `lambda` again. `match.arg()` ran
  unconditionally on the fresh-fit path, so the documented fixed-penalty option
  failed with `'arg' must be NULL or a character vector`. A `lambda` of length
  greater than one now gives an explicit error rather than failing downstream.

- `select_stepwise_ic()` drops indicator columns that are constant. A factor
  level that is declared but never observed (for example `Species` on
  `iris[1:100, ]`) produces an all-zero indicator, and `step_zv()` ran before
  `step_dummy()`, so it only ever saw the factor column. The indicator survived
  into the design matrix, reached `MASS::stepAIC()` as a degenerate predictor,
  and made `step_scale()` warn about dividing by zero. A second `step_zv()` now
  sits between `step_dummy()` and `step_scale()`. Centering still precedes
  dummification, so indicators remain uncentered and the intercept stays
  interpretable at the reference level of every factor, while scaling still
  follows it so `tidy(scale_coef = TRUE)` keeps reporting a common per-SD
  metric. Coefficients, intercepts and selected variables are otherwise
  unchanged.

- `infer_boot(debias = TRUE)` now has an effect. A blanket assignment overwrote
  every debiased estimate with the raw selector coefficient whenever a replicate
  had at least one non-selected term, so `debias = TRUE` matched
  `debias = FALSE` in essentially every realistic case. The assignment now only
  patches terms that are still missing, which is what it was meant to do.

- `infer_selective()` passes `ntimes = 1` to `selectiveInference::fsInf()`
  again. Without it fsInf uses its default of 2 and no longer stops where
  `MASS::stepAIC()` stopped, which tripped the step-count check more often and
  rerouted inference to `type = "active"` — an event that conditions on the
  first *k* steps as if *k* were fixed in advance and so does not adjust for the
  IC stopping rule at all. The chosen conditioning event is now recorded as
  `attr(x, "meta")$conditioning`.

- `infer_selective()` estimates sigma with `selectiveInference::estimateSigma()`
  when `p > n/2` for `glmnet` selectors as well, not just stepwise ones. The
  `glmnet` branch previously let `fixedLassoInf()` fall back to `sd(y)`, which
  contains the signal variance and produced badly over-covered intervals.

- `infer_selective()` forwards `conf.level` to the intercept-only fallback.
  When no variables were selected the interval was always computed at the 95%
  default while the returned object still reported the level the user asked for.

- `infer_selective()` now signals the no-selection fallback with a warning
  rather than a message, and records `attr(x, "meta")$unadjusted_fallback`.
  Messages are swallowed by knitr, `suppressMessages()` and simulation loops, so
  unadjusted intervals could be collected under a "Selective" label without any
  visible signal.

- `infer_selective()` now produces intervals at the requested `conf.level`. It
  passed `(1 - conf.level) / 2` as the `alpha` argument of
  `selectiveInference::fsInf()` and `fixedLassoInf()`, but that `alpha` is the
  total miscoverage rather than the per-tail miscoverage. The default
  `conf.level = 0.95` therefore returned 97.5% intervals, which were
  conservative: their false rejection rate under the null was roughly 2.5%
  rather than 5%. Reported p-values are unaffected, as they never depended on
  `alpha`. This also aligns selected terms with the non-selected terms filled in
  under `nonselection = "uncertain_nulls"`, which were already using
  `conf.level` directly.

- `infer_selective()` no longer fails when `selectiveInference::fsInf()`'s own
  AIC/BIC stopping rule stops at a different step than `MASS::stepAIC()` did. In
  that case it now conditions on the model that `select_stepwise_ic()` actually
  returned.

- `infer_selective()` handles selectors that chose no variables, returning
  unadjusted inference for the intercept-only model instead of erroring.

- `infer_selective()` now rejects `glmnet` selectors with `alpha != 1` up front.
  Selective inference is only valid for the lasso here, and these fits
  previously produced invalid intervals rather than an error.

- `infer_selective()` estimates sigma with `selectiveInference::estimateSigma()`
  when `p > n/2`, where the `fsInf()` default is unsuitable.

- `infer_pipe()` records its nonselection mode as `"uncertain_nulls"`; it
  previously recorded `"uncertain"`, which is not one of the supported values.

## Other changes

- The default for `debias` in `infer_boot()` is now `FALSE` (was `TRUE`).

- The package no longer depends on `hdrm`, which is not available on CRAN.

- Clearer error message from `infer_pipe()` when given an unsupported selector.

- Fixed a warning showing up in tests due to deprecation in tidyselect 1.2.0.


# selectInferToolkit 0.4.2

- Changed `select_factors_together = FALSE` by default in stepwise selection
  (currently a warning is created when this is set to `TRUE`)


# selectInferToolkit 0.4.1

- First public release using `select_` and `infer_` nomenclature
