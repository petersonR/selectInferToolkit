# selectInferToolkit 0.4.3

## New features

- New vignette, "Available methods in selectInferToolkit Package", documenting
  which selection method / inference method / tuning option combinations are
  currently supported. The vignette probes every combination when it is built,
  so its compatibility matrix and flow diagrams reflect the package as of the
  build rather than a hand-maintained table.

- `select_glmnet()` gains an explicit `alpha` argument. It was previously
  reachable only through `...`, and is now recorded on the fitted selector.

## Bug fixes

- `select_glmnet()` now carries `alpha` through re-selection. Previously
  `reselect()` fell back to the default `alpha = 1`, so every bootstrap
  replicate in `infer_boot()` refit a lasso even when the original selector was
  a ridge or elastic net fit.

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


# selectInferToolkit 0.4.2

- Changed `select_factors_together = FALSE` by default in stepwise selection
  (currently a warning is created when this is set to `TRUE`)


# selectInferToolkit 0.4.1

- First public release using `select_` and `infer_` nomenclature
