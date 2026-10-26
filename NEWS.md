# selectInferToolkit (development version)

- **Breaking:** `infer_boot()` replaces the reserved `n_cores` with
  `cluster`: pass a cluster from `parallel::makeCluster()` (or
  `makeForkCluster()`) to fit the resamples in parallel; the default `NULL`
  fits them sequentially. The progress bar is shown either way. Warnings
  from fitting the resamples, which a cluster would otherwise drop, are now
  given once each with the number of resamples that raised them. A seed for
  each resample is drawn before any fitting, and the resample's rows (and any
  random numbers its fit draws) come from that seed, so for a given seed the
  results are the same with or without a cluster. Passing `n_cores` is now an error
  pointing to `cluster` (#5).

- `infer_boot()` is faster: the design is applied to the data once and each
  resample takes its rows, instead of rebuilding the model matrix per
  replicate, and replicates are summarized as a coefficient matrix rather
  than per-replicate `tidy()` tables. Because of the per-resample seeds,
  bootstrap results for a given `set.seed()` differ from earlier versions
  (#7).

- **Breaking:** for `select_ncvreg()`, `infer_boot()` refits each resample
  at `lambda_eff` and pads with `gamma * lambda_eff` (`lambda_eff` with
  `pad = "all"`). `lambda_eff`, now stored in the selector's meta, is the
  smallest lambda in the run of the path next to the cross-validated choice
  that gives the same selected model. CV error is nearly flat over that run,
  so the CV choice can sit near the smallest selected effect. In the
  simulations in #34 (independent predictors, one strong effect) the null
  zone at the CV choice did not shrink as `n` grew, while at `lambda_eff` it
  shrank roughly like `1/sqrt(n)`. Predictions and `coef()` stay at
  the CV choice, so `estimate` can now differ from `coef`. `lambda_eff` is
  reported in the inferrer's meta. `lambda_eff` is searched only over the
  cross-validated lambdas, which are finite in every fold, and each resample
  is fit only down to the lambda it needs. A resample whose path ends before
  that lambda (a binomial fit that saturates, e.g. with separation, or one
  that reaches `dfmax`) used to stop the whole bootstrap; it is now fit at
  the smallest lambda it reached, with a warning giving the count
  (`n_short_path` in the meta). A selector from `reselect()` has no
  `lambda_eff`, so its resamples are refit at `lambda_used`. With
  `penalty = "lasso"` this applies too, with a warning when `lambda_eff` is
  below the CV choice (#34).

- `select_stepwise_ic()` checks the outcome's type only on the first fit, so
  the two-unique-values note is no longer repeated for every resample.

- `select_ncvreg(lambda = "compact")` now uses the standard one-SE rule: the
  largest lambda whose CV error is within one SE of the minimum, the SE taken
  at the minimum. It used `cve < min(cve + cvse)`, a stricter cutoff that
  could pick a smaller lambda and a larger model than documented.

# selectInferToolkit 0.5.0

This release replaces `infer_boot()`'s `debias` with a new `pad` method,
rebuilds the predictor matrix on `model.matrix()` (dropping `recipes`, `purrr`
and `forcats`), and fixes several bugs. Results for a given `set.seed()` change
for anything using cross-validation or the bootstrap. The two biggest changes
are listed first.

- **Breaking:** `infer_boot()` gains `pad`, and loses `debias` and
  `estimation_data`. Because a non-selected coefficient is recorded as 0 in
  each replicate, the percentile interval of a small, rarely selected
  coefficient collapses toward `[0, 0]`. `pad` fixes this:
  * `"zero_side"` (the new default) pushes the endpoints of any interval that
    contains 0 out to at least `-delta` / `delta`, a region of "reasonably
    confident null" that shrinks with `n`. Intervals that exclude 0 are
    unchanged, so large effects pay nothing.
  * `"all"` widens every interval by `delta`.
  * `"none"` keeps the previous (`debias = FALSE`) behavior.

  `delta` is set from the selector: `gamma * lambda` for MCP/SCAD (`lambda`
  with `"all"`), `lambda` for the lasso, `sqrt(k)` times the full-model
  standard error for stepwise IC, and 0 for the full and null models. It is
  returned in a new `null_radius` column of `tidy()`, unscaled along with the
  intervals when `scale_coef = FALSE`; the column is left out when nothing is
  padded (`pad = "none"`, or the full and null models). Zero-side padding
  warns for lasso selectors, whose bias does not vanish for large effects
  (use `pad = "all"`), and any padding warns for an elastic net and for
  non-gaussian families.

  `debias = TRUE` (refitting OLS within each replicate, with uncertain-null
  refits for non-selections) is removed; padding needs the selector's own
  thresholded estimates. `estimation_data` only chose the data that refit used,
  so it is removed too. `ciratio()` takes `pad` in place of `debias`.

- The predictor matrix is now built the way `model.matrix()` builds it, and
  one internal term map drives `tidy()`, `predict()`, `reselect()` and every
  `infer_*` method, instead of `recipes` plus name-matching (#29). As a
  result:
  - Factor levels that start with a digit (e.g. `factor(am)`, giving `am1`)
    are no longer reported as unselected with coefficient 0.
  - Columns keep their `model.matrix()` names, e.g. `spmuch less` rather than
    `spmuch.less`.
  - Character predictors work with every selector and are treated as
    factors, as in `lm()`. Previously `select_full_model()` and
    `select_ncvreg()` errored on them, and `select_glmnet()` silently dropped
    them.
  - Formulas may now include interactions (`x * z`) and transformations
    (`log(x)`, `poly(x, 2)`). Numeric variables are centered before
    interactions are formed.
  - Ordered factors now error, asking for a conversion to numeric scores or
    unordered levels; previously some selectors errored and
    `select_glmnet()` silently dropped them.
  - Missing values now error in every selector, with a suggestion to impute
    or drop rows. Previously `select_stepwise_ic()` silently dropped
    incomplete rows. `predict()` still returns `NA` for incomplete rows of new
    data.
  - A factor level unseen when the selector was fit now gives a clear error
    in `predict()`.
  - With `select_stepwise_ic(select_factors_together = TRUE)`, dummies are now
    standardized like every other column, so `tidy(scale_coef = TRUE)` is on
    a per-SD metric for them too. Their bootstrap intervals were previously
    `NA` for levels whose names were not syntactic.
  - `select_stepwise_ic()` no longer re-estimates centering and scaling on
    each `reselect()`, matching the other selectors and `?reselect`. This
    changes the scaled metric of its bootstrap draws, not which terms are
    selected.
  - `recipes`, `purrr` and `forcats` are no longer dependencies, and the
    test suite runs in about half the time.

- Results for a given `set.seed()` change for anything using
  cross-validation or the bootstrap. `recipes` drew a random number for every
  recipe step it created, which shifted the CV folds and the bootstrap
  resamples.

- `reselect()` on a `select_ncvreg()` selector now replays the arguments
  originally passed through `...`. Previously it dropped them, so a
  `penalty = "lasso"` or `"SCAD"` selector was refit, and bootstrapped, with
  ncvreg's default MCP penalty.

- `infer_upsi()` intervals for gaussian models now use t quantiles, matching
  their p-values; they previously used normal quantiles, so they were
  slightly too narrow. The same applies to the intervals that
  `nonselection = "uncertain_nulls"` fills in for unselected terms.

- Logical predictors now work with every selector. They are coded like a
  two-level factor, giving a column named `xTRUE`, as in `model.matrix()`.
  Previously
  `select_full_model()` errored, and `select_glmnet()` and `select_ncvreg()`
  reported them as unselected even when they were in the model.

- Coefficients that `glm()` cannot estimate (aliased columns, e.g. a factor
  level absent from a bootstrap resample or a `reselect()` subset) are no
  longer reported as selected with coefficient 0 by `select_full_model()` and
  `select_stepwise_ic()`, and no longer make `predict()` return `NA`.

- The selector help pages and README were corrected and updated (#27). Among
  other fixes, `?select_glmnet` and `?select_ncvreg` now explain
  `lambda = "best"` / `"compact"`; `select_ncvreg()`'s `lambda` is documented
  as `"best"` or `"compact"` only (a numeric value was documented but never
  accepted); and the README's stepwise selective-inference example uses
  `criterion = "cp"`.

- `?tidy.inferrer` and `?tidy.selector` now document every output column and
  explain the scaled metric: by default coefficients are per standard
  deviation of each predictor, `scale_coef = FALSE` gives per-unit slopes, and
  the intercept is always at the predictor means (#23).

- Help pages reorganized (#26): each `infer_*` function has its own page, the
  `selector` and `inferrer` methods (`tidy()`, `predict()`, `coef()`,
  `print()`) have "Methods for ..." pages, and `reselect()` has its own. The
  constructors `as_selector()` and `as_inferrer()` stay exported for writing
  new methods but are marked internal. Stale Value sections were replaced.

- `ciratio()` is no longer exported; it remains available as
  `selectInferToolkit:::ciratio()` (#26).

- `infer_boot()` now errors on any unrecognized argument instead of silently
  ignoring it through `...`. `type = "residual"` errors as not yet implemented,
  and supplying `n_cores` warns that it currently has no effect (it is reserved
  for #5) (#25).

- `boot()`, the worker behind `infer_boot()`, is no longer exported, so it no
  longer masks `boot::boot()` (#25).

- `select_ncvreg()` selectors now default to `infer_boot()` in `infer()`
  (was the experimental `infer_pipe()`), since zero-side padding targets MCP
  (#24). `infer_pipe()` is still available directly.

- `infer()` passes `nonselection` only to methods that take it
  (`infer_upsi()`, `infer_selective()`). When the default method has no such
  option (`infer_boot()`, `infer_pipe()`) and `nonselection` is supplied, it
  now warns that it is ignored instead of silently dropping it (#24). The three
  `nonselection` modes are now defined in `?infer_upsi`.

- `infer_selective()` for `glmnet` selectors now conditions on a refit of the
  lasso at the selected lambda with a tight convergence tolerance (`thresh =
  1e-14`) (#18). With glmnet's default tolerance, the active set and signs on
  correlated designs can be slightly off, and the polyhedral intervals
  silently undercovered (false coverage rate 0.076 vs. a 0.05 target in the
  issue's example; 0.044 after this change). The previous "exact" refit via
  `coef(..., exact = TRUE)` never actually refit, because glmnet skips it when
  lambda is already on the path. When the refit's active set or signs differ
  from the selector's, `on_mismatch` controls whether this warns (default),
  is silent, or errors; the warning suggests re-selecting with
  `control = list(thresh = 1e-14)`.

- `infer_selective()` now works for `select_glmnet()` selectors with
  `family = "binomial"` (#19). `selectiveInference::fixedLassoInf()` requires
  the intercept in `beta` for the binomial family (and its absence for
  gaussian), and the binomial result has no `vmat`, so estimates are taken from
  its debiased `coef0` instead.

- `select_stepwise_ic()` gains `criterion`, choosing which form of the
  information criterion to minimize. The default `"deviance"` is
  `MASS::stepAIC()`'s `n*log(RSS/n) + mult*df`; the new `"cp"` is the Mallows-Cp
  form `RSS + mult*sigma^2*df` that `selectiveInference` conditions on.

  Selecting with `criterion = "cp"` means `infer_selective()` conditions on
  exactly the model that was selected. Across every setting tested this
  eliminated the fallback described below. 

- `select_stepwise_ic()` gains `sigma`, defining the `criterion = "cp"` penalty.
  When `NULL` the value `selectiveInference::fsInf()` derives is used and
  recorded, so selection and inference condition on an identical criterion.
  `reselect()` replays the argument rather than the resolved value, so a `NULL`
  sigma is re-estimated per bootstrap resample. Supplying a known sigma did not
  measurably change type I error relative to the estimated default.

- `infer_selective()` gains `on_mismatch`, controlling what happens when the
  selector's stopping point and `selectiveInference`'s disagree. The options are:
  `"silent-fall-back"`, `warn-fall-back` (default), and `"stop"`. Warnings and errors
  point the user towards appropriately re-running the selector to more carefully
  match the inferrer's structural conditions (e.g. adding `criterion = "cp"` to
  `select_stepwise_ic`). 

- `infer_selective()` no longer estimates sigma with
  `selectiveInference::estimateSigma()` for `stepwise_ic` selectors when
  `p > n/2`, reverting a 0.4.3 change. Neither `fs()` nor `fsInf()` does this by
  default -- `fsInf()` uses the full-model OLS estimate when `n >= 2p` and
  `sd(y)` otherwise -- and the automatic switch changed inferences in ways that
  are not obvious. It also runs an unseeded cross-validation, so it
  advanced the RNG stream and broke `set.seed()` reproducibility across package
  versions. `estimateSigma()` is now reached only via `use_cv_sigma = TRUE`,
  which warns that its effect on inferences is unclear. `glmnet` selectors are
  unchanged, where the same caveat about `set.seed()` still applies.

- `infer_selective()` restates `selectiveInference`'s "p > n/2, and sd(y) used
  as an estimate of sigma" warning in terms of this package's arguments. The
  original points at `selectiveInference::estimateSigma()`, which users of this
  package reach through `use_cv_sigma = TRUE` rather than calling directly; the
  replacement says so, and notes that `sd(y)` is biased upward whenever a
  predictor is genuinely associated with the outcome. Other warnings from
  `selectiveInference` pass through unchanged.

- `infer_selective()` no longer emits that warning twice on the fallback path.
  It called `fsInf()` a second time with `sigma = NULL`, so the sigma was
  re-derived and re-warned about; the second call now reuses the value the first
  one reported. Results are unchanged -- the derivation is deterministic.

- `%||%` is now imported from `rlang`. It was used in `R/utils.R` but never
  imported, so the package relied on the base R 4.4 definition despite
  declaring `R (>= 3.5)`.


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
