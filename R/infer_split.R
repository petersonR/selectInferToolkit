
# TODO: Check intercept back-transformation in tidy.inferrer()
#
# When scale_coef = FALSE, this function divides the slope estimates
# and confidence limits by their design-matrix SDs, but leaves the
# intercept estimate and confidence interval unchanged.
#
# The package's design_X() centers and scales predictors using the
# selector's training design. Consequently, the fitted intercept
# corresponds to the centered/standardized predictor reference point,
# not necessarily to X = 0 on the original predictor scale.
#
# Observed with infer_split() using mtcars and select_glmnet():
#   Selected predictors: cyl, hp, wt
#   Manual held-out lm() intercept:       37.05876
#   tidy(fit, scale_coef = FALSE):         19.52682
#
# The unscaled slope estimates match manual lm(), suggesting the
# regression and selected-column mapping are working correctly.
#
# Potential fix: back-transform the intercept using the predictor
# centers and scales stored in the selector design. Its standard error,
# confidence interval, and p-value also need to be recomputed using
# the fitted coefficient covariance matrix (not just shifted).
#
# This should be reviewed for Gaussian/binomial/Poisson models,
# factors, interactions, and transformed predictors. Also consider
# effects on other inference methods before changing shared behavior.




#' Sample-splitting post-selection inference
#'
#' Randomly divide observations into independent selection and inference sets,
#' run one of the package's selectors on the former, then perform ordinary
#' unpenalized model-based inference using ONLY the latter. The existing
#' `infer_upsi()` engine is reused on held-out data, so results inherit the
#' package's `inferrer` conventions. This is single-split inference, not PoSI
#' simultaneous inference or multiple sample splitting.
#'
#' @param formula Model formula supplied to the selector.
#' @param data A data frame with rows representing independent observations.
#' @param selector A function such as `select_glmnet`, `select_ncvreg`,
#'   `select_stepwise_ic`, `select_full_model`, or `select_null_model`.
#'   Pass the function, not an already-fitted selector or a function call.
#' @param family One of `"gaussian"`, `"binomial"`, or `"poisson"`.
#' @param prop Fraction of observations used for selection, in (0, 1).
#' @param seed Optional integer seed. The caller's RNG state is restored.
#' @param strat_var Optional single column name used for stratified splitting.
#'   Each stratum must contain at least two observations. For stratification
#'   of binary outcomes, pass the response column name here.
#' @param conf_level Two-sided confidence level in (0, 1). Only 0.95 is
#'   currently supported if `infer_upsi()` does not accept a CI level.
#' @param ... Additional arguments forwarded unchanged to `selector()`
#'   (for example `lambda = "compact"`, `alpha = 1`). Do not include `data`,
#'   `formula`, `family`, or `fitted_selector` here.
#'
#' @return An object inheriting from `inferrer`, with an additional
#'   `split_inferrer` class. Metadata are available in `$split` and the
#'   original selection object in `$split_selector`. The `tidy()` method
#'   retains the package's full term-level row structure. For nonselected
#'   terms, inferential columns are `NA` (the `nonselection = "ignored"`
#'   convention).
#'
#' @details The selector is fitted entirely on the first partition; regular
#'   model-based Wald or t inference is then computed on the disjoint second
#'   partition. Interpretation requires independent observations and a
#'   correctly specified inference model. For logistic models, complete
#'   separation and sparse outcome counts can invalidate Wald inference.
#'   For Poisson models, overdispersion requires additional consideration.
#'   If `strat_var` depends on the outcome, conditioning on the observed split
#'   allocation can affect the usual model-based inference justification;
#'   use stratification with care.
#'
#' @examples
#' \dontrun{
#'   fit <- infer_split(mpg ~ ., mtcars,
#'                      selector = select_glmnet,
#'                      family = "gaussian", lambda = "compact",
#'                      alpha = 1, prop = 0.5, seed = 1234)
#'   broom::tidy(fit)
#' }
#' @export
infer_split <- function(formula, data, selector = select_glmnet,
                        family = c("gaussian", "binomial", "poisson"),
                        prop = 0.5, seed = NULL, strat_var = NULL,
                        conf_level = 0.95, ...) {
  family <- match.arg(family)
  if (!inherits(formula, "formula")) stop("`formula` must be a formula.", call. = FALSE)
  if (!is.function(selector)) stop("`selector` must be a function, e.g. select_glmnet, not a fitted object or call.",
                                   call. = FALSE)

  if (!is.null(strat_var) && (length(strat_var) != 1L || !is.character(strat_var) || !(strat_var %in% names(data))))
    stop("`strat_var` must name one column of `data`.", call. = FALSE)

  # Use only complete cases for all fields needed by the full original formula
  # and optionally the stratifying variable. This is done BEFORE randomization
  # so selection/inference splits do not silently drop different rows.

  relevant <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  complete <- stats::complete.cases(relevant)

  if (!is.null(strat_var)) complete <- complete & !is.na(data[[strat_var]])

  original_rows <- which(complete)
  data <- data[complete, , drop = FALSE]
  n <- nrow(data)

  if (n < 4L) stop("At least four complete observations are needed.", call. = FALSE)

  if (!is.null(seed)) {
    old_rng_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_rng_exists) old_rng <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (old_rng_exists) assign(".Random.seed", old_rng, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(as.integer(seed))
  }

  if (is.null(strat_var)) {
    n_select <- max(2L, min(n - 2L, floor(n * prop)))
    select_idx <- sort(sample.int(n, size = n_select, replace = FALSE))
  } else {
    strata <- as.character(data[[strat_var]])
    groups <- split(seq_len(n), factor(strata, levels = unique(strata)))
    if (any(lengths(groups) < 2L))
      stop("Every stratum must have at least two complete observations.", call. = FALSE)
    select_idx <- sort(unlist(lapply(groups, function(g) {
      k <- max(1L, min(length(g) - 1L, floor(length(g) * prop)))
      sample(g, k, replace = FALSE)
    }), use.names = FALSE))
  }
  inference_idx <- setdiff(seq_len(n), select_idx)
  if (length(select_idx) < 2L || length(inference_idx) < 2L)
    stop("Both partitions must contain at least two rows.", call. = FALSE)
  selection_data <- data[select_idx, , drop = FALSE]
  inference_data <- data[inference_idx, , drop = FALSE]

  # The formula/data/family supplied here come exclusively from the selection
  # subset. Tuning and cross-validation take place exclusively there as well.
  selected_fit <- selector(formula = formula, data = selection_data,
                           family = family, ...)
  if (!inherits(selected_fit, "selector"))
    stop("`selector()` did not return a `selector` object.", call. = FALSE)

  # Reuse the package's existing unpenalized inferential machinery, passing
  # only held-out observations. Its `nonselection = "ignored"` convention
  # retains nonselected terms as rows, with NA inference statistics.
  out <- infer_upsi(selected_fit, data = inference_data,
                    nonselection = "ignored")
  if (!inherits(out, "inferrer"))
    stop("`infer_upsi()` did not return an `inferrer` object.", call. = FALSE)
  out$split_selector <- selected_fit
  out$split <- list(
    selection_rows = original_rows[select_idx],
    inference_rows = original_rows[inference_idx],
    n_selection = length(select_idx),
    n_inference = length(inference_idx),
    n_excluded = sum(!complete),
    prop = prop,
    seed = seed,
    strat_var = strat_var,
    family = family,
    conf_level = conf_level
  )
  class(out) <- unique(c("split_inferrer", class(out)))
  out
}


#' Print single-split inference
#' @param x A `split_inferrer` object.
#' @param ... Unused.
#' @export
print.split_inferrer <- function(x, ...) {
  cat("Single-split post-selection inference (held-out regression)\n")
  cat(sprintf("Selection: %d rows; inference: %d rows; family: %s\n",
              x$split$n_selection, x$split$n_inference, x$split$family))
  tab <- broom::tidy(x)
  cat(sprintf("%d term row(s) returned (including nonselections with NA inference).\n", nrow(tab)))
  print(tab, ...)
  invisible(x)
}
