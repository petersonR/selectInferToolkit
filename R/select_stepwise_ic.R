#' Stepwise selection with AIC or BIC
#'
#' @description Forward, backward or bidirectional stepwise selection via
#' [MASS::stepAIC()]. Predictors are standardized first; see
#' [tidy.selector()].
#'
#' @param formula a model formula, as for [lm()]. It may include
#'   interactions (`x * z`) and in-formula transformations (`log(x)`,
#'   `poly(x, 2)`); factor, logical and character predictors are
#'   treatment-coded. Ordered factors and missing values are not supported.
#'   See [tidy.selector()] for how the columns are standardized and named.
#' @param data a data frame
#' @param family outcome distributional family: `"gaussian"` or
#'   `"binomial"` (`"poisson"` is not yet supported)
#' @param penalty `"AIC"` (default) or `"BIC"`
#' @param select_factors_together should the dummy columns of a factor enter
#'   or leave the model together?
#' @param direction the search direction: `"forward"` (default), `"backward"`
#'   or `"both"`
#' @param criterion which form of the information criterion to minimize:
#'
#'   * `"deviance"` (default): `n * log(RSS / n) + k * df`, as in
#'     [MASS::stepAIC()].
#'   * `"cp"`: the Mallows-Cp form `RSS + k * sigma^2 * df`, which is what
#'     `selectiveInference` conditions on.
#'
#'   Here `k = 2` for AIC and `log(n)` for BIC.
#'
#'   The two can stop at different steps. When they do, [infer_selective()]
#'   falls back from conditioning on the IC stopping rule (`type = "aic"`) to
#'   conditioning on a fixed number of steps (`type = "active"`), which treats
#'   the number of selected terms as fixed in advance; the effect of this seems
#'   to be minor. `"cp"` avoids the fallback. It requires
#'   `family = "gaussian"`, `direction = "forward"` and
#'   `select_factors_together = FALSE`.
#'
#' @param sigma residual standard deviation defining the `criterion = "cp"`
#'   penalty. Ignored when `criterion = "deviance"`. When `NULL` the same rule
#'   `selectiveInference::fsInf()` uses is applied.
#'
#' @param ... additional arguments passed to [MASS::stepAIC()]
#' @param trace passed to [MASS::stepAIC()]
#' @param fitted_selector a previously fit `selector`, used by [reselect()]
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#' @importFrom stats lm  model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @return a `selector`; see [tidy.selector()]. Its default inference method
#'   is [infer_boot()].
#' @export
#


select_stepwise_ic <- function(
    formula, data, family = c("gaussian", "binomial"),
    select_factors_together = FALSE,
    penalty = c("AIC", "BIC"),
    direction = c("forward", "backward", "both"),
    criterion = c("deviance", "cp"),
    sigma = NULL,
    trace = 0,
    fitted_selector = NULL,
    ...
) {


  if (is.null(fitted_selector)) {
    family    <- match.arg(family)
    penalty   <- match.arg(penalty)
    direction <- match.arg(direction)
    criterion <- match.arg(criterion)
    if (missing(formula))
      stop("Must supply formula")
  } else {
    meta      <- attr(fitted_selector, "meta")
    family    <- meta$family
    penalty   <- meta$penalty
    direction <- meta$direction
    formula   <- attr(fitted_selector, "orig_formula")
    select_factors_together <- meta$select_factors_together
    criterion <- meta$criterion %||% "deviance"
    # replay the sigma *argument*, not the value it resolved to: with sigma
    # left NULL the rule re-estimates on each resample, which is what
    # replaying the selection procedure means
    sigma     <- meta$sigma_arg
  }

  # Check for outcome misspecification
  outcome_var <- all.vars(formula)[1]
  y <- data[[outcome_var]]
  if (is.factor(y) || is.character(y)) {
    if (family == "gaussian") {
      stop("Outcome variable '", outcome_var, "' is a factor/character. ",
           "Did you mean to specify family = \"binomial\"?")
    }
  } else if (is.numeric(y) && length(unique(y)) == 2 && family == "gaussian") {
    message("Note: Outcome '", outcome_var, "' has only 2 unique values. ",
            "Consider setting family = \"binomial\" if this is a binary outcome.")
  }

  k_val <- if (penalty == "AIC") 2 else log(nrow(data))

  # Dummies are left uncentered (center_factors = FALSE), so the intercept
  # stays interpretable at "every indicator = 0", i.e. at the reference level
  # of each factor. They are still divided by their standard deviation, so
  # tidy(scale_coef = TRUE) reports every coefficient on a common per-SD
  # metric
  design <- if (is.null(fitted_selector))
    design_build(formula, data, center_factors = FALSE) else
    attr(fitted_selector, "design")
  X <- design_X(design, data)
  y <- design_y(design, data)

  # The units stepAIC adds and drops: each column on its own, or with
  # select_factors_together, each formula term (all of a factor's dummies)
  map <- design$map[design$map$kept, ]
  units <- if (select_factors_together)
    split(map$column, factor(map$term, levels = unique(map$term))) else
    stats::setNames(as.list(map$column), map$column)
  # syntactic stand-in names, for the formulas (and for stepAIC's trace); the
  # way back to columns is by position, never by parsing these
  unit_ids <- make.names(names(units), unique = TRUE)
  unit_df <- stepwise_unit_frame(X, y, units, unit_ids)

  formula_full <- reformulate(unit_ids, ".y")
  formula_null <- .y ~ 1
  formula_start <- if (direction == "backward") formula_full else formula_null

  sigma_used <- NULL

  if (criterion == "cp") {
    # Select on the same Mallows-Cp criterion that selectiveInference conditions
    # on, so infer_selective() never has to reconcile two different stopping
    # rules. Not ideal to use this because it won't match MASS::stepAIC,
    # usually users will go the other path in the `else` statement below.
    if (family != "gaussian")
      stop("criterion = \"cp\" requires family = \"gaussian\"; the Cp-form ",
           "stopping rule is defined for least squares only.")
    if (direction != "forward")
      stop("criterion = \"cp\" requires direction = \"forward\"; ",
           "selectiveInference::fs() only walks forward.")
    if (select_factors_together)
      stop("criterion = \"cp\" requires select_factors_together = FALSE; ",
           "the Cp path operates on a numeric design matrix.")

    fs_result <- selectiveInference::fs(X, y)

    si_fit <- suppressWarnings(selectiveInference::fsInf(
      fs_result, sigma = sigma, type = "aic", mult = k_val, ntimes = 1))

    sigma_used <- si_fit$sigma
    khat <- si_fit$khat

    keep <- unit_ids[fs_result$action[seq_len(khat)]]
    formula_sel <- if (length(keep)) reformulate(keep, ".y") else formula_null

    selected_model <- glm(formula_sel, data = unit_df, family = family)
    selected_model$call$data <- unit_df

  } else {

    model_start <- glm(formula_start, data = unit_df, family = family)

    model_start$call$data <- unit_df

    selected_model <- MASS::stepAIC(
      model_start,
      scope = list(lower = formula_null, upper = formula_full),
      direction = direction,
      k = k_val,
      trace = trace,
      ...
    )
  }

  ## bookkeeping: back from units to design columns. Aliased (NA)
  ## coefficients are dropped, and so is any unit left with none
  selected_coefs <- drop_aliased(stepwise_column_coefs(selected_model, units, unit_ids))
  selected_units <- attr(terms(selected_model), "term.labels")
  selected_terms <- names(units)[match(selected_units, unit_ids)]
  selected_terms <- selected_terms[vapply(units[selected_terms], function(cols)
    any(cols %in% names(selected_coefs)), logical(1))]

  meta_information <- list(
    family = family,
    select_factors_together = select_factors_together,
    penalty = penalty,
    direction = direction,
    criterion = criterion,
    # sigma_arg is what to replay on reselect(); sigma_used is the value this
    # fit actually stopped on, and is what infer_selective() must condition with
    sigma_arg = sigma,
    sigma_used = sigma_used
  )

  ## output
  as_selector(
    selected_model,
    name = "stepwise_ic",
    label = "Stepwise IC-based",
    design = design,
    default_infer = "boot",
    orig_formula = formula,
    selected_terms = selected_terms,
    selected_coefs = selected_coefs,
    meta = meta_information
  )
}

# A data frame with the outcome as `.y` and one variable per unit, named by
# `ids`: a single column, or for a factor kept whole, the matrix of its dummy
# columns, which glm() and stepAIC() treat as one term
stepwise_unit_frame <- function(X, y, units, ids) {
  df <- data.frame(.y = y)
  for (k in seq_along(units)) {
    cols <- units[[k]]
    df[[ids[k]]] <- if (length(cols) == 1) X[, cols] else
      unname(X[, cols, drop = FALSE])
  }
  df
}

# The fitted coefficients, named by design column. Each coefficient's unit
# and position within it come from the model matrix's `assign` attribute, so
# no names are parsed
stepwise_column_coefs <- function(fit, units, ids) {
  cf <- stats::coef(fit)
  assign <- attr(stats::model.matrix(fit), "assign")
  labels <- attr(terms(fit), "term.labels")
  cols <- vapply(seq_along(cf), function(j) {
    if (assign[j] == 0) return("(Intercept)")
    unit <- units[[match(labels[assign[j]], ids)]]
    pos <- sum(assign[seq_len(j)] == assign[j])
    unit[pos]
  }, character(1))
  stats::setNames(unname(cf), cols)
}
