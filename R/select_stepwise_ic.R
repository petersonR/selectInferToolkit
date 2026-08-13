#' Stepwise forward/backward/bidirectional selection with AIC/BIC
#'
#'
#' @description This function implements forward/backward/bidirectional stepwise regression,
#' for use in the `selectInferToolkit` package
#'
#' @param formula a formula.
#' @param data data set
#' @param family outcome distributional family
#' @param penalty AIC, BIC
#' @param select_factors_together should categorical variables be jointly selected?
#' @param direction the mode of step wise search, can be one of "both", "backward", or "forward", with a default of "forward"
#' @param criterion which form of the information criterion to minimize.
#'   `"deviance"` (default) mirrors [MASS::stepAIC()]
#'   `"cp"` is the Mallows-Cp form, mirrors `selectiveInference`
#'
#'   The two may disagree,
#'   in which case the results returned by [`infer_selective()`] has to fall
#'   back from passing `type = "aic"` to selectiveInference to `type = "active"`,
#'   which may have minor inferential consequences as it doesn't account for
#'   uncertainty in the number of selected features, proceeding as though
#'   the number of steps taken by the algorithm was pre-ordained. The effect of
#'   this seems to be minor.
#'
#'   Requires `family = "gaussian"`, `direction = "forward"` and
#'   `select_factors_together = FALSE`.
#'
#' @param sigma residual standard deviation defining the `criterion = "cp"`
#'   penalty. Ignored when `criterion = "deviance"`. When `NULL` the same rule
#'   `selectiveInference::fsInf()` uses is applied.
#'
#' @param ... Additional arguments
#' @param trace passed to MASS::stepAIC
#' @param fitted_selector a previously fit `selector`, used for resampling
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#' @importFrom stats lm  model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @import recipes
#' @return A  `selector` object
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

  # Build recipe: zv -> center -> dummy -> zv -> scale.
  #
  # step_center() deliberately runs before step_dummy(), so indicators are left
  # uncentered and the intercept stays interpretable at "every indicator = 0",
  # i.e. at the reference level of each factor. step_scale() runs after, so
  # indicators are still divided by their standard deviation and
  # tidy(scale_coef = TRUE) reports every coefficient on a common per-SD metric
  rec_spec <- recipe(formula, data = data) %>%
    step_zv(all_predictors()) %>%
    step_center(all_numeric_predictors())

  if (!select_factors_together) {
    rec_spec <- rec_spec %>%
      step_dummy(all_factor_predictors(),
                 naming = function(...) dummy_names(..., sep = ""))
  }

  rec_spec <- rec_spec %>%
    step_zv(all_predictors()) %>%
    step_scale(all_numeric_predictors())


  ## prep on current data (important for reffiting)
  rec_obj <- prep(rec_spec, training = data)
  df <- bake(rec_obj, new_data = data)

  y_name <- names(bake(rec_obj, data[1, , drop = FALSE], all_outcomes()))
  x_names <- names(bake(rec_obj, data[1, , drop = FALSE], all_predictors()))

  # term name cleaning
  x_terms <- ifelse(grepl("[^a-zA-Z0-9._]", x_names),paste0("`", x_names, "`"),
                    x_names)

  # collect all terms
  formula_full <- as.formula( paste0(y_name, " ~ ", paste(x_terms, collapse = " + ")))
  formula_null <- as.formula(paste0(y_name, " ~ 1"))
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

    Xmat <- as.matrix(bake(rec_obj, new_data = data, all_predictors()))
    yvec <- bake(rec_obj, new_data = data, all_outcomes())[[1]]

    fs_result <- selectiveInference::fs(Xmat, yvec)

    si_fit <- suppressWarnings(selectiveInference::fsInf(
      fs_result, sigma = sigma, type = "aic", mult = k_val, ntimes = 1))

    sigma_used <- si_fit$sigma
    khat <- si_fit$khat

    keep <- x_names[fs_result$action[seq_len(khat)]]
    keep_terms <- ifelse(grepl("[^a-zA-Z0-9._]", keep), paste0("`", keep, "`"), keep)
    formula_sel <- as.formula(paste0(y_name, " ~ ", paste(keep_terms, collapse = " + ")))

    selected_model <- glm(formula_sel, data = df, family = family, x = TRUE,
                          y = TRUE, model = TRUE)
    selected_model$call$data <- df

  } else {

    model_start <- glm( formula_start, data = df, family = family, x = TRUE,
                        y = TRUE, model = TRUE )

    model_start$call$data <- df

    selected_model <- MASS::stepAIC(
      model_start,
      scope = list(lower = formula_null, upper = formula_full),
      direction = direction,
      k = k_val,
      trace = trace,
      ...
    )
  }

  ## bookkeeping
  mm <- model.matrix(formula, data = data)
  all_terms <- make.names(colnames(mm))
  if(all_terms[1] =="X.Intercept.") all_terms[1] = "(Intercept)"

  selected_terms <- attr(terms(selected_model), "term.labels")
  selected_coefs <- coef(selected_model)

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

  orig_formula =formula

  ## output
  as_selector(
    selected_model,
    name = "stepwise_ic",
    label = "Stepwise IC-based",
    all_terms = all_terms,
    recipe_obj = rec_obj,
    default_infer = "boot",
    #formula_full = formula_full,
    orig_formula = formula,
    selected_terms = selected_terms,
    selected_coefs = selected_coefs,
    meta = meta_information
  )
}
