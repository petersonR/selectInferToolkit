#' Inference based on selective inference
#'
#' A wrapper for the `selectiveInference` functions on `selector` objects
#'
#' @param object a `selector` object
#' @param data data must be passed to infer
#' @param nonselection  A character string specifying how to handle variables
#'   not selected by model selection procedure. One of "ignored",
#'   "confident_nulls" or "uncertain_nulls" supported
#' @param conf.level .95 by default
#' @param use_cv_sigma estimate Sigma via CV (if FALSE, uses SI defaults)
#' @param sigma optional known or pre-computed residual standard deviation. When
#'   supplied it is passed straight through to the `selectiveInference` function
#'   and no sigma is estimated. The automatic estimate used when `p > n/2`
#'   calls `selectiveInference::estimateSigma()` (for `glmnet`), which runs
#'   cross-validation, thus important to `set.seed()`.
#' @param on_mismatch what to report when the selector's stopping point and
#'   `selectiveInference`'s disagree. For `stepwise_ic` selectors the two
#'   minimize **slightly** different criteria and can disagree. Inference falls
#'   back to the model the selector returned, by conditioning on a fixed number
#'   of steps; this argument controls how loudly that is reported:
#'
#'   * `"warn-fall-back"` (default) warns and points at
#'     `select_stepwise_ic(criterion = "cp")`, which makes the two rules agree.
#'   * `"silent-fall-back"` falls back quietly.
#'   * `"stop"` errors instead.
#'
#'   The fallback does not account for uncertainty in how many variables were
#'   selected, proceeding as though the number of steps had been pre-ordained;
#'   the effect of this appears to be minor.
#'
#' @param ... arguments passed to `selectiveInference` function(s)
#'
#' @importFrom broom tidy
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom selectiveInference fs fsInf fixedLassoInf
#'
#' @return `inferrer` object
#' @rdname infer
#' @export
#'
infer_selective <- function(
    object,
    data,
    nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
    conf.level = .95,
    use_cv_sigma = FALSE,
    sigma = NULL,
    on_mismatch = c("warn-fall-back", "silent-fall-back", "stop"),
    ...
  ){

  if (!inherits(object, "selector"))
    stop("`object` must be a `selector`. ",
         "Did you pass an `inferrer` by mistake? Use a select_* function first.")

  # Check method supported
  supported <- c("glmnet", "stepwise_ic")
  type <- attr(object, "name")


  if(!(type %in% supported))
    stop("Currently SI only supported for stepwise IC or `glmnet`")

  if (type == "glmnet") {
    alpha <- attr(object, "meta")[["alpha"]]

    if (is.null(alpha) || alpha != 1) {
      stop("Currently SI only supported for glmnet with alpha = 1 (lasso penalty)")
    }
  }

  nonselection <- match.arg(nonselection)
  on_mismatch <- match.arg(on_mismatch)

  # grab useful components from model
  X <- bake(attr(object, "recipe_obj"), new_data = data, all_predictors())
  y <- bake(attr(object, "recipe_obj"), new_data = data, all_outcomes())[[1]]
  beta <- coef(object)
  meta <- attr(object, "meta")

  sig <- NULL
  # recorded on the returned inferrer so callers can tell when the numbers are
  # not actually selection-adjusted
  si_meta <- list()

  if(type == "stepwise_ic") {
    if(meta$direction != "forward")
      stop("Only forward stepwise with IC currently supported")

    if(meta$family != "gaussian")
      stop("Only Gaussian supported for selective inference with stepwise IC (try glmnet?)")

    ## Run stepwise IC for purpose of SI, using fs function
    fs_result <- fs(as.matrix(X), y)

    # Determine sigma
    p <- ncol(X)
    n_obs <- length(y)

    # A criterion = "cp" selector already stopped on an exact sigma. Reusing it
    # is what makes the two stopping rules agree; re-estimating here (or letting
    # fsInf pick its own) may move the stopping point and reintroduce the
    # mismatch that criterion exists to remove.
    sel_criterion <- meta$criterion %||% "deviance"

    if(!is.null(sigma)) {
      sig <- sigma
      if(sel_criterion == "cp" && !isTRUE(all.equal(sigma, meta$sigma_used)))
        warning(paste(
          "`sigma` differs from the value this selector selected with",
          sprintf("(%.6g)", meta$sigma_used),
          "- the stopping rules may no longer agree."))
    } else if(sel_criterion == "cp") {
      sig <- meta$sigma_used
    } else if(use_cv_sigma) {
      sig <- selectiveInference::estimateSigma(as.matrix(X), y)$sigmahat
      warning("use_cv_sigma with stepwise_ic may yield unexpected results")
    } else {
      sig <- NULL  # let fsInf use its default
    }

    sel_vars_stepaic <- names(beta)[names(beta) != "(Intercept)"]

    if(length(sel_vars_stepaic) == 0) {
      warning("No variables selected: falling back to the intercept-only model")
      si_meta$unadjusted_fallback <- TRUE

      empty_model <- tidy(infer_upsi(object, data = data, conf.level = conf.level))

      inferences <- data.frame(term = empty_model$term, selected = 1,
                               estimate = empty_model$estimate,
                               ci_low = empty_model$ci_low,
                               ci_high = empty_model$ci_high,
                               p_value = empty_model$p_value)
      res <- list()
    } else {
      # Get IC-based selection with confidence intervals
      mult <- ifelse(meta$penalty == "AIC", 2, log(length(y)))

      res <- withCallingHandlers(selectiveInference::fsInf(
        fs_result,
        sigma = sig,
        type = "aic",
        mult = mult,
        # stop at the first IC increase, which is what MASS::stepAIC() does
        ntimes = 1,
        # not to be confused with the other alpha.
        alpha = 1 - conf.level,
        ...
      ), warning = si_sigma_warning)
      names(res$vars) <- names(X)[res$vars]
      bb <- res$sign* as.vector(res$vmat %*% y)

      # fsInf(type = "aic") re-runs its own stopping rule, which does not always
      # terminate at the same step as MASS::stepAIC() did. `on_mismatch` decides
      # which way to resolve it.

      si_meta$conditioning <- "aic"

      if(length(sel_vars_stepaic) != length(res$vars)) {

        # The sigma the type = "aic" call above used, including the one it
        # derived (and warned about) when `sig` was NULL. Captured before `res`
        # is overwritten below.
        sig_used <- res$sigma

        # Shared preamble; the two paths differ in what they then do about it,
        # so don't describe the fallback in text the "stop" path also prints.
        msg <- paste0(
          "Selective inference stopped at ", length(res$vars), " variable(s) ",
          "where the selector kept ", length(sel_vars_stepaic),
          " (sigma = ", format(sig_used, digits = 3), ").\n",
          "To make the two rules agree, re-run selection with the matching ",
          "criterion:\n",
          "  select_stepwise_ic(..., criterion = \"cp\")"
        )

        if(on_mismatch == "stop")
          stop(paste0(msg, "\nOr allow the fallback with ",
                      "on_mismatch = \"warn-fall-back\" / \"silent-fall-back\"."),
               call. = FALSE)

        if(on_mismatch == "warn-fall-back")
           warning(paste0(msg, "\nFalling back to conditioning on a fixed ",
                             "number of steps."))

        # type = "active" conditions on the first k steps as though k had been
        # fixed in advance.
        #
        # Pass `sig_used` rather than `sig`: when `sig` is NULL fsInf derives a
        # sigma and warns while doing so, and a second NULL would repeat that
        # warning verbatim. The derivation is deterministic, so this is the same
        # number either way, and reusing it makes explicit that both
        # conditioning events are built on one sigma.
        si_meta$conditioning <- "active"
        res <- selectiveInference::fsInf(
          fs_result,
          sigma = sig_used,
          type = "active",
          k = length(sel_vars_stepaic),
          alpha = 1 - conf.level,
          ...
        )
        names(res$vars) <- names(X)[res$vars]
        bb <- res$sign* as.vector(res$vmat %*% y)
      }

      inferences <- data.frame(term = names(res$vars), selected = 1, estimate = bb,
                               ci_low = res$ci[,1], ci_high = res$ci[,2],
                               p_value = res$pv)

    }
  }

  ## Run selective inference on glmnet
  if(type == "glmnet") {
    n<- nrow(X)
    p <- ncol(X)
    sig <- NULL
    b <- coef(object, use_native = TRUE, s=meta$lambda_used,
              exact = TRUE, x = X, y = y)[-1]

    if(!is.null(sigma)) {
      sig <- sigma
    } else if(use_cv_sigma) {
      # Similar to estimateSigma, but no new CV required.
      nz = sum(b != 0)
      rss <- min(object$cvm) * n
      sig <- sqrt(rss/(n - nz - 1))

    } else if(p > n / 2) {

      sig <- selectiveInference::estimateSigma(as.matrix(X), y)$sigmahat
      message(paste("p > n/2: sigma estimated with cross-validation, so set.seed()",
                    "or supply `sigma` for reproducible intervals."))
    }

    # fixed lasso function requires no intercept in beta vector
    if(all(b == 0)) {
      warning("No betas selected at that value of lambda")
      si_meta$unadjusted_fallback <- TRUE
      res <- list(
        vmat = rbind(rep(NA, length(y))),
        vars = c(None = NA),
        ci = cbind(NA, NA),
        pv = NA
      )

    } else {
      res <- withCallingHandlers(selectiveInference::fixedLassoInf(
        x = as.matrix(X),
        y = y,
        beta = b,
        lambda = meta$lambda_used * n,
        family = meta$family,
        alpha = 1 - conf.level,
        sigma = sig,
        ...
      ), warning = si_sigma_warning)
    }
    bb <- res$vmat %*% y
    inferences <- data.frame(term = names(res$vars), selected = 1, estimate = bb,
                             ci_low = res$ci[,1], ci_high = res$ci[,2],
                             p_value = res$pv)
  }


  # Handle non-selections
  term_to_col <- tibble(
    term =  colnames(X),
    col  = colnames(X))

  results <- fill_in_nonselections(inferences, object,
                                   nonselection = nonselection, X = X, y = y,
                                   conf.level = conf.level, term_to_col = term_to_col )

  # Return inferrer class
  as_inferrer(
    res, "selective", label = "Selective",
    nonselection = nonselection,
    conf.level = conf.level, selector = object, meta = si_meta,
    inferences = results)
}

