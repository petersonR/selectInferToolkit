#' Inference for `selector` via bootstrapping
#'
#' @param object a `selector` object
#' @param data the data used to fit the selector
#' @param inference_target is inference requested on all or selected only
#' @param pad how to pad the bootstrap percentile intervals, which otherwise
#'   collapse toward `[0, 0]` for small nonzero coefficients the selector
#'   rarely picks. Within each replicate a non-selected coefficient is 0.
#'
#'   * `"zero_side"` (default): an interval that contains 0 has each endpoint
#'     pushed out to at least `-delta` / `delta`; intervals that exclude 0 are
#'     unchanged. `[-delta, delta]` is a region of "reasonably confident null"
#'     that shrinks with `n`.
#'   * `"none"`: the raw percentile interval ("confident nulls").
#'   * `"all"`: every interval is widened by `delta` on both sides.
#'
#'   The radius `delta` is set from the selector (scaled metric) and returned
#'   in the `null_radius` column of `tidy()`. The column is left out when no
#'   interval could be padded (`pad = "none"`, or a radius of 0):
#'
#'   * `select_ncvreg()`: `gamma * lambda` for MCP/SCAD with `"zero_side"`,
#'     `lambda` with `"all"`.
#'   * `select_glmnet()` and lasso `select_ncvreg()`: `lambda`. The lasso's
#'     bias does not vanish for large effects, so `"zero_side"` warns; `"all"`
#'     is the uniformly valid choice.
#'   * `select_stepwise_ic()`: `sqrt(k)` times the full-model standard error,
#'     with `k = 2` (AIC) or `log(n)` (BIC). Requires `n > p + 1`.
#'   * `select_full_model()` / `select_null_model()`: 0, so no padding.
#'
#'   Coverage guarantees are for the gaussian family in an orthonormal design;
#'   other families warn.
#' @param conf.level .95 by default
#' @param type the bootstrap type. Only `"paired"` (resampling rows) is
#'   implemented; `"residual"` is reserved and currently an error.
#' @param B number of bootstrap resamples
#' @param n_cores reserved for parallel bootstrapping (see GitHub issue #5).
#'   Resampling is currently sequential, so this has no effect, and
#'   supplying it warns.
#' @param ... not used. Any argument passed here is an error, so that a
#'   misspelled or removed argument (such as `debias`) can't be silently
#'   ignored.
#' @return an `inferrer` object; see [tidy.inferrer()]. Its inferences include
#'   `prop_selected` (the proportion of resamples in which each term was
#'   selected) and, when intervals are padded, `null_radius` (the padding
#'   radius; see `pad`).
#' @importFrom dplyr right_join group_by summarize bind_rows
#'
#' @export

infer_boot <- function(
  object,
  data,
  inference_target = c("selections", "all"),
  pad = c("zero_side", "none", "all"),
  conf.level = .95,
  type = c("paired", "residual"),
  B = 250,
  n_cores = 4,
  ...) {

  if (!inherits(object, "selector"))
    stop("`object` must be a `selector`. ",
         "Did you pass an `inferrer` by mistake? Use a select_* function first.")

  # `...` would otherwise silently swallow arguments, changing results
  # unannounced
  dots <- list(...)
  if (length(dots)) {
    nms <- names(dots)
    if (is.null(nms)) nms <- rep("", length(dots))
    removed <- intersect(nms, c("debias", "estimation_data"))
    if (length(removed))
      stop("`", paste(removed, collapse = "`, `"), "` ",
           if (length(removed) > 1) "were" else "was",
           " removed from infer_boot(); see `pad` for handling near-zero ",
           "intervals.", call. = FALSE)
    unknown <- ifelse(nzchar(nms), paste0("`", nms, "`"), "(unnamed)")
    stop("Unused argument(s) to infer_boot(): ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }

  # A bit of argument checking
  inference_target <- match.arg(inference_target)
  pad <- match.arg(pad)
  type <- match.arg(type)

  if (type != "paired")
    stop("type = \"", type, "\" is not yet implemented; use \"paired\".",
         call. = FALSE)
  if (!missing(n_cores))
    warning("`n_cores` is reserved for parallel bootstrapping and currently ",
            "has no effect.", call. = FALSE)

  boot(object, data = data, B = B, inference_target = inference_target,
       pad = pad, conf.level = conf.level)
}


# Internal worker behind infer_boot(): resample, reselect, and summarize.
# Not exported, so it doesn't mask boot::boot().
#
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if filter select mutate summarise bind_rows rename arrange right_join transmute
#' @importFrom broom tidy
#' @importFrom stats lm model.frame model.matrix na.pass
#' @importFrom tibble tibble
#' @importFrom MASS stepAIC
#' @importFrom rlang sym
#' @importFrom parallel detectCores clusterExport clusterEvalQ makeCluster
#' @importFrom pbapply pblapply
#' @importFrom stats as.formula glm coef formula median quantile sd terms
#' @importFrom utils tail
#' @importFrom rlang .data
#' @noRd
boot <- function(object, data, B,
                 inference_target = c("selections", "all"),
                 pad = c("zero_side", "none", "all"),
                 conf.level) {
  stopifnot(inherits(object, "selector"))
  pad <- match.arg(pad)

  design <- attr(object, "design")
  X_full <- design_X(design, data)
  y_full <- design_y(design, data)

  # Uncomment after debug
  # # do with parallel computing, Number of cores to use
  # cl <- parallel::makeCluster(n_cores)
  #
  # # Export required variables to each worker
  # parallel::clusterExport(cl, varlist = c("object", "data", "nonselection"), envir = environment())
  #
  # # Ensure required packages are loaded in each worker
  # parallel::clusterEvalQ(cl, {
  #   library(broom)
  #   library(dplyr)
  #   library(selectInferToolkit)
  # })

  message("Bootstrapping with B = ", B, " replicates...")
  boot_fits <- pbapply::pblapply(1:B, function(b) {
    boot_idx <- sample(1:nrow(data), replace = TRUE)
    data_boot <- data[boot_idx,]

    sel_boot = reselect(object, newdata = data_boot)
    val_boot <- tidy(sel_boot)

    val_boot$estimate <- ifelse(is.na(val_boot$coef), 0, val_boot$coef)

    val_boot
  })

  boot_df<- bind_rows(boot_fits, .id = "bootstrap")

  if(inference_target=="selections") {
    results <- boot_df  %>%
      filter(.data$term %in% names(coef(object))) %>%
      select("term", "coef", "estimate") %>%
      group_by(.data$term)  %>%
      summarise(
        estimate_m = mean(.data$estimate),
        ci_low   = quantile(.data$estimate, (1 - conf.level) / 2),
        ci_high  = quantile(.data$estimate, 1 - (1 - conf.level) / 2),
        prop_selected = mean(coef != 0),
        .groups = "drop"
      )%>%
      rename(estimate = "estimate_m") %>%
      right_join(tidy(object)[,1], by = "term")%>%
      arrange(match(.data$term, unique(boot_df$term)))
  }

  if(inference_target=="all") {
    results <- boot_df  %>%
      select("term", "coef", "estimate") %>%
      group_by(.data$term)  %>%
      summarise(
        estimate_m = mean(.data$estimate),
        ci_low   = quantile(.data$estimate, (1 - conf.level) / 2),
        ci_high  = quantile(.data$estimate, 1 - (1 - conf.level) / 2),
        prop_selected = mean(coef != 0),
        .groups = "drop"
      )%>%
      rename(estimate = "estimate_m") %>%
      arrange(match(.data$term, unique(boot_df$term)))
  }


  # Pad the percentile intervals (scaled metric; the intercept is never padded)
  radius <- pad_radius(object, pad, X_full, y_full, results$term)
  results$null_radius <- unname(radius[results$term])
  results <- pad_intervals(results, pad)
  # Only report the radius when padding could have changed an interval
  # (not for pad = "none", nor for the full and null models' radius of 0)
  if (!any(results$null_radius > 0, na.rm = TRUE))
    results$null_radius <- NULL

  meta_information <- list(
    B = B,
    inference_target = inference_target,
    pad = pad
  )

  as_inferrer(
    boot_df,
    name = "boot",
    label = "Bootstrap",
    nonselection = "N/A",
    inferences = results,
    conf.level = conf.level,
    selector = object,
    meta = meta_information
  )
}


# Null-zone radius for each term, on the scaled metric; NA for the intercept
# (never padded). See the `pad` documentation in infer_boot().
pad_radius <- function(object, pad, X, y, terms) {
  radius <- stats::setNames(rep(NA_real_, length(terms)), terms)
  slopes <- terms != "(Intercept)"
  if (pad == "none" || !any(slopes)) return(radius)

  name <- attr(object, "name")
  meta <- attr(object, "meta")

  if (name %in% c("full_model", "null_model")) {
    radius[slopes] <- 0
    return(radius)
  }

  if (!identical(meta$family, "gaussian"))
    warning("Padding radii are derived for the gaussian family; ",
            "coverage for family = \"", meta$family, "\" is untested.",
            call. = FALSE)

  if (name == "ncvreg") {
    penalty <- meta$penalty
    gamma <- if (inherits(object, "cv.ncvreg")) object$fit$gamma else object$gamma
    if (penalty == "lasso" && pad == "zero_side")
      warn_lasso_zero_side()
    radius[slopes] <- if (pad == "zero_side" && penalty != "lasso")
      gamma * meta$lambda_used else meta$lambda_used

  } else if (name == "glmnet") {
    if (pad == "zero_side")
      warn_lasso_zero_side()
    if (!isTRUE(meta$alpha == 1))
      warning("Padding by lambda does not account for the ridge part of an ",
              "elastic net penalty (alpha < 1).", call. = FALSE)
    radius[slopes] <- meta$lambda_used

  } else if (name == "stepwise_ic") {
    # Stepwise IC is hard thresholding of the t statistic at sqrt(k)
    k <- if (meta$penalty == "AIC") 2 else log(length(y))
    if (nrow(X) <= ncol(X) + 1)
      stop("pad = \"", pad, "\" for stepwise selection needs the full-model ",
           "standard errors, which require n > p + 1. Use pad = \"none\".",
           call. = FALSE)
    fit <- fit_on_columns(X, y, colnames(X), meta$family)
    se <- summary(fit)$coefficients[, "Std. Error"]
    names(se) <- attr(fit, "column_names")[names(se)]
    cols <- intersect(terms[slopes], names(se))
    radius[cols] <- sqrt(k) * se[cols]
  } else {
    stop("Padding is not defined for selector \"", name, "\"; ",
         "use pad = \"none\".", call. = FALSE)
  }

  radius
}

warn_lasso_zero_side <- function() {
  warning("The lasso's bias does not vanish for large effects, so ",
          "pad = \"zero_side\" does not guarantee coverage. ",
          "Consider pad = \"all\" or select_ncvreg(penalty = \"MCP\").",
          call. = FALSE)
}

# Apply the padding rule to ci_low/ci_high using the null_radius column
pad_intervals <- function(results, pad) {
  d <- results$null_radius
  ok <- !is.na(d) & !is.na(results$ci_low) & !is.na(results$ci_high)
  lo <- results$ci_low
  hi <- results$ci_high
  if (pad == "zero_side") {
    z <- ok & lo <= 0 & hi >= 0
    lo[z] <- pmin(lo[z], -d[z])
    hi[z] <- pmax(hi[z], d[z])
  } else if (pad == "all") {
    lo[ok] <- lo[ok] - d[ok]
    hi[ok] <- hi[ok] + d[ok]
  }
  results$ci_low <- lo
  results$ci_high <- hi
  results
}
