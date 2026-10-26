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
#'     unchanged. `[-delta, delta]` is a region of "reasonably confident null",
#'     which tends to shrink as `n` grows.
#'   * `"none"`: the raw percentile interval ("confident nulls").
#'   * `"all"`: every interval is widened by `delta` on both sides.
#'
#'   The radius `delta` is set from the selector (scaled metric) and returned
#'   in the `null_radius` column of `tidy()`. The column is left out when no
#'   interval could be padded (`pad = "none"`, or a radius of 0):
#'
#'   * `select_ncvreg()`: `gamma * lambda_eff` for MCP/SCAD with
#'     `"zero_side"`, `lambda_eff` with `"all"` (see `select_ncvreg()`
#'     resampling below).
#'   * `select_glmnet()`: `lambda`; lasso `select_ncvreg()`: `lambda_eff`.
#'     The lasso's
#'     bias does not vanish for large effects, so `"zero_side"` warns.
#'     `"all"`, which also widens intervals that exclude 0, is the
#'     alternative to consider.
#'   * `select_stepwise_ic()`: `sqrt(k)` times the full-model standard error,
#'     with `k = 2` (AIC) or `log(n)` (BIC). Requires `n > p + 1`.
#'   * `select_full_model()` / `select_null_model()`: 0, so no padding.
#'
#'   The radii are motivated by the gaussian family in an orthonormal design;
#'   other families warn.
#'
#'   For `select_ncvreg()`, every resample is refit at `lambda_eff`, the
#'   smallest penalty giving the selected model (see [select_ncvreg()]),
#'   rather than at the cross-validated penalty, which can sit near the
#'   smallest selected effect rather than the noise level (see GitHub issue
#'   #34). So `estimate` can differ from `coef` for a coefficient in MCP's
#'   shrinkage zone. `lambda_eff` is reported in the inferrer's meta.
#'
#'   A resample whose path ends before `lambda_eff` (a binomial fit that
#'   saturates, e.g. under separation, or one that reaches a `dfmax` passed to
#'   [select_ncvreg()]) is fit at the smallest penalty it reached, which
#'   shrinks it more; their number is reported in a warning and as
#'   `n_short_path` in the meta.
#'
#'   Warnings from fitting the resamples (e.g. from `glm()` or `ncvreg()`) are
#'   collected and given once each, with the number of resamples that raised
#'   them, with or without a cluster. `lambda_eff` is motivated by MCP and SCAD; with the lasso it is
#'   used too, with a warning when it is below the cross-validated penalty.
#' @param conf.level .95 by default
#' @param type the bootstrap type. Only `"paired"` (resampling rows) is
#'   implemented; `"residual"` is reserved and currently an error.
#' @param B number of bootstrap resamples
#' @param cluster `NULL` (default) to fit the resamples sequentially, or a
#'   cluster from [parallel::makeCluster()] to fit them in parallel. Setting
#'   up and stopping the cluster is left to you, so one cluster can serve
#'   several calls. A seed for each resample is drawn before any fitting, so
#'   for a given seed the results are the same with or without a cluster.
#'   Each worker
#'   must have selectInferToolkit installed (a fork cluster from
#'   [parallel::makeForkCluster()] shares the calling session's instead).
#'   The progress bar is shown either way; see [pbapply::pboptions()] to turn
#'   it off.
#' @param ... not used. Any argument passed here is an error, so that a
#'   misspelled or removed argument (such as `debias`) can't be silently
#'   ignored.
#' @return an `inferrer` object; see [tidy.inferrer()]. Its inferences include
#'   `prop_selected` (the proportion of resamples in which each term was
#'   selected) and, when intervals are padded, `null_radius` (the padding
#'   radius; see `pad`).
#' @examples
#' set.seed(1)
#' sel <- select_ncvreg(mpg ~ ., data = mtcars)
#' set.seed(2)
#' inf <- infer_boot(sel, data = mtcars, B = 50)
#' tidy(inf)
#'
#' \donttest{
#' # In parallel: make the cluster once, reuse it, and stop it when done
#' cl <- parallel::makeCluster(2)
#' set.seed(2)
#' inf_par <- infer_boot(sel, data = mtcars, B = 50, cluster = cl)
#' parallel::stopCluster(cl)
#' all.equal(tidy(inf), tidy(inf_par)) # same resamples, same results
#' }
#' @export

infer_boot <- function(
  object,
  data,
  inference_target = c("selections", "all"),
  pad = c("zero_side", "none", "all"),
  conf.level = .95,
  type = c("paired", "residual"),
  B = 250,
  cluster = NULL,
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
    if ("n_cores" %in% nms)
      stop("`n_cores` was removed from infer_boot(); pass a cluster from ",
           "parallel::makeCluster() as `cluster` instead.", call. = FALSE)
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
  if (!is.null(cluster) && !inherits(cluster, "cluster"))
    stop("`cluster` must be NULL or a cluster from parallel::makeCluster().",
         call. = FALSE)

  boot(object, data = data, B = B, inference_target = inference_target,
       pad = pad, conf.level = conf.level, cluster = cluster)
}


# Internal worker behind infer_boot(): resample, reselect, and summarize.
# Not exported, so it doesn't mask boot::boot().
#
#' @importFrom tibble tibble
#' @importFrom stats coef quantile
#' @noRd
boot <- function(object, data, B,
                 inference_target = c("selections", "all"),
                 pad = c("zero_side", "none", "all"),
                 conf.level, cluster = NULL) {
  stopifnot(inherits(object, "selector"))
  pad <- match.arg(pad)

  # The design is applied once; each resample takes rows of X_full and y_full
  # (see design_rows())
  # ncvreg resamples are refit at lambda_eff, not at the CV choice (#34)
  resampler <- ncvreg_at_lambda_eff(object)

  design <- attr(object, "design")
  X_full <- design_X(design, data)
  y_full <- design_y(design, data)
  terms <- design_terms(design)

  # Draw a seed for each resample before fitting any, so the results don't
  # depend on whether or how the replicates are spread over a cluster, even
  # for fits that draw random numbers themselves. Each replicate draws its rows
  # from its seed, so only B integers are held up front, not B x n
  seeds <- sample.int(.Machine$integer.max, B)
  rng_kind <- RNGkind()

  message("Bootstrapping with B = ", B, " replicates...")
  if (is.null(cluster)) {
    # The replicates reseed this session's RNG; restore the stream as it was
    # after drawing the seeds
    rng_state <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", rng_state, envir = globalenv()), add = TRUE)
    boot_fits <- pbapply::pblapply(seeds, boot_replicate, selector = resampler,
                                   X_full = X_full, y_full = y_full,
                                   terms = terms, rng_kind = rng_kind)
  } else {
    # Send the selector and data to each worker once, not with every batch
    parallel::clusterCall(cluster, boot_worker_set, resampler, X_full,
                          y_full, terms, rng_kind)
    on.exit(parallel::clusterCall(cluster, boot_worker_clear), add = TRUE)
    boot_fits <- pbapply::pblapply(seeds, boot_worker_replicate, cl = cluster)
  }

  # B x terms matrix of resample coefficients (scaled metric), 0 when not
  # selected
  coefs <- matrix(unlist(lapply(boot_fits, `[[`, "coef")), nrow = B,
                  byrow = TRUE, dimnames = list(NULL, terms))
  n_short_path <- sum(vapply(boot_fits, `[[`, logical(1), "short_path"))
  if (n_short_path > 0)
    warning(n_short_path, " of ", B, " resamples' paths ended before ",
            "lambda = ", signif(attr(resampler, "meta")$lambda_used, 3),
            " (the fit saturated or reached dfmax); each of those is fit at ",
            "the smallest lambda it reached, which shrinks it more.",
            call. = FALSE)
  # Warnings from the fits, once each with their count (workers don't relay
  # warnings, so they are collected the same way in both modes)
  fit_warnings <- table(unlist(lapply(boot_fits, `[[`, "warnings")))
  for (w in names(fit_warnings))
    warning("In ", fit_warnings[[w]], " of ", B, " resamples: ", w,
            call. = FALSE)

  probs <- c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2)
  q <- apply(coefs, 2, quantile, probs, names = FALSE)
  results <- tibble(
    term = terms,
    estimate = unname(colMeans(coefs)),
    ci_low = q[1, ],
    ci_high = q[2, ],
    prop_selected = unname(colMeans(coefs != 0))
  )
  if (inference_target == "selections") {
    nonselected <- !terms %in% names(coef(object))
    results[nonselected, c("estimate", "ci_low", "ci_high",
                           "prop_selected")] <- NA
  }

  # Pad the percentile intervals (scaled metric; the intercept is never padded)
  # (the resampler's penalty, so ncvreg pads with lambda_eff)
  radius <- pad_radius(resampler, pad, X_full, y_full, results$term)
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
  if (attr(object, "name") == "ncvreg") {
    meta_information$lambda_eff <- attr(resampler, "meta")$lambda_used
    meta_information$n_short_path <- n_short_path
  }

  # One row per resample and term: the resample's estimate, 0 when not
  # selected
  boot_df <- tibble(
    bootstrap = rep(as.character(seq_len(B)), each = length(terms)),
    term = rep(terms, B),
    estimate = as.vector(t(coefs))
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

# The selector the resamples replay: for ncvreg, the same selector with its
# penalty moved to lambda_eff; any other selector as is
ncvreg_at_lambda_eff <- function(object) {
  if (attr(object, "name") != "ncvreg") return(object)
  meta <- attr(object, "meta")
  # selectors without a lambda_eff (from reselect(), or fit before it was
  # stored) keep their penalty
  lambda_eff <- meta$lambda_eff %||% meta$lambda_used
  if (meta$penalty == "lasso" && lambda_eff < meta$lambda_used)
    warning("Resamples are refit at lambda_eff, the smallest lambda giving ",
            "the selected model. This is motivated by MCP and SCAD and is ",
            "untested for the lasso.", call. = FALSE)
  meta$lambda_used <- lambda_eff
  attr(object, "meta") <- meta
  object
}

# One bootstrap replicate: from `seed`, draw the rows and reselect on them,
# and return the coefficient of every term (0 when not selected), whether an
# ncvreg path ended before its penalty, and any other warnings (all reported
# once by boot()). `rng_kind` is the calling session's RNGkind(), so workers
# draw with the same generator
boot_replicate <- function(seed, selector, X_full, y_full, terms, rng_kind) {
  set.seed(seed, kind = rng_kind[1], normal.kind = rng_kind[2],
           sample.kind = rng_kind[3])
  n <- length(y_full)
  idx <- sample(1:n, replace = TRUE)
  rows <- design_rows(X_full[idx, , drop = FALSE], y_full[idx])
  short_path <- FALSE
  warnings <- character()
  sel <- withCallingHandlers(
    reselect(selector, newdata = rows),
    warning = function(w) {
      if (inherits(w, "ncvreg_short_path")) short_path <<- TRUE else
        warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  cf <- unname(coef(sel)[terms])
  cf[is.na(cf)] <- 0
  list(coef = cf, short_path = short_path, warnings = unique(warnings))
}

# Each cluster worker keeps the selector and data here between batches
.boot_worker <- new.env(parent = emptyenv())

boot_worker_set <- function(selector, X_full, y_full, terms, rng_kind) {
  .boot_worker$selector <- selector
  .boot_worker$X_full <- X_full
  .boot_worker$y_full <- y_full
  .boot_worker$terms <- terms
  .boot_worker$rng_kind <- rng_kind
  invisible(NULL)
}

boot_worker_clear <- function() {
  rm(list = ls(.boot_worker), envir = .boot_worker)
  invisible(NULL)
}

boot_worker_replicate <- function(seed) {
  boot_replicate(seed, .boot_worker$selector, .boot_worker$X_full,
                 .boot_worker$y_full, .boot_worker$terms,
                 .boot_worker$rng_kind)
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
