# The design: how a formula and data become the standardized numeric matrix
# every selector fits on.
#
# Built once, on the data a selector is first fit to, and stored on the
# selector. Everything after that (reselect(), predict(), tidy(), the infer_*
# methods) reuses it, so the columns, their names, and their centers and
# scales never change with new data. Columns are named as model.matrix()
# names them (`x1`, `g3mid`, `am1`, `fTRUE`, `x1:x2`); those names are only
# ever used as labels, never parsed back into formulas.
#
# The term map (`design$map`) has one row per model-matrix column:
#   column  the column's name
#   term    the formula term it belongs to (a factor's dummies share one)
#   factor  whether that term involves a factor (or logical/character)
#   center  value subtracted from the column
#   scale   value the centered column is divided by (its SD)
#   kept    FALSE for zero-variance columns, which are left out of the matrix

# @param center_factors if FALSE, columns of terms involving a factor are left
#   uncentered, so that the intercept is the prediction at each factor's
#   reference level (and the means of the numeric predictors). They are still
#   divided by their SD.
#' @importFrom stats model.frame model.matrix model.response terms
#' @importFrom stats delete.response .getXlevels sd
design_build <- function(formula, data, center_factors = TRUE) {
  if (!inherits(formula, "formula"))
    stop("`formula` must be a formula.", call. = FALSE)

  tt <- terms(formula, data = data)
  if (attr(tt, "response") == 0)
    stop("`formula` must have an outcome on the left-hand side.",
         call. = FALSE)
  if (attr(tt, "intercept") == 0)
    stop("Models without an intercept are not supported.", call. = FALSE)

  mf <- design_frame(tt, data, xlev = NULL)
  # the frame's terms carry `predvars`, so that data-dependent terms such as
  # poly() or splines::ns() are evaluated on new data with the training basis
  tt <- attr(mf, "terms")
  check_predictor_types(mf, tt)
  check_complete(mf)

  # Center numeric predictors before model.matrix() forms interactions, so
  # main effects in a model with x:z are effects at the mean of the other
  # variable, not at 0
  vars <- design_numeric_vars(mf, tt)
  var_center <- vapply(mf[vars], mean, numeric(1))

  tt_x <- delete.response(tt)
  xlev <- .getXlevels(tt_x, mf)
  mm <- design_raw_matrix(tt_x, mf, var_center)

  assign <- attr(mm, "assign")
  term_labels <- attr(tt_x, "term.labels")
  keep_cols <- assign != 0
  mm <- mm[, keep_cols, drop = FALSE]
  if (!ncol(mm))
    stop("`formula` has no predictors.", call. = FALSE)
  if (anyDuplicated(colnames(mm)))
    stop("Two design columns share the name `",
         colnames(mm)[anyDuplicated(colnames(mm))], "`; rename the ",
         "variables or factor levels involved.", call. = FALSE)

  term <- term_labels[assign[keep_cols]]
  factor_vars <- names(mf)[vapply(mf, is_factor_like, logical(1))]
  fac <- attr(tt_x, "factors")
  term_has_factor <- vapply(term_labels, function(lab)
    any(rownames(fac)[fac[, lab] > 0] %in% factor_vars), logical(1))
  is_fac <- unname(term_has_factor[term])

  center <- colMeans(mm)
  if (!center_factors) center[is_fac] <- 0
  scale <- apply(mm, 2, sd)
  kept <- is.finite(scale) & scale > 0

  structure(list(
    terms = tt,
    outcome = deparse(tt[[2L]]),
    xlev = xlev,
    var_center = var_center,
    map = tibble::tibble(
      column = colnames(mm),
      term = term,
      factor = is_fac,
      center = unname(center),
      scale = unname(scale),
      kept = unname(kept)
    )
  ), class = "selector_design")
}

# The standardized predictor matrix for `data`, kept columns only.
# allow_na: predict() may pass rows with missing values (giving NA
# predictions); fitting never does.
design_X <- function(design, data, allow_na = FALSE) {
  tt_x <- delete.response(design$terms)
  mf <- design_frame(tt_x, data, xlev = design$xlev)
  if (!allow_na) check_complete(mf)
  mm <- design_raw_matrix(tt_x, mf, design$var_center)
  map <- design$map[design$map$kept, ]
  mm <- mm[, map$column, drop = FALSE]
  X <- sweep(sweep(mm, 2, map$center), 2, map$scale, "/")
  attr(X, "assign") <- NULL
  attr(X, "contrasts") <- NULL
  rownames(X) <- NULL
  X
}

# The outcome, as given (factor outcomes stay factors)
design_y <- function(design, data) {
  mf <- design_frame(design$terms, data, xlev = design$xlev)
  y <- model.response(mf)
  if (anyNA(y))
    stop("The outcome has missing values; remove those rows first.",
         call. = FALSE)
  unname(y)
}

# Names of the design's columns, kept or not, with the intercept first: the
# rows tidy() reports
design_terms <- function(design) c("(Intercept)", design$map$column)

# Each kept column's SD, named by column: the factor between the scaled and
# per-unit metrics
design_sds <- function(design) {
  map <- design$map[design$map$kept, ]
  stats::setNames(map$scale, map$column)
}

design_frame <- function(tt, data, xlev) {
  data <- as.data.frame(data)
  # Logicals as two-level factors, so their column is always `xTRUE`, even in
  # new data where only one value occurs
  lgl <- vapply(data, is.logical, logical(1))
  if (attr(tt, "response") == 1)
    lgl[names(data) %in% all.vars(tt[[2L]])] <- FALSE
  data[lgl] <- lapply(data[lgl], factor, levels = c(FALSE, TRUE))
  tryCatch(
    model.frame(tt, data, xlev = xlev, na.action = stats::na.pass,
                drop.unused.levels = FALSE),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("has new levels?", msg))
        stop(sub("^factor ", "Factor ", msg),
             ": a selector can only be applied to data whose factor levels ",
             "appeared when it was fit.", call. = FALSE)
      stop(e)
    })
}

design_raw_matrix <- function(tt_x, mf, var_center) {
  for (v in names(var_center))
    mf[[v]] <- mf[[v]] - var_center[[v]]
  model.matrix(tt_x, mf)
}

# Plain numeric predictor variables (not factors, not matrix-valued like
# poly()), which are centered before interactions are formed
design_numeric_vars <- function(mf, tt) {
  vars <- setdiff(names(mf), deparse(tt[[2L]]))
  vars[vapply(mf[vars], function(v) is.numeric(v) && is.null(dim(v)),
              logical(1))]
}

is_factor_like <- function(x)
  is.factor(x) || is.character(x) || is.logical(x)

check_predictor_types <- function(mf, tt) {
  vars <- setdiff(names(mf), deparse(tt[[2L]]))
  ord <- vars[vapply(mf[vars], is.ordered, logical(1))]
  if (length(ord))
    stop("Ordered factor predictor(s): ", paste0("`", ord, "`", collapse = ", "),
         ". Convert them first, to numeric scores (as.numeric()) or to ",
         "unordered levels (factor(x, ordered = FALSE)).", call. = FALSE)
}

check_complete <- function(mf) {
  na_vars <- names(mf)[vapply(mf, anyNA, logical(1))]
  if (length(na_vars))
    stop("Missing values in ", paste0("`", na_vars, "`", collapse = ", "),
         ". Impute them or remove those rows (e.g. with na.omit()) first.",
         call. = FALSE)
}
