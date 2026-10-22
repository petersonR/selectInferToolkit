#' Post-selection inference
#'
#' This function performs a `selector`'s default inference method. Alternative
#' inference methods may be available via `infer_*`, which you should use
#' instead if you want to do specific tuning.
#'
#' The default method is set by the selector:
#'
#' * `select_stepwise_ic()`, `select_ncvreg()`: [infer_boot()]
#' * `select_glmnet()`: [infer_selective()]
#' * `select_full_model()`, `select_null_model()`: [infer_upsi()]
#'
#' @param object a `selector` object
#' @param data the data used to fit the selector
#' @param nonselection how to handle variables the selector did not select;
#'   see [infer_upsi()]. Used only when the default method takes it
#'   ([infer_upsi()], [infer_selective()]). [infer_boot()] and [infer_pipe()]
#'   have no `nonselection` option, so supplying it there warns and it is
#'   ignored.
#' @param ... additional arguments passed to the default `infer_*` method
#'
#' @return an `inferrer` object; see [tidy.inferrer()]
#'
#' @importFrom broom tidy
#'
#' @export
infer <- function(object, data,
                  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
                  ...){

  if (!inherits(object, "selector"))
    stop("`object` must be a `selector`. ",
         "Did you pass an `inferrer` by mistake? Use a select_* function first.")

  method <- attr(object, "default_infer")
  # record before match.arg(), which makes missing() FALSE
  nonselection_supplied <- !missing(nonselection)
  nonselection <- match.arg(nonselection)

  if (method == "boot") {
    message("Default inference method is bootstrapping (infer_boot). ",
            "This may take a moment. Set a seed for reproducibility, ",
            "or use infer_upsi() / infer_selective() for faster alternatives.")
  }

  infer_fn <- paste0("infer_", method)
  args <- list(object = object, data = data, ...)

  if (method %in% c("upsi", "selective")) {
    args$nonselection <- nonselection
  } else if (nonselection_supplied) {
    warning("The default inference method for this selector, ", infer_fn,
            "(), has no `nonselection` option; it is ignored. ",
            "Use infer_upsi() or infer_selective() to control non-selections.",
            call. = FALSE)
  }

  do.call(infer_fn, args = args)
}
