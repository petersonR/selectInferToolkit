#' PIPE inference for `ncvreg` selectors (experimental)
#'
#' Pointwise inference for penalized estimators (PIPE), via
#' [ncvreg::intervals()]. Gives an estimate, interval and p-value for every
#' candidate term, selected or not, so there is no `nonselection` option.
#' Currently only available for [select_ncvreg()] selectors.
#'
#' @param object a `selector` from [select_ncvreg()]
#' @param data the data used to fit the selector
#' @param conf.level .95 by default
#' @param ... arguments passed to [ncvreg::intervals()]
#' @return an `inferrer` object; see [tidy.inferrer()]
#'
#' @importFrom tibble tibble
#' @importFrom ncvreg intervals
#' @importFrom rlang .data
#' @export
#'
infer_pipe <- function(object, data, conf.level = .95, ...) {

  if (!inherits(object, "selector"))
    stop("`object` must be a `selector`. ",
         "Did you pass an `inferrer` by mistake? Use a select_* function first.")

  supported <- c("ncvreg")
  type <- attr(object, "name")
  if(!(type %in% supported))
    stop("PIPE inference is currently only supported for `ncvreg`-based selectors")

  X <- design_X(attr(object, "design"), data)
  beta <- coef(object)
  meta <- attr(object, "meta")

  pipe_results <- intervals(object, level = conf.level, X = X, ...)
  inferences <- pipe_results %>%
    select(term = "variable", estimate = "estimate", ci_low = "lower",
           ci_high = "upper", p_value = "p.value") %>%
    tibble()


  as_inferrer(
    pipe_results, "pipe", label = "PIPE",
    nonselection = "uncertain_nulls",
    conf.level = conf.level,
    selector = object,
    meta = list(...),
    inferences = inferences)
}
