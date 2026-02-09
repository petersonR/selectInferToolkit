#' Inference for selector_pen_cv class except bootstrap which is it's own function
#'
#' @param object model of selector_pen_cv class returned from  selector_pen_cv function
#' @param data data must be passed to infer
#' @param conf.level .95 by default
#' @param ... arguments passed to `ncvreg::intervals()` function
#' @return an `inferror` object
#'
#' @rdname infer
#' @importFrom tibble tibble
#' @importFrom ncvreg intervals
#' @export
#'
infer_pipe <- function(object, data, conf.level = .95, ...) {

  supported <- c("ncvreg")
  type <- attr(object, "name")
  if(!(type %in% supported))
    stop("Currently SI only supported for `ncvreg`-based selectors")

  X <- bake(attr(object, "recipe_obj"), new_data = data, all_predictors())
  beta <- coef(object)
  meta <- attr(object, "meta")

  pipe_results <- intervals(object, level = conf.level, X = as.matrix(X), ...)
  inferences <- pipe_results %>%
    select(term = variable, estimate, ci_low = lower, ci_high = upper, p_value = p.value) %>%
    tibble()


  as_inferrer(
    pipe_results, "pipe", label = "PIPE",
    nonselection = "uncertain",
    conf.level = conf.level,
    selector = object,
    meta = list(...),
    inferences = inferences)
}
