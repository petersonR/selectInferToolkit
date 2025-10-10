#' Post-selection inference
#'
#' @param model model of S3 class returned from stepwise_ic, pen_cv, or similar
#' @param method A character string specifying method of post-selection inference.Currently "hybrid", "selectiveinf" or
#' "boot" supported
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#'
#' @return A list of class `infer_*` containing:'
#' \item{model}{A data frame with post-selection inference results}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{median CI length across all variables in model}
#' \item{nonselection}{method chosen  to deal with non selection}
#' \item{infmethod}{Inference method chosen}
#' \item{selection_method}{Stepwsie,returned for selector_stepwise_ic class only}
#' \item{direction}{the mode of step wise search, returned for selector_stepwise_ic class only}
#' \item{penalty}{penalty used (AIC or BIC), returned for selector_stepwise_ic class only}
#' \item{lambda}{selected lambda for inference , either "lambda.min" or "lambda.1se"; returned for selector_pen class only}
#' \item{alpha}{selected alpha for inference, returned for selector_pen class only}
#' \item{B}{The number of bootstrap replicates used (only for bootstrap selection method)}
#'
#' @importFrom broom tidy
#' @export
infer <- function(model, method=c("hybrid", "boot", "selectiveinf", "PIPE"),
                          nonselection=c("ignored", "uncertain_nulls", "confident_nulls"),...){

  method <- match.arg(method)
  nonselection <- match.arg(nonselection)

  UseMethod("infer")
}
