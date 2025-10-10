
# Helper: clean colnames to avoid special characters
#' Title
#'
#' @param nms a vector with column names to be cleaned
#'
#' @return a vector with clean columns
#'
#'

clean_colnames_fn <- function(nms) {
  ifelse(grepl("[^a-zA-Z0-9._]", nms), paste0("`", nms, "`"), nms)
}




# Transform factor variables to dummies if needed
#' Title
#'
#' @param data  data to be processed
#' @param make_levels whether to make dummy variables for each factor level
#'
#' @return model matrix with clean dataset

dummy_col_fn <- function(data, make_levels) {
  if (!make_levels) return(data)

  # Identify factor and numeric variables
  fact_vars <- names(data)[sapply(data, is.factor)]
  if (length(fact_vars) == 0) return(data)

  dummy_data <- model.matrix(~ . - 1, data = data[, fact_vars, drop = FALSE])[,-1]
  numeric_data <- data[, !names(data) %in% fact_vars, drop = FALSE]

  cbind(numeric_data, dummy_data)
}






#' Stepwise forwad/backward/bidirectional selection with AIC/BIC
#'
#'
#' @description This function implements forward/backward/bidirectional stepwise regression,
#' for use in the selectInferToolkit package
#'
#'
#' @param x Dataframe/model matrix with predictors (without intercept)
#' @param y outcome vector
#' @param family currently gaussian supported
#' @param std if TRUE (default), standardize design matrix
#' @param penalty AIC or BIC
#' @param direction the mode of step wise search, can be one of "both", "backward", or "forward", with a default of "forward"
#' @param make_levels  whether to model selection after dummy coding for categorical variables (defult FALSE)
#' @param ... Additional arguments that can be passed with stepAIC function in MASS package
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if select
#' @importFrom broom tidy
#' @importFrom stats lm  model.frame model.matrix na.pass
#' @importFrom MASS stepAIC
#' @return A list of class `selector_ic` containing:#'
#' \item{beta}{a tibble containing term names and coefficients}
#' \item{std}{Was desing matrix standadrized}
#' \item{penalty}{penalty used (AIC or BIC)}
#' \item{direction}{the mode of step wise search}
#' \item{x}{the model dataframe used}
#' \item{y}{repsonse used in vector}
#' \item{model_sum}{the stepwise-selected model  details is returned}
#' @export

step_ic <- function(x, y, family = "gaussian", std = FALSE,
                    penalty = "AIC", direction = "forward",
                    make_levels = FALSE, ...) {
  if (is.matrix(x)) {
    if (std == TRUE) {
      x_df <- as.data.frame(x, check.names = FALSE)
      numeric_cols <- sapply(x_df, is.numeric)
      x_df[numeric_cols] <- lapply(x_df[numeric_cols], scale)
      colnames(x_df) <- colnames(x)
      x_std <- x_df
    } else {
      x_std <- as.data.frame(x, check.names = FALSE)
    }
  } else if (is.data.frame(x)) {
    if (std == TRUE) {
      numeric_cols <- sapply(x, is.numeric)
      x[numeric_cols] <- lapply(x[numeric_cols], scale)
      colnames(x) <- colnames(x) # optional, redundant unless colnames were altered
      x_std <- x
    } else {
      x_std <- x
    }
  }

  raw_data <- as.data.frame(cbind(x_std, y))
  rownames(raw_data) <- NULL

  # fit full model to get term names
  full_model <- lm(y ~ ., data = raw_data)
  coef_est <- coef(full_model)
  full_model_df <- data.frame(term = names(coef_est), estimate = as.numeric(coef_est))
  # Set penalty value
  k_val <- if (penalty == "AIC") 2 else if (penalty == "BIC") log(nrow(raw_data)) else stop("Unsupported penalty")

  # Prepare x_input and raw_data_fs if needed
  x_input <- if (make_levels) dummy_col_fn(x_std, TRUE) else x_std
  use_scope <- make_levels || direction == "forward"

  if (use_scope) {
    clean_colnames <- clean_colnames_fn(colnames(x_input))
    raw_data_fs <- as.data.frame(cbind(x_input, y))
    scope_formula <- as.formula(paste("~", paste(clean_colnames, collapse = " + ")))
    model <- stepAIC(
      lm(y ~ 1, data = raw_data_fs),
      scope = list(lower = ~1, upper = scope_formula),
      direction = direction,
      k = k_val,
      trace = 0,
      ...
    )
  } else {
    model <- stepAIC(
      lm(y ~ ., data = raw_data),
      direction = direction,
      trace = 0,
      k = k_val,
      ...
    )
  }

  # Extract model coefficients
  coefs <- coef(model)
  mod_df <- data.frame(term = names(coefs), estimate = as.numeric(coefs), stringsAsFactors = FALSE)
  mod_df$term <- gsub("`", "", mod_df$term)

  # Create final coefficient table
  all_terms <- if (make_levels) colnames(raw_data_fs)[colnames(raw_data_fs) != "y"] else full_model_df$term
  all_terms <- full_model_df$term
  coef_full <- merge(
    data.frame(term = all_terms, stringsAsFactors = FALSE),
    mod_df,
    by = "term",
    all.x = TRUE,
    all.y = T,
    sort = F
  )
  coef_full <- coef_full[match(all_terms, coef_full$term), ]

  # Assemble final output
  data <- model[["model"]]
  val <- list(
    beta = coef_full,
    std = std,
    penalty = penalty,
    direction = direction,
    x_original = x,
    y = data[[1]],
    x_model = raw_data[, setdiff(names(raw_data), "y"), drop = FALSE],
    model_sum = model,
    make_levels = make_levels,
    family = family
  )
  class(val) <- "selector_ic"
  val


}

#' Title
#'
#' @param x model of class `selector_ic`
#' @param ... additional arguments (currently not supported)
#' @method print `selector_ic`
#' @return returns x invisibly
#' @export
print.selector_ic <- function(x, ...) {
  cat("Stepwise Model Selection Summary:\n")

  # Model direction
  cat("Direction of Selection: ", x$direction, "\n")

  # Penalty used for selection
  cat("Penalty used: ", x$penalty, "\n")

  # Standard errors used in the model
  if (x$std) {
    cat("Design Matrix standardized: TRUE\n")
  } else {
    cat("Design Matrix standardized: FALSE\n")
  }

  # Model coefficients (first few)
  cat("\nFinal Model Coefficients:\n")
  print(summary(x[["model_sum"]])[["coefficients"]][, 1])
}


#' Title
#'
#' @param x model of class `selector_ic`
#' @param ... Additional arguments passed to the generic `tidy` function.
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @return  A tibble containing the tidied coefficients of the model.
#' @export
tidy.selector_ic <- function(x, ...) {
  ret <- as_tibble(summary(x[["model_sum"]])$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  ret <- ret %>% select(term, estimate)
  return(ret)
}
