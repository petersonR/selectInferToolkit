

#' Inference for selector.ic class except bootstrap which is it's own function
#'
#'
#' @param model model returned from selector_ic  class
#' @param method A character string specifying method of post-selection inference.Currently "hybrid"or  "selectiveinf"
#' @param nonselection  A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @return infer_ic class list with
#' \item{model}{A dataframe with model results including columns for term, estimate, std.error, statistics, p.value, conf.low, conf.high, ci_ln}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{medain CI length across all variables in model}
#' \item{selection_method}{Stepwsie}
#' \item{direction}{the mode of step wise search}
#' \item{penalty}{penalty used (AIC or BIC)}
#' \item{infmethod}{Inference method chosen}
#' \item{nonselection}{method chosen  to deal with non selection}
#'
#'

infer.selector.ic <- function(
    model,
    method = c("hybrid", "selectiveinf", "boot"),
    nonselection = c("ignored", "confident_nulls", "uncertain_nulls")
  ) {

  method <- match.arg(method)
  nonselection <- match.arg(nonselection)

  if(method == "selectiveinf" & model$direction != "forward") {
    warning("Direction must be set to 'forward' for selectiveInference")
  }

  if (method == "hybrid" && nonselection == "ignored") {
    mod<- tidy(model[["model_sum"]], conf.int=T)
    mod$ci_ln <- mod$conf.high - mod$conf.low

    full_mod <-data.frame(term=model[["beta"]][["term"]]) %>%
      dplyr::left_join(mod, by = "term")

    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=  full_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method="Stepwise",direction = model[["direction"]],penalty= model[["penalty"]],
                   infmethod = method, nonselection = nonselection
                   )
    class(result) <- "infer_ic"
    result
  }
   else if (method == "hybrid" && nonselection == "confident_nulls") {
     mod<- tidy(model[["model_sum"]], conf.int=T)

        full_mod= data.frame(term=model[["beta"]][["term"]]) %>%
          dplyr::select(term) %>%
          dplyr::left_join(mod, by = "term") %>%
          dplyr::mutate(  estimate = ifelse(is.na(estimate), 0,  estimate),
                   p.value = ifelse(is.na(p.value), 1, p.value),
                   std.error= ifelse(is.na(std.error), 0, std.error),
                   conf.low = ifelse(is.na(conf.low), 0, conf.low),
                   conf.high = ifelse(is.na(conf.high), 0, conf.high),
          ) %>% dplyr::select(term, estimate, p.value, std.error,conf.low, conf.high)
        full_mod$ci_ln <- full_mod$conf.high - full_mod$conf.low

      ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
      ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)


      result <- list(model=  full_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                     selection_method="Stepwise",direction = model[["direction"]],penalty= model[["penalty"]],
                     infmethod = method, nonselection = nonselection)
      class(result) <- "infer_ic"
      result
   }
  else if (method == "hybrid" && nonselection == "uncertain_nulls") {
    mod<- tidy(model[["model_sum"]], conf.int=T)
    res =residuals(model[["model_sum"]])
    x <-model[["x_model"]]
    x_dup<- as.data.frame(model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))[,-1],
                          check.names=FALSE)
    #mod$term[mod$term=="(Intercept)"] <-"intercept"
    full_mod <-data.frame(term=model[["beta"]][["term"]]) %>%
      dplyr::left_join(mod, by = "term")
    final_mod= get_uncertain_nulls (mod=full_mod, res=res, x=x_dup)

    ci_avg_ratio  <- mean(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)


    result <- list(model=   final_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method="Stepwise",direction = model[["direction"]],penalty= model[["penalty"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_ic"
    result


  }
  else if (method == "selectiveinf" && nonselection == "ignored") {

    x <-data.frame(model[["x_original"]], check.names = FALSE)
    x <- droplevels(x)
    x_mat= model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))[,-1]
    y <- model[["y"]]
    std=model[["std"]]

    if (model[["penalty"]]=="AIC"){
      fs_si_aic = sel_inf_fs(x_mat, y, std=std)
    }else{
      fs_si_aic = sel_inf_fs(x_mat, y,mult= log(length(y)),std=std)
    }

    full_mod <-data.frame(term=model[["beta"]][["term"]]) %>%
      dplyr::left_join(fs_si_aic , by = "term")  %>%
      mutate(ci_ln=conf.high-conf.low)

    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=  full_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method="Stepwise",direction = model[["direction"]],penalty= model[["penalty"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_ic"
    return(result)
  }
  else if (method == "selectiveinf" && nonselection == "confident_nulls") {

    x <-data.frame(model[["x_original"]], check.names = FALSE)
    x <- droplevels(x)
    x_mat= model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))[,-1]
    y <- model[["y"]]
    std=model[["std"]]


    if (model[["penalty"]]=="AIC"){
      fs_si_aic = sel_inf_fs(x_mat, y,std=std)
    }else{
      fs_si_aic = sel_inf_fs(x_mat, y,mult= log(length(y)),std=std)
    }
    full_mod <-data.frame(term=model[["beta"]][["term"]]) %>%
      dplyr::left_join(fs_si_aic , by = "term")%>%
      mutate(  estimate = ifelse(is.na(estimate), 0,  estimate),
               p.value = ifelse(is.na(p.value), 1, p.value),
               conf.low = ifelse(is.na(conf.low), 0, conf.low),
               conf.high = ifelse(is.na(conf.high), 0, conf.high),
      ) %>% select(term, estimate, p.value, conf.low, conf.high) %>%
      mutate(ci_ln=conf.high-conf.low)

    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=  full_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method="Stepwise",direction = model[["direction"]],penalty= model[["penalty"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_ic"
    result


  }
  else if (method == "selectiveinf" && nonselection == "uncertain_nulls") {

    x <-data.frame(model[["x_original"]], check.names = FALSE)
    x <- droplevels(x)
    x_mat= model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))[,-1]
    y <- model[["y"]]
    std=model[["std"]]

    if (model[["penalty"]]=="AIC"){
      fs_si = sel_inf_fs( x_mat, y,std=std)

      # get the residuals
      # Extract relevant terms and coefficients
      terms <- fs_si$term[fs_si$term != "(Intercept)"]
      beta <- setNames(fs_si$estimate, fs_si$term)

      # Subset the design matrix and align coefficients
      x_subset <- x_mat[, terms, drop = FALSE]
      beta_vector <- beta[colnames(x_subset)]
      beta_vector[is.na(beta_vector)] <- 0

      # Compute XB
      xb <- x_subset %*% beta_vector + ifelse("(Intercept)" %in% names(beta), beta["(Intercept)"], 0)
      res = y-as.numeric(xb)


    }else{
      fs_si = sel_inf_fs( x_mat, y,mult= log(length(y)),std=std)
      # get the residuals
      # Extract relevant terms and coefficients
      terms <- fs_si$term[fs_si$term != "(Intercept)"]
      beta <- setNames(fs_si$estimate, fs_si$term)

      # Subset the design matrix and align coefficients
      x_subset <- x_mat[, terms, drop = FALSE]
      beta_vector <- beta[colnames(x_subset)]
      beta_vector[is.na(beta_vector)] <- 0

      # Compute XB
      xb <- x_subset %*% beta_vector + ifelse("(Intercept)" %in% names(beta), beta["(Intercept)"], 0)
      res = y-as.numeric(xb)

    }

    full_mod <-data.frame(term=model[["beta"]][["term"]]) %>%
      dplyr::left_join(fs_si , by = "term") %>%
      mutate(std.error=NA, statistic=NA) %>%
      dplyr::relocate(term, estimate,std.error, statistic, p.value, conf.low, conf.high)

    x_dup<- as.data.frame( x_mat, check.names=FALSE)

    final_mod =get_uncertain_nulls (mod=full_mod, res=res, x=  x_dup) %>% select(-std.error,-statistic)

    ci_avg_ratio  <- mean(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(full_mod$ci_ln[full_mod$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  final_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio  ,
                   selection_method="Stepwise",direction = model[["direction"]],penalty= model[["penalty"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_ic"
    result



  }
}




#' Inference for selector.pen class except bootstrap which is it's own function
#'
#' @param model model of selector_pen class returned from  pen_cv function
#' @param method A character string specifying method of post-selection inference.Currently "hybrid", "selectiveinf" or
#' "boot" supported
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @return "infer_pen" class list with
#' \item{model}{A dataframe with model results including columns for term, estimate, std.error, statistics, p.value, conf.low, conf.high, ci_ln}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{median CI length across all variables in model}
#' \item{selection_method}{'lasso' or 'MCP' Note: Same as in ncvreg, even with elastic net-it says lasso but can be differntiated with value of alpha}
#' \item{lambda}{selected lambda for inference , either "lambda.min" or "lambda.1se"}
#' \item{alpha}{selected alpha for inference}
#' \item{infmethod}{Inference method chosen}
#' \item{nonselection}{method chosen  to deal with non selection}
#'

infer.selector.pen <- function(model, method = "hybrid", nonselection = "ignored"){
  x <-model[["x"]]
  y <- model[["y"]]

  if (method == "hybrid" && nonselection == "ignored") {
    non_zero_terms  <- model[["beta"]]$term[model[["beta"]]$estimate != 0]
    non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
    selected_data <-  data.frame(y = model[["y"]], data.frame(model[["x"]], check.names = FALSE),check.names = FALSE) %>%
                      select(y,all_of(non_zero_terms))

    fit_lso <- broom::tidy(lm(y ~ ., data = selected_data), conf.int = TRUE) %>%mutate(
      term = gsub("`", "", term),  # Remove backticks
      ci_ln = conf.high - conf.low  # Calculate confidence interval length
    )

    lso_ignore_mod <- data.frame(term=model[["beta"]][["term"]], check.names = FALSE) %>%
      select(term) %>% dplyr::left_join(fit_lso, by = "term")

    ci_avg_ratio  <- mean(lso_ignore_mod$ci_ln[lso_ignore_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(lso_ignore_mod$ci_ln[lso_ignore_mod$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  lso_ignore_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection
                   )
    class(result) <- "infer_pen"
    result


  }
  else if (method == "hybrid" && nonselection == "confident_nulls") {
    non_zero_terms  <- model[["beta"]]$term[model[["beta"]]$estimate != 0]
    non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]

    selected_data <-  data.frame(y = model[["y"]], data.frame(model[["x"]], check.names = FALSE),
                                 check.names = FALSE) %>%
      select(y,all_of(non_zero_terms))

    fit_lso <- broom::tidy(lm(y ~ ., data = selected_data), conf.int = TRUE) %>%mutate(
      term = gsub("`", "", term),  # Remove backticks
    )


    lso_mod <- data.frame(term=model[["beta"]][["term"]])   %>% select(term) %>% dplyr::left_join(fit_lso, by = "term") %>%
      mutate(estimate = ifelse(is.na(estimate),0,estimate),
             p.value = ifelse(is.na(p.value), 1, p.value),
             conf.low = ifelse(is.na(conf.low), 0, conf.low),
             conf.high = ifelse(is.na(conf.high), 0, conf.high),
      ) %>% select(term, estimate, p.value, conf.low, conf.high)%>%
      mutate(ci_ln = conf.high- conf.low)

    ci_avg_ratio  <- mean(lso_mod$ci_ln[lso_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(lso_mod $ci_ln[lso_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model= lso_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_pen"
    result
  }
  else if (method == "hybrid" && nonselection == "uncertain_nulls") {

    non_zero_terms  <- model[["beta"]]$term[model[["beta"]]$estimate != 0]
    non_zero_terms  <- non_zero_terms[non_zero_terms != "(Intercept)"]
    selected_data <-  data.frame(y = model[["y"]], data.frame(model[["x"]], check.names = FALSE),
                                 check.names = FALSE) %>%
      select(y,all_of(non_zero_terms))

    xbeta<- as.matrix(cbind("(Intercept)"=1, model[["x"]])) %*% model[["beta"]][["estimate"]]
    res <- y - xbeta

    fit_lso <- broom::tidy(lm(y ~ ., data = selected_data), conf.int = TRUE) %>%mutate(
      term = gsub("`", "", term),  # Remove backticks
    )

    lso_ignore_mod <- data.frame(term=model[["beta"]][["term"]]) %>%
      select(term) %>% dplyr::left_join(fit_lso, by = "term")
    final_mod= get_uncertain_nulls (mod= lso_ignore_mod, res=res, x=data.frame(model[["x"]], check.names = FALSE))

    ci_avg_ratio  <- mean(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=   final_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_pen"
    return(result)
  }
  else if (method == "selectiveinf" && nonselection == "ignored"){
    x <-model[["x_original"]]
    y <- model[["y"]]
    lambda=model[["lambda"]]
    std=model[["std"]]

    x_mat <- model.matrix(y ~ .,data.frame(x, y = model[["y"]],check.names = F) )[, -1]
    fit_lso= sel_inf(x_mat,y,lam = lambda, std=std)

    lso_mod <- data.frame(term=model[["beta"]][["term"]])   %>% select(term) %>%
      dplyr::left_join(fit_lso, by = "term")  %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      mutate(ci_ln=conf.high- conf.low)

    ci_avg_ratio  <- mean(lso_mod$ci_ln[lso_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(lso_mod $ci_ln[lso_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=   lso_mod ,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   infmethod = method, nonselection = nonselection,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]]
                   )

    class(result) <- "infer_pen"
    return(result )
  }
  else if (method == "selectiveinf" && nonselection == "confident_nulls") {
    x <-model[["x_original"]]
    y <- model[["y"]]
    lambda=model[["lambda"]]
    std=model[["std"]]

    x_mat <- model.matrix(y ~ .,data.frame(x, y = model[["y"]],check.names = F) )[, -1]
    fit_lso= sel_inf(x_mat,y,lam = lambda, std=std)


    lso_mod <- data.frame(term=model[["beta"]][["term"]])   %>% select(term) %>%
      dplyr::left_join(fit_lso, by = "term") %>%
      mutate(estimate = ifelse(is.na(estimate),0,estimate),
             p.value = ifelse(is.na(p.value), 1, p.value),
             conf.low = ifelse(is.na(conf.low), 0, conf.low),
             conf.high = ifelse(is.na(conf.high), 0, conf.high),
      ) %>% select(term, estimate, p.value, conf.low, conf.high) %>%
      mutate(ci_ln = conf.high- conf.low)

    ci_avg_ratio  <- mean(lso_mod$ci_ln[lso_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(lso_mod $ci_ln[lso_mod$term != "(Intercept)"] , na.rm=T)

    result <- list(model=   lso_mod ,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_pen"
    return(result )


  }
  else if (method == "selectiveinf" && nonselection == "uncertain_nulls"){

    x <-model[["x_original"]]
    y <- model[["y"]]
    lambda=model[["lambda"]]
    std=model[["std"]]

    x_mat <- model.matrix(y ~ .,data.frame(x, y = model[["y"]],check.names = F) )[, -1]
    fit_lso= sel_inf(x_mat,y,lam = lambda, std=std)


    lso_mod <- data.frame(term=model[["beta"]][["term"]])   %>% dplyr::select(term) %>%
      dplyr::left_join(fit_lso, by = "term")  %>%
      dplyr::mutate(std.error=NA, statistic=NA) %>%
      dplyr::relocate(term, estimate,std.error, statistic, p.value, conf.low, conf.high)

    beta <- ifelse(is.na(lso_mod$estimate),0, lso_mod$estimate)
    fitted_values <- as.matrix(cbind(1,x_mat)) %*% as.matrix(beta)
    res <- y - fitted_values

    final_mod = get_uncertain_nulls (mod= lso_mod , res=res, x=data.frame(model[["x"]], check.names = FALSE))%>% select(-std.error,-statistic)

    ci_avg_ratio  <- mean(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(final_mod$ci_ln[final_mod$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  final_mod,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio  ,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],
                   alpha=model[["alpha"]],
                   infmethod = method, nonselection = nonselection)
    class(result) <- "infer_ic"
    result
  }
}




#' Inference for selector_ic class based on bootstrap
#' other methods are supposed in infer.selector.ic
#'
#' @param model model returned from selector_ic  class
#' @param B  Number of bootstraps samples
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param parallel  Whether to run bootstrap in parallel or not
#' @param ... Additional arguments
#' @return "boot_ic" class list with
#'   \item{model}{A dataframe with model results including columns for term, mean_estimate,  conf.low, conf.high, median_p.value,ci_ln, prop.select, prop.reject}
#'   \item{ci_avg_ratio}{Average CI length across all variables in model}
#'   \item{ci_median_ratio}{median CI length across all variables in model}
#'   \item{selection_method}{Stepwsie}
#'   \item{direction}{selected the mode of step wise search}
#'   \item{penalty}{penalty used (AIC or BIC)}
#'   \item{infmethod}{Inference method chosen}
#'   \item{nonselection}{method chosen  to deal with non selection}
#'   \item{B}{The number of bootstrap replicates used}
#'


boot.selector.ic  <- function(model, B=10,nonselection = "ignored",parallel=  FALSE,... ){
  direction=model[["direction"]]
  x <-model[["x_model"]]
  y <- model[["y"]]
  #x_mat= model.matrix(y ~., model.frame(~ ., cbind(x,y=model[["y"]]), na.action=na.pass))


  if(model[["penalty"]]=="AIC"){
    if(nonselection == "ignored"){
      results=boot_stepwise_aic (x, y, B=B,family="gaussian",direction=direction, nonselector="ignored",
                                 parallel=  parallel, model=model)

      ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
      ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)

      result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                     selection_method="Stepwise",direction = direction,penalty= "AIC",
                     infmethod = "bootstrap", nonselection = nonselection, B=B
      )
      class(result) <- "boot_ic"
      result


    } else if (nonselection == "confident_nulls"){
      results= boot_stepwise_aic (x, y, B=B,family="gaussian",direction=direction, nonselector="confident_nulls",
                                 parallel=  parallel, model=model)

      ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
      ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)

      result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                     selection_method="Stepwise",direction = direction,penalty= "AIC",
                     infmethod = "bootstrap", nonselection = nonselection, B=B
      )
      class(result) <- "boot_ic"
      result

    } else if (nonselection == "uncertain_nulls"){
      results= boot_stepwise_aic (x, y, B=B,family="gaussian",direction=direction, nonselector="uncertain_nulls",
                                  parallel=  parallel, model=model)
      ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
      ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)

      result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                     selection_method="Stepwise",direction = direction,penalty= "AIC",
                     infmethod = "bootstrap", nonselection = nonselection, B=B
      )
      class(result) <- "boot_ic"
      result

    }
  }
  else if (model[["penalty"]]=="BIC"){
    if(nonselection == "ignored"){

      results=boot_stepwise_bic (x, y, B=B,family="gaussian",direction=direction, nonselector="ignored",
                                 parallel=  parallel, model=model)
      ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
      ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)


      result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                     selection_method="Stepwise",direction = direction,penalty= "BIC",
                     infmethod = "bootstrap", nonselection = nonselection, B=B
      )
      class(result) <- "boot_ic"
      result

    } else if (nonselection == "confident_nulls"){
      results= boot_stepwise_bic (x, y, B=B,family="gaussian",direction=direction, nonselector="confident_nulls",
                                  parallel=  parallel, model=model)
      ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
      ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)


      result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                     selection_method="Stepwise",direction = direction,penalty= "BIC",
                     infmethod = "bootstrap", nonselection = nonselection, B=B
      )
      class(result) <- "boot_ic"
      result

    } else if (nonselection == "uncertain_nulls"){
      results= boot_stepwise_bic (x, y, B=B,family="gaussian",direction=direction, nonselector="uncertain_nulls",
                                  parallel=  parallel, model=model)
      ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
      ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)


      result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                     selection_method="Stepwise",direction = direction,penalty= "BIC",
                     infmethod = "bootstrap", nonselection = nonselection, B=B
      )
      class(result) <- "boot_ic"
      result


    }

  }

}


#' Title Inference for selector_pen class based on bootstrap, other methods are supposed in infer.selector.pen

#'
#' @param model model of selector_pen class returned from  pen_cv function
#' @param B  The number of bootstrap replicates.
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#' @param parallel whether to run bootstrap in parallel for faster computation
#' @param ...
#'
#' @return "boot_pen" class list with
#' \item{model}{A dataframe with model results including columns for term, mean_estimate,  conf.low, conf.high,ci_ln, prop.select}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{median CI length across all variables in model}
#' \item{selection_method}{lasso' or 'MCP' Note: Same as in ncvreg, even with elasticnet-it says lasso but can be differntiated with value of alpha}
#' \item{lambda}{selected lambda for inference , either "lambda.min" or "lambda.1se"}
#' \item{alpha}{selected alpha for inference}
#' \item{infmethod}{Inference method chosen}
#' \item{nonselection}{method chosen  to deal with non selection}
#' \item{B}{The number of bootstrap replicates used}
#' @export
#'
#'

boot.selector.pen  <-function(model, B=10,nonselection = "ignored",parallel=  FALSE,... ){


  if(nonselection == "ignored"){
    results=boot_pen(model,B=2, nonselection = "ignored",  parallel= parallel,... )
    ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],alpha=model[["alpha"]],
                   infmethod = "bootstrap", nonselection = nonselection, B=B)
    class(result) <- "boot_pen"
    result

  }else if (nonselection == "confident_nulls"){
    results=boot_pen(model,B=B, nonselection = "confident_nulls", parallel= parallel,... )
    ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],alpha=model[["alpha"]],
                   infmethod = "bootstrap", nonselection = nonselection, B=B)
    class(result) <- "boot_pen"
    result

  }else if (nonselection == "uncertain_nulls"){
    results=boot_pen(model,B=B, nonselection = "uncertain_nulls",  parallel= parallel,... )
    ci_avg_ratio  <- mean(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)
    ci_median_ratio <-  median(results$ci_ln[results$term != "(Intercept)"] , na.rm=T)


    result <- list(model=  results,ci_avg_ratio =ci_avg_ratio ,ci_median_ratio =ci_median_ratio,
                   selection_method=model[["penalty"]],lambda= model[["lambda"]],alpha=model[["alpha"]],
                   infmethod = "bootstrap", nonselection = nonselection, B=B)
    class(result) <- "boot_pen"
    result

  }




}



#' Post-selection inference
#'
#' @param model model of selector_ic or selector_pen class returned from step_ic or pen_cv function
#' @param method A character string specifying method of post-selection inference.Currently "hybrid", "selectiveinf" or
#' "boot" supported
#' @param nonselection A character string specifying how to handle variables not selected by model selection procedure. One of
#' "ignored", "confident_nulls" or "uncertain_nulls" supported
#'
#' @return A list of class `infer_ic` or `infer_pen` containing:#'
#' \item{model}{A data frame with post-selection inference results}
#' \item{ci_avg_ratio}{Average CI length across all variables in model}
#' \item{ci_median_ratio}{median CI length across all variables in model}
#' \item{nonselection}{method chosen  to deal with non selection}
#' \item{infmethod}{Inference method chosen}
#' \item{selection_method}{Stepwsie,returned for selector_ic class only}
#' \item{direction}{the mode of step wise search, returned for selector_ic class only}
#' \item{penalty}{penalty used (AIC or BIC), returned for selector_ic class only}
#' \item{lambda}{selected lambda for inference , either "lambda.min" or "lambda.1se"; returned for selector_pen class only}
#' \item{alpha}{selected alpha for inference, returned for selector_pen class only}
#' \item{B}{The number of bootstrap replicates used (only for bootstrap selection method)}
#'
#' @importFrom broom tidy
#' @export
#'
#'
#'


infer <- function(model, method="hybrid", nonselection="ignored",...){
  if(method =="hybrid"){
    if (nonselection=="ignored"){
      if (class(model)=="selector_ic"){
        infer.selector.ic (model)
      } else{
        infer.selector.pen (model)
      }


    } else if (nonselection=="confident_nulls"){
      if (class(model)=="selector_ic"){
        infer.selector.ic (model,nonselection="confident_nulls" )
      } else{
        infer.selector.pen (model,nonselection="confident_nulls")
      }

    }
    else{
      if (class(model)=="selector_ic"){
        infer.selector.ic (model,nonselection="uncertain_nulls" )
      } else{
        infer.selector.pen (model,nonselection="uncertain_nulls")
      }

    }
  }else if (method =="selectiveinf"){
    if (nonselection=="ignored"){

      if (class(model)=="selector_ic"){
        infer.selector.ic (model, method = "selectiveinf",nonselection="ignored")
      } else{
        infer.selector.pen (model, method ="selectiveinf",nonselection="ignored")
      }


    } else if (nonselection=="confident_nulls"){
      if (class(model)=="selector_ic"){
        infer.selector.ic (model,nonselection="confident_nulls",method ="selectiveinf" )
      } else{
        infer.selector.pen (model,nonselection="confident_nulls", method ="selectiveinf")
      }

    }
    else{
      if (class(model)=="selector_ic"){
        infer.selector.ic (model,nonselection="uncertain_nulls",method ="selectiveinf")
      } else{
        infer.selector.pen (model,nonselection="uncertain_nulls",method ="selectiveinf")
      }
    }
  }
  else if (method =="boot"){
    if (nonselection=="ignored"){
      if (class(model)=="selector_ic"){
        boot.selector.ic (model,nonselection="ignored",...)
      } else{
        boot.selector.pen (model,nonselection="ignored",...)
      }

    } else if (nonselection=="confident_nulls"){
      if (class(model)=="selector_ic"){
        boot.selector.ic (model,nonselection="confident_nulls",...)
      } else{
        boot.selector.pen (model,nonselection="confident_nulls",...)
      }


    }
    else if (nonselection=="uncertain_nulls") {
      if (class(model)=="selector_ic"){
        boot.selector.ic (model,nonselection="uncertain_nulls",...)
      } else{
        boot.selector.pen (model,nonselection="uncertain_nulls",...)
      }

    }
  }
}




#' Title
#' @param x model of class `infer_ic`
#' @return returns x invisibly
#' @export
print.infer_ic <- function(x, ...) {

    # Penalty used for selection
    cat("Selection method: ", x[["selection_method"]], "  ",x[["penalty"]],".  Direction: " ,x[["direction"]], "\n")

    # lambda
    cat("Inference methohd: ", x[["infmethod"]], "\n")

    cat ("Method for handling null: ", x[["nonselection"]], "\n")

    # Average CI length
    cat ("Average confidence interval length ", x[["ci_avg_ratio"]], "\n")

    # Median CI length
    cat ("Median confidence interval length ", x[["ci_median_ratio"]], "\n")

    # coeff <-c(x[["model"]][["estimate"]])
    # names(coeff)<- x[["model"]][["term"]]
    #
    # cat("\nFinal Model Results:\n")
    # print( coeff)


}

#' Title
#' @param x model of class `boot_ic`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export
print.boot_ic <- function(x, ...) {

 # Penalty used for selection
    cat("Selection method: ", x[["selection_method"]], "  ",x[["penalty"]],".  Direction: " ,x[["direction"]], "\n")

    # lambda
    cat("Inference methohd: ", x[["infmethod"]], "with ", x[["B"]],"bootstrap samples","\n")

    cat ("Method for handling null: ", x[["nonselection"]], "\n")

    # Average CI length
    cat ("Average confidence interval length: ", x[["ci_avg_ratio"]], "\n")

    # Median CI length
    cat ("Median confidence interval length: ", x[["ci_median_ratio"]], "\n")
}

#' Title
#'
#' @param x x model of class `infer_pen`
#' @return returns x invisibly
#' @export

print.infer_pen <- function(x, ...) {
  if (x[["alpha"]] ==1) {
    var_method <- c("lasso")
  } else{
    var_method <- c("elastic Net")
  }

  # Penalty used for selection
  cat("Selection method: ", var_method, ".  ","Choice of lambda: " ,x[["lambda"]], "\n")

  # lambda
  cat("Inference methohd: ", x[["infmethod"]], "\n")

  cat ("Method for handling null: ", x[["nonselection"]], "\n")

  # Average CI length
  cat ("Average confidence intervals length ", x[["ci_avg_ratio"]],"\n")

  # Median CI length
  cat ("Median confidence intervals length ", x[["ci_median_ratio"]],"\n")

  # # Model coefficients
  # coeff <-c(x[["model"]][["estimate"]])
  # names(coeff)<-x[["model"]][["term"]]
  #
  # cat("\nFinal Model Results:\n")
  # print( coeff)

}


#' Title
#' @param x x model of class `boot_pen`
#' @return returns x invisibly
#' @export
print.boot_pen <- function(x, ...) {
  if (x[["alpha"]] ==1) {
    var_method <- c("lasso")
  } else{
    var_method <- c("elastic Net")
  }

  # Penalty used for selection
  cat("Selection method: ", var_method, ".  ","Choice of lambda: " ,x[["lambda"]], "\n")

  # lambda
  cat("Inference methohd: ", x[["infmethod"]], "with ", x[["B"]],"bootstrap samples","\n")

  cat ("Method for handling null: ", x[["nonselection"]], "\n")

  # Average CI length
  cat ("Average confidence intervals length ", x[["ci_avg_ratio"]],"\n")

  # Median CI length
  cat ("Median confidence intervals length ", x[["ci_median_ratio"]],"\n")

}


#' Title
#' @param x model of class `infer_ic`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom  tidy
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.infer_ic <- function(x, ...) {

  ret<- as_tibble(x[["model"]])
  return(ret)
}


#' Title
#' @param x model of class `infer_pen`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export

tidy.infer_pen <- function(x, ...) {

  ret<- as_tibble(x[["model"]])
  return(ret)
}

#' Title
#' @param x model of class `boot_ic`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.boot_ic <- function(x, ...) {

  ret<- as_tibble(x[["model"]])
  return(ret)
}

#' Title
#'
#' @param x model of class `boot_pen`
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom broom tidy
#' @param ... currently not used
#' @return A tibble containing the tidied coefficients of the model.
#' @export
tidy.boot_pen <- function(x, ...) {

  ret<- as_tibble(x[["model"]])
  return(ret)
}
