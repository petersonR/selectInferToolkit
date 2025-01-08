#' Title
#'
#' @param model
#' @param method
#' @param nonselection
#'
#' @return
#' @importFrom broom tidy
#' @export
#'
#' @examples
#'
#'
#'


infer <- function(model, method="hybrid", nonselection="ignored"){
  if(method =="hybrid"){
    if (nonselection=="ignored"){
      if (model[["penalty"]] =="AIC" | aic_mod[["penalty"]] =="BIC"){
        mod<- tidy(model[["model"]], conf.int=T)
        mod$ci_ln <- mod$conf.high - mod$conf.low

        full_mod <-data.frame(term=model[["beta"]][["term"]]) %>%
          dplyr::left_join(mod, by = "term")

        full_mod$ci_avg_ratio <- mean(full_mod$ci_ln , na.rm=T)
        full_mod$ci_median_ratio <- median(full_mod$ci_ln , na.rm=T)
        full_mod
      } else{

      }


    } else if (nonselection=="confident_nulls"){

    }
    else{

    }
  }else if (method =="selectiveinf"){
    if (nonselection=="ignored"){

    } else if (nonselection=="confident_nulls"){

    }
    else{

    }
  }
  else{
    if (nonselection=="ignored"){

    } else if (nonselection=="confident_nulls"){

    }
    else{

    }
  }
}
