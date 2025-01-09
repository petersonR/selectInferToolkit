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
      mod<- tidy(model[["model"]], conf.int=T)

      full_mod= data.frame(term=model[["beta"]][["term"]]) %>%
      select(term) %>%
      left_join(mod, by = "term") %>%
      mutate(  estimate = ifelse(is.na(estimate), 0,  estimate),
               p.value = ifelse(is.na(p.value), 1, p.value),
               std.error= ifelse(is.na(std.error), 0, std.error),
               conf.low = ifelse(is.na(conf.low), 0, conf.low),
               conf.high = ifelse(is.na(conf.high), 0, conf.high),
      ) %>% select(term, estimate, p.value, std.error,statistic,conf.low, conf.high)
      full_mod$ci_ln <- full_mod$conf.high - full_mod$conf.low
      full_mod$ci_avg_ratio <- mean(full_mod$ci_ln , na.rm=T)
      full_mod$ci_median_ratio <- median(full_mod$ci_ln , na.rm=T)
      full_mod

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
