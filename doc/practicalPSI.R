## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,fig.height = 5, fig.width = 7)
library(practicalPSI)
library(tidyverse)


## -----------------------------------------------------------------------------
data("raw_data")
summary(raw_data)

## -----------------------------------------------------------------------------
y= raw_data$hdl1
x <- raw_data %>% dplyr::select(-hdl1)

## -----------------------------------------------------------------------------
aic_model=step_ic (x=x,y=y,std = TRUE, penalty = "AIC", direction = "both")

## -----------------------------------------------------------------------------
lassomin_model =pen_cv (x=x,y=y,penalty= "lasso",lambda="lambda.min",std=TRUE)

## -----------------------------------------------------------------------------
aic_hybrid =infer(aic_model,  method = "hybrid", nonselection = "ignored")

## -----------------------------------------------------------------------------
lasso_sel_confident  = infer(lassomin_model , method="selectiveinf",nonselection="confident_nulls")

## -----------------------------------------------------------------------------
model<- lm(y~., data=cbind(y,x), x= TRUE, y=TRUE)
full_model_linear <- full_boot(model, B=5, family="gaussian",parallel = TRUE)

full_model_linear

