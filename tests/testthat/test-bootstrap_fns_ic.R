##### Test mtcars data ######

data("mtcars")
library(testthat)
set.seed(123)
sel0 <- select_null_model(mpg ~ ., data = mtcars)
sel1 <- select_stepwise_ic(mpg ~ ., data = mtcars)
sel2 <- select_glmnet(mpg ~ ., data = mtcars)
sel3 <- select_ncvreg(mpg ~ ., data = mtcars, fold = sel2$foldid)
sel4 <- select_full_model(mpg ~ ., data = mtcars)

test_that("basic inferrer bootstrap functionality; null model", {

  # run vanilla version
  expect_no_warning({
    inf1 <- infer_boot(sel0, data = mtcars, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
  })

  # try debiasing everything
  expect_no_warning({
    inf2 <- infer_boot(sel0, data = mtcars, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
  })

  # try for "all" inference target
  expect_no_warning({
    inf3 <- infer_boot(sel0, data = mtcars, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
  })

  # try debiasing
  expect_no_warning({
    inf4 <- infer_boot(sel0, data = mtcars, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
  })

  # build in some additional testing
})

test_that("basic inferrer bootstrap functionality; stepwise IC", {

  # run vanilla version
  expect_no_warning({
    inf1 <- infer_boot(sel1, data = mtcars, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
  })

  # coef for `cyl` should be selected and nonzero
  expect_equal(vals1$selected[2], 1)
  expect_lt(cyl_coef <- vals1$coef[2], 0 )

  # coef for `gear` should be zero
  expect_equal(vals1$coef[10], 0)
  expect_true(is.na(vals1$estimate[10]))

  # try debiasing everything
  expect_no_warning({
    inf2 <- infer_boot(sel1, data = mtcars, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
  })

  # coefs should be the same
  expect_equal(vals1$coef, vals2$coef)

  # try for "all" inference target
  expect_no_warning({
    inf3 <- infer_boot(sel1, data = mtcars, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
  })

  # try debiasing
  expect_no_warning({
    inf4 <- infer_boot(sel1, data = mtcars, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
  })

  # build in some additional testing
})

test_that("basic inferrer bootstrap functionality; glmnet", {

  set.seed(123)
  # run vanilla version
  expect_no_warning({
    inf1 <- infer_boot(sel2, data = mtcars, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
  })

  # coef for `cyl` should be selected and nonzero w/0 ci upper bound
  expect_equal(vals1$selected[2], 1)
  expect_lt(cyl_coef <- vals1$coef[2], 0 )
  cyl_ci_high <- unname(vals1$ci_high[2])
  expect_equal(cyl_ci_high, 0)

  # coef for `gear` should be zero
  expect_equal(vals1$coef[10], 0)
  expect_true(is.na(vals1$estimate[10]))

  # try debiasing everything
  expect_no_warning({
    inf2 <- infer_boot(sel2, data = mtcars, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
  })

  # coefs should be the same
  expect_equal(vals1$coef, vals2$coef)

  cyl_ci_high <- unname(vals2$ci_high[2])
  expect_gt(cyl_ci_high, 0)

  # try for "all" inference target
  expect_no_warning({
    inf3 <- infer_boot(sel2, data = mtcars, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
  })

  # try debiasing
  expect_no_warning({
    inf4 <- infer_boot(sel2, data = mtcars, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
  })

  # build in some additional testing
})

# small here when comparing coeff
test_that("basic inferrer bootstrap functionality; ncvreg", {

  set.seed(123)
  # run vanilla version
  expect_no_warning({
    inf1 <- infer_boot(sel3, data = mtcars, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
  })

  # coef for `wt` should be selected and nonzero w/0 ci upper bound
  expect_equal(vals1$selected[6], 1)
  expect_lt(cyl_coef <- vals1$coef[6], 0 )
  cyl_ci_high <- unname(vals1$ci_high[6])
  expect_equal(cyl_ci_high, 0)

  # coef for `gear` should be zero
  expect_equal(vals1$coef[10], 0)
  expect_true(is.na(vals1$estimate[10]))

  # try debiasing everything
  expect_no_warning({
    inf2 <- infer_boot(sel3, data = mtcars, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
  })

  # coefs should be the same
  expect_equal(vals1$coef, vals2$coef)

  cyl_ci_high <- unname(vals2$ci_high[6])
  expect_lt(cyl_ci_high, 0) # error

  # try for "all" inference target
  expect_no_warning({
    inf3 <- infer_boot(sel3, data = mtcars, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
  })

  # try debiasing
  expect_no_warning({
    inf4 <- infer_boot(sel3, data = mtcars, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
  })

  # build in some additional testing
})

test_that("basic inferrer bootstrap functionality; full model", {

  # run vanilla version
  expect_no_warning({
    inf1 <- infer_boot(sel4, data = mtcars, B = 50, debias = FALSE)
    vals1 <- tidy(inf1)
  })

  # try debiasing everything
  expect_no_warning({
    inf2 <- infer_boot(sel4, data = mtcars, B = 50, debias = TRUE)
    vals2 <- tidy(inf2)
  })

  # try for "all" inference target
  expect_no_warning({
    inf3 <- infer_boot(sel4, data = mtcars, B = 50, debias = FALSE, inference_target = "all")
    vals3 <- tidy(inf3)
  })

  # try debiasing
  expect_no_warning({
    inf4 <- infer_boot(sel4, data = mtcars, B = 50, debias = TRUE, inference_target = "all")
    vals4 <- tidy(inf4)
  })
  # build in some additional testing
})

test_that("basic inferrer bootstrap functionality; glmnet + out-of-sample estimation", {

  set.seed(123)
  # run vanilla version
  expect_warning({
    inf1 <- infer_boot(sel2, data = mtcars, B = 50, debias = FALSE, estimation_data = "out-of-sample")
    vals1 <- tidy(inf1)
  })

  # coef for `cyl` should be selected and nonzero
  expect_equal(vals1$selected[2], 1)
  expect_lt(cyl_coef <- vals1$coef[2], 0 )

  # coef for `gear` should be zero
  expect_equal(vals1$coef[10], 0)
  expect_true(is.na(vals1$estimate[10]))

  # try debiasing everything
  expect_warning({
    inf2 <- infer_boot(sel2, data = mtcars, B = 50, debias = TRUE,
                       estimation_data = "out-of-sample")
    vals2 <- tidy(inf2)
  })

  # coefs should be the same
  expect_equal(vals1$coef, vals2$coef)

  # try for "all" inference target
  expect_warning({
    inf3 <- infer_boot(sel2, data = mtcars[1:20,], B = 50, debias = FALSE,
                       inference_target = "all", estimation_data = "out-of-sample")
    vals3 <- tidy(inf3)
  })

  # try debiasing
  expect_warning({
    inf4 <- infer_boot(sel2, data = rbind(mtcars, mtcars, mtcars), B = 50, debias = TRUE,
                       inference_target = "all", estimation_data = "out-of-sample")
    vals4 <- tidy(inf4)
  })

  # build in some additional testing
})




skip() # Code below requires updating

# ##### Test HERS data ######
# Test HERS Data set
y= hers$hdl1
#x <-model.matrix(hdl1 ~., model.frame(~ ., hers, na.action=na.pass))[,-1]
x <- hers %>% dplyr::select(-hdl1)



test_that("Full model boot works ", {
  expect_silent({
    model<- lm(y~., data=cbind(y,x), x= TRUE, y=TRUE)
    obj1 <-full_boot(model, B=5, family="gaussian",parallel = FALSE)

  })

  expect_no_error(
    obj1 <- full_boot(model, B=5, family="gaussian",parallel = TRUE)

  )

})

test_that("Full model stepwsie aic boot bi-dirctional works ", {
  expect_silent({
    aic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "both")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "both")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "both")
    obj1p <-infer(aic_mod, method = "boot", B=5, n_cores = 2)
    obj2p <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj3p <-infer(aic_mod, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")

    aic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "both")
    obj4p <-infer(aic_mod_nostd, method = "boot", B=5, n_cores = 2)
    obj5p <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj6p <-infer(aic_mod_nostd, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")


  })

})


test_that("Full model stepwsie aic boot forward selection works ", {
  expect_silent({
    aic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "forward",make_levels = T)
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "forward")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "forward")
    obj1p <-infer(aic_mod, method = "boot", B=5, n_cores = 2)
    obj2p <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj3p <-infer(aic_mod, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")

    aic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "forward")
    obj4p <-infer(aic_mod_nostd, method = "boot", B=5, n_cores = 2)
    obj5p <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj6p <-infer(aic_mod_nostd, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie aic boot backward selection works ", {
  expect_silent({
    aic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "backward")
    obj1 <- infer(aic_mod, method = "boot", B=5)
    obj2 <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(aic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    aic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "backward")
    obj4 <- infer(aic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(aic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    aic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "backward")
    obj1p <-infer(aic_mod, method = "boot", B=5, n_cores = 2)
    obj2p <- infer(aic_mod, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj3p <-infer(aic_mod, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")

    aic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "backward")
    obj4p <-infer(aic_mod_nostd, method = "boot", B=5, n_cores = 2)
    obj5p <- infer(aic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj6p <-infer(aic_mod_nostd, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie bic boot bi-dirctional works ", {
  expect_silent({
    bic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "both",penalty = "BIC")
    obj1 <- infer(bic_mod, method = "boot", B=5)
    obj2 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(bic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    bic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "both",penalty = "BIC")
    obj4 <- infer(bic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    bic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "both",penalty = "BIC")
    obj1p <-infer(bic_mod, method = "boot", B=5, n_cores = 2)
    obj2p <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj3p <-infer(bic_mod, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")

    bic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "both", penalty = "BIC")
    obj4p <-infer(bic_mod_nostd, method = "boot", B=5, n_cores = 2)
    obj5p<- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj6p <-infer(bic_mod_nostd, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")


  })

})

test_that("Full model stepwsie bic boot forward selection works ", {
  expect_silent({
    bic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "forward",penalty = "BIC")
    obj1 <- infer(bic_mod, method = "boot", B=5)
    obj2 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(bic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    bic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "forward",penalty = "BIC")
    obj4 <- infer(bic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    bic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "forward", penalty = "BIC")
    obj1p <-infer(bic_mod, method = "boot", B=5, n_cores = 2)
    obj2p <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj3p <-infer(bic_mod, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")

    bic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "forward",penalty = "BIC")
    obj4p <-infer(bic_mod_nostd, method = "boot", B=5, n_cores = 2)
    obj5p <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj6p  <-infer(bic_mod_nostd, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")


  })





})

test_that("Full model stepwsie bic boot backward selection works ", {
  expect_silent({
    bic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "backward",penalty = "BIC")
    obj1 <- infer(bic_mod, method = "boot", B=5)
    obj2 <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls")
    obj3 <-infer(bic_mod, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")

    bic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "backward",penalty = "BIC")
    obj4 <- infer(bic_mod_nostd, method = "boot", B=5)
    obj5 <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls")
    obj6 <-infer(bic_mod_nostd, method = "boot", B=5, parallel=F, nonselection = "uncertain_nulls")


  })

  expect_no_error({
    bic_mod=stepwise_ic (x=x,y=y,std = TRUE, direction = "backward",penalty = "BIC")
    obj1p <-infer(bic_mod, method = "boot", B=5, n_cores = 2)
    obj2p <- infer(bic_mod, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj3p <-infer(bic_mod, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")

    bic_mod_nostd=stepwise_ic (x=x,y=y,std = FALSE, direction = "backward",penalty = "BIC")
    obj4p <-infer(bic_mod_nostd, method = "boot", B=5, n_cores = 2)
    obj5p <- infer(bic_mod_nostd, method = "boot", B=5, nonselection = "confident_nulls",n_cores = 2)
    obj6p <-infer(bic_mod_nostd, method = "boot", B=5, n_cores = 2, nonselection = "uncertain_nulls")


  })





})

#testthat::test_file("tests/testthat/test-bootstrap_fns_ic.R")
