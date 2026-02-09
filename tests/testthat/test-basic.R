data("mtcars")
library(testthat)

test_that("basic selector operations (full model, no selection)", {
  sel <- select_full_model(mpg ~ ., mtcars)

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    predict(sel, newdata = mtcars[1:5,])
    predict(sel, newdata = mtcars)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = mtcars))

})

test_that("basic selector operations (null model, no selection)", {
  sel <- select_null_model(mpg ~ ., mtcars)

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    predict(sel, newdata = mtcars[1:5,])
    predict(sel, newdata = mtcars)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = mtcars))

})

test_that("basic selector operations (stepwise IC)", {
  sel <- select_stepwise_ic(mpg ~ ., mtcars)

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    predict(sel, newdata = mtcars[1:5,])
    predict(sel, newdata = mtcars)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = mtcars))

})

test_that("basic selector operations (glmnet)", {
  sel <- select_glmnet(mpg ~ ., mtcars)
  selector_object <- sel

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel)
    predict(sel, newdata = mtcars[1:5,])
    predict(sel, newdata = mtcars)
  })

})

test_that("basic selector operations (ncvreg)", {
  sel <- select_ncvreg(mpg ~ ., mtcars)

  expect_no_warning({
    capture_output(print(sel))
    tidy(sel, scale_coef = FALSE)
    predict(sel, newdata = mtcars[1:5,])
    predict(sel, newdata = mtcars)
  })
})

test_that("reselector operations", {
  sel <- select_stepwise_ic(mpg ~ ., mtcars)
  rsel <- reselect(sel, mtcars)
  expect_identical(coef(sel), coef(rsel))
  expect_identical(predict(sel, newdata = mtcars), predict(rsel, newdata = mtcars))
  expect_identical(tidy(sel), tidy(rsel))

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = mtcars[1:15,])
  # should at least have same selections, a bit different due to pre-processing
  expect_equal(
    names(coef(rsel2)),
    names(coef(select_stepwise_ic(mpg ~ ., mtcars[1:15,]))))

  set.seed(1)
  sel <- select_glmnet(mpg ~ ., mtcars)
  rsel <- reselect(sel, mtcars)

  # expect estimates to be close, may differ slightly
  rsel_error <- max(tidy(sel)$coef - tidy(rsel)$coef, na.rm = TRUE)
  expect_lt(rsel_error, .01)

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = mtcars[-7,])
  set.seed(1)
  sel15 <- select_glmnet(mpg ~ ., mtcars)

  # No variance across same method with same seed
  expect_equal(tidy(sel15), tidy(sel))
})

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
 # expect_equal(cyl_ci_high, 0)

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
  #expect_gt(cyl_ci_high, 0)

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
test_that("Basic inferrer bootstrap functionality; ncvreg", {

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
 # expect_equal(cyl_ci_high, 0)

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
  #expect_lt(cyl_ci_high, 0) # error

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

test_that("basic inferrer UPSI functionality", {
  expect_no_warning({
    inf0 <- infer(sel0, data = mtcars)
    capture_output(print(inf0))
    tidy(inf0)
  })

  expect_no_warning({
    inf1 <- infer_upsi(sel1, data = mtcars)
    capture_output(print(inf1))
    tidy(inf1)
  })

  expect_no_warning({
    inf2 <- infer_upsi(sel2, data = mtcars)
    capture_output(print(inf2))
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_upsi(sel3, data = mtcars)
    capture_output(print(inf3))
    tidy(inf3)
  })

  expect_no_warning({
    inf4 <- infer_upsi(sel4, data = mtcars)
    tidy(inf4)
  })

})

test_that("basic inferrer UPSI, uncertain null approach", {
  expect_no_warning({
    inf0 <- infer(sel0, data = mtcars, nonselection = "uncertain")
    tidy(inf0)
  })

  expect_no_warning({
    inf1 <- infer_upsi(sel1, data = mtcars, nonselection = "uncertain")
    tidy(inf1)
  })

  expect_no_warning({
    inf2 <- infer_upsi(sel2, data = mtcars, nonselection = "uncertain")
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_upsi(sel3, data = mtcars, nonselection = "uncertain")
    tidy(inf3)
  })

  expect_no_warning({
    inf4 <- infer_upsi(sel4, data = mtcars, nonselection = "uncertain")
    tidy(inf4)
  })

})

test_that("selective inference + glmnet", {
  sel1 <- select_glmnet(mpg ~ ., data = mtcars)

  expect_no_warning({
    inf1 <- infer_selective(sel1, data = mtcars, nonselection = "uncertain")
    tidy(inf1)
  })

  expect_no_warning({
    inf2 <- infer_selective(sel1, data = mtcars, nonselection = "ignore")
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_selective(sel1, data = mtcars, nonselection = "confident")
    tidy(inf3)
  })

})

test_that("selective inference + forward AIC", {
  sel1 <- select_stepwise_ic(mpg ~ ., data = mtcars, direction = "forward")

  expect_no_warning({
    inf1 <- infer_selective(sel1, data = mtcars, nonselection = "uncertain")
    tidy(inf1)
  })

  expect_no_warning({
    inf2 <- infer_selective(sel1, data = mtcars, nonselection = "ignore")
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_selective(sel1, data = mtcars, nonselection = "confident")
    tidy(inf3)
  })

})

test_that("PIPE inference + ncvreg", {
  sel1 <- select_ncvreg(mpg ~ ., data = mtcars)

  expect_no_warning({
    inf1 <- infer_pipe(sel1, data = mtcars)
    tidy(inf1)
  })


})

