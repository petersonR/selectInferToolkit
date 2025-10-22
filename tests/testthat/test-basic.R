data("mtcars")
library(testthat)

test_that("basic selector operations (full model, no selection)", {
  sel <- select_full_model(mpg ~ ., mtcars)

  expect_no_warning({
    print(sel)
    tidy(sel)
    predict(sel, newdata = mtcars[1:5,])
    predict(sel, newdata = mtcars)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = mtcars))

})

test_that("basic selector operations (null model, no selection)", {
  sel <- select_null_model(mpg ~ ., mtcars)

  expect_no_warning({
    print(sel)
    tidy(sel)
    predict(sel, newdata = mtcars[1:5,])
    predict(sel, newdata = mtcars)
  })

  expect_equal(unname(predict.glm(sel)), predict(sel, newdata = mtcars))

})


test_that("basic selector operations (stepwise IC)", {
  sel <- select_stepwise_ic(mpg ~ ., mtcars)

  expect_no_warning({
    print(sel)
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
    print(sel)
    tidy(sel)
    predict(sel, newdata = mtcars[1:5,])
    predict(sel, newdata = mtcars)
  })

})

test_that("basic selector operations (ncvreg)", {
  sel <- select_ncvreg(mpg ~ ., mtcars)

  expect_no_warning({
    print(sel)
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
  rsel_error <- max(tidy(sel)$estimate - tidy(rsel)$estimate, na.rm = TRUE)
  expect_lt(rsel_error, .001)

  # Try to re-fit with re-select to "new" data
  rsel2 <- reselect(sel, newdata = mtcars[-7,])
  set.seed(1)
  sel15 <- select_glmnet(mpg ~ ., mtcars)

  # should at least have same selections
  expect_equal(
    names(attr(rsel2, "selected_coefs")),
    names(attr(sel15, "selected_coefs"))
  )

  # No variance across same method with same seed
  expect_equal(tidy(sel15), tidy(sel))
})

set.seed(123)
sel0 <- select_null_model(mpg ~ ., data = mtcars)
sel1 <- select_stepwise_ic(mpg ~ ., data = mtcars)
sel2 <- select_glmnet(mpg ~ ., data = mtcars)
sel3 <- select_ncvreg(mpg ~ ., data = mtcars, fold = sel2$foldid)
sel4 <- select_full_model(mpg ~ ., data = mtcars)

test_that("basic inferrer bootstrap functionality", {
  expect_no_warning({
    inf0 <- infer_boot(sel0, data = mtcars, B = 100)
    val <- print(inf0)
    tidy(inf0)
  })

  expect_no_warning({
    inf1 <- infer(sel1, data = mtcars, B = 16)
    print(inf1)
    tidy(inf1)
  })

  expect_no_warning({
    inf2 <- infer_boot(sel2, data = mtcars, B = 100)
    print(inf2)
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_boot(sel3, data = mtcars, B = 100)
    print(inf3)
    tidy(inf3)
  })

  expect_no_warning({
    inf4 <- infer_boot(sel4, data = mtcars, B = 100)
    print(inf4)
    tidy(inf4)
  })

})

test_that("basic inferrer UPSI functionality", {
  expect_no_warning({
    inf0 <- infer(sel0, data = mtcars)
    print(inf0)
    tidy(inf0)
  })

  expect_no_warning({
    inf1 <- infer_upsi(sel1, data = mtcars)
    print(inf1)
    tidy(inf1)
  })

  expect_no_warning({
    inf2 <- infer_upsi(sel2, data = mtcars)
    print(inf2)
    tidy(inf2)
  })

  expect_no_warning({
    inf3 <- infer_upsi(sel3, data = mtcars)
    print(inf3)
    tidy(inf3)
  })

  expect_no_warning({
    inf4 <- infer_upsi(sel4, data = mtcars)
    print(inf4)
    tidy(inf4)
  })

})
