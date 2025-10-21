data("mtcars")

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
    tidy(sel)
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
sel1 <- select_stepwise_ic(mpg ~ ., data = mtcars)
sel2 <- select_glmnet(mpg ~ ., data = mtcars)
sel3 <- select_ncvreg(mpg ~ ., data = mtcars)

infer(sel1, data = mtcars, B = 10)
infer_boot(sel2, data = mtcars, B = 10)
infer_boot(sel3, data = mtcars, B = 10) # issues with this, to start next time...

