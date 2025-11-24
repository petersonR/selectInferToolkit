####### Test IRIS data ########

data(iris)
iris <- iris[1:100,]

set.seed(123)

#skip() # tests require updating, currently skipped


# Add another unbalanced factor
iris$Group <- factor(sample(c('A', 'B'), nrow(iris), replace = TRUE))

# Add a nzv variable
iris$NotUseful <- 2

# Add a binary variable
iris$BV <- rbinom(nrow(iris), 1, prob = .5)

# Add an unbalanced binary variable
iris$UBV <- rbinom(nrow(iris), 1, prob = .02)




test_that("Stepwise AIC forward seelction works", {

  expect_no_error({
    sel <- select_stepwise_ic(Sepal.Length ~ ., iris, direction = "forward")

      inf <- infer_selective(sel, data = iris, nonselection = "ignore")
      capture_output(print(inf))
      tidy(inf)

      inf_conf <- infer_selective(sel, data = iris, nonselection = "confident")
      capture_output(print(inf_conf))
      tidy(inf_conf)

      inf_un <- infer_selective(sel, data = iris, nonselection = "uncertain_nulls")
      capture_output(print(inf_un))
      tidy(inf_un)

  })

  # Test 1: if select == 0, estimate must be NA for ignore case
  expect_true(
    sum(inf$select == 0 & !is.na(inf$estimate)) == 0
  )


  # Test 2: confident_nulls then estimate = 0 whenever select = 0
  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  # Test 3: uncertain_nulls → estimate is NA or 0? (your logic said "not NA and not 0"?)
  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

})

test_that("Stepwise BIC forward seelction works", {

  expect_no_error({
    sel <- select_stepwise_ic(Sepal.Length ~ ., iris, direction = "forward",penalty = "BIC")
    inf <- infer_selective(sel, data = iris, nonselection = "ignore")
    capture_output(print(inf))
    tidy(inf)

    inf_conf <- infer_selective(sel, data = iris, nonselection = "confident")
    capture_output(print(inf_conf))
    tidy(inf_conf)

    inf_un <- infer_selective(sel, data = iris, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    tidy(inf_un)

  })

  # Test 1: if select == 0, estimate must be NA for ignore case
  expect_true(
    sum(inf$select == 0 & !is.na(inf$estimate)) == 0
  )


  # Test 2: confident_nulls then estimate = 0 whenever select = 0
  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  # Test 3: uncertain_nulls → estimate is NA or 0? (your logic said "not NA and not 0"?)
  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

})


test_that("Lasso min works (glmnet) ", {
  expect_no_error({
    sel_glm <- select_glmnet(Sepal.Length ~ ., iris)
    inf <- infer_selective(sel_glm, data = iris, nonselection = "ignore")
    capture_output(print(inf))
    tidy(inf)

    inf_conf <- infer_selective(sel_glm, data = iris, nonselection = "confident_nulls")
    capture_output(print(inf_conf))
    tidy(inf_conf)

    inf_un <- infer_selective(sel_glm, data = iris, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    tidy(inf_un)

  })

  # Test 1: if select == 0, estimate must be NA for ignore case
  expect_true(
    sum(inf$select == 0 & !is.na(inf$estimate)) == 0,
  )


  # Test 2: confident_nulls then estimate = 0 whenever select = 0
  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  # Test 3: uncertain_nulls → estimate is NA or 0? (your logic said "not NA and not 0"?)
  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

})

test_that("Lasso 1se works (glmnet) ", {
  expect_no_error({
    sel_glm <- select_glmnet(Sepal.Length ~ ., iris,lambda = "compact")
    inf <- infer_selective(sel_glm, data = iris, nonselection = "ignore")
    capture_output(print(inf))
    tidy(inf)

    inf_conf <- infer_selective(sel_glm, data = iris, nonselection = "confident_nulls")
    capture_output(print(inf_conf))
    tidy(inf_conf)

    inf_un <- infer_selective(sel_glm, data = iris, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    tidy(inf_un)

  })

  # Test 1: if select == 0, estimate must be NA for ignore case
  expect_true(
    sum(inf$select == 0 & !is.na(inf$estimate)) == 0,
  )


  # Test 2: confident_nulls then estimate = 0 whenever select = 0
  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  # Test 3: uncertain_nulls → estimate is NA or 0? (your logic said "not NA and not 0"?)
  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

})




####### Test HERS data ########
data("hers")
force(hers)

test_that("HERS Stepwise AIC forward selection works", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward")

    inf <- infer_selective(sel, data = hers, nonselection = "ignore")
    capture_output(print(inf))
    tidy(inf)

    inf_conf <- infer_selective(sel, data = hers, nonselection = "confident")
    capture_output(print(inf_conf))
    tidy(inf_conf)

    inf_un <- infer_selective(sel, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    tidy(inf_un)


  })

  # Test 1: if select == 0, estimate must be NA for ignore case
  expect_true(
    sum(inf$select == 0 & !is.na(inf$estimate)) == 0
  )


  # Test 2: confident_nulls then estimate = 0 whenever select = 0
  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  # Test 3: uncertain_nulls → estimate is NA or 0? (your logic said "not NA and not 0"?)
  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

})

test_that("HERS Stepwise BIC forward selection works", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward", penalty = "BIC")
    inf <- infer_upsi(sel, data = hers)
    capture_output(print(inf))
    tidy(inf)

    inf_conf <- infer_upsi(sel, data = hers, nonselection = "confident_nulls")
    capture_output(print(inf_conf))
    tidy(inf_conf)

    inf_un <- infer_upsi(sel, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    tidy(inf_un)


  })

  # Test 1: if select == 0, estimate must be NA for ignore case
  expect_true(
    sum(inf$select == 0 & !is.na(inf$estimate)) == 0
  )


  # Test 2: confident_nulls then estimate = 0 whenever select = 0
  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  # Test 3: uncertain_nulls → estimate is NA or 0? (your logic said "not NA and not 0"?)
  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

})



test_that("HERS Lasso min works (ncvreg,glmnet) ", {
  expect_no_error({
    sel_glm <- select_glmnet(hdl1 ~ ., hers)
    inf <- infer_upsi(sel_glm, data = hers)
    capture_output(print(inf))
    tidy(inf)

    inf_conf <- infer_upsi(sel_glm, data = hers, nonselection = "confident_nulls")
    capture_output(print(inf_conf))
    tidy(inf_conf)

    inf_un <- infer_upsi(sel_glm, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    tidy(inf_un)


    sel_ncv <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1,fold = sel_glm$foldid)
    inf_ncv <- infer_upsi(sel_ncv, data = hers)
    capture_output(print(inf_ncv))
    tidy(inf_ncv)

    inf_conf_ncv <- infer_upsi(sel_glm, data = hers, nonselection = "confident_nulls")
    capture_output(print(inf_conf_ncv))
    tidy(inf_conf_ncv)

    inf_un_ncv <- infer_upsi(sel_glm, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un_ncv))
    tidy(inf_un_ncv)

  })

  # Test 1: if select == 0, estimate must be NA for ignore case
  expect_true(
    sum(inf$select == 0 & !is.na(inf$estimate)) == 0,
    sum(inf_ncv$select == 0 & !is.na(inf_ncv$estimate)) == 0

  )


  # Test 2: confident_nulls then estimate = 0 whenever select = 0
  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  expect_equal(
    sum(inf_conf_ncv$select == 0 & inf_conf_ncv$estimate == 0),
    sum(inf_conf_ncv$select == 0)
  )

  # Test 3: uncertain_nulls → estimate is NA or 0? (your logic said "not NA and not 0"?)
  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

  expect_equal(
    sum(inf_un_ncv$select == 0 & !is.na(inf_un_ncv$estimate) & inf_un_ncv$estimate != 0),
    sum(inf_un_ncv$select == 0)
  )

})

test_that("HERS Lasso 1se works (ncvreg,glmnet) ", {
  expect_no_error({
    sel_glm <- select_glmnet(hdl1 ~ ., hers,lambda = "compact")
    inf <- infer_upsi(sel_glm, data = hers)
    capture_output(print(inf))
    tidy(inf)

    inf_conf <- infer_upsi(sel_glm, data = hers, nonselection = "confident_nulls")
    capture_output(print(inf_conf))
    tidy(inf_conf)

    inf_un <- infer_upsi(sel_glm, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    tidy(inf_un)


    sel_ncv <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1,fold = sel_glm$foldid,
                             lambda = "compact")
    inf_ncv <- infer_upsi(sel_ncv, data = hers)
    capture_output(print(inf_ncv))
    tidy(inf_ncv)

    inf_conf_ncv <- infer_upsi(sel_glm, data = hers, nonselection = "confident_nulls")
    capture_output(print(inf_conf_ncv))
    tidy(inf_conf_ncv)

    inf_un_ncv <- infer_upsi(sel_glm, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un_ncv))
    tidy(inf_un_ncv)

  })

  # Test 1: if select == 0, estimate must be NA for ignore case
  expect_true(
    sum(inf$select == 0 & !is.na(inf$estimate)) == 0,
    sum(inf_ncv$select == 0 & !is.na(inf_ncv$estimate)) == 0

  )


  # Test 2: confident_nulls then estimate = 0 whenever select = 0
  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  expect_equal(
    sum(inf_conf_ncv$select == 0 & inf_conf_ncv$estimate == 0),
    sum(inf_conf_ncv$select == 0)
  )

  # Test 3: uncertain_nulls → estimate is NA or 0? (your logic said "not NA and not 0"?)
  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

  expect_equal(
    sum(inf_un_ncv$select == 0 & !is.na(inf_un_ncv$estimate) & inf_un_ncv$estimate != 0),
    sum(inf_un_ncv$select == 0)
  )

})




