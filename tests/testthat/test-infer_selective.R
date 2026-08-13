####### Test IRIS data ########
data(iris)

set.seed(123)

# Add another unbalanced factor
iris$Group <- factor(sample(c('A', 'B'), nrow(iris), replace = TRUE))

# Add a nzv variable
iris$NotUseful <- 2

# Add a binary variable
iris$BV <- rbinom(nrow(iris), 1, prob = .5)

# Add an unbalanced binary variable
iris$UBV <- rbinom(nrow(iris), 1, prob = .02)




test_that("Stepwise AIC forward selection works", {

  expect_no_error({
    sel <- select_stepwise_ic(Sepal.Length ~ ., iris, direction = "forward",
                              select_factors_together = F)

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

  # Test 3: uncertain_nulls → estimate is NA or 0?
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
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward",
                              select_factors_together = F)

    inf <- infer_selective(sel, data = hers, nonselection = "ignore")
    capture_output(print(inf))
    print(tidy(inf), n=35)

    inf_conf <- infer_selective(sel, data = hers, nonselection = "confident")
    capture_output(print(inf_conf))
    print(tidy(inf_conf), n=35)

    inf_un <- infer_selective(sel, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    print(tidy(inf_un), n=35)


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
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward",
                              select_factors_together = F,penalty = "BIC")


    inf <- infer_selective(sel, data = hers, nonselection = "ignore")
    capture_output(print(inf))
    print(tidy(inf), n=35)

    inf_conf <- infer_selective(sel, data = hers, nonselection = "confident")
    capture_output(print(inf_conf))
    print(tidy(inf_conf), n=35)

    inf_un <- infer_selective(sel, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    print(tidy(inf_un), n=35)


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


    inf <- infer_selective(sel_glm, data = hers, nonselection = "ignore")
    capture_output(print(inf))
    print(tidy(inf), n=35)

    inf_conf <- infer_selective(sel_glm, data = hers, nonselection = "confident")
    capture_output(print(inf_conf))
    print(tidy(inf_conf), n=35)

    inf_un <- infer_selective(sel_glm, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    print(tidy(inf_un), n=35)

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
    sel_glm <- select_glmnet(hdl1 ~ ., hers,lambda = "compact")


    inf <- infer_selective(sel_glm, data = hers, nonselection = "ignore")
    capture_output(print(inf))
    print(tidy(inf), n=35)

    inf_conf <- infer_selective(sel_glm, data = hers, nonselection = "confident")
    capture_output(print(inf_conf))
    print(tidy(inf_conf), n=35)

    inf_un <- infer_selective(sel_glm, data = hers, nonselection = "uncertain_nulls")
    capture_output(print(inf_un))
    print(tidy(inf_un), n=35)

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




test_that("on_mismatch controls how the fallback is reported", {
  skip_if_not_installed("selectiveInference")

  # seed 36: stepAIC keeps 3 variables, the Cp-form rule stops at 2
  set.seed(36)
  n <- 100; p <- 20
  d <- data.frame(y = rnorm(n), matrix(rnorm(n * p), n, p))
  fit <- select_stepwise_ic(y ~ ., data = d, direction = "forward",
                            penalty = "BIC")
  k_sel <- sum(names(coef(fit)) != "(Intercept)")

  # every setting reports the selector's model; only the messaging differs
  quiet <- infer_selective(fit, data = d, sigma = 1,
                           on_mismatch = "silent-fall-back")
  expect_identical(attr(quiet, "meta")$conditioning, "active")
  expect_equal(sum(tidy(quiet)$selected), k_sel + 1L)
  expect_no_warning(infer_selective(fit, data = d, sigma = 1,
                                    on_mismatch = "silent-fall-back"))

  # "stop" errors, and names the fix
  expect_error(
    infer_selective(fit, data = d, sigma = 1, on_mismatch = "stop"),
    'criterion = "cp"', fixed = TRUE)

  # the default warns and points at criterion = "cp"
  expect_identical(eval(formals(infer_selective)$on_mismatch)[1],
                   "warn-fall-back")

  # no mismatch -> no fallback, no warning, under every setting
  set.seed(11)
  d2 <- data.frame(y = rnorm(n), matrix(rnorm(n * p), n, p))
  cp <- select_stepwise_ic(y ~ ., data = d2, direction = "forward",
                           penalty = "AIC", criterion = "cp")
  for (om in c("warn-fall-back", "silent-fall-back", "stop")) {
    ii <- expect_no_warning(infer_selective(cp, data = d2, on_mismatch = om))
    expect_identical(attr(ii, "meta")$conditioning, "aic")
  }
})

test_that("selectiveInference's sigma-fallback warning is restated for this package", {
  skip_if_not_installed("selectiveInference")

  # p > n/2 so fsInf() falls back to sd(y) and warns, pointing at
  # estimateSigma() -- a function users reach through use_cv_sigma, not directly
  set.seed(3)
  n <- 40; p <- 30
  d <- data.frame(y = rnorm(n), matrix(rnorm(n * p), n, p))
  fit <- select_stepwise_ic(y ~ ., data = d, direction = "forward",
                            penalty = "AIC")

  ws <- character(0)
  withCallingHandlers(
    try(infer_selective(fit, data = d, on_mismatch = "silent-fall-back"),
        silent = TRUE),
    warning = function(w) { ws <<- c(ws, conditionMessage(w))
                            invokeRestart("muffleWarning") })

  sigma_ws <- grep("sd\\(y\\)", ws, value = TRUE)
  skip_if_not(length(sigma_ws) > 0, "fsInf did not hit its sigma fallback here")

  # restated in terms of this package's arguments...
  expect_match(sigma_ws[1], "use_cv_sigma", fixed = TRUE)
  # ...and upstream's "call estimateSigma yourself" phrasing is gone
  expect_false(any(grepl("you may want to use the estimateSigma function",
                         ws, fixed = TRUE)))
  # exactly once, not once per internal fsInf call
  expect_length(sigma_ws, 1L)

  # unrelated warnings still pass through: a cp selector given a conflicting
  # sigma warns about that, and si_sigma_warning must not swallow it.
  # selectiveInference also warns that the polyhedral constraint no longer
  # holds -- which is the point -- so collect rather than expect_warning().
  cp <- select_stepwise_ic(y ~ ., data = d, direction = "forward",
                           penalty = "AIC", criterion = "cp")
  ws2 <- character(0)
  withCallingHandlers(
    invisible(infer_selective(cp, data = d, sigma = 2,
                              on_mismatch = "silent-fall-back")),
    warning = function(w) { ws2 <<- c(ws2, conditionMessage(w))
                            invokeRestart("muffleWarning") })
  expect_true(any(grepl("differs from the value this selector selected", ws2)))
  expect_true(any(grepl("Constraint not satisfied", ws2)))
})
