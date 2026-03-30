run_bootstrap_checks <- function(sel, data) {

  inf <- infer_boot(sel, data = data, B = 5, debias = FALSE,
                    inference_target = "selections")

  vals <- tidy(inf)

  inf_all <- infer_boot(sel, data = data, B = 5, debias = FALSE,
                        inference_target = "all")

  val_conf <- tidy(inf_all)

  expect_true(sum(vals$selected == 0 & !is.na(vals$estimate)) == 0)

  expect_no_error(tidy(inf))
  expect_no_error(tidy(inf_all))
}

####### Test IRIS data ########
test_that("bootstrap behavior (IRIS reduced suite)", {

  data(iris)
  iris <- iris[1:100,]

  iris$Group <- factor(sample(c("A", "B"), nrow(iris), replace = TRUE))
  iris$NotUseful <- 2
  iris$BV <- rbinom(nrow(iris), 1, .5)
  iris$UBV <- rbinom(nrow(iris), 1, .02)

  set.seed(123)

  # 1. baseline AIC both
  sel1 <- select_stepwise_ic(Sepal.Length ~ ., iris, direction = "both")

  # 2. BIC both
  sel2 <- select_stepwise_ic(Sepal.Length ~ ., iris,
                             direction = "both", penalty = "BIC")

  # 3. forward AIC
  sel3 <- select_stepwise_ic(Sepal.Length ~ ., iris,
                             direction = "forward")

  # 4. backward BIC + factor handling
  sel4 <- select_stepwise_ic(Sepal.Length ~ ., iris,
                             direction = "backward",
                             penalty = "BIC",
                             select_factors_together = FALSE)

  run_bootstrap_checks(sel1, iris)
  run_bootstrap_checks(sel2, iris)
  run_bootstrap_checks(sel3, iris)
  run_bootstrap_checks(sel4, iris)
})


####### Test HERS data ########
test_that("bootstrap behavior (HERS reduced suite)", {

  data(hers)
  force(hers)

  set.seed(123)

  sel1 <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward")
  sel2 <- select_stepwise_ic(hdl1 ~ ., hers,
                             direction = "backward",
                             penalty = "BIC",
                             select_factors_together = FALSE)

  run_bootstrap_checks(sel1, hers)
  run_bootstrap_checks(sel2, hers)
})



#testthat::test_file("tests/testthat/test-bootstrap_fns_ic.R")

