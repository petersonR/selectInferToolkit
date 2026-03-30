run_inference_suite <- function(sel, data) {

  set.seed(1)

  configs <- list(
    list(inf_target = "selections", est = "in-sample", debias = FALSE),
    list(inf_target = "all",        est = "in-sample", debias = FALSE),
    list(inf_target = "selections", est = "out-of-sample", debias = FALSE),
    list(inf_target = "all",        est = "out-of-sample", debias = FALSE),
    list(inf_target = "selections", est = "in-sample", debias = TRUE)
  )

  for (cfg in configs) {

    inf <- infer_boot(
      sel,
      data = data,
      B = 5,
      debias = cfg$debias,
      inference_target = cfg$inf_target,
      estimation_data = cfg$est
    )

    expect_no_error(tidy(inf))
  }
}

test_that("ncvreg inference pipeline works (IRIS reduced)", {

  data(iris)
  iris <- iris[1:100,]

  iris$Group <- factor(sample(c("A","B"), nrow(iris), TRUE))
  iris$BV <- rbinom(nrow(iris), 1, .5)
  iris$UBV <- rbinom(nrow(iris), 1, .02)

  # LASSO min
  sel1 <- select_ncvreg(Sepal.Length ~ ., iris, penalty = "lasso", alpha = 1)

  # MCP compact
  sel2 <- select_ncvreg(Sepal.Length ~ ., iris,
                        penalty = "MCP", alpha = 1, lambda = "compact")

  run_inference_suite(sel1, iris)
  run_inference_suite(sel2, iris)
})

test_that("glmnet inference pipeline works (IRIS reduced)", {

  data(iris)
  iris <- iris[1:100,]

  sel1 <- select_glmnet(Sepal.Length ~ ., iris)
  sel2 <- select_glmnet(Sepal.Length ~ ., iris, lambda = "compact")
  sel3 <- select_glmnet(Sepal.Length ~ ., iris, alpha = 0.5)

  run_inference_suite(sel1, iris)
  run_inference_suite(sel2, iris)
  run_inference_suite(sel3, iris)
})


test_that("ncvreg/glmnet inference works (HERS reduced)", {

  data(hers)
  force(hers)

  sel1 <- select_ncvreg(hdl1 ~ ., hers, penalty = "lasso", alpha = 1)
  sel2 <- select_glmnet(hdl1 ~ ., hers, alpha = 0.5)

  run_inference_suite(sel1, hers)
  run_inference_suite(sel2, hers)
})




