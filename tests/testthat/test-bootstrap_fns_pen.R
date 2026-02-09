##### Test IRIS data ######

data(iris)
iris <- iris[1:100,]

set.seed(123)

skip() # tests require updating, currently skipped

# Add another unbalanced factor
iris$Group <- factor(sample(c('A', 'B'), nrow(iris), replace = TRUE))

# Add a nzv variable
#iris$NotUseful <- 2

# Add a binary variable
iris$BV <- rbinom(nrow(iris), 1, prob = .5)

# Add an unbalanced binary variable
iris$UBV <- rbinom(nrow(iris), 1, prob = .02)

x <- iris[, -which(names(iris) == "hdl1")]
y<- iris$hdl1


test_that("lasso min ncvreg  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., iris, penalty ="lasso", alpha=1)
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., iris, penalty ="lasso", alpha=1)
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("lasso 1se ncvreg  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., iris, penalty ="lasso", alpha=1,lambda = "compact")
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., iris, penalty ="lasso", alpha=1,lambda = "compact")
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("mcp min ncvreg  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., iris, penalty ="MCP", alpha=1)
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., iris, penalty ="MCP", alpha=1)
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("mcp 1se ncvreg  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., iris, penalty ="MCP", alpha=1,lambda = "compact")
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., iris, penalty ="MCP", alpha=1,lambda = "compact")
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("lasso min glmnet  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., iris)
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., iris)
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("lasso 1se glmnet  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., iris, lambda = "compact")
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <-select_glmnet(hdl1 ~ ., iris, lambda = "compact")
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})


test_that("elastic net works, one case not wokring", {
  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., iris,alpha=0.5)
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., iris,alpha=0.5)
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    # inf_outall <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
    #                          estimation_data ="out-of-sample")
    # capture_output(print(inf_outall))
    # val_conf <- tidy(inf_outall)

  })

})

test_that("elastic net compact  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., iris, lambda = "compact", alpha=0.5)
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <-select_glmnet(hdl1 ~ ., iris, lambda = "compact",alpha=0.5)
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})


######## Test HERS data ######
data("hers")
force(hers)


test_that("lasso min ncvreg  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1)
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1)
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("lasso 1se ncvreg  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1,lambda = "compact")
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="lasso", alpha=1,lambda = "compact")
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("mcp min ncvreg  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="MCP", alpha=1)
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="MCP", alpha=1)
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("mcp 1se ncvreg  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="MCP", alpha=1,lambda = "compact")
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_ncvreg(hdl1 ~ ., hers, penalty ="MCP", alpha=1,lambda = "compact")
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("lasso min glmnet  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., hers)
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., hers)
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})

test_that("lasso 1se glmnet  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., hers, lambda = "compact")
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <-select_glmnet(hdl1 ~ ., hers, lambda = "compact")
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})


test_that("elastic net works, one case not wokring", {
  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., hers,alpha=0.5)
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., hers,alpha=0.5)
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    # inf_outall <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
    #                          estimation_data ="out-of-sample")
    # capture_output(print(inf_outall))
    # val_conf <- tidy(inf_outall)

  })

})

test_that("elastic net compact  works", {
  expect_no_error({
    set.seed(1)
    sel <- select_glmnet(hdl1 ~ ., hers, lambda = "compact", alpha=0.5)
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

  expect_no_error({
    set.seed(1)
    sel <-select_glmnet(hdl1 ~ ., hers, lambda = "compact",alpha=0.5)
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

    inf_out <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                          estimation_data ="out-of-sample")
    vals <- tidy(inf_out)
    capture_output(print(inf_out))

    inf_outall <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                             estimation_data ="out-of-sample")
    capture_output(print(inf_outall))
    val_conf <- tidy(inf_outall)

  })

})
