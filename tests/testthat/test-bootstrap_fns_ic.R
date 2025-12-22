####### Test IRIS data ########

data(iris)
iris <- iris[1:100,]

set.seed(123)

# Add another unbalanced factor
iris$Group <- factor(sample(c('A', 'B'), nrow(iris), replace = TRUE))

# Add a nzv variable
iris$NotUseful <- 2

# Add a binary variable
iris$BV <- rbinom(nrow(iris), 1, prob = .5)

# Add an unbalanced binary variable
iris$UBV <- rbinom(nrow(iris), 1, prob = .02)


test_that("AIC bi-directional, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "both")
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})

test_that("AIC bi-directional, debias=F, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "both")
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC bi-directional, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "both")
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})

test_that("AIC bi-directional, debias=T, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "both")
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC bi-directional, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "both",penalty = "BIC")
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})

test_that("BIC bi-directional, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "both",penalty = "BIC")
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})


test_that("AIC forward, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "forward")
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC forward,, debias=F, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "forward")
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC forward,, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "forward")
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})

test_that("AIC forward,, debias=T, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "forward")
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC forward seperate factors, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "forward",
                              penalty = "BIC",select_factors_together = F)
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC forward seperate factors, debias=F, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "forward",
                              penalty = "BIC",select_factors_together = F)
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC forward seperate factors, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "forward",
                              penalty = "BIC", select_factors_together = F)
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})

test_that("BIC forward seperate factors, debias=T, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "forward",
                              penalty = "BIC", select_factors_together = F)
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = T, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC  backwards seperate factors, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "backward",
                              penalty = "BIC",select_factors_together = F)
    inf <- infer_boot(sel, data = iris, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC  backwards seperate factors, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., iris, direction = "backward",
                              penalty = "AIC",select_factors_together = F)
    inf <- infer_boot(sel, data = iris, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = iris,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})


####### Test HERS data ########
data("hers")
force(hers)

test_that("AIC bi-directional, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers,  direction = "both")
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC bi-directional, debias=F, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "both")
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC bi-directional, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "both")
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC bi-directional, debias=T, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "both")
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC bi-directional, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "both",penalty = "BIC")
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})

test_that("BIC bi-directional, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "both",penalty = "BIC")
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})


test_that("AIC forward, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward")
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC forward, debias=F, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward")
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC forward, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward")
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC forward, debias=T, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward")
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC forward seperate factors, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward",
                              penalty = "BIC",select_factors_together = F)
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC forward seperate factors, debias=F, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward",
                              penalty = "BIC",select_factors_together = F)
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC forward seperate factors, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward",
                              penalty = "BIC", select_factors_together = F)
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  expect_true(
    sum(val_conf$prop_selected < 0.5 &
          (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  )

})

test_that("BIC forward seperate factors, debias=T, out-of-sample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "forward",
                              penalty = "BIC", select_factors_together = F)
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections",
                      estimation_data ="out-of-sample")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = T, inference_target ="all",
                          estimation_data ="out-of-sample")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("BIC  backwards seperate factors, debias=F, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "backward",
                              penalty = "BIC",select_factors_together = F)
    inf <- infer_boot(sel, data = hers, B = 20, debias = FALSE,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

test_that("AIC  backwards seperate factors, debias=T, insample", {

  expect_no_error({
    sel <- select_stepwise_ic(hdl1 ~ ., hers, direction = "backward",
                              penalty = "AIC",select_factors_together = F)
    inf <- infer_boot(sel, data = hers, B = 20, debias = T,  inference_target ="selections")
    vals <- tidy(inf)
    capture_output(print(inf))

    inf_all <- infer_boot(sel, data = hers,B = 20, debias = FALSE, inference_target ="all")
    capture_output(print(inf_all))
    val_conf <- tidy(inf_all)

  })

  # Test 1: if select == 0, estimate must be NA for selections case
  expect_true(
    sum(vals$selected == 0 & !is.na(vals$estimate)) == 0
  )


  # Test 2: for inference target all, variables selected less than half the time,
  # should have one of the limit as 0
  # expect_true(
  #   sum(val_conf$prop_selected < 0.5 &
  #         (val_conf$ci_low ==0 |val_conf$ci_high==0 )) == sum(val_conf$prop_selected < 0.5 )
  # )

})

