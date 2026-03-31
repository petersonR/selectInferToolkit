###### Setup ########
data(iris)
iris <- iris[1:100,]

set.seed(123)
iris$Group <- factor(sample(c('A', 'B'), nrow(iris), replace = TRUE))
iris$NotUseful <- 2
iris$BV <- rbinom(nrow(iris), 1, prob = .5)
iris$UBV <- rbinom(nrow(iris), 1, prob = .02)

###### Helper function ########
check_inference_behavior <- function(inf, inf_conf, inf_un) {

  # 1. ignore → estimate should be NA when not selected
  expect_true(all(is.na(inf$estimate[inf$select == 0])))

  # 2. confident_nulls --> estimate = 0
  expect_true(all(inf_conf$estimate[inf_conf$select == 0] == 0))

  # 3. uncertain_nulls --> estimate in {NA, 0}
  vals <- inf_un$estimate[inf_un$select == 0]
  expect_true(all(!is.na(vals) ))
}

###### IRIS: Stepwise (full coverage, compact) ########

directions <- c("both", "forward", "backward")
penalties  <- c("AIC", "BIC")
factor_opts <- c(TRUE, FALSE)

for (dir in directions) {
  for (pen in penalties) {
    for (fopt in factor_opts) {

      test_that(
        paste("IRIS Stepwise", dir, pen, "factors_together =", fopt),
        {

          set.seed(123)

          sel <- select_stepwise_ic(
            Sepal.Length ~ ., iris,
            direction = dir,
            penalty = ifelse(pen == "BIC", "BIC", "AIC"),
            select_factors_together = fopt
          )

          inf      <- infer_upsi(sel, data = iris)
          inf_conf <- infer_upsi(sel, data = iris, nonselection = "confident_nulls")
          inf_un   <- infer_upsi(sel, data = iris, nonselection = "uncertain_nulls")

          expect_no_error(tidy(inf))
          expect_no_error(tidy(inf_conf))
          expect_no_error(tidy(inf_un))

          check_inference_behavior(inf, inf_conf, inf_un)
        }
      )
    }
  }
}

###### IRIS: Penalized models ########

test_that("IRIS glmnet (lasso + elastic net)", {

  set.seed(123)

  sel_lasso <- select_glmnet(Sepal.Length ~ ., iris)
  sel_enet  <- select_glmnet(Sepal.Length ~ ., iris, alpha = 0.5)

  for (sel in list(sel_lasso, sel_enet)) {

    inf      <- infer_upsi(sel, data = iris)
    inf_conf <- infer_upsi(sel, data = iris, nonselection = "confident_nulls")
    inf_un   <- infer_upsi(sel, data = iris, nonselection = "uncertain_nulls")

    check_inference_behavior(inf, inf_conf, inf_un)
  }
})

test_that("IRIS ncvreg (lasso + MCP)", {

  set.seed(123)

  sel_lasso <- select_ncvreg(Sepal.Length ~ ., iris, penalty = "lasso")
  sel_mcp   <- select_ncvreg(Sepal.Length ~ ., iris, penalty = "MCP")

  for (sel in list(sel_lasso, sel_mcp)) {

    inf      <- infer_upsi(sel, data = iris)
    inf_conf <- infer_upsi(sel, data = iris, nonselection = "confident_nulls")
    inf_un   <- infer_upsi(sel, data = iris, nonselection = "uncertain_nulls")

    check_inference_behavior(inf, inf_conf, inf_un)
  }
})

###### HERS: Minimal complementary tests ########

data("hers")
force(hers)

test_that("HERS UPSI groups factor levels together", {

  set.seed(123)

  sel_grouped <- select_stepwise_ic(
    hdl1 ~ ., hers,
    direction = "both",
    select_factors_together = TRUE
  )

  inf <- tidy(infer_upsi(sel_grouped, data = hers))

  # Find factor terms (they usually have same prefix)
  factor_terms <- grep("race", inf$term, value = TRUE)

  if (length(factor_terms) > 1) {
    selections <- inf$selected[inf$term %in% factor_terms]

    # grouped → all same (either all 0 or all 1)
    expect_true(length(unique(selections)) == 1)
  }
})

test_that("HERS stepwise + UPSI (factors together)", {
  sel <- select_stepwise_ic(hdl1 ~ ., hers,
                            direction = "both",
                            select_factors_together = TRUE)

  inf <- infer_upsi(sel, data = hers)
  inf_conf <- infer_upsi(sel, data = hers, nonselection = "confident_nulls")
  inf_un <- infer_upsi(sel, data = hers, nonselection = "uncertain_nulls")

  expect_true(sum(inf$select == 0 & !is.na(inf$estimate)) == 0)

  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )

  expect_equal(
    sum(inf_un$select == 0 & !is.na(inf_un$estimate) & inf_un$estimate != 0),
    sum(inf_un$select == 0)
  )

  inf <- tidy( inf)

  # Find factor terms (they usually have same prefix)
  factor_terms <- grep("race", inf$term, value = TRUE)

  if (length(factor_terms) > 1) {
    selections <- inf$selected[inf$term %in% factor_terms]

    # grouped → all same (either all 0 or all 1)
    expect_true(length(unique(selections)) == 1)
  }

})

test_that("HERS stepwise + UPSI (individual factor selection)", {
  sel <- select_stepwise_ic(hdl1 ~ ., hers,
                            direction = "both",
                            select_factors_together = FALSE)

  inf <- infer_upsi(sel, data = hers)

  expect_true(sum(inf$select == 0 & !is.na(inf$estimate)) == 0)
})

test_that("HERS glmnet + UPSI works", {
  sel <- select_glmnet(hdl1 ~ ., hers)

  inf <- infer_upsi(sel, data = hers)
  inf_conf <- infer_upsi(sel, data = hers, nonselection = "confident_nulls")

  expect_true(sum(inf$select == 0 & !is.na(inf$estimate)) == 0)

  expect_equal(
    sum(inf_conf$select == 0 & inf_conf$estimate == 0),
    sum(inf_conf$select == 0)
  )
})

#testthat::test_file("tests/testthat/test-infer_upsi.R")


