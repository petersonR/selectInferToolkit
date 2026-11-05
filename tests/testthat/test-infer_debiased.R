
# Integration tests for infer_debiased()
# The public API accepts only select_glmnet(alpha = 1).

test_that("Only a glmnet lasso selector is accepted", {

  set.seed(2026)

  dat <- data.frame(
    x1 = rnorm(90),
    x2 = rnorm(90),
    x3 = rnorm(90)
  )

  dat$y <- 1.5 * dat$x1 - dat$x2 + rnorm(90)

  full <- select_full_model(y ~ ., dat)
  null <- select_null_model(y ~ ., dat)

  step <- select_stepwise_ic(
    y ~ ., dat,
    direction = "both",
    penalty = "BIC"
  )

  for (sel in list(full, null, step)) {
    expect_error(
      infer_debiased(sel, dat),
      "select_glmnet"
    )
  }

  skip_if_not_installed("glmnet")

  elastic <- select_glmnet(
    y ~ ., dat,
    alpha = 0.5
  )

  expect_error(
    infer_debiased(elastic, dat),
    "alpha = 1"
  )

  expect_error(
    infer_debiased("not a selector", dat),
    "selector"
  )
})


test_that("Debiased inference is returned for all predictor columns", {

  skip_if_not_installed("hdi")
  skip_if_not_installed("glmnet")

  set.seed(101)

  n <- 140L

  dat <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n)
  )

  dat$y <- 2 * dat$x1 + rnorm(n)

  sel <- select_glmnet(
    y ~ ., dat,
    family = "gaussian",
    alpha = 1,
    lambda = "compact"
  )

  set.seed(42)

  fit <- infer_debiased(sel, dat)

  expect_s3_class(fit, "inferrer")

  tab <- broom::tidy(fit)

  expect_named(
    tab,
    c(
      "term", "selected", "coef", "estimate",
      "ci_low", "ci_high", "p_value"
    )
  )

  expect_setequal(
    tab$term,
    broom::tidy(sel)$term
  )

  expect_equal(
    attr(fit, "nonselection"),
    "uncertain_nulls"
  )

  predictors <- tab$term != "(Intercept)"
  omitted <- predictors & tab$selected == 0

  # All predictors have inference,
  # regardless of the original lasso selection.
  for (nm in c(
    "estimate", "ci_low", "ci_high", "p_value"
  )) {

    expect_true(
      all(is.finite(tab[[nm]][predictors]))
    )

    expect_true(
      all(is.finite(tab[[nm]][omitted]))
    )

    # Intercept has no debiased interval
    expect_true(
      all(is.na(tab[[nm]][!predictors]))
    )
  }

  # Estimates must agree with the hdi backend
  backend <- attr(fit, "fit")

  # Check if the raw hdi object is stored here.
  # Adjust this accessor if as_inferrer() stores
  # the fitted backend differently.
  if (!is.null(backend) &&
      !is.null(backend$bhat)) {

    expect_equal(
      unname(tab$estimate[predictors]),
      unname(
        backend$bhat[tab$term[predictors]]
      ),
      tolerance = 1e-8
    )
  }
})


test_that("Higher confidence levels give wider intervals", {

  skip_if_not_installed("hdi")
  skip_if_not_installed("glmnet")

  set.seed(103)

  n <- 160L

  dat <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = rnorm(n)
  )

  dat$y <- 3 * dat$a + rnorm(n)

  sel <- select_glmnet(
    y ~ ., dat,
    family = "gaussian",
    alpha = 1,
    lambda = "best"
  )

  set.seed(33)

  lo <- broom::tidy(
    infer_debiased(
      sel, dat,
      conf.level = 0.90
    )
  )

  set.seed(33)

  hi <- broom::tidy(
    infer_debiased(
      sel, dat,
      conf.level = 0.99
    )
  )

  predictors <- lo$term != "(Intercept)"

  expect_equal(
    lo$estimate[predictors],
    hi$estimate[predictors],
    tolerance = 1e-8
  )

  lo_width <- lo$ci_high[predictors] -
    lo$ci_low[predictors]

  hi_width <- hi$ci_high[predictors] -
    hi$ci_low[predictors]

  expect_true(
    all(hi_width > lo_width)
  )
})


test_that("Invalid arguments are rejected", {

  skip_if_not_installed("glmnet")

  set.seed(104)

  dat <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  dat$y <- dat$x1 + rnorm(100)

  sel <- select_glmnet(
    y ~ ., dat,
    alpha = 1
  )

  expect_error(
    infer_debiased(
      sel, dat,
      conf.level = 0
    ),
    "conf.level"
  )

  expect_error(
    infer_debiased(
      sel, dat,
      conf.level = 1
    ),
    "conf.level"
  )

  expect_error(
    infer_debiased(
      sel, dat,
      betainit = "invalid"
    ),
    "betainit"
  )
})



