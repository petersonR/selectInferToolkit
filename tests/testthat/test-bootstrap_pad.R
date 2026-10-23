test_that("pad_intervals applies the zero-side and uniform rules", {
  res <- data.frame(
    term = c("(Intercept)", "big", "straddle", "at_zero", "near"),
    ci_low = c(-1, 0.5, -0.1, 0, -0.5),
    ci_high = c(1, 0.9, 0.2, 0, 0.1),
    null_radius = c(NA, 0.3, 0.3, 0.3, 0.3)
  )

  zs <- selectInferToolkit:::pad_intervals(res, "zero_side")
  # the intercept (NA radius) and intervals excluding 0 are untouched
  expect_equal(zs$ci_low[1:2], res$ci_low[1:2])
  expect_equal(zs$ci_high[1:2], res$ci_high[1:2])
  # intervals containing 0 are pushed out to at least +/- delta
  expect_equal(zs$ci_low[3:5], c(-0.3, -0.3, -0.5))
  expect_equal(zs$ci_high[3:5], c(0.3, 0.3, 0.3))

  all <- selectInferToolkit:::pad_intervals(res, "all")
  expect_equal(all$ci_low, c(-1, 0.2, -0.4, -0.3, -0.8))
  expect_equal(all$ci_high, c(1, 1.2, 0.5, 0.3, 0.4))

  none <- selectInferToolkit:::pad_intervals(res, "none")
  expect_equal(none[, c("ci_low", "ci_high")], res[, c("ci_low", "ci_high")])
})

make_sparse <- function(n = 200, p = 10, seed = 1) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, dimnames = list(NULL, paste0("x", 1:p)))
  data.frame(y = drop(X[, 1:2] %*% c(1, 0.5)) + rnorm(n), X)
}

test_that("MCP zero-side padding uses gamma * lambda and spares large effects", {
  d <- make_sparse()
  sel <- select_ncvreg(y ~ ., d, penalty = "MCP")
  gamma <- sel$fit$gamma
  lam <- attr(sel, "meta")$lambda_used

  set.seed(2)
  inf_zs <- infer_boot(sel, d, B = 50, inference_target = "all")
  set.seed(2)
  inf_none <- infer_boot(sel, d, B = 50, inference_target = "all", pad = "none")
  set.seed(2)
  inf_all <- infer_boot(sel, d, B = 50, inference_target = "all", pad = "all")
  zs <- tidy(inf_zs); none <- tidy(inf_none); all <- tidy(inf_all)

  slopes <- zs$term != "(Intercept)"
  expect_equal(zs$null_radius[slopes], rep(gamma * lam, sum(slopes)))
  expect_equal(all$null_radius[slopes], rep(lam, sum(slopes)))
  expect_false("null_radius" %in% names(none))
  expect_true(is.na(zs$null_radius[!slopes]))

  # the strong signal's interval excludes 0, so it is not padded
  expect_equal(zs[zs$term == "x1", c("ci_low", "ci_high")],
               none[none$term == "x1", c("ci_low", "ci_high")])

  # a never-selected null gets exactly the null zone instead of [0, 0]
  never <- none$term[slopes & none$ci_low == 0 & none$ci_high == 0]
  expect_true(length(never) > 0)
  z <- zs[zs$term %in% never, ]
  expect_equal(z$ci_low, -z$null_radius, ignore_attr = TRUE)
  expect_equal(z$ci_high, z$null_radius, ignore_attr = TRUE)

  # uniform padding widens every slope interval by lambda on each side
  expect_equal(all$ci_low[slopes], none$ci_low[slopes] - lam)
  expect_equal(all$ci_high[slopes], none$ci_high[slopes] + lam)

  expect_equal(attr(inf_zs, "meta")$pad, "zero_side")
})

test_that("null_radius is unscaled along with the intervals", {
  d <- make_sparse()
  d$x3 <- d$x3 * 10
  sel <- select_ncvreg(y ~ ., d, penalty = "MCP")
  set.seed(3)
  inf <- infer_boot(sel, d, B = 20, inference_target = "all")
  sc <- tidy(inf)
  un <- tidy(inf, scale_coef = FALSE)
  expect_equal(un$null_radius[un$term == "x3"],
               sc$null_radius[sc$term == "x3"] / sd(d$x3))
})

test_that("stepwise padding uses sqrt(k) times the full-model SE", {
  d <- make_sparse()
  sel <- select_stepwise_ic(y ~ ., d, penalty = "BIC")
  set.seed(4)
  res <- tidy(infer_boot(sel, d, B = 10))

  Xs <- as.data.frame(scale(d[, -1]))
  se <- summary(lm(d$y ~ ., data = Xs))$coefficients[-1, "Std. Error"]
  expect_equal(res$null_radius[-1], unname(sqrt(log(nrow(d))) * se),
               tolerance = 1e-6)

  # n <= p + 1 has no full-model SE
  d_small <- d[1:11, ]
  sel_small <- select_stepwise_ic(y ~ ., d_small, penalty = "AIC")
  expect_error(infer_boot(sel_small, d_small, B = 2), "n > p \\+ 1")
  expect_no_error(infer_boot(sel_small, d_small, B = 2, pad = "none"))
})

test_that("padding warns outside its validated settings", {
  d <- make_sparse()
  sel_lasso <- select_ncvreg(y ~ ., d, penalty = "lasso")
  expect_warning(infer_boot(sel_lasso, d, B = 5), "lasso")
  expect_no_warning(infer_boot(sel_lasso, d, B = 5, pad = "all"))

  d$y <- as.numeric(d$y > 0)
  sel_bin <- select_ncvreg(y ~ ., d, family = "binomial", penalty = "MCP")
  expect_warning(infer_boot(sel_bin, d, B = 5), "gaussian")
  expect_no_warning(infer_boot(sel_bin, d, B = 5, pad = "none"))
})

test_that("removed infer_boot() arguments error instead of being ignored", {
  d <- make_sparse()
  sel <- select_ncvreg(y ~ ., d, penalty = "MCP")
  expect_error(infer_boot(sel, d, B = 2, debias = TRUE), "`debias` was removed")
  expect_error(infer_boot(sel, d, B = 2, estimation_data = "in-sample"),
               "`estimation_data` was removed")
})

test_that("infer_boot() rejects unknown and reserved arguments (#25)", {
  d <- make_sparse()
  sel <- select_ncvreg(y ~ ., d, penalty = "MCP")
  expect_error(infer_boot(sel, d, B = 2, debais = TRUE),
               "Unused argument\\(s\\) to infer_boot\\(\\): `debais`")
  expect_error(infer_boot(sel, d, B = 2, type = "residual"),
               "not yet implemented")
  expect_warning(infer_boot(sel, d, B = 2, n_cores = 2), "no effect")
  expect_false("boot" %in% getNamespaceExports("selectInferToolkit"))
})

test_that("null_radius is only reported when padding can change an interval", {
  d <- make_sparse()
  sel <- select_full_model(y ~ ., d)
  set.seed(3)
  inf <- infer_boot(sel, d, B = 5)
  expect_false("null_radius" %in% names(tidy(inf)))
  expect_false("null_radius" %in% names(tidy(inf, scale_coef = FALSE)))
})
