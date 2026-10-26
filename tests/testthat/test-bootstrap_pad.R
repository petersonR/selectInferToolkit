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

test_that("MCP zero-side padding uses gamma * lambda_eff and spares large effects", {
  d <- make_sparse()
  sel <- select_ncvreg(y ~ ., d, penalty = "MCP")
  gamma <- sel$fit$gamma
  lam <- attr(sel, "meta")$lambda_eff

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

  # a null whose raw interval lies within the null zone (e.g. [0, 0] when
  # it is never selected) gets exactly the null zone
  inside <- none$term[slopes & none$ci_low >= -zs$null_radius &
                        none$ci_high <= zs$null_radius]
  expect_true(length(inside) > 0)
  z <- zs[zs$term %in% inside, ]
  expect_equal(z$ci_low, -z$null_radius, ignore_attr = TRUE)
  expect_equal(z$ci_high, z$null_radius, ignore_attr = TRUE)

  # uniform padding widens every slope interval by lambda on each side
  expect_equal(all$ci_low[slopes], none$ci_low[slopes] - lam)
  expect_equal(all$ci_high[slopes], none$ci_high[slopes] + lam)

  expect_equal(attr(inf_zs, "meta")$pad, "zero_side")
  expect_equal(attr(inf_zs, "meta")$lambda_eff, lam)
})

test_that("lambda_eff is the bottom of the run of lambdas giving the selected model (#34)", {
  set.seed(4)
  n <- 1000
  X <- matrix(rnorm(n * 30), n, dimnames = list(NULL, paste0("x", 1:30)))
  d <- data.frame(y = X[, 1] + rnorm(n), X)
  sel <- select_ncvreg(y ~ ., d)
  meta <- attr(sel, "meta")
  path <- sel$fit
  active <- path$beta[-1, ] != 0
  i_used <- match(meta$lambda_used, path$lambda)
  i_eff <- match(meta$lambda_eff, path$lambda)

  expect_true(i_eff >= i_used)
  expect_true(meta$lambda_eff %in% meta$lambda_seq)
  # the same model all the way down to lambda_eff, and a different one after
  for (i in i_used:i_eff) expect_identical(active[, i], active[, i_used])
  if (i_eff < length(path$lambda))
    expect_false(identical(active[, i_eff + 1], active[, i_used]))
  # with one strong effect, CV's choice sits near that effect and lambda_eff
  # near the noise level
  expect_lt(meta$lambda_eff, meta$lambda_used / 2)
  # predictions and coef() stay at the CV choice
  expect_equal(coef(sel), coef(sel$fit, lambda = meta$lambda_used)[coef(sel$fit,
               lambda = meta$lambda_used) != 0], ignore_attr = TRUE)
})

test_that("ncvreg resamples are refit at lambda_eff (#34)", {
  d <- make_sparse()
  sel <- select_ncvreg(y ~ ., d, penalty = "MCP")
  meta <- attr(sel, "meta")

  # one resample, rebuilt with a direct ncvreg() call on the hand-scaled design
  set.seed(5)
  inf <- infer_boot(sel, d, B = 1, inference_target = "all", pad = "none")
  # the resample's rows come from its own seed, the first draw after set.seed()
  set.seed(5)
  set.seed(sample.int(.Machine$integer.max, 1))
  idx <- sample(1:nrow(d), replace = TRUE)
  X <- scale(as.matrix(d[, -1]))
  fit <- ncvreg::ncvreg(X[idx, ], d$y[idx], lambda = meta$lambda_seq,
                        penalty = "MCP")
  b <- coef(fit, lambda = meta$lambda_eff)
  td <- tidy(inf)
  expect_equal(td$estimate, unname(b), tolerance = 1e-6)
})

test_that("lambda_eff with the lasso warns only when it moves the penalty", {
  d <- make_sparse()
  sel <- select_ncvreg(y ~ ., d, penalty = "lasso")
  meta <- attr(sel, "meta")
  set.seed(6)
  if (meta$lambda_eff < meta$lambda_used) {
    expect_warning(infer_boot(sel, d, B = 2, pad = "all"),
                   "untested for the lasso")
  }
  # at the bottom of its run already: nothing changes, so no warning
  meta$lambda_eff <- meta$lambda_used
  attr(sel, "meta") <- meta
  expect_no_warning(infer_boot(sel, d, B = 2, pad = "all"))
})

test_that("lambda_eff stays within the CV lambdas when cv.ncvreg trims the path", {
  path <- list(lambda = c(1, 0.5, 0.25, 0.1),
               beta = rbind(0, c(0, 1, 1, 1), c(0, 0, 0, 0)))
  # the CV list stops at 0.25 (e.g. a fold saturated at 0.1)
  expect_equal(selectInferToolkit:::ncvreg_lambda_eff(path, 0.5, c(1, 0.5, 0.25)), 0.25)
  expect_equal(selectInferToolkit:::ncvreg_lambda_eff(path, 0.5, path$lambda), 0.1)
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
  msgs <- character()
  withCallingHandlers(infer_boot(sel_lasso, d, B = 5), warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_match(msgs, "bias does not vanish", all = FALSE)
  if (attr(sel_lasso, "meta")$lambda_eff < attr(sel_lasso, "meta")$lambda_used)
    expect_match(msgs, "untested for the lasso", all = FALSE)
  # pad = "all" warns at most that resamples are refit at lambda_eff (#34)
  ws <- character()
  withCallingHandlers(infer_boot(sel_lasso, d, B = 5, pad = "all"),
                      warning = function(w) {
                        ws <<- c(ws, conditionMessage(w))
                        invokeRestart("muffleWarning")
                      })
  expect_true(all(grepl("untested for the lasso", ws)))

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
  expect_error(infer_boot(sel, d, B = 2, n_cores = 2),
               "`n_cores` was removed.*`cluster`")
})

test_that("infer_boot() rejects unknown and reserved arguments (#25)", {
  d <- make_sparse()
  sel <- select_ncvreg(y ~ ., d, penalty = "MCP")
  expect_error(infer_boot(sel, d, B = 2, debais = TRUE),
               "Unused argument\\(s\\) to infer_boot\\(\\): `debais`")
  expect_error(infer_boot(sel, d, B = 2, type = "residual"),
               "not yet implemented")
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

test_that("saturated binomial resamples fall back to the smallest lambda reached", {
  set.seed(24)
  n <- 60
  X <- matrix(rnorm(n * 10), n, dimnames = list(NULL, paste0("x", 1:10)))
  d <- data.frame(y = rbinom(n, 1, plogis(2 * X[, 1] - 1.5 * X[, 2])), X)
  sel <- suppressWarnings(select_ncvreg(y ~ ., d, family = "binomial"))

  ws <- character()
  set.seed(1)
  inf <- withCallingHandlers(
    infer_boot(sel, d, B = 50, pad = "none"),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  n_sat <- attr(inf, "meta")$n_short_path
  expect_gt(n_sat, 0)
  expect_match(ws, paste0("^", n_sat, " of 50 resamples' paths ended"), all = FALSE)
  expect_true(all(is.finite(tidy(inf)$ci_low[tidy(inf)$selected == 1])))

  # a direct reselect() on saturating data says so, and uses a larger lambda
  meta <- attr(sel, "meta")
  set.seed(1)
  idx <- NULL
  for (k in 1:200) {
    i <- sample(1:n, replace = TRUE)
    f <- suppressWarnings(ncvreg::ncvreg(X[i, ], d$y[i], family = "binomial",
                                         lambda = meta$lambda_seq))
    if (min(f$lambda) > meta$lambda_used) { idx <- i; break }
  }
  skip_if(is.null(idx), "no saturating resample found")
  ws <- character()
  re <- withCallingHandlers(reselect(sel, d[idx, ]), warning = function(w) {
    ws <<- c(ws, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_match(ws, "path ended before lambda", all = FALSE)
  expect_gt(attr(re, "meta")$lambda_used, meta$lambda_used)
  # the original data's lambda_eff isn't carried over to other data
  expect_null(attr(re, "meta")$lambda_eff)
})
