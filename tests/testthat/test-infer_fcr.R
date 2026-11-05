# tests/testthat/test-infer_fcr-real-data.R
# Real-data integration checks for BY-style FCR intervals.
# Inference remains on the selector's standardized full-model design scale.

library(testthat)

# Use only nonconstant predictors: a constant or extremely rare binary
# column can make the full-model inference unidentifiable.
data(iris)
iris_fcr <- iris[1:100, , drop = FALSE]
set.seed(123)
iris_fcr$Group <- factor(sample(c("A", "B"), nrow(iris_fcr), TRUE))
iris_fcr$BV <- rbinom(nrow(iris_fcr), 1, 0.5)
iris_fcr$Species <- NULL  # not used as response/predictor here

iris_formula <- Sepal.Length ~ Sepal.Width + Petal.Length +
  Petal.Width + Group + BV

# Check all-model-column rows are returned, even when not selected.
check_fcr_real <- function(sel, fit, dat, q = 0.05,
                           dependence = "standard", nonselection = "ignored") {
  tab <- broom::tidy(fit)
  sel_tab <- broom::tidy(sel)
  meta <- attr(fit, "meta")


  candidates <- tab$term != "(Intercept)"
  p <- sum(candidates)
  R <- sum(tab$selected[candidates] == 1)
  expect_equal(meta$p, p)
  expect_equal(meta$R, R)
  H <- if (dependence == "arbitrary") sum(1 / seq_len(p)) else 1
  expected_level <- if (R) 1 - q * R / (p * H) else NA_real_
  expect_equal(meta$adjusted_level, expected_level)

  omitted <- candidates & tab$selected == 0
  if (nonselection == "ignored") {
    for (v in c("estimate", "ci_low", "ci_high", "p_value"))
      expect_true(all(is.na(tab[[v]][omitted])))
  } else if (nonselection == "confident_nulls") {
    expect_true(all(tab$estimate[omitted] == 0))
    expect_true(all(tab$ci_low[omitted] == 0))
    expect_true(all(tab$ci_high[omitted] == 0))
    expect_true(all(tab$p_value[omitted] == 1))
  }

  # Independently refit the COMPLETE model on the selection design.
  design <- attr(sel, "design")
  X <- design_X(design, dat)
  y <- design_y(design, dat)
  family <- attr(sel, "meta")$family
  ref_fit <- fit_on_columns(X, y, colnames(X), family)
  expect_gt(stats::df.residual(ref_fit), 0)
  expect_equal(ref_fit$rank, ncol(X) + 1L)

  regular <- tidy_on_columns(ref_fit, 1 - q)
  selected_terms <- tab$term[candidates & tab$selected == 1]

  if (R) {
    adjusted <- tidy_on_columns(ref_fit, expected_level)
    ids <- match(selected_terms, tab$term)
    ref_ids <- match(selected_terms, adjusted$term)
    expect_false(anyNA(ref_ids))
    expect_equal(tab$estimate[ids], adjusted$estimate[ref_ids], tolerance = 1e-8)
    expect_equal(tab$ci_low[ids], adjusted$ci_low[ref_ids], tolerance = 1e-8)
    expect_equal(tab$ci_high[ids], adjusted$ci_high[ref_ids], tolerance = 1e-8)
    expect_equal(tab$p_value[ids], adjusted$p_value[ref_ids], tolerance = 1e-8)
  }

  if (nonselection == "uncertain_nulls" && any(omitted)) {
    ids <- which(omitted)
    ref_ids <- match(tab$term[ids], regular$term)
    expect_equal(tab$estimate[ids], regular$estimate[ref_ids], tolerance = 1e-8)
    expect_equal(tab$ci_low[ids], regular$ci_low[ref_ids], tolerance = 1e-8)
    expect_equal(tab$ci_high[ids], regular$ci_high[ref_ids], tolerance = 1e-8)
  }
  invisible(tab)
}

# IRIS: 3 directions x 2 penalties x 2 factor-grouping options.
for (direction in c("both", "forward", "backward")) {
  for (penalty in c("AIC", "BIC")) {
    for (group_factors in c(TRUE, FALSE)) {
      local({
        dir <- direction
        pen <- penalty
        grouped <- group_factors
        test_that(sprintf("IRIS FCR stepwise: %s, %s, grouped=%s",
                          dir, pen, grouped), {
          set.seed(123)
          sel <- select_stepwise_ic(
            iris_formula, iris_fcr, direction = dir, penalty = pen,
            select_factors_together = grouped
          )
          fit <- infer_fcr(sel, iris_fcr, q = 0.05)
          check_fcr_real(sel, fit, iris_fcr)
        })
      })
    }
  }
}

# IRIS: stepwise and both dependence options, with numeric CI verification.
test_that("IRIS FCR arbitrary-dependence intervals match full model", {
  sel <- select_stepwise_ic(
    iris_formula, iris_fcr, direction = "both", penalty = "BIC",
    select_factors_together = TRUE
  )
  standard <- infer_fcr(sel, iris_fcr, dependence = "standard")
  arbitrary <- infer_fcr(sel, iris_fcr, dependence = "arbitrary")
  a <- check_fcr_real(sel, arbitrary, iris_fcr, dependence = "arbitrary")
  s <- broom::tidy(standard)
  chosen <- a$selected == 1 & a$term != "(Intercept)"
  if (any(chosen)) {
    expect_true(all((a$ci_high[chosen] - a$ci_low[chosen]) >=
                    (s$ci_high[chosen] - s$ci_low[chosen]) - 1e-10))
  }
})

# IRIS: nonselection modes must keep SAME full-model targets.
test_that("IRIS FCR nonselection modes retain the full-model estimand", {
  sel <- select_stepwise_ic(
    iris_formula, iris_fcr, direction = "forward", penalty = "BIC",
    select_factors_together = TRUE
  )
  for (mode in c("ignored", "confident_nulls", "uncertain_nulls")) {
    fit <- infer_fcr(sel, iris_fcr, nonselection = mode)
    check_fcr_real(sel, fit, iris_fcr, nonselection = mode)
  }
})

# Optional MCP and SCAD checks. These test nonconvex penalized selection,
# not simply a full selector or a lasso.
test_that("IRIS FCR works after ncvreg MCP/SCAD selection", {
  skip_if_not_installed("ncvreg")
  for (penalty in c("MCP", "SCAD")) {
    set.seed(123)
    sel <- select_ncvreg(iris_formula, iris_fcr, penalty = penalty)
    fit <- infer_fcr(sel, iris_fcr, dependence = "arbitrary")
    check_fcr_real(sel, fit, iris_fcr, dependence = "arbitrary")
  }
})

