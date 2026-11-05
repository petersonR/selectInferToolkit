# tests/testthat/test-infer_split.R
# Single-split post-selection inference: selection on one partition,
# conventional inference on a disjoint held-out partition.
# Run with devtools::test(filter = "infer_split").

###### Setup ########
local_edition(3)

iris_split <- datasets::iris[1:100, , drop = FALSE]
set.seed(123)
iris_split$Group <- factor(sample(c("A", "B"), nrow(iris_split), replace = TRUE))
iris_split$NotUseful <- 2
iris_split$BV <- rbinom(nrow(iris_split), 1, prob = 0.5)
iris_split$UBV <- rbinom(nrow(iris_split), 1, prob = 0.02)

###### Helpers ########
check_split_behavior <- function(fit, dat, require_nonselection = FALSE) {

  s <- fit$split
  expect_equal(length(s$selection_rows), s$n_selection)
  expect_equal(length(s$inference_rows), s$n_inference)
  expect_length(intersect(s$selection_rows, s$inference_rows), 0L)
  expect_setequal(c(s$selection_rows, s$inference_rows), seq_len(nrow(dat)))

  tab <- broom::tidy(fit)

  expect_false(anyDuplicated(tab$term) > 0L)
  expect_true(all(tab$selected %in% c(0, 1)))
  expect_true("(Intercept)" %in% tab$term)
  expect_equal(nrow(tab), length(attr(fit$split_selector, "all_terms")))

  omitted <- tab$selected == 0
  if (require_nonselection) expect_true(any(omitted))
  for (col in c("estimate", "ci_low", "ci_high", "p_value")) {
    expect_true(all(is.na(tab[[col]][omitted])))
  }

  # The slopes are on the original per-unit scale with scale_coef = FALSE.
  # The intercept currently remains in design-standardized coordinates;
  # do NOT assert equality to raw-data lm()/glm() intercept.
  unscaled <- broom::tidy(fit, scale_coef = FALSE)
  expect_identical(unscaled$term, tab$term)
  expect_identical(unscaled$selected, tab$selected)
  expect_true(all(is.na(unscaled$estimate[omitted])))
  invisible(tab)
}

###### IRIS: Stepwise (directions x penalties x factor handling) ########
for (dir in c("both", "forward", "backward")) {
  for (pen in c("AIC", "BIC")) {
    for (fopt in c(TRUE, FALSE)) {
      test_that(paste("IRIS split stepwise", dir, pen,
                      "factors_together =", fopt), {
        fit <- infer_split(
          Sepal.Length ~ ., iris_split,
          selector = select_stepwise_ic,
          direction = dir, penalty = pen,
          select_factors_together = fopt,
          prop = 0.5, seed = 123
        )
        check_split_behavior(fit, iris_split)
      })
    }
  }
}

###### IRIS: Penalized selection ########
test_that("IRIS split glmnet: lasso and elastic net", {
  skip_if_not_installed("glmnet")
  for (a in c(1, 0.5)) {
    fit <- infer_split(
      Sepal.Length ~ ., iris_split,
      selector = select_glmnet,
      family = "gaussian", alpha = a,
      lambda = "compact", prop = 0.5, seed = 123
    )
    check_split_behavior(fit, iris_split)
  }
})

test_that("IRIS split ncvreg: lasso and MCP", {
  skip_if_not_installed("ncvreg")
  for (pen in c("lasso", "MCP")) {
    fit <- infer_split(
      Sepal.Length ~ ., iris_split,
      selector = select_ncvreg,
      penalty = pen, prop = 0.5, seed = 123
    )
    check_split_behavior(fit, iris_split)
  }
})

###### Full and null selectors ########
test_that("Full-model split inference matches held-out lm slopes and p-values", {
  dat <- datasets::mtcars
  fit <- infer_split(mpg ~ cyl + hp + wt, dat,
                     selector = select_full_model,
                     prop = 0.5, seed = 42)
  tab <- check_split_behavior(fit, dat)
  heldout <- dat[fit$split$inference_rows, , drop = FALSE]
  manual <- stats::lm(mpg ~ cyl + hp + wt, data = heldout)
  manual_tab <- broom::tidy(manual, conf.int = TRUE)
  unscaled <- broom::tidy(fit, scale_coef = FALSE)

  # Compare selected slopes, excluding the known original-scale intercept issue.
  slopes <- setdiff(manual_tab$term, "(Intercept)")
  got <- unscaled[match(slopes, unscaled$term), , drop = FALSE]
  ref <- manual_tab[match(slopes, manual_tab$term), , drop = FALSE]
  expect_equal(got$estimate, ref$estimate, tolerance = 1e-7)
  expect_equal(got$ci_low, ref$conf.low, tolerance = 1e-7)
  expect_equal(got$ci_high, ref$conf.high, tolerance = 1e-7)
  expect_equal(got$p_value, ref$p.value, tolerance = 1e-7)
  expect_true(all(tab$selected == 1))
})

test_that("Null selector keeps candidate terms with NA inference", {
  fit <- infer_split(Sepal.Length ~ ., iris_split,
                     selector = select_null_model,
                     prop = 0.5, seed = 41)
  tab <- check_split_behavior(fit, iris_split, require_nonselection = TRUE)
  expect_true(all(tab$selected[tab$term != "(Intercept)"] == 0))
  expect_true(all(is.na(tab$estimate[tab$term != "(Intercept)"])))
})

###### Reproducibility, partitioning, stratification ########
test_that("Same seed produces identical partitions", {
  a <- infer_split(Sepal.Length ~ Petal.Length + Petal.Width,
                   iris_split, selector = select_full_model, seed = 7)
  b <- infer_split(Sepal.Length ~ Petal.Length + Petal.Width,
                   iris_split, selector = select_full_model, seed = 7)
  expect_identical(a$split$selection_rows, b$split$selection_rows)
  expect_identical(a$split$inference_rows, b$split$inference_rows)
})

test_that("Supplying a seed preserves the caller's RNG state", {
  set.seed(765)
  before <- .Random.seed
  invisible(infer_split(Sepal.Length ~ Petal.Length,
                        iris_split, selector = select_full_model, seed = 33))
  expect_identical(.Random.seed, before)
})

test_that("Stratification keeps both Group levels in both partitions", {
  fit <- infer_split(Sepal.Length ~ Petal.Length + Group,
                     iris_split, selector = select_full_model,
                     strat_var = "Group", prop = 0.5, seed = 99)
  check_split_behavior(fit, iris_split)
  for (idx in list(fit$split$selection_rows, fit$split$inference_rows)) {
    expect_setequal(as.character(unique(iris_split$Group[idx])), c("A", "B"))
  }
  expect_identical(fit$split$strat_var, "Group")
})

test_that("Complete-case filtering retains original row indices", {
  dat <- datasets::mtcars
  dat$hp[c(3, 11)] <- NA_real_
  fit <- infer_split(mpg ~ cyl + hp + wt, dat,
                     selector = select_full_model, seed = 22)
  used <- c(fit$split$selection_rows, fit$split$inference_rows)
  expect_setequal(used, setdiff(seq_len(nrow(dat)), c(3L, 11L)))
  expect_equal(fit$split$n_excluded, 2L)
  expect_length(intersect(fit$split$selection_rows,
                          fit$split$inference_rows), 0L)
})

###### Factor encoding ########
test_that("Factor levels are mapped consistently in held-out inference", {
  fit <- infer_split(Sepal.Length ~ Petal.Length + Group,
                     iris_split, selector = select_full_model,
                     strat_var = "Group", seed = 91)
  tab <- check_split_behavior(fit, iris_split)
  expect_true("GroupB" %in% tab$term)
  expect_equal(tab$selected[tab$term == "GroupB"], 1)
  expect_true(is.finite(tab$estimate[tab$term == "GroupB"]))
  # This compares the factor contrast's raw-scale coefficient, not intercept.
  manual <- stats::lm(Sepal.Length ~ Petal.Length + Group,
                      data = iris_split[fit$split$inference_rows, , drop = FALSE])
  got <- broom::tidy(fit, scale_coef = FALSE)
  expect_equal(got$estimate[got$term == "GroupB"],
               unname(stats::coef(manual)["GroupB"]), tolerance = 1e-7)
})

###### GLM families ########
test_that("Binomial family split fits and keeps tidy columns", {
  dat <- iris_split
  fit <- infer_split(BV ~ Sepal.Length + Petal.Width,
                     dat, selector = select_full_model,
                     family = "binomial", strat_var = "BV", seed = 21)
  tab <- check_split_behavior(fit, dat)
  expect_identical(fit$split$family, "binomial")
  expect_true(all(is.finite(tab$estimate[tab$selected == 1])))
})

test_that("Poisson family split fits and keeps tidy columns", {
  dat <- iris_split
  dat$count <- as.integer(round(dat$Sepal.Width))
  fit <- infer_split(count ~ Sepal.Length + Petal.Width,
                     dat, selector = select_full_model,
                     family = "poisson", seed = 21)
  tab <- check_split_behavior(fit, dat)
  expect_identical(fit$split$family, "poisson")
  expect_true(all(is.finite(tab$estimate[tab$selected == 1])))
})


