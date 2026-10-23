# Edge cases in predictor types and factor levels. The core check is
# coef_tidy_consistent(): every nonzero coefficient must show up in tidy() as
# selected with the same value. Combinations that are currently broken are
# marked with expect_known_bug(), which turns into a failure once they pass.

edge_data <- function(f_values, n = 80, seed = 201) {
  set.seed(seed)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$f <- f_values(n)
  # a strong signal on the factor so that selectors keep it
  d$y <- d$x1 + 2 * as.numeric(as.factor(d$f)) + rnorm(n)
  d
}

edge_types <- list(
  syntactic = function(n) factor(sample(c("lo", "mid", "hi"), n, TRUE)),
  spaces    = function(n) factor(sample(c("much less", "same", "more"), n, TRUE)),
  punct     = function(n) factor(sample(c("a-b", "c/d", "e+f"), n, TRUE)),
  digit     = function(n) factor(sample(c("0", "1"), n, TRUE)),
  character = function(n) sample(c("p", "q"), n, TRUE),
  logical   = function(n) sample(c(TRUE, FALSE), n, TRUE),
  empty_lev = function(n) factor(sample(c("a", "b"), n, TRUE),
                                 levels = c("a", "b", "c"))
)

edge_selectors <- list(
  full     = function(d) select_full_model(y ~ ., d),
  stepwise = function(d) select_stepwise_ic(y ~ ., d, direction = "backward"),
  together = function(d) select_stepwise_ic(y ~ ., d, direction = "backward",
                                            select_factors_together = TRUE),
  glmnet   = function(d) { set.seed(1); select_glmnet(y ~ ., d) },
  ncvreg   = function(d) { set.seed(1); select_ncvreg(y ~ ., d) }
)

# Known failures as of this writing, "type/selector" -> description (none
# at present; add entries here, and they turn into failures once fixed)
known_bugs <- character(0)

for (type in names(edge_types)) {
  for (s in names(edge_selectors)) {
    test_that(paste0("coef() and tidy() agree: ", type, " / ", s), {
      d <- edge_data(edge_types[[type]])
      ok <- tryCatch(
        suppressWarnings({
          sel <- edge_selectors[[s]](d)
          coef_tidy_consistent(sel) &&
            # the factor carries real signal; dropping it is a failure too
            any(grepl("^f", names(coef(sel)))) &&
            all(is.finite(predict(sel, d)))
        }),
        error = function(e) FALSE)
      key <- paste0(type, "/", s)
      if (key %in% names(known_bugs))
        expect_known_bug(ok, known_bugs[[key]])
      else
        expect_true(ok)
    })
  }
}

test_that("logical predictors act as a 0/1 indicator named xTRUE", {
  d <- edge_data(edge_types$logical)
  ref <- coef(lm(y ~ ., d))
  for (sel in list(select_full_model(y ~ ., d),
                   select_stepwise_ic(y ~ ., d, direction = "backward"),
                   { set.seed(1); select_glmnet(y ~ ., d, lambda = "compact") })) {
    td <- tidy(sel, scale_coef = FALSE)
    expect_true("fTRUE" %in% td$term[td$selected == 1])
    # predict() converts logical new data through the recipe
    expect_true(all(is.finite(predict(sel, d[1:5, ]))))
  }
  # with no selection, the unscaled slope is the lm() coefficient
  td <- tidy(select_full_model(y ~ ., d), scale_coef = FALSE)
  expect_equal(td$coef[td$term == "fTRUE"], unname(ref["fTRUE"]))
})

test_that("a single-level factor errors rather than returning nonsense", {
  d <- edge_data(function(n) factor(rep("a", n)))
  expect_error(select_full_model(y ~ ., d))
  expect_error(select_stepwise_ic(y ~ ., d))
})

test_that("reselect() on a resample missing a level keeps the design fixed", {
  d <- edge_data(edge_types$syntactic)
  sel <- select_full_model(y ~ ., d)
  part <- d[d$f != "mid", ]   # "hi" is the reference level
  rsel <- reselect(sel, part)
  expect_equal(tidy(rsel)$term, tidy(sel)$term)

  set.seed(3)
  sel_g <- select_glmnet(y ~ ., d)
  set.seed(3)
  expect_no_error(rsel_g <- reselect(sel_g, part))
  expect_equal(tidy(rsel_g)$term, tidy(sel_g)$term)

  # the absent level's dummy cannot be estimated, so it is not selected,
  # and predictions stay finite
  expect_equal(tidy(rsel)$selected[tidy(rsel)$term == "fmid"], 0)
  expect_true(all(is.finite(predict(rsel, d))))

  # same for a stepwise fit that keeps the factor
  sel_s <- select_stepwise_ic(y ~ ., d, direction = "backward")
  rsel_s <- reselect(sel_s, part)
  expect_false(anyNA(coef(rsel_s)))
  expect_true(all(is.finite(predict(rsel_s, d))))
})

test_that("predict() with a level unseen in training errors", {
  d <- edge_data(edge_types$syntactic)
  sel <- select_full_model(y ~ ., d)
  new <- d[1:3, ]
  new$f <- factor(c("lo", "mid", "new"))
  expect_error(predict(sel, new), "appeared when it was fit")
})

test_that("digit-leading levels keep their model.matrix() names (#29)", {
  d <- edge_data(edge_types$digit)
  for (sel in list(select_full_model(y ~ ., d),
                   select_stepwise_ic(y ~ ., d, direction = "backward"),
                   { set.seed(1); select_glmnet(y ~ ., d) })) {
    td <- tidy(sel)
    expect_equal(td$term, c("(Intercept)", "x1", "x2", "f1"))
    expect_equal(td$selected[td$term == "f1"], 1)
  }
})

test_that("ordered factors error, asking for a conversion", {
  d <- edge_data(function(n) factor(sample(c("lo", "mid", "hi"), n, TRUE),
                                    levels = c("lo", "mid", "hi"),
                                    ordered = TRUE))
  for (s in names(edge_selectors))
    expect_error(edge_selectors[[s]](d), "Ordered factor", info = s)
})

test_that("missing values error in every selector", {
  d <- edge_data(edge_types$syntactic)
  d$x2[3] <- NA
  for (s in names(edge_selectors))
    expect_error(edge_selectors[[s]](d), "Missing values in `x2`", info = s)
  d <- edge_data(edge_types$syntactic)
  d$y[3] <- NA
  expect_error(select_full_model(y ~ ., d), "Missing values in `y`")

  # but predict() gives NA for incomplete rows of new data
  sel <- select_full_model(y ~ ., edge_data(edge_types$syntactic))
  new <- d[1:3, ]
  new$x1[2] <- NA
  expect_equal(is.na(predict(sel, new)), c(FALSE, TRUE, FALSE))
})

test_that("interactions and in-formula transforms work", {
  d <- edge_data(edge_types$syntactic)
  d$x3 <- exp(rnorm(nrow(d)))
  sel <- select_full_model(y ~ x1 * f + log(x3) + poly(x2, 2), d)
  td <- tidy(sel, scale_coef = FALSE)
  expect_true(all(c("x1:flo", "log(x3)", "poly(x2, 2)2") %in% td$term))

  # slopes not involved in an interaction match lm(); main effects within
  # one are at the mean of the other variable, as numeric variables are
  # centered before interactions are formed
  ref <- coef(lm(y ~ x1 * f + log(x3) + poly(x2, 2), d))
  expect_equal(td$coef[td$term == "log(x3)"], unname(ref["log(x3)"]))
  expect_equal(td$coef[td$term == "x1:flo"], unname(ref["x1:flo"]))
  expect_equal(predict(sel, d), unname(predict(lm(y ~ x1 * f + log(x3) +
                                                   poly(x2, 2), d))))
  # poly() on new data uses the training basis
  expect_equal(predict(sel, d[1:5, ]), predict(sel, d)[1:5])
})
