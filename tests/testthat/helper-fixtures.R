# Shared fixtures for the characterization (golden), oracle, and edge-case
# tests. Kept small so the whole set stays fast.

# Compare term names across naming conventions (backticks, spaces, make.names)
norm_names <- function(x) {
  x <- make.names(gsub("`", "", x))
  x[x == "X.Intercept."] <- "(Intercept)"
  x
}

fixture_numeric <- function(n = 60, seed = 101) {
  set.seed(seed)
  X <- matrix(rnorm(n * 6), n, dimnames = list(NULL, paste0("x", 1:6)))
  data.frame(y = drop(X[, 1:2] %*% c(2, 1)) + rnorm(n), X)
}

# Factor levels here are deliberately ones the package already handles
# (syntactic names and names with spaces); problem levels live in
# test-edge_factors.R
fixture_mixed <- function(n = 80, seed = 102) {
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n),
    g3 = factor(sample(c("lo", "mid", "hi"), n, TRUE),
                levels = c("lo", "mid", "hi")),
    sp = factor(sample(c("much less", "about same", "more"), n, TRUE)),
    bin = factor(sample(c("no", "yes"), n, TRUE))
  )
  eta <- 1.5 * d$x1 - d$x2 + 1.2 * (d$g3 == "hi") + 0.8 * (d$sp == "more")
  d$y <- eta + rnorm(n)
  d$y01 <- rbinom(n, 1, plogis(eta - mean(eta)))
  d
}

# The raw design the package should reproduce: treatment-coded model matrix
raw_design <- function(formula, data) {
  X <- model.matrix(formula, data)[, -1, drop = FALSE]
  colnames(X) <- norm_names(colnames(X))
  X
}

# Every column centered and divided by its SD, as select_glmnet(),
# select_ncvreg() and select_full_model() do
std_design <- function(formula, data) scale(raw_design(formula, data))

# Name-agnostic consistency check: the nonzero slopes in coef() and the
# selected slopes in tidy() must be the same set of numbers. This is the
# invariant issue #29 breaks.
coef_tidy_consistent <- function(sel) {
  cf <- coef(sel)
  cf <- cf[names(cf) != "(Intercept)" & !is.na(cf) & cf != 0]
  td <- tidy(sel)
  td <- td[td$term != "(Intercept)" & td$selected == 1, ]
  length(cf) == nrow(td) &&
    isTRUE(all.equal(sort(unname(cf)), sort(td$coef), tolerance = 1e-8))
}

# A test of a known bug: skip while it fails, fail loudly once it passes, so
# the marker gets removed when the bug is fixed
expect_known_bug <- function(ok, issue) {
  if (isTRUE(ok))
    testthat::fail(paste0("Known bug (", issue, ") now passes; ",
                          "remove the expect_known_bug() marker."))
  else
    testthat::skip(paste0("Known bug: ", issue))
}

# Plain data frame with normalized term names, for snapshotting
golden <- function(x, digits = 8) {
  x <- as.data.frame(x)
  if ("term" %in% names(x)) x$term <- norm_names(x$term)
  num <- vapply(x, is.numeric, logical(1))
  x[num] <- lapply(x[num], signif, digits = digits)
  x
}
