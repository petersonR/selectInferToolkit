# Compare CI lengths and discoveries across selectors and inference methods

A research helper: fits the full model, stepwise AIC and BIC, lasso and
MCP (each with `lambda = "best"` and `"compact"`), runs UPSI, the
bootstrap and (where supported) selective inference on each, and
tabulates the mean and median CI length and the number of significant
terms. Internal; call it as `selectInferToolkit:::ciratio()`.

## Usage

``` r
ciratio(
  formula,
  data,
  family = c("gaussian", "binomial", "poisson"),
  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
  conf.level = 0.95,
  B = 250,
  direction = "forward",
  select_factors_together = F,
  pad = c("zero_side", "none", "all"),
  inference_target = "selections",
  ...
)
```

## Arguments

- formula:

  a formula

- data:

  a data frame

- family:

  outcome distributional family

- nonselection:

  how UPSI and selective inference handle non-selected variables; see
  [`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer_upsi.md)

- conf.level:

  .95 by default

- B:

  number of bootstrap resamples

- direction:

  stepwise search direction: `"forward"` (default), `"backward"` or
  `"both"`

- select_factors_together:

  should categorical variables be jointly selected (stepwise)?

- pad:

  how bootstrap intervals are padded; see
  [`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md)

- inference_target:

  bootstrap inference target; see
  [`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md)

- ...:

  passed to the full-model and MCP selectors

## Value

a list of three matrices, each with one row per selector and columns
`UPSI`, `Bootstrap` and `Selective Inference` (`NA` where a method
doesn't apply). All exclude the intercept.

- avg_ci_ln:

  mean CI length

- med_ci_ln:

  median CI length

- no_sign_disc:

  number of terms with p \< 0.05 (bootstrap: CI excluding 0)
