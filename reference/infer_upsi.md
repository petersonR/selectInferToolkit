# Unadjusted post-selection inference (UPSI)

UPSI is sometimes referred to as "hybrid-OLS", but essentially we re-fit
the selected model to the data as though we never used that same data to
fit the model. It is common, easy, and ill-advised.

## Usage

``` r
infer_upsi(
  object,
  data,
  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
  conf.level = 0.95
)
```

## Arguments

- object:

  a `selector` object

- data:

  the data used to fit the selector

- nonselection:

  how to handle variables the selector did not select:

  - `"ignored"` (default): no inference; their `estimate`, `ci_low`,
    `ci_high` and `p_value` are `NA`.

  - `"confident_nulls"`: treated as exactly zero, with `estimate = 0`, a
    degenerate interval `[0, 0]` and `p_value = 1`.

  - `"uncertain_nulls"`: each non-selected variable is added, one at a
    time, to the selected model, which is refit on `data`; its estimate,
    interval and p-value come from that fit. These are not adjusted for
    selection.

  Partial names are matched (e.g. `"uncertain"`).

- conf.level:

  .95 by default

## Value

an `inferrer` object; see
[`tidy.inferrer()`](https://petersonr.github.io/selectInferToolkit/reference/inferrer-methods.md)
