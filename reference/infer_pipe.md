# PIPE inference for `ncvreg` selectors (experimental)

Pointwise inference for penalized estimators (PIPE), via
[`ncvreg::intervals()`](https://pbreheny.github.io/ncvreg/reference/intervals.html).
Gives an estimate, interval and p-value for every candidate term,
selected or not, so there is no `nonselection` option. Currently only
available for
[`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md)
selectors.

## Usage

``` r
infer_pipe(object, data, conf.level = 0.95, ...)
```

## Arguments

- object:

  a `selector` from
  [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md)

- data:

  the data used to fit the selector

- conf.level:

  .95 by default

- ...:

  arguments passed to
  [`ncvreg::intervals()`](https://pbreheny.github.io/ncvreg/reference/intervals.html)

## Value

an `inferrer` object; see
[`tidy.inferrer()`](https://petersonr.github.io/selectInferToolkit/reference/inferrer-methods.md)
