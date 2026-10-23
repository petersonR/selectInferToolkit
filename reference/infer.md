# Post-selection inference

This function performs a `selector`'s default inference method.
Alternative inference methods may be available via `infer_*`, which you
should use instead if you want to do specific tuning.

## Usage

``` r
infer(
  object,
  data,
  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
  ...
)
```

## Arguments

- object:

  a `selector` object

- data:

  the data used to fit the selector

- nonselection:

  how to handle variables the selector did not select; see
  [`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer_upsi.md).
  Used only when the default method takes it
  ([`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer_upsi.md),
  [`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer_selective.md)).
  [`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md)
  and
  [`infer_pipe()`](https://petersonr.github.io/selectInferToolkit/reference/infer_pipe.md)
  have no `nonselection` option, so supplying it there warns and it is
  ignored.

- ...:

  additional arguments passed to the default `infer_*` method

## Value

an `inferrer` object; see
[`tidy.inferrer()`](https://petersonr.github.io/selectInferToolkit/reference/inferrer-methods.md)

## Details

The default method is set by the selector:

- [`select_stepwise_ic()`](https://petersonr.github.io/selectInferToolkit/reference/select_stepwise_ic.md),
  [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md):
  [`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md)

- [`select_glmnet()`](https://petersonr.github.io/selectInferToolkit/reference/select_glmnet.md):
  [`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer_selective.md)

- [`select_full_model()`](https://petersonr.github.io/selectInferToolkit/reference/select_full_model.md),
  [`select_null_model()`](https://petersonr.github.io/selectInferToolkit/reference/select_null_model.md):
  [`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer_upsi.md)
