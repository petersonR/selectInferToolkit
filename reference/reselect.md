# Re-run a selection on new data

Re-does the selection, possibly on new data, with the same settings as
the original. The pre-processing is not re-estimated: the design of
`selector_obj` (its columns, factor levels, centers and scales) is
applied to `newdata`. This is what
[`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/infer_boot.md)
calls on each resample.

## Usage

``` r
reselect(selector_obj, newdata)
```

## Arguments

- selector_obj:

  a `selector`

- newdata:

  a data frame (or the same one)

## Value

a new `selector`
