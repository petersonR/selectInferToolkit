# Constructor for `selector`s

Create a `selector` wrapper around a selection algorithm so it can be
used with the `selectInferToolkit` package. Users don't call this
directly; the `select_*` functions do. It is exported for writing new
selectors.

## Usage

``` r
as_selector(
  x,
  name,
  label = name,
  design,
  orig_formula,
  selected_terms,
  selected_coefs,
  default_infer,
  meta = list()
)
```

## Arguments

- x:

  an object from a selection algorithm

- name:

  name of the selector

- label:

  label of the selector (for pretty printing)

- design:

  the selector's design (internal): how the formula and data become the
  standardized predictor matrix, and the term map behind
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html)

- orig_formula:

  Original formula provided by user

- selected_terms:

  names of selected variables

- selected_coefs:

  a vector of only selected coefficients

- default_infer:

  the root string of the default infer method

- meta:

  a list containing important meta-information

## Value

`x` with class `selector` prepended and the other arguments stored as
attributes.
