# Constructor for `inferrer`s

Create an `inferrer` wrapper around a post-selection inference method so
its results can be harmonized across selection algorithms. Users don't
call this directly; the `infer_*` functions do. It is exported for
writing new inference methods.

## Usage

``` r
as_inferrer(
  x,
  name,
  label = name,
  nonselection,
  inferences,
  conf.level,
  selector,
  meta = list()
)
```

## Arguments

- x:

  a slot for the main inferential object

- name:

  brief name for method

- label:

  label for method

- nonselection:

  how non-selections were/are handled

- inferences:

  a slot for post-selection inferences that should have confidence
  intervals (at least)

- conf.level:

  the confidence level used for intervals (1-`sig.level`)

- selector:

  a carried-forward `selector` object

- meta:

  a list containing important meta-information from the method

## Value

`x` with class `inferrer` prepended and the other arguments stored as
attributes.
