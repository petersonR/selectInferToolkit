# Constructor for `selector`s

Create a `selector` wrapper around selection algorithms so they can be
used with the `selectInferToolkit` package. This is not a user-facing
function.

Returns predictions for `selector` objects on new data

Returns a tibble with all candidate variables, estimates (scaled &
unscaled)

Returns a vector with all selected coefficients (scaled only, includes
intercept)

Re-do the selection, possibly on new data. Does not re-do the
pre-processing; instead uses the original preprocess recipe from
`selector_obj`.

## Usage

``` r
as_selector(
  x,
  name,
  label = name,
  all_terms,
  recipe_obj,
  selected_coefs,
  default_infer,
  meta = list()
)

# S3 method for class 'selector'
predict(object, newdata = NULL, ...)

# S3 method for class 'selector'
tidy(x, scale_coef = TRUE, ...)

# S3 method for class 'selector'
print(x, ...)

# S3 method for class 'selector'
coef(object, use_native = FALSE, ...)

reselect(selector_obj, newdata)
```

## Arguments

- x:

  a `selector`

- name:

  name of the selector

- label:

  label of the selector (for pretty printing)

- all_terms:

  a slot containing names of all terms

- recipe_obj:

  preprocessor trained from recipes package

- selected_coefs:

  a vector of only selected coefficients

- default_infer:

  the root string of the default infer method

- meta:

  a list containing important meta-information

- object:

  a selector object

- newdata:

  a new data set (or same one)

- ...:

  objects passed to native function, otherwise not used.

- scale_coef:

  should scaled betas be returned, or unscaled?

- use_native:

  if true, passes call to original class `coef`

- selector_obj:

  a selector object

## Value

An S3 object (list) of class `selector` containing:' x all_terms meta
