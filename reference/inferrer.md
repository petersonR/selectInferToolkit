# tidy method for `inferrer` object

Returns a tibble with all candidate variables, estimates (scaled &
unscaled)

## Usage

``` r
# S3 method for class 'inferrer'
tidy(x, scale_coef = TRUE, ...)

# S3 method for class 'inferrer'
print(x, ...)
```

## Arguments

- x:

  an `inferrer`

- scale_coef:

  should scaled betas be returned, or unscaled?

- ...:

  additional parameters (not yet used)
