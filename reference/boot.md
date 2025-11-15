# Inference for `selector` via bootstrapping

Inference for `selector` via bootstrapping

Bootstrapping selection process

## Usage

``` r
infer_boot(
  object,
  data,
  inference_target = c("selections", "all"),
  debias = TRUE,
  estimation_data = c("in-sample", "out-of-sample"),
  conf.level = 0.95,
  type = c("paired", "residual"),
  B = 250,
  n_cores = 4,
  ...
)

boot(
  object,
  data,
  B,
  inference_target = c("selections", "all"),
  debias = TRUE,
  estimation_data = c("in-sample", "out-of-sample"),
  conf.level,
  n_cores,
  ...
)
```

## Arguments

- object:

  a `selector` object

- data:

  data must be passed to infer

- inference_target:

  is inference requested on all or selected only

- debias:

  should estimates be debiased (no, non-selections, or all)

- estimation_data:

  within a bootstrap, should in-sample or out-of-sample residuals be
  used for estimation (defaults to FALSE)

- conf.level:

  .95 by default

- type:

  what type of bootstrap (currently only `paired` supported)

- B:

  The number of bootstrap replicates.

- n_cores:

  number of cores for parallel computation

- ...:

  any additional arguments to that can be passed to fitting engine

## Value

`inferrer` s3 class with things like...

an `inferrer` object
