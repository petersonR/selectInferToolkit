# Constructor for `inferrer`s

Create an `inferrer` wrapper around post-selection inference algorithms
so they can be used with the `selectInferToolkit` package and can
harmonize results across various selection algorithms (`inferrer`s).
This is not a user-facing function.

This function performs a `selector`'s default inference method.
Alternative inference methods may be available via `infer_*`, which you
should use instead if you want to do specific tuning.

A wrapper for the `selectiveInference` functions on `selector` objects

UPSI is sometimes referred to as "hybrid-OLS", but essentially we re-fit
the selected model to the data as though we never used that same data to
fit the model. It is common, easy, and ill-advised.

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

infer(
  object,
  data,
  nonselection = c("ignored", "uncertain_nulls", "confident_nulls"),
  ...
)

infer_pipe(object, data, conf.level = 0.95, ...)

infer_selective(
  object,
  data,
  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
  conf.level = 0.95,
  use_cv_sigma = FALSE,
  ...
)

infer_upsi(
  object,
  data,
  nonselection = c("ignored", "confident_nulls", "uncertain_nulls"),
  conf.level = 0.95
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

  A character string specifying how to handle variables not selected by
  model selection procedure. One of "ignored", "confident_nulls" or
  "uncertain_nulls" supported

- inferences:

  a slot for post-selection inferences that should have confidence
  intervals (at least)

- conf.level:

  .95 by default

- selector:

  a carried-forward `selector` object

- meta:

  a list containing important meta-information from the method

- object:

  a `selector` object

- data:

  data must be passed to `infer` methods

- ...:

  arguments passed to `selectiveInference` function(s)

- use_cv_sigma:

  estimate Sigma via CV (if FALSE, uses SI defaults)

## Value

An S3 object (list) of class `inferrer`

A list of class `infer_*` containing:'

- ci_avg_ratio:

  Average CI length across all variables in model

- ci_median_ratio:

  median CI length across all variables in model

- nonselection:

  method chosen to deal with non selection

- infmethod:

  Inference method chosen

- selection_method:

  Stepwsie,returned for selector_stepwise_ic class only

- direction:

  the mode of step wise search, returned for selector_stepwise_ic class
  only

- penalty:

  penalty used (AIC or BIC), returned for selector_stepwise_ic class
  only

- lambda:

  selected lambda for inference , either "lambda.min" or "lambda.1se";
  returned for selector_pen class only

- alpha:

  selected alpha for inference, returned for selector_pen class only

- B:

  The number of bootstrap replicates used (only for bootstrap selection
  method)

an `inferror` object

`inferrer` object

`inferrer` object
