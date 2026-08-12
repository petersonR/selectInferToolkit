# Available methods in selectInferToolkit Package

## Overview

The `selectInferToolkit` package provides a unified interface for
**post-selection inference** (PSI), statistical inference that accounts
for model selection. Ignoring the model selection while performing
inference leads to confidence intervals that are too narrow and p-values
that are anti-conservative, as the selection and inference are performed
on the same dataset.

This vignette is designed to document which combinations of selection
method, inference method, and tuning options are currently supported by
the package. Rather than maintaining a static table by hand, the
vignette probes every combination automatically each time it is
rendered: it fits a small synthetic dataset, attempts every pipeline,
and records whether each combination succeeds, produces a warning, or
fails. The compatibility matrix and flow diagrams below therefore
reflect the state of the package at the time this vignette was built.

The probe dataset uses the “A / B / N” simulation design of Patrick
Breheny, implemented as `gen_data_abn()` in the
[hdrm](https://github.com/pbreheny/hdrm) package: `n = 150` observations
on `p = 10` predictors, arranged as two blocks of three
exchangeably-correlated variables (`rho = 0.8`) plus four independent
noise variables. Within each block the `A` variable has a nonzero
coefficient (0.3 and -0.4) while the two `B` variables are correlated
with it but have coefficients of exactly zero — precisely the
correlated-but-null predictors that selection procedures tend to pick up
by mistake. The design is reproduced in base R inside this vignette
rather than taken from `hdrm` only so that the package carries no
non-CRAN dependencies.

### Selection methods

The package currently supports four families of variable/model
selection, each configurable with tuning parameters:

| Function | Method | Key options |
|----|----|----|
| [`select_full_model()`](https://petersonr.github.io/selectInferToolkit/reference/select_full_model.md) | No selection and all variables retained | — |
| [`select_stepwise_ic()`](https://petersonr.github.io/selectInferToolkit/reference/select_stepwise_ic.md) | Forward / backward / bi-directional stepwise | `penalty` (AIC / BIC), `direction` |
| [`select_glmnet()`](https://petersonr.github.io/selectInferToolkit/reference/select_glmnet.md) | Lasso / elastic net / ridge via `glmnet` | `alpha` (0 = ridge, between 0 and 1 = elastic net, 1 = lasso), `lambda` |
| [`select_ncvreg()`](https://petersonr.github.io/selectInferToolkit/reference/select_ncvreg.md) | MCP, SCAD, or lasso via `ncvreg` | `penalty` (MCP / SCAD / lasso), `lambda` |

### Inference methods

Given a selected model object, the package offers four inference
approaches:

| Function | Method | Key options |
|----|----|----|
| [`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer.md) | Unadjusted post-selection inference (UPSI) | `nonselection` |
| [`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer.md) | Selective inference: exact conditioning on the selection event; currently requires lasso or forward stepwise selection | `nonselection` |
| [`infer_boot()`](https://petersonr.github.io/selectInferToolkit/reference/boot.md) | Bootstrap-based inference: resamples the full selection + estimation pipeline | `inference_target`, `debias`, `estimation_data` |
| [`infer_pipe()`](https://petersonr.github.io/selectInferToolkit/reference/infer.md) | PIPE: projection-based test statistics and intervals for a penalized fit (wraps [`ncvreg::intervals()`](https://pbreheny.github.io/ncvreg/reference/intervals.html)); currently requires `ncvreg` selection, and is experimental | — |

### The `nonselection` argument

For
[`infer_upsi()`](https://petersonr.github.io/selectInferToolkit/reference/infer.md)
and
[`infer_selective()`](https://petersonr.github.io/selectInferToolkit/reference/infer.md),
variables that were *not* selected present a choice about how to handle
their inference:

- **`"ignored"`** - only selected variables receive inference;
  non-selected variables are still listed in the output, but their
  estimates and CIs are returned as `NA`.
- **`"confident_nulls"`** - treat non-selected variables as exact zeros:
  point estimate and both CI limits are zero, i.e. a point mass at zero
  with no uncertainty.
- **`"uncertain_nulls"`** - aims to make inference on all variables,
  conditioning on the selected model and checking each non-selection
  against residuals of that model individually.

## Compatibility matrix

The table below summarizes which pipelines are currently functional.
Each row is one *selector flavor* - a specific model selection function
with a particular combination of tuning parameters (e.g. `select_glmnet`
with `alpha=1.0, lambda=best`). Each column group is one inference
method.

**How to read the table:**

- For `infer_selective` and `infer_upsi`, the cell shows the consensus
  result across all three `nonselection` values. If all three behave the
  same way the cell shows a single icon; if they differ a tally is shown
  (e.g. ✅×2 ❌×1).
- For `infer_boot`, results are split into two columns by
  `estimation_data` (in-sample vs out-of-sample) but collapsed across
  `inference_target` × `debias`, since currently those options do not
  affect whether the method runs successfully.
- For `infer_pipe`, there are no tuning options - the cell simply
  indicates whether the method accepts output from that selector.

**Notable constraints visible in the table:**

- `infer_selective` only works with lasso-penalized glmnet selection
  (`alpha = 1.0`) and forward stepwise selection.
- `infer_pipe` works with `select_ncvreg` but not with stepwise or
  glmnet selectors.
- `infer_upsi` and `infer_boot` are the most broadly compatible, working
  across all selector families.
- The out-of-sample `infer_boot` column is marked ⚠️ throughout: the
  method runs for every selector, but warns that out-of-sample
  estimation is experimental and needs a large sample size. Treat those
  cells as “runs, but use with care” rather than “recommended”.

### Flow diagrams

The diagrams below visualize the same information as the compatibility
matrix but as a directed graph, making it easier to trace which full
pipelines are available for a given selector.

**How to read the diagrams:**

- **Left column** - selector flavors (specific function + tuning
  parameter combination). All nodes in this column share the same color,
  indicating they belong to the same selector family.
- **Middle column** - inference methods. Each method has its own color,
  carried through to its edges and terminal nodes.
- **Right column** - supported option values for the chosen inference
  method (nonselection values for `infer_upsi`/`infer_selective`; option
  dimensions for `infer_boot`; “(no options)” for `infer_pipe`).
- **Solid edges** - the combination is fully supported (at least one
  option variant runs without any warning).
- **Dashed edges** - the combination runs, but every option variant that
  works emits a warning; see the corresponding ⚠️ cell in the matrix
  above.
- **Missing edges** - the combination is not supported and is excluded
  from the diagram.

One diagram is shown per selector family. `select_full_model` is omitted
because it has no tuning parameters, so its diagram would reduce to the
single row already shown in the matrix above.

#### `select_stepwise_ic` (penalty × direction)

**Interpretation:** All six AIC/BIC × direction combinations support
`infer_upsi` and `infer_boot`. `infer_selective` is only available for
`direction = "forward"`: the conditioning it performs is defined in
terms of a forward-stepwise path, and
[`selectiveInference::fsInf()`](https://rdrr.io/pkg/selectiveInference/man/fsInf.html)
implements that case only. `infer_pipe` is not available for stepwise
selection.

#### `select_glmnet` (alpha × lambda: ridge/elastic net/lasso × best/compact)

**Interpretation:** `infer_selective` (shown in blue) is only connected
for `alpha = 1.0` (lasso) flavors. Ridge (`alpha = 0`) and elastic net
(`alpha = 0.5`) can only be paired with `infer_upsi` or `infer_boot`.
All `lambda` options work equivalently for supported inference methods.

#### `select_ncvreg` (penalty × lambda: MCP/SCAD/lasso × best/compact)

**Interpretation:** All three penalties (MCP, SCAD, lasso) and both
`lambda` options support `infer_upsi`, `infer_boot`, and `infer_pipe`.
`infer_selective` is not available for `ncvreg` selectors — note that
this applies even to `penalty = "lasso"`, because selective inference
needs the `glmnet` parameterization of the lasso fit rather than
`ncvreg`’s.

## All probed combinations

The table below is the raw output behind the matrix and diagrams: one
row per selector flavor × inference method × option combination, with
the error or warning message where one was produced. Use the filter
boxes to narrow it down.

## For developers

This vignette is designed to update itself automatically as new methods
are added to the package. The probing logic in the `probe` chunk calls
each function via
`get(fn_name, envir=getNamespace("selectInferToolkit"))`, so any newly
exported `select_*` or `infer_*` function is discovered without editing
this file. Tuning *options*, however, are not discoverable, so a new
method with its own options needs one of the following small edits.

To add a **new selection method** with tuning parameters, add one entry
to `sel_grids` in the `candidates` chunk:

``` r

select_mymethod = expand.grid(
  option1 = c("a", "b"),
  option2 = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)
```

To add a **new inference method that uses `nonselection=`**, add its
name to `inf_fns_nonsel` in this vignette’s Rmd.

To add a **new inference method with its own option grid** (like
`infer_boot`), add a dedicated probe block in the `probe` chunk
following the pattern used for `grid_boot`.

The compatibility matrix and flow diagrams will include the new method
automatically on the next render.
