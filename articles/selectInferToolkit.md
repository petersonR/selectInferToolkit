# Using the selectInferToolkit Package

## Introduction

In the era of big data and complex regression problems, researchers
often employ model selection procedures like step wise regression or
penalized methods (e.g., lasso) to identify important variables.
However, classical statistical inference assumes the model was chosen a
prior—independent of the data. When this assumption is violated,
standard statistical tests and confidence intervals can become overly
optimistic, leading to unreliable conclusions. `selectInferToolkit` aims
to provide a user-friendly framework in R to facilitate post-selection
inferential methods and model selection methods.

## Methods

### Model selection methods

For the conventional stepwise selection algorithms, we employed
“forward”, “backward” and “bidirectional” (also known as “bidirectional
elimination”) methods. Forward selection begins with an empty model and
progressively adds the most significant predictors. The process
continues until no further statistically significant improvements are
observed by some predefined criterion. Backward selection starts with a
full model that includes all predictors and systematically removes the
least significant predictors. The process stops when all remaining
predictors are statistically significant by some predefined criterion.
The stepwise selection approach allows the model to add variables
(forward selection) and remove variables (backward elimination) at each
step, based on specific criteria to determine the best-fitting model. We
used both the Akaike Information Criterion (AIC) and Bayesian
Information Criterion (BIC) for model selection with stepwise
regression.

Penalized regression methods aim to minimize the log-likelihood function
under a constraint that penalizes large absolute values of coefficients
and/or model complexity. The L1 penalty used in lasso (the sum of
absolute values of regression coefficients multiplied by the penalty
factor, $\lambda$), enables simultaneous variable selection and
coefficient estimation, making it particularly useful for
high-dimensional settings. However, one limitation of lasso is that, in
the presence of multicollinearity, it tends to select one variable from
a group of highly correlated variables and exclude the others.
Additionally, when there are highly correlated variables, the estimation
can become unstable, and lasso may not perform as well as Ridge
regression which uses L2 penalty (the sum of the squares of the
regression coefficients multiplied by their respective penalty factors).
The elastic net combines both L1 and L2 penalties, introducing an
additional penalty factor to provide more stability in model estimation
and allowing for more robust variable selection, particularly in the
presence of multicollinearity. Finally, the minimax concave penalty
(MCP) is an alternative method that produces less biased regression
coefficients than sparse models. MCP includes an additional tuning
parameter, $\gamma$, which controls the concavity of the penalty (i.e.,
how quickly the penalty decreases). Many additional forms of penalized
regression have been proposed, we focus on this set in our package.

### Target of Inference/Handling Non-selections

(under construction)

### Post-selection inference methods

#### UPSI

The first approach involves ignoring the model selection process (no
matter which model selection method is used) when making inferences on
the final selected model. The ‘unadjusted post-selection inference’
(UPSI) approach, sometimes referred to as two-stage or hybrid solution
for inference post-regularization, is to first use a stepwise/penalized
model to select variables, then fit an OLS model with the selected
variables to obtain standard errors and CIs. While, this is invalid in
most scenarios, recent research (Zhao et el.) suggests that, under
certain conditions—specifically, when the sample size is large, the true
model is sparse, and the predictors have low mutual correlations—the set
of variables selected by the lasso will converge to a deterministic set
with high probability. This we provide this method to standardize the
implementation in practice.

#### Bootstrap

The package provides two different ways to implement non-parametric
bootstrap based on target of inference.

**Bootstrap CIs for the Selected Model**

Let $s$ be a model selection procedure yielding the prime model
${\widehat{M}}_{s} \subset \{ 1,\ldots,p\}$.  
We estimate $\widehat{\beta} \mid {\widehat{M}}_{s}$, i.e., coefficients
conditional on the selected model.  
For $j \in {\widehat{M}}_{s}$, the coefficient is
${\widehat{\beta}}_{j}$; otherwise, NA.

    *Procedure:*
      - For each bootstrap iteration:  
        1. Resample data with replacement.  
        2. Apply \( s \) to the resample.  
        3. Record coefficients for \( j \in \hat{M}_s \); set to zero if missing.  
        4. Compute CIs from bootstrap quantiles.

**Bootstrap CIs for All Variables**

If inference targets all $p$ variables:

1.  *Treating non-selections as confident nulls:*
    1.  Resample data with replacement.  
    2.  Apply $s$ to the resample.  
    3.  et coefficients of non-selections to zero.  
2.  *Treating non-selections as uncertain nulls:*
    1.  Resample data with replacement.  
    2.  Apply $s$ to the resample.  
    3.  After selection, regress residuals on each non-selected variable
        individually.

Both yield bootstrap distributions for all variables, enabling CI
calculation.

There is an option to refit OLS within each iteration (essentially a
debiased bootstrap approach).

#### Selective Inference

Selective inference is a post-selection inferential technique that
focuses on making valid inferences on coefficients by conditioning on
the model selection process itself and implemented in
`selectiveInference` R package. Currently, they’re available after using
forward step wise selection or lasso regression to select the model. For
more details on this method, consult the documentation for the
`selectiveInference` R package or [its associated
paper](https://www.pnas.org/doi/10.1073/pnas.1507583112).

The rest of the document goes through several use cases of the package
on different data sets.

## General workflow for using pacakge

TBD

## Use case: HERS data

TBD. For now please see the package readme for a use-case.
