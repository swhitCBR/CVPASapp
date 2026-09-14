---
output:
  word_document: default
  html_document:
    keep_md: true
  pdf_document: default
editor_options: 
  markdown: 
---

## Model Selection

A standardized updated model selection procedure was applied uniformly across all component models.

### Candidate sets

Let $\mathcal{M}^{[g]}$ represent the full set of candidate models considered for sub-model $g$ and $\mathcal{X}^{[g]}_{\mathcal{M}}$ represent the full design matrix. The design matrix has rows $i=1,2,\dots N^{[g]}$ corresponding to the original observations and columns $a^{[g]}=1,2,\dots,A_{\mathcal{M}}^{[g]}$, containing the set of indicator variables and covariates required to specify the fixed-effect portions ($\beta$s) of $\mathcal{M}^{[g]}$. The first column of $\mathcal{X}^{[g]}_{\mathcal{M}}$ contains only $1$s to represent a fixed intercept term. All candidate models for model component $g$ are defined in relation to $\mathcal{X}^{[g]}_{\mathcal{M}}$ based on a binary inclusion matrix, $\mathcal{{I}}^{[g]}$, with dimensions $B_{\mathcal{M}}^{[g]} \times A_{\mathcal{M}}^{[g]}$, where each row is a vector of $1$s and $0$s, denoted $\mathbf{I}^{[g]}_{j}$ (for $j=1,2,\dots,B_{\mathcal{M}}^{[g]}$), that uniquely identifies the terms used in the candidate model. Individual candidate models are denoted $M^{[g]}_{j}$ (for $M^{[g]}_{j}\in\mathcal{M}^{[g]}_{}$), with $\mathbf{X}^{[g]}_{\mathcal{j}}$ representing the corresponding subset of the full design matrix used to fit model $M^{[g]}_{j}$. The value $B_{\mathcal{M}}^{[g]}$ is the number of candidate models for model component $g$. Row sums of $\mathbf{\mathcal{I}}^{[g]}_\mathcal{M}$ define the number of fixed-effect parameters estimated within each model. The parameter count for a given model, $K^{[g]}_j$, was equal to the rank of the fixed-effect design matrix plus 1 for the standard deviation ($\sigma_{\text{Release},j}$) estimated for the release-group intercept. The parameter count for the CJS models also included 6 additional parameters for the linear combinations defining detection-related parameters.

### Confidence sets

The confidence sets included all models with $\Delta_i<2$. We used this threshold because it represented models that had “substantial empirical support” according to Burnham and Anderson (2007). The model selection procedure identified a set of top-performing candidate models referred to as the "confidence set", $\mathcal{C}^{[g]}$:
$$\mathcal{C}^{[g]}=\{M^{[g]}_{j} \in \mathcal{M}^{[g]}:\Delta_{j}\le d\}$$where $d$ is a relative performance threshold defined in terms of the $\Delta_j$. 

Model averaging is focused on generating predictions based on parameter estimates and the relative performance of models in the confidence set $\mathcal{C}^{[g]}$ while accounting for model selection uncertainty. Here, we continue with notation used in the previous section and use subscripts to denote reduced versions of matrices that include or reference only those columns from $\mathcal{X}^{[g]}_{\mathcal{M}}$ that are needed to specify $\mathcal{C}^{[g]}$. The relative performance of models in this set was measured using weights $w^{[g]}_i$, which are based only on $\mathcal{C}^{[g]}$ and not $\mathcal{M}^{[g]}$. Model-averaged predictions were calculated by generating a vector of linear predictors under each model and then averaging over them.

## Model Averaging

### Individual model predictions

The vector of logit-linear predictions ($\mathbf{\hat{η}}_j^{[g]}$) for the fitted data for model $M^{[g]}_{j}$ was based on the general model equation applied to all models with different sub-components:
$$\mathbf{\hat{η}}_j^{[g]}=\mathbf{X}^{[g]}_{j} \hat{\mathbf{β}}_j^{[g]}+\mathbf{Z}^{[g]}\hat{\mathbf{b}}^{[g]}_j,$$where $\mathbf{Z}^{[g]}$ is an index vector for release group and $\hat{\mathbf{b}}^{[g]}_j$ is the vector of random intercept estimates: $\mathbf{b}_j \sim \mathcal{N}(0, \sigma _{\text{r},j})$. Interval estimates for specific models were computed by propagating the estimated variance in the fixed and random effects. The marginal variance estimates for the predicted vectors, $\text{Var}_\text{mar}$, was computed using the design matrix and the observed fixed-effect covariance, $\hat{\mathbf{V}}_j$:
$$\text{Var}_\text{mar}\left(\hat{\mathbf{η}}_j^{[g]}\right)=\mathbf{X}_{j}^{\top }\hat{\mathbf{V}}_j^{\beta}\mathbf{X}_{j}.$$This represents the uncertainty related to covariate relationships and ignores release group level error. The uncertainty related to predicting the response for an individual from an unknown release group was incorporated by summing the marginal variance and the estimated release group level variance, $\hat{\sigma} _{\text{r},j}$, assuming independence between these effects:
$$\text{Var}\left(\mathbf{\hat{η}}_j^{[g]}\right)=\text{Var}_\text{mar}\left(\hat{\mathbf{η}}_j^{[g]}\right)+\hat{\sigma} _{\text{r},j}^{2}.$$

### Model-averaged predictions for component models

The model-averaged linear predictor vector ($\mathbf{\hat{η}}_\text{avg}^{[g]}$) was then calculated as a weighted average across models in the confidence set:
$$\hat{\mathbf{η}}^{[g]}_\text{avg}=\sum_{j=1}^{B_{\mathcal{C}}}{\hat{\mathbf{η}}_j \cdot w_j }.$$The final step in obtaining model predictions involved transforming vectors of linear predictors $\mathbf{η}$s into probabilities $\mathbf{p}$s via the inverse logit
function:$$\mathbf{\hat{p}}_\text{avg}^{[g]} =logit^{-1}\left(\mathbf{\hat{η}}_\text{avg}^{[g]}\right).$$
For the first two survival models, the alternative set of predictions was based on the same prediction procedure but with columns of the design matrices that reference the alternate route assigned a value of $1$. The alternate route usage probabilities are complementary and the alternate route was calculated by subtraction: $\hat{\mathbf{Ψ}}_{\text{avg}}^{SJL}=1-\hat{\mathbf{Ψ}}_{\text{avg}}^{ORE}$ and $\hat{\mathbf{Ψ}}_{\text{avg}}^{MAC}=1-\hat{\mathbf{Ψ}}_{\text{avg}}^{TRN}$.

#### Standard errors and confidence intervals for predicted probabilities

Estimating the uncertainty surrounding model averaged prediction involved combining the variances associated with model-specific predictions and model-to-model variation (model selection uncertainty). Variance among models forming the confidence set, $\text{Var}_{\mathcal{C}}\left(\mathbf{\hat{η}}_j^{[g]}\right)$, was computed as the sum of the squared difference between individual candidate model predictions and the weighted-average logit scale predictor:
$$\text{Var}_{\mathcal{C}}\left(\mathbf{\hat{η}}_j^{[g]}\right)=(\hat{\mathbf{η}}^{[g]}_{j} - \hat{\mathbf{η}}^{[g]}_{avg})^2.$$The standard error for the weighted-average, $SE\left(\hat{\mathbf{η}}^{[g]}_\text{avg}\right)$, was then computed using the prediction error and candidate model weights as follows:
$$SE\left(\hat{\mathbf{η}}^{[g]}_\text{avg}\right)=\sum_{i=1}^{B_{\mathcal{C}}} w_j \cdot \sqrt{\text{Var}\left(\mathbf{\hat{η}}_j^{[g]}\right)+\text{Var}_{\mathcal{C}}\left(\mathbf{\hat{η}}_j^{[g]}\right)}.$$Wald-based confidence intervals for predicted probabilities were computed for each of the five component models by applying an inverse logit transformation:
$$\mathbf{\hat{p}}_\text{lcl,ucl}^{[g]}=logit^{-1}\left(\mathbf{\hat{η}}_\text{avg}^{[g]} \pm 1.96\cdot \text{SE}\left[\mathbf{\hat{η}}_\text{avg}^{[g]}\right] \right).$$
The variance for each predicted probability was computed using the expression
$$\text{Var}\left(\mathbf{\hat{p}}_\text{avg}^{[g]}\right) = \left(\mathbf{\hat{p}}_\text{avg}^{[g]}(1 - \mathbf{\hat{p}}_\text{avg}^{[g]})  \right)^2 \cdot \text{Var}\left[\mathbf{\hat{η}}_\text{avg}^{[g]}\right].$$
This formula is based on the delta method where (i.e., $\text{Var}\left[g(\hat{\mu})\right] \approx \left[g'(\hat{\mu} )\right]^{2} \cdot \text{Var}\left(\hat{\mu}\right)$) applied to to the inverse logit function [@VerHoef_2012_WhoInventedDelta].
