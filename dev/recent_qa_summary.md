# Recent Q&A: `get_overall_surv()` Component 1, Derivatives, and the `logitnorm` Package

This document collects answers to the last five questions discussed about the `get_overall_surv` family of functions in this project (`R/get_overall_surv.R`, `dev/get_overall_surv_logit.R`, `dev/get_overall_surv_logitnorm.R`).

---

## 1. Does `get_overall_surv()` have a "Component 1"?

No. `R/get_overall_surv.R`'s `get_overall_surv()` function jumps straight to `# --- COMPONENT 2 ---`; it has no Component 1 step at all. Its roxygen docstring originally described `E_prop`/`V_prop` as "logit-scale means"/"variances," but the function body treats them as **already-converted proportion-scale** moments — it unpacks the 6 columns directly into `E_Psi_SLJ`, `V_Psi_SLJ`, etc. and moves straight into the sub-metric allocation layer.

Component 1 exists only in the sibling functions `get_overall_surv_logit()` and the original `get_overall_surv_OLD()`, which accept raw logit-scale `MU_mat`/`SIGMA2_mat` and convert them to proportion-scale moments before anything else happens:

- `f_mu`: the logistic function $f(\mu) = \dfrac{1}{1+e^{-\mu}}$, evaluated at the logit-scale mean.
- `f_pr`: its first derivative $f'(\mu) = f(\mu)(1-f(\mu))$.
- `f_sec`: its second derivative $f''(\mu) = f(\mu)(1-f(\mu))(1-2f(\mu))$.
- `E_prop`: the proportion-scale mean, via a second-order Taylor (delta-method) correction:
$$E[p] \approx f(\mu) + \frac{\sigma^2}{2}f''(\mu)$$
- `V_prop`: the proportion-scale variance, via the standard first-order delta method:
$$\text{Var}(p) \approx [f'(\mu)]^2\sigma^2$$

So `get_overall_surv()` presumes this conversion has already been done by its caller before `E_prop`/`V_prop` are passed in.

---

## 2. Docstring fix in `R/get_overall_surv.R`

Given the finding above, the `@param` docstring was corrected to accurately describe the inputs:

**Before:**
```r
#' @param E_prop A matrix where each row is a scenario and the 6 columns are logit-scale means ...
#' @param V_prop A matrix of the same dimensions containing logit-scale variances.
```

**After:**
```r
#' @param E_prop A matrix where each row is a scenario and the 6 columns are proportion-scale means (already converted from the logit scale) ...
#' @param V_prop A matrix of the same dimensions containing proportion-scale variances (already converted from the logit scale).
```

This avoids future callers mistakenly passing raw logit-scale values directly into `get_overall_surv()`.

---

## 3. How does the second-derivative calculation work?

The `f_sec` line in `get_overall_surv_logit()` (and `get_overall_surv_OLD()`) is:

```r
f_sec <- f_mu * (1 - f_mu) * (1 - 2 * f_mu)
```

**Derivation.** Let $f(x) = \dfrac{1}{1+e^{-x}}$ be the logistic function. Its first derivative has the well-known self-referential form:

$$f'(x) = f(x)\bigl(1-f(x)\bigr)$$

Apply the product rule to $f'(x) = f(x) - f(x)^2$ to get the second derivative:

$$f''(x) = \frac{d}{dx}\Bigl[f(x)\bigl(1-f(x)\bigr)\Bigr] = f'(x)\bigl(1-f(x)\bigr) + f(x)\bigl(-f'(x)\bigr) = f'(x)\Bigl[\bigl(1-f(x)\bigr) - f(x)\Bigr]$$

$$= f'(x)\bigl(1-2f(x)\bigr) = f(x)\bigl(1-f(x)\bigr)\bigl(1-2f(x)\bigr)$$

So `f_sec` is $f''(\mu)$ evaluated at each scenario's logit-scale mean, reusing `f_mu` ($f(\mu)$) computed on the line above. The code computes this elementwise across the whole `MU_mat` matrix at once — `f_mu`, `f_pr`, `f_sec` are all matrices with the same shape as `MU_mat`.

This second derivative drives the mean-bias correction term in `E_prop`:

$$E[p] \approx f(\mu) + \frac{\sigma^2}{2}f''(\mu)$$

**Sign behavior:** $f''(\mu)$ is positive when $f(\mu) < 0.5$ and negative when $f(\mu) > 0.5$ (zero exactly at $\mu = 0$, where $f(\mu) = 0.5$). This reflects the S-curve's asymmetric curvature on either side of its inflection point, and is why the correction pulls proportions toward 0.5 when variance is large, regardless of which side of 0.5 the mean started on.

---

## 4. How does this relate to Frederic & Lad (2008)?

The delta-method calculation in Component 1 is an **approximation** to the same quantity that Frederic & Lad (2008), *"Two Moments of the Logitnormal Distribution,"* compute **exactly**.

- **Frederic & Lad (2008)** characterize the exact first two moment functions of the Logitnormal$(\mu, \sigma^2)$ family, parameterized by the Normal mean $\mu$ and signal-to-noise ratio $\mu/\sigma$. Because closed-form expressions for logit-normal moments don't exist, their $E[X]$ and $\text{Var}(X)$ are obtained via **numerical integration** over the exact logit-normal density.

- **Component 1** instead uses a **second-order Taylor (delta-method) expansion** of the logistic transform around $\mu$ — a local, closed-form approximation using only $f(\mu)$, $f'(\mu)$, $f''(\mu)$, and $\sigma^2$, with no integration.

**Relationship:** the delta-method formula is an asymptotic approximation to the quantity Frederic & Lad compute numerically. As $\sigma^2 \to 0$, the two agree to $O(\sigma^4)$ — the Taylor series is truncated after the quadratic term, capturing leading-order curvature bias but dropping higher-order terms the exact integral includes. The approximation degrades as $\sigma$ grows relative to the curvature of $f$ near $\mu$.

This connects to a point Frederic & Lad make directly: a Logitnormal specification intended to represent "diffuse" prior information can, in some regimes, actually represent strong prior information that the true proportion is very likely close to 0 or 1 — precisely the large-variance regime where the delta-method approximation is weakest, and where the original Beta-fit confidence interval (in `get_overall_surv_OLD()`'s Component 4) could fail outright.

The `logitnorm` R package (Wutzler 2012) computes these moments via numerical integration directly, providing a way to validate — or replace — Component 1's Taylor approximation with exact logit-normal moments.

---

## 5. New function: `get_overall_surv_logitnorm()`

`dev/get_overall_surv_logitnorm.R` defines `get_overall_surv_logitnorm()`, identical to `get_overall_surv_logit()` in every respect except Component 1: instead of the Taylor/delta-method approximation, it computes exact proportion-scale moments using `logitnorm::momentsLogitnorm()`.

Since `momentsLogitnorm()` is not vectorized over `mu`/`sigma`, it's applied elementwise via `mapply()` and reshaped back into matrices matching `MU_mat`'s dimensions:

```r
sigma_mat <- sqrt(SIGMA2_mat)
moments   <- mapply(
  function(m, s) logitnorm::momentsLogitnorm(mu = m, sigma = s),
  m = as.vector(MU_mat), s = as.vector(sigma_mat)
)
E_prop <- matrix(moments["mean", ], nrow = nrow(MU_mat), ncol = ncol(MU_mat))
V_prop <- matrix(moments["var", ],  nrow = nrow(MU_mat), ncol = ncol(MU_mat))
```

Components 2–4 (sub-metric allocation, top-level propagation, and the logit-scale delta-method confidence interval) are unchanged from `get_overall_surv_logit()`.

**Validation.** Comparing the two functions on four test scenarios (two probability profiles crossed with low/high logit-scale variance) showed close agreement for low-variance scenarios, with modest divergence for higher-variance scenarios — e.g. one scenario's $S_{HOR\_CHP}$ confidence interval width shrank from 0.374 (delta-method) to about 0.365 (exact `logitnorm` moments). This is consistent with the delta-method approximation being least accurate exactly where variance is largest, as discussed in Section 4 above.

Note: the `logitnorm` package must be installed (`install.packages("logitnorm")`) for this function to run.

---

## References

- Atchison, J., & Shen, S. M. (1980). Logistic-normal distributions: Some properties and uses. *Biometrika*, 67(2), 261–272. https://doi.org/10.1093/biomet/67.2.261

- Frederic, P., & Lad, F. (2008). Two moments of the logitnormal distribution. *Communications in Statistics – Simulation and Computation*, 37(7), 1263–1269. https://doi.org/10.1080/03610910801983178

- Mead, R. (1965). A generalised logit-normal distribution. *Biometrics*, 21(3), 721–732. https://doi.org/10.2307/2528553
