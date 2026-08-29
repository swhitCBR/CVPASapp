# Component 1: Matrix Derivatives & Layer Mapping

Component 1 of `get_overall_surv_OLD` converts each logit-scale normal parameter (mean $\mu$, variance $\sigma^2$) into approximate moments on the proportion (0–1) scale, using a second-order Taylor expansion of the logistic function around $\mu$.

## Setup

For each of the 6 parameters, the model assumes the *logit* of the true proportion is normally distributed:

$$\text{logit}(p) \sim N(\mu, \sigma^2)$$

We want $E[p]$ and $\text{Var}(p)$ where

$$p = \text{logit}^{-1}(X) = \frac{1}{1+e^{-X}}, \qquad X \sim N(\mu, \sigma^2)$$

## Derivatives of the logistic function

Evaluated at $X = \mu$:

- **`f_mu`**: the logistic function itself

$$f(\mu) = \frac{1}{1+e^{-\mu}}$$

- **`f_pr`**: the first derivative (standard logistic derivative identity)

$$f'(\mu) = f(\mu)\bigl(1-f(\mu)\bigr)$$

- **`f_sec`**: the second derivative

$$f''(\mu) = f(\mu)\bigl(1-f(\mu)\bigr)\bigl(1-2f(\mu)\bigr)$$

## Delta-method moment approximations

- **`E_prop`**:

$$E[p] \approx f(\mu) + \frac{\sigma^2}{2} f''(\mu)$$

This is the second-order Taylor expansion of $E[f(X)]$ around $\mu$:

$$E[f(X)] \approx f(\mu) + \frac{1}{2}f''(\mu)\,\text{Var}(X)$$

The first-order term vanishes because $E[X-\mu]=0$. This correction matters because the logistic function is nonlinear (S-shaped), so simply plugging $\mu$ into $f$ underestimates/overestimates the mean depending on curvature — the correction pulls the mean estimate toward 0.5 when $\sigma^2$ is large.

- **`V_prop`**:

$$\text{Var}(p) \approx \bigl[f'(\mu)\bigr]^2 \sigma^2$$

This is the standard first-order delta method for variance:

$$\text{Var}(f(X)) \approx \bigl[f'(\mu)\bigr]^2 \text{Var}(X)$$

## Application

The code then applies this to the `MU_mat`/`SIGMA2_mat` matrices all at once (vectorized across the 6 columns and all scenario rows), and unpacks the 6 resulting proportion-scale mean/variance pairs into individual named vectors (`E_Psi_SLJ`, `V_Psi_SLJ`, etc.) for use in the later hierarchical propagation steps.

## Caveat

This is only a local (Taylor-series) approximation — it will degrade when $\sigma^2$ is large relative to how sharply the logistic curves near $\mu$ (i.e., near the extremes of $p$), which is presumably why the file also contains a Monte Carlo version (`get_overall_surv_sim_OLD`) for comparison.
