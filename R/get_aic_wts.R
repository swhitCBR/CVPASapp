get_aic_wts <- function(df, aic_column) {
  delta_aic <- df[[aic_column]] - min(df[[aic_column]], na.rm = TRUE)
  rel_likelihood <- exp(-0.5 * delta_aic)
  weights <- rel_likelihood / sum(rel_likelihood, na.rm = TRUE)
  return(weights)
}
