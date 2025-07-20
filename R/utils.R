#' Helper function untuk membuat diagnostic plots
#'
#' @param model Objek model lm
#' @export
diagnostic_plots <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
}

#' Extract model information
#'
#' @param model Objek model lm
#' @return List berisi informasi model
#' @export
model_info <- function(model) {
  info <- list(
    n_obs = nobs(model),
    n_predictors = length(coef(model)) - 1,
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    f_statistic = summary(model)$fstatistic[1],
    p_value = pf(summary(model)$fstatistic[1], 
                 summary(model)$fstatistic[2], 
                 summary(model)$fstatistic[3], 
                 lower.tail = FALSE),
    aic = AIC(model),
    bic = BIC(model)
  )
  
  class(info) <- "model_info"
  return(info)
}

#' Print method untuk model_info
#' @export
print.model_info <- function(x, ...) {
  cat("\n========== INFORMASI MODEL ==========\n\n")
  cat("Jumlah observasi:", x$n_obs, "\n")
  cat("Jumlah prediktor:", x$n_predictors, "\n")
  cat("R-squared:", round(x$r_squared, 4), "\n")
  cat("Adjusted R-squared:", round(x$adj_r_squared, 4), "\n")
  cat("F-statistic:", round(x$f_statistic, 4), "\n")
  cat("P-value (model):", format.pval(x$p_value), "\n")
  cat("AIC:", round(x$aic, 2), "\n")
  cat("BIC:", round(x$bic, 2), "\n")
  cat("\n====================================\n")
}