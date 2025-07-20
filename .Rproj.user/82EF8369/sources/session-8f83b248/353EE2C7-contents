#' Uji Homoskedastisitas
#'
#' Melakukan berbagai uji homoskedastisitas pada model regresi
#'
#' @param model Objek model lm
#' @param alpha Tingkat signifikansi (default = 0.05)
#' @param plot Logical, apakah menampilkan plot (default = TRUE)
#' @return List berisi hasil uji homoskedastisitas
#' @export
uji_homoskedastisitas <- function(model, alpha = 0.05, plot = TRUE) {
  if (!inherits(model, "lm")) {
    stop("Input harus berupa objek lm")
  }
  
  # Breusch-Pagan test
  bp_test <- lmtest::bptest(model)
  
  # Goldfeld-Quandt test
  gq_test <- lmtest::gqtest(model)
  
  # White test (menggunakan squared terms)
  X <- model.matrix(model)[, -1]  # Remove intercept
  if (ncol(X) > 0) {
    X_squared <- X^2
    colnames(X_squared) <- paste0(colnames(X), "_sq")
    
    # Gabungkan X dan X^2
    X_white <- cbind(X, X_squared)
    
    # Regresikan residual kuadrat terhadap X dan X^2
    res_sq <- residuals(model)^2
    white_model <- lm(res_sq ~ X_white)
    white_stat <- nobs(white_model) * summary(white_model)$r.squared
    white_df <- ncol(X_white)
    white_pvalue <- pchisq(white_stat, df = white_df, lower.tail = FALSE)
  } else {
    white_stat <- NA
    white_pvalue <- NA
  }
  
  # Glejser test
  abs_res <- abs(residuals(model))
  fitted_vals <- fitted(model)
  glejser_model <- lm(abs_res ~ fitted_vals)
  glejser_pvalue <- summary(glejser_model)$coefficients[2, 4]
  
  # Plot jika diminta
  if (plot) {
    par(mfrow = c(2, 2))
    
    # Residual vs Fitted
    plot(fitted(model), residuals(model),
         main = "Residual vs Fitted Values",
         xlab = "Fitted Values", ylab = "Residuals",
         pch = 20, col = "blue")
    abline(h = 0, col = "red", lty = 2)
    
    # Scale-Location plot
    sqrt_abs_res <- sqrt(abs(residuals(model)))
    plot(fitted(model), sqrt_abs_res,
         main = "Scale-Location Plot",
         xlab = "Fitted Values", ylab = "sqrt(|Residuals|)",
         pch = 20, col = "blue")
    
    # Residual vs Predictor (untuk model sederhana)
    if (ncol(X) == 1) {
      plot(X[,1], residuals(model),
           main = paste("Residual vs", colnames(X)[1]),
           xlab = colnames(X)[1], ylab = "Residuals",
           pch = 20, col = "blue")
      abline(h = 0, col = "red", lty = 2)
    }
    
    # Histogram of residuals
    hist(residuals(model), main = "Histogram of Residuals",
         xlab = "Residuals", col = "lightgreen", freq = FALSE)
    
    par(mfrow = c(1, 1))
  }
  
  # Hasil
  hasil <- list(
    breusch_pagan = list(
      statistic = bp_test$statistic,
      p_value = bp_test$p.value,
      keputusan = ifelse(bp_test$p.value > alpha, "Homoskedastis", "Heteroskedastis")
    ),
    goldfeld_quandt = list(
      statistic = gq_test$statistic,
      p_value = gq_test$p.value,
      keputusan = ifelse(gq_test$p.value > alpha, "Homoskedastis", "Heteroskedastis")
    ),
    white_test = list(
      statistic = white_stat,
      p_value = white_pvalue,
      keputusan = ifelse(is.na(white_pvalue), "NA", 
                        ifelse(white_pvalue > alpha, "Homoskedastis", "Heteroskedastis"))
    ),
    glejser_test = list(
      p_value = glejser_pvalue,
      keputusan = ifelse(glejser_pvalue > alpha, "Homoskedastis", "Heteroskedastis")
    )
  )
  
  class(hasil) <- "uji_homoskedastisitas"
  return(hasil)
}

#' Print method untuk uji_homoskedastisitas
#' @export
print.uji_homoskedastisitas <- function(x, ...) {
  cat("\n========== UJI HOMOSKEDASTISITAS ==========\n\n")
  
  cat("1. Breusch-Pagan Test:\n")
  cat("   Statistic:", round(x$breusch_pagan$statistic, 4), "\n")
  cat("   P-value:", round(x$breusch_pagan$p_value, 4), "\n")
  cat("   Keputusan:", x$breusch_pagan$keputusan, "\n\n")
  
  cat("2. Goldfeld-Quandt Test:\n")
  cat("   Statistic:", round(x$goldfeld_quandt$statistic, 4), "\n")
  cat("   P-value:", round(x$goldfeld_quandt$p_value, 4), "\n")
  cat("   Keputusan:", x$goldfeld_quandt$keputusan, "\n\n")
  
  cat("3. White Test:\n")
  if (!is.na(x$white_test$statistic)) {
    cat("   Statistic:", round(x$white_test$statistic, 4), "\n")
    cat("   P-value:", round(x$white_test$p_value, 4), "\n")
  }
  cat("   Keputusan:", x$white_test$keputusan, "\n\n")
  
  cat("4. Glejser Test:\n")
  cat("   P-value:", round(x$glejser_test$p_value, 4), "\n")
  cat("   Keputusan:", x$glejser_test$keputusan, "\n")
  
  cat("\n=========================================\n")
}