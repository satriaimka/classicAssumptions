#' Uji Autokorelasi
#'
#' Melakukan berbagai uji autokorelasi pada model regresi
#'
#' @param model Objek model lm
#' @param alpha Tingkat signifikansi (default = 0.05)
#' @param plot Logical, apakah menampilkan plot (default = TRUE)
#' @return List berisi hasil uji autokorelasi
#' @export
uji_autokorelasi <- function(model, alpha = 0.05, plot = TRUE) {
  if (!inherits(model, "lm")) {
    stop("Input harus berupa objek lm")
  }
  
  # Durbin-Watson test
  dw_test <- lmtest::dwtest(model)
  
  # Breusch-Godfrey test (untuk autokorelasi order tinggi)
  bg_test <- lmtest::bgtest(model)
  
  # Ljung-Box test
  residuals <- residuals(model)
  lb_test <- Box.test(residuals, lag = min(10, length(residuals)/5), type = "Ljung-Box")
  
  # Run test
  runs_test <- tseries::runs.test(as.factor(residuals > median(residuals)))
  
  # Plot jika diminta
  if (plot) {
    par(mfrow = c(2, 2))
    
    # Residual vs Order
    plot(residuals, type = "o",
         main = "Residual vs Order",
         xlab = "Observation Order", ylab = "Residuals",
         pch = 20, col = "blue")
    abline(h = 0, col = "red", lty = 2)
    
    # ACF plot
    acf(residuals, main = "ACF of Residuals")
    
    # PACF plot
    pacf(residuals, main = "PACF of Residuals")
    
    # Residual lag plot
    n <- length(residuals)
    plot(residuals[-n], residuals[-1],
         main = "Residual Lag Plot",
         xlab = "Residual(t-1)", ylab = "Residual(t)",
         pch = 20, col = "blue")
    abline(lm(residuals[-1] ~ residuals[-n]), col = "red")
    
    par(mfrow = c(1, 1))
  }
  
  # Interpretasi Durbin-Watson
  dw_value <- dw_test$statistic
  if (dw_value < 1.5) {
    dw_interpretation <- "Indikasi autokorelasi positif"
  } else if (dw_value > 2.5) {
    dw_interpretation <- "Indikasi autokorelasi negatif"
  } else {
    dw_interpretation <- "Tidak ada indikasi autokorelasi"
  }
  
  # Hasil
  hasil <- list(
    durbin_watson = list(
      statistic = dw_test$statistic,
      p_value = dw_test$p.value,
      interpretation = dw_interpretation,
      keputusan = ifelse(dw_test$p.value > alpha, "Tidak ada autokorelasi", "Ada autokorelasi")
    ),
    breusch_godfrey = list(
      statistic = bg_test$statistic,
      p_value = bg_test$p.value,
      keputusan = ifelse(bg_test$p.value > alpha, "Tidak ada autokorelasi", "Ada autokorelasi")
    ),
    ljung_box = list(
      statistic = lb_test$statistic,
      p_value = lb_test$p.value,
      keputusan = ifelse(lb_test$p.value > alpha, "Tidak ada autokorelasi", "Ada autokorelasi")
    ),
    runs_test = list(
      statistic = runs_test$statistic,
      p_value = runs_test$p.value,
      keputusan = ifelse(runs_test$p.value > alpha, "Random (tidak ada autokorelasi)", "Tidak random (ada autokorelasi)")
    )
  )
  
  class(hasil) <- "uji_autokorelasi"
  return(hasil)
}

#' Print method untuk uji_autokorelasi
#' @export
print.uji_autokorelasi <- function(x, ...) {
  cat("\n========== UJI AUTOKORELASI ==========\n\n")
  
  cat("1. Durbin-Watson Test:\n")
  cat("   Statistic:", round(x$durbin_watson$statistic, 4), "\n")
  cat("   P-value:", round(x$durbin_watson$p_value, 4), "\n")
  cat("   Interpretasi:", x$durbin_watson$interpretation, "\n")
  cat("   Keputusan:", x$durbin_watson$keputusan, "\n\n")
  
  cat("2. Breusch-Godfrey Test:\n")
  cat("   Statistic:", round(x$breusch_godfrey$statistic, 4), "\n")
  cat("   P-value:", round(x$breusch_godfrey$p_value, 4), "\n")
  cat("   Keputusan:", x$breusch_godfrey$keputusan, "\n\n")
  
  cat("3. Ljung-Box Test:\n")
  cat("   Statistic:", round(x$ljung_box$statistic, 4), "\n")
  cat("   P-value:", round(x$ljung_box$p_value, 4), "\n")
  cat("   Keputusan:", x$ljung_box$keputusan, "\n\n")
  
  cat("4. Runs Test:\n")
  cat("   Statistic:", round(x$runs_test$statistic, 4), "\n")
  cat("   P-value:", round(x$runs_test$p_value, 4), "\n")
  cat("   Keputusan:", x$runs_test$keputusan, "\n")
  
  cat("\n=====================================\n")
}