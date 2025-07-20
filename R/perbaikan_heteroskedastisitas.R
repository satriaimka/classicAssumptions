#' Perbaikan Heteroskedastisitas menggunakan WLS
#'
#' Melakukan perbaikan heteroskedastisitas dengan Weighted Least Squares
#'
#' @param model Objek model lm
#' @param weight_method Metode penentuan weight: "fitted", "residual", "log_fitted", "custom" (default = "fitted")
#' @param custom_weights Vektor weight kustom (jika method = "custom")
#' @param alpha Tingkat signifikansi untuk uji homoskedastisitas (default = 0.05)
#' @param verbose Logical, apakah menampilkan proses (default = TRUE)
#' @return List berisi model WLS dan informasi perbaikan
#' @export
perbaikan_heteroskedastisitas <- function(model, weight_method = "fitted", 
                                        custom_weights = NULL, alpha = 0.05, 
                                        verbose = TRUE) {
  if (!inherits(model, "lm")) {
    stop("Input harus berupa objek lm")
  }
  
  if (verbose) {
    cat("\n========== PERBAIKAN HETEROSKEDASTISITAS ==========\n")
    cat("Metode weight:", weight_method, "\n\n")
  }
  
  # Get original data
  data <- model$model
  formula_orig <- formula(model)
  
  # Calculate weights based on method
  fitted_vals <- fitted(model)
  residuals_orig <- residuals(model)
  
  weights <- switch(weight_method,
    "fitted" = {
      # Weight = 1/fitted^2 (untuk variance proportional to fitted values)
      if (verbose) cat("Menggunakan weight = 1/fitted^2\n")
      1 / fitted_vals^2
    },
    "residual" = {
      # Weight = 1/residual^2 (untuk variance proportional to residuals)
      if (verbose) cat("Menggunakan weight = 1/residual^2\n")
      abs_res <- abs(residuals_orig)
      abs_res[abs_res == 0] <- min(abs_res[abs_res > 0])  # Avoid division by zero
      1 / abs_res^2
    },
    "log_fitted" = {
      # Weight = 1/log(fitted)^2 (untuk variance proportional to log of fitted values)
      if (verbose) cat("Menggunakan weight = 1/log(fitted)^2\n")
      log_fitted <- log(abs(fitted_vals) + 1)  # Add 1 to avoid log(0)
      1 / log_fitted^2
    },
    "adaptive" = {
      # Adaptive method: estimate variance function first
      if (verbose) cat("Menggunakan metode adaptive untuk estimasi variance\n")
      
      # Step 1: Regress squared residuals on fitted values
      res_sq <- residuals_orig^2
      var_model <- lm(res_sq ~ fitted_vals + I(fitted_vals^2))
      
      # Step 2: Use predicted variance as weights
      pred_var <- fitted(var_model)
      pred_var[pred_var <= 0] <- min(pred_var[pred_var > 0])  # Ensure positive
      1 / pred_var
    },
    "custom" = {
      if (is.null(custom_weights)) {
        stop("Custom weights harus disediakan jika method = 'custom'")
      }
      if (length(custom_weights) != nobs(model)) {
        stop("Panjang custom weights harus sama dengan jumlah observasi")
      }
      if (verbose) cat("Menggunakan custom weights\n")
      custom_weights
    }
  )
  
  # Ensure weights are positive and finite
  weights[!is.finite(weights) | weights <= 0] <- min(weights[is.finite(weights) & weights > 0])
  
  # Fit WLS model
  if (verbose) cat("Melakukan estimasi WLS...\n")
  wls_model <- lm(formula_orig, data = data, weights = weights)
  
  # Test homoscedasticity on both models
  if (verbose) cat("Menguji homoskedastisitas pada model original dan WLS...\n")
  
  # Original model tests
  orig_bp <- lmtest::bptest(model)
  orig_gq <- lmtest::gqtest(model)
  
  # WLS model tests
  wls_bp <- lmtest::bptest(wls_model)
  wls_gq <- lmtest::gqtest(wls_model)
  
  # Summary of improvements
  orig_homo_pass <- orig_bp$p.value > alpha && orig_gq$p.value > alpha
  wls_homo_pass <- wls_bp$p.value > alpha && wls_gq$p.value > alpha
  
  improvement <- !orig_homo_pass && wls_homo_pass
  
  if (verbose) {
    cat("\n--- HASIL UJI HOMOSKEDASTISITAS ---\n")
    cat("Model Original:\n")
    cat("  Breusch-Pagan p-value:", round(orig_bp$p.value, 4), 
        ifelse(orig_bp$p.value > alpha, "(Homoskedastis)", "(Heteroskedastis)"), "\n")
    cat("  Goldfeld-Quandt p-value:", round(orig_gq$p.value, 4),
        ifelse(orig_gq$p.value > alpha, "(Homoskedastis)", "(Heteroskedastis)"), "\n")
    
    cat("\nModel WLS:\n")
    cat("  Breusch-Pagan p-value:", round(wls_bp$p.value, 4),
        ifelse(wls_bp$p.value > alpha, "(Homoskedastis)", "(Heteroskedastis)"), "\n")
    cat("  Goldfeld-Quandt p-value:", round(wls_gq$p.value, 4),
        ifelse(wls_gq$p.value > alpha, "(Homoskedastis)", "(Heteroskedastis)"), "\n")
    
    cat("\nPerbaikan berhasil:", ifelse(improvement, "Ya ✓", "Tidak ✗"), "\n")
  }
  
  # Additional diagnostics
  orig_aic <- AIC(model)
  wls_aic <- AIC(wls_model)
  
  # Result
  hasil <- list(
    original_model = model,
    wls_model = wls_model,
    weights = weights,
    weight_method = weight_method,
    improvement = improvement,
    original_tests = list(
      breusch_pagan = orig_bp,
      goldfeld_quandt = orig_gq,
      homoscedastic = orig_homo_pass
    ),
    wls_tests = list(
      breusch_pagan = wls_bp,
      goldfeld_quandt = wls_gq,
      homoscedastic = wls_homo_pass
    ),
    model_comparison = list(
      original_aic = orig_aic,
      wls_aic = wls_aic,
      aic_improvement = orig_aic - wls_aic
    )
  )
  
  class(hasil) <- "perbaikan_heteroskedastisitas"
  return(hasil)
}

#' Plot diagnostics untuk perbaikan heteroskedastisitas
#'
#' @param hasil Hasil dari perbaikan_heteroskedastisitas()
#' @export
plot.perbaikan_heteroskedastisitas <- function(hasil, ...) {
  par(mfrow = c(2, 2))
  
  # Original model residual plot
  plot(fitted(hasil$original_model), residuals(hasil$original_model),
       main = "Original Model: Residual vs Fitted",
       xlab = "Fitted Values", ylab = "Residuals",
       pch = 20, col = "blue")
  abline(h = 0, col = "red", lty = 2)
  
  # WLS model residual plot
  plot(fitted(hasil$wls_model), residuals(hasil$wls_model),
       main = "WLS Model: Residual vs Fitted",
       xlab = "Fitted Values", ylab = "Residuals",
       pch = 20, col = "green")
  abline(h = 0, col = "red", lty = 2)
  
  # Weights plot
  plot(fitted(hasil$original_model), hasil$weights,
       main = "Weights vs Fitted Values",
       xlab = "Fitted Values", ylab = "Weights",
       pch = 20, col = "purple")
  
  # Scale-Location comparison
  sqrt_abs_res_orig <- sqrt(abs(residuals(hasil$original_model)))
  sqrt_abs_res_wls <- sqrt(abs(residuals(hasil$wls_model)))
  
  plot(fitted(hasil$original_model), sqrt_abs_res_orig,
       main = "Scale-Location Comparison",
       xlab = "Fitted Values", ylab = "sqrt(|Residuals|)",
       pch = 20, col = "blue")
  points(fitted(hasil$wls_model), sqrt_abs_res_wls, pch = 20, col = "green")
  legend("topright", legend = c("Original", "WLS"), 
         col = c("blue", "green"), pch = 20)
  
  par(mfrow = c(1, 1))
}

#' Print method untuk perbaikan_heteroskedastisitas
#' @export
print.perbaikan_heteroskedastisitas <- function(x, ...) {
  cat("\n========== HASIL PERBAIKAN HETEROSKEDASTISITAS ==========\n\n")
  
  cat("Metode weight yang digunakan:", x$weight_method, "\n")
  cat("Perbaikan berhasil:", ifelse(x$improvement, "Ya ✓", "Tidak ✗"), "\n\n")
  
  cat("Perbandingan Uji Homoskedastisitas:\n")
  cat("Test                | Original | WLS      | Status\n")
  cat("--------------------|----------|----------|----------\n")
  cat("Breusch-Pagan       |", 
      sprintf("%8.4f", x$original_tests$breusch_pagan$p.value), "|",
      sprintf("%8.4f", x$wls_tests$breusch_pagan$p.value), "|",
      ifelse(x$wls_tests$breusch_pagan$p.value > x$original_tests$breusch_pagan$p.value, 
             "Membaik", "Memburuk"), "\n")
  cat("Goldfeld-Quandt     |", 
      sprintf("%8.4f", x$original_tests$goldfeld_quandt$p.value), "|",
      sprintf("%8.4f", x$wls_tests$goldfeld_quandt$p.value), "|",
      ifelse(x$wls_tests$goldfeld_quandt$p.value > x$original_tests$goldfeld_quandt$p.value, 
             "Membaik", "Memburuk"), "\n")
  
  cat("\nPerbandingan Model:\n")
  cat("Kriteria            | Original | WLS      | Perubahan\n")
  cat("--------------------|----------|----------|----------\n")
  cat("AIC                 |", 
      sprintf("%8.2f", x$model_comparison$original_aic), "|",
      sprintf("%8.2f", x$model_comparison$wls_aic), "|",
      sprintf("%8.2f", x$model_comparison$aic_improvement), "\n")
  cat("Homoskedastik       |", 
      sprintf("%8s", ifelse(x$original_tests$homoscedastic, "Ya", "Tidak")), "|",
      sprintf("%8s", ifelse(x$wls_tests$homoscedastic, "Ya", "Tidak")), "|",
      ifelse(x$wls_tests$homoscedastic && !x$original_tests$homoscedastic, 
             "Diperbaiki", ifelse(x$wls_tests$homoscedastic, "Tetap baik", "Belum teratasi")), "\n")
  
  cat("\nStatistik Weight:\n")
  cat("Minimum weight:", sprintf("%.6f", min(x$weights)), "\n")
  cat("Maksimum weight:", sprintf("%.6f", max(x$weights)), "\n")
  cat("Mean weight:", sprintf("%.6f", mean(x$weights)), "\n")
  cat("Std weight:", sprintf("%.6f", sd(x$weights)), "\n")
  
  cat("\n=======================================================\n")
  
  cat("\nUntuk melihat plot diagnostik, gunakan: plot(hasil)\n")
  cat("Model WLS tersedia di: hasil$wls_model\n")
}