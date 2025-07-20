#' Uji Multikolinearitas
#'
#' Melakukan uji multikolinearitas pada model regresi
#'
#' @param model Objek model lm
#' @param threshold_vif Nilai threshold untuk VIF (default = 10)
#' @param plot Logical, apakah menampilkan plot (default = TRUE)
#' @return List berisi hasil uji multikolinearitas
#' @export
uji_multikolinearitas <- function(model, threshold_vif = 10, plot = TRUE) {
  if (!inherits(model, "lm")) {
    stop("Input harus berupa objek lm")
  }
  
  # Check if model has more than one predictor
  if (ncol(model.matrix(model)) <= 2) {
    cat("Model hanya memiliki satu prediktor. Tidak ada multikolinearitas.\n")
    return(invisible(NULL))
  }
  
  # VIF calculation
  vif_values <- car::vif(model)
  
  # Correlation matrix
  X <- model.matrix(model)[, -1]  # Remove intercept
  cor_matrix <- cor(X)
  
  # Eigenvalues for condition index
  XtX <- t(X) %*% X
  eigenvalues <- eigen(XtX)$values
  condition_index <- sqrt(max(eigenvalues) / eigenvalues)
  
  # Tolerance
  if (is.matrix(vif_values)) {
    # For categorical variables with multiple levels
    tolerance <- 1 / vif_values[, "GVIF^(1/(2*Df))"]^2
  } else {
    tolerance <- 1 / vif_values
  }
  
  # Plot jika diminta
  if (plot) {
    par(mfrow = c(2, 2))
    
    # VIF plot
    if (is.matrix(vif_values)) {
      vif_to_plot <- vif_values[, "GVIF^(1/(2*Df))"]^2
    } else {
      vif_to_plot <- vif_values
    }
    
    barplot(vif_to_plot, 
            main = "VIF Values",
            ylab = "VIF", 
            col = ifelse(vif_to_plot > threshold_vif, "red", "lightblue"),
            las = 2)
    abline(h = threshold_vif, col = "red", lty = 2)
    
    # Correlation heatmap (simple version)
    if (ncol(cor_matrix) > 1) {
      image(cor_matrix, main = "Correlation Matrix",
            axes = FALSE, col = heat.colors(20))
      axis(1, at = seq(0, 1, length.out = ncol(cor_matrix)),
           labels = colnames(cor_matrix), las = 2)
      axis(2, at = seq(0, 1, length.out = ncol(cor_matrix)),
           labels = colnames(cor_matrix), las = 2)
    }
    
    # Condition Index plot
    barplot(condition_index, 
            main = "Condition Index",
            ylab = "Condition Index",
            col = ifelse(condition_index > 30, "red", "lightgreen"))
    abline(h = 30, col = "red", lty = 2)
    
    # Tolerance plot
    barplot(tolerance,
            main = "Tolerance Values",
            ylab = "Tolerance",
            col = ifelse(tolerance < 0.1, "red", "lightblue"),
            las = 2)
    abline(h = 0.1, col = "red", lty = 2)
    
    par(mfrow = c(1, 1))
  }
  
  # Identifikasi variabel bermasalah
  if (is.matrix(vif_values)) {
    problem_vars <- rownames(vif_values)[vif_values[, "GVIF^(1/(2*Df))"]^2 > threshold_vif]
  } else {
    problem_vars <- names(vif_values)[vif_values > threshold_vif]
  }
  
  # Hasil
  hasil <- list(
    vif_values = vif_values,
    correlation_matrix = cor_matrix,
    condition_index = condition_index,
    tolerance = tolerance,
    problem_variables = problem_vars,
    max_condition_index = max(condition_index),
    multicollinearity_detected = length(problem_vars) > 0 || max(condition_index) > 30
  )
  
  class(hasil) <- "uji_multikolinearitas"
  return(hasil)
}

#' Print method untuk uji_multikolinearitas
#' @export
print.uji_multikolinearitas <- function(x, ...) {
  cat("\n========== UJI MULTIKOLINEARITAS ==========\n\n")
  
  cat("1. Variance Inflation Factor (VIF):\n")
  if (is.matrix(x$vif_values)) {
    print(round(x$vif_values, 4))
  } else {
    vif_df <- data.frame(
      Variable = names(x$vif_values),
      VIF = round(x$vif_values, 4),
      Status = ifelse(x$vif_values > 10, "Bermasalah", "OK")
    )
    print(vif_df, row.names = FALSE)
  }
  
  cat("\n2. Correlation Matrix:\n")
  print(round(x$correlation_matrix, 3))
  
  cat("\n3. Condition Index:\n")
  cat("   Maximum:", round(x$max_condition_index, 4), "\n")
  cat("   Status:", ifelse(x$max_condition_index > 30, "Indikasi multikolinearitas", "OK"), "\n")
  
  cat("\n4. Tolerance:\n")
  tol_df <- data.frame(
    Variable = names(x$tolerance),
    Tolerance = round(x$tolerance, 4),
    Status = ifelse(x$tolerance < 0.1, "Bermasalah", "OK")
  )
  print(tol_df, row.names = FALSE)
  
  if (x$multicollinearity_detected) {
    cat("\n[!] MULTIKOLINEARITAS TERDETEKSI\n")
    if (length(x$problem_variables) > 0) {
      cat("Variabel bermasalah:", paste(x$problem_variables, collapse = ", "), "\n")
    }
  } else {
    cat("\n[✓] Tidak ada multikolinearitas\n")
  }
  
  cat("\n=========================================\n")
}