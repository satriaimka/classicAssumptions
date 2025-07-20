#' Perbaikan Multikolinearitas
#'
#' Melakukan perbaikan multikolinearitas dengan menghapus variabel bermasalah
#'
#' @param model Objek model lm
#' @param threshold_vif Nilai threshold untuk VIF (default = 10)
#' @param method Metode penghapusan: "stepwise", "highest_first", "correlation_based" (default = "stepwise")
#' @param verbose Logical, apakah menampilkan proses (default = TRUE)
#' @return List berisi model yang diperbaiki dan informasi perbaikan
#' @export
perbaikan_multikolinearitas <- function(model, threshold_vif = 10, 
                                      method = "stepwise", verbose = TRUE) {
  if (!inherits(model, "lm")) {
    stop("Input harus berupa objek lm")
  }
  
  # Check if model has more than one predictor
  if (ncol(model.matrix(model)) <= 2) {
    if (verbose) cat("Model hanya memiliki satu prediktor. Tidak perlu perbaikan.\n")
    return(list(
      original_model = model,
      fixed_model = model,
      removed_variables = character(0),
      method_used = method,
      improvement = FALSE
    ))
  }
  
  if (verbose) {
    cat("\n========== PERBAIKAN MULTIKOLINEARITAS ==========\n")
    cat("Metode:", method, "\n")
    cat("Threshold VIF:", threshold_vif, "\n\n")
  }
  
  # Get original data and formula
  original_formula <- formula(model)
  data <- model$model
  
  # Track removed variables
  removed_vars <- character(0)
  current_formula <- original_formula
  iteration <- 1
  
  # Method 1: Stepwise removal (remove highest VIF iteratively)
  if (method == "stepwise") {
    repeat {
      current_model <- lm(current_formula, data = data)
      
      # Check VIF
      if (ncol(model.matrix(current_model)) <= 2) break
      
      vif_values <- car::vif(current_model)
      
      # Handle different VIF output formats
      if (is.matrix(vif_values)) {
        max_vif <- max(vif_values[, "GVIF^(1/(2*Df))"]^2)
        max_var <- rownames(vif_values)[which.max(vif_values[, "GVIF^(1/(2*Df))"]^2)]
      } else {
        max_vif <- max(vif_values)
        max_var <- names(vif_values)[which.max(vif_values)]
      }
      
      if (verbose) {
        cat("Iterasi", iteration, "- VIF tertinggi:", round(max_vif, 4), 
            "pada variabel:", max_var, "\n")
      }
      
      if (max_vif <= threshold_vif) {
        if (verbose) cat("Semua VIF <= threshold. Proses selesai.\n")
        break
      }
      
      # Remove variable with highest VIF
      removed_vars <- c(removed_vars, max_var)
      
      # Update formula
      current_formula <- update(current_formula, paste(". ~ . -", max_var))
      iteration <- iteration + 1
      
      if (verbose) {
        cat("Menghapus variabel:", max_var, "\n")
      }
    }
  }
  
  # Method 2: Remove highest VIF first (one-shot removal)
  else if (method == "highest_first") {
    current_model <- model
    vif_values <- car::vif(current_model)
    
    if (is.matrix(vif_values)) {
      problem_vars <- rownames(vif_values)[vif_values[, "GVIF^(1/(2*Df))"]^2 > threshold_vif]
      if (length(problem_vars) > 0) {
        max_vif_var <- rownames(vif_values)[which.max(vif_values[, "GVIF^(1/(2*Df))"]^2)]
      }
    } else {
      problem_vars <- names(vif_values)[vif_values > threshold_vif]
      if (length(problem_vars) > 0) {
        max_vif_var <- names(vif_values)[which.max(vif_values)]
      }
    }
    
    if (length(problem_vars) > 0) {
      removed_vars <- max_vif_var
      current_formula <- update(current_formula, paste(". ~ . -", max_vif_var))
      current_model <- lm(current_formula, data = data)
      
      if (verbose) {
        cat("Menghapus variabel dengan VIF tertinggi:", max_vif_var, "\n")
      }
    }
  }
  
  # Method 3: Correlation-based removal
  else if (method == "correlation_based") {
    X <- model.matrix(model)[, -1]  # Remove intercept
    cor_matrix <- cor(X)
    
    # Find pairs with high correlation
    high_cor_pairs <- which(abs(cor_matrix) > 0.8 & cor_matrix != 1, arr.ind = TRUE)
    
    if (nrow(high_cor_pairs) > 0) {
      # Remove variable with highest average correlation
      avg_cor <- apply(abs(cor_matrix), 1, function(x) mean(x[x != 1]))
      var_to_remove <- names(avg_cor)[which.max(avg_cor)]
      
      removed_vars <- var_to_remove
      current_formula <- update(current_formula, paste(". ~ . -", var_to_remove))
      current_model <- lm(current_formula, data = data)
      
      if (verbose) {
        cat("Menghapus variabel dengan korelasi tertinggi:", var_to_remove, "\n")
      }
    }
  }
  
  # Final model
  final_model <- lm(current_formula, data = data)
  
  # Compare models
  original_vif <- if (ncol(model.matrix(model)) > 2) {
    vif_orig <- car::vif(model)
    if (is.matrix(vif_orig)) max(vif_orig[, "GVIF^(1/(2*Df))"]^2) else max(vif_orig)
  } else {
    NA
  }
  
  final_vif <- if (ncol(model.matrix(final_model)) > 2) {
    vif_final <- car::vif(final_model)
    if (is.matrix(vif_final)) max(vif_final[, "GVIF^(1/(2*Df))"]^2) else max(vif_final)
  } else {
    NA
  }
  
  if (verbose) {
    cat("\n--- HASIL PERBAIKAN ---\n")
    cat("Variabel dihapus:", ifelse(length(removed_vars) > 0, 
                                   paste(removed_vars, collapse = ", "), "Tidak ada"), "\n")
    cat("VIF maksimum sebelum:", ifelse(is.na(original_vif), "NA", round(original_vif, 4)), "\n")
    cat("VIF maksimum sesudah:", ifelse(is.na(final_vif), "NA", round(final_vif, 4)), "\n")
    cat("R-squared sebelum:", round(summary(model)$r.squared, 4), "\n")
    cat("R-squared sesudah:", round(summary(final_model)$r.squared, 4), "\n")
  }
  
  # Result
  hasil <- list(
    original_model = model,
    fixed_model = final_model,
    removed_variables = removed_vars,
    method_used = method,
    improvement = length(removed_vars) > 0,
    original_vif_max = original_vif,
    final_vif_max = final_vif,
    original_r_squared = summary(model)$r.squared,
    final_r_squared = summary(final_model)$r.squared,
    final_formula = current_formula
  )
  
  class(hasil) <- "perbaikan_multikolinearitas"
  return(hasil)
}

#' Print method untuk perbaikan_multikolinearitas
#' @export
print.perbaikan_multikolinearitas <- function(x, ...) {
  cat("\n========== HASIL PERBAIKAN MULTIKOLINEARITAS ==========\n\n")
  
  cat("Metode yang digunakan:", x$method_used, "\n")
  cat("Perbaikan dilakukan:", ifelse(x$improvement, "Ya", "Tidak"), "\n\n")
  
  if (x$improvement) {
    cat("Variabel yang dihapus:\n")
    for (i in 1:length(x$removed_variables)) {
      cat(" ", i, ".", x$removed_variables[i], "\n")
    }
    
    cat("\nPerbandingan Model:\n")
    cat("                    | Sebelum  | Sesudah  | Perubahan\n")
    cat("--------------------|----------|----------|----------\n")
    cat("VIF Maksimum        |", 
        sprintf("%8.4f", ifelse(is.na(x$original_vif_max), 0, x$original_vif_max)), "|",
        sprintf("%8.4f", ifelse(is.na(x$final_vif_max), 0, x$final_vif_max)), "|",
        sprintf("%8.4f", ifelse(is.na(x$original_vif_max) || is.na(x$final_vif_max), 0, 
                               x$final_vif_max - x$original_vif_max)), "\n")
    cat("R-squared           |", 
        sprintf("%8.4f", x$original_r_squared), "|",
        sprintf("%8.4f", x$final_r_squared), "|",
        sprintf("%8.4f", x$final_r_squared - x$original_r_squared), "\n")
    
    cat("\nFormula baru:\n")
    cat(deparse(x$final_formula), "\n")
    
  } else {
    cat("Tidak ada variabel yang perlu dihapus.\n")
  }
  
  cat("\n=====================================================\n")
}