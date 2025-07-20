#' Transformasi Variabel
#'
#' Melakukan transformasi pada variabel dependen untuk mengatasi pelanggaran asumsi
#'
#' @param data Data frame
#' @param formula Formula model
#' @param max_iter Maksimum iterasi transformasi (default = 3)
#' @param alpha Tingkat signifikansi (default = 0.05)
#' @param verbose Logical, apakah menampilkan proses (default = TRUE)
#' @return List berisi model terbaik dan hasil transformasi
#' @export
transformasi_otomatis <- function(data, formula, max_iter = 3, alpha = 0.05, verbose = TRUE) {
  # Daftar transformasi
  transformations <- list(
    "original" = function(y) y,
    "log" = function(y) {
      if (any(y <= 0)) {
        warning("Data mengandung nilai <= 0, menambahkan konstanta kecil")
        y <- y + abs(min(y)) + 0.001
      }
      log(y)
    },
    "1/x" = function(y) {
      if (any(y == 0)) {
        warning("Data mengandung nilai 0, menambahkan konstanta kecil")
        y <- y + 0.001
      }
      1/y
    },
    "1/x^2" = function(y) {
      if (any(y == 0)) {
        warning("Data mengandung nilai 0, menambahkan konstanta kecil")
        y <- y + 0.001
      }
      1/y^2
    }
  )
  
  # Extract response variable name
  response_var <- all.vars(formula)[1]
  
  results <- list()
  
  for (i in 1:length(transformations)) {
    trans_name <- names(transformations)[i]
    
    if (verbose) {
      cat("\n--- Mencoba transformasi:", trans_name, "---\n")
    }
    
    # Copy data
    data_trans <- data
    
    # Apply transformation
    tryCatch({
      data_trans[[response_var]] <- transformations[[i]](data[[response_var]])
      
      # Fit model
      model <- lm(formula, data = data_trans)
      
      # Test assumptions
      norm_test <- uji_normalitas(model, alpha = alpha, plot = FALSE)
      homo_test <- uji_homoskedastisitas(model, alpha = alpha, plot = FALSE)
      
      # Check if all normality tests pass
      norm_pass <- all(c(
        norm_test$shapiro_wilk$keputusan == "Normal",
        norm_test$kolmogorov_smirnov$keputusan == "Normal"
      ))
      
      # Check if homoscedasticity tests pass
      homo_pass <- all(c(
        homo_test$breusch_pagan$keputusan == "Homoskedastis",
        homo_test$glejser_test$keputusan == "Homoskedastis"
      ))
      
      results[[trans_name]] <- list(
        model = model,
        transformation = trans_name,
        normalitas = norm_pass,
        homoskedastisitas = homo_pass,
        norm_test = norm_test,
        homo_test = homo_test,
        all_pass = norm_pass && homo_pass
      )
      
      if (verbose) {
        cat("Normalitas:", ifelse(norm_pass, "PASS ✓", "FAIL ✗"), "\n")
        cat("Homoskedastisitas:", ifelse(homo_pass, "PASS ✓", "FAIL ✗"), "\n")
      }
      
      # If all assumptions are met, stop
      if (norm_pass && homo_pass && i < length(transformations)) {
        if (verbose) {
          cat("\n[✓] Semua asumsi terpenuhi dengan transformasi", trans_name, "\n")
        }
        break
      }
      
    }, error = function(e) {
      if (verbose) {
        cat("Error pada transformasi", trans_name, ":", e$message, "\n")
      }
      results[[trans_name]] <- list(
        model = NULL,
        transformation = trans_name,
        error = e$message
      )
    })
  }
  
  # Find best model
  best_model <- NULL
  best_trans <- "original"
  
  for (res_name in names(results)) {
    res <- results[[res_name]]
    if (!is.null(res$model) && res$all_pass) {
      best_model <- res$model
      best_trans <- res$transformation
      break
    }
  }
  
  # If no model passes all tests, find the best one
  if (is.null(best_model)) {
    for (res_name in names(results)) {
      res <- results[[res_name]]
      if (!is.null(res$model)) {
        if (is.null(best_model) || 
            (res$normalitas && !results[[best_trans]]$normalitas) ||
            (res$homoskedastisitas && !results[[best_trans]]$homoskedastisitas)) {
          best_model <- res$model
          best_trans <- res$transformation
        }
      }
    }
  }
  
  hasil <- list(
    best_model = best_model,
    best_transformation = best_trans,
    all_results = results,
    formula_original = formula,
    data_original = data
  )
  
  class(hasil) <- "transformasi_hasil"
  return(hasil)
}

#' Print method untuk transformasi_hasil
#' @export
print.transformasi_hasil <- function(x, ...) {
  cat("\n========== HASIL TRANSFORMASI ==========\n\n")
  
  cat("Formula original:", deparse(x$formula_original), "\n")
  cat("Transformasi terbaik:", x$best_transformation, "\n\n")
  
  cat("Ringkasan hasil semua transformasi:\n")
  for (trans in names(x$all_results)) {
    res <- x$all_results[[trans]]
    cat("\n", trans, ":\n", sep = "")
    if (!is.null(res$error)) {
      cat("  Error:", res$error, "\n")
    } else if (!is.null(res$model)) {
      cat("  Normalitas:", ifelse(res$normalitas, "PASS ✓", "FAIL ✗"), "\n")
      cat("  Homoskedastisitas:", ifelse(res$homoskedastisitas, "PASS ✓", "FAIL ✗"), "\n")
    }
  }
  
  cat("\n========================================\n")
}