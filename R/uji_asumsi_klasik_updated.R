#' Uji Asumsi Klasik Lengkap dengan Perbaikan Otomatis
#'
#' Melakukan semua uji asumsi klasik sekaligus dengan opsi perbaikan otomatis
#'
#' @param model Objek model lm
#' @param alpha Tingkat signifikansi (default = 0.05)
#' @param plot Logical, apakah menampilkan plot (default = TRUE)
#' @param fix_multicollinearity Logical, apakah melakukan perbaikan multikolinearitas (default = FALSE)
#' @param fix_heteroscedasticity Logical, apakah melakukan perbaikan heteroskedastisitas (default = FALSE)
#' @param multicollinearity_method Metode perbaikan multikolinearitas (default = "stepwise")
#' @param heteroscedasticity_method Metode perbaikan heteroskedastisitas (default = "fitted")
#' @param transform Logical, apakah melakukan transformasi otomatis jika asumsi dilanggar (default = FALSE)
#' @return List berisi hasil semua uji asumsi klasik dan perbaikan
#' @export
uji_asumsi_klasik_plus <- function(model, alpha = 0.05, plot = TRUE, 
                                 fix_multicollinearity = FALSE,
                                 fix_heteroscedasticity = FALSE,
                                 multicollinearity_method = "stepwise",
                                 heteroscedasticity_method = "fitted",
                                 transform = FALSE) {
  if (!inherits(model, "lm")) {
    stop("Input harus berupa objek lm")
  }
  
  cat("\n====== UJI ASUMSI KLASIK REGRESI LINEAR PLUS ======\n")
  
  # Simpan model original
  original_model <- model
  current_model <- model
  
  # 1. Uji Multikolinearitas terlebih dahulu (jika ada lebih dari 1 prediktor)
  multi_result <- NULL
  multi_fix_result <- NULL
  
  if (ncol(model.matrix(current_model)) > 2) {
    cat("\n[1/5] Menguji Multikolinearitas...\n")
    multi_result <- uji_multikolinearitas(current_model, plot = plot)
    
    # Perbaikan multikolinearitas jika diminta dan terdeteksi
    if (fix_multicollinearity && multi_result$multicollinearity_detected) {
      cat("\n[PERBAIKAN] Mengatasi multikolinearitas...\n")
      multi_fix_result <- perbaikan_multikolinearitas(current_model, 
                                                     method = multicollinearity_method,
                                                     verbose = TRUE)
      if (multi_fix_result$improvement) {
        current_model <- multi_fix_result$fixed_model
        cat("[✓] Multikolinearitas berhasil diperbaiki!\n")
      }
    }
  } else {
    cat("\n[SKIP] Model hanya memiliki satu prediktor - multikolinearitas tidak relevan\n")
  }
  
  # 2. Uji Normalitas
  cat("\n[2/5] Menguji Normalitas...\n")
  norm_result <- uji_normalitas(current_model, alpha = alpha, plot = plot)
  
  # 3. Uji Homoskedastisitas
  cat("\n[3/5] Menguji Homoskedastisitas...\n")
  homo_result <- uji_homoskedastisitas(current_model, alpha = alpha, plot = plot)
  
  # Perbaikan heteroskedastisitas jika diminta
  hetero_fix_result <- NULL
  if (fix_heteroscedasticity) {
    # Check if heteroscedasticity exists
    homo_pass <- all(c(
      homo_result$breusch_pagan$keputusan == "Homoskedastis",
      homo_result$glejser_test$keputusan == "Homoskedastis"
    ))
    
    if (!homo_pass) {
      cat("\n[PERBAIKAN] Mengatasi heteroskedastisitas dengan WLS...\n")
      hetero_fix_result <- perbaikan_heteroskedastisitas(current_model,
                                                       weight_method = heteroscedasticity_method,
                                                       alpha = alpha,
                                                       verbose = TRUE)
      if (hetero_fix_result$improvement) {
        current_model <- hetero_fix_result$wls_model
        cat("[✓] Heteroskedastisitas berhasil diperbaiki dengan WLS!\n")
        
        # Re-test homoscedasticity on fixed model
        cat("\n[RE-TEST] Menguji ulang homoskedastisitas setelah WLS...\n")
        homo_result_after <- uji_homoskedastisitas(current_model, alpha = alpha, plot = FALSE)
      }
    }
  }
  
  # 4. Uji Autokorelasi
  cat("\n[4/5] Menguji Autokorelasi...\n")
  auto_result <- uji_autokorelasi(current_model, alpha = alpha, plot = plot)
  
  # 5. Transformasi jika diminta dan masih ada pelanggaran
  trans_result <- NULL
  if (transform) {
    # Re-check normalitas dan homoskedastisitas pada model akhir
    norm_check <- uji_normalitas(current_model, alpha = alpha, plot = FALSE)
    homo_check <- uji_homoskedastisitas(current_model, alpha = alpha, plot = FALSE)
    
    norm_pass <- all(c(
      norm_check$shapiro_wilk$keputusan == "Normal",
      norm_check$kolmogorov_smirnov$keputusan == "Normal"
    ))
    
    homo_pass <- all(c(
      homo_check$breusch_pagan$keputusan == "Homoskedastis",
      homo_check$glejser_test$keputusan == "Homoskedastis"
    ))
    
    if (!norm_pass || !homo_pass) {
      cat("\n[5/5] Melakukan transformasi otomatis...\n")
      
      # Extract formula and data from current model
      formula_current <- formula(current_model)
      data_current <- current_model$model
      
      trans_result <- transformasi_otomatis(data_current, formula_current, alpha = alpha)
      
      if (!is.null(trans_result$best_model)) {
        current_model <- trans_result$best_model
        cat("[✓] Transformasi berhasil diterapkan!\n")
      }
    }
  }
  
  # Final assessment
  final_norm <- uji_normalitas(current_model, alpha = alpha, plot = FALSE)
  final_homo <- uji_homoskedastisitas(current_model, alpha = alpha, plot = FALSE)
  final_auto <- uji_autokorelasi(current_model, alpha = alpha, plot = FALSE)
  final_multi <- if (ncol(model.matrix(current_model)) > 2) {
    uji_multikolinearitas(current_model, plot = FALSE)
  } else {
    NULL
  }
  
  # Summary of final status
  final_norm_pass <- all(c(
    final_norm$shapiro_wilk$keputusan == "Normal",
    final_norm$kolmogorov_smirnov$keputusan == "Normal"
  ))
  
  final_homo_pass <- all(c(
    final_homo$breusch_pagan$keputusan == "Homoskedastis",
    final_homo$glejser_test$keputusan == "Homoskedastis"
  ))
  
  final_auto_pass <- all(c(
    final_auto$durbin_watson$keputusan == "Tidak ada autokorelasi",
    final_auto$breusch_godfrey$keputusan == "Tidak ada autokorelasi"
  ))
  
  final_multi_pass <- if (!is.null(final_multi)) {
    !final_multi$multicollinearity_detected
  } else {
    TRUE
  }
  
  # Hasil lengkap
  hasil <- list(
    # Model progression
    original_model = original_model,
    final_model = current_model,
    model_changed = !identical(original_model, current_model),
    
    # Original test results
    original_tests = list(
      normalitas = norm_result,
      homoskedastisitas = homo_result,
      autokorelasi = auto_result,
      multikolinearitas = multi_result
    ),
    
    # Final test results
    final_tests = list(
      normalitas = final_norm,
      homoskedastisitas = final_homo,
      autokorelasi = final_auto,
      multikolinearitas = final_multi
    ),
    
    # Fix results
    perbaikan = list(
      multikolinearitas = multi_fix_result,
      heteroskedastisitas = hetero_fix_result,
      transformasi = trans_result
    ),
    
    # Summary
    summary = list(
      original_status = list(
        normalitas_pass = all(c(
          norm_result$shapiro_wilk$keputusan == "Normal",
          norm_result$kolmogorov_smirnov$keputusan == "Normal"
        )),
        homoskedastisitas_pass = all(c(
          homo_result$breusch_pagan$keputusan == "Homoskedastis",
          homo_result$glejser_test$keputusan == "Homoskedastis"
        )),
        autokorelasi_pass = all(c(
          auto_result$durbin_watson$keputusan == "Tidak ada autokorelasi",
          auto_result$breusch_godfrey$keputusan == "Tidak ada autokorelasi"
        )),
        multikolinearitas_pass = if (!is.null(multi_result)) {
          !multi_result$multicollinearity_detected
        } else {
          TRUE
        }
      ),
      final_status = list(
        normalitas_pass = final_norm_pass,
        homoskedastisitas_pass = final_homo_pass,
        autokorelasi_pass = final_auto_pass,
        multikolinearitas_pass = final_multi_pass,
        all_pass = final_norm_pass && final_homo_pass && final_auto_pass && final_multi_pass
      ),
      improvements_made = list(
        multikolinearitas = !is.null(multi_fix_result) && multi_fix_result$improvement,
        heteroskedastisitas = !is.null(hetero_fix_result) && hetero_fix_result$improvement,
        transformasi = !is.null(trans_result)
      )
    )
  )
  
  class(hasil) <- "uji_asumsi_klasik_plus"
  return(hasil)
}

#' Print method untuk uji_asumsi_klasik_plus
#' @export
print.uji_asumsi_klasik_plus <- function(x, ...) {
  cat("\n\n========== RINGKASAN UJI ASUMSI KLASIK PLUS ==========\n\n")
  
  # Status original vs final
  cat("STATUS ASUMSI KLASIK:\n")
  cat("                     | Original | Final   | Status\n")
  cat("---------------------|----------|---------|----------\n")
  
  assumptions <- c("Normalitas", "Homoskedastisitas", "Autokorelasi", "Multikolinearitas")
  orig_status <- c(
    x$summary$original_status$normalitas_pass,
    x$summary$original_status$homoskedastisitas_pass,
    x$summary$original_status$autokorelasi_pass,
    x$summary$original_status$multikolinearitas_pass
  )
  final_status <- c(
    x$summary$final_status$normalitas_pass,
    x$summary$final_status$homoskedastisitas_pass,
    x$summary$final_status$autokorelasi_pass,
    x$summary$final_status$multikolinearitas_pass
  )
  
  for (i in 1:4) {
    orig_symbol <- ifelse(orig_status[i], "PASS ✓", "FAIL ✗")
    final_symbol <- ifelse(final_status[i], "PASS ✓", "FAIL ✗")
    
    status_change <- if (!orig_status[i] && final_status[i]) {
      "Diperbaiki"
    } else if (orig_status[i] && final_status[i]) {
      "Tetap baik"
    } else if (orig_status[i] && !final_status[i]) {
      "Memburuk"
    } else {
      "Belum teratasi"
    }
    
    cat(sprintf("%-20s | %-8s | %-7s | %s\n", 
                assumptions[i], orig_symbol, final_symbol, status_change))
  }
  
  cat("\nSTATUS KESELURUHAN:\n")
  cat("Original:", ifelse(all(orig_status), "Semua asumsi terpenuhi ✓", "Ada asumsi yang dilanggar ✗"), "\n")
  cat("Final   :", ifelse(x$summary$final_status$all_pass, "Semua asumsi terpenuhi ✓", "Ada asumsi yang dilanggar ✗"), "\n")
  
  # Perbaikan yang dilakukan
  if (any(unlist(x$summary$improvements_made))) {
    cat("\nPERBAIKAN YANG DILAKUKAN:\n")
    
    if (x$summary$improvements_made$multikolinearitas) {
      cat("✓ Multikolinearitas: Menghapus", length(x$perbaikan$multikolinearitas$removed_variables), 
          "variabel (", paste(x$perbaikan$multikolinearitas$removed_variables, collapse = ", "), ")\n")
    }
    
    if (x$summary$improvements_made$heteroskedastisitas) {
      cat("✓ Heteroskedastisitas: WLS dengan metode", x$perbaikan$heteroskedastisitas$weight_method, "\n")
    }
    
    if (x$summary$improvements_made$transformasi) {
      cat("✓ Transformasi:", x$perbaikan$transformasi$best_transformation, "\n")
    }
  } else {
    cat("\nTidak ada perbaikan yang dilakukan.\n")
  }
  
  # Model comparison
  if (x$model_changed) {
    cat("\nPERBANDINGAN MODEL:\n")
    orig_r2 <- summary(x$original_model)$r.squared
    final_r2 <- summary(x$final_model)$r.squared
    
    cat("R-squared original:", round(orig_r2, 4), "\n")
    cat("R-squared final   :", round(final_r2, 4), "\n")
    cat("Perubahan R-squared:", round(final_r2 - orig_r2, 4), "\n")
    
    # AIC comparison if available
    tryCatch({
      orig_aic <- AIC(x$original_model)
      final_aic <- AIC(x$final_model)
      cat("AIC original:", round(orig_aic, 2), "\n")
      cat("AIC final   :", round(final_aic, 2), "\n")
      cat("Perubahan AIC:", round(final_aic - orig_aic, 2), 
          ifelse(final_aic < orig_aic, "(Membaik)", "(Memburuk)"), "\n")
    }, error = function(e) {
      # AIC comparison not possible (e.g., different sample sizes)
    })
  }
  
  cat("\n====================================================\n")
  
  cat("\nAkses komponen hasil:\n")
  cat("- $original_model    : Model original\n")
  cat("- $final_model       : Model setelah perbaikan\n")
  cat("- $original_tests    : Hasil uji pada model original\n")
  cat("- $final_tests       : Hasil uji pada model final\n")
  cat("- $perbaikan         : Detail perbaikan yang dilakukan\n")
  cat("- $summary           : Ringkasan status dan perbaikan\n")
}