#' Uji Asumsi Klasik Lengkap
#'
#' Melakukan semua uji asumsi klasik sekaligus
#'
#' @param model Objek model lm
#' @param alpha Tingkat signifikansi (default = 0.05)
#' @param plot Logical, apakah menampilkan plot (default = TRUE)
#' @param transform Logical, apakah melakukan transformasi otomatis jika asumsi dilanggar (default = FALSE)
#' @return List berisi hasil semua uji asumsi klasik
#' @export
uji_asumsi_klasik <- function(model, alpha = 0.05, plot = TRUE, transform = FALSE) {
  if (!inherits(model, "lm")) {
    stop("Input harus berupa objek lm")
  }
  
  cat("\n====== UJI ASUMSI KLASIK REGRESI LINEAR ======\n")
  
  # 1. Uji Normalitas
  cat("\n[1/4] Menguji Normalitas...\n")
  norm_result <- uji_normalitas(model, alpha = alpha, plot = plot)
  
  # 2. Uji Homoskedastisitas
  cat("\n[2/4] Menguji Homoskedastisitas...\n")
  homo_result <- uji_homoskedastisitas(model, alpha = alpha, plot = plot)
  
  # 3. Uji Autokorelasi
  cat("\n[3/4] Menguji Autokorelasi...\n")
  auto_result <- uji_autokorelasi(model, alpha = alpha, plot = plot)
  
  # 4. Uji Multikolinearitas
  cat("\n[4/4] Menguji Multikolinearitas...\n")
  multi_result <- uji_multikolinearitas(model, plot = plot)
  
  # Summary
  norm_pass <- all(c(
    norm_result$shapiro_wilk$keputusan == "Normal",
    norm_result$kolmogorov_smirnov$keputusan == "Normal"
  ))
  
  homo_pass <- all(c(
    homo_result$breusch_pagan$keputusan == "Homoskedastis",
    homo_result$glejser_test$keputusan == "Homoskedastis"
  ))
  
  auto_pass <- all(c(
    auto_result$durbin_watson$keputusan == "Tidak ada autokorelasi",
    auto_result$breusch_godfrey$keputusan == "Tidak ada autokorelasi"
  ))
  
  multi_pass <- !multi_result$multicollinearity_detected
  
  # Hasil
  hasil <- list(
    normalitas = norm_result,
    homoskedastisitas = homo_result,
    autokorelasi = auto_result,
    multikolinearitas = multi_result,
    summary = list(
      normalitas_pass = norm_pass,
      homoskedastisitas_pass = homo_pass,
      autokorelasi_pass = auto_pass,
      multikolinearitas_pass = multi_pass,
      all_pass = norm_pass && homo_pass && auto_pass && multi_pass
    ),
    model_original = model
  )
  
  # Transformasi jika diminta dan ada pelanggaran
  if (transform && (!norm_pass || !homo_pass)) {
    cat("\n\n====== TRANSFORMASI OTOMATIS ======\n")
    
    # Extract formula and data from model
    formula <- formula(model)
    data <- model$model
    
    trans_result <- transformasi_otomatis(data, formula, alpha = alpha)
    hasil$transformasi <- trans_result
    
    # Re-test assumptions on transformed model if successful
    if (!is.null(trans_result$best_model)) {
      cat("\n\n====== UJI ULANG SETELAH TRANSFORMASI ======\n")
      hasil$after_transform <- list(
        normalitas = uji_normalitas(trans_result$best_model, alpha = alpha, plot = FALSE),
        homoskedastisitas = uji_homoskedastisitas(trans_result$best_model, alpha = alpha, plot = FALSE)
      )
    }
  }
  
  class(hasil) <- "uji_asumsi_klasik_hasil"
  return(hasil)
}

#' Print method untuk uji_asumsi_klasik_hasil
#' @export
print.uji_asumsi_klasik_hasil <- function(x, ...) {
  cat("\n\n========== RINGKASAN UJI ASUMSI KLASIK ==========\n\n")
  
  cat("1. Normalitas:", ifelse(x$summary$normalitas_pass, "PASS ✓", "FAIL ✗"), "\n")
  cat("2. Homoskedastisitas:", ifelse(x$summary$homoskedastisitas_pass, "PASS ✓", "FAIL ✗"), "\n")
  cat("3. Autokorelasi:", ifelse(x$summary$autokorelasi_pass, "PASS ✓", "FAIL ✗"), "\n")
  cat("4. Multikolinearitas:", ifelse(x$summary$multikolinearitas_pass, "PASS ✓", "FAIL ✗"), "\n")
  
  cat("\nStatus keseluruhan:", ifelse(x$summary$all_pass, 
                                      "Semua asumsi terpenuhi ✓", 
                                      "Ada asumsi yang dilanggar ✗"), "\n")
  
  if (!is.null(x$transformasi)) {
    cat("\n--- Hasil Transformasi ---\n")
    cat("Transformasi terbaik:", x$transformasi$best_transformation, "\n")
    
    if (!is.null(x$after_transform)) {
      norm_pass_after <- all(c(
        x$after_transform$normalitas$shapiro_wilk$keputusan == "Normal",
        x$after_transform$normalitas$kolmogorov_smirnov$keputusan == "Normal"
      ))
      
      homo_pass_after <- all(c(
        x$after_transform$homoskedastisitas$breusch_pagan$keputusan == "Homoskedastis",
        x$after_transform$homoskedastisitas$glejser_test$keputusan == "Homoskedastis"
      ))
      
      cat("\nSetelah transformasi:\n")
      cat("- Normalitas:", ifelse(norm_pass_after, "PASS ✓", "FAIL ✗"), "\n")
      cat("- Homoskedastisitas:", ifelse(homo_pass_after, "PASS ✓", "FAIL ✗"), "\n")
    }
  }
  
  cat("\n=================================================\n")
  
  cat("\nUntuk detail lengkap, akses komponen berikut:\n")
  cat("- $normalitas\n")
  cat("- $homoskedastisitas\n")
  cat("- $autokorelasi\n")
  cat("- $multikolinearitas\n")
  if (!is.null(x$transformasi)) {
    cat("- $transformasi\n")
  }
}