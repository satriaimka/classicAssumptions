#' Generate laporan uji asumsi klasik
#'
#' @param hasil_uji Hasil dari uji_asumsi_klasik
#' @param file Nama file output (opsional)
#' @export
generate_report <- function(hasil_uji, file = NULL) {
  if (!inherits(hasil_uji, "uji_asumsi_klasik_hasil")) {
    stop("Input harus hasil dari fungsi uji_asumsi_klasik()")
  }
  
  report <- c(
    "LAPORAN UJI ASUMSI KLASIK REGRESI LINEAR",
    "=========================================",
    "",
    paste("Tanggal:", Sys.Date()),
    paste("Waktu:", format(Sys.time(), "%H:%M:%S")),
    "",
    "RINGKASAN HASIL",
    "---------------",
    paste("1. Normalitas:", ifelse(hasil_uji$summary$normalitas_pass, "PASS", "FAIL")),
    paste("2. Homoskedastisitas:", ifelse(hasil_uji$summary$homoskedastisitas_pass, "PASS", "FAIL")),
    paste("3. Autokorelasi:", ifelse(hasil_uji$summary$autokorelasi_pass, "PASS", "FAIL")),
    paste("4. Multikolinearitas:", ifelse(hasil_uji$summary$multikolinearitas_pass, "PASS", "FAIL")),
    "",
    "DETAIL UJI NORMALITAS",
    "--------------------",
    paste("Shapiro-Wilk p-value:", round(hasil_uji$normalitas$shapiro_wilk$p_value, 4)),
    paste("Kolmogorov-Smirnov p-value:", round(hasil_uji$normalitas$kolmogorov_smirnov$p_value, 4)),
    paste("Anderson-Darling p-value:", round(hasil_uji$normalitas$anderson_darling$p_value, 4)),
    paste("Jarque-Bera p-value:", round(hasil_uji$normalitas$jarque_bera$p_value, 4)),
    "",
    "DETAIL UJI HOMOSKEDASTISITAS",
    "---------------------------",
    paste("Breusch-Pagan p-value:", round(hasil_uji$homoskedastisitas$breusch_pagan$p_value, 4)),
    paste("Goldfeld-Quandt p-value:", round(hasil_uji$homoskedastisitas$goldfeld_quandt$p_value, 4)),
    paste("Glejser p-value:", round(hasil_uji$homoskedastisitas$glejser_test$p_value, 4)),
    "",
    "DETAIL UJI AUTOKORELASI",
    "----------------------",
    paste("Durbin-Watson statistic:", round(hasil_uji$autokorelasi$durbin_watson$statistic, 4)),
    paste("Durbin-Watson p-value:", round(hasil_uji$autokorelasi$durbin_watson$p_value, 4)),
    paste("Breusch-Godfrey p-value:", round(hasil_uji$autokorelasi$breusch_godfrey$p_value, 4)),
    "",
    "DETAIL UJI MULTIKOLINEARITAS",
    "---------------------------",
    paste("Kondisi multikolinearitas:", ifelse(hasil_uji$multikolinearitas$multicollinearity_detected, 
                                              "TERDETEKSI", "TIDAK TERDETEKSI")),
    paste("Maximum Condition Index:", round(hasil_uji$multikolinearitas$max_condition_index, 4))
  )
  
  if (!is.null(hasil_uji$transformasi)) {
    report <- c(report,
                "",
                "HASIL TRANSFORMASI",
                "-----------------",
                paste("Transformasi terbaik:", hasil_uji$transformasi$best_transformation))
  }
  
  if (!is.null(file)) {
    writeLines(report, file)
    cat("Laporan disimpan ke:", file, "\n")
  } else {
    cat(paste(report, collapse = "\n"))
  }
  
  invisible(report)
}