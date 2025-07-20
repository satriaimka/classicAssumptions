#' Uji Normalitas Residual
#'
#' Melakukan berbagai uji normalitas pada residual model regresi
#'
#' @param model Objek model lm
#' @param alpha Tingkat signifikansi (default = 0.05)
#' @param plot Logical, apakah menampilkan plot (default = TRUE)
#' @return List berisi hasil uji normalitas
#' @export
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' uji_normalitas(model)
uji_normalitas <- function(model, alpha = 0.05, plot = TRUE) {
  if (!inherits(model, "lm")) {
    stop("Input harus berupa objek lm")
  }
  
  # Ekstrak residual
  residuals <- residuals(model)
  
  # Shapiro-Wilk test
  sw_test <- shapiro.test(residuals)
  
  # Kolmogorov-Smirnov test
  ks_test <- ks.test(residuals, "pnorm", 
                      mean = mean(residuals), 
                      sd = sd(residuals))
  
  # Anderson-Darling test
  ad_test <- nortest::ad.test(residuals)
  
  # Jarque-Bera test
  jb_test <- tseries::jarque.bera.test(residuals)
  
  # Plot jika diminta
  if (plot) {
    par(mfrow = c(2, 2))
    
    # Histogram
    hist(residuals, main = "Histogram Residual", 
         xlab = "Residual", freq = FALSE, col = "lightblue")
    curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), 
          add = TRUE, col = "red", lwd = 2)
    
    # Q-Q plot
    qqnorm(residuals, main = "Q-Q Plot")
    qqline(residuals, col = "red")
    
    # Density plot
    plot(density(residuals), main = "Density Plot Residual", 
         xlab = "Residual", ylab = "Density")
    
    # Boxplot
    boxplot(residuals, main = "Boxplot Residual", 
            ylab = "Residual", col = "lightgreen")
    
    par(mfrow = c(1, 1))
  }
  
  # Hasil
  hasil <- list(
    shapiro_wilk = list(
      statistic = sw_test$statistic,
      p_value = sw_test$p.value,
      keputusan = ifelse(sw_test$p.value > alpha, "Normal", "Tidak Normal")
    ),
    kolmogorov_smirnov = list(
      statistic = ks_test$statistic,
      p_value = ks_test$p.value,
      keputusan = ifelse(ks_test$p.value > alpha, "Normal", "Tidak Normal")
    ),
    anderson_darling = list(
      statistic = ad_test$statistic,
      p_value = ad_test$p.value,
      keputusan = ifelse(ad_test$p.value > alpha, "Normal", "Tidak Normal")
    ),
    jarque_bera = list(
      statistic = jb_test$statistic,
      p_value = jb_test$p.value,
      keputusan = ifelse(jb_test$p.value > alpha, "Normal", "Tidak Normal")
    )
  )
  
  class(hasil) <- "uji_normalitas"
  return(hasil)
}

#' Print method untuk uji_normalitas
#' @export
print.uji_normalitas <- function(x, ...) {
  cat("\n========== UJI NORMALITAS RESIDUAL ==========\n\n")
  
  cat("1. Shapiro-Wilk Test:\n")
  cat("   Statistic:", round(x$shapiro_wilk$statistic, 4), "\n")
  cat("   P-value:", round(x$shapiro_wilk$p_value, 4), "\n")
  cat("   Keputusan:", x$shapiro_wilk$keputusan, "\n\n")
  
  cat("2. Kolmogorov-Smirnov Test:\n")
  cat("   Statistic:", round(x$kolmogorov_smirnov$statistic, 4), "\n")
  cat("   P-value:", round(x$kolmogorov_smirnov$p_value, 4), "\n")
  cat("   Keputusan:", x$kolmogorov_smirnov$keputusan, "\n\n")
  
  cat("3. Anderson-Darling Test:\n")
  cat("   Statistic:", round(x$anderson_darling$statistic, 4), "\n")
  cat("   P-value:", round(x$anderson_darling$p_value, 4), "\n")
  cat("   Keputusan:", x$anderson_darling$keputusan, "\n\n")
  
  cat("4. Jarque-Bera Test:\n")
  cat("   Statistic:", round(x$jarque_bera$statistic, 4), "\n")
  cat("   P-value:", round(x$jarque_bera$p_value, 4), "\n")
  cat("   Keputusan:", x$jarque_bera$keputusan, "\n")
  
  cat("\n============================================\n")
}