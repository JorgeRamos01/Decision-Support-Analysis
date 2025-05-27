library(forecast)
library(ggplot2)

# Función para calcular el modelo de Holt-Winters y las medidas de error
calcular_holt_winters <- function(periodos, valores, frecuencia, alpha = NULL, beta = NULL, gamma = NULL) {
  # Asegurarse de que los periodos y valores sean vectores numéricos
  periodos <- as.numeric(periodos)
  valores <- as.numeric(valores)
  
  # Crear la serie de tiempo con frecuencia estacional
  serie_time <- ts(valores, start = periodos[1], frequency = frecuencia)
  
  # Ajustar el modelo de Holt-Winters con los parámetros especificados
  modelo_hw <- HoltWinters(serie_time, alpha = alpha, beta = beta, gamma = gamma)
  
  # Obtener los valores ajustados
  valores_ajustados <- fitted(modelo_hw)[,1]
  
  # Ajustar la longitud de los valores observados para coincidir con los valores ajustados
  valores_observados <- valores[(length(valores) - length(valores_ajustados) + 1):length(valores)]
  periodos_ajustados <- periodos[(length(valores) - length(valores_ajustados) + 1):length(periodos)]
  
  # Calcular las medidas de error
  mae <- mean(abs(valores_observados - valores_ajustados), na.rm = TRUE)
  mse <- mean((valores_observados - valores_ajustados)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  mape <- mean(abs((valores_observados - valores_ajustados) / valores_observados) * 100, na.rm = TRUE)
  
  # Crear data frames para la gráfica
  df_original <- data.frame(time = periodos, value = valores)
  df_ajustado <- data.frame(time = periodos_ajustados, value = valores_ajustados)
  
  # Crear la gráfica
  grafica <- ggplot() +
    geom_line(data = df_original, aes(x = time, y = value), color = "black") +
    geom_line(data = df_ajustado, aes(x = time, y = value), color = "blue", linetype = "dashed") +
    labs(title = paste("Holt-Winters (alpha =", alpha, ", beta =", beta, ", gamma =", gamma, ")"),
         x = "Período",
         y = "Valor",
         color = "Serie") +
    theme_minimal()
  
  # Imprimir la gráfica
  print(grafica)
  
  # Imprimir las medidas de error
  cat("MAE:", mae, "\n")
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
  cat("MAPE:", mape, "%\n")
}

# Crear vectores de ejemplo con una frecuencia estacional adecuada.
# En este caso, se utiliza una frecuencia de 4 para simular datos trimestrales.
periodos <- 1:24
valores <- c(100, 105, 102, 108, 111, 115, 113, 117, 120, 118, 122, 125, 
             130, 135, 132, 138, 141, 145, 143, 147, 150, 148, 152, 155)

# Ejecutar la función con los vectores y una frecuencia estacional, y definir los parámetros alpha, beta y gamma
calcular_holt_winters(periodos, valores, frecuencia = 4, alpha = 0.8, beta = 0.2, gamma = 0.2)