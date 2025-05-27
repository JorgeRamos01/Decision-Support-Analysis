library(forecast)
library(ggplot2)

# Función para calcular la tendencia lineal y las medidas de error
calcular_tendencia_lineal <- function(periodos, valores) {
  # Asegurarse de que los periodos y valores sean vectores numéricos
  periodos <- as.numeric(periodos)
  valores <- as.numeric(valores)
  
  # Crear la serie de tiempo
  serie_time <- ts(valores, start = periodos[1], frequency = 1)
  
  # Ajustar un modelo de tendencia lineal
  modelo_tendencia <- tslm(serie_time ~ trend)
  
  # Obtener los valores ajustados
  valores_ajustados <- fitted(modelo_tendencia)
  
  # Calcular las medidas de error
  valores_observados <- valores
  mae <- mean(abs(valores_observados - valores_ajustados), na.rm = TRUE)
  mse <- mean((valores_observados - valores_ajustados)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  mape <- mean(abs((valores_observados - valores_ajustados) / valores_observados) * 100, na.rm = TRUE)
  
  # Crear data frames para la gráfica
  df_original <- data.frame(time = periodos, value = valores)
  df_ajustado <- data.frame(time = periodos, value = valores_ajustados)
  
  # Crear la gráfica
  grafica <- ggplot() +
    geom_line(data = df_original, aes(x = time, y = value), color = "black") +
    geom_line(data = df_ajustado, aes(x = time, y = value), color = "blue", linetype = "dashed") +
    labs(title = "Tendencia Lineal",
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

# Ejemplo de uso de la función con vectores de periodos y valores
periodos <- 1:12
valores <- c(100, 105, 102, 108, 111, 115, 117, 120, 122, 125, 128, 130)

# Ejecutar la función con los vectores
calcular_tendencia_lineal(periodos, valores)