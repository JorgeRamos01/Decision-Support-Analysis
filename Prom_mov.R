library(forecast)
library(ggplot2)
library(zoo)

# Función para calcular el promedio móvil simple y las medidas de error usando vectores
calcular_promedio_movil_vectores <- function(periodos, valores, N) {
  # Asegurarse de que los periodos y valores sean vectores
  periodos <- as.numeric(periodos)
  valores <- as.numeric(valores)
  
  # Crear la serie de tiempo
  serie_time <- ts(valores, start = periodos[1], frequency = 1)
  
  # Calcular el promedio móvil simple
  promedio_movil <- rollmean(serie_time, k = N, fill = NA)
  
  # Extraer los valores del promedio móvil y las observaciones correspondientes para los cálculos
  valores_observados <- valores[N:length(valores)]
  valores_promedio_movil <- promedio_movil[N:length(serie_time)]
  
  # Calcular las medidas de error
  mae <- mean(abs(valores_observados - valores_promedio_movil), na.rm = TRUE)
  mse <- mean((valores_observados - valores_promedio_movil)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  mape <- mean(abs((valores_observados - valores_promedio_movil) / valores_observados) * 100, na.rm = TRUE)
  
  # Crear data frames para la gráfica
  df_original <- data.frame(time = periodos, value = valores)
  df_promedio_movil <- data.frame(time = periodos, value = c(rep(NA, N-1), valores_promedio_movil))
  
  # Crear la gráfica
  grafica <- ggplot() +
    geom_line(data = df_original, aes(x = time, y = value), color = "black") +
    geom_line(data = df_promedio_movil, aes(x = time, y = value), color = "blue", linetype = "dashed") +
    labs(title = paste("Promedio Móvil Simple (N =", N, ")"),
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

# Ejecutar la función con los vectores y un valor de N
calcular_promedio_movil_vectores(periodos, valores, N = 3)
