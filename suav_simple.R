# Instalar y cargar las librerías necesarias
if (!require(forecast)) install.packages("forecast")
if (!require(ggplot2)) install.packages("ggplot2")
library(forecast)
library(ggplot2)

# Función para calcular el suavizamiento exponencial simple y las medidas de error
calcular_suavizamiento_exponencial <- function(periodos, valores, alpha = 0.2) {
  # Asegurarse de que los periodos y valores sean vectores numéricos
  periodos <- as.numeric(periodos)
  valores <- as.numeric(valores)
  
  # Crear la serie de tiempo
  serie_time <- ts(valores, start = periodos[1], frequency = 1)
  
  # Aplicar el suavizamiento exponencial simple
  modelo_ses <- ses(serie_time, alpha = alpha)
  
  # Obtener los valores suavizados
  valores_suavizados <- fitted(modelo_ses)
  
  # Calcular las medidas de error
  valores_observados <- valores
  mae <- mean(abs(valores_observados - valores_suavizados), na.rm = TRUE)
  mse <- mean((valores_observados - valores_suavizados)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  mape <- mean(abs((valores_observados - valores_suavizados) / valores_observados) * 100, na.rm = TRUE)
  
  # Crear data frames para la gráfica
  df_original <- data.frame(time = periodos, value = valores)
  df_suavizado <- data.frame(time = periodos, value = valores_suavizados)
  
  # Crear la gráfica
  grafica <- ggplot() +
    geom_line(data = df_original, aes(x = time, y = value), color = "black") +
    geom_line(data = df_suavizado, aes(x = time, y = value), color = "blue", linetype = "dashed") +
    labs(title = paste("Suavizamiento Exponencial Simple (alpha =", alpha, ")"),
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

# Ejecutar la función con los vectores y un valor de alpha
calcular_suavizamiento_exponencial(periodos, valores, alpha = 0.3)