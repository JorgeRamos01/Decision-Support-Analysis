# Cargar las librerías necesarias
library(car) # Para VIF
library(lmtest) # Para la prueba de Goldfeld-Quandt
library(nortest) # Para la prueba de Shapiro-Wilk
library(dplyr) # Para el manejo del dataframe

# Crear un dataframe de ejemplo
set.seed(123)
example_data <- data.frame(
  y = rnorm(100, mean = 50, sd = 10),
  x1 = rnorm(100, mean = 5, sd = 2),
  x2 = rnorm(100, mean = 10, sd = 5),
  x3 = rnorm(100, mean = 20, sd = 3)
)

# Definir la función para realizar la regresión y pruebas
regression_analysis <- function(dependent_col, independent_cols, dataframe) {
  # Crear la fórmula para la regresión
  formula <- as.formula(paste(dependent_col, "~", paste(independent_cols, collapse = " + ")))
  
  # Ajustar el modelo de regresión
  model <- lm(formula, data = dataframe)
  print(summary(model))
  
  # Obtener los residuos del modelo
  residuals <- model$residuals
  
  # Prueba de Shapiro-Wilk para normalidad de los residuos
  shapiro_test <- shapiro.test(residuals)
  print(shapiro_test)
  
  # Interpretación de la prueba de Shapiro-Wilk
  if (shapiro_test$p.value > 0.05) {
    print("Los residuos siguen una distribución normal.")
  } else {
    print("Los residuos no siguen una distribución normal.")
  }
  
  # Prueba de Goldfeld-Quandt para heterocedasticidad
  gq_test <- gqtest(model)
  print(gq_test)
  
  # Interpretación de la prueba de Goldfeld-Quandt
  if (gq_test$p.value > 0.05) {
    print("No hay evidencia de heterocedasticidad.")
  } else {
    print("Hay evidencia de heterocedasticidad.")
  }
  
  # Calcular el VIF para detectar multicolinealidad
  vif_values <- vif(model)
  print(vif_values)
  
  # Interpretación de VIF
  if (any(vif_values > 10)) {
    print("Existe presencia de multicolinealidad.")
  } else {
    print("No existe presencia de multicolinealidad.")
  }
}

# Probar la función con el dataframe de ejemplo
regression_analysis("y", c("x1", "x2", "x3"), example_data)