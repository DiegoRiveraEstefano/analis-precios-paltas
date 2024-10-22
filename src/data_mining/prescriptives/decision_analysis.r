library(rpart)
library(rpart.plot)

decision_analysis <- function(data) {
  # Construcción del árbol de decisión para recomendar cambios de precios
  modelo_arbol_prescriptivo <- rpart(
    AveragePrice ~ Total.Volume + year + region,
    data = data, method = "anova"
  )

  # Visualización del árbol de decisión prescriptivo
  rpart.plot(modelo_arbol_prescriptivo, main = "Árbol de Decisión Prescriptivo")
}
