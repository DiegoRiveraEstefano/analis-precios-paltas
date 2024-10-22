library(lpSolve)


resource_optimization <- function(data) {
  # Definimos la función objetivo: maximizar ingresos
  beneficios <- c(1.33, 1.35, 0.93) # Precios promedio de ejemplo

  # Definimos las restricciones (volumen máximo por región)
  restricciones <- matrix(c(1036.74, 674.28, 794.7), nrow = 1)

  # Limites de las restricciones
  limite <- c(2000) # Capacidad máxima de distribución

  # Optimización (maximización del beneficio)
  solucion <- lp("max", beneficios, restricciones, "<=", limite, all.int = TRUE)

  # Resultados
  return(solucion)
}
