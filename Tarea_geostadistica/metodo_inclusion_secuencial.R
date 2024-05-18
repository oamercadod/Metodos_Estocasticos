# Instalar y cargar librerías necesarias
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Definir los puntos y el bloque V
block_V <- data.frame(
  x = c(57.5, 57.5, 72.5, 72.5, 57.5),
  y = c(22.5, 32.5, 32.5, 22.5, 22.5)
)

existing_wells <- data.frame(
  x = c(60.6, 60.9),
  y = c(36.2, 17.7)
)

#additional_points <- data.frame(
#  x = c(60.4, 60.9, 56.3, 76.4, 63.9, 77.4, 73.9, 69.4),
#  y = c(37.2, 36.1, 34.6, 23.3, 21.5, 25.3, 34.8, 35.6)
#)
#simulacion 1
#additional_points <- data.frame(
#  x = c(55.08, 69.75, 56.65, 63.12, 68.29, 65.21, 62.23, 72.79),
#  y = c(39.21, 21.85, 39.53, 39.02, 26.52, 32.47, 32.71, 24.92)
#)
#simulacion 2
#additional_points <- data.frame(
#  x = c(77.06, 69.91, 60.10, 55.55, 71.95, 70.47, 68.73, 57.51),
#  y = c(21.71, 39.21, 38.31, 28.39, 33.19, 36.19, 26.17, 22.75)
#)
#simulacion 3
#additional_points <- data.frame(
#  x = c(71.42, 74.09, 71.56, 73.24, 74.34, 60.96, 69.91, 77.12),
#  y = c(21.18, 38.85, 36.20, 23.62, 35.97, 20.52, 33.91, 31.62)
#)
#simulacion 4
#Pozos_adicionales <- data.frame(
#  x = c(63.47, 59.77, 58.04, 66.99, 79.51, 66.82, 56.56, 64.87),
#  y = c(24.88, 33.92, 28.12, 28.13, 30.62, 20.86, 38.47, 27.04)
#)
#simulacion 5
additional_points <- data.frame(
  x = c(74.51, 64.48, 79.01, 65.98, 65.32, 77.61, 65.70, 71.08),
  y = c(25.56, 29.41, 36.19, 25.18, 36.19, 21.74, 24.73, 22.09)
)
#simulacion 6
#additional_points <- data.frame(
#  x = c(65.73, 60.64, 76.16, 76.93, 56.20, 59.52, 59.77, 62.47),
#  y = c(28.42, 27.25, 37.40, 34.41, 29.53, 34.31, 27.73, 33.17)
#)
#simulacion 7
#additional_points <- data.frame(
#  x = c(77.30, 65.73, 74.79, 66.98, 56.63, 74.74, 73.51, 62.63),
#  y = c(33.95, 20.26, 33.56, 22.01, 39.79, 33.28, 29.41, 32.18)
#)
#simulacion 8
#additional_points <- data.frame(
#  x = c(74.96, 77.90, 65.12, 76.64, 70.06, 55.93, 59.39, 67.86),
#  y = c(30.00, 20.92, 22.04, 35.12, 26.36, 29.02, 32.81, 26.02)
#)
#simulacion 9
#additional_points <- data.frame(
#  x = c(66.04, 77.38, 57.00, 59.38, 77.65, 63.36, 62.32, 72.07),
#  y = c(26.49, 30.92, 25.50, 31.76, 23.70, 30.96, 22.10, 24.52)
#)
#simulacion 10
#additional_points <- data.frame(
#  x = c(72.15, 58.86, 76.18, 69.22, 76.62, 77.59, 79.69, 59.98),
#  y = c(26.16, 37.31, 39.33, 36.92, 36.50, 38.12, 36.98, 37.59)
#)

# Función para calcular la distancia euclidiana entre dos puntos
euclidean_distance <- function(p1, p2) {
  sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
}

# Función para calcular la varianza de estimación (suma de las distancias euclidianas al cuadrado)
cal_variancia <- function(existing_wells, selected_points) {
  total_cuadrado_dist <- sum(apply(selected_points, 1, function(point) {
    sum(apply(existing_wells, 1, function(existing_well) euclidean_distance(point, existing_well)^2))
  }))
  return(total_cuadrado_dist)
}

sequential_inclusion_top <- function(existing_wells, additional_points, n, k, top_n = 10) {
  best_variances <- numeric(0)
  best_points_list <- list()
  
  total_variance <- cal_variancia(existing_wells, existing_wells)  # Varianza acumulada
  
  # Generar todas las combinaciones posibles de puntos adicionales
  all_combinations <- combn(nrow(additional_points), k)
  
  for (i in 1:ncol(all_combinations)) {
    selected_indices <- all_combinations[, i]
    selected_points <- additional_points[selected_indices, ]
    
    # Calcular la varianza de estimación con los puntos seleccionados
    variance <- cal_variancia(existing_wells, rbind(existing_wells, selected_points))
    
    # Actualizar las mejores varianzas y combinaciones
    if (length(best_variances) == 0 || variance < max(best_variances, na.rm = TRUE) || length(best_variances) < top_n) {
      best_variances <- c(best_variances, variance)
      best_points_list[[length(best_points_list) + 1]] <- selected_points
    }
  }
  
  # Ordenar las mejores varianzas y combinaciones
  order_indices <- order(best_variances)
  best_variances <- best_variances[order_indices]
  best_points_list <- best_points_list[order_indices]
  
  # Asignar nombres a los puntos seleccionados
  named_points_list <- lapply(best_points_list[1:top_n], function(points) {
    named_points <- paste0("P", seq_along(points))
    colnames(points) <- c("x", "y")
    return(points)
  })
  
  # Crear un data frame con las 10 mejores combinaciones
  result <- data.frame(varianza = best_variances[1:top_n], combinacion = sapply(named_points_list, function(x) paste(rownames(x), collapse = ",")))
  
  return(result)
}


# Ejemplo de uso
n <- nrow(additional_points)  # Número total de puntos
k <- 4  # Número de puntos a usar

result_sequential_inclusion_top <- sequential_inclusion_top(existing_wells, additional_points, n, k, top_n = 3)
result_sequential_inclusion_top

# Obtener la combinación de puntos que minimiza la varianza
best_combination <- result_sequential_inclusion_top$combinacion[1]
selected_indices <- as.numeric(unlist(strsplit(best_combination, ",")))
selected_points <- additional_points[selected_indices, ]

# Crear un data frame con los puntos seleccionados, incluyendo los identificadores
selected_points_with_id <- cbind(selected_points, ID = paste0("P", selected_indices))

# Graficar los pozos existentes, los puntos adicionales y las estaciones seleccionadas
ggplot() +
  geom_polygon(data = block_V, aes(x = x, y = y), fill = "lightblue") +
  geom_point(data = existing_wells, aes(x = x, y = y), color = "red", size = 3) +
  geom_point(data = additional_points, aes(x = x, y = y), color = "blue", size = 3) +
  geom_point(data = selected_points_with_id, aes(x = x, y = y), color = "green", size = 3) +
  geom_text(data = selected_points_with_id, aes(x = x, y = y, label = ID), vjust = -0.5) +
  ggtitle("Distribución de Pozos Existenes, Puntos Adicionales y Estaciones Seleccionadas") +
  xlab("Longitud") +
  ylab("Latitud") +
  theme_minimal()
result_sequential_inclusion_top$varianza/50000
