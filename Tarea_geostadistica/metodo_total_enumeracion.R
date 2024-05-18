# Instalar y cargar librerías necesarias
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Definir los puntos y el bloque V
Bloque_V <- data.frame(
  x = c(57.5, 57.5, 72.5, 72.5, 57.5),
  y = c(22.5, 32.5, 32.5, 22.5, 22.5)
)

Pozos_datos <- data.frame(
  x = c(60.6, 60.9),
  y = c(36.2, 17.7)
)

#Pozos_adicionales <- data.frame(
#  x = c(60.4, 60.9, 56.3, 76.4, 63.9, 77.4, 73.9, 69.4),
#  y = c(37.2, 36.1, 34.6, 23.3, 21.5, 25.3, 34.8, 35.6)
#)
#simulacion 1
#Pozos_adicionales <- data.frame(
#  x = c(55.08, 69.75, 56.65, 63.12, 68.29, 65.21, 62.23, 72.79),
#  y = c(39.21, 21.85, 39.53, 39.02, 26.52, 32.47, 32.71, 24.92)
#)
#simulacion 2
#Pozos_adicionales <- data.frame(
#  x = c(77.06, 69.91, 60.10, 55.55, 71.95, 70.47, 68.73, 57.51),
#  y = c(21.71, 39.21, 38.31, 28.39, 33.19, 36.19, 26.17, 22.75)
#)
#simulacion 3
#Pozos_adicionales <- data.frame(
#  x = c(71.42, 74.09, 71.56, 73.24, 74.34, 60.96, 69.91, 77.12),
#  y = c(21.18, 38.85, 36.20, 23.62, 35.97, 20.52, 33.91, 31.62)
#)
#simulacion 4
#Pozos_adicionales <- data.frame(
#  x = c(63.47, 59.77, 58.04, 66.99, 79.51, 66.82, 56.56, 64.87),
#  y = c(24.88, 33.92, 28.12, 28.13, 30.62, 20.86, 38.47, 27.04)
#)
#simulacion 5
#Pozos_adicionales <- data.frame(
#  x = c(74.51, 64.48, 79.01, 65.98, 65.32, 77.61, 65.70, 71.08),
#  y = c(25.56, 29.41, 36.19, 25.18, 36.19, 21.74, 24.73, 22.09)
#)
#simulacion 6
#Pozos_adicionales <- data.frame(
#  x = c(65.73, 60.64, 76.16, 76.93, 56.20, 59.52, 59.77, 62.47),
#  y = c(28.42, 27.25, 37.40, 34.41, 29.53, 34.31, 27.73, 33.17)
#)
#simulacion 7
#Pozos_adicionales <- data.frame(
#  x = c(77.30, 65.73, 74.79, 66.98, 56.63, 74.74, 73.51, 62.63),
#  y = c(33.95, 20.26, 33.56, 22.01, 39.79, 33.28, 29.41, 32.18)
#)
#simulacion 8
#Pozos_adicionales <- data.frame(
#  x = c(74.96, 77.90, 65.12, 76.64, 70.06, 55.93, 59.39, 67.86),
#  y = c(30.00, 20.92, 22.04, 35.12, 26.36, 29.02, 32.81, 26.02)
#)
#simulacion 9
#Pozos_adicionales <- data.frame(
#  x = c(66.04, 77.38, 57.00, 59.38, 77.65, 63.36, 62.32, 72.07),
#  y = c(26.49, 30.92, 25.50, 31.76, 23.70, 30.96, 22.10, 24.52)
#)
#simulacion 10
Pozos_adicionales <- data.frame(
  x = c(72.15, 58.86, 76.18, 69.22, 76.62, 77.59, 79.69, 59.98),
  y = c(26.16, 37.31, 39.33, 36.92, 36.50, 38.12, 36.98, 37.59)
)

# Función para calcular la distancia euclidiana entre dos puntos
distancia_euclidiana <- function(p1, p2) {
  sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
}

# Función para calcular la varianza de estimación (suma de las distancias euclidianas al cuadrado)
cal_variancia <- function(Pozos_datos, Puntos_seleccionados) {
  total_cuadrado_dist <- sum(apply(Puntos_seleccionados, 1, function(point) {
    sum(apply(Pozos_datos, 1, function(Pozos_datos) distancia_euclidiana(point, Pozos_datos)^2))
  }))
  return(total_cuadrado_dist)
}

# Método de enumeración total
total_enumeration <- function(Pozos_datos, Pozos_adicionales, n, k) {
  # Calcular todas las posibles combinaciones
  combinations <- combn(nrow(Pozos_adicionales), k)
  mejor_variancia <- Inf
  mejores_puntos <- NULL
  
  # Iterar sobre todas las combinaciones
  for (i in 1:ncol(combinations)) {
    Puntos_seleccionados <- Pozos_adicionales[combinations[, i], ]
    # Calcular la varianza para esta combinación
    variancia <- cal_variancia(Pozos_datos, Puntos_seleccionados)
    # Actualizar si es mejor que la mejor encontrada hasta ahora
    if (variancia < mejor_variancia) {
      mejor_variancia <- variancia
      mejores_puntos <- Puntos_seleccionados
    }
  }
  
  return(list(points = mejores_puntos, variancia = mejor_variancia))
}

# definir los puntos deseados
n <- nrow(Pozos_adicionales)  # Número total de puntos adicionales
k <- 4  # Número de puntos a seleccionar
result_total_enumeration <- total_enumeration(Pozos_datos, Pozos_adicionales, n, k)
mejores_puntos_total_enumeration <- result_total_enumeration$points
mejor_variancia_total_enumeration <- result_total_enumeration$variancia

# Crear el gráfico
ggplot() +
  geom_polygon(data = Bloque_V, aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  geom_point(data = Pozos_datos, aes(x = x, y = y), color = "red", size = 3) +
  geom_point(data = Pozos_adicionales, aes(x = x, y = y), color = "blue", size = 3) +
  geom_point(data = mejores_puntos_total_enumeration, aes(x = x, y = y), color = "green", size = 3) +
  #geom_text(data = Pozos_datos, aes(x = x, y = y, label = "P1"), vjust = -0.5, hjust = -0.5, size = 3, color = "red") +
  geom_text(data = Pozos_adicionales, aes(x = x, y = y, label = paste0("P", seq_along(Pozos_adicionales$x))), vjust = -0.5, hjust = -0.5, size = 3, color = "blue") +
  #geom_text(data = mejores_puntos_total_enumeration, aes(x = x, y = y, label = paste0("P", seq_along(mejores_puntos_total_enumeration$x))), vjust = -0.5, hjust = -0.5, size = 3, color = "green") +
  xlim(50, 80) + ylim(15, 40) +
  labs(title = "Ubicación de pozos y bloque V",
       x = "Longitud",
       y = "Latitud") +
  theme_minimal()

# Método de enumeración total
total_enumeration <- function(Pozos_datos, Pozos_adicionales, k, top_n = 3) {
  # Combinar las coordenadas con los nombres de los pozos adicionales
  Pozos_adicionales_con_nombres <- cbind(seq_len(nrow(Pozos_adicionales)), Pozos_adicionales)
  
  # Calcular todas las posibles combinaciones
  combinations <- combn(nrow(Pozos_adicionales_con_nombres), k)
  mejores_combinaciones <- data.frame(matrix(ncol = k + 1, nrow = top_n))
  colnames(mejores_combinaciones) <- c("Combinacion", "Varianza")
  mejores_combinaciones$Varianza <- Inf
  
  # Iterar sobre todas las combinaciones
  for (i in 1:ncol(combinations)) {
    Puntos_seleccionados <- Pozos_adicionales_con_nombres[combinations[, i], ]
    # Calcular la varianza para esta combinación
    varianza <- cal_variancia(Pozos_datos, Puntos_seleccionados[, -1])
    # Buscar si esta combinación es una de las mejores
    if (varianza < max(mejores_combinaciones$Varianza)) {
      idx <- which.max(mejores_combinaciones$Varianza)
      mejores_combinaciones[idx, "Combinacion"] <- paste("P", Puntos_seleccionados[, 1], sep = "", collapse = ", ")
      mejores_combinaciones[idx, "Varianza"] <- varianza
    }
  }
  
  # Ordenar las combinaciones por varianza
  mejores_combinaciones <- mejores_combinaciones[order(mejores_combinaciones$Varianza),]
  
  # Retornar las mejores combinaciones
  return(mejores_combinaciones)
}

result_total_enumeration <- total_enumeration(Pozos_datos, Pozos_adicionales, k, top_n = 3)
result_total_enumeration<-data.frame(result_total_enumeration)
resultado_final<-colnames(c("Combinacion","varianza"))
resultado_final$combinacion<-result_total_enumeration$Combinacion
resultado_final$varianza<- result_total_enumeration$Varianza/50000
resultado_final<-data.frame(resultado_final)
resultado_final
