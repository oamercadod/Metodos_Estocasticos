####Tarea exploracion de datos - uso de Estadistica descriptiva
#### Oma Mercado Díaz - Métodos Estocásticos
install.packages("readxl")
install.packages("moments")

library(readxl)
library(moments)

getwd()

data <- read_excel("C:/ejercicio1.xlsx")

## 1. Haga un histograma de A con anchura de clases de 1, 2 y 5 unidades.
## ¿Qué fracción de los datos tiene valores entre 5 y 10?
data$A<- as.numeric(data$A)
data$B<- as.numeric(data$B)

# 1. Histograma de A con anchura de clases de 1, 2 y 5 unidades
hist_A_1 <- hist(data$A, breaks = 10, plot = F)
plot(hist_A_1, xlab = "Datos A", ylab = "Frecuencia", main = "Histograma de datos A clase 1")
hist_A_2 <- hist(data$A, breaks = 5, plot = F)
plot(hist_A_2, xlab = "Datos A", ylab = "Frecuencia", main = "Histograma de datos A clase 2")
hist_A_5 <- hist(data$A, breaks = 2, plot = F)
plot(hist_A_5, xlab = "Datos A", ylab = "Frecuencia", main = "Histograma de datos A clase 5")
# Fracción de los datos de A entre 5 y 10
fraccion_A_5_10 <- sum(data$A >= 5 & data$A <= 10) / length(data$A)
cat("La fracción de los datos de A que tienen valores entre 5 y 10 es:", fraccion_A_5_10, "\n")

# 2. Histograma de B con anchura de clases de 1, 2 y 5 unidades
hist_B_1 <- hist(data$B, breaks = 15, plot = F)
plot(hist_B_1, xlab = "Datos B", ylab = "Frecuencia", main = "Histograma de datos B clase 1")
hist_B_2 <- hist(data$B, breaks = 12, plot = F)
plot(hist_B_2, xlab = "Datos B", ylab = "Frecuencia", main = "Histograma de datos B clase 2")
hist_B_5 <- hist(data$B, breaks = 4, plot = F)
plot(hist_B_5, xlab = "Datos B", ylab = "Frecuencia", main = "Histograma de datos B clase 5")
fraccion_B_10_15 <- sum(data$B >= 10 & data$B <= 15) / length(data$B)
cat("La fracción de los datos de B que tienen valores entre 10 y 15 es:", fraccion_B_10_15, "\n")

# 3. Distribución acumulativa de A y B
acumulativo_A <- cumsum(hist(data$A, plot = F)$counts) / length(data$A) 
plot(acumulativo_A, type = "l", main = "Acumulativo datos A", xlab = "N. de datos", ylab = "Acumulativo A")
acumulativo_B <- cumsum(hist(data$B, plot = F)$counts) / length(data$B)
plot(acumulativo_B, type = "l", main = "Acumulativo datos B", xlab = "N. de datos", ylab = "Acumulativo B")

# 4. Estadísticas de A y B
mean_A <- mean(data$A)
cat("La media de los datos de A es:", mean_A, "\n")
mean_B <- mean(data$B)
cat("La media de los datos de B es:", mean_B, "\n")

var_A <- var(data$A)
cat("La varianza de los datos de A es:", var_A, "\n")
var_B <- var(data$B)
cat("La varianza de los datos de B es:", var_B, "\n")

#skewness
skewness_A <- moments::skewness(data$A)
cat("El valor de aasimetria de los datos de A es:", skewness_A, "\n")
skewness_B <- moments::skewness(data$B)
cat("El valor de aasimetria de los datos de B es:", skewness_B, "\n")

#kurtosis
kurtosis_A <- moments::kurtosis(data$A)
cat("La curtosis de los datos de A es:", kurtosis_A, "\n")
kurtosis_B <- moments::kurtosis(data$B)
cat("La curtosis de los datos de B es:", kurtosis_B, "\n")

quantiles_A <- quantile(data$A)
cat("Los cuantiles de los datos de A es:", quantiles_A, "\n")
quantiles_B <- quantile(data$B)
cat("Los cuantiles de los datos de B es:", quantiles_B, "\n")

median_A <- median(data$A)
cat("La mediana de los datos de A es:", median_A, "\n")
median_B <- median(data$B)
cat("La mediana de los datos de B es:", median_B, "\n")

IQR_A <- IQR(data$A)
cat("El valor del rango intercuartil de los datos de A es:", IQR_A, "\n")
IQR_B <- IQR(data$B)
cat("El valor del rango intercuartil de los datos de B es:", IQR_B, "\n")

# 5. Diagrama de caja y bigotes
boxplot_A <- boxplot(data$A, main = "Diagrama de caja de los valores de A", ylab = "Valor de A")
boxplot_B <- boxplot(data$B, main = "Diagrama de caja de los valores de B", ylab = "Valor de B")

# Posibles valores atípicos
valor_atipico_en_A <- boxplot_A$out
cat("Los valores atipicos dentro de los datos de A son:", valor_atipico_en_A, "\n")
valor_atipico_en_B <- boxplot_B$out
cat("Los valores atipicos dentro de los datos de B son:", valor_atipico_en_B, "\n")

# 6. y 7. Área a limpiar
area_limpiar_A <- (sum(data$A > 5) / length(data$A)) * 8000
cat("El área a limpiar para una concetración crítica de 5 mg/kg es:", area_limpiar_A, "\n")
area_limpiar_B <- (sum(data$B > 10) / length(data$B)) * 8000 
cat("El área a limpiar para una concetración crítica de 10 mg/kg es:", area_limpiar_B, "\n")

# 8. Coeficiente de correlación entre A y B

plot(data$A,data$B, main = "Datos de contaminantes A-B", xlab = "datos B (mg/kg) ", ylab = "datos A (mg/kg)")
lines(with(data, lowess(data$A ~ data$B)))
correlacion_AB <- cor(data$A, data$B)
cat("El coeficiente de correlación entre los valores de A y B es:", correlacion_AB, "\n")

# 9. Fracción de los datos con A menor que 5 y B menor que 10
fraccion_A_B_entre_5_10 <- sum(data$A < 5 & data$B < 10) / length(data$A)
cat(" La fracción de los datos que tiene un valor A menor que 5 y un valor B menor que 10 es:", fraccion_A_B_entre_5_10, "\n") 

# 10. Fracción de los datos con A menor que 5 o B menor que 10
fraccion_AB_5_10 <- sum(data$A < 5 | data$B < 10) / length(data$A)
cat(" La fracción de los datos que tiene un valor A menor que 5 o un valor B menor que 10 es:", fraccion_AB_5_10, "\n")