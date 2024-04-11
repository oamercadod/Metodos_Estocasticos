library(ggplot2)
library(forecast)
library(readr)
library(dplyr)
library(lubridate)
library(Spectrum)
library(moments)
library(EMD)
library(trend)
SMWR_Lebrija <- read_csv("C:/Users/SGC-075920/Downloads/SMWR_Lebrija.txt", 
                         col_names = FALSE)
colnames(SMWR_Lebrija)<-"CAUDAL"

###############
caudal<-data.frame(as.numeric(SMWR_Lebrija$CAUDAL))
colnames(caudal)<-"caudal"
###Asumiremos que los datos fueron tomados desde la fecha "2000-01-01" para
## poder crear los agrgados
# Crear un vector de fechas
fecha_inicio <- as.Date("2000-01-01")
fechas <- seq(fecha_inicio, by = "day", length.out = 7432)

# Añadir las fechas al dataframe de caudal
caudal$fecha <- fechas

# Convertir la columna de fecha a un formato de fecha
datos_caudal <- caudal
datos_caudal$fecha <- as.Date(datos_caudal$fecha)

# Agregar columnas de mes y año
datos_caudal <- mutate(datos_caudal, mes = format(fecha, "%m"), año = format(fecha, "%Y"))

###### 
##### 1. Agregar la serie
### Calcular agregado caudal medio mensual
caudal_medio_mensual <- datos_caudal %>%
  group_by(año, mes) %>%
  summarise(caudal_medio_mensual = mean(caudal, na.rm = TRUE))

### Calcular agregado caudal medio anual
caudal_medio_anual <- datos_caudal %>%
  group_by(año) %>%
  summarise(caudal_medio_anual = mean(caudal, na.rm = TRUE))

### Calcular agregado caudal medio mensual multianual
caudal_medio_mensual_multianual <- datos_caudal %>%
  group_by(mes) %>%
  summarise(caudal_medio_mensual_multianual = mean(caudal, na.rm = TRUE))

######
##### 2. Realizar la estimación de los estadísticos o momentos
### de cada una de las 4 series 
### Calcular la media
mean_caudal <- mean(datos_caudal$caudal, na.rm = TRUE)

### Calcular la varianza y la desviación estándar
var_caudal <- var(datos_caudal$caudal, na.rm = TRUE)
sd_caudal <- sd(datos_caudal$caudal, na.rm = TRUE)

### Calcular el coeficiente de variación
cv_caudal <- sd_caudal / mean_caudal

### Calcular el coeficiente de asimetría
skewness_caudal <- moments::skewness(datos_caudal$caudal, na.rm = TRUE)

### Calcular el coeficiente de curtosis
kurtosis_caudal <- moments::kurtosis(datos_caudal$caudal, na.rm = TRUE)

### Calcular la covarianza y la correlación
cov_caudal <- cov(datos_caudal$caudal, datos_caudal$caudal, use = "complete.obs")
cor_caudal <- cor(datos_caudal$caudal, datos_caudal$caudal, use = "complete.obs")

### Calcular la autocorrelación y la autocorrelación parcial
acf_caudal <- forecast::Acf(datos_caudal$caudal, lag.max = 20, plot = FALSE)
pacf_caudal <- forecast::Pacf(datos_caudal$caudal, lag.max = 20, plot = FALSE)

# Calcular la función de densidad espectral
psd_caudal <- spec.pgram(datos_caudal$caudal, spans = c(4,4), taper = 0, plot = FALSE)


######
##### para la serie agregada mensual multianual 
### Calcular la media
mean_caudal_medio_mensual_multianual <- mean(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, na.rm = TRUE)

### Calcular la varianza y la desviación estándar
var_caudal_medio_mensual_multianual <- var(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, na.rm = TRUE)
sd_caudal_medio_mensual_multianual <- sd(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, na.rm = TRUE)

### Calcular el coeficiente de variación
cv_caudal_medio_mensual_multianual <- sd_caudal_medio_mensual_multianual / mean_caudal_medio_mensual_multianual

### Calcular el coeficiente de asimetría
skewness_caudal_medio_mensual_multianual <- moments::skewness(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, na.rm = TRUE)

### Calcular el coeficiente de curtosis
kurtosis_caudal_medio_mensual_multianual <- moments::kurtosis(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, na.rm = TRUE)

### Calcular la covarianza y la correlación
cov_caudal_medio_mensual_multianual <- cov(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, use = "complete.obs")
cor_caudal_medio_mensual_multianual <- cor(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, use = "complete.obs")

### Calcular la autocorrelación y la autocorrelación parcial
acf_caudal_medio_mensual_multianual <- Acf(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, lag.max = 20, plot = FALSE)
pacf_caudal_medio_mensual_multianual <- Pacf(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, lag.max = 20, plot = FALSE)

# Calcular la función de densidad espectral
psd_caudal_medio_mensual_multianual <- spec.pgram(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, spans = c(4,4), taper = 0, plot = FALSE)




######
##### Para la serie medio mensual 
### Calcular la media
mean_caudal_medio_mensual <- mean(caudal_medio_mensual$caudal_medio_mensual, na.rm = TRUE)

### Calcular la varianza y la desviación estándar
var_caudal_medio_mensual <- var(caudal_medio_mensual$caudal_medio_mensual, na.rm = TRUE)
sd_caudal_medio_mensual <- sd(caudal_medio_mensual$caudal_medio_mensual, na.rm = TRUE)

### Calcular el coeficiente de variación
cv_caudal_medio_mensual <- sd_caudal_medio_mensual / mean_caudal_medio_mensual

### Calcular el coeficiente de asimetría
skewness_caudal_medio_mensual <- skewness(caudal_medio_mensual$caudal_medio_mensual, na.rm = TRUE)

### Calcular el coeficiente de curtosis
kurtosis_caudal_medio_mensual <- kurtosis(caudal_medio_mensual$caudal_medio_mensual, na.rm = TRUE)

### Calcular la covarianza y la correlación
cov_caudal_medio_mensual <- cov(caudal_medio_mensual$caudal_medio_mensual, caudal_medio_mensual$caudal_medio_mensual, use = "complete.obs")
cor_caudal_medio_mensual <- cor(caudal_medio_mensual$caudal_medio_mensual, caudal_medio_mensual$caudal_medio_mensual, use = "complete.obs")

### Calcular la autocorrelación y la autocorrelación parcial
acf_caudal_medio_mensual <- Acf(caudal_medio_mensual$caudal_medio_mensual, lag.max = 20, plot = FALSE)
pacf_caudal_medio_mensual <- Pacf(caudal_medio_mensual$caudal_medio_mensual, lag.max = 20, plot = FALSE)

# Calcular la función de densidad espectral
psd_caudal_medio_mensual <- spec.pgram(caudal_medio_mensual$caudal_medio_mensual, spans = c(4,4), taper = 0, plot = FALSE)


######
##### Para la serie medio anual 
### Calcular la media
mean_caudal_medio_anual <- mean(caudal_medio_anual$caudal_medio_anual, na.rm = TRUE)

### Calcular la varianza y la desviación estándar
var_caudal_medio_anual <- var(caudal_medio_anual$caudal_medio_anual, na.rm = TRUE)
sd_caudal_medio_anual <- sd(caudal_medio_anual$caudal_medio_anual, na.rm = TRUE)

### Calcular el coeficiente de variación
cv_caudal_medio_anual <- sd_caudal_medio_anual / mean_caudal_medio_anual

### Calcular el coeficiente de asimetría
skewness_caudal_medio_anual <- skewness(caudal_medio_anual$caudal_medio_anual, na.rm = TRUE)

### Calcular el coeficiente de curtosis
kurtosis_caudal_medio_anual <- kurtosis(caudal_medio_anual$caudal_medio_anual, na.rm = TRUE)

### Calcular la covarianza y la correlación
cov_caudal_medio_anual <- cov(caudal_medio_anual$caudal_medio_anual, caudal_medio_anual$caudal_medio_anual, use = "complete.obs")
cor_caudal_medio_anual <- cor(caudal_medio_anual$caudal_medio_anual, caudal_medio_anual$caudal_medio_anual, use = "complete.obs")

### Calcular la autocorrelación y la autocorrelación parcial
acf_caudal_medio_anual <- Acf(caudal_medio_anual$caudal_medio_anual, lag.max = 20, plot = FALSE)
pacf_caudal_medio_anual <- Pacf(caudal_medio_anual$caudal_medio_anual, lag.max = 20, plot = FALSE)

# Calcular la función de densidad espectral
psd_caudal_medio_anual <- spec.pgram(caudal_medio_anual$caudal_medio_anual, spans = c(4,4), taper = 0, plot = FALSE)


#######
#### 3. Graficas
# Graficar las series de tiempo
plot(datos_caudal$fecha, datos_caudal$caudal, type = "l", xlab = "fecha",
     ylab = "caudal", main = "Serie de tiempo de caudal diario")

caudal_medio_mensual$fecha<- paste(caudal_medio_mensual$año,caudal_medio_mensual$mes, "01", sep = "-" )
caudal_medio_mensual$fecha<- as.Date(caudal_medio_mensual$fecha)
plot(caudal_medio_mensual$fecha, caudal_medio_mensual$caudal_medio_mensual, type = "l", xlab = "fecha",
     ylab = "caudal", main = "Serie de tiempo de caudal medio mensual")

plot(caudal_medio_mensual_multianual$mes, caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, type = "l", xlab = "Mes",
     ylab = "caudal", main = "Serie de tiempo de caudal medio mensual multianual")

plot(caudal_medio_anual$año, caudal_medio_anual$caudal_medio_anual, type = "l", xlab = "año",
     ylab = "caudal", main = "Serie de tiempo de caudal medio anual")

############# correlograma total de cada serie de tiempo
Acf(datos_caudal$caudal, lag.max = 20, main = "Correlograma total caudal diario")
Acf(caudal_medio_anual$caudal_medio_anual, lag.max = 20, main = "Correlograma total caudal medio Anual")
Acf(caudal_medio_mensual$caudal_medio_mensual, lag.max = 20, main = "Correlograma total caudal medio Mensual")
Acf(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, lag.max = 20, main = "Correlograma total medio mensual Multianual")

############ correlograma parcial de cada serie de tiempo
Pacf(datos_caudal$caudal, lag.max = 20, main = "Correlograma Parcial caudal diario")
Pacf(caudal_medio_anual$caudal_medio_anual, lag.max = 20, main = "Correlograma Parcial caudal medio Anual")
Pacf(caudal_medio_mensual$caudal_medio_mensual, lag.max = 20, main = "Correlograma Parcial caudal medio Mensual")
Pacf(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, lag.max = 20, main = "Correlograma Parcial caudal mensual Multianual")

############ desviaciones estándar de cada serie de tiempo
plot(sd_caudal, type = "p", main = "Desviaciones Estándar caudal Diario")
plot(sd_caudal_medio_anual, type = "p", main = "Desviaciones Estándar medio anual")
plot(sd_caudal_medio_mensual, type = "p", main = "Desviaciones Estándar medio mensual")
plot(sd_caudal_medio_mensual_multianual, type = "p", main = "Desviaciones Estándar mensual Multianual")

######### Graficar los coeficientes de asimetría de la serie de tiempo
plot(skewness_caudal, type = "p", main = "Coeficientes de Asimetría caudal diario")
plot(skewness_caudal_medio_anual, type = "p", main = "Coeficientes de Asimetría medio anual")
plot(skewness_caudal_medio_mensual, type = "p", main = "Coeficientes de Asimetría medio mensual")
plot(skewness_caudal_medio_mensual_multianual, type = "p", main = "Coeficientes de Asimetría mensual Multianual")

######### Graficar los coeficientes de curtosis de la serie de tiempo
plot(kurtosis_caudal, type = "p", main = "Coeficientes de Curtosis caudal diario")
plot(kurtosis_caudal_medio_anual, type = "p", main = "Coeficientes de Curtosis medio anual")
plot(kurtosis_caudal_medio_mensual, type = "p", main = "Coeficientes de Curtosis medio mensual")
plot(kurtosis_caudal_medio_mensual_multianual, type = "p", main = "Coeficientes de Curtosis menusla Multianual")

# Graficar el periodograma de la serie de tiempo
plot(psd_caudal, main = "Periodograma caudal diario")
plot(psd_caudal_medio_anual, main = "Periodograma medio anual")
plot(psd_caudal_medio_mensual, main = "Periodograma medio mensual")
plot(psd_caudal_medio_mensual_multianual, main = "Periodograma mensual Multianual")

####### # Calcular y graficar las curvas IMF utilizando EMD
imfs_diario <- emd(datos_caudal$caudal)
plot(imfs_diario[[2]], type='l', xlab = "Datos por dias", ylab = "imf", main= "Curva IMF caudal diario")

imfs_anual <- emd(caudal_medio_anual$caudal_medio_anual)
plot(imfs_anual[[2]], type='l',xlab = "Datos por Año", ylab = "imf", main= "Curva IMF medio anual")

imfs_mensual <- emd(caudal_medio_mensual$caudal_medio_mensual)
plot(imfs_mensual[[2]], type='l',xlab = "Datos por meses", ylab = "imf", main = "Curvas IMF medio mensual")

imfs_mensual_multianual <- emd(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual)
plot(imfs_mensual_multianual[[2]], type='l',xlab = "Meses", ylab = "imf", main = "Curvas IMF mensual Multianual")

###### Graficar el histograma de la serie de tiempo
hist(datos_caudal$caudal, ylab = "Frecuencia",xlab = "caudal", main = "Histograma caudal diario")
hist(caudal_medio_anual$caudal_medio_anual, ylab = "Frecuencia",xlab = "caudal", main = "Histograma caudal medio anual")
hist(caudal_medio_mensual$caudal_medio_mensual, ylab = "Frecuencia",xlab = "caudal", main = "Histograma caudal medio mensual")
hist(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, ylab = "Frecuencia",xlab = "caudal", main = "Histograma caudal medio mensual Multianual")

####### # Graficar el diagrama de caja y bigotes de la serie de tiempo
boxplot(datos_caudal$caudal, main = "Diagrama caudal diario")
boxplot(caudal_medio_anual$caudal_medio_anual, main = "Diagrama caudal medio anual")
boxplot(caudal_medio_mensual$caudal_medio_mensual, main = "Diagrama caudal medio mensual")
boxplot(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, main = "Diagrama caudal medio mensual Multianual")

#######
##### 4. analisis de saltos
#### Datos diarios
# Prueba de Pettitt
pettitt_test <- function(x) {
  n <- length(x)
  ranks <- rank(x)
  U <- sum(outer(ranks, ranks, `>`))
  W <- U - n*(n+1)/4
  p_value <- 2*pt(-abs(W/sqrt(n*(n+1)*(2*n+1)/6)), df = n-1)
  return(list(statistic = W, p_value = p_value))
}
result_pettitt <- pettitt_test(datos_caudal$caudal)

# Prueba de Suma de Rangos (Wilcoxon)
wilcox_test <- wilcox.test(datos_caudal$caudal, mu = median(datos_caudal$caudal))

# Prueba de Kruskal-Wallis
kruskal_test <- kruskal.test(datos_caudal$caudal ~ seq_along(datos_caudal$caudal))

# Prueba t de Student
t_test <- t.test(datos_caudal$caudal)

# resultados
result_pettitt
wilcox_test
kruskal_test
t_test

#### Datos medio anuales
# Prueba de Pettitt
pettitt_test <- function(x) {
  n <- length(x)
  ranks <- rank(x)
  U <- sum(outer(ranks, ranks, `>`))
  W <- U - n*(n+1)/4
  p_value <- 2*pt(-abs(W/sqrt(n*(n+1)*(2*n+1)/6)), df = n-1)
  return(list(statistic = W, p_value = p_value))
}
result_pettitt_anual <- pettitt_test(caudal_medio_anual$caudal_medio_anual)

# Prueba de Suma de Rangos (Wilcoxon)
wilcox_test_anual <- wilcox.test(caudal_medio_anual$caudal_medio_anual, mu = median(caudal_medio_anual$caudal_medio_anual))

# Prueba de Kruskal-Wallis
kruskal_test_anual <- kruskal.test(caudal_medio_anual$caudal_medio_anual ~ seq_along(caudal_medio_anual$caudal_medio_anual))

# Prueba t de Student
t_test_anual <- t.test(caudal_medio_anual$caudal_medio_anual)

# resultados
result_pettitt_anual
wilcox_test_anual
kruskal_test_anual
t_test_anual

#### Datos medio mensuales
# Prueba de Pettitt
pettitt_test <- function(x) {
  n <- length(x)
  ranks <- rank(x)
  U <- sum(outer(ranks, ranks, `>`))
  W <- U - n*(n+1)/4
  p_value <- 2*pt(-abs(W/sqrt(n*(n+1)*(2*n+1)/6)), df = n-1)
  return(list(statistic = W, p_value = p_value))
}
result_pettitt_mensual <- pettitt_test(caudal_medio_mensual$caudal_medio_mensual)

# Prueba de Suma de Rangos (Wilcoxon)
wilcox_test_mensual <- wilcox.test(caudal_medio_mensual$caudal_medio_mensual, mu = median(caudal_medio_mensual$caudal_medio_mensual))

# Prueba de Kruskal-Wallis
kruskal_test_mensual <- kruskal.test(caudal_medio_mensual$caudal_medio_mensual ~ seq_along(caudal_medio_mensual$caudal_medio_mensual))

# Prueba t de Student
t_test_mensual <- t.test(caudal_medio_mensual$caudal_medio_mensual)

# resultados
result_pettitt_mensual
wilcox_test_mensual
kruskal_test_mensual
t_test_mensual

#### Datos medio multianuales
# Prueba de Pettitt
pettitt_test <- function(x) {
  n <- length(x)
  ranks <- rank(x)
  U <- sum(outer(ranks, ranks, `>`))
  W <- U - n*(n+1)/4
  p_value <- 2*pt(-abs(W/sqrt(n*(n+1)*(2*n+1)/6)), df = n-1)
  return(list(statistic = W, p_value = p_value))
}
result_pettitt_multianual <- pettitt_test(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual)

# Prueba de Suma de Rangos (Wilcoxon)
wilcox_test_multianual <- wilcox.test(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, mu = median(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual))

# Prueba de Kruskal-Wallis
kruskal_test_multianual <- kruskal.test(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual ~ seq_along(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual))

# Prueba t de Student
t_test_multianual <- t.test(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual)

# resultados
result_pettitt_multianual
wilcox_test_multianual
kruskal_test_multianual
t_test_multianual

############
###datos diarios
# Prueba de coeficiente de correlación ρ de Spearman
spearman_test <- cor.test(seq_along(datos_caudal$caudal), datos_caudal$caudal, method = "spearman")

# Prueba de Mann-Kendall

# Realizar la prueba de Mann-Kendall
mk_test <- mk.test(datos_caudal$caudal)

# Prueba de Mann-Kendall modificada
mod_mk_test <- mk.test(datos_caudal$caudal, alternative = "two.sided")

# Prueba de Regresión Lineal
lm_test <- lm(datos_caudal$caudal ~ seq_along(datos_caudal$caudal))

# Output de los resultados
spearman_test
mk_test
mod_mk_test
summary(lm_test)

###datos anuales
# Prueba de coeficiente de correlación ρ de Spearman
spearman_test_anual <- cor.test(seq_along(caudal_medio_anual$caudal_medio_anual), caudal_medio_anual$caudal_medio_anual, method = "spearman")

# Prueba de Mann-Kendall

# Realizar la prueba de Mann-Kendall
mk_test_anual <- mk.test(caudal_medio_anual$caudal_medio_anual)

# Prueba de Mann-Kendall modificada
mod_mk_test_anual <- mk.test(caudal_medio_anual$caudal_medio_anual, alternative = "two.sided")

# Prueba de Regresión Lineal
lm_test_anual <- lm(caudal_medio_anual$caudal_medio_anual ~ seq_along(caudal_medio_anual$caudal_medio_anual))

# Output de los resultados
spearman_test_anual
mk_test_anual
mod_mk_test_anual
summary(lm_test_anual)

###datos mensuales
# Prueba de coeficiente de correlación ρ de Spearman
spearman_test_mensuales <- cor.test(seq_along(caudal_medio_mensual$caudal_medio_mensual), caudal_medio_mensual$caudal_medio_mensual, method = "spearman")

# Prueba de Mann-Kendall

# Realizar la prueba de Mann-Kendall
mk_test_mensual <- mk.test(caudal_medio_mensual$caudal_medio_mensual)

# Prueba de Mann-Kendall modificada
mod_mk_test_mensual <- mk.test(caudal_medio_mensual$caudal_medio_mensual, alternative = "two.sided")

# Prueba de Regresión Lineal
lm_test_mensual <- lm(caudal_medio_mensual$caudal_medio_mensual ~ seq_along(caudal_medio_mensual$caudal_medio_mensual))

# Output de los resultados
spearman_test_mensuales
mk_test_mensual
mod_mk_test_mensual
summary(lm_test_mensual)


###datos multianual
# Prueba de coeficiente de correlación ρ de Spearman
spearman_test_multianual <- cor.test(seq_along(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual), caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, method = "spearman")

# Prueba de Mann-Kendall

# Realizar la prueba de Mann-Kendall
mk_test_multianual <- mk.test(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual)

# Prueba de Mann-Kendall modificada
mod_mk_test_multianual <- mk.test(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual, alternative = "two.sided")

# Prueba de Regresión Lineal
lm_test_multianual <- lm(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual ~ seq_along(caudal_medio_mensual_multianual$caudal_medio_mensual_multianual))

# Output de los resultados
spearman_test_multianual
mk_test_multianual
mod_mk_test_multianual
summary(lm_test_multianual)

###########
######## Pruebas de hipótesis de cambio de distribución
##### datos diarios
# Prueba de Kolmogorov-Smirnov
ks_test <- ks.test(datos_caudal$caudal, "pnorm", mean(datos_caudal$caudal), sd(datos_caudal$caudal))

# Prueba de Kruskal-Wallis
kruskal_test <- kruskal.test(datos_caudal$caudal ~ seq_along(datos_caudal$caudal))

# Output de los resultados
ks_test
kruskal_test

##### datos anual
# Prueba de Kolmogorov-Smirnov
ks_test_anual <- ks.test(caudal_medio_anual$caudal_medio_anual, "pnorm", mean(caudal_medio_anual$caudal_medio_anual), sd(caudal_medio_anual$caudal_medio_anual))

# Prueba de Kruskal-Wallis
kruskal_test_anual <- kruskal.test(caudal_medio_anual$caudal_medio_anual ~ seq_along(caudal_medio_anual$caudal_medio_anual))

# Output de los resultados
ks_test_anual
kruskal_test_anual

#########
####### 5. Realice un análisis del flujo en Río Lebrija 
########### a partir de todos los cálculos realizados
