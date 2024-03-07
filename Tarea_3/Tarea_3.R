# Instalar y cargar el paquete VGAM (si no está instalado)
if (!requireNamespace("VGAM","trend","tidyverse","remotes","lterdatasampler",
                      "readxl","MASS",quietly = TRUE)) {
 
install.packages("VGAM")
install.packages("trend")
install.packages("tidyverse")
install.packages("remotes")
install.packages("lterdatasampler")
install.packages("readxl")
install.packages("MASS")
}
library(MASS)
library(trend)
library(tidyverse)
library(remotes)
library(lterdatasampler)
library(readxl)

Taller3 <- read_excel("C:/tarea_metodos/Taller3.xlsx")
x<- Taller3$`CaudalMax_Dia_Anual_m3/s`
#####
## 1. Pruebe si estos valores pueden ser modelados como variables estocásticas
##independientes.


#### Realizamos la prueba Von Neuman para medir la aleoteriedad e independencia 

x <- x[!is.na(x)]
d <- diff(x)
n <- length(x)
mx <- mean(x)

# para los datos el valor de Von Neuman se calcula:

VN <- sum(d^2)/sum((x - mx)^2)
VN
# Para datos sin sesgos el valos VN es: 2.15411
# Los datos son Independiente


###### 2. Pruebe si se puede detectar una tendencia.

### Realizamos la prueba de Mann-kendall para probar si hay tendencias 
mk.test(x)

# z = 0.83648, n = 52, p-value = 0.4029
# S: 1.070000e+02
# varS :1.605833e+04 
# tau: 8.072426e-02

# La hipotesis alternativa se acepta y se rechaza la hipotesis nula ya que
# No hay tendencias en los datos

###### 3.Grafique los datos en papel de probabilidad Gumbel
# y determina los parámetros de la distribución Gumbel.
# ¿Cuál es la estimación de la inundación de 1000 años?

# Datos de caudal máximo diario anual

caudal_maximo <- Taller3$`CaudalMax_Dia_Anual_m3/s`
# Ordenar los datos de forma ascendente
caudal_ordenado <- sort(caudal_maximo)

# Calcular las probabilidades no excedidas
n <- length(caudal_ordenado)
prob_no_excedida <- (n:1 - 0.44) / (n + 0.12)

# Calcular los cuantiles de la distribución Gumbel
quantiles_gumbel <- -log(-log(prob_no_excedida))

# Ajustar una recta a los cuantiles de la distribución Gumbel
ajuste <- lm(caudal_ordenado ~ quantiles_gumbel)

# Graficar el papel de probabilidad Gumbel con líneas divisorias
plot(quantiles_gumbel, caudal_ordenado, main = "Papel de Probabilidad Gumbel", xlab = "Probabilidad no excedida", ylab = "Caudal máximo diario anual (m3/s)", ylim = range(caudal_ordenado), xlim = range(quantiles_gumbel))
abline(h = caudal_ordenado, lty = 2, col = "blue")

# Determinar los parámetros de la distribución Gumbel
a <- coef(ajuste)[1]
b <- coef(ajuste)[2]
loc <- a - b * (0.5772)  # 0.5772 es la constante de Euler-Mascheroni
scale <- b

# Estimar la inundación de 1000 años
inundacion_1000 <- scale - loc * - log(-log(1 / 1000))
# Imprimir el caudal de 1000 años
cat("El caudal de 1000 años es:", inundacion_1000, "m3/s\n")
#### la estimación de la inundación de 1000 años es:3305.668 m3/s 

##### 4.Determine los parámetros de la distribución Gumbel mediante
# regresión lineal de Qmax contra con isiendo el rango del máximo más pequeño.
# Estima la inundación de 1000 años y sus límites de confianza del 95%. 

caudal <- Taller3$`CaudalMax_Dia_Anual_m3/s`
# Ordenar los datos de Qmax de menor a mayor
caudal_ordenado <- sort(caudal)

# Calcular i/(n + 1) para cada dato ordenado
i <- 1:length(caudal_ordenado)
prob <- i / (length(caudal_ordenado) + 1)

# Calcular -log(-log(i/(n + 1))) para cada dato
gumbel_prob <- -log(-log(prob))

# Realizar la regresión lineal de Qmax contra -log(-log(i/(n + 1)))
regression <- lm(caudal_ordenado ~ gumbel_prob)

# Obtener los parámetros de la distribución Gumbel
loc <- coef(regression)[1]
scale <- coef(regression)[2]

# Calcular la inundación de 1000 años
prob_1000 <- 1 / 1000
gumbel_prob_1000 <- -log(-log(prob_1000))
caudal_1000 <- scale - loc * gumbel_prob_1000

# Calcular los límites de confianza del 95%
conf_int <- confint(regression, level = 0.95)

# Estimar los límites de confianza del 95% para la inundación de 1000 años
gumbel_prob_upper <- gumbel_prob_1000 + qnorm(0.975) * summary(regression)$sigma
gumbel_prob_lower <- gumbel_prob_1000 - qnorm(0.975) * summary(regression)$sigma
caudal_1000_upper <- caudal_1000 + gumbel_prob_upper
caudal_1000_lower <- caudal_1000 + gumbel_prob_lower

# Imprimir los resultados
cat("EStimacion de 1000 años es:", caudal_1000, "m3/s\n")
## EStimacion de inundacion para 1000 años es: 2823.907 m3/s
print(paste("Límites de confianza del 95%:", caudal_1000_lower, "-", caudal_1000_upper))
## Los límites de confianza del 95%: estan entre 2721.954 - 2921.994"

###### 5. Determine los parámetros de la distribución Gumbel con el método
# de momentos. Estima la inundación de 1000 años y sus límites
# de confianza del 95%.

# Calcular los momentos muestrales
media_muestral <- mean(caudal_ordenado)
varianza_muestral <- var(caudal_ordenado)

# Calcular los parámetros de la distribución Gumbel
mu <- media_muestral - 0.5772 * sqrt(varianza_muestral)
sigma <- sqrt((pi^2 / 6) * varianza_muestral)

# Calcular la inundación de 1000 años
prob_1000_ <- 1 / 1000
caudal_1000_ <- qgumbel(1 - prob_1000_, loc = mu, scale = sigma)

# Calcular los límites de confianza del 95%
conf_int <- qgumbel(c(0.025, 0.975), loc = mu, scale = sigma)

# Imprimir los resultados
print("Parámetros de la distribución Gumbel:")
cat("El valor de Mu es:", mu)
## El valor de Mu es: 1179.434
cat("El valor de Sigma es:", sigma)
## El valor de Sigma es: 656.7535
cat("La estimacion de Inundación de 1000 años es:", caudal_1000_, "m3/s\n")
## La estimacion de Inundación de 1000 años es: 5715.798
print(paste("Límites de confianza del 95%:", conf_int))
## Límite inferior de confianza del 95%: 322.15"
## Límite superior de confianza del 95%: 3593.82"

####### 6. Estima la inundación de 1000 años asumiendo una distribución
# lognormal de los valores máximos.

# Ajustar la distribución lognormal a los datos
fit <- fitdistr(caudal_ordenado, "lognormal")

# Obtener los parámetros de la distribución lognormal
meanlog <- fit$estimate["meanlog"]
sdlog <- fit$estimate["sdlog"]

# Calcular la probabilidad de excedencia para 1000 años
prob_excedencia_1000_A <- 1/1000

# Calcular el caudal correspondiente a 1000 años
caudal_1000_A <- qlnorm(1 - prob_excedencia_1000_A, meanlog = meanlog, sdlog = sdlog)

# Imprimir el resultado
cat("El valor de inundacion para mil años es",caudal_1000_A, "m3/s\n")
## El valor de inundacion para mil años es 4037.033 m3/s

###### 7. Prueba si los datos pueden considerarse como resultados
# de una distribución Gumbel. 

### Para determinar si los datos se ajustan a una distribucion
# de gumbel usamos el test de Kolmogorov-Smirnov

# Realizar el test de Kolmogorov-Smirnov
ks_test <- ks.test(caudal_ordenado, "pgumbel", loc = loc, scale = scale)

# Imprimir el resultado
print(ks_test)

### los valores de D = 0.058184, p-value = 0.9946

# El valor de p indican que los datos si aparentemente si siguen una
# distribucion de gumbel

###### 8.  Prueba si los datos pueden considerarse como resultados
# de una distribución lognormal.

## usamos nuevamente el test de Kolmogorov-Smirnov pero esta vez usamos la
# funcion "plnorm".

# Realizar el test de Kolmogorov-Smirnov
ks_test <- ks.test(caudal_ordenado, "plnorm", meanlog = meanlog, sdlog = sdlog)

# Imprimir el resultado
print(ks_test)

## Obtenemos los valores de D = 0.053272, p-value = 0.9985

### los datos parecen ajustarse a una distribución lognormal

###### 9. ¿Cuál es la probabilidad de que la inundación de 1000 años
# ocurra al menos una vez en los próximos 40 años?

prob_no_ocur <- (1 - 1/1000)^40
prob_ocurrir_al_menos_una_vez <- 1 - prob_no_ocur
cat("La probabilidad de que la inundacion de 1000 años ocurra al menos
    una vez en los proximos 40 años es:", prob_ocurrir_al_menos_una_vez)

# La probabilidad de que la inundacion de 1000 años ocurra al menos
# una vez en los proximos 40 años es: 0.03922979


###### 10. ¿Cuál es la probabilidad de que la inundación de 1000 años
# ocurra dos veces en los próximos 100 años?
# usamos una distribucion de Poisson para calcular la probabilidad 

lambda <- 1/1000
t <- 100
k <- 2
prob_poisson <- ( (lambda * t)^k * exp(-lambda * t) ) / factorial(k)
cat("La probabilidad que la inundacion de 1000 años ocurra
    dos veces en los proximos 100 años es",prob_poisson)
## La probabilidad que la inundacion de 1000 años ocurra
# dos veces en los proximos 100 años es 0.004524187
