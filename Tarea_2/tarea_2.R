# Taller 2. #Probabilidad y Variables Aleatorias
#### Omar Mercado Díaz Metodos estocasticos

install.packages("readxl")
install.packages("moments")
install.packages("mvtnorm")
library(readxl)
library(moments)
library(mvtnorm)

############################################################
# Ejercicio 1. La conductividad hidráulica en alguna ubicación no observada
#se modela con una distribución log-normal. La media de Y = ln K es 2.0 y
#la varianza es 1.5. ¿Cuál es la media y la varianza de K?

mu <- 2.0
sigma2 <- 1.5
media_K <- exp(mu + sigma2/2)
varianza_K <- (exp(sigma2) - 1) * exp(2*mu + sigma2)

media_K
varianza_K

#Resultados ejercicio 1
cat("La media de k es:", media_K, "\n")
cat("La varianza de k es:", varianza_K, "\n")

##################################################################
# Ejercicio 2. La conductividad hidráulica para un acuífero tiene una distribución lognormal 
#con media 10 m/d y varianza 200 m2/d2. ¿Cuál es la probabilidad de que en 
#una ubicación no observada la conductividad sea mayor que 30 m/d?

# Parámetros

mu <- log(10)
sigma <- sqrt(200)
x <- 30

# Calcular probabilidad
probabilidad_30 <- 1 - pnorm(log(x), mean = mu, sd = sigma)
Porcentaje_30 <-round(probabilidad_30*100, digits = 1)

#Resultados ejercicio 2
cat("La probabilidad de que la conductividad en una ubicacion no observada
    sea mayor a 30 m/d es:", probabilidad_30, "o un porcentaje de:", Porcentaje_30, "%")

###############################################################
#Ejercicio 3. Basado en un análisis geológico, se obtuvieron las siguientes
#probabilidades de clases de textura que ocurren en un acuífero:
#Pr[arena]=0.7, Pr[arcilla]=0.2, Pr[turba]=0.1.
#La siguiente tabla muestra las distribuciones de probabilidad de
#las clases de conductividad para las tres texturas:


# Probabilidades de textura
Prob_arena <- 0.7
Prob_arcilla <- 0.2
Prob_turba <- 0.1

# Distribuciones de probabilidad de conductividad para cada textura
prob_conductiv <- matrix(c(
  0.0, 0.0, 0.0, 0.1, 0.4, 0.3, 0.1, 0.1,
  0.3, 0.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0,
  0.1, 0.3, 0.3, 0.2, 0.1, 0.0, 0.0, 0.0
), nrow = 3, byrow = TRUE)

prob_total <- Prob_arena * prob_conductiv[1,] +
  Prob_arcilla * prob_conductiv[2,] +   Prob_turba * prob_conductiv[3,]

#Resultados ejercicio 3
cat("La distribucion de probabilidad total de la conductividad para el acuifero es:","\n",
    "Probabilidad de una Conductividad de 1*10^-3 m/d:", prob_total[1],"\n",
    "Probabilidad de una Conductividad de 1*10^-2 m/d:", prob_total[2],"\n",
    "Probabilidad de una Conductividad de 1*10^-1 m/d:", prob_total[3],"\n",
    "Probabilidad de una Conductividad de 1*10^0 m/d :", prob_total[4],"\n",
    "Probabilidad de una Conductividad de 1*10^1 m/d :", prob_total[5],"\n",
    "Probabilidad de una Conductividad de 2*10^1 m/d :", prob_total[6],"\n",
    "Probabilidad de una Conductividad de 5*10^1 m/d :", prob_total[7],"\n",
    "Probabilidad de una Conductividad de 1*10^2 m/d :", prob_total[8],"\n")

###############################################################
#Ejercicio 4.Considerar dos variables aleatorias Z1 y Z2 con media 10 y 25
# y varianzas 300 y 450 respectivamente. El coeficiente de correlación 
# entre ambas variables es igual a 0.7.

# a. Calcular la covarianza entre Z1 y Z2.
# b. Calcular el valor esperado de Y = Z1 + Z2.
# c. Calcular la varianza de Y = Z1 + Z2.

mu_Z1 <- 10
mu_Z2 <- 25
var_Z1 <- 300
var_Z2 <- 450
corr <- 0.7

# Covarianza entre Z1 y Z2
cov_Z1_Z2 <- corr * sqrt(var_Z1) * sqrt(var_Z2)

# Valor esperado de Y = Z1 + Z2
E_Y <- mu_Z1 + mu_Z2

# Varianza de Y = Z1 + Z2
var_Y <- var_Z1 + var_Z2 + 2 * cov_Z1_Z2

#Resultados ejercicio 4
cat("La Covarianza entre Z1 y Z2 es:", cov_Z1_Z2, "\n")
cat("El Valor esperado de Y = Z1 + Z2 es:", E_Y, "\n")
cat("La Varianza de Y = Z1 + Z2 es:", var_Y, "\n")

#Ejercicio 5. 

mu_Z1 <- 10
mu_Z2 <- 25
var_Z1 <- 300
var_Z2 <- 450
cov_Z1_Z2 <- 315

# Matriz de covarianza
sigma_Z1 <- sqrt(var_Z1)
sigma_Z2 <- sqrt(var_Z2)
rho_matrix <- matrix(c(sigma_Z1^2, cov_Z1_Z2, cov_Z1_Z2, sigma_Z2^2), nrow = 2)

# Pr[Z1 < 30]
prob_Z1_lt_30 <- pmvnorm(mean = c(mu_Z1, mu_Z2), sigma = rho_matrix, lower = c(-Inf, -Inf), upper = c(30, Inf))

# Pr[Z2 < 40]
prob_Z2_lt_40 <- pmvnorm(mean = c(mu_Z1, mu_Z2), sigma = rho_matrix, lower = c(-Inf, -Inf), upper = c(Inf, 40))

# Pr[Z1 + Z2 < 50]
prob_Z1_Z2_lt_50 <- pmvnorm(mean = c(mu_Z1, mu_Z2), sigma = rho_matrix, lower = c(-Inf, -Inf), upper = c(50, 50))

# Pr[Z1 < 30 ∩ Z2 < 40]
prob_Z1_lt_30_Z2_lt_40 <- pmvnorm(mean = c(mu_Z1, mu_Z2), sigma = rho_matrix, lower = c(-Inf, -Inf), upper = c(30, 40))

# Pr[Z1 < 30 ∪ Z2 < 40]
prob_Z1_lt_30_union_Z2_lt_40 <- prob_Z1_lt_30 + prob_Z2_lt_40 - prob_Z1_lt_30_Z2_lt_40

# Resultados

cat("La Probabilidad [Z1 < 30] es:", prob_Z1_lt_30, "\n")
cat("La Probabilidad [Z2 < 40] es:", prob_Z2_lt_40, "\n")
cat("La Probabilidad [Z1 + Z2 < 50] es:", prob_Z1_Z2_lt_50, "\n")
cat("La Probabilidad [Z1 < 30 ∩ Z2 < 40] es:", prob_Z1_lt_30_Z2_lt_40, "\n")
cat("La Probabilidad [Z1 < 30 ∪ Z2 < 40] es:", prob_Z1_lt_30_union_Z2_lt_40, "\n")

#################################################################
# Ejercicio 6.
# a. Derivación de Y(Z1 + Z2) = YZ1 + YZ2

cat("Para derivar la expresión Y(Z1 + Z2) = YZ1 + YZ2, donde Y es una constante y Z1 y Z2 son variables aleatorias, podemos utilizar las propiedades de la esperanza matemática o valor esperado de una variable aleatoria. La esperanza matemática de una constante multiplicada por una variable aleatoria es igual a la constante multiplicada por la esperanza matemática de la variable aleatoria. Formalmente, tenemos:\n\n")

cat("E[aX] = a * E[X]\n\n")

cat("Dado que Z1 y Z2 son independientes, la esperanza matemática de la suma de variables aleatorias es la suma de las esperanzas matemáticas de las variables aleatorias individuales. Por lo tanto:\n\n")

cat("E[Z1 + Z2] = E[Z1] + E[Z2]\n\n")

cat("Reemplazando esto en la expresión E[Y(Z1 + Z2)], obtenemos:\n\n")

cat("E[Y(Z1 + Z2)] = Y * (E[Z1] + E[Z2])\n\n")

cat("Expandiendo el lado derecho de la ecuación, obtenemos:\n\n")

cat("E[Y(Z1 + Z2)] = Y * E[Z1] + Y * E[Z2]\n\n")

cat("Lo cual es equivalente a:\n\n")

cat("E[YZ1 + YZ2]\n\n")

cat("la expresión Y(Z1 + Z2) = YZ1 + YZ2 usando las propiedades de la esperanza matemática y el hecho de que Z1 y Z2 son independientes.\n\n")

# b. Derivación de Yy(S) = exp(isb) Yz(as)

# Derivación de la expresión

cat("Para derivar la expresión Yy(S) = exp(isb) Yz(as), donde Y es una constante, y = a + bZ, y z es una variable aleatoria, podemos utilizar las propiedades de la esperanza matemática o valor esperado de una variable aleatoria. Dado que y es una combinación lineal de a y z, podemos expresar Yy(S) como:\n\n")

cat("Yy(S) = Y(a + bZ)\n\n")

cat("Usando la propiedad de linealidad de la esperanza matemática, tenemos:\n\n")

cat("Yy(S) = Y * a + Y * b * E[Z]\n\n")

cat("Dado que z es una variable aleatoria y E[Z] es su esperanza matemática, podemos sustituir E[Z] por z:\n\n")

cat("Yy(S) = Y * a + Y * b * z\n\n")

cat("Entonces, la expresión Yy(S) = exp(isb) Yz(as) se puede reescribir como:\n\n")

cat("Y * a + Y * b * z = exp(i * b) * Y * z * a\n\n")

cat("Dividiendo ambos lados por Y y simplificando, obtenemos:\n\n")

cat("a + b * z = exp(i * b) * a * z\n\n")

# c. Derivación de Yy(S) = [Yz(S)]^M
# Derivación de la expresión

cat("Para derivar la expresión Yy(S) = [Yz(S)]^M, donde Y = sumatoria(Z_k) para k desde 1 hasta M, podemos utilizar las propiedades de la esperanza matemática o valor esperado de una variable aleatoria. Dado que Y es una suma de variables aleatorias Z_k, podemos expresar Yy(S) como:\n\n")

cat("Yy(S) = [sumatoria(Z_k)]^M\n\n")

cat("Usando la propiedad de linealidad de la esperanza matemática, tenemos:\n\n")

cat("Yy(S) = [sumatoria(E[Z_k])]^M\n\n")

cat("Dado que Z_k son variables aleatorias, podemos expresar E[Z_k] como z:\n\n")

cat("Yy(S) = [sumatoria(z)]^M\n\n")

cat("Como z es una constante, la suma [sumatoria(z)] es igual a M * z. Por lo tanto, podemos reescribir la expresión como:\n\n")

cat("Yy(S) = (M * z)^M\n\n")

cat("Finalmente, simplificando la expresión, obtenemos:\n\n")

cat("Yy(S) = (M^M) * z^M\n\n")

# d. Derivación de Kz(S)
# Derivación de la expresión

cat("Para obtener la expresión Kz(S) = sumatoria((((is)^n)/n)kn) para n desde 1 hasta infinito, si kn = (1/i^n)*((d^k)Kz(S)/ds^k), evaluado en s=0, podemos utilizar la definición de la función generatriz de momentos.\n\n")

cat("La función generatriz de momentos de una variable aleatoria Z está dada por:\n\n")

cat("M_Z(t) = E[e^{tZ}]\n\n")

cat("Expandiendo la exponencial en serie de Taylor, tenemos:\n\n")

cat("M_Z(t) = E[1 + tZ + (t^2 Z^2)/2! + (t^3 Z^3)/3! + ...]\n\n")

cat("Para Kz(S) = M_Z(is), donde S es la variable aleatoria, podemos escribir:\n\n")

cat("Kz(S) = sumatoria(((is)^n)/n!) E[Z^n]\n\n")

cat("Dado que kn = (1/i^n) (d^n Kz(S)/dS^n), podemos expresar E[Z^n] como (d^n Kz(S)/dS^n) y evaluar en S = 0 para obtener kn.\n\n")

cat("Sustituyendo en la fórmula original, tenemos:\n\n")

cat("Kz(S) = sumatoria(((is)^n)/n) (1/i^n) (d^n Kz(S)/dS^n)\n\n")

cat("Evaluando en S = 0, obtenemos la expresión final:\n\n")

cat("Kz(0) = sumatoria(((is)^n)/n) (1/i^n) (d^n Kz(0)/dS^n)\n\n")

cat("Donde Kz(0) representa la variable aleatoria evaluada en S = 0.\n\n")
