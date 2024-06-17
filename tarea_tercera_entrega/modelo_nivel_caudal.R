# Instalar y cargar los paquetes necesarios
if (!requireNamespace("rstan", quietly = TRUE)) {
  install.packages("rstan")
}
library(rstan)

# Datos de nivel y caudal
data <- data.frame(
  nivel = c(550, 600, 650, 700, 750, 800, 850, 900),
  caudal = c(2167, 2425, 2800, 3260, 3860, 4780, 6190, 7620)
)

# Código del modelo en Stan
nivel_caudal_stan_code <- "
data {
  int<lower=0> N;  // Número de observaciones
  vector[N] nivel;  // Niveles observados
  vector[N] caudal;  // Caudales observados
}

parameters {
  real alpha;  // Intercepto
  real beta;  // Pendiente
  real<lower=0> sigma;  // Desviación estándar
}

model {
  nivel ~ normal(alpha + beta * caudal, sigma);  // Modelo lineal
}
"

# Compilar el modelo
nivel_caudal_stan_model <- stan_model(model_code = nivel_caudal_stan_code)

# Datos para el modelo
stan_data <- list(
  N = nrow(data),
  nivel = data$nivel,
  caudal = data$caudal
)

# Ajustar el modelo
fit <- sampling(
  nivel_caudal_stan_model, 
  data = stan_data, 
  chains = 4, 
  iter = 2000, 
  warmup = 500, 
  thin = 1, 
  seed = 123
)

# Resumen de los resultados
print(fit)

# Extraer muestras
samples <- extract(fit)

# Resumen de las muestras
alpha_samples <- samples$alpha
beta_samples <- samples$beta
sigma_samples <- samples$sigma

# Predicciones
predicted_nivel <- function(caudal, alpha, beta) {
  return(alpha + beta * caudal)
}

# Predicciones y intervalos de confianza
caudales_pred <- seq(min(data$caudal), max(data$caudal), length.out = 100)
nivel_pred_mean <- sapply(caudales_pred, function(caudal) mean(predicted_nivel(caudal, alpha_samples, beta_samples)))
nivel_pred_lower <- sapply(caudales_pred, function(caudal) quantile(predicted_nivel(caudal, alpha_samples, beta_samples), 0.025))
nivel_pred_upper <- sapply(caudales_pred, function(caudal) quantile(predicted_nivel(caudal, alpha_samples, beta_samples), 0.975))

# Graficar los resultados
plot(data$caudal, data$nivel, type = "l", pch = 26, xlab = "Caudal (m³/s)", ylab = "Nivel (cm)", main = "Modelo: Nivel vs Caudal", col = "black")
points(data$caudal, data$nivel, pch = 16, col = "red")
abline(a = mean(alpha_samples), b = mean(beta_samples), col = "orange", lwd = 2)
lines(caudales_pred, nivel_pred_mean, col = "orange", lwd = 2)
lines(caudales_pred, nivel_pred_lower, col = "blue", lwd = 1, lty = 2)
lines(caudales_pred, nivel_pred_upper, col = "blue", lwd = 1, lty = 2)

# Agregar leyenda fuera del gráfico
legend("topright", legend = c("Predicción media", "Intervalo de predicción"), col = c("orange", "blue"), lty = c(1, 1, 2), lwd = c(2, 2, 1), bty = "n", y.intersp = 1.5, inset = c(0, 0.5), xpd = F)


####
# Serie de caudales para los cuales se desea predecir el nivel
caudales_a_predecir <- posterior_caudal_medio 

# Predicción de los niveles correspondientes a los caudales dados
niveles_predichos <- sapply(caudales_a_predecir, function(caudal) predicted_nivel(caudal, mean(alpha_samples), mean(beta_samples)))

# Mostrar los resultados
resultados <- data.frame(caudal = caudales_a_predecir, nivel_predicho = niveles_predichos)
print(resultados)
