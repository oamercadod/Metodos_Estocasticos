library(rstan)

# Especificación del modelo bayesiano en Stan
sensibilidad_stan_code <- "
data {
  real caudal_medio_obs;
  real desviacion_estandar_obs;
  real pri_mean;  // Media de la distribución a priori para caudal_medio
}

parameters {
  real caudal_medio;
  real<lower=0> desviacion_estandar;
}

model {
  caudal_medio ~ normal(pri_mean, 20);  // Distribución a priori con media variable
  desviacion_estandar ~ normal(0, 2000);  // Distribución a priori
  caudal_medio_obs ~ normal(caudal_medio, desviacion_estandar_obs);  // Función de verosimilitud
}
"

# Compilar el modelo
sensibilidad_stan_model <- stan_model(model_code = sensibilidad_stan_code)

# Datos de observación
caudal_medio_obs <- 3474.55  # Ejemplo de caudal medio observado
desviacion_estandar_obs <- 994.02  # Ejemplo de desviación estándar observada

# Ajustar el modelo a los datos con diferentes medias a priori
pri_means <- c(3450, 3480, 3500)
sensibilidad_resultados <- list()
colors <- c("red", "blue", "green")

for (i in 1:length(pri_means)) {
  stan_data_sensibilidad <- list(
    caudal_medio_obs = caudal_medio_obs,
    desviacion_estandar_obs = desviacion_estandar_obs,
    pri_mean = pri_means[i]
  )
  
  sensibilidad_bayes_result <- sampling(
    sensibilidad_stan_model, 
    data = stan_data_sensibilidad, 
    chains = 4, 
    iter = 2000, 
    warmup = 500, 
    thin = 1, 
    seed = 123
  )
  
  sensibilidad_resultados[[i]] <- extract(sensibilidad_bayes_result)$caudal_medio
}

# Visualización de los resultados
plot(density(sensibilidad_resultados[[1]], na.rm = TRUE), col = colors[1], lwd = 2, ylim = c(0, 0.1), 
     main = "Distribución posterior del caudal medio con diferentes medias a priori", 
     xlab = "Caudal medio (m³/s)", ylab = "Densidad")
for (i in 2:length(pri_means)) {
  lines(density(sensibilidad_resultados[[i]], na.rm = TRUE), col = colors[i], lwd = 2)
}
legend("topright", legend = paste("Media a priori =", pri_means), col = colors, lwd = 2)
