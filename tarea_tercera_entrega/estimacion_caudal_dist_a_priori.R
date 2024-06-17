# Cargar paquetes
library(rstan)
library(ggplot2)

# Datos observados
caudal_medio_obs <- 3474.55
desviacion_estandar_obs <- 994.02

# Especificación del modelo bayesiano
stan_code <- "
data {
  real caudal_medio_obs;
  real desviacion_estandar_obs;
}

parameters {
  real<lower=0> caudal_medio;
  real<lower=0> desviacion_estandar;
}

model {
  caudal_medio ~ normal(4137.75, 1934.82);  # Distribución a priori
  desviacion_estandar ~ normal(0, 2000);  # Distribución a priori

  caudal_medio_obs ~ normal(caudal_medio, desviacion_estandar_obs);  # Función de verosimilitud
}

generated quantities {
  real caudal_medio_pred;
  caudal_medio_pred <- normal_rng(caudal_medio, desviacion_estandar);  # Predicción del caudal medio
}
"

# Compilar el modelo
stan_model <- stan_model(model_code = stan_code)

# Datos para pasar al modelo
stan_data <- list(caudal_medio_obs = caudal_medio_obs,
                  desviacion_estandar_obs = desviacion_estandar_obs)

# Ajustar el modelo a los datos
bayes_result <- sampling(stan_model, data = stan_data, chains = 4)

# Resumen de los resultados
print(bayes_result)

# Gráficos
plot(bayes_result)

# Obtener la distribución posterior para el caudal medio
posterior_caudal_medio <- extract(bayes_result)$caudal_medio
Posterior_caudal_medio_pred<- extract(bayes_result)$caudal_medio_pred
desviacion_estandar_modelo<- extract(bayes_result)$desviacion_estandar
# Visualización de la distribución posterior caudal medio
ggplot(data.frame(c(posterior_caudal_medio)), aes(x = c(posterior_caudal_medio))) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  ggtitle("Distribución posterior del caudal medio") +
  xlab("Caudal medio (m³/s)") +
  ylab("Densidad")
plot(posterior_caudal_medio, type="l")
min(posterior_caudal_medio)
max(posterior_caudal_medio)
mean(posterior_caudal_medio)
sd(posterior_caudal_medio)
