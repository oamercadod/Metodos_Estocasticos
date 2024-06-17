library(rstan)
library(rBayesianOptimization)

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
  desviacion_estandar ~ normal(0, 10);  // Distribución a priori
  caudal_medio_obs ~ normal(caudal_medio, desviacion_estandar_obs);  // Función de verosimilitud
}
"

# Compilar el modelo
sensibilidad_stan_model <- stan_model(model_code = sensibilidad_stan_code)

# Datos de observación
caudal_medio_obs <- 3605.29
desviacion_estandar_obs <- 889.11  # Ejemplo de desviación estándar observada

# Función objetivo para la optimización bayesiana
objective_function <- function(pri_mean) {
  stan_data_sensibilidad <- list(
    caudal_medio_obs = caudal_medio_obs,
    desviacion_estandar_obs = desviacion_estandar_obs,
    pri_mean = pri_mean
  )
  
  sensibilidad_bayes_result <- sampling(
    sensibilidad_stan_model, 
    data = stan_data_sensibilidad, 
    chains = 4, 
    iter = 2000, 
    warmup = 500, 
    thin = 1, 
    seed = 123,
    refresh = 0
  )
  
  caudal_medio_samples <- extract(sensibilidad_bayes_result)$caudal_medio
  log_lik <- sum(dnorm(caudal_medio_obs, mean(caudal_medio_samples), sd(caudal_medio_samples), log = TRUE))
  
  return(list(Score = log_lik, Pred = pri_mean))
}

# Rango de valores para la media a priori
bounds <- list(pri_mean = c(3000,4000 ))

# Ejecutar la optimización bayesiana
opt_results <- BayesianOptimization(
  FUN = objective_function,
  bounds = bounds,
  init_points = 5,
  n_iter = 20,
  acq = "ei",
  kappa = 2,
  verbose = TRUE
)

# Mejor valor encontrado para la media a priori
opt_results$Best_Par
