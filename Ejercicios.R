#Ejercicio 2
# Librerías
library(ggplot2)
library(dplyr)

# Configuración inicial
set.seed(123)
n <- 100
pi_real <- 0.4
alpha_prior <- 2
beta_prior <- 2

# Simulación de y real
y <- rbinom(1, n, pi_real)

# Función para calcular posterior ajustada por nivel de mentira
posterior <- function(y, n, mu = 0) {
  y_observado <- y - rbinom(1, y, mu)
  alpha_post <- alpha_prior + y_observado
  beta_post <- beta_prior + n - y_observado
  list(y = y, y_obs = y_observado, alpha = alpha_post, beta = beta_post)
}

# Simulaciones para los 4 casos usando el mismo y real
sin_mentiras <- posterior(y, n, mu = 0)
mentira_baja <- posterior(y, n, mu = 0.1)
mentira_media <- posterior(y, n, mu = 0.3)
mentira_alta <- posterior(y, n, mu = 0.5)

# Opcional: mostrar valores observados
cat("y real:", y, "\n")
cat("Sin mentiras:", sin_mentiras$y_obs, "\n")
cat("Mentira baja:", mentira_baja$y_obs, "\n")
cat("Mentira media:", mentira_media$y_obs, "\n")
cat("Mentira alta:", mentira_alta$y_obs, "\n")

# Crear data frame para graficar
x_vals <- seq(0, 1, length.out = 1000)

posteriores <- tibble(
  x = rep(x_vals, 4),
  densidad = c(
    dbeta(x_vals, sin_mentiras$alpha, sin_mentiras$beta),
    dbeta(x_vals, mentira_baja$alpha, mentira_baja$beta),
    dbeta(x_vals, mentira_media$alpha, mentira_media$beta),
    dbeta(x_vals, mentira_alta$alpha, mentira_alta$beta)
  ),
  caso = factor(rep(c("Sin mentiras", "Mentira baja", "Mentira media", "Mentira alta"),
                    each = length(x_vals)))
)

# Gráfico con ggplot2
ggplot(posteriores, aes(x = x, y = densidad, color = caso)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#ffa600","#97bd3f", "#3fbb8d","#48abb5")) +
  labs(title = "Muestras Según Niveles de Mentira",
       x = expression(pi[A]),
       y = "Densidad",
       color = "Caso") +
  theme_minimal(base_size=10)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5))


#Ejercicio 3
# Ejercicio 2 - Versión con 1000 muestras
library(ggplot2)
library(dplyr)

# Configuración inicial
set.seed(123)
n <- 100
pi_real <- 0.4
alpha_prior <- 2
beta_prior <- 2
nsim <- 1000
x_vals <- seq(0, 1, length.out = 1000)

# Simulación de y real
y <- rbinom(1, n, pi_real)

# Función para calcular posterior ajustada por nivel de mentira
posterior <- function(y, n, mu = 0) {
  y_observado <- y - rbinom(1, y, mu)
  alpha_post <- alpha_prior + y_observado
  beta_post <- beta_prior + n - y_observado
  c(alpha = alpha_post, beta = beta_post)
}

# Función para obtener densidades promedio por nivel de mentira
simular_densidades <- function(mu, y, n, nsim, x_vals) {
  densidades <- matrix(0, nrow = nsim, ncol = length(x_vals))
  
  for (i in 1:nsim) {
    post <- posterior(y, n, mu)
    densidades[i, ] <- dbeta(x_vals, post["alpha"], post["beta"])
  }
  
  colMeans(densidades)
}

# Simulaciones para cada caso
dens_sin <- simular_densidades(mu = 0, y, n, nsim, x_vals)
dens_baja <- simular_densidades(mu = 0.1, y, n, nsim, x_vals)
dens_media <- simular_densidades(mu = 0.3, y, n, nsim, x_vals)
dens_alta <- simular_densidades(mu = 0.5, y, n, nsim, x_vals)

# Crear data frame para graficar
posteriores <- tibble(
  x = rep(x_vals, 4),
  densidad = c(dens_sin, dens_baja, dens_media, dens_alta),
  caso = factor(rep(c("Sin mentiras", "Mentira baja", "Mentira media", "Mentira alta"),
                    each = length(x_vals)))
)

# Gráfico con ggplot2
ggplot(posteriores, aes(x = x, y = densidad, color = caso)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("#ffa600","#97bd3f", "#3fbb8d","#48abb5")) +
  labs(title = "Muestras Según Niveles de Mentira",
       x = expression(pi[A]),
       y = "Densidad",
       color = "Caso") +
  theme_minimal(base_size = 10) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5)
  )
