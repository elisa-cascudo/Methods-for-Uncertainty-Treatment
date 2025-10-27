# Instalar LearnBayes y Bolstad
# TAREA IV
# Modelo Normal-Normal

# Primero introducimos los datos proporcionados en el enunciado
sigma.y <- 10
mu <- c(20, 30, 40, 50, 60, 70)
prob.mu <- c(0.1, 0.15, 0.25, 0.25, 0.15, 0.1)
y <- c(38.6, 42.4, 57.5, 40.5, 51.7, 67.1, 33.4, 60.9, 64.1, 40.1, 40.7, 6.4)

# Distribución a priori (normal discreta)
barplot(
  prob.mu,
  names.arg = mu,
  xlab = "Media",
  ylab = "Distribución a priori",
  main = "Distribución a priori"
)

#funcion de verosimilitud

verosimilitud <- function(mu){
  prod(dnorm(y, mean=mu, sd = sigma.y))
}

verosimilitud.mu <- sapply(mu, verosimilitud)
vero.data <- data.frame(mu = mu, Verosimilitud = verosimilitud.mu)

#CALCULAR LA DISTRIBUCION A POSTERIORI PARA MU
dist.unnorm <- verosimilitud.mu * prob.mu 
dist.posterior <- dist.unnorm / sum(dist.unnorm )
dist.postdata <- data.frame(Media=mu, Distribucion = dist.posterior)
# aqui hemos calculado el vector de las nuevas probabilidades asignadas a las mu del inicio!


#calcular un intervalo de probabilidad al 80% para mu
prob=0.8
mat <- as.matrix(dist.postdata)
discint(mat, prob)

#REPETIR CALCULO DE DISTRIBUCION A POSTERIORI Y CALCULAR UN INTERVALO DE PROBABILIDAD AL 80% PARA MU CON MU0=60 Y SIGMA0=5
mu0=60
sigma0=5
n.mu=1e4
n <- length(y)
y_bar <- mean(y)
#Nueva distribucion
results <- normgcp(y, sigma.y, density="normal", params=c(mu0, sigma0), n.mu)
sigma_post <- sqrt(1 / (n / sigma.y^2 + 1 / sigma0^2)) #resultado: 2.5
mu_post<- sigma_final^2 * (n * y_bar / sigma.y^2 + mu0 / sigma0^2) #resultado: 48.96

#Calculo del intervalo
p1 <- results$mu
p2 <- results$posterior
#Intervalos de probabilidad 80%
cdf <- sintegral(p1,p2,n.pts = length(p1))$cdf
#A continuacion calculamos los limites inferior y superior del intervalo de probabilidad con los comandos 
lcb <- cdf$x[with(cdf, which.max(x[y<=0.1]))] #45.82
ucb <- cdf$x[with(cdf, which.max(x[y<=0.9]))] #52.17
#Obtenemos el intervalo de p con percentil 80%