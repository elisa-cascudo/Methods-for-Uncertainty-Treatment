#Tarea IV
#a
#Empresa  de software
x1 <- c(-1, 1, 5)
y1 <- c(0.6, 0.3, 0.1)
# Esperanza
esperanza <- sum(x1 * y1)
esperanza

# Varianza
varianza <- sum((x1 - esperanza)^2 * y1)
varianza

#Empresa  de hardware
x2 <- c(-1, 1, 3)
y2 <- c(0.4, 0.4, 0.2)
# Esperanza
esperanza <- sum(x2 * y2)
esperanza

# Varianza
varianza <- sum((x2 - esperanza)^2 * y2)
varianza


#Empresa  de biotecnologia 
x3 <- c(-1, 0, 6)
y3 <- c(0.2, 0.7, 0.1)
# Esperanza
esperanza <- sum(x3 * y3)
esperanza

# Varianza
varianza <- sum((x3 - esperanza)^2 * y3)
varianza



par(mfrow = c(1, 3))
barplot(names.arg=x1, y1, space=3, main="Empresa de Software", xlab="Posible Beneficio/Pérdida", ylab= 'Probabilidad'); abline(h=0)
barplot(names.arg=x2, y2, space=3, main="Empresa de Hardware", xlab= "Posible Beneficio/Pérdida"); abline(h=0)
barplot(names.arg=x3, y3, space=3,main= "Empresa de Biotecnología", xlab= "Posible Beneficio/Pérdida"); abline(h=0)
par(mfrow = c(1, 1))


# Parámetros
a <- 25
b <- 45

# Crear secuencia de valores para variable continua
x <- seq(a - 5, b + 5, length.out = 1000)

# Función de densidad de probabilidad (PDF)
# Para distribución uniforme continua
pdf <- ifelse(x >= a & x <= b, 1/(b - a), 0)

# Función de distribución acumulativa (CDF)
cdf <- ifelse(x < a, 0,
              ifelse(x > b, 1, (x - a)/(b - a)))

# Configurar gráficos en una cuadrícula de 1x2
par(mfrow = c(1, 2))

# Gráfica 1: Función de Densidad de Probabilidad (PDF)
plot(x, pdf,
     type = "l",  # línea continua
     lwd = 2,     # grosor de línea
     col = "blue",
     main = "Función de Densidad de Probabilidad (PDF)",
     xlab = "x",
     ylab = "f(x)",
     ylim = c(0, max(pdf) * 1.2))


# Gráfica 2: Función de Distribución Acumulativa (CDF)
plot(x, cdf,
     type = "l",  # línea continua
     lwd = 2,
     col = "red",
     main = "Función de Distribución Acumulativa (CDF)",
     xlab = "x",
     ylab = "F(x) = P(X ≤ x)",
     ylim = c(0, 1))


# Restaurar configuración de gráficos
par(mfrow = c(1, 1))

# Mostrar estadísticas
cat("Parámetros de la Distribución Uniforme Continua:\n")
cat("a =", a, "\n")
cat("b =", b, "\n")
cat("Rango:", b - a, "\n")
cat("Altura de PDF:", 1/(b - a), "\n")
cat("\nMedia teórica:", (a + b)/2, "\n")
cat("Varianza teórica:", (b - a)^2/12, "\n")
cat("Desviación estándar:", sqrt((b - a)^2/12), "\n")


# Parámetros de la distribución exponencial
# Si X ~ Exp(λ) y la media es 8, entonces λ = 1/8
media <- 8
lambda <- 1/media

# Crear un vector de valores x para las gráficas
x <- seq(0, 40, length.out = 500)

# Calcular la función de densidad (PDF)
densidad <- dexp(x, rate = lambda)

# Calcular la función de distribución (CDF)
distribucion <- pexp(x, rate = lambda)

# Configurar para mostrar dos gráficas en una ventana
par(mfrow = c(1, 2))

# Gráfica 1: Función de densidad de probabilidad (PDF)
plot(x, densidad, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     main = "Función de Densidad de Probabilidad\nX ~ Exp(λ=1/8)",
     xlab = "Tiempo de llamada (minutos)",
     ylab = "f(x)",
     las = 1)
grid()


# Gráfica 2: Función de distribución acumulada (CDF)
plot(x, distribucion, 
     type = "l", 
     col = "darkgreen", 
     lwd = 2,
     main = "Función de Distribución Acumulada\nX ~ Exp(λ=1/8)",
     xlab = "Tiempo de llamada (minutos)",
     ylab = "F(x) = P(X ≤ x)",
     las = 1)
grid()


# Restaurar la configuración de gráficas
par(mfrow = c(1, 1))

# Información adicional
cat("\n=== Información de la Distribución ===\n")
cat("Parámetro λ (rate):", lambda, "\n")
cat("Media (E[X]):", media, "minutos\n")
cat("Mediana:", round(mediana, 2), "minutos\n")
cat("Desviación estándar:", media, "minutos\n")
cat("Varianza:", media^2, "minutos²\n")


#Integrar funciones para la esperanza del ultimo ejercicio
funcion1 <- function(x) { (0.10 + 0.60 * x) * (1/8) * exp(-x / 8) }
funcion2 <- function(x) { (0.40 + 0.30 * x) * (1/8) * exp(-x / 8) }

resultado1 <-integrate(funcion1, lower = 0, upper = 1)
resultado2 <- integrate(funcion2, lower = 1, upper = Inf)

print(resultado1)
print(resultado2)


# Define lambda for Exponential(1/8)
lambda <- 1/8

# Define PDF of Exponential(1/8)
f <- function(x) {
  lambda * exp(-lambda * x)
}

# Define C(x)^2 for x in [0, 1]
C1_squared <- function(x) {
  (0.10 + 0.60 * x)^2 * f(x)
}

# Define C(x)^2 for x in (1, ∞)
C2_squared <- function(x) {
  (0.40 + 0.30 * x)^2 * f(x)
}

# Compute the two parts of the expected value
E_C_squared_1 <- integrate(C1_squared, lower = 0, upper = 1)
E_C_squared_2 <- integrate(C2_squared, lower = 1, upper = Inf)

# Add both parts
E_C_squared <- E_C_squared_1$value + E_C_squared_2$value

# Print result
cat("E[C(X)^2] ≈", round(E_C_squared, 6), "\n")

