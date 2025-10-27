#CASO PRACTICO III
#EJERCICIO 1
#instalar LearBAyes y BOlstad
# Obtén una muestra aleatoria de tamaño 1000 de la distribución a posteriori de p, y
# representa su densidad empírica2, comparándola en la misma gráfica con la densidad
# a priori y a posteriori de p3.

#POR EL EJERCICIO ANTERIOR, SABEMOS PRIORI BETA(1,1); POSTERIORI BETA(8,23)
#PRIMERO CREAMOS LA MUESTRA ALEATORIA
set.seed(123) #PARA QUE SALGAN SIEMPRE  IGUAL
x <- rbeta(1000, shape1 = 23, shape2 = 8)

plot(density(x), ylab = "Densidad", main = "Representación densidades simulada, a priori y a posteriori", col = "black") #SIMULACION

curve(dbeta(x, 1, 1), from = 0, to = 1, col = "blue", add = TRUE) #A PRIORI
curve(dbeta(x, 23, 8), from = 0, to = 1, col = "red", add = TRUE) # A POSTERIORI
#ahora una leyenda para claridad
legend("topleft",  legend = c("Empírica (simulada)", "A priori Beta(1,1)", "A
posteriori Beta(23,8)"), col = c("black", "blue", "red"), lwd = 2)

#Vamos a simular otra vez la posterior porque nos da mas informacion a traves de esta funcion
results <- binogcp(22, 29, density="beta", params=c(1,1), n.pi=1e4)
#ahora vamos a calcular la ESPERANZA y DESVIACION TIPICA A POSTERIORI
#ESPERANZA
#recuerda
# posterior the posterior probability of π given x and n
# pi the vector of possible π values used in the prior

p1 <- results$pi
p2 <- results$posterior

dens <- p1*p2
#ahora calculamos la esperanza a partir de la definicion para una distribucino continua
post.mean <- sintegral(p1, dens)$value #0.7419
class(post.mean)
#DESVIACION TIPICA A POSTERIORI
dens <- (p1-post.mean)^2*p2
post.var <- sintegral(p1,dens)$value #0.006
post.sd <- sqrt(post.var) #0.0774

#CALCULAR UN INTERVALO DE PROBABILIDAD LA 90% PARA P 
#cual es el intervalo de p que tiene un 90% robabilidad de que su valor este entre esos valores. 
#Por eso quitamos un 5% de la cola izquierda y otro de la cola derecha
#Para calcular el intervarlo de probabilidad, primero obtenemos la funcion de distribucion empirica. 
cdf <- sintegral(p1,p2,n.pts = length(p1))$cdf
#A continuacion calculamos los limites inferior y superior del intervalo de probabilidad con los comandos 
lcb <- cdf$x[with(cdf, which.max(x[y<=0.05]))] #0.606
ucb <- cdf$x[with(cdf, which.max(x[y<=0.95]))] #0.859
#Obtenemos el intervalo de p con percentil 90%

#CALCULAR LA HIPOTESIS H0 P<= 0.4 H1 P>0.4
ph0 <- cdf$y[with(cdf, which.max(y[x<=0.4]))]
ph1 <- 1 - ph0
#otra alternativa para obtener estos valores
#Integrales entre menos infinito y 0,4 y 0,4 e infinito de la probabilidad
#ya conocmeos la probabilidad, es el vector p1
p0=0.4
x0 = p1[p1<=p0]
x1 = p1[p1>p0]
P0 <- sintegral(x0, p2[p1<=p0])$value #0.00005
P1 <- sintegral(x1, p2[p1>p0])$value #0.99995


#probabilidad predictiva de que 9 de 10 ninos apruebe la educacion secundaria
# P(x>=9)=p(x=9)+P(X=10) y utilizamos la funcion de masa del modelo beta binomial para esto
#obtenido de wikipedia
#DEFINICION DE LA FUNCION BETA BINOMIAL PMF
predict <- function(k,n,a,b){
  choose(n,k)*beta(k+a,n-k+b)/beta(a,b)
}

probmas9 = predict(9,10,23,8) + predict(10, 10, 23, 8) #0.2664
