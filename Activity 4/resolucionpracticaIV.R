bhd <- read.table(file="BHD.txt", header = FALSE)
colnames(bhd) <- c('CRIM','ZN','INDUS','CHAS','NOX','RM','AGE','DIS','RAD','TAX','PTRATIO','B','LSTAT','MEDV')



#MEDV: valor medio de casas ocupadas por sus propietarios en 1000 d´olares.
#Primero hacemos el plot de las variables unas con otras
par(mfrow = c(3, 3))  # Set up 4x4 plotting area
for (x in colnames(bhd)) {
  if (x != "MEDV") {
    plot(bhd[[x]], bhd$MEDV,
         xlab = x,
         ylab = "MEDV",
         main = paste(x, "vs MEDV"))
  }
}
par(mfrow = c(1, 2))
plot(bhd$B, bhd$MEDV,
     xlab = "B",
     ylab = "MEDV",
     main = "B vs MEDV")
plot(sqrt(bhd$B), bhd$MEDV,
     xlab = "sqrt(B)",
     ylab = "MEDV",
     main = "sqrt(B) vs MEDV")

logdis = log(bhd$DIS)
plot(logdis, bhd$MEDV,
     xlab = "DIS",
     ylab = "MEDV",
     main = "DIS vs MEDV")

#indus la relacion lineal es muy debil ademas despues en summary index nos da un p value muy elevado, de 0.83 demostrando que no es significativo

#Para CRIM:
# El p-valor ≈ 0.93 indica que, condicionalmente a las demás variables del modelo, no hay evidencia de que log(CRIM) tenga un efecto distinto de cero sobre MEDV.
# En otras palabras: cuando controlas por todo lo demás (NOX, RM, TAX, etc.), CRIM ya no explica variación adicional.
#Por ejemplo el valor socio economico ya puede ser un suficiene indicativo

#En una primera instancia: 
fit <- lm(
  bhd$MEDV ~ log(bhd$CRIM) + bhd$INDUS + bhd$CHAS + bhd$NOX +
    bhd$RM + log(bhd$DIS) + bhd$RAD + bhd$TAX +
    bhd$PTRATIO + log(bhd$LSTAT),
  data = bhd,
  x = TRUE,
  y = TRUE
)

summary(fit)
#B la quito porque incluso con la transformacion sqrt se mantiene muy pegada a la derecha
#Estimacion de minimos cuadrados

fit <- lm( bhd$MEDV ~ bhd$CHAS + bhd$NOX + bhd$RM + log(bhd$DIS) + bhd$RAD + bhd$TAX + bhd$PTRATIO + log(bhd$LSTAT), data = bhd, x=TRUE,y=TRUE)
summary(fit)

#Distribucion a posteriori
#simulamos los posibles valores de los coeficiente sy sus probabilidades
theta.sample=blinreg(fit$y,fit$x,5000)


# Representamos los histogramas de los coeficientes y sigma
par(mfrow = c(3, 3))

hist(theta.sample$beta[,1], main = 'CHAS',       xlab = expression(beta[1]))
hist(theta.sample$beta[,2], main = 'NOX',        xlab = expression(beta[2]))
hist(theta.sample$beta[,3], main = 'RM',         xlab = expression(beta[3]))
hist(theta.sample$beta[,4], main = 'log(DIS)',   xlab = expression(beta[4]))
hist(theta.sample$beta[,5], main = 'RAD',        xlab = expression(beta[5]))
hist(theta.sample$beta[,6], main = 'TAX',        xlab = expression(beta[6]))
hist(theta.sample$beta[,7],  main = 'PTRATIO',    xlab = expression(beta[7]))
hist(theta.sample$beta[,8], main = 'log(LSTAT)', xlab = expression(beta[8]))
hist(theta.sample$sigma,     main = 'ERROR SD',   xlab = expression(sigma))

# El modelo está bien comportado (posteriores normales, sin multimodalidades).
# 
# Algunos coeficientes tienen alta certeza en su signo (CHAS, RAD, PTRATIO, log(LSTAT)).
# 
# Otros requieren revisar colinealidad (NOX, RM, TAX).
# 
# La incertidumbre global (σ) es moderada, lo que indica un buen ajuste general.

# A continuaci´on, podemos calcular para cada par´ametro individual sus percentiles 5,
# 50 y 95. Para la salida, usamos los comandos apply and quantile para resumir los
# valores de la matriz de simulaciones de , theta.sample$beta. De manera an´aloga se
# procede con las simulaciones de .
apply(theta.sample$beta,2,quantile,c(0.05,0.5,0.95))
quantile(theta.sample$sigma, c(.05, .5, .95))

#Ahora predecimos una estimacion, no calculamos una predicion, segun distintos escenarios
## Valores fijos (medias del modelo)
NOX      <-  2.6184
RM       <-  1.7332
TAX      <- -0.0127
PTRATIO  <- -0.82531
logLSTAT <- -9.2302

## Niveles que vamos a comparar
CHAS0 <- 0; CHAS1 <- 1
logDIS_low  <- 2.25
logDIS_high <- 3.25
RAD_low  <- -7
RAD_high <- -5

## OJO: cada covariate row debe seguir el mismo orden del modelo:
## c(1, CHAS, NOX, RM, log(DIS), RAD, TAX, PTRATIO, log(LSTAT))

## 8 escenarios (2 x 2 x 2)
cov1 <- c(1, CHAS0, NOX, RM, logDIS_low,  RAD_low,  TAX, PTRATIO, logLSTAT)
cov2 <- c(1, CHAS1, NOX, RM, logDIS_low,  RAD_low,  TAX, PTRATIO, logLSTAT)
cov3 <- c(1, CHAS0, NOX, RM, logDIS_high, RAD_low,  TAX, PTRATIO, logLSTAT)
cov4 <- c(1, CHAS1, NOX, RM, logDIS_high, RAD_low,  TAX, PTRATIO, logLSTAT)
cov5 <- c(1, CHAS0, NOX, RM, logDIS_low,  RAD_high, TAX, PTRATIO, logLSTAT)
cov6 <- c(1, CHAS1, NOX, RM, logDIS_low,  RAD_high, TAX, PTRATIO, logLSTAT)
cov7 <- c(1, CHAS0, NOX, RM, logDIS_high, RAD_high, TAX, PTRATIO, logLSTAT)
cov8 <- c(1, CHAS1, NOX, RM, logDIS_high, RAD_high, TAX, PTRATIO, logLSTAT)

X1 <- rbind(cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8)

## Estimación de la media esperada E[MEDV | X] desde la posterior
mean.draws=blinregexpected(X1,theta.sample)

## Histos de las 8 distribuciones (medias esperadas por escenario)
par(mfrow = c(2,4), mar=c(4,4,3,1))
hist(mean.draws[,1], main="CHAS=0, logDIS=2.25, RAD=-7",  xlab="E[MEDV]")
hist(mean.draws[,2], main="CHAS=1, logDIS=2.25, RAD=-7",  xlab="E[MEDV]")
hist(mean.draws[,3], main="CHAS=0, logDIS=3.25, RAD=-7",  xlab="E[MEDV]")
hist(mean.draws[,4], main="CHAS=1, logDIS=3.25, RAD=-7",  xlab="E[MEDV]")
hist(mean.draws[,5], main="CHAS=0, logDIS=2.25, RAD=-5",  xlab="E[MEDV]")
hist(mean.draws[,6], main="CHAS=1, logDIS=2.25, RAD=-5",  xlab="E[MEDV]")
hist(mean.draws[,7], main="CHAS=0, logDIS=3.25, RAD=-5",  xlab="E[MEDV]")
hist(mean.draws[,8], main="CHAS=1, logDIS=3.25, RAD=-5",  xlab="E[MEDV]")

#Ahora predecimos una estimacion, no calculamos una predicion, segun distintos escenarios

NOX      <-  2.6184
RM       <-  1.7332
TAX      <- -0.0127
PTRATIO  <- -0.82531
logLSTAT <- -9.2302


CHAS0 <- 0; CHAS1 <- 1
logDIS_low  <- 2.25
logDIS_high <- 3.25
RAD_low  <- -7


cov1 <- c(1, CHAS0, NOX, RM, logDIS_low,  RAD_low,  TAX, PTRATIO, logLSTAT)
cov2 <- c(1, CHAS1, NOX, RM, logDIS_low,  RAD_low,  TAX, PTRATIO, logLSTAT)
cov3 <- c(1, CHAS0, NOX, RM, logDIS_high, RAD_low,  TAX, PTRATIO, logLSTAT)
cov4 <- c(1, CHAS1, NOX, RM, logDIS_high, RAD_low,  TAX, PTRATIO, logLSTAT)
cov5 <- c(1, CHAS0, NOX, RM, logDIS_low,  RAD_high, TAX, PTRATIO, logLSTAT)
cov6 <- c(1, CHAS1, NOX, RM, logDIS_low,  RAD_high, TAX, PTRATIO, logLSTAT)
cov7 <- c(1, CHAS0, NOX, RM, logDIS_high, RAD_high, TAX, PTRATIO, logLSTAT)
cov8 <- c(1, CHAS1, NOX, RM, logDIS_high, RAD_high, TAX, PTRATIO, logLSTAT)

X1 <- rbind(cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8)

mean.draws <- blinregpred(X1, theta.sample)

par(mfrow = c(2,4), mar=c(4,4,3,1))
hist(mean.draws[,1], main="CHAS=0, logDIS=2.25, RAD=-7",  xlab="E[MEDV]")
hist(mean.draws[,2], main="CHAS=1, logDIS=2.25, RAD=-7",  xlab="E[MEDV]")
hist(mean.draws[,3], main="CHAS=0, logDIS=3.25, RAD=-7",  xlab="E[MEDV]")
hist(mean.draws[,4], main="CHAS=1, logDIS=3.25, RAD=-7",  xlab="E[MEDV]")
hist(mean.draws[,5], main="CHAS=0, logDIS=2.25, RAD=-5",  xlab="E[MEDV]")
hist(mean.draws[,6], main="CHAS=1, logDIS=2.25, RAD=-5",  xlab="E[MEDV]")
hist(mean.draws[,7], main="CHAS=0, logDIS=3.25, RAD=-5",  xlab="E[MEDV]")
hist(mean.draws[,8], main="CHAS=1, logDIS=3.25, RAD=-5",  xlab="E[MEDV]")

#Evaluacion del modelo con residuos bayesianos
prob.out=bayesresiduals(fit,theta.sample,2)
par(mfrow=c(1,1))
plot(bhd$MEDV,prob.out)