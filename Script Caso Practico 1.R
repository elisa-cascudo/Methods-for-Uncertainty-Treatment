habitos <- read.csv("student_habits_performance.csv", header=TRUE, sep=",")
gym <- read.csv("gym_members_exercise_tracking.csv", header=TRUE, sep=",")

#diagrama de barras variables discretas
# Dividir la ventana gráfica en 1 fila y 3 columnas
par(mfrow = c(1, 3))

# Graficar los tres barplot
barplot(table(habitos$edad),
        main = "Frecuencia Edad Estudiantes",
        xlab = "Edad Estudiantes",
        ylab = "Frecuencia",
        col = "darkblue")

barplot(table(habitos$frecuencia_deporte),
        main = "Frecuencia Horas de Deporte Diarias",
        xlab = "Horas de Deporte Diarias",
        ylab = "Frecuencia",
        col = "darkblue")

barplot(table(habitos$indice_salud_mental),
        main = "Frecuencia Índice Salud Mental",
        xlab = "Índice Salud Mental",
        ylab = "Frecuencia",
        col = "darkblue")

# Opcional: volver al layout original (1 gráfico por ventana)
par(mfrow = c(1, 1))

#diagrama de sectores variables discretas
pie(table(habitos$edad), main= "Frecuencia Edad Estudiantes")
pie(table(habitos$frecuencia_deporte), main= "Frecuencia Horas de Deporte Diarias")
pie(table(habitos$indice_salud_mental), main= "Frecuencia Índice Salud Mental")

#histograma variables discretas
par(mfrow = c(2, 3))
hist(habitos$horas_estudio_por_dia,
     main= "Horas de Estudio al día",
     xlab="Horas de Estudio al Día",
     ylab="Frecuencia")

hist(habitos$horas_rrss,
     main= "Horas en RRSS al día",
     xlab="Horas en RRSS al día",
     ylab="Frecuencia")

hist(habitos$horas_netflix,
     main= "Horas en Netflix",
     xlab="Horas en Netflix",
     ylab="Frecuencia")

hist(habitos$horas_sueno,
     main= "Horas de Sueño",
     xlab="Horas de Sueño",
     ylab="Frecuencia")


hist(habitos$porcentaje_presencialidad,
     main= "Porcentaje de Presencialidad",
     xlab="Porcentaje de Presencialidad",
     ylab="Frecuencia")

hist(habitos$puntuacion_examen,
     main= "Puntuación Examen",
     xlab="Puntuación Examen",
     ylab="Frecuencia")
par(mfrow = c(1, 1))

#diagrama de sectores variables discretas



for (col in names(habitos)){
  col_data <- habitos[[col]]
  if (is.numeric(col_data)){
    percentiles <- quantile (col_data, probs= c(0.2, 0.9), na.rm=TRUE)
    iqr <- IQR(col_data)
    qvar <- var(col_data)
    n <- length(col_data)
    sd <- sd(col_data)
    asimetria <- skewness(col_data) #library e1071
    curtosis <- kurtosis(col_data)
    cat("Column: ", col, "\n")
    cat("20th percentile: ", percentiles[1], "\n")
    cat("90th percentile: ", percentiles[2], "\n")
    cat("IQR: ", iqr, "\n")
    cat("Quasi-varianza: ", qvar, "\n")
    cat("Varianza: ", qvar*(n-1)/n, "\n")
    cat("Desviacion Estandar: ", sd, "\n")
    cat("Coef Asim: ", asimetria, "\n")
    cat("Coef Curtosis: ", curtosis, "\n\n")
  }
}
  
moda <- function(x){
  table <-table(x)
  y <- as.numeric(names(table[table==max(table)]))
  return (y)
}

moda(habitos$edad)
moda(habitos$frecuencia_deporte)
moda(habitos$indice_salud_mental)

#boxplots discretas habitos
Boxplot( ~ edad + frecuencia_deporte + indice_salud_mental, data=habitos2, id=list(method="y"), ylab="")
#boxplots continuas habitos
Boxplot( ~ horas_estudio_por_dia + horas_netflix + horas_rrss + horas_sueno, data=habitos2, id=list(method="none"), ylab="")
Boxplot( ~ porcentaje_presencialidad + puntuacion_examen, data=habitos2, id=list(method="none"), ylab="")
#boxplot discreta admision
Boxplot( ~ University.Rating, data=admision, id=list(method="y"), main="Diagrama de cajas para el ranking de universidades de procedencia")


