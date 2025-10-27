master <- read.csv("Admission_Predict.csv",header=TRUE, sep=",")

barplot(table(master$University.Rating),
        main = "Ranking Universidad de Origen de Estudiantes",
        xlab = "Ranking Universidad de Origen",
        ylab = "Frecuencia",
        col = "darkblue")

boxplot(master$University.Rating, range=1.5)

summary(master$University.Rating)
moda <- function(x){
  table <-table(x)
  y <- as.numeric(names(table[table==max(table)]))
  return (y)
}
moda(master$University.Rating)

info <- function(col_data){
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
info(master$University.Rating)