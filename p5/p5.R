wolf <- 0.048834
muestras <- c(100, 200, 1000, 2000, 10000, 20000)
iteracion <- 10
i = 1
porcentaje <- numeric(length(6*iteracion))
inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
desde <- 3
hasta <- 7
cuantos <- 500
parte <- function() {
    valores <- generador(muestra)
    return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
	for (muestra in muestras) {
		for (replica in 1:iteracion) {
montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
integral <- (sum(montecarlo) / (cuantos * muestra))
resultado <- ((pi / 2) * integral)
error <- abs(((wolf - resultado)/wolf)*100)
porcentaje[i] <- error
#print (resultado)
#print (error)
i = i + 1  
}
}
stopImplicitCluster()
mat <- matrix(porcentaje, ncol=6, nrow=iteracion)
png("p5_porcentaje.png")
lbls = c("100", "200", "1000", "2000", "10000", "20000")
boxplot(mat, col=c(20,7,19, 64, 98, 53), names=lbls, ylab="Porcentaje de error", xlab="Tamaño de muestra")
graphics.off()
