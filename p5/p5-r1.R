radio <- 1 
real <- 3.141592
muestras <- c(100, 200, 1000, 2000, 10000, 20000)
iteracion <- 10
i = 1
cuantos <- 500
error <- numeric(length(6*iteracion))

valor <- function() {
    base <- runif(muestra, -radio, radio)
	altura <- runif(muestra, -radio, radio)
    return(sum(base**2 + altura**2 <= 1))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
	for(muestra in muestras){
		for (replica in 1:iteracion) {
			montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% valor()
			aproximado <- sum(montecarlo)/(cuantos * muestra)
			aprox <- (4*aproximado)
			porcentaje <- abs(((real - aprox)/real)*100)
			error[i] <- porcentaje
			i = i + 1
}
}
stopImplicitCluster()
mat <- matrix(error, ncol=6, nrow=iteracion)
png("p5_pi.png")
lbls = c("100", "200", "1000", "2000", "10000", "20000")
boxplot(mat, col=c(20,7,19, 64, 98, 53), names=lbls, ylab="Porcentaje de error", xlab="Tamaño de muestra")
graphics.off()