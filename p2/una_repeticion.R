library(parallel)
dim <- 10
num <-  dim^2
p=0.2
actual <- matrix((runif(num)<p), nrow=dim, ncol=dim)
suppressMessages(library("sna"))
png("p2_t0.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
graphics.off()
 
paso <- function(pos) {
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
 
for (iteracion in 1:2) {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)
    if (sum(siguiente) == 0) { # todos murieron
        print("Ya no queda nadie vivo.")
        break;
    }
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    salida = paste("p2_t", iteracion, ".png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
}
