start.process <- Sys.time()

repetir = 100 
duracion = 1000

datos <-  data.frame()

for (dim in 1:100) {
  resultado <- sapply(1:repetir,
                         function(x) {
 pos <- rep(0, dim)
    origen <- rep(0, dim)
    contador <- 0
    for (t in 1:dur) {
        cambiar <- sample(1:dim, 1)
        cambio <- 1
        if (runif(1) < 0.5) {
            cambio <- -1
        }
        pos[cambiar] <- pos[cambiar] + cambio
        d <- dist(pos)
       if (sum(pos == origen) == dim) {
            contador <-  contador + 1
        }
    }
    return(contador)
}
                          )
  datos <- rbind(datos, resultado)
}

end.process <- Sys.time()

Time.process <- end.process - start.process
print(Time.process)