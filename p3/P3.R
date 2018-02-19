
primo <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
 
Nprimos = read.csv("primes1.txt", sep=" ", header=FALSE) #eliminar encabezado de txt
Nprimos$V1 = NULL #borrar primer columna[nombre]
Nprimos$V10 = NULL #borrar ultima columna[nombre]
m = as.matrix(Nprimos) #crear matriz de ...
v = as.vector(t(m)) #crear vector de una matriz

veci <- v[seq(1, 1000000, 20000)] #elegir numeros al azar
vecp <- v[999951:1000000]
vecr <- v[1:50]
vecnp <- (1:50)
vecf <- sort(rbind (vecr, veci, vecp, vecnp))

menmay <- vecf[1:200]
maymen <- vecf[200:1]
aleatorio1 <- sample(maymen)
aleatorio2 <- sample(maymen)
aleatorio3 <- sample(maymen)


replicas <- 50
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
ot <-  numeric()
it <-  numeric()
at1 <-  numeric()
at2 <-  numeric()
at3 <-  numeric()

for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = menmay, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = maymen, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    	at1 <- c(at1, system.time(foreach(n = aleatorio1, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
	at2 <- c(at2, system.time(foreach(n = aleatorio2, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
	at3 <- c(at3, system.time(foreach(n = aleatorio3, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
	
}
stopImplicitCluster()
summary(ot)
summary(it)
summary(at1)
summary(at2)
summary(at3)

tiempo <- t(rbind(ot, it, at1, at2, at3))
png("p3_tiempos1.png")
boxplot (tiempo)
graphics.off()