
source("distancia.R")
source("caminata.R")

#Tiempo dimensiones

num=10
dim=1
elapsedtime <- rep(0, dim)
for (i in 1:num) { time=system.time(caminata(dim, 5000,ed.orig))
elapsedtime[dim]=time[3]
dim=dim+1
}
elapsedtime
plot(1:10,elapsedtime,xlab="Dimensiones",type="o",col="red", ylab="Tiempo", main="porcentaje de tiempo entre cada dimension")



#Porcentaje

dim <- 10
resu = numeric()
porcent <- rep(0, dim)
var=1
for (i in 1:dim) { resu = caminata(var, 5000, md.orig) 
p <-(resu*100)/5000
porcent[var] <- p
var <- var+1
}
porcent
plot(1:dim,porcent,xlab="Dimensiones",type="o",col="red", ylab="Tiempo", main="porcentaje de veces que cae en cada  dimension")
)