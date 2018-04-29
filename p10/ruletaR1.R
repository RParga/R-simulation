library(testit)

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}
objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}
normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}
n <- 50
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
init <- 200

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

  ############################mutacion
  
  # for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
  muta<-function (i){ 
    if (runif(1) < pm) {
      return( mutacion(unlist(p[i,]), n))
    }
  }
 
  ########################## cruzas sin ruleta
  #for (i in 1:rep) { # una cantidad fija de reproducciones 
  
  cruza<- function(i){
    padres <- sample(1:tam, 2, replace=FALSE)
    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    hijo1 <-  hijos[1:n] # primer hijo
    hijo2 <-  hijos[(n+1):(2*n)] # segundo hijo
    hijos<-rbind(hijo1,hijo2)
    return(hijos)
  }
  
  ########################## cruzas con ruleta
 
  cruzaruleta<- function(i){
    padres <- sample(1:tam, 2,prob = pr, replace=FALSE)
    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    hijo1 <-  hijos[1:n] # primer hijo
    hijo2 <-  hijos[(n+1):(2*n)] # segundo hijo
    hijos<-rbind(hijo1,hijo2)
    return(hijos)
  }
  
  ##########################objetivo factible
  # for (i in 1:tam) {#objetivo, factible
  fc<-function(i){
    obj <- c(objetivo(p[i,], valores))
    fact <- c(factible(p[i,], pesos, capacidad))
    res<-(cbind(obj,fact))
    return(res)
  }
  
  ##########################

for (replica in 1:1){
  
  ############################con ruleta
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  pm <- 0.05
  rep <- 50
  tmax <- 50
  mejoresrul <- double()
  
  for (iter in 1:tmax) {
    
    p$obj <- NULL
    p$fact <- NULL
    
    p<- rbind(p,foreach(i = 1:tam, .combine=rbind) %dopar% muta(i)) #mutaciones
    padresvalor<-foreach(i=1:tam, .combine = c)%dopar% objetivo(p[i,], valores)
    pr= padresvalor/sum(padresvalor)
    p<- rbind(p,foreach(i = 1:rep, .combine=rbind) %dopar% cruzaruleta(i)) #cruces con ruleta 
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
    rownames(p)<-c(1:dim(p)[1])
    p<-data.frame(sapply(p,function(x)as.numeric(as.character(x))))#Convertir de caracter a numerico
    p<-cbind(p,foreach(i=1:tam, .combine=rbind) %dopar% fc(i)) #objetivo factible
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejorul <- max(factibles$obj)
    mejoresrul <- c(mejoresrul, mejorul)
  }
  
  ###############################sin ruleta
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  pm <- 0.05
  rep <- 50
  tmax <- 50
  mejores <- double()
  
  for (iter in 1:tmax) {
    p$obj <- NULL
    p$fact <- NULL
    
    p<- rbind(p,foreach(i = 1:tam, .combine=rbind) %dopar% muta(i)) #mutaciones
    
    p<- rbind(p,foreach(i = 1:rep, .combine=rbind) %dopar% cruza(i))#Cruces sin ruleta
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
    rownames(p)<-c(1:dim(p)[1])
    p<-data.frame(sapply(p,function(x)as.numeric(as.character(x))))
    p<-cbind(p,foreach(i=1:tam, .combine=rbind) %dopar% fc(i)) #objetivo factible
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
  }
  
  stopImplicitCluster() 
  
}
g<-data.frame()

g<-cbind(1:tmax,mejores, mejoresrul)
colnames(g)=c("Paso", "Original", "Ruleta")
g<-as.data.frame(g)
  png(paste("P10R1",replica,".png"), width=600, height=300)
  library(ggplot2)
g1<-ggplot() + 
  geom_line(data = g, aes(x=Paso , y=Original, color="Original"))+
    scale_y_continuous(name="Mayor valor", limits =c( 0.95*min(g$Original), 1.05*optimo)) +
  scale_x_continuous(name="Paso", limits = c(0, 50))+
  geom_hline(yintercept=optimo, linetype="dashed", color = "green",size=0.6)
  
g1+geom_line(data = g, aes(x=g$Paso, y=g$Ruleta, color="Ruleta"))+
  guides(size=FALSE,color=guide_legend(title="Cruza"))
graphics.off()