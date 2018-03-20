x=data.frame()
Resultados=data.frame()
for (i in 1:5){
  
  source('paralelizada.R', encoding = 'UTF-8')
  
  source('p6no.R', encoding = 'UTF-8')
  Resultados=cbind(TiempoT,TiempoO) 
  x=rbind(x,Resultados)  
}

colnames(x)=c("Paralelizado",No paralelizado")
png("Prac6.png",width=600, height=800,pointsize = 20)
boxplot(x,col=c("Blue","Red"),ylab="Tiempo de ejecución (s)")
graphics.off()