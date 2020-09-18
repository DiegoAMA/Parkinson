#Exportar
library(readxl)
base <- read_excel("Pruebas/P52/p52 pies mov.xlsx",
                   sheet = "Derecha", col_names = FALSE)
#Izquierda
#Derecha
#View(base)

#Funcion para ciclos
ciclo<-function(t,g){
  A<-c(t[1:length(table(t))]-t[1])/(t[length(table(t))]-t[1])*100
  B<-c(g[1:length(table(t))])
  C<-spline(A,B,xout = 0:100)
  C<-c(0:100,data.frame(C)[1:101,2])
  dim(C)<-c(101,2)
  C
}

#funcion aplicada
N<-length(base)
CICLOS<-list()

i<-1
j<-1
while(j<=N/2 && i<=N-1){
  CICLOS[[j]]<-ciclo(base[,i,1],base[,i+1,1])
  i<-i+2
  j<-j+1
}
#dimencionamiento de ciclos y desviacion
  CP<-c(0:201)
  dim(CP)<-c(101,2)
  Des<-c(0:201)
  dim(Des)<-c(101,2)
  
#calculo promedio y desviacion
if(N/2==1){
  CP<-CICLOS[[1]]
  plot(CP,type="l",col="red",main=expression("Aceleraciones capturadas"),
       xlab="% Ciclo", ylab="Aceleración [g]",
       ylim=c(0, max(CICLOS[[1]][,2])+0.05),lwd=2)
  abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")

    
  }else if(N/2==2){
  for (i in 1:101) {
    CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2]))
    Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2]))
  }
  plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
       xlab="% Ciclo", ylab="Aceleración [g]",
       ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
  abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
  lines(CICLOS[[2]],col="blue",lwd=1)
  lines(CP,col="red",lwd=1.5)

  
  }else if(N/2==3){
  for (i in 1:101) {
    CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2]))
    Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2]))
  } 
  plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
       xlab="% Ciclo", ylab="Aceleración [g]",
       ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
  abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
  lines(CICLOS[[2]],col="blue",lwd=1)
  lines(CICLOS[[3]],col="black",lwd=1)
  lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==4){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==5){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==6){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==7){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  
  }else if(N/2==8){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==9){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==10){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==11){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==12){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==13){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CICLOS[[13]],col="cornsilk4",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==14){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CICLOS[[13]],col="cornsilk4",lwd=1)
    lines(CICLOS[[14]],col="blue4",lwd=1)
    lines(CP,col="red",lwd=1.5)
  
  
  }else if(N/2==15){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CICLOS[[13]],col="cornsilk4",lwd=1)
    lines(CICLOS[[14]],col="blue4",lwd=1)
    lines(CICLOS[[15]],col="brown4",lwd=1)
    lines(CP,col="red",lwd=1.5)
    
  }else if(N/2==16){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CICLOS[[13]],col="cornsilk4",lwd=1)
    lines(CICLOS[[14]],col="blue4",lwd=1)
    lines(CICLOS[[15]],col="brown4",lwd=1)
    lines(CICLOS[[16]],col="brown2",lwd=1)
    lines(CP,col="red",lwd=1.5)
    
  }else if(N/2==17){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2],CICLOS[[17]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2],CICLOS[[17]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CICLOS[[13]],col="cornsilk4",lwd=1)
    lines(CICLOS[[14]],col="blue4",lwd=1)
    lines(CICLOS[[15]],col="brown4",lwd=1)
    lines(CICLOS[[16]],col="brown2",lwd=1)
    lines(CICLOS[[17]],col="blue2",lwd=1)
    lines(CP,col="red",lwd=1.5)
    
  }else if(N/2==18){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2],CICLOS[[17]][i,2],CICLOS[[18]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2],CICLOS[[17]][i,2],CICLOS[[18]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CICLOS[[13]],col="cornsilk4",lwd=1)
    lines(CICLOS[[14]],col="blue4",lwd=1)
    lines(CICLOS[[15]],col="brown4",lwd=1)
    lines(CICLOS[[16]],col="brown2",lwd=1)
    lines(CICLOS[[17]],col="blue2",lwd=1)
    lines(CICLOS[[18]],col="chocolate4",lwd=1)
    lines(CP,col="red",lwd=1.5)
    
  }else if(N/2==19){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2],CICLOS[[17]][i,2],CICLOS[[18]][i,2],CICLOS[[19]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2],CICLOS[[17]][i,2],CICLOS[[18]][i,2],CICLOS[[19]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CICLOS[[13]],col="cornsilk4",lwd=1)
    lines(CICLOS[[14]],col="blue4",lwd=1)
    lines(CICLOS[[15]],col="brown4",lwd=1)
    lines(CICLOS[[16]],col="brown2",lwd=1)
    lines(CICLOS[[17]],col="blue2",lwd=1)
    lines(CICLOS[[18]],col="chocolate4",lwd=1)
    lines(CICLOS[[19]],col="cyan2",lwd=1)
    lines(CP,col="red",lwd=1.5)
    
  }else if(N/2==20){
    for (i in 1:101) {
      CP[i,2]<-mean(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2],CICLOS[[17]][i,2],CICLOS[[18]][i,2],CICLOS[[19]][i,2],CICLOS[[20]][i,2]))
      Des[i,2]<-sd(c(CICLOS[[1]][i,2],CICLOS[[2]][i,2],CICLOS[[3]][i,2],CICLOS[[4]][i,2],CICLOS[[5]][i,2],CICLOS[[6]][i,2],CICLOS[[7]][i,2],CICLOS[[8]][i,2],CICLOS[[9]][i,2],CICLOS[[10]][i,2],CICLOS[[11]][i,2],CICLOS[[12]][i,2],CICLOS[[13]][i,2],CICLOS[[14]][i,2],CICLOS[[15]][i,2],CICLOS[[16]][i,2],CICLOS[[17]][i,2],CICLOS[[18]][i,2],CICLOS[[19]][i,2],CICLOS[[20]][i,2]))
    } 
    plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
         xlab="% Ciclo", ylab="Aceleración [g]",
         ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=1)
    abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
    lines(CICLOS[[2]],col="blue",lwd=1)
    lines(CICLOS[[3]],col="black",lwd=1)
    lines(CICLOS[[4]],col="brown",lwd=1)
    lines(CICLOS[[5]],col="yellow",lwd=1)
    lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
    lines(CICLOS[[7]],col="moccasin",lwd=1)
    lines(CICLOS[[8]],col="gold",lwd=1)
    lines(CICLOS[[9]],col="darkslateblue",lwd=1)
    lines(CICLOS[[10]],col="deeppink1",lwd=1)
    lines(CICLOS[[11]],col="chartreuse2",lwd=1)
    lines(CICLOS[[12]],col="chocolate3",lwd=1)
    lines(CICLOS[[13]],col="cornsilk4",lwd=1)
    lines(CICLOS[[14]],col="blue4",lwd=1)
    lines(CICLOS[[15]],col="brown4",lwd=1)
    lines(CICLOS[[16]],col="brown2",lwd=1)
    lines(CICLOS[[17]],col="blue2",lwd=1)
    lines(CICLOS[[18]],col="chocolate4",lwd=1)
    lines(CICLOS[[19]],col="cyan2",lwd=1)
    lines(CICLOS[[20]],col="deeppink4",lwd=1)
    lines(CP,col="red",lwd=1.5)
  }
  
  
#coordenadas desv
Des1<-c(0:100,CP[,2]+Des[,2])
dim(Des1)<-c(101,2)
Des2<-c(0:100,CP[,2]-Des[,2])
dim(Des2)<-c(101,2)

plot(CP,type="l",col="red",main=expression("Promedio y desviación estandar"),
     xlab="% Ciclo", ylab="Aceleración [g]",
     ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=2)
abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
lines(Des1,col="blue",lwd=2)
lines(Des2,col="blue",lwd=2)

#Expostaciones
pdf("Pruebas/P52/P52R Pies Mov DER1.pdf",width=11.39,height=8.27)
plot(CICLOS[[1]],type="l",col="green",main=expression("Aceleraciones capturadas"),
     xlab="% Ciclo", ylab="Aceleración [g]",
     ylim=c(0, max(CP[,2])+max(Des[,2])+0.1),lwd=1)
abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
lines(CICLOS[[2]],col="blue",lwd=1)
lines(CICLOS[[3]],col="black",lwd=1)
lines(CICLOS[[4]],col="brown",lwd=1)
lines(CICLOS[[5]],col="yellow",lwd=1)
lines(CICLOS[[6]],col="mediumslateblue",lwd=1)
lines(CICLOS[[7]],col="moccasin",lwd=1)
lines(CICLOS[[8]],col="gold",lwd=1)
lines(CICLOS[[9]],col="darkslateblue",lwd=1)
lines(CICLOS[[10]],col="deeppink1",lwd=1)
lines(CICLOS[[11]],col="chartreuse2",lwd=1)
lines(CICLOS[[12]],col="chocolate3",lwd=1)
lines(CICLOS[[13]],col="cornsilk4",lwd=1)
lines(CICLOS[[14]],col="blue4",lwd=1)
lines(CICLOS[[15]],col="brown4",lwd=1)
lines(CICLOS[[16]],col="brown2",lwd=1)
lines(CICLOS[[17]],col="blue2",lwd=1)
lines(CICLOS[[18]],col="chocolate4",lwd=1)
lines(CICLOS[[19]],col="cyan2",lwd=1)
lines(CICLOS[[20]],col="deeppink4",lwd=1)
lines(CP,col="red",lwd=1.5)
dev.off()
dev.off()

pdf("Pruebas/P52/P52R Pies Mov DER2.pdf",width=11.39,height=8.27)
plot(CP,type="l",col="red",main=expression("Promedio y desviación estandar"),
     xlab="% Ciclo", ylab="Aceleración [g]",
     ylim=c(0, max(CP[,2])+max(Des[,2])),lwd=2)
abline(h=c(0.5,1,1.5),v=c(10,20,30,40,50,60,70,80,90),col="pink")
lines(Des1,col="blue",lwd=2)
lines(Des2,col="blue",lwd=2)
dev.off()

library(xlsx)
write.xlsx(data.frame(CP,Des[,2]), "Pruebas/P52/RESULTADOs.xlsx") 

#width=11.39,height=8.27

