setwd("C:/Users/wazud/Desktop/Maestria/INVESTIGACION/Pruebas UNAM")

library(readxl)
library(ggplot2)
library(randomcoloR)

#Exportar
url<-"P7/P7 reposo manos.xlsx"
extremidad<-"Izquierda"
#extremidad<-"Derecha"

base <- read_excel(url,sheet = extremidad, col_names = FALSE)

#Ciclos
num_ci<-length(base[1,])/2
ciclos<-list()

i<-2
j<-1
while (i<= num_ci*2) {
  ciclos[[j]]<-data.frame(na.omit(base[,c(i-1,i)]))
  MIN<-min(ciclos[[j]][,1])
  MAX<-max(ciclos[[j]][,1])
  
  k<-1
  for (k in k:length(ciclos[[j]][,1])) {
    ciclos[[j]][k,1]<-((ciclos[[j]][k,1]-MIN)/(MAX-MIN))*100
  }
  ciclos[[j]]<-data.frame(spline(ciclos[[j]][,1],ciclos[[j]][,2],xout = 0:100))[,2]
  i=i+2
  j=j+1
  
}

color<-randomColor(num_ci)
plot(ciclos[[1]],type="l",
     ylab="Aceleración [m/s^2]",xlab="porcentaje de ciclo %",
     col=color[1],ylim=c(0.5,1.5))
legend("topright",legend=c(1:num_ci),cex=0.5,fill=color)
grid()
lines(ciclos[[2]],col=color[2])  
lines(ciclos[[3]],col=color[3])  
lines(ciclos[[4]],col=color[4])  
lines(ciclos[[5]],col=color[5])  
lines(ciclos[[6]],col=color[6])  
lines(ciclos[[7]],col=color[7])  
lines(ciclos[[8]],col=color[8])  
lines(ciclos[[9]],col=color[9])  
lines(ciclos[[10]],col=color[10])  
lines(ciclos[[11]],col=color[11])  
lines(ciclos[[12]],col=color[12])  
lines(ciclos[[13]],col=color[13])  
lines(ciclos[[14]],col=color[14])  
lines(ciclos[[15]],col=color[15])  
lines(ciclos[[16]],col=color[16])  
lines(ciclos[[17]],col=color[17])  
lines(ciclos[[18]],col=color[18])  
lines(ciclos[[19]],col=color[19])  
lines(ciclos[[20]],col=color[20])  


#Eliminación de ciclos
names(ciclos)<-as.character(c(1:length(ciclos)))
ciclos[as.character(c(1,7,9,10,11,12))] <- NULL

porc<-data.frame(0:100)
C<-data.frame(cbind(porc,as.data.frame(ciclos)))
length(C[1,])-1
names(C)<-c("porcentaje",as.character(1:(length(C)-1)))#Checar los eliminados
View(C)
names(C)

tema<-theme(axis.text=element_text(size=20),
            axis.title=element_text(size=22,face="bold"),
            plot.title = element_text(size = 30, face = "bold"))

a1<-ggplot(data=C,aes(x=C[,1],y=C[,2],col=color[1]))+
  geom_line(show.legend = FALSE)+
  xlab("Ciclo [%]")+
  ylab("Aceleración [g]")+
  ggtitle("Ciclos Obtenidos")+tema

#####Ciclos a plotear####
a2<-geom_line(data=C,aes(y=C[,3]),col=color[2])
a3<-geom_line(data=C,aes(y=C[,4]),col=color[3])  
a4<-geom_line(data=C,aes(y=C[,5]),col=color[4])  
a5<-geom_line(data=C,aes(y=C[,6]),col=color[5])  
a6<-geom_line(data=C,aes(y=C[,7]),col=color[6])  
a7<-geom_line(data=C,aes(y=C[,8]),col=color[7])  
a8<-geom_line(data=C,aes(y=C[,9]),col=color[8])  
a9<-geom_line(data=C,aes(y=C[,10]),col=color[9])  
a10<-geom_line(data=C,aes(y=C[,11]),col=color[10])  
a11<-geom_line(data=C,aes(y=C[,12]),col=color[11])  
a12<-geom_line(data=C,aes(y=C[,13]),col=color[12])  
a13<-geom_line(data=C,aes(y=C[,14]),col=color[13])  
a14<-geom_line(data=C,aes(y=C[,15]),col=color[14])  
a15<-geom_line(data=C,aes(y=C[,16]),col=color[15])  
a16<-geom_line(data=C,aes(y=C[,17]),col=color[16])  
a17<-geom_line(data=C,aes(y=C[,18]),col=color[17])
a18<-geom_line(data=C,aes(y=C[,19]),col=color[18])  
a19<-geom_line(data=C,aes(y=C[,20]),col=color[19])  
a20<-geom_line(data=C,aes(y=C[,21]),col=color[20])  

#####Preparación resultados####
length(C)-1
graf_ciclos<-a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15#+a16+a17+a18+a19+a20
graf_ciclos

prom<-as.data.frame(apply(C[,2:length(C)],1,mean))#Checar núm ciclos
desv<-as.data.frame(apply(C[,2:length(C)],1,sd))

result<-cbind(C$porcentaje,prom,desv)
names(result)<-c("p_ciclo","promedio","desviación_estandar")
plot.data<-cbind(result,lower=result$promedio-result$desviación_estandar,
                 upper=result$promedio+result$desviación_estandar)


graf<-ggplot(data=plot.data,aes(x=p_ciclo,y=promedio))+
  geom_line(col="red",size=1.2)+
  geom_ribbon(data=plot.data,aes(ymin=lower,ymax=upper,x=p_ciclo),
              alpha=0.3,colour="darkblue",fill="lightblue")

graf_final<-graf+ggtitle("Aceleración Promedio y Desviación Estandar")+
  xlab("Porcentaje de Ciclo [%]")+
  ylab("Aceleración [g]")+tema

graf_final


###Salvar información####
setwd("C:/Users/wazud/Desktop/Maestria/Repositorio Tesis/Parkinson Repositorio")
nombre_graf1<-"ciclos_PulD.png"
nombre_graf2<-"prom_PulD.png"
nombre_archivo<-"p14r.csv"

#Graficos ciclos

png(nombre_graf1,width = 1500, height = 750)
graf_ciclos
dev.off()

png(nombre_graf2,width = 1500, height = 750)
graf_final
dev.off()



