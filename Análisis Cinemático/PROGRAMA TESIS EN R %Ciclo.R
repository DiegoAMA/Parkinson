library(readxl)
library(ggplot2)
library(randomcoloR)
library(scales)

#Funciones
cycle_process<-function(dataframe){
  num_ci<-ncol(base)/2
  ciclos<-list()
  
  i<-2
  j<-1
  while (i<= num_ci*2) {
    ciclos[[j]]<-data.frame(na.omit(base[,c(i-1,i)]))
    ciclos[[j]][,1]<-rescale(ciclos[[j]][,1],to=c(0,100))
    ciclos[[j]]<-spline(ciclos[[j]][,1],ciclos[[j]][,2],xout = 0:100)
    ciclos[[j]]<-as.data.frame(ciclos[[j]])$y
    i=i+2
    j=j+1
  }
  names(ciclos)<-paste("C",as.character(c(1:length(ciclos))),
                       sep="_")
  ciclos
} #Extracción y procesamiento de ciclos
cycles_plot<-function(ciclos_procesados){
  
  num_ci<-length(ciclos_procesados)
  color<-randomColor(num_ci)
  plot(ciclos_procesados[[1]],type="l",
       ylab="Aceleración [g]",xlab="porcentaje de ciclo %",
       col=color[1],ylim=c(-0.1,1.5))
  legend("topright",legend=c(1:num_ci),cex=0.5,fill=color)
  grid()
  for (i in seq_along(1:(num_ci-1))) {
    lines(ciclos[[i+1]],col=color[i+1])  
  }
  
  
} #Ploteo de todos los ciclos
erase_cycles<-function(cycles,...){
  v<-paste("C",c(...),sep="_")
  cycles[c(v)] <- NULL  
  cycles
}#Eliminación de ciclos

#Importación datos
setwd("C:/Users/wazud/Desktop/Maestria/INVESTIGACION/Pruebas UNAM")
url<-"P46/p46 manos mov.xlsx"
extremidad<-"Derecha" #"Izquierda"/"Derecha
base <- read_excel(url,sheet = extremidad, col_names = FALSE)

#Procesamiento
ciclos<-cycle_process(base)
cycles_plot(ciclos)
ciclos2<-erase_cycles(ciclos,3,5,4)
cycles_plot(ciclos2)

C<-cbind("% ciclo"=c(0:100),data.frame(ciclos2))#En data frame
View(C)

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
graf_ciclos<-a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12#+a13+a14+a15+a16+a17
graf_ciclos

prom<-as.data.frame(apply(C[,2:length(C)],1,mean))#Checar núm de ciclos+1
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
nombre_graf1<-"ciclos_mov_MI.png"
nombre_graf2<-"prom_mov_MI.png"
nombre_archivo<-"p14r.csv"

#Graficos ciclos

png(nombre_graf1,width = 1500, height = 750)
graf_ciclos
dev.off()

png(nombre_graf2,width = 1500, height = 750)
graf_final
dev.off()



