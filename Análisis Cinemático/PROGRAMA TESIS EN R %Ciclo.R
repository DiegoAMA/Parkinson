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
all_ggplot<-function(df,x="p"){
  num_ci<-length(df)-1
  x<-ifelse(x=="p","Ciclo [%]","Segundo [s]")
  color<-randomColor(num_ci)
  tema<-theme(axis.text=element_text(size=20),
              axis.title=element_text(size=22,face="bold"),
              plot.title = element_text(size = 30, face = "bold"))
  plot<-ggplot(data=C,aes(x=C[,1],y=C[,2],col=color[1]))+
    geom_line(show.legend = FALSE)+
    xlab(x)+
    ylab("Aceleración [g]")+
    ggtitle("Ciclos Obtenidos")+tema
  for (i in seq_along(1:(num_ci-1))) {
    plot <- plot + geom_line(aes_string(y = names(df)[i+1]),col=color[i+1])
  }
  plot
}

#Importación datos
setwd("C:/Users/wazud/Desktop/Maestria/INVESTIGACION/Pruebas UNAM")
url<-"P46/p46 manos mov.xlsx"
extremidad<-"Derecha" #"Izquierda"/"Derecha
base <- read_excel(url,sheet = extremidad, col_names = FALSE)

#Procesamiento
ciclos<-cycle_process(base)
cycles_plot(ciclos)
ciclos_f<-erase_cycles(ciclos,13,5,4)
cycles_plot(ciclos_f)

C<-cbind("% ciclo"=c(0:100),data.frame(ciclos_f))#En data frame
View(C)

#####Ciclos finales en ggplot####
all_ggplot(C)

prom<-as.data.frame(apply(C[,2:length(C)],1,mean))#aceleracion promedio
desv<-as.data.frame(apply(C[,2:length(C)],1,sd))#desviacion estandar

result<-cbind(C$`% ciclo`,prom,desv)
names(result)<-c("p_ciclo","promedio","desviación_estandar")
plot.data<-cbind(result,lower=result$promedio-result$desviación_estandar,
                 upper=result$promedio+result$desviación_estandar)


graf<-ggplot(data=plot.data,aes(x=p_ciclo,y=promedio))+
  geom_line(col="red",size=1.2)+
  geom_ribbon(data=plot.data,aes(ymin=lower,ymax=upper,x=p_ciclo),
              alpha=0.3,colour="darkblue",fill="lightblue")
graf
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



