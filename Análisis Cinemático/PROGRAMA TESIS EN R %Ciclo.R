library(readxl)
library(ggplot2)
library(randomcoloR)
library(scales)
library(tidyverse)

#Funciones--------------------
cycle_process<-function(dataframe){
  num_ci<-ncol(dataframe)/2
  ciclos<-list()
  if(num_ci!=0){
    i<-2
    j<-1
    while (i<= num_ci*2) {
      ciclos[[j]]<-data.frame(na.omit(dataframe[,c(i-1,i)]))
      ciclos[[j]][,1]<-rescale(ciclos[[j]][,1],to=c(0,100))
      ciclos[[j]]<-spline(ciclos[[j]][,1],ciclos[[j]][,2],xout = 0:100)
      ciclos[[j]]<-as.data.frame(ciclos[[j]])$y
      i=i+2
      j=j+1
    }
    names(ciclos)<-paste("C",as.character(c(1:length(ciclos))),
                         sep="_")
  }else{
    ciclos<-NA
  }
  ciclos
} #Extracción y procesamiento de ciclos
cycles_boxplot<-function(ciclos_procesados){
  ciclos_procesados<-t(as.data.frame(ciclos_procesados))
  max<-max(ciclos_procesados)
  min<-min(ciclos_procesados)
  boxplot(ciclos_procesados,
          ylab="Aceleración [g]",xlab="porcentaje de ciclo %",
          col="darkred",ylim=c(min-0.05,max+0.05))
  grid(nx=101,ny=NULL)
  
} #Boxplot de los ciclos para detectar outliers
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
replace_outliers<-function(data){
  data<-t(as.data.frame(data))
  qrts<-quantile(data,probs = c(0.25,0.75))
  caps<-quantile(data,probs = c(0.05,0.95))
  iqr<-qrts[2]-qrts[1]
  h<-1.5*iqr
  data[data<qrts[1]-h]<-caps[1]
  data[data>qrts[2]+h]<-caps[2]
  as.data.frame(t(data))
}#Remplaza outliers por el percentil 10 y 90 respectivamente
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
}#Todos los ciclos finales en ggplot
mean_ds_ggplot<-function(df){
  tema<-theme(axis.text=element_text(size=20),
              axis.title=element_text(size=22,face="bold"),
              plot.title = element_text(size = 30, face = "bold"))
  graf<-ggplot(data=df,aes(x=p_ciclo,y=promedio))+
    geom_line(col="red",size=1.2)+
    geom_ribbon(data=df,aes(ymin=lower,ymax=upper,x=p_ciclo),
                alpha=0.3,colour="darkblue",fill="lightblue")
  graf_final<-graf+ggtitle("Aceleración Promedio y Desviación Estandar")+
    xlab("Porcentaje de Ciclo [%]")+
    ylab("Aceleración [g]")+tema
  
  graf_final
}#Dado p_ciclo, lower, upper grafica el prom y su desv

#Importación datos---------------------
#(Ya estan guardados como datos.RData, con un load se cargan)
#Creación de lista de los datos
archivos<-list.files(path="Datos Cinemáticos/",
                     pattern = ".xlsx")
hojas<-paste("Hoja",1:52,sep="")#Para extraccion
pacientes<-paste("P",1:52,sep="")#para la lista

#creacion de lista#
datos<-as.list(1:10)
names<-str_sub(archivos,1,str_locate(archivos,".xlsx")[,1]-1)#Sustrae nombre unicamente
names(datos)<-names#asigna nombres
datos<-lapply(datos, list)
for (i in seq_along(names)) {
  datos[[i]]<-lapply(pacientes,list)
  names(datos[[1]])<-pacientes
}

for (i in seq_along(archivos)) {
  for (j in seq_along(pacientes)) {
    a<-paste("Datos Cinemáticos/",archivos[i],sep="")
    datos[[i]][[j]]<-read_excel(a,col_names = FALSE,sheet=hojas[j])  
  }  
}#Exportacion de datos en lista

#save(datos,file="Datos Cinemáticos/datos.RData")
#Carga de datos.RData guardados
load("Datos Cinemáticos/datos.RData")


#Procesamiento--------
data<-datos$MANO_MOV_DER$P1
ciclos<-cycle_process(data)
cycles_boxplot(ciclos)
ciclos_f<-replace_outliers(ciclos)
cycles_boxplot(ciclos_f)
cycles_plot(ciclos)
cycles_plot(ciclos_f)

C<-cbind("% ciclo"=c(0:100),data.frame(ciclos_f))#En data frame

#####Ciclos finales en ggplot####
all_ggplot(C)

prom<-as.data.frame(apply(C[,2:length(C)],1,mean))#aceleracion promedio
desv<-as.data.frame(apply(C[,2:length(C)],1,sd))#desviacion estandar

result<-cbind(C$`% ciclo`,prom,desv)
names(result)<-c("p_ciclo","promedio","desviación_estandar")
plot.data<-cbind(result,lower=result$promedio-result$desviación_estandar,
                 upper=result$promedio+result$desviación_estandar)

mean_ds_ggplot(plot.data)


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

