#Comparación persona enferma y sana
titulo<-"Mano Izquierda Postural"

url.1<-"P7/P7 postural manos.xlsx"
extremidad.1<-"Derecha.2"

url.2<-"P46/p46 manos postural.xlsx"
extremidad.2<-"Derecha.2"

comp<-function(url.1,url.2,extr.1,extr.2,titulo){
  datos.1<- read_excel(url.1,sheet = extremidad.1, col_names = FALSE)
  datos.1<-datos.1[datos.1[,1]>5&datos.1[,1]<=15,]
  datos.1<-as.data.frame(datos.1)
  m.1<-mean(datos.1[,2])
  
  datos.2<- read_excel(url.2,sheet = extremidad.2, col_names = FALSE)
  datos.2<-datos.2[datos.2[,1]>5&datos.2[,1]<=15,]
  datos.2<-as.data.frame(datos.2)
  m.2<-mean(datos.2[,2])
  
  compar<-ggplot()+
    geom_line(data=datos.1,aes(x=datos.1[,1], y=datos.1[,2],
                               color="E. Parkinson"))+
    geom_hline(aes(yintercept = m.1,color="E. Parkinson"),size=1.4)+
    geom_line(data=datos.2,aes(x=datos.2[,1], y=datos.2[,2],
                               color="Persona Sana"))+
    geom_hline(aes(yintercept = m.2,color="Persona Sana"),size=1.4)+
    xlab("tiempo")+
    ylab("aceleración [g]")+
    ggtitle(titulo)
  print(paste("Promedio acel. E.P.",m.1))
  print(paste("Promedio acel. Sano",m.2))
  return(compar)
}

comp(url.1,url.2,extremidad.1,extremidad.2,titulo)

