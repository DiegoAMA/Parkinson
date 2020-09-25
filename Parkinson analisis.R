setwd("C:/Users/wazud/Desktop/Maestria/Repositorio Tesis")
library(readxl)
library(dummies)
library(GGally)
library(ggplot2)
library(stats)
library(vcd)
library(lattice)
library(NbClust)
library(factoextra)
library(cluster)
library(nortest)
library(agricolae)
library(clustMixType)
library(fpc)

#_________________________________________________
####importanción y preparación de datos####
park <- read_excel("Base de Datos Parkinson UTMON.xlsx")
park<-park[c(6:143),c(1,2,4,9,10,12,13,14)]
names(park)

#Hay un f minuscula que lo pone como NA, por ello se corrige antes de
table(park$SEXO)
park[park$SEXO=="f",1]="F"
park$SEXO<-factor(park$SEXO,levels=c("F","M"),labels=c("M","H"))

park$OFF<-factor(park$`PRESENCIA DE OFF PROBLEMATICOS`,
                 levels=c("1 SI","0 NO"),
                 labels=c("SI","NO"))
park$`PRESENCIA DE OFF PROBLEMATICOS`<-NULL
park$DISCINESIA<-factor(park$`PRESENCIA DE DISKINESIAS PROBLEMATICAS`,
                                                      levels=c("1 SI","0 NO"),
                                                      labels=c("SI","NO"))
park$`PRESENCIA DE DISKINESIAS PROBLEMATICAS`<-NULL

sintomas<-as.data.frame(dummy(park$PRESENTACION))

i<-1
for (i in 1:length(sintomas[,1])) {
  ifelse(sintomas[i,3]==1,sintomas[i,c(1,2)]<-1,sintomas[i,c(1,2)])  
}
park$PRESENTACION<-NULL
park<-cbind(park,sintomas[,c(1,2)])
park$RIGIDOACINETICA<-factor(park$`PRESENTACION)1 RIGIDOACINETICA`,
                             levels=c("1","0"),labels=c("SI","NO"))
park$`PRESENTACION)1 RIGIDOACINETICA`<-NULL
park$TREMORIGENA<-factor(park$`PRESENTACION)2 TREMORIGENA`,
                         levels=c("1","0"),labels=c("SI","NO"))
park$`PRESENTACION)2 TREMORIGENA`<-NULL

colnames(park)<-c("SEXO","EDAD","ENFERMEDADES","AÑOS_EVOL","UPDRS","OFF","DISCINESIA",
                  "RIGIDOACINETICA","TREMORIGENA")
rm(sintomas)
rm(i)

#________________________________________________________________
####Análisis Estádistico ####

summary(park)
ggpairs(park)+theme(plot.background = element_rect(fill="white"),
                    panel.background = element_rect(fill = "lightblue",
                                                    colour = "lightblue",
                                                    size = 0.5, linetype = "solid"),)

park_num<-data.frame(park$EDAD,park$ENFERMEDADES,park$AÑOS_EVOL,park$UPDRS)
park_dico<-data.frame(park$SEXO,park$OFF,park$DISCINESIA,park$RIGIDOACINETICA,
                      park$TREMORIGENA)

cor(park_num)
#Existe correlación de 0.4 entre la edad y el núm de enfermedades
#bajo tratamiento, también de 0.48 en el UPDRS con los años de la enfermedad

ggplot(park[park$AÑOS_EVOL<25,], aes(x=EDAD,y=UPDRS,
                                     size=AÑOS_EVOL,
                                     col=ENFERMEDADES)) + 
  geom_point(alpha=0.5) +
  xlab("Edad")+
  ylab("UPDRS III")+
  ggtitle("Variables Cuantitativas con Mayor Correlación")+
  scale_size_continuous(range = c(5,20))


#Test chi cuadrada
chisq.test(park_dico[,1],park_dico[,2])
chisq.test(park_dico[,1],park_dico[,3])
chisq.test(park_dico[,1],park_dico[,4])
chisq.test(park_dico[,1],park_dico[,5])#Sexo y temblor
t<-table(park$SEXO,park$TREMORIGENA,dnn=c("Sexo","Tremorigeno"))
round(prop.table(t,1),digits=3)
round(prop.table(t,2),digits=3)
#El 85% de los hombres padecen temblores
#En mujeres el 64% 

chisq.test(park_dico[,2],park_dico[,3])#Congelamiento y Discinecia
chisq.test(park_dico[,2],park_dico[,4])
chisq.test(park_dico[,2],park_dico[,5])
t<-table(park$OFF,park$DISCINESIA,dnn=c("Congelamiento","Discinecia"))
round(prop.table(t,1),digits=3)
round(prop.table(t,2),digits=3)
#Si presenta Discinecia, hay 80% de prob de que padezca congelamiento
#Si no, 79% de no padecer

chisq.test(park_dico[,3],park_dico[,4])
chisq.test(park_dico[,3],park_dico[,5])

chisq.test(park_dico[,4],park_dico[,5])#Rigidez y Temblor
t<-table(park$RIGIDOACINETICA,park$TREMORIGENA,
         dnn=c("Rigidez","Tremorigeno"))
round(prop.table(t,1),digits=3)
round(prop.table(t,2),digits=3)
#El 100% de los que no tienen rigidez tienen temblores

mosaic(~park.RIGIDOACINETICA+park.DISCINESIA+park.TREMORIGENA,
           data=park_dico,main="SINTOMAS",
           labeling=labeling_values,
       labeling_args = list(set_varnames = c(park.RIGIDOACINETICA="Rigidez", park.DISCINESIA="Discinesia",
                                             park.TREMORIGENA="Temblores")))
15/138#porcentaje 3 sintomas
39/138#Si temblores, No rigidez, No discinesia
36/138#No temblores, Si rigidez, No discinecia
37/138#Si temblores, Si rigidez, No discinecia
names(park)
bwplot(UPDRS~TREMORIGENA|SEXO,data=park,col="red",main="UPDRS - Temblores, vs Sexo",
       par.settings = list(box.rectangle = list(fill = c("lightgreen","lightblue"))))
#No se percibe gran diferencia entre estas variables

bwplot(UPDRS~DISCINESIA|OFF,data=park,col="red",main="UPDRS - Discinesia,- Congelamiento",
       par.settings = list(box.rectangle = list(fill = c("lightgreen","lightblue"))))
#Los que poseen congelamiento tienen un UPDRS claramente mayor

bwplot(UPDRS~RIGIDOACINETICA|TREMORIGENA,data=park,col="red",main="UPDRS - Rigidez,- Tembloes",
       par.settings = list(box.rectangle = list(fill = c("lightgreen","lightblue"))))
#Se confirma que no existe enfermos sin temblores y sin rigidez
#pero pueden no tener temblores y presentar un elevado UPDRS

#EL análisis de regresión lineal y anova queda descartado
#debido a que los residuales no poseen un comportamiento normal
#no son homocedasticos, aún quitando valores y elevando 
#al cuadrado las variables numéricas como sus interacciones


#________________________________________________________________
####Análisis Clusters####

                  ###k MEDOIDES###

#Outliers y preparación de datos para K-medoides
names(park)
boxplot(park$EDAD)
boxplot(park$ENFERMEDADES)#Quitar mayores a 5
boxplot(park$AÑOS_EVOL)#Quitar mayores a 30
boxplot(park$UPDRS)

park.2<-park[park$ENFERMEDADES<5,]
park.2<-park.2[park.2$AÑOS_EVOL<30,]

boxplot(park.2$EDAD)
boxplot(park.2$ENFERMEDADES)
boxplot(park.2$AÑOS_EVOL)
boxplot(park.2$UPDRS)

#as.numeric 1=SI,2=NO
#as.numeric 1=M, 2=H

i<-1
for (i in i:9) {
  ifelse(is.factor(park.2[,i]),
         park.2[,i]<-as.numeric(park.2[,i]),
         park.2[,i])
}

park.2.scaled<-scale(park.2)

#Calculo de núumero de clusters

fviz_nbclust(park.2.scaled,kmeans,method = "wss")#2, 5 y 7
NbClust(park.2.scaled,min.nc=2,max.nc=10,method="kmeans",
        index="all")#2, 4, 7

#K medoides
park_cluster.2<-pam(park.2.scaled,2)
park_cluster.3<-pam(park.2.scaled,3)
park_cluster.5<-pam(park.2.scaled,5)
park_cluster.7<-pam(park.2.scaled,7)

par(mfrow=c(2,2))
fviz_cluster(park_cluster.2,data=park.2.scaled)#Buena clasificación
fviz_cluster(park_cluster.3,data=park.2.scaled)#Opción
fviz_cluster(park_cluster.5,data=park.2.scaled)
fviz_cluster(park_cluster.7,data=park.2.scaled)

park.2$cluster.2<-as.data.frame(park_cluster.2$clustering)[,1]
park.2$cluster.3<-as.data.frame(park_cluster.3$clustering)[,1]

#Comparación de 2 y 3 clusters
par(mfrow=c(2,4))

boxplot(SEXO~cluster.2,data=park.2)
boxplot(SEXO~cluster.3,data=park.2)

boxplot(EDAD~cluster.2,data=park.2)
boxplot(EDAD~cluster.3,data=park.2)

boxplot(ENFERMEDADES~cluster.2,data=park.2)
boxplot(ENFERMEDADES~cluster.3,data=park.2)

boxplot(AÑOS_EVOL~cluster.2,data=park.2)
boxplot(AÑOS_EVOL~cluster.3,data=park.2)

boxplot(OFF~cluster.2,data=park.2)
boxplot(OFF~cluster.3,data=park.2)

boxplot(DISCINESIA~cluster.2,data=park.2)
boxplot(DISCINESIA~cluster.3,data=park.2)

boxplot(RIGIDOACINETICA~cluster.2,data=park.2)
boxplot(RIGIDOACINETICA~cluster.3,data=park.2)

boxplot(TREMORIGENA~cluster.2,data=park.2)
boxplot(TREMORIGENA~cluster.3,data=park.2)
#Debido al análisis se manejaran dos clusters con kmedoides
park.2$cluster.3<-NULL

par(mfrow=c(1,1))

dif<-function(num_var){
  boxplot(park.2[,num_var]~cluster.2,data=park.2)
  anova<-aov(park.2[,num_var]~cluster.2,data=park.2)
  print(summary(anova))
  RE<-residuals(anova)
  lol<-ad.test(RE)
  if(lol$p.value>=0.05){
    print("----Residuales Normales----")
    dif<-LSD.test(anova,"cluster.2")
    print("LSD.test: ")
    print(dif$groups)
    
  }else{
    print("----Residuales NO Normales----")
    dif<-wilcox.test(park.2[park.2$cluster.2==1,num_var],
                     park.2[park.2$cluster.2==2,num_var],
                     paired = F)
    paste("wilcox coef:  ",dif$p.value)
  }
}#Función para análisis de variables

#SEXO
dif(1)#No existe diferencia

#EDAD
dif(2)#NO existe diferencia a un 95%

#Núm de enfermedades
dif(3)#No existe diferencia

#Años de evolución 
dif(4)#Existe diferencia

#UPDRS III
dif(5)# Existe Diferencia

#Congelamiento
dif(6)#Existe diferencia

#Discinesia
dif(7)#Existe diferencia

#RIGIDOACINETICA
dif(8)#Existe diferencia

#TREMORIGENA
dif(9)#No existe diferencia

#El método hace diferencia en UPDRS III, Congelamiento,
#Discinesia, Años de evolución y Rigiddez

ggplot(park.2,aes(y=UPDRS,x=EDAD,size=OFF,col=cluster.2,alpha=0.5))+
  geom_point()+
  scale_size_continuous(range = c(5,15))

G1<-park.2[park.2$cluster.2==1,]
G1$cluster.2<-NULL
G2<-park.2[park.2$cluster.2==2,]
G2$cluster.2<-NULL

cluster1<-apply(G1, 2, mean)
cluster2<-apply(G2, 2, mean)

resultado<-cbind(cluster1,cluster2)
resultado<-as.data.frame(round(resultado,0))
resultado

write.csv(resultado,file="park_kmedoides.csv")


            ###CLUSTER KPROTOTYPE###

park.proto<-park[park$ENFERMEDADES<5,]
park.proto<-park.proto[park.proto$AÑOS_EVOL<30,]

clusterprot.2<-kproto(park.proto, 2, iter.max = 10000)
clusterprot.3<-kproto(park.proto, 3, iter.max = 10000)

park.2$prot.2<-as.data.frame(clusterprot.2$cluster)[,1]
park.2$prot.3<-as.data.frame(clusterprot.3$cluster)[,1]


#Comparación de 2 y 3 clusters
par(mfrow=c(2,4))

boxplot(SEXO~prot.2,data=park.2)
boxplot(SEXO~prot.3,data=park.2)

boxplot(EDAD~prot.2,data=park.2)
boxplot(EDAD~prot.3,data=park.2)

boxplot(ENFERMEDADES~prot.2,data=park.2)
boxplot(ENFERMEDADES~prot.3,data=park.2)

boxplot(AÑOS_EVOL~prot.2,data=park.2)
boxplot(AÑOS_EVOL~prot.3,data=park.2)

boxplot(OFF~prot.2,data=park.2)
boxplot(OFF~prot.3,data=park.2)

boxplot(DISCINESIA~prot.2,data=park.2)
boxplot(DISCINESIA~prot.3,data=park.2)

boxplot(RIGIDOACINETICA~prot.2,data=park.2)
boxplot(RIGIDOACINETICA~prot.3,data=park.2)

boxplot(TREMORIGENA~prot.2,data=park.2)
boxplot(TREMORIGENA~prot.3,data=park.2)

#En esta ocación se menajaran los 3 clusters con kproto
park.2$prot.2<-NULL

par(mfrow=c(1,1))

dif<-function(num_var){
  boxplot(park.2[,num_var]~prot.3,data=park.2)
  anova<-aov(park.2[,num_var]~prot.3,data=park.2)
  print(summary(anova))
  RE<-residuals(anova)
  lol<-ad.test(RE)
  if(lol$p.value>=0.05){
    print("----Residuales Normales----")
    dif<-LSD.test(anova,"prot.3")
    print("LSD.test: ")
    print(dif$groups)
    
  }else{
    print("----Residuales NO Normales----")
    dif<-wilcox.test(park.2[park.2$prot.3==1,num_var],
                     park.2[park.2$prot.3==2,num_var],
                     paired = F)
    print(paste("wilcox coef 1-2:  ",dif$p.value))
    
    dif<-wilcox.test(park.2[park.2$prot.3==1,num_var],
                     park.2[park.2$prot.3==3,num_var],
                     paired = F)
    print(paste("wilcox coef 1-3:  ",dif$p.value))
    
    dif<-wilcox.test(park.2[park.2$prot.3==2,num_var],
                     park.2[park.2$prot.3==3,num_var],
                     paired = F)
    print(paste("wilcox coef 2-3:  ",dif$p.value))
  }
}#Función para análisis de variables

#SEXO
dif(1)#No existe diferencia

#EDAD
dif(2)#Existe diferencia en 1-2, 2-3

#Núm de enfermedades
dif(3)#Existe diferencia en 1-2, 2-3

#Años de evolución 
dif(4)#Existe diferencia en 1-2, 1-3

#UPDRS III
dif(5)#Existe diferencia en 1-2, 1-3

#Congelamiento
dif(6)#Existe diferencia en 1-2,2-3

#Discinesia
dif(7)#Existe diferencia en 1-2, 1-3

#RIGIDOACINETICA
dif(8)# No Existe diferencia

#TREMORIGENA
dif(9)#No existe diferencia

ggplot(park.2,aes(y=UPDRS,x=EDAD,size=OFF,col=prot.3,alpha=0.5))+
  geom_point()+
  scale_size_continuous(range = c(5,15))


P1<-park.2[park.2$prot.3==1,]
P1$prot.3<-NULL
P1$cluster.2<-NULL
P2<-park.2[park.2$prot.3==2,]
P2$prot.3<-NULL
P2$cluster.2<-NULL
P3<-park.2[park.2$prot.3==3,]
P3$prot.3<-NULL
P3$cluster.2<-NULL

cluster1<-apply(P1, 2, mean)
cluster2<-apply(P2, 2, mean)
cluster3<-apply(P3, 2, mean)

resultado<-cbind(cluster1,cluster2,cluster3)
resultado<-as.data.frame(round(resultado,0))
resultado

write.csv(resultado,file="park_kproto.csv")


####Validacion Clusters####

silueta.k<-silhouette(park_cluster.2$clustering,dist(park.2.scaled))
summary(silueta.k)#Media de 0.26
fviz_silhouette(silueta.k)
#kmedoide con 3 clusters da una media de 0.18

silueta.p<-silhouette(clusterprot.3$cluster,dist(park.2.scaled))
summary(silueta.p)#Media de 0.13
fviz_silhouette(silueta.p)
#kproto con 2 cluster da una media de 0.2

stats.k<-cluster.stats(dist(park.2.scaled),park_cluster.2$clustering)
stats.k$dunn

stats.p<-cluster.stats(dist(park.2.scaled),clusterprot.3$cluster)
stats.p$dunn

compar<-cluster.stats(dist(park.2.scaled),
                      clusterprot.3$cluster,
                      park_cluster.2$clustering)

compar$corrected.rand #Indice de correción aleatoria (bajo)
compar$vic#Distancia entre los dos métodos, (alto)

#por lo que el más optimo es el de kmedoides con dos clusters

