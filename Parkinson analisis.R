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

par(mar=c(1,1,1,1))

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
    dif<-wilcox.test(park.2[park.2$cluster.2==1,1],
                     park.2[park.2$cluster.2==2,1],
                     paired = F)
    paste("wilcox coef:  ",dif$p.value)
  }
}#Función para análisis de variables

#SEXO
dif(1)#No existe diferencia

#EDAD
dif(2)#NO existe diferencia

#Núm de enfermedades
dif(3)#No existe diferencia

#Años de evolución 
dif(4)#No hay diferencia por los outliers del grupo 2

#UPDRS III
dif(5)# Existe Diferencia

#Congelamiento
dif(6)#No existe diferencia por los outilers 

#Discinesia
dif(7)#No existe diferencia

#RIGIDOACINETICA
dif(8)#No existe diferencia

#TREMORIGENA
dif(9)#No existe diferencia

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
resultado<-round(resultado,0)

###CLUSTER KPROTOTYPE
library(readxl)
MUESTRAkproto<-MUESTRA<- read_excel("C:/Users/Diego Arturo/Desktop/MUESTRA COMPLETA2.xlsx", sheet = "K Proto F")
attach(MUESTRAkproto)

Aclusterkproto<-(MUESTRAkproto)

a <- lambdaest(data.frame(c(MUESTRAkproto)))

library(clustMixType)
set.seed(80)
RR<-kproto(data.frame(c(MUESTRAkproto)), 4, iter.max = 10000, keep.data = TRUE)
names(RR)

RR$centers

centers<-c(RR$centers[,3],RR$centers[,1])
dim(centers)<-c(4,2)

#UPDRS,AEV,TREM,OFF,RIGI
plot(AEV,UPDRS,col=RR$cluster,pch=19)
points(centers,cex=3,col=2,pch=4)
plot(UPDRS~SEX,col=RR$cluster,pch=19)
plot(ED~SEX,col=RR$cluster,pch=19)

b2<-RR$cluster
table(b2)

#Exportar
KPa<-c(UPDRS,SEX,ED,AEV,OFF,RIGI,TREM,ENF,MEDIC,MED_EX,b2)
dim(KPa)<-c(138,11)
colnames(KPa) <- c(names(MUESTRA),"Grupo")

library(xlsx)
write.xlsx(KPa, "Resultados Conglomerado/KPa-2.xlsx", sheet = "KPaF")

MUESTRA<- read_excel("C:/Users/Diego Arturo/Desktop/MUESTRA COMPLETA2.xlsx", sheet = "Cluster")
attach(MUESTRA)
KPb<-c(UPDRS,SEX,ED,AEV,OFF,RIGI,TREM,ENF,MEDIC,MED_EX,b2)
dim(KPb)<-c(138,11)
colnames(KPb) <- c(names(MUESTRA),"Grupo")
write.xlsx(KPb, "Resultados Conglomerado/KPbF.xlsx", sheet = "KPbF")

#GRUPOS
G1<-subset(KPa, KPa[,11]==1)
g1<-subset(KPb, KPb[,11]==1)

G2<-subset(KPa, KPa[,11]==2)
g2<-subset(KPb, KPb[,11]==2)

G3<-subset(KPa, KPa[,11]==3)
g3<-subset(KPb, KPb[,11]==3)

G4<-subset(KPa, KPa[,11]==4)
g4<-subset(KPb, KPb[,11]==4)


GG1<-rep(c("G1"),times=table(b2)[1])
GG2<-rep(c("G2"),times=table(b2)[2])
GG3<-rep(c("G3"),times=table(b2)[3])
GG4<-rep(c("G4"),times=table(b2)[4])

#variables para analisis
GRUPOS<-c(GG1,GG2,GG3,GG4)
aUPDRS<-c(g1[,1],g2[,1],g3[,1],g4[,1])
aSEX<-c(G1[,2],G2[,2],G3[,2],G4[,2])
aSEX2<-c(g1[,2],g2[,2],g3[,2],g4[,2])
aED<-c(g1[,3],g2[,3],g3[,3],g4[,3])
aAEV<-c(g1[,4],g2[,4],g3[,4],g4[,4])
aOFF<-c(G1[,5],G2[,5],G3[,5],G4[,5])
aOFF2<-c(g1[,5],g2[,5],g3[,5],g4[,5])
aRIGI<-c(G1[,6],G2[,6],G3[,6],G4[,6])
aRIGI2<-c(g1[,6],g2[,6],g3[,6],g4[,6])
aTREM<-c(G1[,7],G2[,7],G3[,7],G4[,7])
aTREM2<-c(g1[,7],g2[,7],g3[,7],g4[,7])
aENF<-c(g1[,8],g2[,8],g3[,8],g4[,8])
aMEDIC<-c(g1[,9],g2[,9],g3[,9],g4[,9])
aMED_EX<-c(g1[,10],g2[,10],g3[,10],g4[,10])

#PARA LA VARIABLE UPDRS
boxplot(g1[,1],g2[,1],g3[,1],g4[,1])
anova<-aov(aUPDRS~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

#para la variable sexo
boxplot(RR$cluster~SEX)
boxplot(SEX~RR$cluster)
anova<-aov(aSEX2~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(g1[,2],g2[,2],exact = FALSE)
wilcox.test(g1[,2],g3[,2],exact = FALSE)
wilcox.test(g1[,2],g4[,2],exact = FALSE)
wilcox.test(g2[,2],g3[,2],exact = FALSE)
wilcox.test(g2[,2],g4[,2],exact = FALSE)
wilcox.test(g3[,2],g4[,2],exact = FALSE)

#para la variable edad
boxplot(g1[,3],g2[,3],g3[,3],g4[,3])
anova<-aov(aED~GRUPOS)
summary(anova)
RE<-residuals(anova)
library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)

library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

#para la variable AÃ±os de evoluciÃ³n
boxplot(g1[,4],g2[,4],g3[,4],g4[,4])
anova<-aov(aAEV~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(g1[,4],g2[,4],exact = FALSE)
wilcox.test(g1[,4],g3[,4],exact = FALSE)
wilcox.test(g1[,4],g4[,4],exact = FALSE)
wilcox.test(g2[,4],g3[,4],exact = FALSE)
wilcox.test(g2[,4],g4[,4],exact = FALSE)
wilcox.test(g3[,4],g4[,4],exact = FALSE)

#PARA LA VARIABLE Congelamiento
boxplot(RR$cluster~OFF)
boxplot(OFF~RR$cluster)
anova<-aov(aOFF2~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(g1[,5],g2[,5],exact = FALSE)
wilcox.test(g1[,5],g3[,5],exact = FALSE)
wilcox.test(g1[,5],g4[,5],exact = FALSE)
wilcox.test(g2[,5],g3[,5],exact = FALSE)
wilcox.test(g2[,5],g4[,5],exact = FALSE)
wilcox.test(g3[,5],g4[,5],exact = FALSE)

#PARA LA VARIABLE Rigidez
boxplot(RR$cluster~RIGI)
boxplot(RIGI~RR$cluster)
anova<-aov(aRIGI2~GRUPOS)
summary(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(g1[,6],g2[,6],exact = FALSE)
wilcox.test(g1[,6],g3[,6],exact = FALSE)
wilcox.test(g1[,6],g4[,6],exact = FALSE)
wilcox.test(g2[,6],g3[,6],exact = FALSE)
wilcox.test(g2[,6],g4[,6],exact = FALSE)
wilcox.test(g3[,6],g4[,6],exact = FALSE)

#PARA LA VARIABLE Problemas de movimiento
boxplot(RR$cluster~TREM)
boxplot(TREM~RR$cluster)
anova<-aov(aTREM2~GRUPOS)
summary(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(g1[,7],g2[,7],exact = FALSE)
wilcox.test(g1[,7],g3[,7],exact = FALSE)
wilcox.test(g1[,7],g4[,7],exact = FALSE)
wilcox.test(g2[,7],g3[,7],exact = FALSE)
wilcox.test(g2[,7],g4[,7],exact = FALSE)
wilcox.test(g3[,7],g4[,7],exact = FALSE)

#PARA LA VARIABLE NÃºmero de Enfermedades
boxplot(g1[,8],g2[,8],g3[,8],g4[,8])
anova<-aov(aENF~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(g1[,8],g2[,8],exact = FALSE)
wilcox.test(g1[,8],g3[,8],exact = FALSE)
wilcox.test(g1[,8],g4[,8],exact = FALSE)
wilcox.test(g2[,8],g3[,8],exact = FALSE)
wilcox.test(g2[,8],g4[,8],exact = FALSE)
wilcox.test(g3[,8],g4[,8],exact = FALSE)

#PARA LA VARIABLE NÃºmero de Medicamentos
boxplot(g1[,9],g2[,9],g3[,9],g4[,9])
anova<-aov(aMEDIC~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(g1[,9],g2[,9],exact = FALSE)
wilcox.test(g1[,9],g3[,9],exact = FALSE)
wilcox.test(g1[,9],g4[,9],exact = FALSE)
wilcox.test(g2[,9],g3[,9],exact = FALSE)
wilcox.test(g2[,9],g4[,9],exact = FALSE)
wilcox.test(g3[,9],g4[,9],exact = FALSE)

#PARA LA VARIABLE NÃºmero de Medicamentos exlcusivos
boxplot(g1[,10],g2[,10],g3[,10],g4[,10])
anova<-aov(aMED_EX~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(g1[,10],g2[,10],exact = FALSE)
wilcox.test(g1[,10],g3[,10],exact = FALSE)
wilcox.test(g1[,10],g4[,10],exact = FALSE)
wilcox.test(g2[,10],g3[,10],exact = FALSE)
wilcox.test(g2[,10],g4[,10],exact = FALSE)
wilcox.test(g3[,10],g4[,10],exact = FALSE)

