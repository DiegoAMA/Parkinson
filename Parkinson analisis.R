setwd("C:/Users/wazud/Desktop/Maestria/Repositorio Tesis")
library(readxl)
library(dummies)

#importanci�n y preparaci�n de datos
park <- read_excel("Base de Datos Parkinson UTMON.xlsx")
attach(park)
park<-park[c(6:143),c(1,2,9,10,12,13,14)]
View(park)
names(park)

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
View(sintomas)

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







#Calculo de número de clusters

library(readxl)

MUESTRA<- read_excel("C:/Users/wazud/Desktop/MUESTRA COMPLETA.xlsx", 
                     sheet = "Cluster")
attach(MUESTRA)

cluster<-c(UPDRS,AEV,TREM,OFF,RIGI)
dim(cluster)<-c(138,5)

n<-nrow(MUESTRA)
wss<-rep(0,10)
wss[1]<-(n-1)*sum(apply(cluster,2,var))
for(i in 2:10)wss[i]<-sum(kmeans(cluster,centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "N�meros de grupos", ylab = "Suma de los cuadrados dentro de los grupos",lwd=1.5,col="blue", cex.lab=1.6)
abline(h=c(5000,10000,15000,20000,25000,30000,35000),v=c(1,2,3,4,5,6,7,8,9,10),col="pink")

#CLUSTER K MEANS 
set.seed(80)
Acluster<-(cluster)
R<-kmeans(Acluster, 4, iter.max = 10000, nstart = 10)
R$centers


plot(Acluster[,2],Acluster[,1],col=R$cluster,pch=19,ylab="UPDRS III",xlab="Sexo")
points(centers,cex=3,col=2,pch=4)
plot(Acluster[,3],Acluster[,1],col=R$cluster,pch=19,ylab="UPDRS III",xlab="Síntoma Tremorigeno")
plot(Acluster[,4],Acluster[,1],col=R$cluster,pch=19,ylab="UPDRS III",xlab="Bloqueo")
plot(Acluster[,4],Acluster[,1],col=R$cluster,pch=19,ylab="UPDRS III",xlab="Rigidez")

b<-R$cluster
table(b)

names(MUESTRA)

#Exportar
KMF<-c(UPDRS,SEX,ED,AEV,OFF,RIGI,TREM,ENF,MEDIC,MED_EX,b)
dim(KMF)<-c(138,11)
colnames(KMF) <- c(names(MUESTRA),"Grupo")
library(xlsx)
write.xlsx(KMF, "Resultados Conglomerado/KMeansF.xlsx", sheet = "KMF")

#GRUPOS
G1<-subset(KMF, KMF[,11]==1)
G2<-subset(KMF, KMF[,11]==2)
G3<-subset(KMF, KMF[,11]==3)
G4<-subset(KMF, KMF[,11]==4)

GG1<-rep(c("G1"),times=table(b)[1])
GG2<-rep(c("G2"),times=table(b)[2])
GG3<-rep(c("G3"),times=table(b)[3])
GG4<-rep(c("G4"),times=table(b)[4])

#variables para analisis
GRUPOS<-c(GG1,GG2,GG3,GG4)
aUPDRS<-c(G1[,1],G2[,1],G3[,1],G4[,1])
aSEX<-c(G1[,2],G2[,2],G3[,2],G4[,2])
aED<-c(G1[,3],G2[,3],G3[,3],G4[,3])
aAEV<-c(G1[,4],G2[,4],G3[,4],G4[,4])
aOFF<-c(G1[,5],G2[,5],G3[,5],G4[,5])
aRIGI<-c(G1[,6],G2[,6],G3[,6],G4[,6])
aTREM<-c(G1[,7],G2[,7],G3[,7],G4[,7])
aENF<-c(G1[,8],G2[,8],G3[,8],G4[,8])
aMEDIC<-c(G1[,9],G2[,9],G3[,9],G4[,9])
aMED_EX<-c(G1[,10],G2[,10],G3[,10],G4[,10])


#PARA LA VARIABLE UPDRS
boxplot(G1[,1],G2[,1],G3[,1],G4[,1])
anova<-aov(aUPDRS~GRUPOS)
summary(anova)
RE<-residuals(anova)
plot(RE)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)

library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

#para la variable sexo
boxplot(R$cluster~SEX)
boxplot(G1[,2],G2[,2],G3[,2],G4[,2])
anova<-aov(aSEX~GRUPOS)
summary(anova)
RE<-residuals(anova)
plot(RE)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)

dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,2],G2[,2],exact = FALSE)
wilcox.test(G1[,2],G3[,2],exact = FALSE)
wilcox.test(G1[,2],G4[,2],exact = FALSE)
wilcox.test(G2[,2],G3[,2],exact = FALSE)
wilcox.test(G2[,2],G4[,2],exact = FALSE)
wilcox.test(G3[,2],G4[,2],exact = FALSE)

#para la variable edad
boxplot(G1[,3],G2[,3],G3[,3],G4[,3])
anova<-aov(aED~GRUPOS)
summary(anova)
RE<-residuals(anova)
plot(RE)

library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)

library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

#para la variable Años de evolución
boxplot(G1[,4],G2[,4],G3[,4],G4[,4])
anova<-aov(aAEV~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)

library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)


wilcox.test(G1[,4],G2[,4],exact = FALSE)
wilcox.test(G1[,4],G3[,4],exact = FALSE)
wilcox.test(G1[,4],G4[,4],exact = FALSE)
wilcox.test(G2[,4],G3[,4],exact = FALSE)
wilcox.test(G2[,4],G4[,4],exact = FALSE)
wilcox.test(G3[,4],G4[,4],exact = FALSE)

#PARA LA VARIABLE Congelamiento
boxplot(R$cluster~OFF)
boxplot(G1[,5],G2[,5],G3[,5],G4[,5])
anova<-aov(aOFF~GRUPOS)
summary(anova)
RE<-residuals(anova)

shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)

library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,5],G2[,5],exact = FALSE)
wilcox.test(G1[,5],G3[,5],exact = FALSE)
wilcox.test(G1[,5],G4[,5],exact = FALSE)
wilcox.test(G2[,5],G3[,5],exact = FALSE)
wilcox.test(G2[,5],G4[,5],exact = FALSE)
wilcox.test(G3[,5],G4[,5],exact = FALSE)

#PARA LA VARIABLE Rigidez
boxplot(R$cluster~RIGI)
boxplot(G1[,6],G2[,6],G3[,6],G4[,6])
anova<-aov(aRIGI~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,6],G2[,6],exact = FALSE)
wilcox.test(G1[,6],G3[,6],exact = FALSE)
wilcox.test(G1[,6],G4[,6],exact = FALSE)
wilcox.test(G2[,6],G3[,6],exact = FALSE)
wilcox.test(G2[,6],G4[,6],exact = FALSE)
wilcox.test(G3[,6],G4[,6],exact = FALSE)

#PARA LA VARIABLE Problemas de movimiento
boxplot(R$cluster~TREM)
boxplot(TREM~R$cluster)
anova<-aov(aTREM~GRUPOS)
summary(anova)
RE<-residuals(anova)

shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,7],G2[,7],exact = FALSE)
wilcox.test(G1[,7],G3[,7],exact = FALSE)
wilcox.test(G1[,7],G4[,7],exact = FALSE)
wilcox.test(G2[,7],G3[,7],exact = FALSE)
wilcox.test(G2[,7],G4[,7],exact = FALSE)
wilcox.test(G3[,7],G4[,7],exact = FALSE)


#PARA LA VARIABLE Número de Enfermedades
boxplot(G1[,8],G2[,8],G3[,8],G4[,8])
anova<-aov(aENF~GRUPOS)
summary(anova)
RE<-residuals(anova)
plot(RE)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)

library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,8],G2[,8],exact = FALSE)
wilcox.test(G1[,8],G3[,8],exact = FALSE)
wilcox.test(G1[,8],G4[,8],exact = FALSE)
wilcox.test(G2[,8],G3[,8],exact = FALSE)
wilcox.test(G2[,8],G4[,8],exact = FALSE)
wilcox.test(G3[,8],G4[,8],exact = FALSE)

#PARA LA VARIABLE Número de Medicamentos
boxplot(G1[,9],G2[,9],G3[,9],G4[,9])
anova<-aov(aMEDIC~GRUPOS)
summary(anova)
RE<-residuals(anova)
plot(RE)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)

library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,9],G2[,9],exact = FALSE)
wilcox.test(G1[,9],G3[,9],exact = FALSE)
wilcox.test(G1[,9],G4[,9],exact = FALSE)
wilcox.test(G2[,9],G3[,9],exact = FALSE)
wilcox.test(G2[,9],G4[,9],exact = FALSE)
wilcox.test(G3[,9],G4[,9],exact = FALSE)

#PARA LA VARIABLE Número de Medicamentos exlcusivos
boxplot(G1[,10],G2[,10],G3[,10],G4[,10])
anova<-aov(aMED_EX~GRUPOS)
summary(anova)
RE<-residuals(anova)
plot(RE)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(nortest)
ad.test(RE)
pearson.test(RE)

library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,10],G2[,10],exact = FALSE)
wilcox.test(G1[,10],G3[,10],exact = FALSE)
wilcox.test(G1[,10],G4[,10],exact = FALSE)
wilcox.test(G2[,10],G3[,10],exact = FALSE)
wilcox.test(G2[,10],G4[,10],exact = FALSE)
wilcox.test(G3[,10],G4[,10],exact = FALSE)


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

#para la variable Años de evolución
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

#PARA LA VARIABLE Número de Enfermedades
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

#PARA LA VARIABLE Número de Medicamentos
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

#PARA LA VARIABLE Número de Medicamentos exlcusivos
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

#CLUSTER K MEANS DIST Mahalanobis

set.seed(80)
Acluster<-(MUESTRA)

cluster<-c(UPDRS,AEV,TREM,OFF,RIGI)
dim(cluster)<-c(138,5)

mean<-colMeans(cluster)
cm<-cov(cluster)
MAHA<-mahalanobis(cluster,mean,cm)


R<-kmeans(MAHA, 4, iter.max = 10000, nstart = 10)
R$centers
b3<-R$cluster


KM<-c(UPDRS,SEX,ED,AEV,OFF,RIGI,TREM,ENF,MEDIC,MED_EX,b3)
dim(KM)<-c(138,11)
colnames(KM) <- c(names(MUESTRA),"Grupo")

library(xlsx)
write.xlsx(KM, "Resultados Conglomerado/MAHA.xlsx", sheet = "KMF")


centers<-c(R$centers[,3],R$centers[,1])
dim(centers)<-c(4,2)
plot(Acluster,col=R$cluster,pch=19)

plot(Acluster$ED,Acluster$UPDRS,col=R$cluster,pch=19)
points(centers,cex=3,col=2,pch=4)
plot(UPDRS~SEX,col=R$cluster,pch=19)
plot(ED~SEX,col=R$cluster,pch=19)

G1<-subset(KM, KM[,11]==1)
G2<-subset(KM, KM[,11]==2)
G3<-subset(KM, KM[,11]==3)
G4<-subset(KM, KM[,11]==4)

GG1<-rep(c("G1"),times=table(b)[1])
GG2<-rep(c("G2"),times=table(b)[2])
GG3<-rep(c("G3"),times=table(b)[3])
GG4<-rep(c("G4"),times=table(b)[4])

#variables para analisis
GRUPOS<-c(GG1,GG2,GG3,GG4)
aUPDRS<-c(G1[,1],G2[,1],G3[,1],G4[,1])
aSEX<-c(G1[,2],G2[,2],G3[,2],G4[,2])
aED<-c(G1[,3],G2[,3],G3[,3],G4[,3])
aAEV<-c(G1[,4],G2[,4],G3[,4],G4[,4])
aOFF<-c(G1[,5],G2[,5],G3[,5],G4[,5])
aRIGI<-c(G1[,6],G2[,6],G3[,6],G4[,6])
aTREM<-c(G1[,7],G2[,7],G3[,7],G4[,7])
aENF<-c(G1[,8],G2[,8],G3[,8],G4[,8])
aMEDIC<-c(G1[,9],G2[,9],G3[,9],G4[,9])
aMED_EX<-c(G1[,10],G2[,10],G3[,10],G4[,10])


#PARA LA VARIABLE UPDRS
boxplot(G1[,1],G2[,1],G3[,1],G4[,1])
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
boxplot(R$cluster~SEX)
boxplot(SEX~R$cluster)
anova<-aov(aSEX~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,2],G2[,2],exact = FALSE)
wilcox.test(G1[,2],G3[,2],exact = FALSE)
wilcox.test(G1[,2],G4[,2],exact = FALSE)
wilcox.test(G2[,2],G3[,2],exact = FALSE)
wilcox.test(G2[,2],G4[,2],exact = FALSE)
wilcox.test(G3[,2],G4[,2],exact = FALSE)

#para la variable edad
boxplot(G1[,3],G2[,3],G3[,3],G4[,3])
anova<-aov(aED~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
library(nortest)
pearson.test(RE)

dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

#para la variable Años de evolución
boxplot(G1[,4],G2[,4],G3[,4],G4[,4])
anova<-aov(aAEV~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,4],G2[,4],exact = FALSE)
wilcox.test(G1[,4],G3[,4],exact = FALSE)
wilcox.test(G1[,4],G4[,4],exact = FALSE)
wilcox.test(G2[,4],G3[,4],exact = FALSE)
wilcox.test(G2[,4],G4[,4],exact = FALSE)
wilcox.test(G3[,4],G4[,4],exact = FALSE)

#PARA LA VARIABLE Congelamiento
boxplot(R$cluster~OFF)
boxplot(OFF~R$cluster)
anova<-aov(aOFF~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,5],G2[,5],exact = FALSE)
wilcox.test(G1[,5],G3[,5],exact = FALSE)
wilcox.test(G1[,5],G4[,5],exact = FALSE)
wilcox.test(G2[,5],G3[,5],exact = FALSE)
wilcox.test(G2[,5],G4[,5],exact = FALSE)
wilcox.test(G3[,5],G4[,5],exact = FALSE)

#PARA LA VARIABLE Rigidez
boxplot(R$cluster~RIGI)
boxplot(RIGI~R$cluster)
anova<-aov(aRIGI~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,6],G2[,6],exact = FALSE)
wilcox.test(G1[,6],G3[,6],exact = FALSE)
wilcox.test(G1[,6],G4[,6],exact = FALSE)
wilcox.test(G2[,6],G3[,6],exact = FALSE)
wilcox.test(G2[,6],G4[,6],exact = FALSE)
wilcox.test(G3[,6],G4[,6],exact = FALSE)

#PARA LA VARIABLE Problemas de movimiento
boxplot(R$cluster~TREM)
boxplot(TREM~R$cluster)
anova<-aov(aTREM~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,7],G2[,7],exact = FALSE)
wilcox.test(G1[,7],G3[,7],exact = FALSE)
wilcox.test(G1[,7],G4[,7],exact = FALSE)
wilcox.test(G2[,7],G3[,7],exact = FALSE)
wilcox.test(G2[,7],G4[,7],exact = FALSE)
wilcox.test(G3[,7],G4[,7],exact = FALSE)

#PARA LA VARIABLE Número de Enfermedades
boxplot(G1[,8],G2[,8],G3[,8],G4[,8])
anova<-aov(aENF~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,8],G2[,8],exact = FALSE)
wilcox.test(G1[,8],G3[,8],exact = FALSE)
wilcox.test(G1[,8],G4[,8],exact = FALSE)
wilcox.test(G2[,8],G3[,8],exact = FALSE)
wilcox.test(G2[,8],G4[,8],exact = FALSE)
wilcox.test(G3[,8],G4[,8],exact = FALSE)

#PARA LA VARIABLE Número de Medicamentos
boxplot(G1[,9],G2[,9],G3[,9],G4[,9])
anova<-aov(aMEDIC~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,9],G2[,9],exact = FALSE)
wilcox.test(G1[,9],G3[,9],exact = FALSE)
wilcox.test(G1[,9],G4[,9],exact = FALSE)
wilcox.test(G2[,9],G3[,9],exact = FALSE)
wilcox.test(G2[,9],G4[,9],exact = FALSE)
wilcox.test(G3[,9],G4[,9],exact = FALSE)

#PARA LA VARIABLE Número de Medicamentos exlcusivos
boxplot(G1[,10],G2[,10],G3[,10],G4[,10])
anova<-aov(aMED_EX~GRUPOS)
summary(anova)
RE<-residuals(anova)
shapiro.test(RE)
library(tseries)
jarque.bera.test(RE)
library(agricolae)
dif<-LSD.test(anova,"GRUPOS")
DIF<-TukeyHSD(anova)

wilcox.test(G1[,10],G2[,10],exact = FALSE)
wilcox.test(G1[,10],G3[,10],exact = FALSE)
wilcox.test(G1[,10],G4[,10],exact = FALSE)
wilcox.test(G2[,10],G3[,10],exact = FALSE)
wilcox.test(G2[,10],G4[,10],exact = FALSE)
wilcox.test(G3[,10],G4[,10],exact = FALSE)
