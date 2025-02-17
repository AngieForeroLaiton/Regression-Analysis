library(ppcor)
library(readxl)
library(dplyr)
library(car) 
library(lmtest)
library(glmtoolbox)
library(tseries)
library(MASS)

#____________________________________________________________________________________________________
###4.Fase de Identificación:

#Base de datos original 

db <- read_excel("C:/Users/danie/Downloads/BASE DE DATOS PROYECTO (3).xlsx")
View(db)

#Quitamos las variables cualitativas:
db1<-select(db,-NIVEL_EDUCA,-SEXO,-F_FREQ,-TRAG_HAB,-ESTADO_SALUD,-B_FREQ,-INDIVIDUO)
View(db1)

cor(db1) #Coeficiente lineal de Pearson para las cuantitativas
pcor(db1) #Coeficientes parciales de las cuantitativas

#Volvemos a colocar las cualitativas ordinales:
db2<-select(db,-SEXO,-INDIVIDUO)
View(db2)

cor(db2,method="kendall") #Coeficiente de kendall para las cuantitativas y cualitativas ordinales
cor(db2,method="spearman")
pcor(db2,method="kendall")#Coeficiente parcial de kendall para las cuantitativas y cualitativas ordinales
pcor(db2,method="spearman")

#Ahora si, volvemos las variables categoricas para el estudio
NIV_ED<-as.factor(db$NIVEL_EDUCA)
SEXO<-as.factor(db$SEXO)
f_freq<-as.factor(db$F_FREQ)
beb_hab<-as.factor(db$TRAG_HAB)
est_sal<-as.factor(db$ESTADO_SALUD)
b_freq<-as.factor(db$B_FREQ)

db3<-cbind(db1,NIV_ED,SEXO,f_freq,beb_hab,est_sal,b_freq)
View(db3)

#Grafica con las variables explicativas escogidas
db4<-select(db3,-f_freq,-beb_hab,-est_sal,-b_freq,-B_PRIMVEZ)
View(db4)
names(pdfFonts())
par(family = "serif")
plot(db4,pch=21,col="#473C8B",family="serif",main="Matriz de Diagramas de Dispersión",lwd=1)

#Se escoge el modelo más sencillo pues las graficas no nos dan una pista de cual es la relación funcional


#____________________________________________________________________________________________________________
####### 5.Fase Estimación e Identificación:

### Este es el primer modelo que se formula:
modelo1<-lm(CIGARR_HAB~1+EDAD+F_PRIMVEZ+NIV_ED+SEXO,data=db4)
summary(modelo1)


## a)PATRONES NO EXPLICADOS ########
stud_res<-studres(modelo1)

par(family = "serif")
plot(modelo1, which=1,main="Modelo vs residuales",lwd=2,col="#473C8B")
plot(db4$SEXO,stud_res,main="Sexo vs Residuales Estudentizados",xlab="Sexo",ylab="Residuales Estudentizados",col=c("#CD7054","#B03060"),pch = 1)
plot(db4$EDAD,stud_res,main="Edad vs Residuales Estudentizados",xlab="Edad",ylab="Residuales Estudentizados",col="#473C8B")
plot(db4$F_PRIMVEZ,stud_res,main="Edad de primera vez de consumo vs Residuales Estudentizados",xlab="Edad primera vez Consumo",ylab="Residuales Estudentizados",col="#473C8B")
plot(db4$NIV_ED,stud_res,main="Nivel Educativo vs Residuales Estudentizados",xlab="Nivel Educativo",ylab="Residuales Estudentizados",col=c("#CD7054","#8B2500","#B03060"))

plot(fitted(modelo1),stud_res,xlab="Valores ajustados del modelo",ylab="Residuales Estudentizados",main="Modelo vs Residuales Estudentizados",lwd=1,col="#473C8B") #Parecen no haber patrones explicados y por tanto 

### b)MULTICOLINEALIDAD #######


vif(modelo1) #gvif (COMO SACARLO?)
eigen<-eigen(t(model.matrix(modelo1))%*%model.matrix(modelo1))

eigen$values[1]/eigen$values #El número de condición es el cociente entre el
#Autovalor mas grande y el mas pequeño.
det(t(model.matrix(modelo1))%*%model.matrix(modelo1)) #El determinante es distinto de 0.

#No hay multicolinealidad

### c) Homoscedasticidad

mean(stud_res)
plot(fitted(modelo1),stud_res)

plot(modelo1, which=3,col="#473C8B", main="Valores Ajustados vs Residuales estandarizados",lwd=2)
plot(modelo1$residuals^2,col="#473C8B", xlab="Observación",ylab="Residuales",main="Residuales al cuadrado",lwd=1)
plot(stud_res^2,col="#473C8B", main="Valores Ajustados vs Residuales al cuadrado",lwd=1)
plot(db4$SEXO,stud_res)
plot(db4$EDAD,stud_res)
plot(db4$F_PRIMVEZ,stud_res)
plot(db4$NIV_ED,stud_res)
plot(modelo1$fitted.values,stud_res)
bptest(modelo1) #No se rechaza la homocedasticidad.

fit_var1<-lm((stud_res^2)~1+EDAD+F_PRIMVEZ+NIV_ED+SEXO,data=db4)
summary(fit_var1) #mirando la significancia de los coeficiente no tienen.

fit_var0<-lm((modelo1$residuals^2)~1+EDAD+F_PRIMVEZ+NIV_ED+SEXO,data=db4)
summary(fit_var0)

###d)#Correlacion en el tiempo (si las observaciones fueron medidas en el orden en el que son reportadas)
#plot(stud_res,type="l")
#abline(h=0,col="red")
#acf(stud_res)
#dwtest(modelo1)

#para verificar si hay patrones en los residuales respecto a alguna variable
#plot(sign(stud_res)[order(db4$SEXO)],type="l")
#plot(sign(stud_res)[order(db4$NIV_ED)],type="l")
#plot(sign(stud_res)[order(db4$F_PRIMVEZ)],type="l")
#plot(sign(stud_res)[order(db4$EDAD)],type="l")


#ESTO PARA EL TRABAJO:
#Prueba de rachas de independencia
x<-factor(sign(stud_res))
runs.test(x) #en caso de que las observaciones tengan un orden natural que pueda implicar correlaci?n
runs.test(x[order(db4$SEXO)])
runs.test(x[order(db4$NIV_ED)])
runs.test(x[order(db4$F_PRIMVEZ)])
runs.test(x[order(db4$EDAD)])

### e) Normalidad de los datos:
#envelope simulado
par(family = "serif")
envelope(modelo1,col="#B03060",ylab="Cuantiles Observados",xlab="Cuantiles Esperados",main="Normal QQ-plot con banda simulada de residuos estudentizados",pch=1)
help(envelope)

#Si no hubieran tantos valores de apalancamiento se podria hacer esto:
plot(modelo1,which=2)
hist(stud_res)
qqPlot(stud_res,pch=20)
shapiro.test(stud_res)
jarque.bera.test(stud_res)

#____________________________________________________________________________________________________________
###6. FASE DE VALIDACIÓN. Nuestro modelo únicamente no cumple con la normalidad luego se debe realizar un bootstrap:


library(boot)
#Aplicamos bootstraping sobre los errores:

set.seed(123)
modelodef<-Boot(modelo1, f = coef,  R = 20000, 
     method = "residual")
summary(modelodef)
sigmaboot<-Boot(modelo1, f = sigma,  R = 20000, 
                method = "residual")
summary(sigmaboot)

help(Boot)

#Volvemos a mirar los supuestos:

#(Para ello se sacaran los residuales estudentizados de manera manual:)

betahat<-summary(modelodef)$bootMed #Estos son los nuevos coeficientes del modelo.
sigmahatnew<-summary(sigmaboot)$bootMed
n<-dim(db4)[1]
X<-model.matrix(modelo1)
Yhat<-X%*%betahat
Res<-db4$CIGARR_HAB-Yhat #Vector de residuales
Res<-as.vector(Res)
a<-length(Res)

H<-X%*%solve(t(X)%*%X)%*%t(X)
Apalancamiento<-diag(H)

Restudent<-numeric(a)

for(i in 1:length(Res)){
  Restudent[i]<-(Res[i]/(sigmahatnew*(1-Apalancamiento[i]))) #Vector de estudentizados
}
Restand<-numeric(a)
for(i in 1:length(Res)){
  Restand[i]<-(Res[i]/sigmahatnew) #Vector de estudentizados
}

###a) Patrones no explicados:

par(family = "serif")
plot(Yhat,Res,main="Modelo vs residuales del modelo con Bootstraping",lwd=1,col="#473C8B",xlab="Valores Ajustados",ylab="Residuales")
abline(h=0,col="red",lwd=2)
plot(db4$SEXO,Restudent,main="Sexo vs Residuales Estudentizados",xlab="Sexo",ylab="Residuales Estudentizados",col=c("#CD7054","#B03060"),pch = 1,lwd=1.5)
plot(db4$EDAD,Restudent,main="Edad vs Residuales Estudentizados",xlab="Edad",ylab="Residuales Estudentizados",col="#473C8B",lwd=1.7)
plot(db4$F_PRIMVEZ,Restudent,main="Edad de primera vez de consumo vs Residuales Estudentizados",xlab="Edad primera vez Consumo",ylab="Residuales Estudentizados",col="#473C8B")
plot(db4$NIV_ED,Restudent,main="Nivel Educativo vs Residuales Estudentizados",xlab="Nivel Educativo",ylab="Residuales Estudentizados",col=c("#CD7054","#8B2500","#B03060"),lwd=1.5)
mean(Res)

###b) El bootstrapoing no afecta de ningun modo a X entonces multicolinealidad sigue sin presentarse.

### c) Homoscedasticidad

mean(Restudent)

plot(Yhat,sqrt(abs(Restand)),col="#473C8B", main="Valores Ajustados vs Residuales estandarizados del Bootstraping",lwd=1,xlab="Valores Ajustados")
plot(Res^2,col="#473C8B", xlab="Observación",ylab="Residuales",main="Residuales al cuadrado del Bootstraping",lwd=1)
plot(db4$SEXO,Restudent)
plot(db4$EDAD,Restudent)
plot(db4$F_PRIMVEZ,Restudent)
plot(db4$NIV_ED,Restudent)
plot(Yhat,Restudent)

fit_var3<-lm((Restudent^2)~1+EDAD+F_PRIMVEZ+NIV_ED+SEXO,data=db4)
summary(fit_var3) #mirando la significancia de los coeficiente no tienen.

fit_var4<-lm((Res^2)~1+EDAD+F_PRIMVEZ+NIV_ED+SEXO,data=db4)
summary(fit_var4)

###d)#Correlacion en el tiempo (si las observaciones fueron medidas en el orden en el que son reportadas)
plot(Restudent,type="l")
abline(h=0,col="red")
acf(Restudent)

#para verificar si hay patrones en los residuales respecto a alguna variable
plot(sign(Restudent)[order(db4$SEXO)],type="l")
plot(sign(Restudent)[order(db4$NIV_ED)],type="l")
plot(sign(Restudent)[order(db4$F_PRIMVEZ)],type="l")
plot(sign(Restudent)[order(db4$EDAD)],type="l")

#Prueba de rachas de independencia
x<-factor(sign(Restudent))
runs.test(x) #en caso de que las observaciones tengan un orden natural que pueda implicar correlaci?n
runs.test(x[order(db4$SEXO)])
runs.test(x[order(db4$NIV_ED)])
runs.test(x[order(db4$F_PRIMVEZ)])
runs.test(x[order(db4$EDAD)])


#### e) Normalidad de los datos:
#envelope simulado pues hay muchas observaciones de alta palanca 


library(qqplotr)
qqPlot(Restudent,dist="norm",envelope=TRUE,lwd=2, pch=1,line="quartiles",ylab="Cuantiles Observados",xlab="Cuantiles Esperados",main="Normal QQ-plot con banda simulada de residuos estudentizados",col="#B03060")
#Podemos ver que los datos se adaptan mucho mejor a la distribución normal



#____________________________________________________________________________________________________________
####7. Observaciones influyentes:


n<-dim(db4)[1]
X<-model.matrix(modelo1)
H<-X%*%solve(t(X)%*%X)%*%t(X)

#Valores de altapalanca (son las mismas con o sin bootstraping)

plot(diag(H))
abline(h=1/n,col="red")

plot(modelo1,which=5)
mean(diag(H))
12/n
diag(H)[1]

s2<-0
s3<-0
for(i in 1:a){
  if(diag(H)[i]>2*12/n){s2<-s2+1}
  if(diag(H)[i]>3*12/n){s3<-s3+1}
}

plot(hatvalues(modelo1),type="h",main="Leverage de las observaciones",ylab="Leverage",xlab="Observación")
abline(h=2*12/n,col="red",lwd=2)
abline(h=3*12/n,col="blue",lwd=2)
sort(hatvalues(modelo1),decreasing=TRUE)

highleverage <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  ratio <-p/n
  plot(hatvalues(fit), main='Leverage',col="#473C8B",xlab="Observaciones",ylab="Leverage")
  abline(h=c(2,3)*ratio, col=c('red',"green"), lty=2,lwd=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
x11()
highleverage(modelo1)

#Valores atipicos:(para el modelo inicial)


plot(modelo1,which=5)
stud_res<-studres(modelo1)
plot(abs(studres(modelo1)),main="Valor absoluto de los residuales Estudentizados",xlab="Observación",ylab="Abs(Residuales)",col="#473C8B")
abline(h=3,col="green",lwd=2)
head(sort(abs(stud_res),decreasing=TRUE))
boxplot(stud_res,col="#B03060",main="Residuales estudentizados",ylab="Residuales")
s1<-0
for(i in 1:a){
  if(abs(stud_res[i])>3){
    s1<-s1+1
  }
}

###Valores atipicos para el modelo final con bootstraping

head(sort(abs(Restudent),decreasing=TRUE))
plot(abs(Restudent),main="Valor absoluto de los residuales Estudentizados con Bootstraping",xlab="Observación",ylab="Abs(Residuales)",col="#473C8B")
abline(h=3,col="green",lwd=2)
boxplot(Restudent,col="#B03060",main="Residuales estudentizados",ylab="Residuales")
s<-0
for(i in 1:a){
  if(abs(Restudent[i])>3){
    s<-s+1
  }
}
Restudent[1823]
#Así podemos fijarnos que en realidad los residuales con bootstraping o sin bootstraping tienden a tener los mismos valores
#pero los segundos se adaptan mejor a una distribución normal.

#S nos cuenta cuantos valores de los residuales studentizados tenemos como atipicos.

#Valores influyentes:(sin bootstraping)

corte <- 4/(n-length(modelo1$coefficients)-2) 
plot(modelo1, which=4, cook.levels=corte,main="Distancia de Cook")
abline(h=corte, lty=2, col="red",lwd=2)
cooksd<-cooks.distance(modelo1)
cooksd[which(cooksd>corte)]

influencePlot(modelo1,modelo1.col="#528B8B",col="#473C8B",id.method="identify",xlab="Valores de alta Palanca",ylab="Residuales Estudentizados",ylim=c(-2,12))
title("Grafico de Influencia",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")
s5<-0
for(i in 1:a){
  if(cooksd[i]>corte){s5<-s5+1}
}
stud_res[4]

fit3<-update(modelo1,subset={setdiff(row(db4)[,1],c(4,12,90 ,95,110,122,216,321,451,458,479,490,532,533,625,684,694,701,708,710 ,752,757,858,960,1053 ,1091,1123,1135,1153,1221,1293,1338,1361,1400,1419,1421,
                                                         1424,1477,1478 ,1483,1509,1547,1648,1682,1687,1791,1823,1831,1834,1847,1857,1944,2054,2070,2118,2130,2140,2142,2180,2185 ,2203,2228,2230,2231,2234,2239,
                                                         2241,2255,2274,2283, 2300,2303 ,2342,2348,2349,2355,2393,2418,2534,2579,2610,2612,2693,2749,2820,2828 ))})
summary(fit3)

#Valores influyentes:(con bootstraping) 

corte <- 4/(n-length(betahat)-2) 
#se necesita sacar la distancia de cook manualmente:
Distancia<-numeric(a)

for(i in 1:a){
  Distancia[i]<-(Restudent[i]^2*(Apalancamiento[i]/(1-Apalancamiento[i]))*1/12)
}

plot(1:a, Distancia, type="h")
abline(h=corte, lty=2, col="red")
cooksd<-Distancia
Distcrit<-cooksd[which(cooksd>corte)]
#Para ver que observaciones son estas:

Obsinf<-numeric(0)

for(i in 1:a){
      if(cooksd[i]>corte){Obsinf=c(Obsinf,i)}#Agregamos los individuos que cumplen la condicion
}

#ESTO NO SIRVE

#db5<-db4[-Obsinf,]
#fit4<-lm(CIGARR_HAB~1+EDAD+F_PRIMVEZ+NIV_ED+SEXO, data=db5)
#summary(fit4)

#modelodinf<-Boot(fit4, f = coef,  R = 20000, method = "residual")
#summary(modelodinf)
#sigmabootinf<-Boot(fit4, f = sigma,  R = 20000, method = "residual")
#summary(sigmaboot)

#_ - _ __ _ _ _ _ _ _  _ _ _ _  _ _ _ _ _ _  _ _ _ _ _ _ _ _ _ _ _ _  _ _ _ _ _ _ _  _ _ _ _ _ _ _ _ _  

####Regresión LAD (BONUS)
library(L1pack)

Modelolad<-lad(CIGARR_HAB~1+EDAD+F_PRIMVEZ+NIV_ED+SEXO,data=db4)
summary(Modelolad)
residuales2<-Modelolad$residuals
plot(Modelolad, which=1)
plot(db4$SEXO,residuales2)
plot(db4$EDAD,residuales2)
plot(db4$F_PRIMVEZ,residuales2)
plot(db4$NIV_ED,residuales2)

#____________________________________________________________________________________________________________________________
####8
#Es significativo
summary(modelo1)
modelo3<-lm(CIGARR_HAB~1,data=db4)
anova(modelo3,modelo1,test="F")

#Todas las variables son explicativas?
summary(modelo1)

#Son necesarias las interacciones?

resettest(modelo1,power=2,type="fitted")
resettest(modelo1,power=2,type="regressor")
help(resetest)

modelo2<-lm(CIGARR_HAB~1+EDAD+exp(-F_PRIMVEZ)+SEXO+NIV_ED,data=db4)
summary(modelo2)

#Conclusiones

plot(db4$F_PRIMVEZ,db3$B_PRIMVEZ)
boxplot(db3$f_freq,db3$b_freq)
