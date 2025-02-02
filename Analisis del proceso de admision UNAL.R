library(readxl)
library(leaps)
library(dplyr)
library(lubridate)
library(memisc)
library(RConics)
library(car)
library(MASS)
library(stats)
library(tibble)


#########     MODELOS DE PREDICCIÓN PARA EL PUNTAJE DE ADMISIÓN DE LA UNIVERSIDAD NACIONAL 2015.

#____________________________________________________________________________________________________________________________

########                          MODELOS CON REGRESIÓN LINEAL MULTIPLE y MÉTODOS DE SELECCIÓN.




##### 1. Lectura de los datos (Se empieza con la lectura de los datos)

DATOS<- read_excel("C:/Users/danie/Downloads/data.xlsx")
View(DATOS)
fechas<-DATOS$ASP_FECHANACIMIENTO #Cambio de la variable fecha de nacimiento a cuantitativa.
EDAD_PRES<-floor(time_length(ymd("2015-06-01")-ymd(fechas),unit="year"))
BaseF<-dplyr::select(DATOS,-ASP_FECHANACIMIENTO) # Nueva base de datos con la variable edad de presentación del examen.
BaseF<-cbind(BaseF,EDAD_PRES)
View(BaseF)
Puntaje<-as.numeric(BaseF$PUN12_TOTAL) #Se coloca la variable Puntaje como numerica.


### Limpieza de los datos (Vemos si hay individuos sin la variable respuesta)

dim(BaseF)
sum(is.na(Puntaje)) #No hay observaciones sin puntaje.

##### 2. Se escoge un porcentaje de datos para entrenamiento y para testeo pues el fin es predictivo.(En este caso es 70/30)

set.seed(1) #Semilla 
train1<-sample(1:nrow(BaseF),size=0.7*nrow(BaseF),replace=FALSE)
train<-rep(FALSE,nrow(BaseF))
train[train1] <- TRUE
testeo <- (!train)

##### 3. Entrenamos y encontramos los mejores modelos usando solo los datos de ENTRENAMIENTO

## Para ello se realizamos una matriz que contiene los datos del testeo.

test.mat <- model.matrix(BaseF[testeo, ]$PUN12_TOTAL ~ ., data = BaseF[testeo, ])

#Se crean los siguientes vectores de 0's pues en los datos de testeo desaparecen algunas categorias de algunas variables.
test.mat<-as.data.frame(test.mat)
ASP_ANOTERMINACION1985<-rep(0,2325)
ASP_ANOTERMINACION1987<-rep(0,2325)
ASP_ANOTERMINACION1988<-rep(0,2325)
ASP_ANOTERMINACION1992<-rep(0,2325)
ASP_ANOTERMINACION1995<-rep(0,2325)
COL_DEPARTAMENTOGuainia<-rep(0,2325)
ASP_RAYA_TIPOMBMP<-rep(0,2325)
ASP_RAYA_TIPOTraslado<-rep(0,2325)


test.mat<-add_column(test.mat,ASP_ANOTERMINACION1985, .after = 1)
test.mat<-add_column(test.mat,ASP_ANOTERMINACION1987, .after = 2)
test.mat<-add_column(test.mat,ASP_ANOTERMINACION1988, .after = 3)
test.mat<-add_column(test.mat,ASP_ANOTERMINACION1992, .after = 5)
test.mat<-add_column(test.mat,ASP_ANOTERMINACION1995, .after = 8)
test.mat<-add_column(test.mat,COL_DEPARTAMENTOGuainia,.after=44)
test.mat<-add_column(test.mat,ASP_RAYA_TIPOMBMP, .after = 77)
test.mat<-add_column(test.mat,ASP_RAYA_TIPOTraslado , .after = 83)


#Matriz de testeo con todas las variables y categorias requeridas.
test.mat<-as.matrix(test.mat)

##################################################

##### Realizamos un modelo inicial con todas las variables para tener un MSE inicial con el cual comparar:

modeloinicial<-lm(PUN12_TOTAL~1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES ,data=BaseF[train,])
summary(modeloinicial)

coefini<-coef(modeloinicial)   #Coeficientes iniciales
predini <-test.mat[, names(coefini)]%*%coefini #Predicción inicial
mod.errorini<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predini)^2) #MSE inicial.
mod.errorini   # Habilidad predictiva de MSE de 8677.533
length(coefini)   #Número de variables totales del modelo.

#Como nuestro objetivo es obtener la mejor predicción no nos quedamos con este modelo como el final, solo lo realizamos con fines
#de comparación, adicionalmente en métodos de selección BestSubset tiene un peso computacional bastante elevado para la cantidad 
#de variables que se tienen entonces solo se seleccionarán variables con Forward y Backward:


##################################################

#### Selección de las variables mediante Forward sin interacciones.

regfit.best1 <- regsubsets(PUN12_TOTAL ~ .,
                           data = BaseF[train, ], method = "forward",nvmax=82)
summary(regfit.best1)

val.errors <- rep(NA, 82)
for (i in 1:82) {
  coef <- coef(regfit.best1, id = i)
  pred <- test.mat[, names(coef)] %*% coef
  val.errors[i] <- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - pred)^2)
}
val.errors   #MSE de todos los modelos con todos los tamaños posibles 
which.min(val.errors)  #El MSE es de 8544.778 (evidentemente es mejor al inicial)


## Visualicemos como se adaptan las predicciones a las observaciones en el TESTEO.

names(pdfFonts())
plot(BaseF[testeo, ]$PUN12_TOTAL,type="l",main="Observaciones vs predicción con Forward",ylab="Puntaje",xlab="Individuo",family="serif")
coeficientes<-as.vector(coef(regfit.best1, 25)) 
prediccion1<-test.mat[, names(coef(regfit.best1, 25))]%*%coeficientes
legend( x = "topleft",legend=c("Obs testeo ","prediccion"), col=c("black","#CD1076"),pch = c(19,19))
lines(prediccion1,col="#CD1076")

## Entonces los coeficientes del modelo final ya con TODOS los datos son:

bestmodelfwd <- regsubsets(PUN12_TOTAL ~ ., data = BaseF,
                           nvmax = 82,method="forward")
coef(bestmodelfwd, 25)


##################################################

#### Selección de las variables mediante Backward sin interacciones:


regfit.best2 <- regsubsets(PUN12_TOTAL ~ .,
                           data = BaseF[train, ], nvmax=82,method = "backward")
summary(regfit.best2)

val.errors2 <- rep(NA, 82)
for (i in 1:82) {
  coef2 <- coef(regfit.best2, id = i)
  pred2 <- test.mat[, names(coef2)] %*% coef2
  val.errors2[i] <- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - pred2)^2)
}
val.errors2   #MSE de los modelos con todos los tamaños
which.min(val.errors2) #MSE de 8548.334 con 21 variables.

## Visualicemos como se adaptan las predicciones a las observaciones con 21 variables.

plot(BaseF[testeo, ]$PUN12_TOTAL,type="l",main="Observaciones vs predicción con Backward",ylab="Puntaje",xlab="Individuo",family="serif")
coeficientes2<-as.vector(coef(regfit.best2, 21))
prediccion2<-test.mat[, names(coef(regfit.best2, 21))]%*%coeficientes2
legend( x = "topleft",legend=c("Obs testeo ","prediccion"), col=c("black","#FF7256"),pch = c(19,19))
lines(prediccion2,col="#FF7256")

## Entonces los coeficientes del modelo final ya con TODOS los datos son:

bestmodelbwd <- regsubsets(PUN12_TOTAL ~ ., data = BaseF,
                           nvmax = 82,method="backward")
coef(bestmodelfwd, 21)

## Hasta ahora y sin interacciones parece que el mejor modelo con habilidad predictiva mirandolo desde 
#el MSE esta dado por el modelo forward con 25 variables que arroja un MSE de 8544.778



###################################################

####Realizamos un modelo de regresión multiple pero con interacciones, en un principio se agregan todas las
#   interacciones que segun el contexto tiene sentido.


## Matriz de los datos de testeo para este modelo:

test.mat1 <- model.matrix(BaseF[testeo, ]$PUN12_TOTAL ~ 1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ESTRATO*ASP_SEXO+ASP_SEXO*ASP_RAYA_TIPO+ASP_SEXO*COL_DEPARTAMENTO+ASP_SEXO*COL_NATURALEZA+COL_DEPARTAMENTO*COL_NATURALEZA, data = BaseF[testeo, ])

## Matriz de testeo con todas las variables requeridas.
test.mat1<-as.matrix(test.mat)
dim(test.mat1)

modelointer<-lm(PUN12_TOTAL~1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ESTRATO*ASP_SEXO+ASP_SEXO*ASP_RAYA_TIPO+ASP_SEXO*COL_DEPARTAMENTO+ASP_SEXO*COL_NATURALEZA+COL_DEPARTAMENTO*COL_NATURALEZA,data=BaseF[train,])
summary(modelointer)

### Observamos que este modelo tiene problemas pues algunos coeficientes no son calculados por falta de datos pertenecientes a estas interacciones, por ejemplo
#   en la categoria SEXO*ADMISIONMBMP se tiene que solo hay un en la base y es hombre, luego no se tiene informacion sobre las mujeres en dicha interacción.



### De las anteriores las únicas que tienen los datos suficientes son las de ASP_SEXO*ASP_ESTRATO y COL_NATURALEZA*ASP_SEXO entonces realizamos ese modelo:

## Matriz del testeo para este modelo
test.mat2 <- model.matrix(BaseF[testeo, ]$PUN12_TOTAL ~ 1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ESTRATO*ASP_SEXO+ASP_SEXO*COL_NATURALEZA, data = BaseF[testeo, ])

#Se agregan las categorias de las variables que desaparecieron por no estar en el testeo:
test.mat2<-as.data.frame(test.mat2)
ASP_ANOTERMINACION1985<-rep(0,2325)
ASP_ANOTERMINACION1987<-rep(0,2325)
ASP_ANOTERMINACION1988<-rep(0,2325)
ASP_ANOTERMINACION1992<-rep(0,2325)
ASP_ANOTERMINACION1995<-rep(0,2325)
COL_DEPARTAMENTOGuainia<-rep(0,2325)
ASP_RAYA_TIPOMBMP<-rep(0,2325)
ASP_RAYA_TIPOTraslado<-rep(0,2325)


test.mat2<-add_column(test.mat2,ASP_ANOTERMINACION1985, .after = 1)
test.mat2<-add_column(test.mat2,ASP_ANOTERMINACION1987, .after = 2)
test.mat2<-add_column(test.mat2,ASP_ANOTERMINACION1988, .after = 3)
test.mat2<-add_column(test.mat2,ASP_ANOTERMINACION1992, .after = 5)
test.mat2<-add_column(test.mat2,ASP_ANOTERMINACION1995, .after = 8)
test.mat2<-add_column(test.mat2,COL_DEPARTAMENTOGuainia,.after=44)
test.mat2<-add_column(test.mat2,ASP_RAYA_TIPOMBMP, .after = 77)
test.mat2<-add_column(test.mat2,ASP_RAYA_TIPOTraslado , .after = 83)

#Matriz de testeo con todas las variables requeridas.
test.mat2<-as.matrix(test.mat2)
dim(test.mat2)

modelointerfin<-lm(PUN12_TOTAL~1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ESTRATO*ASP_SEXO+ASP_SEXO*COL_NATURALEZA,data=BaseF[train,])
summary(modelointerfin)

coefinint<-coef(modelointerfin) #Coeficientes iniciales
predinint <-test.mat2[, names(coefinint)]%*%coefinint #Predicción inicial
mod.errorint<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predinint)^2)
mod.errorint #Error MSE de 8709.146

###  La habilidad predictiva decrece conlas interacciones aplicadas, sin embargo veamos que ocurre con forward.


##  veamos que ocurre con el método de selección forward para las anteriores interacciones:


regfit.best1int <- regsubsets(PUN12_TOTAL ~ 1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ESTRATO*ASP_SEXO+ASP_SEXO*COL_NATURALEZA,
                             data = BaseF[train, ], method = "forward",nvmax=89)
summary(regfit.best1int)

val.errors3 <- rep(NA, 89)
for (i in 1:89) {
  coef <- coef(regfit.best1int, id = i)
  pred <- test.mat2[, names(coef)] %*% coef
  val.errors3[i] <- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - pred)^2)
}
val.errors3
which.min(val.errors3) #El MSE es de 8544.778 y también selecciona 25 variables.

##Los coeficientes son los siguientes:

coef(regfit.best1int, 25) 

#Así si se compara con el modelo obtenido por forward sin interacciones en efecto se obtiene el mismo modelo obtenido aqui y por tanto el mismo error de MSE
#luego estas interacciones realmente no estan teniendo peso a la hora de predecir y se pueden quitar del modelo.


######(Concluimos que con métodos de seleccion y regresión lineal multiple el mejor modelo predictivo es de forward con 25 variables y interacciones).



#____________________________________________________________________________________________________________________________

########                          MODELOS CON EL MODELO LINEAL GENERALIZADO:


#Para estos modelos se toma la familia como Gamma pues la normal nos da el lm utilizado desde un inicio en el curso.

library(glmtoolbox)

DATOS<- read_excel("C:/Users/danie/Downloads/data.xlsx")
fechas<-DATOS$ASP_FECHANACIMIENTO # Cambio de la variable fecha de nacimiento:
EDAD_PRES<-floor(time_length(ymd("2015-06-01")-ymd(fechas),unit="year"))
BaseF<-dplyr::select(DATOS,-ASP_FECHANACIMIENTO) # Nueva Base de datos con la variable edad de presentación del examen.
BaseF<-cbind(BaseF,EDAD_PRES)
View(BaseF)
BaseF$PUN12_TOTAL<-as.numeric(BaseF$PUN12_TOTAL)

set.seed(1)
train1<-sample(1:nrow(BaseF),size=0.7*nrow(BaseF),replace=FALSE)
train<-rep(FALSE,nrow(BaseF))
train[train1] <- TRUE
train[5060]<-TRUE #Se colocan estos datos obligatoriamente en el testeo pues necesitamos representación de estas categorias que tienen solo 1 dato en la base.
train[2952]<-TRUE
testeo <- (!train)

## Matriz del testeo:
test.mat3<-BaseF[testeo,]

help(glm)
modelocompletoG<-glm(PUN12_TOTAL~1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES,family=Gamma("inverse"),subset = train,data = BaseF)
summary(modelocompletoG)
glm.probs2 <- predict(modelocompletoG, newdata=test.mat3, type = "response")
val.errors4<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - glm.probs2)^2)
val.errors4    #MSE de 8530.567

plot(BaseF[testeo, ]$PUN12_TOTAL,type="l",main="Observaciones vs predicción con MLG familia Gamma",ylab="Puntaje",xlab="Individuo",family="serif")
legend( x = "topleft",legend=c("Obs testeo ","prediccion"), col=c("black","#FFD700"),pch = c(19,19))
lines(glm.probs2,col="#FFD700")

## Ahora el modelo final ya con TODOS los datos es:

modelocompletoG<-glm(PUN12_TOTAL~1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES,family=Gamma("inverse"),data = BaseF)
summary(modelocompletoG)

################################################

#### Modelo lineal generalizado (familia Gamma) con una interacción dada con la unica variable cuantitativa:


modelocompletoG2<-glm(PUN12_TOTAL~1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_SEXO+COL_DEPARTAMENTO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+EDAD_PRES*ASP_ANOTERMINACION,family=Gamma("inverse"),subset = train,data = BaseF)
summary(modelocompletoG2)
glm.probs3 <- predict(modelocompletoG2, newdata=test.mat3, type = "response")
val.errors5<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - glm.probs3)^2)
val.errors5    #MSE de 8549.803

### En general ensayando con otras o más interacciónes observamos que el MSE no disminuye respecto al modelo sin interacciones
# y por el contrario aumenta, por ejemplo con las interacciones utilizadas por el contexto anteriormente:

modelocompletoG3<-glm(PUN12_TOTAL~1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_SEXO+COL_DEPARTAMENTO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ESTRATO*ASP_SEXO+ASP_SEXO*COL_NATURALEZA,family=Gamma("inverse"),subset = train,data = BaseF)
summary(modelocompletoG3)
glm.probs4 <- predict(modelocompletoG3, newdata=test.mat3, type = "response")
val.errors6<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - glm.probs4)^2)
val.errors6    #MSE de 8566.692

###### (Concluimos hasta este momento que por ahora en los modelos lineales Generalizados con la familia gamma el mejor modelo es el que tiene todas las variables sin interacciones
#       y en general comparando con los modelos de regresión lineal multiple (familia normal) el modelo gamma esta teniendo una habilidad predictiva mejorada.)



#____________________________________________________________________________________________________________________________

########                          MODELOS CON MÉTODOS DE REGULARIZACIÓN:

library(glmnet)
library(readxl)
library(lubridate)

## Para estos modelos debemos calibrar el hiperparametro lambda.

DATOS<- read_excel("C:/Users/danie/Downloads/data.xlsx")
fechas<-DATOS$ASP_FECHANACIMIENTO # Cambio de la variable fecha de nacimiento:
EDAD_PRES<-floor(time_length(ymd("2015-06-01")-ymd(fechas),unit="year"))
BaseF<-dplyr::select(DATOS,-ASP_FECHANACIMIENTO) # Nueva Base de datos con la variable edad de presentación del examen.
BaseF<-cbind(BaseF,EDAD_PRES)
View(BaseF)
Puntaje<-as.numeric(BaseF$PUN12_TOTAL)

## Se particionan los datos

set.seed(1)
train1<-sample(1:nrow(BaseF),size=0.7*nrow(BaseF),replace=FALSE)
train<-rep(FALSE,nrow(BaseF))
train[train1] <- TRUE
testeo <- (!train)

###################################################

### LASSO sin interacciones:

## Se toma la matriz de diseño correspondiente a las variables

x <- model.matrix(PUN12_TOTAL ~ ., BaseF)[, -1]

## respuestas observadass en el testeo
Puntaje.test <- Puntaje[testeo]

## Se calibra el lambda y se escoje el mejor en MSE:

set.seed(1)
x11()
cv.out1 <- cv.glmnet(x[train, ], Puntaje[train], alpha = 1)
plot(cv.out1,col="#9A32CD",family="serif", ylab="MSE")
title("Calibración de Lambda",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")
bestlam <- cv.out1$lambda.min
bestlam

## Se entrena el modelo con los datos de entrenamiento

grid <- 10^seq(10, -2, length = 100)

lasso.mod <- glmnet(x[train, ], Puntaje[train], alpha = 1,
                    lambda = grid)
x11()
plot(lasso.mod, family="serif",ylab="Coeficientes",xlab="Norma L1")
title("Comportamiento de los coeficientes",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")

## Se mira como se comporta el modelo entrenado anteriormente con el mejor lambda.

lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[testeo, ])
val.errors7<-mean((lasso.pred - Puntaje.test)^2) #MSE de 8538.874
val.errors7

out <- glmnet(x, Puntaje, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:85, ]
lasso.coef[lasso.coef != 0] # Modelo resultante de lasso con el mejor lambda.


## Como se ajusta las predicciones a las observaciones:

plot(BaseF[testeo, ]$PUN12_TOTAL,type="l",main="Observaciones vs predicción con LASSO sin interacciones",ylab="Puntaje",xlab="Individuo",family="serif")
legend( x = "topleft",legend=c("Obs testeo ","prediccion"), col=c("black","#CD5B45"),pch = c(19,19))
lines(lasso.pred,col="#CD5B45")

#######################################

### LASSO con todas las posibles interacciones:

Puntaje<-as.numeric(BaseF$PUN12_TOTAL)
x <- model.matrix(PUN12_TOTAL ~ 1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ANOTERMINACION*COL_DEPARTAMENTO+ASP_ANOTERMINACION*COL_NATURALEZA+ASP_ANOTERMINACION*ASP_ESTADOCIVIL+ASP_ANOTERMINACION*ASP_SEXO+ASP_ANOTERMINACION*ASP_ESTRATO+ASP_ANOTERMINACION*ASP_RAYA_TIPO+ASP_ANOTERMINACION*EDAD_PRES+COL_NATURALEZA*EDAD_PRES+
                    COL_NATURALEZA*ASP_ESTADOCIVIL+COL_NATURALEZA*ASP_SEXO+COL_NATURALEZA*ASP_RAYA_TIPO+EDAD_PRES*ASP_ESTADOCIVIL+EDAD_PRES*ASP_SEXO+EDAD_PRES*ASP_ESTRATO+EDAD_PRES*ASP_RAYA_TIPO+ASP_ESTADOCIVIL*ASP_SEXO+ASP_ESTADOCIVIL*ASP_ESTRATO+ASP_ESTADOCIVIL*ASP_RAYA_TIPO+ASP_ESTRATO*ASP_RAYA_TIPO, BaseF)[, -1]
dim(x)
Puntaje.test <- Puntaje[testeo]

##Se calibra lambda(Aca se demora un poco)

set.seed(1)
cv.out2 <- cv.glmnet(x[train, ], Puntaje[train], alpha = 1)
plot(cv.out2,family="serif", ylab="MSE",col="#9A32CD")
title("Calibración del Lambda",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")
bestlam2 <- cv.out2$lambda.min
bestlam2

#Se entrena el modelo con los datos de entrenamiento: (Aca se demora un poco)

grid <- 10^seq(10, -2, length = 100)

lasso.mod2 <- glmnet(x[train, ], Puntaje[train], alpha = 1,
                    lambda = grid)
x11()

plot(lasso.mod2, family="serif",ylab="Coeficientes",xlab="Norma L1")
title("Comportamiento de los coeficientes",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")

#Se realizan las predicciones para el testeo con el mejor lambda obtenido:

lasso.pred2 <- predict(lasso.mod2, s = bestlam2,
                      newx = x[testeo, ])

val.errors8<-mean((lasso.pred2 - Puntaje.test)^2)
val.errors8  #MSE de 8558.592 (el modelo no mejora con respecto al anterior con todas las interacciones)

#Modelo resultante con el mejor lambda:

out <- glmnet(x, Puntaje, alpha = 1, lambda = grid)
lasso.coef2 <- predict(out, type = "coefficients", s = bestlam2)[1:1855, ]
lasso.coef2[lasso.coef2 != 0]

#######################################

### LASSO con algunas interacciones escogidas según  el contexto:

##Matriz de testeo

Puntaje<-as.numeric(BaseF$PUN12_TOTAL)
x <- model.matrix(PUN12_TOTAL ~ 1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ESTRATO*ASP_SEXO+ASP_SEXO*ASP_RAYA_TIPO+ASP_SEXO*COL_DEPARTAMENTO+ASP_SEXO*COL_NATURALEZA+COL_DEPARTAMENTO*COL_NATURALEZA, BaseF)[, -1]
dim(x)
Puntaje.test <- Puntaje[testeo]

#Calibración del lambda
set.seed(1)
cv.out3 <- cv.glmnet(x[train, ], Puntaje[train], alpha = 1)
plot(cv.out3,family="serif", ylab="MSE",col="#9A32CD")
title("Calibración del Lambda",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")
bestlam3 <- cv.out3$lambda.min

## Se entrena el modelo con los datos de entranemiento
grid <- 10^seq(10, -2, length = 100)
lasso.mod3 <- glmnet(x[train, ], Puntaje[train], alpha = 1,
                    lambda = grid)
x11()
plot(lasso.mod3, family="serif",ylab="Coeficientes",xlab="Norma L1")
title("Comportamiento de los coeficientes",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")

lasso.pred3 <- predict(lasso.mod3, s = bestlam3,
                      newx = x[testeo, ])

val.errors9<-mean((lasso.pred3 - Puntaje.test)^2) 
val.errors9  #MSE de 8529.745 (mejora con respecto a los dos anteriores)

out <- glmnet(x, Puntaje, alpha = 1, lambda = grid)
lasso.coef3 <- predict(out, type = "coefficients", s = bestlam3)[1:164, ]
lasso.coef3[lasso.coef3 != 0]

x11()
plot(BaseF[testeo, ]$PUN12_TOTAL,type="l",main="Observaciones vs predicción con LASSO con interacciones",ylab="Puntaje",xlab="Individuo",family="serif")
legend( x = "topleft",legend=c("Obs testeo ","prediccion"), col=c("black","#90EE90"),pch = c(19,19))
lines(lasso.pred3,col="#90EE90")

###################################################

### RIDGE sin interacciones:

## Matriz de testeo
x <- model.matrix(PUN12_TOTAL ~ ., BaseF)[, -1]
Puntaje.test <- Puntaje[testeo]

#Calibración del lambda

set.seed(1)
cv.out4 <- cv.glmnet(x[train, ], Puntaje[train], alpha = 0) 
x11()
plot(cv.out4,family="serif", ylab="MSE",col="#9A32CD")
title("Calibración del Lambda",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")
bestlam4 <- cv.out4$lambda.min
bestlam4
log(bestlam4)

## Se entrena el modelo

grid <- 10^seq(10, -2, length = 100)
ridge.mod1 <- glmnet(x[train, ], Puntaje[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)

ridge.pred1 <- predict(ridge.mod1, s = bestlam4, newx = x[testeo, ])
val.errors10<-mean((ridge.pred1 - Puntaje.test)^2)  #Error de 8563.312
val.errors10

###################################################

### RIDGE con interacciones:

x <- model.matrix(PUN12_TOTAL ~ 1+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+EDAD_PRES+ASP_ESTRATO*ASP_SEXO+ASP_SEXO*ASP_RAYA_TIPO+ASP_SEXO*COL_DEPARTAMENTO+ASP_SEXO*COL_NATURALEZA+COL_DEPARTAMENTO*COL_NATURALEZA, BaseF)[, -1]
Puntaje.test <- Puntaje[testeo]

## Calibración de lambda
set.seed(1)
cv.out5 <- cv.glmnet(x[train, ], Puntaje[train], alpha = 0) #10-fold crossval.
x11()
plot(cv.out5,family="serif", ylab="MSE",col="#9A32CD")
title("Calibración del Lambda",
      adj = 0.5,  # Título a la derecha
      line = 3,
      family="serif")
bestlam5 <- cv.out5$lambda.min
bestlam5

## Modelo entrenado:

grid <- 10^seq(10, -2, length = 100)
ridge.mod2 <- glmnet(x[train, ], Puntaje[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)

## Predicción:

ridge.pred2 <- predict(ridge.mod2, s = 17.35044, newx = x[testeo, ])
val.errors11<-mean((ridge.pred2 - Puntaje.test)^2)
val.errors11 # MSE de 8550.288 (mejora con respecto al anterior dado en RIDGE)

##### (Concluimos que el mejor modelo de regularización esta dado por LASSO con algunas interacciones dado que nos
#     da un MSE 8529.745 y comparando con los modelos de los otros métodos este es por ahora el menor MSE dado por
#     tanto, hasta ahora este es el mejor modelo con habilidad predictiva.)



#____________________________________________________________________________________________________________________________

########                          MODELOS NO PARAMETRICOS

library(gam)

##################################################

## No parametrica con SPLINES: (Primero estimamos un modelo con la edad PRESS unicamente como variable cuantitativa)

DATOS<- read_excel("C:/Users/danie/Downloads/data.xlsx")
fechas<-DATOS$ASP_FECHANACIMIENTO # Cambio de la variable fecha de nacimiento:
EDAD_PRES<-floor(time_length(ymd("2015-06-01")-ymd(fechas),unit="year"))
BaseF<-dplyr::select(DATOS,-ASP_FECHANACIMIENTO) # Nueva Base de datos con la variable edad de presentación del examen.
BaseF<-cbind(BaseF,EDAD_PRES)
View(BaseF)
Puntaje<-as.numeric(BaseF$PUN12_TOTAL)

## Particion de los datos

set.seed(1) #Semilla
train1<-sample(1:nrow(BaseF),size=0.7*nrow(BaseF),replace=FALSE)
dato<-c(5060,2952) #Se necesitan estos datos en en entrenamiento para que se puedan obtener las estimaciones de estas categorias.
train1<-c(train1,dato)
train<-rep(FALSE,nrow(BaseF))
train[train1] <- TRUE
testeo <- (!train)


BaseF2<-as.data.frame(BaseF[train1,]) #Base de datos con solo el entrenamiento.
testeo.M<-BaseF[testeo,]              #Base de datos con solo el testeo.
Puntajetrain<-as.numeric(BaseF2$PUN12_TOTAL)  #Variable respuesta solo para train.

#### Empezamos con un modelo inicial gam poniendo un grado de 4: (modelo referencia)

modgam<- gam(Puntajetrain~ s(EDAD_PRES, 4)+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO,
             data = BaseF2)
summary(modgam)

predicciones1 <- predict(modgam, newdata = testeo.M)
val.errors12<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predicciones1)^2)
val.errors12 #Error de MSE de 8501.307 (El menor hasta ahora)

plot(testeo.M$PUN12_TOTAL,predicciones1,main="Observaciones vs Predicción con regresión local",family="serif",ylab="Predicción",xlab="Observación",col="darkblue")

## Veamos cual es el mejor grado que se puede ingresar al spline en terminos del MSE:

Errores<-as.numeric()
for(i in 1:10){
  modgam<- gam(Puntajetrain~ s(EDAD_PRES, i)+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO,
               data = BaseF2)
  predicC <- predict(modgam, newdata = testeo.M)
  Errores[i]<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predicC)^2)
}
Errores
which.min(Errores) #Con un spline de grado 8 se minimiza el error

#Entrenamos el modelo:

modgambest<- gam(Puntajetrain~ s(EDAD_PRES, 8)+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO,
                 data = BaseF2)
summary(modgambest)
plot(modgambest, se = TRUE, col = "#79CDCD",lwd=2)

predicciones2 <- predict(modgambest, newdata = testeo.M)
val.errors13<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predicciones2)^2)
val.errors13 #MSE de 8456.443 (menor MSE hasta el momento)

plot(testeo.M$PUN12_TOTAL,predicciones2,main="Observaciones vs Predicción con regresión local",family="serif",ylab="Predicción",xlab="Observación",col="darkblue")

###########################################################

### Modelo no parametrico con SPLINES e interacciones:

#Se calibra el grado del polinomio

Errores2<-as.numeric()
for(i in 1:10){
  modgam<- gam(Puntajetrain~ s(EDAD_PRES, i)+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+ASP_SEXO*ASP_RAYA_TIPO,
               data = BaseF2)
  predics2 <- predict(modgam, newdata = testeo.M)
  Errores2[i]<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predics2)^2)
}
Errores2
which.min(Errores2) #Grado 8 es el mejor.

#Se entrena el modelo

modgambest2<- gam(Puntajetrain~ s(EDAD_PRES, 8)+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO+ASP_SEXO*ASP_RAYA_TIPO,
                  data = BaseF2)
summary(modgambest2)

plot(modgambest2, se = TRUE, col = "blue")

predicciones3 <- predict(modgambest2, newdata = testeo.M)
val.errors14<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predicciones3 )^2)
val.errors14 #MSE de 8452.524
plot(testeo.M$PUN12_TOTAL,predicciones3,main="Observaciones vs Predicción con regresión local",family="serif",ylab="Predicción",xlab="Observación",col="darkblue")

##############################################

## Como los splines solo se pueden colocar a variables cuantitativas  ¿que pasa cuando colocamos el año de terminación como una variables
#  cuantitativa que se interpreta como el número de años que tiene de bachiller una persona y se le agrega un spline?


DATOS<- read_excel("C:/Users/danie/Downloads/data.xlsx")
fechas<-DATOS$ASP_FECHANACIMIENTO # Cambio de la variable fecha de nacimiento:
EDAD_PRES<-floor(time_length(ymd("2015-06-01")-ymd(fechas),unit="year"))
BaseF<-dplyr::select(DATOS,-ASP_FECHANACIMIENTO) # Nueva Base de datos con la variable edad de presentación del examen.
BaseF<-cbind(BaseF,EDAD_PRES)
BaseF$ASP_ANOTERMINACION<-2015-as.numeric(BaseF$ASP_ANOTERMINACION)  #Cambio de la variable del año de terminación por la cantidad de tiempo siendo bachiller a la hora de presentar el examen.
View(BaseF)

## Partición de los datos:

set.seed(1)
train1<-sample(1:nrow(BaseF),size=0.7*nrow(BaseF),replace=FALSE)
dato<-c(5060,2952)
train1<-c(train1,dato)
train<-rep(FALSE,nrow(BaseF))
train[train1] <- TRUE
testeo <- (!train)


BaseF2<-as.data.frame(BaseF[train1,]) # Base de datos con solo el entrenamiento
testeo.M<-BaseF[testeo,]              # Base de datos con solo el testeo
Puntajetrain<-as.numeric(BaseF2$PUN12_TOTAL)

#Entrenamos el modelo de referencia:

modgam2<- gam(Puntajetrain~ s(EDAD_PRES, 4)+s(ASP_ANOTERMINACION,5)+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO,
              data = BaseF2)
summary(modgam2)

plot(modgam2, se = TRUE, col = "blue")

predicciones4 <- predict(modgam2, newdata = testeo.M)
val.errors15<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predicciones4)^2)
val.errors15 # MSE de 8466.006

#Veamos cuales son los mejores valores de los polinomios de spline para obtener el menor MSE
Errores3<-NULL
for(i in 1:10){
  for(j in 1:10){
    modgam2<- gam(Puntajetrain~ s(EDAD_PRES, i)+s(ASP_ANOTERMINACION,j)+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO,
                  data = BaseF2)
    predics3 <- predict(modgam2, newdata = testeo.M)
    Errores3<-cbind(Errores3,mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predics3)^2))
  }
  
}
Errores3
which.min(Errores3) #80 se da en i=8 y j=10

## Se entrena el modelo para el cual se tiene el menor MSE
modgambest3<- gam(Puntajetrain~ s(EDAD_PRES, 8)+s(ASP_ANOTERMINACION,10)+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO,
                  data = BaseF2)
summary(modgambest3)

plot(modgambest3, se = TRUE, col = "blue")

predicciones5 <- predict(modgambest3, newdata = testeo.M)
val.errors16<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predicciones5)^2)
val.errors16 # Error de MSE de 8416.138 (Este error es mucho menor que todos los errores obtenidos anteriormente)
plot(BaseF[testeo, ]$PUN12_TOTAL,type="l",main="Observaciones vs predicción con no parámetrica",ylab="Puntaje",xlab="Individuo",family="serif")
legend( x = "topleft",legend=c("Obs testeo ","prediccion"), col=c("black","#A020F0"),pch = c(19,19))
lines(predicciones5,col="#A020F0")
plot(testeo.M$PUN12_TOTAL,predicciones5,main="Observaciones vs Predicción con splines",family="serif",ylab="Predicción",xlab="Observación",col="darkblue")



##############################################

### No parametrica con regresión local:

DATOS<- read_excel("C:/Users/danie/Downloads/data.xlsx")
fechas<-DATOS$ASP_FECHANACIMIENTO # Cambio de la variable fecha de nacimiento:
EDAD_PRES<-floor(time_length(ymd("2015-06-01")-ymd(fechas),unit="year"))
BaseF<-dplyr::select(DATOS,-ASP_FECHANACIMIENTO) # Nueva Base de datos con la variable edad de presentación del examen.
BaseF<-cbind(BaseF,EDAD_PRES)

BaseF2<-as.data.frame(BaseF[train1,]) #Base de datos con solo el entrenamiento
testeo.M<-BaseF[testeo,] #Base de datos con solo el testeo
Puntajetrain<-as.numeric(BaseF2$PUN12_TOTAL)

## Se entrena el modelo
modgamlo <- gam(Puntajetrain ~ lo(EDAD_PRES, span=0.4)+ASP_ANOTERMINACION+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO ,data = BaseF2)
plot.Gam(modgamlo, se = TRUE , col = "green")
summary(modgamlo)

## Predicciones para el testeo
predicciones6<- predict(modgamlo, newdata = testeo.M)
plot(testeo.M$PUN12_TOTAL,predicciones6,main="Observaciones vs Predicción con regresión local",family="serif",ylab="Predicción",xlab="Observación",col="darkblue")
val.errors17<- mean((as.numeric(BaseF$PUN12_TOTAL[testeo]) - predicciones6)^2)
val.errors17 #Error de 8494.914

#=====================================================================================================================================

### (En conclusión el mejor modelo de habilidad predictiva es un modelo no parametrico con dos splines y tomando el año como cuantitativo)

#======================================================================================================================================

######                                      VISUALIZACIÓN DEL MEJOR MODELO Y PREDICCIONES:


DATOS<- read_excel("C:/Users/danie/Downloads/data.xlsx")
fechas<-DATOS$ASP_FECHANACIMIENTO # Cambio de la variable fecha de nacimiento:
EDAD_PRES<-floor(time_length(ymd("2015-06-01")-ymd(fechas),unit="year"))
BaseF<-dplyr::select(DATOS,-ASP_FECHANACIMIENTO) # Nueva Base de datos con la variable edad de presentación del examen.
BaseF<-cbind(BaseF,EDAD_PRES)
BaseF$ASP_ANOTERMINACION<-2015-as.numeric(BaseF$ASP_ANOTERMINACION)  #Cambio de la variable del año de terminación por la cantidad de tiempo siendo bachiller a la hora de presentar el examen.
View(BaseF)
BaseF$PUN12_TOTAL<-as.numeric(BaseF$PUN12_TOTAL)

mejormodelofin <- gam(PUN12_TOTAL~ s(EDAD_PRES, 8)+s(ASP_ANOTERMINACION,10)+COL_DEPARTAMENTO+COL_NATURALEZA+ASP_ESTADOCIVIL+ASP_SEXO+ASP_ESTRATO+ASP_RAYA_TIPO,
                      data = BaseF)
plot.Gam(mejormodelofin, se = TRUE , col = "green")
summary(mejormodelofin)

prediccionesf <- predict(mejormodelofin, newdata = BaseF)
#Predicciones para todos los datos y visualización del ajuste:

plot(BaseF$PUN12_TOTAL,type="l",main="Observaciones vs predicción con No parametrica",ylab="Puntaje",xlab="Individuo",family="serif")
legend( x = "topleft",legend=c("Observación ","Prediccion"), col=c("black","#8B6508"),pch = c(19,19))
lines(prediccionesf,col="#8B6508") #Se puede observar un buen ajuste sin sobreajuste.

#PREDICCIONES DEL EXCEL:

#Nuevos datos:
data2 <- read_excel("C:/Users/danie/Downloads/data2.xlsx")

#Se necesitan la edad de presentacion y tiempo de bachiller en el momento de la presentación por tanto:

fechas2<-data2$ASP_FECHANACIMIENTO # Cambio de la variable fecha de nacimiento:
EDAD_PRES<-floor(time_length(ymd("2015-06-01")-ymd(fechas2),unit="year"))
DataF<-dplyr::select(data2,-ASP_FECHANACIMIENTO) # Nueva Base de datos con la variable edad de presentación del examen.
DataF<-cbind(DataF,EDAD_PRES)
DataF$ASP_ANOTERMINACION<-2015-as.numeric(DataF$ASP_ANOTERMINACION)  #Cambio de la variable del año de terminación por la cantidad de tiempo siendo bachiller a la hora de presentar el examen.
View(DataF)

Prediccionesfinales<-predict(mejormodelofin, newdata = DataF)
PREDICCION<-as.data.frame(Prediccionesfinales)

library(openxlsx)
View(PREDICCION)
write.xlsx(PREDICCION,"pred_Reg_Grupo_6.xlsx")

