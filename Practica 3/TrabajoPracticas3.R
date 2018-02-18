## ----echo=FALSE, message=FALSE, results='hide', warning=FALSE------------
  set.seed(1)
  library("ISLR")
  library("class")
  library("e1071")
  library(ROCR)
  library(MASS)
  library("glmnet")
  library(gbm)
  library(randomForest)
  library(tree)

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
##Ejercicio 1
##Apartados a y b
auto <- Auto
pairs(auto, col="blue")

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ----echo=FALSE----------------------------------------------------------
##Dibujo de las gráficas individualmente (de las que veo relación con mpg)
plot(x=auto$mpg, y=auto$displacement, main="Relación mpg y cilindrada", xlab="MPG", ylab="Cilindrada", col="blue")

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ----echo=FALSE----------------------------------------------------------
plot(x=auto$mpg, y=auto$horsepower, main="Relación mpg y potencia", xlab="MPG", ylab="Potencia", col="blue")

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ----echo=FALSE----------------------------------------------------------
plot(x=auto$mpg, y=auto$weight, main="Relación mpg y peso", xlab="MPG", ylab="Peso", col="blue")

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------

###Apartados c y d
##Hago el etiquetado (0,1 para LR y -1 y +1 para knn)
set.seed(1)
mediana <- median(auto$mpg)
datos <- data.frame(auto$displacement, auto$weight, auto$horsepower)
training_ind <- sample(x=(1:nrow(datos)), size=0.8*nrow(datos))
training <- datos[training_ind,]
test <- datos[-training_ind,]
label_training <- auto$mpg[training_ind]
label_test <- auto$mpg[-training_ind]
positivos <- label_training>mediana
label_training[positivos] <- 1
label_training[!positivos] <- 0
positivos <- label_test>mediana
label_test[positivos] <- 1
label_test[!positivos] <- 0


## ------------------------------------------------------------------------
##Aplico glm (regresión logística)
set.seed(1)

model <- glm(formula=label_training~.,family=binomial,data=training)
summary(model)

##Con predict calculo las probabilidades
probs <- predict(model,test,type="response")
##Hago el error de clasificación
label_prediction <- probs
label_prediction[label_prediction>=0.5] <- 1
label_prediction[label_prediction<0.5] <- 0
error <- label_test!=label_prediction
error <- error[error==TRUE]
error <- length(error)/length(label_test)
error

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
#Función que normaliza un conjunto de datos
normalizar <- function(datos)
{
  for(i in 1:ncol(datos))
  {
    minimo <- min(datos[,i], na.rm=TRUE)
    maximo <- max(datos[,i], na.rm=TRUE)
    
    if(minimo == 0 && maximo==0){datos[,i]<-0}else
    {
      datos[,i] <- (datos[,i]-minimo)/(maximo-minimo)
    }
  }
  
  return(datos)
}

## ------------------------------------------------------------------------
##A continuación aplicamos knn
set.seed(1)

datos_normalizados <- normalizar(datos)
##
##
training_knn <- datos_normalizados[training_ind,]
test_knn <- datos_normalizados[-training_ind,]
label_training_knn <- label_training
label_training_knn[label_training_knn==0] <- -1
prediction <- knn(training_knn, test_knn, cl=label_training_knn, k=3)
##########Etiquetas reales del conjunto de test
label_test_knn <- label_test
label_test_knn[label_test_knn==0] <- -1
##########Calculo el error
error <- label_test_knn[label_test_knn!=prediction]
error <- length(error)/length(label_test_knn)
##############################
error

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Aplicamos tune.knn para ver que k sería el óptimo para nuestro caso
##las etiquetas deben ser booleanos
label_training_knn[label_training_knn==-1] <- 0
label_training_knn[label_training_knn==1] <- 1

knn.cross <- tune.knn(x = training_knn, y = as.logical(label_training_knn), k = 1:20,
                      tunecontrol=tune.control(sampling = "cross"), cross=10)

## ------------------------------------------------------------------------
summary(knn.cross)

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ----echo=FALSE , warning=FALSE , message=FALSE , results='hide'---------
##Ahora dibujaremos las curvas ROC
pred_rocr_glm <- ROCR::prediction(label_test,label_prediction)
pred_rocr_knn <- ROCR::prediction(label_test_knn, prediction)
perf_rocr_knn <- ROCR::performance(pred_rocr_knn,"tpr","fpr")
perf_rocr_glm <- ROCR::performance(pred_rocr_glm,"tpr","fpr")
auc_knn <- as.numeric(performance(pred_rocr_knn,"auc")@y.values)
auc_glm <- as.numeric(performance(pred_rocr_glm,"auc")@y.values)
#Curva ROC para K-nn
plot(perf_rocr_knn,type='o', main = paste('Area Bajo la Curva =',round(auc_knn,2)))


## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ----echo=FALSE----------------------------------------------------------
##Curva ROC para regresión logístíca
plot(perf_rocr_glm,type='o', main = paste('Area Bajo la Curva =',round(auc_glm,2)))

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Apartado e (Bonus)

#Primero hacemos validación cruzada para regresión logística
set.seed(1)

probs <- rep(1,nrow(Auto))
errores <- vector()

for(i in 1:5)
{
    ##Saco la submuestra de test (20%)
    test <- sample(x=1:nrow(datos), size=(0.2*nrow(datos)), prob=probs)
    
    ##Pongo probs a 0 del subconjunto cogido para que no se vuelva a repetir
    probs[test] <- 0
    
    mediana <- median(Auto$mpg)
    label <- Auto$mpg
    positivos <- label>=mediana
    label[positivos] <- 1
    label[!positivos] <- 0
    
    label_test <- label[test]
    label_training <- label[-test]
    
    
    training_set <- datos[-test,]
    test_set <- datos[test,]

    ##Aplicamos ahora la función glm sobre el training data set
    model <- glm(formula=label_training ~ ., family=binomial, data=training_set)
    model_probs <- predict(model,test_set,type="response")
    label_prediction <- model_probs
    label_prediction[label_prediction>=0.5] <- 1
    label_prediction[label_prediction<0.5] <- 0
    error <- label_prediction!=label_test
    error <- error[error==TRUE]
    errores[i] <- length(error)/length(label_test)
    
}

##El error promedio es el siguiente:

mean(errores)

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Aplicamos validación cruzada al modelo k-nn
set.seed(1)

probs <- rep(1,nrow(Auto))
errores <- vector()

for(i in 1:5)
{
   ##Saco la submuestra de test (20%)
    test <- sample(x=1:nrow(datos), size=(0.2*nrow(datos)), prob=probs)
    
    ##Pongo probs a 0 del subconjunto cogido para que no se vuelva a repetir
    probs[test] <- 0
    
    ##Realizo el etiquetado
    mediana <- median(Auto$mpg)
    label <- Auto$mpg
    positivos <- label>=mediana
    label[positivos] <- 1
    label[!positivos] <- -1
    
    label_test <- label[test]
    label_training <- label[-test]
    
    training_set <- datos_normalizados[-test,]
    test_set <- datos_normalizados[test,]
    ##Aplicamos la funcion k-nn
    prediction <- knn(training_set, test_set, cl=label_training, k=5)
    
    ##Vemos el error
    error <- prediction[prediction!=label_test]
    errores[i] <- length(error)/length(label_test)
}

mean(errores)

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
############EJERCICIO 2
dataBoston <- Boston

## ------------------------------------------------------------------------
##Apartado a
set.seed(1)
##Creo un array que será nuestro Y (variable que predeciremos)

label_boston <- dataBoston$crim
##Elimino del dataset la columna que consideraremos etiqueta

dataBoston <- dataBoston[,-1]


##Aplico el modelo lasso sobre todo el conjunto de datos.
##Para aplicar regresión Lasso, tenemos que elegir el parámetro
##alpha de la función glmnet como '1'. Primero averiguo mediante
##Validación cuzada el mejor lambda posible

modelo_lasso <- cv.glmnet(as.matrix(dataBoston), label_boston, alpha=1)
mejor_lambda <- modelo_lasso$lambda.min


##A continuación aplico regresión lasso y hago una predicción de los
##coeficientes para el mejor lambda anteriormente calculado
##A glmnet hay que pasar el dataSet como una matriz

out <- glmnet(as.matrix(dataBoston), label_boston, alpha=1)
coefs <- predict(out, type="coefficients", s=mejor_lambda)


##Valor de los coeficientes:
coefs



## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Cogeremos los coeficientes cuyo valor absoluto sea mayor que
##el umbral 10^-2
coeficientes_validos <- coefs[2:nrow(coefs),]
coeficientes_validos <- abs(coeficientes_validos)>=10^-2

## ------------------------------------------------------------------------
dataBoston <- dataBoston[,coeficientes_validos]

##Nos quedamos con 10 características

## ------------------------------------------------------------------------
##Apartado b

set.seed(1)
##Averiguo mejor lambda

ridge.cv <- cv.glmnet(as.matrix(dataBoston), label_boston, alpha=0)
mejor_lambda <- ridge.cv$lambda.min

##Aplicamos ahora regresión ridge
##Predecimos los coeficientes de la misma manera que en el modelo lasso
##con la función predict

out <- glmnet(as.matrix(dataBoston), label_boston, alpha=0)
coefs <- predict(out, type="coefficients", s=mejor_lambda)
coefs <- coefs[1:nrow(coefs),]

##Los coeficientes (pesos) son los siguientes
coefs

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------

##A continución calculamos la predicción con predict
##y posteriormente el error residual cuadrático
prediccion <- predict(out, s=mejor_lambda, newx=as.matrix(dataBoston))

error <- mean((prediccion-label_boston)^2)


error

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Apartado c

##Creamos el etiquetado

mediana <- median(label_boston)
label_crim <- label_boston
positivos <- label_crim >= mediana
label_crim[positivos] <- +1
label_crim[!positivos] <- -1
label_crim <- as.factor(label_crim)


##Para que tune svm funcione debo tener la etiqueta en
##el dataset

dataBoston <- cbind(dataBoston, label_crim)

## ------------------------------------------------------------------------
set.seed(1)
out.tune <- tune(svm ,label_crim ~ ., data = dataBoston, kernel="linear", 
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))


## ------------------------------------------------------------------------
summary(out.tune)

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Con $best.parameters saco el mejor valor del coste
mejor_modelo <- out.tune$best.parameters

boston.svm <- svm(label_crim~., data=dataBoston, kernel="linear", cost=mejor_modelo)
prediccion <- predict(boston.svm, dataBoston)



##Tabla de confusión
table(predict=prediccion, truth=label_crim)

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
error <- prediccion[prediccion!=dataBoston$label_crim]
error <- length(error)/nrow(dataBoston)
error

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Bonus-3

##Validación cruzada para svm
set.seed(1)

probs <- rep(1,nrow(dataBoston))
errores_training <- vector()
errores_test <- vector()

##Encuentro el coste óptimo para nuestro caso
out.tune <- out.tune <- tune(svm ,label_crim ~ ., data = dataBoston, kernel="linear",
ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

mejor_modelo <- out.tune$best.parameters

for(i in 1:5)
{
  test <- sample(x=1:nrow(dataBoston), prob = probs, size = 0.2*nrow(dataBoston))
  
  ##Pongo las probabilidades de los elementos cogidos a test a 0 para
  ##que en la siguiente iteración no sean cogidos de nuevo
  probs[test] <- 0
  
  training_set <- dataBoston[-test,]
  test_set <- dataBoston[test,]
  
  ##Aplico svm
  boston.svm <- svm(label_crim ~., data=training_set, kernel="linear", cost=mejor_modelo)
  prediccion_training <- predict(boston.svm, training_set)
  prediccion_test <- predict(boston.svm, test_set)
  
  ##Calculo los errores de training y test
  error_training <- prediccion_training[prediccion_training!=training_set$label_crim]
  errores_training[i] <- length(error_training)/nrow(training_set)
  error_test <- prediccion_test[prediccion_test!=test_set$label_crim]
  errores_test[i] <- length(error_test)/nrow(test_set)
}

error_test <- mean(errores_test)
error_training <- mean(errores_training)

## ------------------------------------------------------------------------
#Error de test
error_test

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
#Error de training
error_training

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
#############EJERCICIO 3

##Apartado a
set.seed(1)
dataBoston3 <- Boston

##Cojo el 80% de los índices de forma aleatoria
##Preparo también nuestra variable que queremos predecir

training <- sample(x=1:nrow(dataBoston3), size=0.8*nrow(dataBoston3))

## ------------------------------------------------------------------------

##Apartado b

set.seed(1)
boston.model <- randomForest(formula= dataBoston3$medv ~ ., data=dataBoston3, 
                             subset=training, mtry=ncol(dataBoston3)-1, importance=TRUE)



## ----echo=FALSE----------------------------------------------------------
##Gráfico de la variación del error con el número de árboles
plot(boston.model, main="Variación error residual con el número de árboles")

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Realizamos la predicción con 'predict'
##introducimos como parámetros el modelo que hemos predicho
##con bagging y el subconjunto de test


boston.pred <- predict(boston.model, newdata = dataBoston3[-training,])

y.medv <- dataBoston3$medv[-training]
sq.error <- mean((boston.pred-y.medv)^2)

##El error cuadrático medio es el siguiente
sq.error

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Apartado c

set.seed(1)

boston.rf.model <- randomForest(formula= dataBoston3$medv ~ ., data=dataBoston3, 
                             subset=training, mtry=(ncol(dataBoston)-1)/3, importance=TRUE)

##Gráfico de la variación del error con el número de árboles
plot(boston.rf.model, main="Variación error residual con el número de árboles")

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
boston.pred.rf <- predict(boston.rf.model, newdata = dataBoston3[-training,])

y.medv.rf <- dataBoston3$medv[-training]
sq.error.rf <- mean((boston.pred.rf-y.medv.rf)^2)

##El error de test es el siguiente
sq.error.rf


## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------

##Apartado d
set.seed(1)

boston.boost <- gbm(formula=dataBoston3$medv[training] ~., data=dataBoston3[training,], 
                    distribution ="gaussian",n.trees =5000 , interaction.depth =4)

summary(boston.boost)

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Hacemos la predicción

boston.pred.boost <- predict(boston.boost, newdata = dataBoston3[-training,], n.trees=5000)
y.medv.boost <- dataBoston3$medv[-training]
sq.error.boost <- mean((boston.pred.boost-y.medv.boost)^2)

##El error de test es el siguiente
sq.error.boost

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##########EJERCICIO 4
#Apartados a, b y c


datos4 <- OJ

##Creo un array que serán las variables respuesta

set.seed(1)

##creamos la submuestra de 800 observaciones para training
training_4 <- sample(x=1:nrow(datos4), size=800)

training.OJ <- datos4[training_4,]
test.OJ <- datos4[-training_4,]


## ------------------------------------------------------------------------
#Ajustamos un árbol y lo dibujamos para interpretarlo (interpretación en el pdf)
set.seed(1)

tree.OJ <- tree(training.OJ$Purchase~., training.OJ)

plot(tree.OJ)
text(tree.OJ, pretty = 0)

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Resumen del árbol

summary(tree.OJ)

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
#Apartado d

purchase.pred <- predict(tree.OJ, newdata = test.OJ, type="class")

##Tabla de confusión
tabla <- table(purchase.pred, datos4$Purchase[-training_4])

tabla


## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
##Calculo la tasa de error de test

errores <- purchase.pred[purchase.pred!=datos4$Purchase[-training_4]]
error <- length(errores)/length(purchase.pred)
error

## ----results=FALSE , echo=FALSE , message=FALSE , results='hide'---------
print("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

## ------------------------------------------------------------------------
#Apartado e

#Por validación cruzada comprobamos el mejor árbol
tree.cv <- cv.tree(tree.OJ)

plot(tree.cv)

