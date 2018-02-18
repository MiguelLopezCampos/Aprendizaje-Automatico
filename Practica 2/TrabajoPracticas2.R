###Autor - Miguel López Campos
library("numDeriv")
library("orthopolynom")
set.seed(1)
#######################MODELOS LINEALES##############
#Apartado 1
funcionE <- function(args)
{
  u <- args[1]
  v <- args[2]
  
  valor <- (u*exp(v)-2*v*exp(-u))^2
  gradV <- 2*(u*exp(v)-2*v*exp(-u))*(u*exp(v)-2*exp(-u))
  gradU <- 2*(u*exp(v)-2*v*exp(-u))*(exp(v)+2*v*exp(-u))
  
  return (c(valor, gradU, gradV))
}


###Función de gradiente descendente
gradiente_descendente <- function(funcion, umbral=10^(-3), inicio, tasa=0.1, max_iters)
{
  ant <- 500.0
  w <- inicio
  act <- 0.0
  iters <- 0
  
  ##Criterio de parada. Mientras el valor de la función en la anterior iteración 
  ##y el de la actual sean prácticamente iguales (según el umbral)
  while(abs(act-ant)> umbral && iters < max_iters)
  {
    #resultados <- funcion(w)
    ant <- act
    resultados <- funcion(w)
    derivadas <- resultados[2:length(resultados)] 
    
    w <- w - tasa*derivadas
    
    
    resultados <- funcion(w)
    act <- resultados[1]
    
    iters <- iters+1
    
  }
  return (list(ant,w,iters))
  
}



#Versión modificada de gradiente descendente para realizar los subapartados 1.a.2 y 1.a.3
gradiente_descendente_mod <- function(funcion, umbral=10^(-3), inicio, tasa=0.1, max_iters)
{
  #ant <- 500.0
  w <- inicio
  act <- 500.0
  iters <- 0
  
  ##En esta versión de gradiente descendente paro cuando el error sea menor 
  ##de 10^-14, como me dice el enunciado del subapartado 1.a.2
  while(act > 10^(-14) && iters < max_iters)
  {
    #resultados <- funcion(w)
    ant <- act
    resultados <- funcion(w)
    derivadas <- resultados[2:length(resultados)] 
    
    w <- w - tasa*derivadas
    
    
    resultados <- funcion(w)
    act <- resultados[1]
    
    iters <- iters + 1
    
  }
  return (list(w,iters))
  
}



valores1 <- gradiente_descendente_mod(funcionE, inicio=c(1,1), tasa=0.1, max_iters = 1000)

cat("\nEl número de iteraciones es :")
cat(valores1[[2]])

cat("\nEl valor de u es ")
cat(valores1[[1]][1])
cat("\nEl valor de v es ")
cat(valores1[[1]][2])

#Los valores óptimos de u y v deberían ser u=0 y v=0. Como vemos, el algoritmo nos devuelve unos valores
#cercanos a 0.

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Apartado 1.b
funcionB <- function(args)
{
  x <- args[1]
  y <- args[2]
  
  valor <- x^2+2*y^2+2*sin(2*pi*x)*sin(2*pi*y)
  gradx <- 2*x + 2*cos(2*pi*x)*sin(2*pi*y)*2*pi
  grady <- 4*y + 2*cos(2*pi*y)*sin(2*pi*x)*2*pi
  
  return(c(valor,gradx,grady))
}


##Hago una variación de gradiente descendente pero que dibuje una gráfica con cada iteracion
gradiente_descendente_grafica <- function(funcion, umbral=10^(-3), inicio, tasa=0.1, max_iters)
{
  ant <- 500.0
  w <- inicio
  act <- 0.0
  iters <- 0
  
  valores <- vector()
  ##Criterio de parada. Mientras el valor de la función en la anterior iteración 
  ##y el de la actual sean prácticamente iguales (según el umbral)
  while(abs(act-ant) > umbral && iters < max_iters)
  {
    #resultados <- funcion(w)
    ant <- act
    resultados <- funcion(w)
    derivadas <- resultados[2:length(resultados)] 
    
    valores[iters+1] <- resultados[1]
    w <- w - tasa*derivadas
    
    
    resultados <- funcion(w)
    act <- resultados[1]
    
    iters <- iters+1

  }
  
  
  #Dibujamos el valor de la función en relación al número de iteración
  iteraciones <- 1:iters
  plot(x=iteraciones, y=valores, type="o", col="blue")
  return (list(w,iters))
  
}

#Como vemos en la gráfica, la función converge a 0
gradiente_descendente_grafica(funcion=funcionB, umbral=10^-5, inicio=c(1,1), tasa=0.01, max_iters=50)

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

gradiente_descendente_grafica(funcion=funcionB, umbral=10^-5, inicio=c(1,1), tasa=0.1, max_iters=50)
cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

resultados11 <- gradiente_descendente(funcion=funcionB, umbral=10^-5, inicio=c(0.1,0.1), tasa=0.01, max_iters=50)
cat("Para x0=0.1 e y0=0.1")
cat("\nValor: ")
cat(resultados11[[1]])
cat("\nPesos: ")
cat(resultados11[[2]][1])
cat(" ")
cat(resultados11[[2]][2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

resultados12 <- gradiente_descendente(funcion=funcionB, umbral=10^-5, inicio=c(1.0,1.0), tasa=0.01, max_iters=50)
cat("Para x0=1 e y0=1")
cat("\nValor: ")
cat(resultados12[[1]])
cat("\nPesos: ")
cat(resultados12[[2]][1])
cat(" ")
cat(resultados12[[2]][2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

resultados13 <- gradiente_descendente(funcion=funcionB, umbral=10^-5, inicio=c(-0.5,-0.5), tasa=0.01, max_iters=50)
cat("Para x0=-0.5 e y0=-0.5")
cat("\nValor: ")
cat(resultados13[[1]])
cat("\nPesos: ")
cat(resultados13[[2]][1])
cat(" ")
cat(resultados13[[2]][2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

resultados14 <- gradiente_descendente(funcion=funcionB, umbral=10^-5, inicio=c(-1,-1), tasa=0.01, max_iters=50)
cat("Para x0=-1 e y0=-1")
cat("\nValor: ")
cat(resultados14[[1]])
cat("\nPesos: ")
cat(resultados14[[2]][1])
cat(" ")
cat(resultados14[[2]][2])


cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

################################################################

##Ejercicio 2
##El ejercicio me pide que cuál es el valor tras 15 iteraciones completas,
##por lo que como criterio de parada no uso un umbral si no directamente 
##un número máximo de iteraciones
coordenada_descendente <- function(funcion, inicio, tasa=0.1, max_iters)
{
  ant <- 500.0
  w <- inicio
  act <- 0.0
  iters <- 0
  
  ##Criterio de parada. Mientras el valor de la función en la anterior iteración 
  ##y el de la actual sean prácticamente iguales (según el umbral)
  while(iters < max_iters)
  {
    #resultados <- funcion(w)
    ant <- act
    resultados <- funcion(w)
    derivadas <- resultados[2:length(resultados)] 
    
    #Primero la coordenada u
    w[1] <- w[1] - tasa*derivadas[1]
    
    #Después la coordenada v
    resultados <- funcion(w)
    derivadas <- resultados[2:length(resultados)]
    w[2] <- w[2] - tasa*derivadas[2]
    
    ##Obtengo el valor de la siguiente iteración
    resultados <- funcion(w)
    act <- resultados[1]
    
    iters <- iters+1
    
  }
  return (list(act,w,iters))
}

##Probamos el algoritmo de coordenada descendente
resultados_coord_desc <- coordenada_descendente(funcionE, c(1,1), tasa=0.1, 15)

cat("\nLos resultados para coordenada descendente en 15 iteraciones es:\n")
cat("u= ")
cat(resultados_coord_desc[[2]][1])
cat("\nv= ")
cat(resultados_coord_desc[[2]][2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

#Como podemos observar, en 15 iteraciones esta variación del algoritmo es bastante peor que el gradiente
#descendente (al menos en este caso). Gradiente descendente en 10 iteraciones nos daba un 
#resultado mucho más próximo al mínimo que en este caso coordenada descendente.
##Recordemos los resultados en gradiente descendente
cat("\nEn gradiente descendente: \n")
cat("\nEl número de iteraciones es :")
cat(valores1[[2]])

cat("\nEl valor de u es ")
cat(valores1[[1]][1])
cat("\nEl valor de v es ")
cat(valores1[[1]][2])



cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)
##########################################################################
#Ejercicio 3


minimizacion_newton <- function(wini, umbral=10^-3, max_iters)
{
  ant <- 500.0
  w <- wini
  act <- 0.0
  iters <- 0
  
  
  valores <- vector()
  
  
  while(iters<max_iters)
  {
    ant <- act
    resultados <- funcionB(as.vector(w))
   # gradiente <- funcionB(wini)[2:length(resultados)]
    gradiente <- resultados[2:length(resultados)]
    
    
    valores[iters+1] <- resultados[1]
    #Creo la función H con las segundas derivadas
    value1 <- 2-8*pi^2*sin(2*pi*w[2])*sin(2*pi*w[1])
    value2 <- 8*pi^2*cos(2*pi*w[1])*cos(2*pi*w[2])
    value3 <- 8*pi^2*cos(2*pi*w[1])*cos(2*pi*w[2])
    value4 <- 4-8*pi^2*sin(2*pi*w[1])*sin(2*pi*w[2])
    
    fila1 <- c(value1, value2)
    fila2 <- c(value3, value4)
    
    H <- rbind(fila1, fila2)
    H_inversa <- solve(H)
    
    w <- w-H_inversa %*% gradiente

    iters <- iters+1
  }
  
  iteraciones <- 1:iters
  plot(x=iteraciones, y=valores, type="o", col="blue")
  
  return(list(ant,w,iters))
}

##TODO
resultados12 <- minimizacion_newton(wini=c(-0.5,-0.5), umbral=10^-5, max_iters=10)
cat("Para x0=1 e y0=1")
cat("\nValor: ")
cat(resultados12[[1]])
cat("\nPesos: ")
cat(resultados12[[2]][1])
cat(" ")
cat(resultados12[[2]][2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


resultados12 <- minimizacion_newton(wini=c(-1,-1), umbral=10^-5, max_iters=10)
cat("Para x0=1 e y0=1")
cat("\nValor: ")
cat(resultados12[[1]])
cat("\nPesos: ")
cat(resultados12[[2]][1])
cat(" ")
cat(resultados12[[2]][2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

resultados12 <- minimizacion_newton(wini=c(0.1,0.1), umbral=10^-5, max_iters=10)
cat("Para x0=1 e y0=1")
cat("\nValor: ")
cat(resultados12[[1]])
cat("\nPesos: ")
cat(resultados12[[2]][1])
cat(" ")
cat(resultados12[[2]][2])


cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


resultados12 <- minimizacion_newton(wini=c(1,1), umbral=10^-5, max_iters=10)
cat("Para x0=1 e y0=1")
cat("\nValor: ")
cat(resultados12[[1]])
cat("\nPesos: ")
cat(resultados12[[2]][1])
cat(" ")
cat(resultados12[[2]][2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)
##########################################################################
#Ejercicio 4

##Función simula_unif
simula_unif <- function(N, dim, rango){
  x <- rep(dim, N)
  #Para los próximos ejercicios me conviene devolver una matriz en lugar de una lista
  #sapply lo que hace es aplicar repetidas veces una función devolviendo una estructura de
  #datos lo más simple y simplificada posible
  a<-sapply(x, runif, min = rango[1], max = rango[2])
  return (a)
}
##Función simula_recta
simula_recta <- function(intervalo=c(-1,1))
{
  p <- simula_unif(N=2, dim=2, intervalo)
  a <- (p[2,2]-p[2,1])/(p[1,2]-p[1,1])
  b <- p[2,1]-p[1,1]*a
  return(list(a,b))
}


rectaF <- simula_recta()




#Creo la muestra de datos
conjunto_datos <- simula_unif(N=100, dim=2, rango=c(-1,1))
conjunto_datos <- t(conjunto_datos)
conjunto_datos <- as.data.frame(conjunto_datos)
names(conjunto_datos) <- c("x","y")

#Realizo el etiquetado
label_log <- conjunto_datos$y-rectaF[[1]]*conjunto_datos$x-rectaF[[2]]
positivos <- label_log > 0
label_log[positivos] <- +1
label_log[!positivos] <- -1

#Añado una columna de 1's
conjunto_datos <- cbind(conjunto_datos, rep(1,nrow(conjunto_datos)))


#Apartado a
gradiente_estocastico <- function(muestra, etiquetas, wini, umbral = 0.01, tasa = 0.01)
{

  umbral <- 0.01
  tasa <- 0.01
  
  
  w <- wini
  diferencia_modulo <- 10
  
  #Criterio de parada ||Want-W|| < umbral (0.01)
  while(diferencia_modulo >= umbral)
  {
    #Genero una "mezcla" de la muestra con la función sample.
    #Lo que hago es coger un vector de enteros que van desde el 1 hasta el número
    #de filas de muestra y sample los permuta aleatoriamente
    permutacion_aleatoria <- sample(x=(1:nrow(muestra)), size=nrow(muestra))
    want <- w
    
    #Para cada punto de la permutación aleatoria
    for(i in permutacion_aleatoria)
    {
      #Actualizo los pesos según el algoritmo de regresión logística
      w <- w - tasa*(-etiquetas[i]*muestra[i,])/(1+exp(etiquetas[i]*muestra[i,]*want))
    }
    
    #Calculo la diferencia de modulos
    #modulowant <- sqrt(sum(want))
    #modulow <- sqrt(sum(w))
    dif <- w-want
    diferencia_modulo <- sqrt(sum(dif^2))
  }
  
  return(w)
}


pesos_estimados <- gradiente_estocastico(conjunto_datos, label_log, c(0,0,0), umbral=0.01, tasa=0.01)
pesos_estimados <- as.vector(pesos_estimados)
#Creo un nuevo dataset para estimar el Error de fuera de la muestra
conjunto_out <- simula_unif(N=500, dim=2, rango=c(-1,1))
conjunto_out <- t(conjunto_out)
conjunto_out <- as.data.frame(conjunto_out)
names(conjunto_out) <- c("x","y")

label_out <- conjunto_out$y-rectaF[[1]]*conjunto_out$x-rectaF[[2]]
positivos <- label_out > 0
label_out[positivos] <- +1
label_out[!positivos] <- -1

#Añado una columna de 1's
conjunto_out <- cbind(conjunto_out, rep(1,nrow(conjunto_out)))
conjunto_out <- as.matrix(conjunto_out)
#eout <- sum(log(1+exp(-label_out*pesos_estimados*conjunto_out)))

sum <- 0.0
for(i in 1:500)
{
  sum <- sum+log(1+exp(sum(-label_out[i]*pesos_estimados*conjunto_out[i,])))
}

cat("El Eout es: ")
cat(sum/500)
cat("\n")


cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)
#########################################################################
#Ejercicio 5
train <- read.table("datos/zip.train", 
                    quote="\"", comment.char="")
test <- read.table("datos/zip.test", 
                   quote="\"", comment.char="")


a <- data.matrix(train)
m <- list()
b <- data.matrix(test)
n <- list()

#Hago una lista con matrices de 16x16 que se corresponderán con los datos
for(i in 1:nrow(a))
{ 
  m[[i]] <- matrix(a[i,2:257], nrow=16, ncol=16)
}

for(i in 1:nrow(b))
{
  n[[i]] <- matrix(b[i,2:257], nrow=16, ncol=16)
}


#Cojo los datos cuyas etiquetas son iguales que 5 o 1
labels_digits_train <- a[,1]
accept1 <- labels_digits_train == 5
accept2 <- labels_digits_train == 1
accept <- accept1+accept2
m <- m[as.logical(accept)]
labels_digits_train <- labels_digits_train[as.logical(accept)]

#Ahora para el conjunto de test
labels_digits_test <- b[,1]
accept1 <- labels_digits_test == 5
accept2 <- labels_digits_test == 1
accept <- accept1+accept2
n <- n[as.logical(accept)]
labels_digits_test <- labels_digits_test[as.logical(accept)]

#Hago la intensidad media
data_means_train <- sapply(m,mean)
data_means_test <- sapply(n, mean)


#Calculo la simetría
simmetry_function <- function(m)
{
  sum <- 0
  for(i in 1:nrow(m))
  {
    for(j in 1:ncol(m))
    {
      sum <- sum + abs(m[i,j]-m[i,ncol(m)-j+1])
    }
  }
  
  sum <- -sum
  return(sum)
}

data_simmetry_train <- sapply(m, simmetry_function)
data_simmetry_test <- sapply(n, simmetry_function)

train <- data.frame(data_means_train, data_simmetry_train)
train <- cbind(train, rep(1,nrow(train)))
names(train) <- c("x","y")

test <- data.frame(data_means_test, data_simmetry_test)
test <- cbind(test, rep(1,nrow(test)))


#Clasifico los 5 como -1 y los 1's como +1
labels_digits_train[labels_digits_train==5] <- -1
labels_digits_test[labels_digits_test==5] <- -1 



#Función de regresión lineal
Regress_lin <- function(datos, label)
{
  descomposicion <- svd(datos)
  #La función diag construye una matriz diagonal a partir de un array
  D <- descomposicion$d
  for(i in 1:length(D))
  {
    if(D[i] > 10^-4)
    {
      D[i] <- 1/D[i]
    }else{D[i] <- 0}
  }
  D <- diag(D)
  V <- descomposicion$v
  U <- descomposicion$u
  #
  D <- t(D)
  pseudo_inverse <- V %*% D %*% t(U)
  w <- as.vector(pseudo_inverse %*% label)
}


#Implementación de PLA
ajusta_PLA <- function(datos, label, max_iter, vini)
{
  change <- 1
  w <- vini
  iter <- 0
  while(change == 1 && iter < max_iter)
  {  
    
    change <- 0
    
    for(i in seq_along(label))
    {
      if(sign(sum(datos[i,]*w))!=sign(label[i]))
      {
        change <- 1
        w <- w + datos[i,]*label[i]
      }
    }
    
    iter <- iter+1
  }
  
  
  return(list(iter,w))
}


#Implementación de Pocket PLA
pocket_PLA <- function(datos, label, t, max_iter_PLA, wini)
{
  w <- wini
  error_mejor <- 1
  
  for(i in 1:t)
  {
    data_tmp <- as.matrix(datos)
    w_temp <- ajusta_PLA(data_tmp,label, max_iter_PLA, w)[[2]]
    etiq_nuev <- datos$x*w_temp[1] + datos$y*w_temp[2] + w_temp[3]
    etiq_nuev[etiq_nuev>0] <- 1
    etiq_nuev[etiq_nuev<0] <- -1
    errores <- label!=etiq_nuev
    errores <- label[errores]
    error_nuevo <- length(errores)/length(label)
    
    if(error_nuevo < error_mejor)
    {
      error_mejor <- error_nuevo
      w <- w_temp
    }
  }
  
  return(w)
}


#Aplico regresión lineal para minimizar el error
pesos <- Regress_lin(train, labels_digits_train)

#Ahora aplico el Pocket PLA, con el que ya hago la estimación de la recta
#Uso 20 iteraciones del pocketPLA y el PLA normal se ejecutará un máximo de 1000 veces
pesos_nuevos <- pocket_PLA(train, labels_digits_train, 20, 1000, pesos)

plot(x=train$x, y=train$y, xlab="Intensidad Media", ylab="Simetría")
points(x=train[labels_digits_train==-1,1], y=train[labels_digits_train==-1,2], col="green")
points(x=train[labels_digits_train==1,1], y=train[labels_digits_train==1,2], col="red")
abline(a=-pesos_nuevos[3]/pesos_nuevos[2], b=-pesos_nuevos[1]/pesos_nuevos[2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

#Ahora pintamos los datos de test con la recta
names(test) <- c("x", "y")
plot(x=test$x, y=test$y, xlab="Intensidad media", ylab="Simetría")
points(x=test[labels_digits_test==-1,1], y=test[labels_digits_test==-1,2], col="green")
points(x=test[labels_digits_test==1,1], y=test[labels_digits_test==1,2], col="red")
abline(a=-pesos_nuevos[3]/pesos_nuevos[2], b=-pesos_nuevos[1]/pesos_nuevos[2])

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

#Calculo el error de dentro de la muestra
label_in <- train$x*pesos_nuevos[1] + train$y*pesos_nuevos[2] + pesos_nuevos[3]
label_in[label_in<0] <- -1
label_in[label_in>0] <- +1
errores <- labels_digits_train[label_in!=labels_digits_train]
errores <- length(errores)
cat("\nEl error dentro de la muestra es: ")
cat((errores/length(label_in))*100)
cat("%\n")

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Calculo el Etest
label_test <- test$x*pesos_nuevos[1] + test$y*pesos_nuevos[2] + pesos_nuevos[3]
label_test[label_test<0] <- -1
label_test[label_test>0] <- +1
errores <- labels_digits_test[label_test!=labels_digits_test]
errores <- length(errores)
cat("\nEl error de test es: ")
cat((errores/length(label_test))*100)
cat("%\n")

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


###########################################################################
###########################################################################
###########################################################################
##Apartado de Sobreajuste
#Ejercicio 1
datos_X <- simula_unif(100, 1, c(-1,1))


#Esta es la función f
funcionF <- function(x, Qf)
{
  suma <- 0.0
  #Sumatoria
  for(i in 0:Qf)
  {
    a <- 0:Qf
    aq <- rnorm(n=1, mean=0, sd=1)
    aq <- aq/(sqrt(sum(1/(2*a+1))))
    #slegendre.polynomials nos devuelve una lista de objetos polynomials
    #que corresponden con los polinomios de Legendre desde k=0 hasta k dado como
    #argumento. al hacer as.function se transforma la clase polynomial a una función
    #la cual evaluaremos sobre x
    L <- as.function(legendre.polynomials(i)[[i+1]])
    suma <- suma+aq*L(x)
  }
  
  return(suma)
}


#Calculamos los Yn sin ruido
#yn <- sapply(datos_X, funcionF, Qf=2)+0.5*rnorm(n=1,mean=0, sd=1)
yn <- vector()
for(i in 1:length(datos_X))
{
  yn[i] <- funcionF(datos_X[i], Qf=2)+0.5*rnorm(n=1,mean=0,sd=1)
}
datos_X <- cbind(datos_X, rep(1,length(datos_X)))
 
#Ahora "linealizamos" los datos
datos_X2 <- cbind(datos_X, datos_X^2)
datos_X2 <- cbind(rep(1, nrow(datos_X2)), datos_X2)

datos_X10 <- cbind(rep(1,length(datos_X)))
for(i in 1:10)
{
  datos_X10 <- cbind(datos_X10, datos_X^i)
}

pesos_g2 <- Regress_lin(datos_X2, yn)
pesos_g10 <- Regress_lin(datos_X10, yn)
curve(pesos_g2[3]*x^2+pesos_g2[2]*x+pesos_g2[1], xlim = c(min(datos_X)-1,max(datos_X)+1), ylim = c(min(yn)-1, max(yn)+1), col="red", ylab="Valor Y")
points(x=datos_X[,1], y=yn)

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

curve(pesos_g10[11]*x^10+pesos_g10[10]+pesos_g10[9]*x^9+pesos_g10[8]*x^7+pesos_g10[7]*x^6+pesos_g10[6]*x^5+pesos_g10[5]*x^4+pesos_g10[4]*x^3+pesos_g10[3]*x^2+pesos_g10[2]*x+pesos_g10[1], xlim = c(min(datos_X)-1,max(datos_X)+1), ylim = c(min(yn)-1, max(yn)+1), col="red", ylab="Valor Y")
points(x=datos_X[,1], y=yn)

cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

#####################################################################################
#####################################################################################
#####################################################################################
####Apartado 3 - Regularización y selección de modelos
##Ejercicio 1
suc <- seq(from=15, to=115, by=10)

#Función simula_gauss
simula_gauss <- function(N, dim, sigma){
  x <- rep(dim, N)
  #Para los próximos ejercicios me conviene devolver una matriz en lugar de una lista
  a <- sapply(x, rnorm, sd = sigma)
  return (a)
}

d <- 3

for(i in suc)
{
  muestra <- simula_gauss(N = d+i, dim=d, sigma=1)
  muestra <- t(muestra)
  muestra <- cbind(muestra, rep(1, nrow(muestra)))
  pesos_reg <- simula_gauss(N=1, dim=d+1, sigma=1)
  pesos_reg <- as.vector(t(pesos_reg))
  y_n <- pesos_reg[1]*muestra[,1] + pesos_reg[2]*muestra[,2] + pesos_reg[3]*muestra[,3] + pesos_reg[4]*muestra[,4] + 0.5*rnorm(n=1, mean=0, sd=1)
  
  lambda <- 0.05/(d+i)

  errores <- vector()
  for(j in 1:(d+i))
  {
    #Estimo los pesos aplicando regresión lineal con regularización
    w_reg <- solve(t(muestra[-j,]) %*% muestra[-j,] + lambda*diag(ncol(muestra[-j,]))) %*% t(muestra[-j,])%*%y_n[-j]
    #aquí saco el error cuadrático
    errores[j] <- ((w_reg[1]*muestra[j,1]+w_reg[2]*muestra[j,2]+w_reg[3]*muestra[j,3]+w_reg[4]*muestra[j,4]) - y_n[j])^2
  }
  
  Ecv <- mean(errores)
  
  cat("Ecv para N= ")
  cat(d+i)
  cat(": ")
  cat(Ecv)
  cat("\n")
  
}


cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


e1mean <- vector()
e1sd <- vector()
e2mean <- vector()
e2sd <- vector()
Ecvmean <- vector()
Ecvsd <- vector()
#Apartado b
for(k in 1:10)
{
  
  e1 <- vector()
  e2 <- vector()
  Ecv <- vector()
  for(i in suc)
  {
    muestra <- simula_gauss(N = d+i, dim=d, sigma=1)
    muestra <- t(muestra)
    muestra <- cbind(muestra, rep(1, nrow(muestra)))
    pesos_reg <- simula_gauss(N=1, dim=d+1, sigma=1)
    pesos_reg <- as.vector(t(pesos_reg))
    y_n <- pesos_reg[1]*muestra[,1] + pesos_reg[2]*muestra[,2] + pesos_reg[3]*muestra[,3] + pesos_reg[4]*muestra[,4] + 0.5*rnorm(n=1, mean=0, sd=1)
    
    lambda <- 0.05/(d+i)


    errores <- vector()
    for(j in 1:(d+i))
    {
      #Estimo los pesos aplicando regresión lineal con regularización
      w_reg <- solve(t(muestra[-j,]) %*% muestra[-j,] + lambda*diag(ncol(muestra[-j,]))) %*% t(muestra[-j,])%*%y_n[-j]
      #aquí saco el error cuadrático
      errores[j] <- ((w_reg[1]*muestra[j,1]+w_reg[2]*muestra[j,2]+w_reg[3]*muestra[j,3]+w_reg[4]*muestra[j,4]) - y_n[j])^2
      
      if(j == 1)
      {
        e1[i/15] <- errores[j]
      }else if(j == 2)
      {
        e2[i/15] <- errores[j]
      }
    }
    
    Ecv[i/15] <- mean(errores)
  }
  
  e1mean[k] <- mean(e1)
  e2mean[k] <- mean(e2)
  Ecvmean[k] <- mean(Ecv)
  e1sd[k] <- mean((e1-e1mean[k])^2)
  e2sd[k] <- mean((e2-e2mean[k])^2)
  Ecvsd[k] <- mean((Ecv-Ecvmean[k])^2)
}