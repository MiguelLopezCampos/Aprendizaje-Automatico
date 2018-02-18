###Autor - Miguel López Campos

#######################Ejercicio de Generación y Visualización de Datos##############

set.seed(1)
#Apartado 1. Construir una función lista = simula_unif(N; dim; rango) que calcule una lista de
#longitud N de vectores de dimensión dim conteniendo números aleatorios uniformes en
#el intervalo rango.
simula_unif <- function(N, dim, rango){
  x <- rep(dim, N)
  #Para los próximos ejercicios me conviene devolver una matriz en lugar de una lista
  #sapply lo que hace es aplicar repetidas veces una función devolviendo una estructura de
  #datos lo más simple y simplificada posible
  a<-sapply(x, runif, min = rango[1], max = rango[2])
  return (a)
}

r <- c(0,5)
#Pruebo la función
y <- simula_unif(N=3, dim=5, c(0,5))
y
cat("Ejemplo ejecución función simula_unif")
cat(y)
cat("\n")

cat("Introduzca algún valor para continuar con la ejecución para continuar con la ejecución")
pausa <- scan(nmax=1)
#Apartado 2. Construir una función lista = simula_gaus(N; dim; sigma) que calcule una lista de
#longitud N de vectores de dimensión dim conteniendo números aleatorios gaussianos
#de media 0 y varianzas dadas por el vector sigma.
simula_gauss <- function(N, dim, sigma){
  x <- rep(dim, N)
  #Para los próximos ejercicios me conviene devolver una matriz en lugar de una lista
  a <- sapply(x, rnorm, sd = sigma)
  return (a)
}

y <- simula_gauss(N=5, dim=5, sigma=1)
y
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

#Apartado 3. Suponer N = 50, dim = 2, rango = [????50; +50] en cada dimensión. Dibujar una gráfica
#de la salida de la función correspondiente.
salida_3 <- simula_unif(N=50, dim=2, c(-50,50))
salida_3 <- t(salida_3)
salida_3 <- as.data.frame(salida_3)
names(salida_3) <- c("x","y")
plot(salida_3, col = 2, main="Ejecución de simula_unif")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Apartado 4
salida_4 <- simula_gauss(N=50, dim=2, c(5,7))
salida_4 <- t(salida_4)
salida_4 <- as.data.frame(salida_4)
names(salida_4) <- c("x","y")
plot(salida_4, col = 2, main="simula_gauss")

cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)
#Apartado 5
simula_recta <- function()
{
  intervalo <- c(-50,50)
  p <- simula_unif(N=2, dim=2, c(-50,50))
  a <- (p[2,2]-p[2,1])/(p[1,2]-p[1,1])
  b <- p[2,1]-p[1,1]*a
  return(list(a,b))
}

cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)
#Apartado 6


muestra <- simula_unif(N=1000, dim=2, c(-50,50))
muestra <- t(muestra)
muestra <- as.data.frame(muestra)
names(muestra) <- c("x", "y")
recta <- simula_recta()
label_ejercicio6 <- rep(0,50)

#calculo las etiquetas de la muestra
label_ejercicio6 <- muestra$y-recta[[1]]*muestra$x-recta[[2]]
positivos <- label_ejercicio6>0
#Si son positivos le asigno 1 y si no -1
label_ejercicio6[positivos] <- 1
label_ejercicio6[!positivos] <- -1

#Dibujo los de clase +1 de color verde y los de clase -1 de color rojo
positivos <- label_ejercicio6==1
plot(muestra[positivos,], col="green", main="Etiquetado de muestra aleatoria")
points(muestra[!positivos,], col="red")

abline(a=recta[[2]], b=recta[[1]])

cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

#Apartado 7
#Primera función
label1 <- (muestra$x-10)^2+(muestra$y-20)^2-400
positivos <- label1>0
label1[positivos] <- 1
label1[!positivos] <- -1
plot(muestra[positivos,], col="green", main="Etiquetado primera función")
points(muestra[!positivos,], col="red")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)
#Segunda función
label2 <- 0.5*(muestra$x+10)^2+(muestra$y-20)^2-400
positivos <- label2>0
label2[positivos] <- 1
label2[!positivos] <- -1
plot(muestra[positivos,], col="green", main="Etiquetado segunda función")
points(muestra[!positivos,], col="red")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)
#Tercera funcón
label3 <- 0.5*(muestra$x+10)^2-(muestra$y+20)^2-400
positivos <- label3>0
label3[positivos] <- 1
label3[!positivos] <- -1
plot(muestra[positivos,], col="green", main="Etiquetado tercera función")
points(muestra[!positivos,], col="red")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)
#Cuarta función
label4 <- muestra$y-20*(muestra$x)*(muestra$x)-5*muestra$x+3
positivos <- label4>0
label4[positivos] <- 1
label4[!positivos] <- -1
plot(muestra[!positivos,], col="red", main="Etiquetado cuarta función")
points(muestra[positivos,], col="green")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Apartado 8
change_muestra <- function(x)
{
  
  n_positivos <- as.integer(length(x[x>0])/10)
  probs <- rep(1, length(x))
  probs[x<0] <- 0
  #Con sample obtengo aleatoriamente un vector de índices aleatorios correspondientes
  #con el 10% de las etiquetas positivas. Estos índices los usaré para cambiar la etiqueta
  ind_aleatorios <- sample(x=(1:length(x)), prob=probs, size=n_positivos)
  new_muestra <-x
  new_muestra[ind_aleatorios] <- -1
  
  n_negativos <- as.integer(length(x[x<0])/10)
  probs <- rep(1, length(x))
  probs[x>0] <- 0
  ind_aleatorios <- sample(x=(1:length(x)), prob=probs, size=n_negativos)
  new_muestra[ind_aleatorios] <- 1
  
  return(new_muestra)
}


label_ejercicio8 <- change_muestra(label_ejercicio6)

##Dibujo la gráfica con la nueva muestra
positivos <- label_ejercicio8==1
plot(muestra[positivos,], col="green", main="Muestra ejercicio 6 con ruido")
points(muestra[!positivos,], col="red")



abline(a=recta[[2]], b=recta[[1]])

cat("Introduzca algún valor para continuar con la ejecución \n")
pausa <- scan(nmax=1)



#Primera función
label_ejercicio81 <- change_muestra(label1)

positivos <- label_ejercicio81>0
plot(muestra[positivos,], col="green")
points(muestra[!positivos,], col="red")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Segunda función
label_ejercicio81 <- change_muestra(label2)

positivos <- label_ejercicio81>0

plot(muestra[positivos,], col="green")
points(muestra[!positivos,], col="red")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Tercera funcón
label_ejercicio81 <- change_muestra(label3)

positivos <- label_ejercicio81>0

plot(muestra[positivos,], col="green")
points(muestra[!positivos,], col="red")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)



#Cuarta función
label_ejercicio81 <- change_muestra(label4)
positivos <- label_ejercicio81>0

plot(muestra[!positivos,], col="red")
points(muestra[positivos,], col="green")
cat("Introduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)



######################################################################################
######################################################################################
####################################Ejercicio 5.3#####################################
#Apartado 1
#Añado como argumento el término independiente 'b', que posteriormente será añadido al
#array de pesos (w)
ajusta_PLA <- function(datos, label, max_iter, vini, b)
{
  change <- 1
  w <- vini
  iter <- 0
  while(change == 1 && iter < max_iter)
  {  
    
    change <- 0
    
    for(i in seq_along(label))
    {
      if(sign(sum(datos[i,]*w)+b)!=sign(label[i]))
      {
        change <- 1
        w <- w + datos[i,]*label[i]
        b <- b + label[i]
      }
    }
    
    iter <- iter+1
  }
  
  length(w) <- length(w)+1
  w[length(w)] <- b
  cat("\nSe han necesitado ")
  cat(iter)
  cat(" iteraciones")
  
  return(list(iter,w))
}

#Apartado 2
prueba <- as.matrix(muestra)
b <- 0
wini <- rep(0, dim(prueba)[2])
valores <- ajusta_PLA(prueba, label_ejercicio6, 1000, wini, b)[[2]]
cat("\nLos pesos son: ")
cat(valores)
positivos <- label_ejercicio6>0
plot(muestra[positivos,], col="green", main="Estimación apartado 2")
points(muestra[!positivos,], col="red")
abline(a=-valores[3]/valores[2], b=-valores[1]/valores[2])
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)

iter <- 0
for(i in 1:10)
{
  wini <- simula_unif(2, 1, c(0,1))
  b <- runif(n=1, min=0, max=1)
  cat("\nValores\n")
  cat(wini)
  cat(" ")
  cat(b)
  valores <- ajusta_PLA(prueba, label_ejercicio6, 1000, wini, b)
  iter <- iter+valores[[1]]
  cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
  pausa <- scan(nmax=1)
}

cat("Se ha necesitado una media de ")
cat(iter/10)
cat(" iteraciones\n")
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)



#Apartado 3
wini <- c(0,0)
b <- 0
ajusteA <- ajusta_PLA(prueba, label_ejercicio8, 10, wini, b)[[2]]
label_apartado3a <- ajusteA[2]*muestra$y+ajusteA[1]*muestra$x+ajusteA[3]
label_apartado3a[label_apartado3a>0] <- 1
label_apartado3a[label_apartado3a<0] <- -1
cat(length(label_apartado3a[label_apartado3a!=label_ejercicio8]))
cat(" errores")
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)


wini <- c(0,0)
b <- 0
ajusteB <- ajusta_PLA(prueba, label_ejercicio8, 100, wini, b)[[2]]
label_apartado3b <- ajusteB[2]*muestra$y+muestra$x*ajusteB[1]+ajusteB[3]
label_apartado3b[label_apartado3b>0] <- 1
label_apartado3b[label_apartado3b<0] <- -1
cat(length(label_apartado3b[label_apartado3b!=label_ejercicio8]))
cat(" errores")
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)

wini <- c(0,0)
b <- 0
ajusteC <- ajusta_PLA(prueba, label_ejercicio8, 1000, wini, b)[[2]]
label_apartado3c <- muestra$y*ajusteC[2]+muestra$x*ajusteC[1]+ajusteC[3]
label_apartado3c[label_apartado3c>0] <- 1
label_apartado3c[label_apartado3c<0] <- -1
cat(length(label_apartado3c[label_apartado3c!=label_ejercicio8]))
cat(" errores")
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)




#Apartado 4
#Etiqueto con la función del apartado 7 del ejercicio anterior
wini <- c(0,0)
b <- 0
ajuste4a <- ajusta_PLA(prueba, label1, 10, wini, b)[[2]]
label_apartado4a <- muestra$y*ajuste4a[2]+muestra$x*ajuste4a[1]+ajuste4a[3]
label_apartado4a[label_apartado4a>0] <- 1
label_apartado4a[label_apartado4a<0] <- -1
cat(length(label_apartado4a[label_apartado4a!=label1]))
cat(" errores")
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)

wini <- c(0,0)
b <- 0
ajuste4b <- ajusta_PLA(prueba, label1, 100, wini, b)[[2]]
label_apartado4b <- muestra$y*ajuste4b[2]+muestra$x*ajuste4b[1]+ajuste4b[3]
label_apartado4b[label_apartado4b>0] <- 1
label_apartado4b[label_apartado4b<0] <- -1
cat(length(label_apartado4b[label_apartado4b!=label1]))
cat(" errores")
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)

wini <- c(0,0)
b <- 0
ajuste4c <- ajusta_PLA(prueba, label1, 1000, wini, b)[[2]]
label_apartado4c <- muestra$y*ajuste4c[2]+muestra$x*ajuste4c[1]+ajuste4c[3]
label_apartado4c[label_apartado4c>0] <- 1
label_apartado4c[label_apartado4c<0] <- -1
cat(length(label_apartado4c[label_apartado4c!=label1]))
cat(" errores")
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)


#Apartado 5
#Hago lo mismo que en PLA pero despues de cada iteración sobre cada uno
#de las componentes de la muestra dibujo en una gráfica la recta así como la muestra
ajusta_PLA_modificado <- function(datos, label, max_iter, vini, b)
{
  change <- 1
  w <- vini
  iter <- 0
  while(change == 1 && iter < max_iter)
  {  
    
    change <- 0
    
    for(i in seq_along(label))
    {
      
      
      if(sign(sum(datos[i,]*w)+b)!=sign(label[i]))
      {
        change <- 1
        w <- w + datos[i,]*label[i]
        b <- b + label[i]
      }
    }
    positivos <- label>0
    plot(datos[positivos,], col="red")
    points(datos[!positivos,], col="green")
    abline(a=(-b/w[2]), b=(-w[1]/w[2]))
    Sys.sleep(0.1)
    
    iter <- iter+1
  }
  
  length(w) <- length(w)+1
  w[length(w)] <- b
  cat("Se han necesitado ")
  cat(iter)
  cat(" iteraciones")
  
  return(w)
}



#Realizo las pruebas sobre la muestra generada en el apartado 8 del ejercicio
#anterior
wini <- c(0,0)
b <- 0
ajusteA <- ajusta_PLA_modificado(prueba, label_ejercicio8, 10, wini, b)
label_apartado3a <- ajusteA[2]*muestra$y+ajusteA[1]*muestra$x+ajusteA[3]
label_apartado3a[label_apartado3a>0] <- 1
label_apartado3a[label_apartado3a<0] <- -1
cat(length(label_apartado3a[label_apartado3a!=label_ejercicio8]))
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)

wini <- c(0,0)
b <- 0
ajusteB <- ajusta_PLA_modificado(prueba, label_ejercicio8, 100, wini, b)
label_apartado3b <- ajusteB[2]*muestra$y+muestra$x*ajusteB[1]+ajusteB[3]
label_apartado3b[label_apartado3b>0] <- 1
label_apartado3b[label_apartado3b<0] <- -1
cat(length(label_apartado3b[label_apartado3b!=label_ejercicio8]))
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)

wini <- c(0,0)
b <- 0
ajusteC <- ajusta_PLA_modificado(prueba, label_ejercicio8, 1000, wini, b)
label_apartado3c <- muestra$y*ajusteC[2]+muestra$x*ajusteC[1]+ajusteC[3]
label_apartado3c[label_apartado3c>0] <- 1
label_apartado3c[label_apartado3c<0] <- -1
cat(length(label_apartado3c[label_apartado3c!=label_ejercicio8]))
cat("\nIntroduzca algún valor para continuar con la ejecución para continuar")
pausa <- scan(nmax=1)

##Apartado 6
#Una buena solución es el perceptron pocket.
#Para comprobar si una solución de pesos es mejor que otra,
#veo el número de fallos con respecto del etiquetado original
#y si los nuevos pesos generados el error es menor, me quedo con esos
ajusta_PLA_MOD <- function(datos, label, t, max_iter_PLA)
{
  vini <- c(0,0)
  b <- 0
  w <- c(vini,b)
  error_mejor <- 1
    
  for(i in 1:t)
  {
    data_tmp <- as.matrix(datos)
    w_temp <- ajusta_PLA(data_tmp,label, max_iter_PLA, w[1:2], w[3])[[2]]
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



pesos_apartado6 <- ajusta_PLA_MOD(muestra, label1, 10, 10)
label_7primerafuncion <- pesos_apartado6[1]*muestra$x+pesos_apartado6[2]*muestra$y+pesos_apartado6[3]
label_7primerafuncion[label_7primerafuncion>0] <- 1
label_7primerafuncion[label_7primerafuncion<0] <- -1
errores <- label1!=label_7primerafuncion
errores <- label1[errores]
Error <- length(errores)/length(label1)
cat("El error es del ")
cat(Error*100)
cat("%\n")



######################################################################################
######################################################################################
####################################Ejercicio 5.4#####################################
#Apartados 1 y 2
#Leo los datos
zip <- read.table("./datos/zip.train", quote="\"", comment.char="")
a <- data.matrix(zip)
m <- list()
#Hago una lista con matrices de 16x16 que se corresponderán con los datos
for(i in 1:nrow(a))
{ 
  m[[i]] <- matrix(a[i,2:257], nrow=16, ncol=16)
}

image(m[[1]])
cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

#Cojo los datos cuyas etiquetas son menores o iguales que 5
labels_digits <- a[,1]
accept1 <- labels_digits == 5
accept2 <- labels_digits == 1
accept <- accept1+accept2
m <- m[as.logical(accept)]
labels_digits <- labels_digits[as.logical(accept)]
cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Apartado 3
#Calculo el valor medio de cada imagen
data_means <- sapply(m,mean)

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

#Aplico sapply con la función que acabo de crear. Así realizará
#la función sobre todas las matrices
data_simmetry <- sapply(m, simmetry_function)
cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)



#Apartado 4
#Para dibujar la gráfica dibujaré los puntos cuyas etiquetas sean 5's de color
#rojo y los 1's de color verde
son_5 <- labels_digits == 5
#Primero dibuja en tipo "nothing" para que los ejes se adapten a la muestra
#es decir, que todos los puntos se puedan representar bien
plot(x=data_means, y=data_simmetry, type = "n", xlab="Intensidad Media", ylab="Simetría", main="Dígitos (1's y 5's)")
points(x=data_means[son_5], y=data_simmetry[son_5], col="green")
points(x=data_means[!son_5], y=data_simmetry[!son_5], col="red")
cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Apartado 5
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




#Apartado 6
#Creo un data frame y posteriormente una matriz con las medias y la simetría
#calculada anteriormente
datos <- data.frame(data_means, data_simmetry, rep(1,length(data_means)))
datos <- as.matrix(datos)
etiquetas <- matrix(labels_digits, ncol = 1)
#Las etiquetas clasificaré los 5 como -1 y los 1's como +1
cincos <- etiquetas==5
etiquetas[cincos] = -1
pesos <- Regress_lin(datos, etiquetas)
plot(x=data_means, y=data_simmetry, type = "n", xlab="Intensidad media", ylab="Simetría", main="Ajuste regresión lineal")
points(x=data_means[son_5], y=data_simmetry[son_5], col="green")
points(x=data_means[!son_5], y=data_simmetry[!son_5], col="red")
#Represento la recta que separa las muestras
abline(a=-pesos[3]/pesos[2], b=-pesos[1]/pesos[2])


cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)

#Apartado 7
##Subapartado a
Error <- vector()
for(i in 1:1000)
{
  #Genero una muestra así como una recta de forma aleatoria
  muestra_7a <- simula_unif(N=100, dim=2, c(-10,10))
  muestra_7a <- t(muestra_7a)
  muestra_7a <- as.data.frame(muestra_7a)
  names(muestra_7a) <- c("x", "y")
  recta_7a <- simula_recta()
  #Función f
  label_7a <- muestra_7a$y+muestra_7a$x*recta_7a[[1]]+recta_7a[[2]]
  ##Realizo las transformaciones correspondientes
  muestra_7a <- data.frame(muestra_7a, rep(1,nrow(muestra_7a)))
  positivos <- label_7a > 0
  label_7a[positivos] <- 1
  label_7a[!positivos] <- -1
  label_7a <- matrix(label_7a, ncol = 1)
  #Ejecuto la regresión lineal
  pesos_7a <- Regress_lin(muestra_7a, label_7a)
  #Realizo un etiquetado con los pesos obtenidos en la regresión lineal sobre
  #la muestra anteriormente generada
  etiquetados <- muestra_7a$y*pesos_7a[2]+muestra_7a$x*pesos_7a[1]+pesos_7a[3]
  positivos <- etiquetados>0
  etiquetados[positivos] <- 1
  etiquetados[!positivos] <- -1
  #Cuento los errores así como el porcentaje de error
  errores <- as.vector(label_7a)!=etiquetados
  errores <- label_7a[errores]
  Error[i] <- length(errores)/100
}

Error_medio <- mean(Error)

cat("\nEl error dentro de la muestra medio es del ")
cat(Error_medio*100)
cat("%")
cat("\n")


cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


##Subapartado b
Error <- vector()
for(i in 1:1000)
{
  #Genero una muestra aleatoria y la convierto a data frame y también una recta aleatorias
  muestra_7b <- simula_unif(N=100, dim=2, c(-10,10))
  muestra_7b <- t(muestra_7b)
  muestra_7b <- as.data.frame(muestra_7b)
  names(muestra_7b) <- c("x", "y")
  recta_7b <- simula_recta()
  #Eiqueto la muestra con la función f
  label_7b <- muestra_7b$y+muestra_7b$x*recta_7b[[1]]+recta_7b[[2]]
  ##Realizo las transformaciones correspondientes para aplicarle la regresión
  muestra_7b <- data.frame(muestra_7b, rep(1,nrow(muestra_7b)))
  positivos <- label_7b > 0
  label_7b[positivos] <- 1
  label_7b[!positivos] <- -1
  label_7b <- matrix(label_7b, ncol = 1)
  pesos_7b <- Regress_lin(datos=muestra_7b, label=label_7b)
  #Ahora genero una muestra aleatoria de 1000 elementos y calculo con ésto
  #el error de fuera de la muestra
  nueva_muestra <- simula_unif(N=1000, dim=2, c(-10,10))
  nueva_muestra <- t(nueva_muestra)
  nueva_muestra <- as.data.frame(nueva_muestra)
  names(nueva_muestra) <- c("x","y")
  #Hago los nuevos etiquetados a la nueva muestra con la función f 
  label_7bnueva <- nueva_muestra$y+nueva_muestra$x*recta_7b[[1]]+recta_7b[[2]]
  positivos <- label_7bnueva > 0
  label_7bnueva[positivos] <- 1
  label_7bnueva[!positivos] <- -1
  #Hago los etiquetados de la nueva muestra con los pesos obtenidos de la regresión lineal
  etiquetados <- nueva_muestra$y*pesos_7b[2]+nueva_muestra$x*pesos_7b[1]+pesos_7b[3]
  positivos <- etiquetados>0
  etiquetados[positivos] <- 1
  etiquetados[!positivos] <- -1
  #Finalmente cuento los errores y calculo el porcentaje de error
  errores <- label_7bnueva!=etiquetados
  errores <- label_7b[errores]
  Error[i] <- length(errores)/1000
}

Error_medio <- mean(Error)

cat("\nEl error fuera de la muestra medio es del ")
cat(Error_medio*100)
cat("%")
cat("\n")


cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


iters <- 0
##Subapartado c
for(i in 1:1000)
{
  #Genero muestra y recta aleatorias
  muestra_7c <- simula_unif(N=10, dim=2, c(-10,10))
  muestra_7c <- t(muestra_7c)
  muestra_7c_PLA <- muestra_7c
  muestra_7c <- as.data.frame(muestra_7c)
  names(muestra_7c) <- c("x", "y")
  recta_7c <- simula_recta()
  #Realizo el etiquetado usando la recta generada
  label_7c <- muestra_7c$y-muestra_7c$x*recta_7c[[1]]-recta_7c[[2]]
  #Hago las modificaciones necesarias para aplicar la función Regress_lin
  muestra_7c <- data.frame(muestra_7c, rep(1,nrow(muestra_7c)))
  label_7c <- matrix(label_7c, ncol = 1)
  positivos <- label_7c > 0
  label_7c[positivos] <- 1
  label_7c[!positivos] <- -1
  pesos_7c <- Regress_lin(muestra_7c, label_7c)
  #Con los pesos obtenidos inicializamos el vector de pesos inicial del perceptron
  ajuste7c <- ajusta_PLA(muestra_7c_PLA, as.vector(label_7c), 1000, pesos_7c[1:2], pesos_7c[3])
  iters <- ajuste7c[[1]]+iters
  
}

cat("\nEl número medio de iteraciones es de ")
cat(iters/1000)
cat("\n")



cat("\nIntroduzca algún valor para continuar con la ejecución")
pausa <- scan(nmax=1)


#Apartado 8
Error <- vector()
for(i in 1:1000)
{
  muestra_8 <- simula_unif(N=1000, dim=2, c(-10,10))
  muestra_8 <- t(muestra_8)
  muestra_8 <- as.data.frame(muestra_8)
  names(muestra_8) <- c("x1","x2")
  label_8 <- muestra_8$x1*muestra_8$x1+muestra_8$x2*muestra_8$x2-25.0
  label_8[label_8>0] <- 1
  label_8[label_8<0] <- -1
  #Extraigo un 10% aleatorio de la muestra con la función sample
  probs <- rep(1,length(label_8))
  ind_aleatorios <- sample(x=(1:length(label_8)), prob=probs, size=length(label_8)/10)
  #Le cambio el signo a la muestra aleatoria elegida
  label_8[ind_aleatorios] <- label_8[ind_aleatorios]*(-1)
  #Aplico el ajuste con la regresión lineal
  muestra_8_reg <- data.frame(muestra_8, rep(1, length(label_8)))
  muestra_8_reg <- as.matrix(muestra_8_reg)
  label_8_reg <- matrix(label_8, ncol=1)
  pesos_8 <- Regress_lin(muestra_8_reg, label_8_reg)
  #Evalúo con los pesos calclados con la regresión lineal
  label_test_8 <- pesos_8[2]*muestra_8$x2+pesos_8[1]*muestra_8$x1+pesos_8[3]
  label_test_8[label_test_8>0] <- 1
  label_test_8[label_test_8<0] <- -1
  #Contamos los errores
  errores <- label_test_8!=as.vector(label_8)
  errores <- label_test_8[errores]
  Error[i] <- length(errores)/1000
}

Error_medio <- mean(Error)

cat("\nEl error dentro de la muestra medio es del ")
cat(Error_medio*100)
cat("%")
cat("\n")


#Creo una función para evaluar muestras con más de 2 dimensiones
evaluacion <- function(x, w)
{
  label_final <- rep(0, nrow(x))
  for(i in 1:ncol(x))
  {
    label_final <- x[,i]*w[i]+label_final
  }
  
  label_final[label_final>0] <- 1
  label_final[label_final<0] <- -1
  return(label_final)
}

muestra_8 <- simula_unif(N=1000, dim=2, c(-10,10))
muestra_8 <- t(muestra_8)
muestra_8 <- as.data.frame(muestra_8)
names(muestra_8) <- c("x1","x2")
#creo la nueva muestra con las variables del enunciado
muestra_8_b <- data.frame(muestra_8$x2^2, muestra_8$x1^2, muestra_8$x1*muestra_8$x2, muestra_8$x2, muestra_8$x1, rep(1, nrow(muestra_8)))
label_8_b <- (muestra_8$x1^2)+(muestra_8$x2^2)-25.0
label_8_b[label_8_b>0] <- 1
label_8_b[label_8_b<0] <- -1
probs <- rep(1,length(label_8_b))
ind_aleatorios <- sample(x=(1:length(label_8_b)), prob=probs, size=length(label_8_b)/10)
#Le cambio el signo a la muestra aleatoria elegida
label_8_b[ind_aleatorios] <- label_8_b[ind_aleatorios]*(-1)
label_8_b <- matrix(label_8_b, ncol=1)
#Ajusto la regresión lineal
pesos <- Regress_lin(datos=muestra_8_b, label=label_8_b)
label_test_8b <- evaluacion(x=muestra_8_b, w=pesos)
errores <- label_test_8b!=as.vector(label_8_b)
errores <- label_test_8b[errores]
Error <- length(errores)/1000

cat("Los pesos obtenidos son: ")
cat(pesos)
cat("\n")

Error <- vector()
for(i in 1:1000)
{
  #Genero una muestra aleatoria a partir de la cual aprenderé
  muestra_8 <- simula_unif(N=1000, dim=2, c(-10,10))
  muestra_8 <- t(muestra_8)
  muestra_8 <- as.data.frame(muestra_8)
  names(muestra_8) <- c("x1","x2")
  #creo la nueva muestra con las variables del enunciado
  muestra_8_c <- data.frame(muestra_8$x2^2, muestra_8$x1^2, muestra_8$x1*muestra_8$x2, muestra_8$x1, muestra_8$x2, rep(1, nrow(muestra_8)))
  label_8_c <- muestra_8$x1^2+muestra_8$x2^2-25.0
  label_8_c[label_8_c>0] <- 1
  label_8_c[label_8_c<0] <- -1
  probs <- rep(1,length(label_8_c))
  ind_aleatorios <- sample(x=(1:length(label_8_c)), prob=probs, size=length(label_8_c)/10)
  #Le cambio el signo a la muestra aleatoria elegida
  label_8_c[ind_aleatorios] <- label_8_c[ind_aleatorios]*(-1)
  label_8_c <- matrix(label_8_c, ncol=1)
  pesos_8c <- Regress_lin(datos=muestra_8_c, label=label_8_c)
  #Genero la muestra con la que averiguaré el error fuera de la muestra
  muestra_8_out <- simula_unif(N=1000, dim=2, c(-10,10))
  muestra_8_out <- t(muestra_8_out)
  muestra_8_out <- as.data.frame(muestra_8_out)
  names(muestra_8_out) <- c("x1","x2")
  #Calculo su etiquetado para la función f
  label_8_out <- muestra_8_out$x1^2+muestra_8_out$x2^2-25.0
  label_8_out[label_8_out>0] <- 1
  label_8_out[label_8_out<0] <- -1
  #Convierto el conjunto de variables para linealizar la función
  muestra_8_out <- data.frame(muestra_8_out$x2^2, muestra_8_out$x1^2, muestra_8_out$x1*muestra_8_out$x2, muestra_8_out$x1, muestra_8_out$x2, rep(1, nrow(muestra_8_out)))
  #Evalúo con los pesos obtenidos anteriormente
  label_test_8 <- evaluacion(x=muestra_8_out, w=pesos_8c)
  errores <- label_8_out!=label_test_8
  errores <- label_8_out[errores]
  Error[i] <- length(errores)/1000
}
Error_medio <- mean(Error)

cat("\nEl error fuera de la muestra medio es del ")
cat((Error_medio)*100)
cat("%")
cat("\n")



