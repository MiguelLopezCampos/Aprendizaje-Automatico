#Ejercicio 2.3 Cree un vector de 10 componentes enteras decrecientes denominado s. Compruebe
#los atributos que tiene el sistema sobre los tipos de los vectores x y s.
s <- 10:1
s
class(s)

#rep lo que hace es repetir un vector tantas veces como se le indique
rep(s, 2)



s+1
x <- 1:5
x
s+x
s
x+s
x
s

#Cuando se realizan operaciones entre vectores el resultante será un vector con la longitud del más grande
y <- -5:5
y
abs(y)
log(y)
log(abs(y))



?range
?prod


s[s>4]
s


#Valores nulos
w <- numeric()
w
length(w)<-10
w
w[3:5]=5
w
good = !is.na(w)
good
w[good]
