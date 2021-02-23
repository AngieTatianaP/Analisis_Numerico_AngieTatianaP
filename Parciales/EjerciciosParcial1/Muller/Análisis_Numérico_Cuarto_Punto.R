library(Rmpfr)

f <- function(x)((667.832865/x)*(1-exp(-0.146843*x))-40) #funcion

#Asignar el intervalo
x0 <-  10.0
x1 <-  15.0
x2 <- (x0 + x1)/2
tol <- 1e-10

# Obtenemos el numero maximo de iteraciones
N <- (1/log(2))*(log(abs(x0-x1)/tol))

iniciales<- c()

#Metodo de muller
h1 <- x1-x0
h2 <- x2-x1
d1 <- (f(x1)-f(x0))/h1
d2 <- (f(x2)-f(x1))/h2
a <- (d2-d1)/h2+h1
i <- 0

#Ciclo que se repite hasta el numero de iteraciones
while(i< N){b<-d2+h2*a
D<-(b^2-4*(f(x2)*a))^(1/2)
if (b > 0){E<-b+D}
if (b < 0){E<-b-D}
h <- 2*f(x2)/E
p <- x2+h
iniciales <- c(iniciales, p)
if (abs((p-x1)/p)<tol){p
  break}
x0<-x1
x1<-x2
x2<-p
h1<-x1-x0
d1<-(f(x1)-f(x0))/h1
d2<-(f(x2)-f(x1))/h2
a <-(d2-d1)/h2+h1
i<-i+1

}
mpfr(result,168)
