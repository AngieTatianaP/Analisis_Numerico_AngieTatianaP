library(Rmpfr)

f <- function(x)(x*sin(x)-1) #Funcion
#Creacion del vector que guardara la sucesion
iniciales <- c()

#Definir el intervalo
x0 <- 0.0
x1 <- 2.0

# Por medio del metodo de biseccion obtenemos el valor de x2
x2 <- (x0 + x1)/2

# Asignamos el valor de la tolerancia
tol <- 1e-5

# Obtenemos el numero maximo de iteraciones
N <- (1/log(2))*(log(abs(x0-x1)/tol))
h1 <- x1-x0
h2 <- x2-x1
d1 <- (f(x1)-f(x0))/h1
d2 <- (f(x2)-f(x1))/h2
a <- (d2-d1)/h2+h1
i <- 0

#Ciclo que se repite hasta el numero de maximo de iteraciones
while(i< floor(N)){
  b <- d2+h2*a
  D<-(b^2-4*(f(x2)*a))^(1/2)
  if (b > 0){E<-b+D}
  if (b < 0){E<-b-D}
  h <- 2*f(x2)/E
  result <- x2+h
  iniciales <- c(iniciales,result)
  if (abs((result-x1)/result)<tol){result
    break
  }
  x0 <- x1
  x1 <- x2
  x2 <- result
  h1 <- x1-x0
  d1 <- (f(x1)-f(x0))/h1
  d2 <- (f(x2)-f(x1))/h2
  a <-(d2-d1)/h2+h1
  i <- i+1
}
#Imprimir respuesta
mpfr(result,168)