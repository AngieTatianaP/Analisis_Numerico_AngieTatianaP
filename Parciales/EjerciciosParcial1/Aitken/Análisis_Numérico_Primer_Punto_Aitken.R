library(Rmpfr)
mimuller <- function (){
  f <- function(x)(((cos(x)^2))-x^2) 
  iniciales <- c()
  x0 <- 0.0
  x1 <- 1.0
  x2 <- (x0 + x1)/2
  tol <- 1e-8
  
  N <- (1/log(2))*(log(abs(x0-x1)/tol))
  h1 <- x1-x0
  h2 <- x2-x1
  d1 <- (f(x1)-f(x0))/h1
  d2 <- (f(x2)-f(x1))/h2
  a <- (d2-d1)/h2+h1
  i <- 0
  
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
  
  cat (iniciales)
  iniciales <- c(iniciales,i)
  return(iniciales)
}

Datos <- mimuller()

p0 = Datos[1]
p1 = Datos[2]
p2 = Datos[2]
i <- Datos[length(Datos)]
Datos[-(length(Datos))]
valor = c()


g = 2
iteraciones = 0; #Contador de iteraciones
for(k in Datos[4:(length(Datos))]-2){ # Ciclo que se repite por la cantidad de datos del vector
  valor = c(valor ,(p2-(((p2-p1)*(p2-p1))/(p2-(2*p1)+p0))))   # terminos del polinomio propuesto.
  p0 =Datos[g]
  p1 =Datos[g+1]
  p2=Datos[g+2]
  g=g+1
  iteraciones = iteraciones + 1
  print(valor)
}
mpfr(valor,168)
cat ("El numero de iteraciones realizadas en el metodo Muller son: ", length(Datos), "\n")
cat ("El numero de iteraciones realizadas en el metodo Aitken son: ", iteraciones)

