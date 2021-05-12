library(pragma)
library(cmna)
library(gaussquad)
library(Rmpfr)

#Función a analizar
f<-function(x) {exp(-x^2)}

derivar <- function(i){
  fprima<-expression(exp(-x^2))
  for(j in 1:i){
    fprima<-D(fprima,"x")
  }
  return(fprima)
}


#Función de la Regla del Trapecio
trapecios <- function (f, a, b, n, tol){
  h<- (b-a)/n
  
  s<-0
  e <- -(h^3)/12
  for(i in (1:(n-1))){
    derivada = derivar(2)
    fun = function(x) eval(derivada)
    error = e * fun(s)
    if(error<tol){
      s<- s + f(a+ (i*h))
    } 
  }
  valor_integral <- (f(a) +f(b)+ 2*s)* (h/2)
  
  return (valor_integral)
}


#Función de la Regla de Simpson
simpson <- function(f, a, b, n){
  if(n%%2 != 0) stop("En la regla de Simpson, n es par!")
  h = (b-a)/n
  i1 = seq(1, n-1, by = 2) # impares
  i2 = seq(2, n-2, by = 2) # pares
  y = f(a+(0:n)*h) # f(a), f(a+h),...,f(a+i*h),...
  h/3 * ( f(a) + f(b) + 4*sum(y[i1]) + 2*sum(sum(y[i2]) ) )
}

#Función de la Regla de Simpson 3/8
simpson3sobre8 <- function(f, a, b, n){
  i1 = ((2*a)+b)/3
  i2 = (a+(2*b))/3
  
  integral = (b-a)/8 * (f(a)+3*f(i1)+3*f(i2)+f(b))
  
  return(integral)
}

cuadratura <- function(f, a, b){
  Lq = legendre.quadrature.rules(10)[[10]] #Nodos y pesos
  # xi = Lq$x; wi = Lq$w
  legendre.quadrature(f, Lq, lower = a, upper = b)
  
}

valor <- trapecios(f,0,1.5,10000, 0.5e-5)
valor = valor*(2/sqrt(pi))
print(valor, digits=20)

valor2 <- simpson(f,0,1.5,10000)
valor2 = valor2*(2/sqrt(pi))
print(valor2, digits=20)

valor3 <- simpson3sobre8(f, 0, 1.5)
valor3 = valor3*(2/sqrt(pi))
print(valor3, digits=20)

valor4 <- cuadratura(f, 0, 1.5)
valor4 = valor4*(2/sqrt(pi))
print(valor4, digits=20)