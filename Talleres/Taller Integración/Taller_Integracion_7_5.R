library(polynom)
library(pragma)
library(cmna)
x<- c(0, 0.2, 0.4, 0.6, 0.8)
y <- c(3.592, 3.110, 3.017, 2.865, 2.658)

#Función de la Regla del Trapecio
trapecios <- function (f, a, b, n){
  h<- (b-a)/n
  
  s<-0
  for(i in (1:(n-1))){
    s<- s + f(a+ (i*h))
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

#Se encuentra el polinomio con poly.calc 
pol <- poly.calc(x, y)
polFun = as.function(pol);


valor <- trapecios(polFun,0,0.8,10000)
print(valor, digits=20)

valor2 <- simpson(polFun,0,0.8,10000)
print(valor2, digits=20)

romberg(polFun, 0, 0.8, 10, tab=FALSE)