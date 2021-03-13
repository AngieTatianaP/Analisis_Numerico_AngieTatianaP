library(pracma)
library(mi)
library(Rmpfr)

par(mar=c(1,1,1,1)) #Reajustar las configuraciones de Plot
Fx <- function(x) x^3 -2*x^2+(4/3)*x-(8/27) #Función

brentFun <- function(f,x0,x1,n,tol) #Declaración de la función Brent
{
  error <- 1 #Valor del error permitido
  errores <- c() #Vector para guardar los valores de los errores
  listaErrorAct = c() 
  listaErrorAnt = c()
  fx0 <- f(x0) #Guardar la función f evaluada en x0
  fx1 <- f(x1) #Guardar la función f evaluada en x1
  if(fx0 *fx1 >=0) #Validar que f(x0) * f(x1) >= 0
  {
    return("La función no está entre corchetes")
  }
  
  if(abs(fx0) < abs(fx1)) # Intercambiar los valores de x0 y x1 para que se cumplan las condiciones
  { 
    aux1 = x0
    x0 = x1
    x1 = aux1
    
    aux2 = fx0
    fx0 = fx1
    fx1 = aux2
  }
  x2 = x0
  fx2 =fx0
  bandera = TRUE
  i = 0
  d = 0
  
  while(i < n & abs(x1-x0) > tol) #Ciclo que se repite hasta la tolerancia deseada
  {
    errorAnterior = error
    
    fx0 = f(x0)
    fx1 = f(x1)
    fx2 = f(x2)
    
    if(fx0 != fx2 & fx1 != fx2)
    {
      op1 = (x0 * fx1 * fx2) / ((fx0 - fx1) *  (fx0 - fx2))
      op2 = (x1 * fx0 * fx2) / ((fx1 - fx0) * (fx1 - fx2))
      op3 = (x2 * fx0 * fx1) / ((fx2 - fx0) * (fx2 - fx1))
      s = op1 + op2 + op3
    }else
    {
      s = x1 - ((fx1 * (x1 - x0))/(fx1-fx0))
    }
    
    if((s < ((3 * x0 + x1)/4)|s>x1)|
       (bandera==TRUE & (abs(s - x1)) >= (abs(x1-x2)/2))|
       (bandera==FALSE & (abs(s-x1)) >= (abs(x2-d)/2))|
       (bandera==TRUE & (abs(x1-x2)) < tol)|
       (bandera==FALSE & (abs(x2-d)) < tol))
    {
      s = (x0 + x1)/2
      bandera = TRUE
    }else
    {
      bandera = FALSE
    }
    fs = f(s)
    d = x2
    x2 = x1
    
    if((fx0 * fs) < 0)
    {
      x1 = s
    }else
    {
      x0 = s
    }
    
    if(abs(fx0)<abs(fx1))
    {
      aux3 = x0
      x0 = x1
      x1 = aux3
    }
    
    error = abs(x1-x0)
    errores = c(errores,as.double(error))
    
    if(i>1)
    {
      listaErrorAnt = c(listaErrorAnt , as.double(errorAnterior))
      listaErrorAct = c(listaErrorAct, as.double(error))
    }
    
    i = i + 1
  }
  
  iteraciones <- c(1:i) 
  #Realizar Gráfico de los errores
  
  plot(iteraciones, errores, main = "Medicion del error Brent", xlab= "Iteraciones", ylab = "Errores", type = 'o', col="blue")
  plot(listaErrorAnt,listaErrorAct, main = "Relación de error Brent",xlab = " Error i ",ylab = " Error i+1 ",type= 'o',col ="red")
  retorno = c(x1,i)
  return(retorno)
}

#Declaración de la función Muller (La función se realizó anteriormente)
mullerFun <- function(f){
  f <- function(x)(x^3 - 2*x^2 + (4/3)*x - (8/27)) #Funci?n
  #Creaci?n del vector que guardara la sucesi?n
  iniciales <- c()
  
  #Definir el intervalo
  x0 <- 1.0
  x1 <- 0.0
  
  # Por medio del metodo de biseccion obtenemos el valor de x2
  x2 <- (x0 + x1)/2
  
  # Asignamos el valor de la tolerancia
  tol <- 1e-56
  
  # Obtenemos el n?mero m?ximo de iteraciones
  N <- (1/log(2))*(log(abs(x0-x1)/tol))
  h1 <- x1-x0
  h2 <- x2-x1
  d1 <- (f(x1)-f(x0))/h1
  d2 <- (f(x2)-f(x1))/h2
  a <- (d2-d1)/h2+h1
  i <- 0
  
  #Ciclo que se repite hasta el n?mero de m?ximo de iteraciones
  while(i< floor(N)){
    b <- d2+h2*a
    D<-(b^2-4*(f(x2)*a))^(1/2)
    if (b > 0){E<-b+D}
    if (b < 0){E<-b-D}
    h <- 2*f(x2)/E
    result <- x2+h
    iniciales <- c(iniciales,result)
    if (abs((result-x1)/result) < tol){result
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
  iniciales <- c(iniciales,i)
  return(iniciales)
}
#---------------------------------------------------------------------------#

#Gráfico de la función
x1 <- seq(-10,10,0.01)
plot (x1,Fx(x1),type="l",col="red", xlab = "x",ylab = "y")
abline(h=0,col="blue")


n = 1000
tol = 2^-50
a = 0.0
b = 5.0

cat ("Método de Brent para la función x^3 -2*x^2+(4/3)*x-(8/27)")

res = brentFun(Fx,a,b,n,tol)
cat ("Raiz:")
mpfr(res[1],168)
cat ("Número de iteraciones", res[2])

cat ("Método de Muller para la función x^3 -2*x^2+(4/3)*x-(8/27)")
Datos = mullerFun (Fx)
plot(Datos,
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(1, 15), type="o", col="#fc0345", lwd=1.5,xlab="iteración",ylab="", cex.axis= 0.8, cex.lab= 0.8)
title(main = "Müller: x³ - 2x² + 4/3x - 8/27", font.main=3)