  library(Rmpfr)
  
  f <- function(x)(((cos(x)^2))-x^2) #Funcion
  
  #Creacion del vector que guardara la sucesion
  iniciales <- c()
  
  #Definir el intervalo
  x0 <- 0.0
  x1 <- 1.0
  
  # Por medio del metodo de biseccion obtenemos el valor de x2
  x2 <- (x0 + x1)/2
  
  # Asignamos el valor de la tolerancia
  tol <- 1e-5
  
  # Obtenemos el número máximo de iteraciones
  N <- (1/log(2))*(log(abs(x0-x1)/tol))
  h1 <- x1-x0
  h2 <- x2-x1
  d1 <- (f(x1)-f(x0))/h1
  d2 <- (f(x2)-f(x1))/h2
  a <- (d2-d1)/h2+h1
  i <- 0
  cat(tol, "\n")
  iniciales <- c()
  valida <- TRUE
  #Ciclo que se repite hasta el número de máximo de iteraciones
  while(valida == TRUE){
    b <- d2+h2*a
    D<-(b^2-4*(f(x2)*a))^(1/2)
    #Validación para la asignación del E
    if (b > 0){E<-b+D}
    if (b < 0){E<-b-D}
    h <- 2*f(x2)/E
    result <- x2+h
    cat(result, "\n")
    iniciales <- c(iniciales,result)
    #Condicional que para el programa cuando el valor sea menor a la toleracia
    if (abs((result-x1)/result)<tol){
      valida == FALSE
      result
     break
    }
    
    #Asignación de las variables 
    x0 <- x1
    x1 <- x2
    x2 <- result
    h1 <- x1-x0
    d1 <- (f(x1)-f(x0))/h1
    d2 <- (f(x2)-f(x1))/h2
    a <-(d2-d1)/h2+h1
    i <- i+1
  }
  
  # Analizar convergencia
  lineal <- FALSE
  for (t in 1:i){
    if (iniciales[t+1] == 0.5*(iniciales[t])){
      lineal <- TRUE
    } else {
      lineal <- FALSE
    }
  }
  if (lineal == TRUE){
    cat("La convergencia es lineal")
  } else {cat("La convergencia NO es lineal")}
  
  cat("Usando precisión de la función mpfr obtenemos como resultado: \n")
  mpfr(result, 500)
  
