dev.off() # Desactivamos todas las ventanas gráficas o dispositivos
par(mar=c(1,1,1,1)) #Redimensionar Pantalla del Gráfico
rm(list=ls()) #Borrar consola


x11() # Abrimos el primer dispositivo

Conf2mas1 = matrix(c(4,1,4,2,4,3), nrow=2, byrow = FALSE) # Creamos un matriz a partir de un vector con los valores c(1:3,3) que es igual que c(1,2,3,3)

layout(Conf2mas1)
layout.show(Conf2mas1)
library(deSolve)
library(EpiDynamics)

Beta = 18.5
Gamma = 1.5
susceptible = 48223786
infected = 34709
recovered = 197
hours = 14
partitions = 1

SIR <- function(t, x, parametros){
  with(as.list(c(parametros, x)),{
    dS <- -(tasaI*S*I)
    dI <- +(tasaI*S*I)- tasaR*I
    dR <- tasaR*I
    derivadas <- c(dS, dI, dR)
    return(list(derivadas))
  })
}

parametros <- c(tasaI= Beta, tasaR = Gamma)

valores_iniciales <- c(S=susceptible, I=infected, R =recovered)

dt <- seq(0, hours, partitions)

solucion = ode(y=valores_iniciales, times=dt, func=SIR,parms=parametros, method = "euler")

parametro <- c(beta= Beta, gamma = Gamma)
v_0 <- c(S=susceptible, I=infected, R =recovered)
result = EpiDynamics::SIR(pars = parametro, init = valores_iniciales, time = dt)


 N <- sum(valores_iniciales)

  #Plot error Susceptibles
  valoresODESusceptibles = c(solucion[,2])
  valoresEpiSusceptibles = c(result$results[,2])
  errores = c()
  i = 1
  max = length(valoresODESusceptibles)
  while(i <= max){
    errorActual =(abs(valoresODESusceptibles[i] - valoresEpiSusceptibles[i] )/valoresEpiSusceptibles[i]) 
    errores[i] = errorActual
    i = i + 1
  }
  plot(dt, errores, type="l", col= "red", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
  title("Error relativo para Susceptibles")
  
  #Plot error Infectados
  valoresODEInfectados = c(solucion[,3])
  valoresEpiInfectados = c(result$results[,3])
  errores = c()
  i = 1
  max = length(valoresODEInfectados)
  while(i <= max){
    errorActual = (abs(valoresODEInfectados[i] - valoresEpiInfectados[i] )/valoresEpiInfectados[i])
    errores[i] = errorActual
    i = i + 1
  }
  plot(dt, errores, type="l", col= "blue", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
  title("Error relativo para Infectados")
  
  #Plot error Recuperados
  valoresODERecuperados = c(solucion[,4])
  valoresEpiRecuperados = c(result$results[,4])
  errores = c()
  i = 1
  max = length(valoresODERecuperados)
  while(i <= max){
    errorActual = (abs(valoresODERecuperados[i] - valoresEpiRecuperados[i] )/valoresEpiRecuperados[i])
    errores[i] = errorActual
    i = i + 1
  }
  plot(dt, errores, type="l", col= "green", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
  title("Error relativo para Recuperados")

  
  #Tabla errores Susceptibles
  valoresODESusceptibles = c(solucion[,2])
  valoresEpiSusceptibles = c(result$results[,2])
  errores = c()
  i = 1
  max = length(valoresODESusceptibles)
  while(i <= max){
    errorActual =(abs(valoresODESusceptibles[i] - valoresEpiSusceptibles[i] )/valoresEpiSusceptibles[i]) 
    errores[i] = errorActual
    i = i + 1
  }
  tablaS = cbind(dt, errores)
  colnames(tablaS) = c("Tiempo", "Error relativo")
  tablaS
  
  #Tabla errores Infectados
  valoresODEInfectados = c(solucion[,3])
  valoresEpiInfectados = c(result$results[,3])
  errores = c()
  i = 1
  max = length(valoresODEInfectados)
  while(i <= max){
    errorActual = (abs(valoresODEInfectados[i] - valoresEpiInfectados[i] )/valoresEpiInfectados[i])
    errores[i] = errorActual
    i = i + 1
  }
  tablaI = cbind(dt, errores)
  colnames(tablaI) = c("Tiempo", "Error relativo")
  tablaI
  
  #Tabla errores Recuperados
  valoresODERecuperados = c(solucion[,4])
  valoresEpiRecuperados = c(result$results[,4])
  errores = c()
  i = 1
  max = length(valoresODERecuperados)
  while(i <= max){
    errorActual = (abs(valoresODERecuperados[i] - valoresEpiRecuperados[i] )/valoresEpiRecuperados[i])
    errores[i] = errorActual
    i = i + 1
  }
  tablaR = cbind(dt, errores)
  colnames(tablaR) = c("Tiempo", "Error relativo")
  tablaR
  
  #######
  simulacion.si <- as.data.frame(solucion)
  
  attach(simulacion.si)
  
  N <- sum(valores_iniciales)
  plot(dt, solucion[,2], type="l", col="blue", ylim=c(0,sum(valores_iniciales)), xlab = "Tiempo (horas)", ylab="Numero de individuos (en miles)")
  lines(dt, solucion[,3], type="l", col="red")
  lines(dt, solucion[,4], type="l", col="green")
  lines(dt, result$results[,2], type = "l", col = "brown")
  lines(dt, result$results[,3], type = "l", col = "orange")
  lines(dt, result$results[,4], type = "l", col = "purple")
  title("Modelo SIR")
  legend((hours)/2, N + 0.25, 
         legend=c("Susceptibles por ODE", "Infectados por ODE", "Recuperados por ODE", "Susceptibles por EpiDynamics", "Infectados por EpiDynamics", "Recuperados por por EpiDynamics"),
         col=c("blue", "red", "green","brown", "orange", "purple"), lty=rep(1, 2))
  
 