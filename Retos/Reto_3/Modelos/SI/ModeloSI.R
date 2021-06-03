dev.off() # Desactivamos todas las ventanas gráficas o dispositivos
par(mar=c(1,1,1,1)) #Redimensionar Pantalla del Gráfico
rm(list=ls()) #Borrar consola


x11() # Abrimos el primer dispositivo

Conf2mas1 = matrix(c(3,1,3,2), nrow=2, byrow=F) # Creamos un matriz a partir de un vector con los valores c(1:3,3) que es igual que c(1,2,3,3)

layout(Conf2mas1)

library(deSolve)

Beta = 18.5
susceptible = 48223786 + 197
infected = 34709
hours = 14
partitions = 1

SI <- function(t, x, parametros){
  with(as.list(c(parametros, x)),{
    dS <- - tasa*Susceptibles*Infectados
    dI <- + tasa*Susceptibles*Infectados
    derivadas <- c(dS, dI)
    return(list(derivadas))
  })
} 


parametros <- c(tasa <-Beta)

valores_iniciales <- c(Susceptibles = susceptible, Infectados = infected)

dt <- seq(0, hora <- hours, particiones <- partitions)

solucion <- ode(y=valores_iniciales, times=dt, func=SI,parms=parametros, method = "euler")

N <- sum(valores_iniciales)
cbind(dt, round(solucion[,2],1))

tabla = cbind(dt, round((solucion[,2]*100/N),1), round((solucion[,3]*100/N),1))
colnames(tabla) = c("Tiempo", "Susceptibles(% de poblacion)", "Infectados(% de poblacion)")
tabla

#Error
paramm <- c(beta=Beta, gamma = 0)

v_0 <- c(S=susceptible, I=infected, R = 0)

result = EpiDynamics::SIR(pars = paramm, init = v_0, time = dt)


#Plot errores Susceptibles
valoresODESuceptibles = c(solucion[,2])
valoresEpiSuceptibles = c(result$results[,2])
errores = c()
i = 1
max = length(valoresODESuceptibles)
while(i <= max){
  errorActual =(abs(valoresODESuceptibles[i] - valoresEpiSuceptibles[i] )/valoresEpiSuceptibles[i]) 
  errores[i] = errorActual
  i = i + 1
}
plot(dt, errores, type="l", col= "red", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
title("Error relativo para Susceptibles")

#Plot errores Infectados
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

#Tabla Errores Infectados
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

#############
result = SIR(pars = paramm, init = v_0, time = dt)

simulacion.si <- as.data.frame(solucion)
attach(simulacion.si)
N <- sum(valores_iniciales)
plot(dt, solucion[,2], type="l", col="blue", ylim=c(0,sum(valores_iniciales)),xlab="Tiempo (en Horas)", ylab="Numero de individuos")
lines(dt, solucion, type="l", col="red")
lines(dt, result$results[,2], type = "l", col = "brown")
lines(dt, result$results[,3], type = "l", col = "orange")
title("Modelo SI:")
legend(hours/2, N/1.5,
       legend=c("Susceptibles por ODE", "Infectados por ODE", "Susceptibles por EpiDynamics", "Infectados por EpiDynamics"), col=c("blue", "red", "brown", "orange"), lty=rep(1, 2))

 