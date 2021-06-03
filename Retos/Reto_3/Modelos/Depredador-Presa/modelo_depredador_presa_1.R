dev.off() # Desactivamos todas las ventanas gráficas o dispositivos
par(mar=c(1,1,1,1)) #Redimensionar Pantalla del Gráfico
rm(list=ls()) #Borrar consola


x11() # Abrimos el primer dispositivo

Conf2mas1 = matrix(c(1,3,2,4), nrow=2, byrow=F) # Creamos un matriz

layout(Conf2mas1)

library(deSolve)

LotVmod <- function (Tiempos, estado, Parm) {
  with(as.list(c(estado, Parm)), {
    dx = x*(alpha - beta*y)#ecuacion 1
    dy = -y*(gamma - delta*x)#ecuacion 2
    return(list(c(dx, dy)))#sistema
  })
}
mod <- function (Tiempos, estado, Parm) {
  with(as.list(c(estado, Parm)), {
    dx = x*(alpha - beta*y)#ecuacion 1
    dy = -y*(gamma - delta*x)#ecuacion 2
    return(list(c(dx, dy)))#sistema
  })
}
#windows()
#Ejemplo
Parm <- c(alpha = 0.4, beta = 0.018, gamma = 0.8, delta = 0.023)
estado <- c(x = 30, y = 4)#estado inicial
Tiempos <- seq(0, 20, by = 1)#tiempo

out <- as.data.frame(ode(func = LotVmod,
                         y = estado, 
                         parms = Parm, 
                         times = Tiempos))

matplot(out[,-1], type = "l",
        xlab = "Tiempo", ylab = "Población")
legend("top", c("Conejos:presa", "Linces:depredador"),
       lty = c(1,2), col = c(1,2), box.lwd = 0)

#####################

library(phaseR)
lotkaVolterra <- function(t, y, parameters) {
  x <- y[1]
  y <- y[2]
  lambda <- parameters[1]
  epsilon <- parameters[2]
  eta <- parameters[3]
  delta <- parameters[4]
  dy <- numeric(2)
  dy[1] <- lambda*x - epsilon*x*y
  dy[2] <- eta*x*y - delta*y
  list(dy)
}

lotkaVolterra.flowField <- flowField(lotkaVolterra,
                                     xlim = c(0, 5),
              ylim = c(0, 8),
               parameters = c(2, 1, 3, 6),
              points = 19, add = FALSE)
grid()
lotkaVolterra.nullclines <- nullclines(lotkaVolterra,
               xlim = c(-1, 5), ylim = c(-1, 8),
               parameters = c(2, 1, 3, 6),
               points = 500)
y0 <- matrix(c(1, 2, 2, 1, 4, 5), ncol = 2, nrow = 3,
             byrow = TRUE)
lotkaVolterra.trajectory <- trajectory(lotkaVolterra,
                                       y0 = y0,
                                       tlim = c(0,10),
                                       parameters = c(2, 1, 3, 6),
             col = rep("purple", 3))

x_reales <- c(30, 41.94538, 58.13304, 73.88688, 77.24295, 48.96452, 22.33082, 13.21147, 11.17702, 11.96950, 14.81758, 19.04755, 26.67699, 36.19183, 50.92922, 68.18433, 79.41561, 63.21616, 30.33528, 15.67904, 11.59387 )
y_reales <- c(4, 4.08157, 5.83161, 11.54430, 31.59323, 62.69670, 61.35243, 40.38283, 23.81268, 13.93059, 8.31745, 5.70828, 4.22040, 3.90746, 4.791611, 8.53165, 21.60282, 51.44460, 66.54528, 49.25306, 30.15441)
errores_x=c()
errores_y=c()
for(i in 1:21){
  # cat(out[i,2], '\n')
  errorActual_x = (abs(out[i,2] - x_reales[i] )/x_reales[i])
  errores_x[i] = errorActual_x
  
  errorActual_y = (abs(out[i,3] - y_reales[i] )/y_reales[i])
  errores_y[i] = errorActual_y
}
dt <- seq(0, 20, 1)
plot(dt, errores_x, type="l", col= "green", xlab = "Tiempo (en años)", ylab = "Error relativo")
title("Error relativo para Presas")

dt <- seq(0, 20, 1)
plot(dt, errores_y, type="l", col= "orange", xlab = "Tiempo (en años)", ylab = "Error relativo")
title("Error relativo para Depredadores")

