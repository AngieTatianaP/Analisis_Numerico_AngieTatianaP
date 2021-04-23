


incognitas<-matrix(c(4410000^2, 4410000, 1,
                     4830000^2, 4830000, 1, 
                     5250000^2, 5250000, 1),3, 3, byrow=TRUE)

resultados<- c(1165978,1329190, 1501474)
gaussianElimination(incognitas, resultados)

mCuadratico <- gaussianElimination(incognitas, resultados)


cat(mCuadratico[1,4], "x^2 + (", mCuadratico[2,4], ")x + (", mCuadratico[3,4], ")")

#----------------------------------------------------------------------------------------

incognitasCu<-matrix(c(4410000^3, 4410000^2, 4410000, 1,
                     4830000^3, 4830000^2, 4830000,1, 
                     5250000^3, 5250000^2, 5250000, 1, 
                     5670000^3, 5670000^2, 5670000, 1),4, 4, byrow=TRUE)

resultadosCu<- c(1165978,1329190, 1501474, 1682830)

gaussianElimination(incognitasCu, resultadosCu)

mCubico <- gaussianElimination(incognitasCu, resultadosCu)


cat(mCubico[1,5], "x^3 + (", mCubico[2,5], ")x^2 + (", mCubico[3,5], ")x + (", mCubico[4,5], ")")

#---------------------------------------------------------------------------------------

par(mar=c(1,1,1,1))

x<-c(4410000, 4830000, 5000000, 5250000, 5670000)

y<-c(1165978, 1329190, 1397831, 1501474, 1682830)

windows()

plot(x,y,main = "Gráfica Reporte", asp = 1,  type='o', col="blue",
     xlab = "Base Imponible", ylab = "Cuota Íntegra", axes = TRUE)

lines(x,y, lwd = 2, col = "red")

#----------------------------------------------------------------------------------------

par(mar=c(1,1,1,1))

windows()

curve((3e-08)*x^2+(0.151)*x-26, main = "Gráfica Reporte", from = 4410000, to= 5670000,
      type = "l", col ="blue", xlab = "Base Imponible", ylab ="Cuota Íntegra", axes = TRUE, 
      xlim = c(4400000,5700000), ylim = c(1000000, 2000000))

