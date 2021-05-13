library(stats)
library(polynom)



par(mar=c(1,1,1,1)) #Redimensionar Pantalla del Gráfico
rm(list=ls()) #Borrar consola



#Datos
x = c( 5.07, 4.35, 3.20, 2.51, 2.23, 2.12, 2.04, 2.12, 2.29, 2.53, 3.53, #Orejita izquierda
       9.01, 9.01, 8.95, 8.79, 8.43, 7.88, 6.91, 5.98, 5.32, 5.07, 4.93, 4.63, #cabecita
       4.30, 3.97, 3.83, 3.80, 3.94, 4.21, 4.82, 5.21, 5.26, 5.22, 4.30, #Nariz
       3.80, 3.53, 3.50, 3.47, 3.64, 3.97, 4.24, 4.68, #Mitad hocico
       4.35, 4.41, 4.52, 4.68, 5.12, 5.45, 5.81, 6.14, 6.42,6.58, 6.94, 7.27, #Mitad hocico
       6.94, 6.86, 6.78, 6.67, 6.50, 6.36, 6.30, 5.79, 5.51, 5.19, 5.12, 5.14,5.45, #Lengua
       4.88, 5.14, # Parte boca
       6.36, 6.94, 7.19,7.33,#Parte boca
       8.79, 9.78,10.41,10.39,10.06,9.26,8.90,8.76,8.46,8.29,7.71, #Orejita derecha
       8.24, 7.91, 7.74, #Parte Cara
       7.11, 7.88, 7.89, 6.92, #Collar
       7.79, 9.56,10.72,11.87,12.53,13.25,13.50,13.42,13.11,12.45, # cuerpo superior
       4.99, 4.49, 4.41, 4.44, 4.60, 4.88, 5.32, 5.95, 6.50, 8.26, 8.93, 9.53, # Cuerpo Inferior
       4.60, 4.44, 4.30, 4.24, 5.95, 5.84, 5.88, 4.55, 4.24,3.75,3.31,3.30,3.72, 3.64, 3.94, 3.75,4.49,4.38,4.66,4.49,5.21, #Patita 1
       6.78, 6.83,6.50,5.88,8.40,8.43,8.29,8.13,5.88,5.26,5.10,5.21,5.87,5.76,6.12,5.87,6.56,6.50,6.72,6.56,7.19,7.80, #Patita 2
       7.80,7.70,7.82,8.35,10.28,7.80,8.40,8.32,8.60,8.40,9.15,9.04,9.42,9.15,9.70,10.19,11.40,12.37,12.42,12.23, #Patita3
       13.25,13.50,13.39,13.17,12.95,13,13.44,14.21,15.15, #Cola arriba
       14.63,14.24,14.10,14.02,14.33,14.16,14.35,14.19,13.99,13.50, #ColaAbajo
       6.61,6.97,7.13,7.27,7,6.66,6.44,6.33,6.42,6.61, #Ojo Derecho
       4.74,5.62, #Nariz 2
       5.84,5.89,5.81,5.59,5.34,5.18,5.12,5.21, #Ojo Izquierdo
       10.28,8.90,8.79,9.23,9.81,10.69, #Patita 3
       6.36,6.53,6.78,6.83,6.80,5.89,
       5.73,5.54,5.32,5.23
)
y = c(17.53,17.32,17.64,17.32,17.07,16.86,16.39,15.96,15.71,15.46,15.32, #Orejita izquierda
      16.61,17.07,17.32,17.90,18.45,18.88,19.18,18.89,18.21,17.53,16.79,16.21, #cabecita
      16.22,16.19,16.11,15.86,15.43,14.96,15, 15.36,15.57,15.79,16.19, #Nariz
      15.86,15.32, 15.05,14.79,14.21,13.82,13.68,13.57, #Mitad hocico
      14.89,14.35,13.90,13.57,13.32,13.21,13.18,13.39,13.68,14,14.39,14.96, #Mitad hocico
      14.39,13.75,13.11,12.64,12.21,11.29,11.11,10.64,10.69,11,11.38,12,13.21,#Lengua
      13.43,12, #Parte boca
      11.29,11.93,12.71,13.25,#Parte boca
      16.79,15.32,13.75,12.81,12.14,11.96,12.21,12.57,13.5,14.39,15.82, #Orejita derecha
      14.61,13.75,12.43, #Parte Cara
      12.32,12.43,11.89,11.73, #Collar
      11.89,9.39, 8.36, 7.68,7.03,5.89,4.81,4.14,3.04,2.21, #cuerpo superior
      11.61,10.46,9.82,9.11,7.71,6.93,5.93,5.14,4.61,3.11, 2.68, 2.43, #Cuerpo Inferior
      7.71,6.11, 4.11,2.57,5.14,3.71,2.32,2.5, 2.57,2.29,1.82,1,0.75, 1.39, 1.93, 0.75,0.61,1.25,1.79,0.61,0.68, #Patita 1
      9.11,6.5,4.61,2.32,7.60,6.11,4.15,2.25,2.32,1.68,1.17,0.68,0.64,1.21,1.68,0.64,0.57,1.04,1.5,0.57,0.5,0.71, #Patita 2
      0.71,1.21, 1.86,2.18,2.32,0.71,0.64,1.04,1.68,0.64,0.61,1.04,1.64,0.61,0.53,0.68,0.86,1.29,1.79,2.5, #Patita 3
      5.89,6.46, 7.18,8.18,9.21,10.29,11.46,12.10,12.32, #Cola Arriba
      11.64,11.11,10.43,9.75,7.71,9.24,6.89,5.89,5.43,4.81, #Cola Abajo
      15.61,15.75,16.21,16.79,17.32,17.25,16.96,16.36,15.86,15.61, #Ojo derecho
      16,16.46, #Nariz 2
      16.96,17.29,17.64,17.79,17.5,17.14,16.79,16.39, #Ojo Izquierdo
      2.32,4.36,5.39,6.46,7,6.89, #Patita 3
      16.57,16.71,16.46,16.14,15.86,17.29,
      16.75,16.93,16.86,16.36
)

errores <- c()
windows() #Abrir una ventana nueva para el gráfico


plot(x,y,main = "Gráfica Silueta Perro", asp = 1, type='p',pch=20, lwd= 0.1, col="grey") #Realizar grafica con los puntos

#Realizar Interpolación (División de la silueta en 107 curvas)

#Oreja Izquierda
curva1x = c(x[1:5])
curva1y = c(y[1:5])
splines = splinefun(curva1x,curva1y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva1x[1], to = curva1x[length(curva1x)])

curva2x = c(x[5:6])
curva2y = c(y[5:6])
splines = splinefun(curva2x,curva2y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva2x[1], to = curva2x[length(curva2x)])

curva3x = c(x[6:7])
curva3y = c(y[6:7])
splines = splinefun(curva3x,curva3y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva3x[1], to = curva3x[length(curva3x)])

curva4x = c(x[7:8])
curva4y = c(y[7:8])
splines = splinefun(curva4x,curva4y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva4x[1], to = curva4x[length(curva4x)])

curva5x = c(x[8:11])
curva5y = c(y[8:11])
splines = splinefun(curva5x,curva5y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva5x[1], to = curva5x[length(curva5x)])

#Cabeza
curva6x = c(x[12:20])
curva6y = c(y[12:20])
splines = splinefun(curva6x,curva6y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva6x[1], to = curva6x[length(curva6x)])

curva7x = c(x[20:21])
curva7y = c(y[20:21])
splines = splinefun(curva7x,curva7y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva7x[1], to = curva7x[length(curva7x)])

curva8x = c(x[21:23])
curva8y = c(y[21:23])
splines = splinefun(curva8x,curva8y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva8x[1], to = curva8x[length(curva8x)])

curva9x = c(x[23:24])
curva9y = c(y[23:24])
splines = splinefun(curva9x,curva9y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva9x[1], to = curva9x[length(curva9x)])

#Nariz
curva10x = c(x[24:26])
curva10y = c(y[24:26])
splines = splinefun(curva10x,curva10y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva10x[1], to = curva10x[length(curva10x)])

curva11x = c(x[26:27])
curva11y = c(y[26:27])
splines = splinefun(curva11x,curva11y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva11x[1], to = curva11x[length(curva11x)])

curva12x = c(x[27:29])
curva12y = c(y[27:29])
splines = splinefun(curva12x,curva12y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva12x[1], to = curva12x[length(curva12x)])

curva13x = c(x[29:31])
curva13y = c(y[29:31])
splines = splinefun(curva13x,curva13y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva13x[1], to = curva13x[length(curva13x)])

curva14x = c(x[31:32])
curva14y = c(y[31:32])
splines = splinefun(curva14x,curva14y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva14x[1], to = curva14x[length(curva14x)])

curva15x = c(x[32:33])
curva15y = c(y[32:33])
splines = splinefun(curva15x,curva15y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva15x[1], to = curva15x[length(curva15x)])

curva16x = c(x[33:34])
curva16y = c(y[33:34])
splines = splinefun(curva16x,curva16y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva16x[1], to = curva16x[length(curva16x)])

#Mitad hocico 1
curva17x = c(x[35:36])
curva17y = c(y[35:36])
splines = splinefun(curva17x,curva17y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva17x[1], to = curva17x[length(curva17x)])

curva18x = c(x[36:38])
curva18y = c(y[36:38])
splines = splinefun(curva18x,curva18y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva18x[1], to = curva18x[length(curva18x)])

curva19x = c(x[38:42])
curva19y = c(y[38:42])
splines = splinefun(curva19x,curva19y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva19x[1], to = curva19x[length(curva19x)])

# Mitad Hocico 2
curva20x = c(x[43:46])
curva20y = c(y[43:46])
splines = splinefun(curva20x,curva20y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva20x[1], to = curva20x[length(curva20x)])

curva21x = c(x[46:52])
curva21y = c(y[46:52])
splines = splinefun(curva21x,curva21y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva21x[1], to = curva21x[length(curva21x)])

curva22x = c(x[52:54])
curva22y = c(y[52:54])
splines = splinefun(curva22x,curva22y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva22x[1], to = curva22x[length(curva22x)])


#Lengua
curva23x = c(x[55:59])
curva23y = c(y[55:59])
splines = splinefun(curva23x,curva23y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva23x[1], to = curva23x[length(curva23x)])

curva24x = c(x[59:60])
curva24y = c(y[59:60])
splines = splinefun(curva24x,curva24y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva24x[1], to = curva24x[length(curva24x)])

curva25x = c(x[60:61])
curva25y = c(y[60:61])
splines = splinefun(curva25x,curva25y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva25x[1], to = curva25x[length(curva25x)])

curva26x = c(x[61:64])
curva26y = c(y[61:64])
splines = splinefun(curva26x,curva26y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva26x[1], to = curva26x[length(curva26x)])

curva27x = c(x[64:65])
curva27y = c(y[64:65])
splines = splinefun(curva27x,curva27y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva27x[1], to = curva27x[length(curva27x)])

curva28x = c(x[65:66])
curva28y = c(y[65:66])
splines = splinefun(curva28x,curva28y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva28x[1], to = curva28x[length(curva28x)])

curva29x = c(x[66:67])
curva29y = c(y[66:67])
splines = splinefun(curva29x,curva29y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva29x[1], to = curva29x[length(curva29x)])

curva30x = c(x[68:69])
curva30y = c(y[68:69])
splines = splinefun(curva30x,curva30y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva30x[1], to = curva30x[length(curva30x)])

curva31x = c(x[70:73])
curva31y = c(y[70:73])
splines = splinefun(curva31x,curva31y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva31x[1], to = curva31x[length(curva31x)])

#Orejita derecha
curva32x = c(x[74:76])
curva32y = c(y[74:76])
splines = splinefun(curva32x,curva32y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva32x[1], to = curva32x[length(curva32x)])

curva33x = c(x[76:77])
curva33y = c(y[76:77])
splines = splinefun(curva33x,curva33y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva33x[1], to = curva33x[length(curva33x)])

curva34x = c(x[77:82])
curva34y = c(y[77:82])
splines = splinefun(curva34x,curva34y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva34x[1], to = curva34x[length(curva34x)])

curva35x = c(x[82:84])
curva35y = c(y[82:84])
splines = splinefun(curva35x,curva35y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva35x[1], to = curva35x[length(curva35x)])

#Parte Cara
curva36x = c(x[85:87])
curva36y = c(y[85:87])
splines = splinefun(curva36x,curva36y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva36x[1], to = curva36x[length(curva36x)])

#Collar
curva37x = c(x[88:89])
curva37y = c(y[88:89])
splines = splinefun(curva37x,curva37y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva37x[1], to = curva37x[length(curva37x)])

curva38x = c(x[89:90])
curva38y = c(y[89:90])
splines = splinefun(curva38x,curva38y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva38x[1], to = curva38x[length(curva38x)])

curva39x = c(x[90:91])
curva39y = c(y[90:91])
splines = splinefun(curva39x,curva39y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva39x[1], to = curva39x[length(curva39x)])

#Parte superior cuerpo
curva40x = c(x[92:95])
curva40y = c(y[92:95])
splines = splinefun(curva40x,curva40y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva40x[1], to = curva40x[length(curva40x)])

curva41x = c(x[95:97])
curva41y = c(y[95:97])
splines = splinefun(curva41x,curva41y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva41x[1], to = curva41x[length(curva41x)])

curva42x = c(x[97:98])
curva42y = c(y[97:98])
splines = splinefun(curva42x,curva42y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva42x[1], to = curva42x[length(curva42x)])

curva43x = c(x[98:100])
curva43y = c(y[98:100])
splines = splinefun(curva43x,curva43y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva43x[1], to = curva43x[length(curva43x)])

curva44x = c(x[100:101])
curva44y = c(y[100:101])
splines = splinefun(curva44x,curva44y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva44x[1], to = curva44x[length(curva44x)])

#parte inferior
curva45x = c(x[102:103])
curva45y = c(y[102:103])
splines = splinefun(curva45x,curva45y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva45x[1], to = curva45x[length(curva44x)])

curva46x = c(x[103:104])
curva46y = c(y[103:104])
splines = splinefun(curva46x,curva46y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva46x[1], to = curva46x[length(curva46x)])

curva47x = c(x[104:110])
curva47y = c(y[104:110])
splines = splinefun(curva47x,curva47y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva47x[1], to = curva47x[length(curva47x)])

curva48x = c(x[111:113])
curva48y = c(y[111:113])
splines = splinefun(curva48x,curva48y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva48x[1], to = curva48x[length(curva48x)])

#patita 1
curva49x = c(x[114:117])
curva49y = c(y[114:117])
splines = splinefun(curva49x,curva49y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva49x[1], to = curva49x[length(curva49x)])

curva50x = c(x[118:120])
curva50y = c(y[118:120])
splines = splinefun(curva50x,curva50y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva50x[1], to = curva50x[length(curva50x)])

curva51x = c(x[121:124])
curva51y = c(y[121:124])
splines = splinefun(curva51x,curva51y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva51x[1], to = curva51x[length(curva51x)])

curva52x = c(x[124:125])
curva52y = c(y[124:125])
splines = splinefun(curva52x,curva52y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva52x[1], to = curva52x[length(curva52x)])

curva53x = c(x[125:126])
curva53y = c(y[125:126])
splines = splinefun(curva53x,curva53y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva53x[1], to = curva53x[length(curva53x)])

curva54x = c(x[126:127])
curva54y = c(y[126:127])
splines = splinefun(curva54x,curva54y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva54x[1], to = curva54x[length(curva54x)])

curva55x = c(x[127:128])
curva55y = c(y[127:128])
splines = splinefun(curva55x,curva55y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva55x[1], to = curva55x[length(curva55x)])

curva56x = c(x[129:130])
curva56y = c(y[129:130])
splines = splinefun(curva56x,curva56y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva56x[1], to = curva56x[length(curva56x)])

curva57x = c(x[130:131])
curva57y = c(y[130:131])
splines = splinefun(curva57x,curva57y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva57x[1], to = curva57x[length(curva57x)])

curva58x = c(x[131:132])
curva58y = c(y[131:132])
splines = splinefun(curva58x,curva58y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva58x[1], to = curva58x[length(curva58x)])

curva59x = c(x[133:134])
curva59y = c(y[133:134])
splines = splinefun(curva59x,curva59y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva59x[1], to = curva59x[length(curva59x)])

#Patitas 1-2
curva60x = c(x[135:136])
curva60y = c(y[135:136])
splines = splinefun(curva60x,curva60y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva60x[1], to = curva60x[length(curva60x)])

curva61x = c(x[136:138])
curva61y = c(y[136:138])
splines = splinefun(curva61x,curva61y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva61x[1], to = curva61x[length(curva61x)])

curva62x = c(x[139:140])
curva62y = c(y[139:140])
splines = splinefun(curva62x,curva62y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva62x[1], to = curva62x[length(curva62x)])

curva63x = c(x[140:142])
curva63y = c(y[140:142])
splines = splinefun(curva63x,curva63y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva63x[1], to = curva63x[length(curva63x)])

curva64x = c(x[143:144])
curva64y = c(y[143:144])
splines = splinefun(curva64x,curva64y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva64x[1], to = curva64x[length(curva64x)])

curva65x = c(x[144:145])
curva65y = c(y[144:145])
splines = splinefun(curva65x,curva65y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva65x[1], to = curva65x[length(curva65x)])

curva66x = c(x[145:146])
curva66y = c(y[145:146])
splines = splinefun(curva66x,curva66y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva66x[1], to = curva66x[length(curva66x)])

curva67x = c(x[146:147])
curva67y = c(y[146:147])
splines = splinefun(curva67x,curva67y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva67x[1], to = curva67x[length(curva67x)])

curva68x = c(x[147:148])
curva68y = c(y[147:148])
splines = splinefun(curva68x,curva68y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva68x[1], to = curva68x[length(curva68x)])

curva69x = c(x[148:149])
curva69y = c(y[148:149])
splines = splinefun(curva69x,curva69y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva69x[1], to = curva69x[length(curva69x)])

curva70x = c(x[150:151])
curva70y = c(y[150:151])
splines = splinefun(curva70x,curva70y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva70x[1], to = curva70x[length(curva70x)])

curva71x = c(x[151:152])
curva71y = c(y[151:152])
splines = splinefun(curva71x,curva71y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva71x[1], to = curva71x[length(curva71x)])

curva72x = c(x[152:153])
curva72y = c(y[152:153])
splines = splinefun(curva72x,curva72y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva72x[1], to = curva72x[length(curva72x)])

curva73x = c(x[152:153])
curva73y = c(y[152:153])
splines = splinefun(curva73x,curva73y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva73x[1], to = curva73x[length(curva73x)])

curva74x = c(x[154:156])
curva74y = c(y[154:156])
splines = splinefun(curva74x,curva74y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva74x[1], to = curva74x[length(curva74x)])

#Patita 3
curva75x = c(x[157:158])
curva75y = c(y[157:158])
splines = splinefun(curva75x,curva75y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva75x[1], to = curva75x[length(curva75x)])

curva76x = c(x[158:159])
curva76y = c(y[158:159])
splines = splinefun(curva76x,curva76y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva76x[1], to = curva76x[length(curva76x)])

curva77x = c(x[159:160])
curva77y = c(y[159:160])
splines = splinefun(curva77x,curva77y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva77x[1], to = curva77x[length(curva77x)])

curva78x = c(x[160:161])
curva78y = c(y[160:161])
splines = splinefun(curva78x,curva78y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva78x[1], to = curva78x[length(curva78x)])

curva79x = c(x[162:163])
curva79y = c(y[162:163])
splines = splinefun(curva79x,curva79y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva79x[1], to = curva79x[length(curva79x)])

curva80x = c(x[163:164])
curva80y = c(y[163:164])
splines = splinefun(curva80x,curva80y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva80x[1], to = curva80x[length(curva80x)])

curva81x = c(x[164:165])
curva81y = c(y[164:165])
splines = splinefun(curva81x,curva81y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva81x[1], to = curva81x[length(curva81x)])

curva82x = c(x[166:167])
curva82y = c(y[166:167])
splines = splinefun(curva82x,curva82y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva82x[1], to = curva82x[length(curva82x)])

curva83x = c(x[167:168])
curva83y = c(y[167:168])
splines = splinefun(curva83x,curva83y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva83x[1], to = curva83x[length(curva83x)])

curva84x = c(x[168:169])
curva84y = c(y[168:169])
splines = splinefun(curva84x,curva84y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva84x[1], to = curva84x[length(curva84x)])

curva85x = c(x[170:172])
curva85y = c(y[170:172])
splines = splinefun(curva85x,curva85y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva85x[1], to = curva85x[length(curva85x)])

curva86x = c(x[172:174])
curva86y = c(y[172:174])
splines = splinefun(curva86x,curva86y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva86x[1], to = curva86x[length(curva86x)])

curva87x = c(x[174:175])
curva87y = c(y[174:175])
splines = splinefun(curva87x,curva87y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva87x[1], to = curva87x[length(curva87x)])

curva88x = c(x[175:176])
curva88y = c(y[175:176])
splines = splinefun(curva88x,curva88y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva88x[1], to = curva88x[length(curva88x)])

#Cola
curva89x = c(x[177:178])
curva89y = c(y[177:178])
splines = splinefun(curva89x,curva89y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva89x[1], to = curva89x[length(curva89x)])


curva90x = c(x[178:180])
curva90y = c(y[178:180])
splines = splinefun(curva90x,curva90y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva90x[1], to = curva90x[length(curva90x)])

curva91x = c(x[180:181])
curva91y = c(y[180:181])
splines = splinefun(curva91x,curva91y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva91x[1], to = curva91x[length(curva91x)])

curva92x = c(x[181:182])
curva92y = c(y[181:182])
splines = splinefun(curva92x,curva92y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva92x[1], to = curva92x[length(curva92x)])

curva93x = c(x[182:185])
curva93y = c(y[182:185])
splines = splinefun(curva93x,curva93y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva93x[1], to = curva93x[length(curva93x)])

#Cola abajo
curva94x = c(x[185:189])
curva94y = c(y[185:189])
splines = splinefun(curva94x,curva94y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva94x[1], to = curva94x[length(curva94x)])

curva95x = c(x[189:191])
curva95y = c(y[189:191])
splines = splinefun(curva95x,curva95y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva95x[1], to = curva95x[length(curva95x)])

curva96x = c(x[191:192])
curva96y = c(y[191:192])
splines = splinefun(curva96x,curva96y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva96x[1], to = curva96x[length(curva96x)])

curva97x = c(x[192:195])
curva97y = c(y[192:195])
splines = splinefun(curva97x,curva97y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva97x[1], to = curva97x[length(curva97x)])

#Ojo
curva98x = c(x[196:199])
curva98y = c(y[196:199])
splines = splinefun(curva98x,curva98y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva98x[1], to = curva98x[length(curva98x)])

curva99x = c(x[199:203])
curva99y = c(y[199:203])
splines = splinefun(curva99x,curva99y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva99x[1], to = curva99x[length(curva99x)])

curva100x = c(x[203:205])
curva100y = c(y[203:205])
splines = splinefun(curva100x,curva100y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva100x[1], to = curva100x[length(curva100x)])

curva101x = c(x[206:207])
curva101y = c(y[206:207])
splines = splinefun(curva101x,curva101y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva101x[1], to = curva101x[length(curva101x)])

curva102x = c(x[207:209])
curva102y = c(y[207:209])
splines = splinefun(curva102x,curva102y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva102x[1], to = curva102x[length(curva102x)])

curva103x = c(x[209:213])
curva103y = c(y[209:213])
splines = splinefun(curva103x,curva103y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva103x[1], to = curva103x[length(curva103x)])

curva104x = c(x[213:215])
curva104y = c(y[213:215])
splines = splinefun(curva104x,curva104y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva104x[1], to = curva104x[length(curva104x)])

#Patica
curva105x = c(x[216:217])
curva105y = c(y[216:217])
splines = splinefun(curva105x,curva105y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva105x[1], to = curva105x[length(curva105x)])

curva106x = c(x[217:218])
curva106y = c(y[217:218])
splines = splinefun(curva106x,curva106y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva106x[1], to = curva106x[length(curva106x)])

curva107x = c(x[218:221])
curva107y = c(y[218:221])
splines = splinefun(curva107x,curva107y, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = curva107x[1], to = curva107x[length(curva107x)])

#Pupilas
curva108x = c(x[222:225])
curva108y = c(y[222:225])
splines = splinefun(curva108x,curva108y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva108x[1], to = curva108x[length(curva108x)])

curva109x = c(x[225:226])
curva109y = c(y[225:226])
splines = splinefun(curva109x,curva109y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva109x[1], to = curva109x[length(curva109x)])

curva110x = c(x[227:231])
curva110y = c(y[227:231])
splines = splinefun(curva110x,curva110y, method = "fmm")
curve(splines(x), add = TRUE, col = "black", from = curva110x[1], to = curva110x[length(curva110x)])

###########################################
f <- poly.calc(c(x[177:178]), c(y[177:178]))
f = as.function(f)
x <- 13.47107
valorEsperado = 6.714286
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

f <- poly.calc(c(x[178:180]), c(y[178:180]))
f = as.function(f);
x <- 13.33333
valorEsperado = 7.571429
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

f <- poly.calc(c(x[180:181]), c(y[180:181]))
f = as.function(f);
x <- 12.92011
valorEsperado = 9.607142
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

f <- poly.calc(c(x[181:182]), c(y[181:182]))
f = as.function(f);
x <- 13.19559
valorEsperado = 10.78571
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

f <- poly.calc(c(x[182:185]), c(y[182:185]))
f = as.function(f);
x <- 13.69146
valorEsperado = 11.75
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

f <- poly.calc(c(x[185:189]), c(y[185:189]))
f = as.function(f);
x <- 14.54545
valorEsperado = 12.28571
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

f <- poly.calc(c(x[189:191]), c(y[189:191]))
f = as.function(f);
x <- 14.82094
valorEsperado = 11.96429
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

f <- poly.calc(c(x[191:192]), c(y[191:192]))
f = as.function(f);
x <- 14.18733
valorEsperado = 10.85714
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

f <- poly.calc(c(x[192:195]), c(y[192:195]))
f = as.function(f);
x <- 14.10468
valorEsperado = 9.642858
valor = f(x)
error =  abs((valor - valorEsperado)) / valorEsperado #Error Relativo
errores <- c(errores,error)

suma <- 0
for (i in 1:length(errores)){
   suma <- suma + errores[i]
}
promedio = suma/length(errores)
cat ("El error promedio de la interpolación cúbica por medio de splines es: ", promedio)