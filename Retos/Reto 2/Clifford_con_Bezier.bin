library(grid) 
library(gridBezier)
library(polynom)
library(PolynomF)

windows()
grid.newpage()

errores <- c() #Vector de Errores

x1 <- c(0.25, 0.27 , 0.5 , 0.48)
y1 <- c(0.83, 1 , 0.9 ,0.79)
grid.bezier(x1,y1,gp=gpar(lwd = 2, col = "red")) #Cabeza

x2 <- c(0.47, 0.64 , 0.53 , 0.46)
y2 <- c(0.8, 0.64, 0.48 ,0.6)
grid.bezier(x2,y2,gp=gpar(lwd = 2, col = "red")) #Oreja Derecha

x3 <- c(0.46, 0.43 , 0.44 , 0.4)
y3 <- c(0.6, 0.68, 0.72 ,0.77)
grid.bezier(x3,y3,gp=gpar(lwd = 2, col = "red")) #Oreja Derecha

x4 <- c(0.43, 0.41 , 0.42 , 0.4)
y4 <- c(0.7, 0.65, 0.61 ,0.59)
grid.bezier(x4,y4,gp=gpar(lwd = 2, col = "red")) #cara

x5 <-c(0.41, 0.61 , 0.63 , 0.73)
y5 <- c(0.56, 0.29, 0.42 ,0.25)
grid.bezier(x5,y5,gp=gpar(lwd = 2, col = "red")) #Parte superior Cuerpo

x6 <-c(0.73, 0.75 , 0.68 , 0.65)
y6 <-c(0.25, 0.11, 0.08,0.08)
grid.bezier(x6,y6,gp=gpar(lwd = 2, col = "red")) #Parte superior Cuerpo

x7 <-c(0.73, 0.78 , 0.59 , 0.83)
y7 <-c(0.25, 0.3, 0.55,0.58)
grid.bezier(x7,y7,gp=gpar(lwd = 2, col = "red")) #Cola
# Calculando error en la curva 
fx <- function(x) {y = 2643.283 - (10427.17*x) + (13685.01*(x^2)) - (5973.824*(x^3))}
x <- 0.711
y <- 0.507
valor <- fx(x)
error <- (abs(valor-y))/y
errores <- c(errores,error)


x8 <-c(0.83, 0.69 , 0.87 , 0.735)
y8 <-c(0.58, 0.46, 0.32,0.205)
grid.bezier(x8,y8,gp=gpar(lwd = 2, col = "red")) #Cola
#Calculando error en la curva
fx <- function(x) {-2827.86 + (10873.06*x) - (13921.6*(x^2)) + (5936.458*(x^3))}
fx
x <- 0.773
y <- 0.4565
valor <- fx(x)
error <- (abs(valor-y))/y
errores <- c(errores,error)


x9 <-c(0.62, 0.68 , 0.72 , 0.55)
y9 <-c(0.1, 0.05, 0.023,0)
grid.bezier(x9,y9,gp=gpar(lwd = 2, col = "red")) #Patita

x10 <-c(0.574, 0.499 , 0.401 , 0.512)
y10 <-c(0.314, 0.345, 0.267,0.0895)
grid.bezier(x10,y10,gp=gpar(lwd = 2, col = "red")) #Patita

x11 <-c(0.548, 0.473 , 0.377 , 0.4106)
y11 <-c(0.0876, 0.084, 0.1,0.0014)
grid.bezier(x11,y11,gp=gpar(lwd = 2, col = "red")) #Patita

x12 <-c(0.4497, 0.4396 , 0.429 , 0.4411)
y12 <-c(0.0544, 0.0459, 0.0284,0.0014)
grid.bezier(x12,y12,gp=gpar(lwd = 2, col = "red")) #Patita

x13 <-c(0.4997, 0.4896 , 0.479 , 0.4911)
y13 <-c(0.0544, 0.0459, 0.0284,0.0014)
grid.bezier(x13,y13,gp=gpar(lwd = 2, col = "red")) #Patita

x14 <-c(0.3446, 0.348 , 0.352 , 0.3021)
y14 <-c(0.434, 0.30093, 0.232,0.0838)
grid.bezier(x14,y14,gp=gpar(lwd = 2, col = "red")) #Patita 2

x15 <-c(0.441, 0.4446 , 0.442 , 0.423)
y15 <-c(0.3554, 0.3006, 0.1918,0.074)
grid.bezier(x15,y15,gp=gpar(lwd = 2, col = "red")) #Patita 2

x16 <-c(0.334, 0.2494 , 0.2406 , 0.2656)
y16 <-c(0.088, 0.0898, 0.0254,0.0014)
grid.bezier(x16,y16,gp=gpar(lwd = 2, col = "red")) #Patita 2

x17 <-c(0.3103, 0.2953 , 0.2807 , 0.2895)
y17 <-c(0.0546, 0.0485, 0.0172,0.0014)
grid.bezier(x17,y17,gp=gpar(lwd = 2, col = "red")) #Patita 2

x18 <-c(0.3403, 0.3253 , 0.3207 , 0.3295)
y18 <-c(0.0546, 0.0485, 0.0172,0.0014)
grid.bezier(x18,y18,gp=gpar(lwd = 2, col = "red")) #Patita 2

x19 <-c(0.4293, 0.4544 , 0.4805 , 0.5072)
y19 <-c(0.1296, 0.107, 0.1032,0.0892)
grid.bezier(x179,y19,gp=gpar(lwd = 2, col = "red")) #Parte Inferior cuerpo

x20 <-c(0.3306, 0.2236 , 0.164 , 0.2436)
y20 <-c(0.1966, 0.275, 0.4314,0.554)
grid.bezier(x20,y20,gp=gpar(lwd = 2, col = "red")) #Parte Inferior cuerpo

x21 <-c(0.22, 0.206 , 0.1935 , 0.1966)
y21 <-c(0.35, 0.327, 0.123,0.0988)
grid.bezier(x21,y21,gp=gpar(lwd = 2, col = "red")) #Patita 3

x22 <-c(0.2956, 0.289 , 0.2918 , 0.2953)
y22 <-c(0.2278, 0.199, 0.1206,0.08)
grid.bezier(x22,y22,gp=gpar(lwd = 2, col = "red")) #Patita 3

x23 <-c(0.2086, 0.166 , 0.104 , 0.169)
y23 <-c(0.0994, 0.099, 0.039,0.0014)
grid.bezier(x23,y23,gp=gpar(lwd = 2, col = "red")) #Patita 3

x24 <-c(0.1813, 0.171 , 0.151 , 0.1802)
y24 <-c(0.0684, 0.06, 0.0334,0.0014)
grid.bezier(x24,y24,gp=gpar(lwd = 2, col = "red")) #Patita 3

x25 <-c(0.2213, 0.211 , 0.191 , 0.2202)
y25 <-c(0.0684, 0.06, 0.0334,0.0014)
grid.bezier(x25,y25,gp=gpar(lwd = 2, col = "red")) #Patita 3

x26 <-c(0.1573, 0.0674 , 0.0286 , 0.1278)
y26 <-c(0.7335, 0.71, 0.8203,0.8426)
grid.bezier(x26,y26,gp=gpar(lwd = 2, col = "red")) #Oreja Izquierda

x27 <-c(0.1278, 0.1614 , 0.191 , 0.2494)
y27 <-c(0.8426, 0.8456, 0.816,0.8454)
grid.bezier(x27,y27,gp=gpar(lwd = 2, col = "red")) #Oreja Izquierda

x28 <-c(0.25, 0.2418 , 0.2392 , 0.2196)
y28 <-c(0.83, 0.827, 0.788,0.7744)
grid.bezier(x28,y28,gp=gpar(lwd = 2, col = "red")) #Cara

x29 <-c(0.2223, 0.184 , 0.1385 , 0.2007)
y29 <-c(0.774, 0.7876, 0.7683,0.713)
grid.bezier(x29,y29,gp=gpar(lwd = 2, fill="black")) #Nariz

x30 <-c(0.2223, 0.244 , 0.3014 , 0.2007)
y30 <-c(0.774, 0.7724, 0.7254,0.713)
grid.bezier(x30,y30,gp=gpar(lwd = 2, fill="black")) #Nariz

x31 <-c(0.2023, 0.1944 , 0.2676 , 0.3354)
y31 <-c(0.713, 0.6678, 0.5758,0.661)
grid.bezier(x31,y31,gp=gpar(lwd = 2, col = "red")) #Hocico

x32 <-c(0.1696, 0.126 , 0.182 , 0.224)
y32 <-c(0.759, 0.699, 0.63,0.65)
grid.bezier(x32,y32,gp=gpar(lwd = 2, col = "red")) #Hocico

x33 <-c(0.3354, 0.3594 , 0.3675 , 0.3754)
y33 <-c(0.661, 0.685, 0.6968,0.7218)
grid.bezier(x33,y33,gp=gpar(lwd = 2, col = "red")) #Boca

x34 <-c(0.3498, 0.3513 , 0.3508 , 0.3318)
y34 <-c(0.6756, 0.6693, 0.6018,0.5824)
grid.bezier(x34,y34,gp=gpar(lwd = 2, col = "red")) #Boca

x35 <-c(0.3318, 0.3254 , 0.3314 , 0.2885)
y35 <-c(0.5824, 0.553,0.5025,0.501)
grid.bezier(x35,y35,gp=gpar(lwd = 2, col = "red")) # Lengua

x36 <-c(0.2784, 0.2578 , 0.2256, 0.2885)
y36 <-c(0.6257, 0.5896, 0.5125, 0.501)
grid.bezier(x36,y36,gp=gpar(lwd = 2, col = "red")) # Lengua

x37 <-c(0.257, 0.2451, 0.2434, 0.2395)
y37 <-c(0.583, 0.606, 0.621, 0.6385)
grid.bezier(x37,y37,gp=gpar(lwd = 2, col = "red")) # Boca

x38 <-c(0.3271, 0.3589, 0.371, 0.3838)
y38 <-c(0.5426, 0.5422, 0.6016, 0.6372)
grid.bezier(x38,y38,gp=gpar(lwd = 2, col = "red")) # Boca

x39 <-c(0.234, 0.269, 0.29, 0.2942)
y39 <-c(0.771, 0.7875, 0.7804, 0.8276)
grid.bezier(x39,y39,gp=gpar(lwd = 2, col = "red")) # Boca

x40 <-c(0.2942, 0.297, 0.237,0.2555)
y40 <-c(0.8276, 0.8792, 0.8412,0.78)
grid.bezier(x40,y40,gp=gpar(lwd = 2, fill="black")) # Ojo Izquierdo

x41 <-c(0.284, 0.277, 0.2522,0.2555)
y41 <-c(0.7977, 0.8265, 0.8205,0.78)
grid.bezier(x41,y41,gp=gpar(lwd = 2, fill="black")) # Ojo Izquierdo

x42 <-c(0.3408, 0.3064, 0.3162,0.3496)
y42 <-c(0.744, 0.7466, 0.8325,0.8283)
grid.bezier(x42,y42,gp=gpar(lwd = 2, fill="black")) # Ojo Derecho

x43 <-c(0.3408, 0.3696, 0.3864,0.3496)
y43 <-c(0.744, 0.7466, 0.8325,0.8283)
grid.bezier(x43,y43,gp=gpar(lwd = 2, fill="black")) # Ojo Derecho

x44 <-c(0.3408, 0.3556, 0.3543,0.3216)
y44 <-c(0.744, 0.7554, 0.8178,0.7914)
grid.bezier(x44,y44,gp=gpar(lwd = 2, fill="black")) # Ojo Derecho

x45 <-c(0.3661, 0.386, 0.406,0.4146)
y45 <-c(0.5838, 0.581, 0.5894,0.5856)
grid.bezier(x45,y45,gp=gpar(lwd = 2, fill="black")) # Collar

x46 <-c(0.4146, 0.4156, 0.4173,0.4146)
y46 <-c(0.5838, 0.5832, 0.5704,0.5556)
grid.bezier(x46,y46,gp=gpar(lwd = 2, fill="black")) # Collar

x47 <-c(0.3461, 0.386, 0.406,0.4146)
y47 <-c(0.5538, 0.551, 0.5594,0.5556)
grid.bezier(x47,y47,gp=gpar(lwd = 2, fill="black")) # Collar

x48 <-c(0.2386, 0.2363, 0.239,0.2401)
y48 <-c(0.5752, 0.5636, 0.5562,0.548)
grid.bezier(x48,y48,gp=gpar(lwd = 2, fill="black")) # Collar

x49 <-c(0.2386, 0.2437, 0.2504,0.2561)
y49 <-c(0.5752, 0.5748, 0.577,0.5765)
grid.bezier(x49,y49,gp=gpar(lwd = 2, fill="black")) # Collar

x50 <-c(0.240,0.24484,0.24645,0.252)
y50 <-c(0.548,0.5485,0.5484,0.5486)
grid.bezier(x50,y50,gp=gpar(lwd = 2, fill="black")) # Collar

#Obtener el error promedio de las curvas
suma = 0
for (i in 1:length(errores)){
  suma = suma + errores[i]
}

promedio = suma/length(errores)
cat ("El error promedio de las curvas de bezier es: ", promedio)