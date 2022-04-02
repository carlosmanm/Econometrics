# Modelo de Regresion Lineal Multiple

file.choose()
ventas = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Betametrica\\2. Econometria Aplicada 1\\material\\ventas.csv")

View(ventas)

names(ventas)

install.packages("forecast")

install.packages("highcharter")

library(forecast)

attach(ventas)

modelo1 = lm(VENTAS_NETAS~GSueldosySalarios+GMarketyPublicidad, data = ventas)
summary(modelo1)
options(scipen = 999) # Te muestra valores completos, que por default es 0

# Prueba de significancia individual

#bk = 0
#bk =!0

# 2t: si n > 30 entonces rechazo Ho

# Si p value es < 0.05 rechazo Ho, en este caso es:  0.000000000691 y 0.0000000000000002, entonces se rechaza Ho.

# Tambien es necesaria la prueba de significancia conjunta (Prueba F)

# Ho: Bo = B1 = B2 = 0
# H1: Bo = B1 = B2 =!0

# Y despues es la R2
# Recordemos que R2 esta entre 0 y 1:
# SIEMPRE Y CUANDO SE TOMA EN CUENTA Bo (no siempre se cumple)

# se explica en terminos de %, aqui el 73% del modelo se explica entre variables de gasto en marketing y publicidad 
# y en sueldos y salarios, lo restante esta explicado por otras variables no consideradas en el modelo.

## Recordemos, que un R2 sea muy alto tampoco significa que sea bueno, puede haber algun problema de
# multicolinealidad, sobreajuste en el modelo, etc

# a medida que incorporamos variables en nuestro modelo, nuestro R2 incrementa, asi que no nevesariamente
# un modelo de 7 variables es mejor que uno de 2.


