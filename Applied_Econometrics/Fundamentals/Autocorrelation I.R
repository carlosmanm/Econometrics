# AUTOCORRELACIÓN

# Causas, consecuencias, identificación y tratamiento

# ------ Cargar librerias. -------- #

install.packages("dynlm")
install.packages("forecast")
install.packages("nlme")
install.packages("sandwich")
install.packages("car")
install.packages("ggplot2")
install.packages("zoo")
install.packages("lmtest")
library(dynlm)
library(forecast)
-library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(zoo)-
library(lmtest)

# ------ Cargar Base de Datos. -------- #

file.choose()

reg = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\REG_1.csv")
attach(reg)
names(reg)

# ------ Estimar el Modelo. -------- #

modelo = lm(M1~GP+TI+IPC, data = reg)
summary(modelo)
# Prueba T (Significancia individual)
# Todas las variables son significativas y Ho se rechaza de que los Betas son 0. Todas pasan la prueba 2t.

# Prueba F (significacia conjunta)
# Todas las variables son significativas debido al P-Value. 

# El R2 ajustado es muy alto. Las variables explican el 96.65% del modelo.

### Este gran modelo puede tener una Autocorrelacion. Dado que son variables en Time Series, muy posiblemente
# uno de los problemas que aquejan a este modelo es la correlacion serial.

# ------ Identificacion del problema. -------- #

# El primer metodo que generalmente se enseña es el metodo grafico. 

# ------ Metodo Grafico. -------- #

plot(modelo$residuals)
plot(modelo$residuals, type="b")
# presionar Zoom para verlos de manera más clara. 
# El metodo grafico es el mas deductorio, aqui se tiene que analizar visualmente si se observa
# algun tipo de patron, de tendencia a la alza, a la baja, o algun tipo de comportamiento que 
# no sea estocastico. 
# Para esto se requiere un grafico mas completo y elaborado.
qplot(x=c(tail(modelo$residuals, -1), 0),
          y=modelo$residuals)
# Aqui se hace un QuickPlot, donde X va a ser el valor de los residuos, rezagando un periodo (-1)
# Que la grafica la centre en 0 (0) (mera estetica), contra una Y que tambien seran residuos
# Es decir, residuos rezagados vs residuos.

# Aqui claramente se ve una estructura de los residuos que va creciendo a traves del tiempo.
# Se aprecia que existe una relacion casi lineal entre los residuos y los residuos pasados. 
# Desde este punto ya podemos identificar que SI hay autocorrelacion. 

# Ahora lo haremos por Durbin Watson. 

# ------ Durbin Watson. -------- #

dwtest(modelo)
# Nos arroja:
# DW = 0.48404, p-value = 8.522e-14

# Ho: No Autocorrelacion
# H1: Autocorrelacion

# Si P value menor que 0.05 rechazo Ho.
# En este caso es MUCHO mas chico. 
# Con este P-Value es mas que suficiente para que usted continue con el analisis. 
# Sin embargo hay cuestiones adicionales que se enseñan en academia, que es ocupar la tabla
# de Durbin-Watson, hacer las reglas de decision, etc. 

# -- Usando la tabla de Durbin-Watson

# En la parte superior se pone K-1, donde K son los coeficientes que el modelo nos da, en este caso
# el modelo nos da 4 que son B0, B1, B2, B3. Entonces 4-1 = 3. Buscamos en la parte superior K=3
# despues es el numero de datos, que son 57, bajamos en la tabla hasta ir con el mas proximo,
# entre ellos esta el 55 y 60, el más cercano es 60 asi que ese se toma. El valor que nos da es
# de DL (Lower) nos da 1.480 y de DU (Upper) nos da 1.689 y el D (DW) que ya teniamos de 0.48404.
# Para empezar este DW tiene a mucho más a 0 que a 2 (y mucho mas que 4), por lo que podriamos 
# decir que es una autocorrelacion positiva. 
# despues ahora toca determinar que hacer, situamos la hipotesis a la cual esta asociada.
# La hipotesis de la positiva es que: Ho = No hay autocorrelacion positiva
# 0 < d < DL (Si esto se cumple se RECHAZA)
# 0 < 0.48 < 1.480 (Si se cumple, se rechaza la Ho) (Dado que SI hay autocorrelacion positiva)

# Dado esto, ocuparemos otro contraste.

# -- Breusch Godfrey

bgtest(modelo, order = 1)

# Ho : No Autocorrelacion de Orden 1
# H1: Autocorrelacion de Orden 1.
# LM test = 32.717, df = 1, p-value = 1.066e-08
# El p-value es MUY significativo, por lo tanto se RECHAZA Ho.

# ------ Atenuar Problema. -------- #

# -- Minimos Cuadrados Generalizados.

# Lo que hacen los Minimos Cuadrados Ordinarios  a traves del metodo de Maxima Verosimilitud
# encontrar la estructura de correlacion de los residuos. Encontrando la estructura de 
# correlacion de los residuos se puede corregir el problema. 
# Sin embargo es un metodo invasivo: Cambia sustancialmente los valores de los coeficientes, 
# entonces, (remedio peor que enfermedad), recordemos que en gran parte lo que nos interesa
# es el valor de los coeficientes, esos Betas son promedios que son utilizados para aproximar
# la funcion de regresion poblacional. Estos NO deben cambiar. 

modelfitted = gls(model = M1~GP+TI+IPC, 
                  correlation = corAR1(), 
                  method = "ML")

# Model Fitted : Modelo Ajustado
# Generalized Least Squares: Minimos Cuadrados Generalizados.
# ML = Maxima Verosimilitud

summary(modelfitted)
summary(modelo)
# Nos interesa el RO. Que aqui se llama Phi (coeficiente de correlacion): 0.8566
# ese el coeficiente de correlacion de los residuos, este es positivo (autocorrelacion positiva)
# a traves de este se corrige toda la DATA y se corre el modelo nuevamente. 
# hacemos un summary del modelfitted, donde que YA ha corregido la autocorrelacion, todos los
# betas han cambiado, y podemos hacer un summary del primer modelo y vemos la gran diferencia.
# este problema ya no tiene problemas de autocorrelacion, pero los betas no son los exactos.


# --- NEWEY WEST (MEJOR)
# Metodo NO invasivo. 

coeftest(modelo, vcov = NeweyWest(modelo))
# La tabla que nos arroja esta asociada al metodo de atenuacion o el metodo para el calculo de 
# los valores estandar libres de autocorrelacion (Newey West)
# Nos da los mismos betas, pero los errores son diferentes, ahora los YA no hay problema de 
# autocorrelacion. Es decir, el error estandar que nos da esta libre de correlacion serial.



###   -- Metodo de Regresion con Rezagos. --- ### 

# se tiene que transformar matriz en serie de tiempo. 

# Es un 90% preferible que se trabaje con este tipo de modelos. ( Y contrastes)
# 1. Crear el modelo y Estimarlo

stmodelo = ts(reg, start = c(2003, 2),
              frequency = 12)
ts.plot(stmodelo)
ts.plot(stmodelo[,2]) # solo para probar que funcione el time series. 

modelolag = dynlm(M1~GP+IPC+TI+L(M1,1),
                  data = stmodelo)
#lag : rezago
summary(modelolag)
# este modelo, con respecto al primero que se hizo es MUCHo mas rico, dado que se toman en cuenta
# el rezago de M1, en este caso se muestra como L(M1, 1), la mayoria de los coeficientes se
# conserva, pero es TI el que cambia, eso podemos verlo. 

# 2. Despues toca averiguar si hay autocorrelacion (Metodo Grafico, Durbin-Watson, o Breusch Godfrey). 

# ERROR DE NOVATO
dwtest(modelolag)
# El Durbin-Watson es un contraste que NO considera variables rezagadas, por lo que NO se puede
# ocupar este contraste. 

# Con el que se debe comprobar es con el bgtest.

bgtest(modelolag, order = 1) # el orden es 1, porque hay 1 rezago.
# Ho: No Autocorrelacion de orden 1
# H1: Autocorrelacion de orden 1
# Nos arroja; LM test = 0.045109, df = 1, p-value = 0.8318
# el p-value no es menor a 0.05 por lo que se Rechaza la Ho. (Hay autocorrelacion)
# si p-value es menor a 0.05 se rechaza Ho
# Queremos que p-value sea mayor a 0.05

# 3. Ahora toca Atenuar el problema. (Minimos Cuadrados Generalizados o Dewey West)
# Contrastemos este modelo con rezago (mejor modelo que solo LM normal, con Dewey West)
modelo1 = coeftest(modelolag, vcov = NeweyWest(modelolag))
# Y listo conservamos los coeficientes, pero con distintos errores, ahora tenemos un modelo
# con mucha mejor informacion y sin autocorrealacion. 


