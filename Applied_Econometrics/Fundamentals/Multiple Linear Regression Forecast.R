# Forecast con un modelo de regresión múltiple

# Prediccion usando los MCO

# Opinion del experto
# Construir un modelo ARIMA y proyecto las variables X (Series Temporales Univariables)
# Tener informacion acerca de lo que realmente va a suceder

# Para una prediccion, en primera, esta se expresa de esta manera:

# yt+1 = Bo + b1x1 t+1 + b2x2 t+1

file.choose()
ventas = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\2. Econometria Aplicada 1\\material\\ventas.csv")
library(forecast)

attach(ventas)

names(ventas)

newdata = data.frame("GSueldosySalarios" = c(171796.41), "GMarketyPublicidad" = c(82914.82))

forecast(modelo1, newdata = newdata)
# Que me haga una proyeccion considerando x1 t+1 y x2 t+1 
# mi tabla llega hasta agosto del 2014, y son los valores que estan en newdata, los de agosto del 2014, entonces
# los valores que arroja forecast son de septiembre del 2014. Que son:
# 1649953, 1377178, 1922727, 1230483, 2069422
# si nosotros cambiamos los valores del data frame new data, cambiamos los valores del b1x1 t+1 y b2x2 t+1
# Nosotros podemos cambiarlo a nuestro gusto porque NO conocemos las proyecciones que necesitamos para poder
# proyectar VENTAS (Y), lo cual es claramente una debilidad del modelo de regresion. 
# pero recordemos que tenemos 3 opciones para poder proyectar nuestras Betas:

# 1) Opinion del experto (subjetivo)
# 2) Construir un modelo ARIMA y proyecto las variables X (Series Temporales Univariables) (más comun)
# 3) Tener informacion acerca de lo que realmente va a suceder

# Ahora, digamos ya proyectamos nuestras Betas, ahora proyectaremos nuestra Y.

# Supongamos ahora 2 periodos adelante, t+2

# Es decir: yt+1 = Bo + b1x1 t+2 + b2x2 t+2

newdata = data.frame("GSueldosySalarios" = c(171796.41, 202079.71), 
                     "GMarketyPublicidad" = c(82914.82, 75747.79))
forecast(modelo1, newdata = newdata)
# Te arroja datos de los siguientes 2 periodos
# 1        1649953, 1377178, 1922727, 1230483, 2069422
# 2        1770972, 1492638, 2049306, 1342953, 2198991
# Nos arroja los valores de las VENTAS para el periodo 1 y 2 que son;
# 1649953 y 1770972 con sus respectivos intervalos de confianza. 
