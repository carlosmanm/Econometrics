# Modelo de Probabilidad Lineal 1

# Cargando librarias -----------

library(foreign)
library(forecast)
library(gmodels)
library(lmtest)
library(ResourceSelection)
library(ROCR)
library(openxlsx)

# Bases de datos --------------

file.choose()

# El archivo es hepatitis.sav -> el cual es tipo Spss

data <- read.spss("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Betametrica\\Micro 1\\bases-de-datos\\hepatitis.sav",
                  use.value.labels = F, to.data.frame = T)

data%>%
  View()

attach(data)

nrow(data) 

# 30 observaciones

# Estimar el MLP -------

mlp <- lm(hepatitis~gramalcodiario+drogas, data = data)

summary(mlp)

# ¿Porque para estos modelos de probabilidad no es bueno aplicar MCO?

# 1. Se evalua la significancia individual
# 2. Interpretacion de coeficientes:
 # Todavia no se puede definir nada claro sobre los coeficientes,
 # dado que estamos ante un modelo lineal de probabilidad. Lo unico
 # que puedo empezar a interpretar es la magnitud

# Ejemplo con -> 
   #             Estimate
# (Intercept)    0.008753    
# gramalcodiario 0.009843   
#  drogas        0.478674 

# Segun la magnitud del las drogas podriamos empezar a sospechar que las drogas 
# al ser el mayor coeficiente, da la impresion que el factor de consumo de drogas aporta
# a la probabilidad de que se tenga hepatitis. Ademas es positivo, y eso es todo lo que podria
# afirmarse, que el factor drogas aporta más que los demas factores en tener o no hepatitis y lo hacer
# de forma positiva. 

# dado que los valores que toma Y son 1 y 0's no habra una recta que pase por los puntos 1 y 0 
# dado que es imposible escalarmente. Por ello el R^2 carece de validez.

# Evaluar valores estimados con el modelo vs las variables de Y
plot(mlp$fitted.values, type ="l")
abline(h=0) 
abline(h=1)
# Esto se hace acotando a que el valor este entre 1 y 0.
# En el grafico se puede observar que nuestro modelo no respeta que la prob vaya de 0 a 1
mlp$fitted.values
# Este es realmente por lo cual los mlp no son los optimos, dado que sufren de muchos problemas
# No se recomienda ocupar mlp

dwtest(mlp)
# No presenta problemas de autocorrelacion

bptest(mlp)

plot(mlp$residuals, type = "l")

# Pronosticos --------

?forecast
# Cuidado con forecast: si esta activado el paquete forecast y el de aTSA, se pueden 
# confundir las funciones, debe estar desactiva aTSA que solo funciona para ARIMAS's

f1 <- forecast(mlp,
                newdata = data.frame(gramalcodiario = c(10), drogas = c(0)))
f1

f2 <- forecast(mlp,
               newdata = data.frame(gramalcodiario = c(10), drogas = c(1)))

f2

# La probabilidad de que tener hepatitis con 10 gm de alcohol y sin consumir drogas es de 
# 0.1071838  en el f1, pero si añadimos en un f2, que si consume drogas (1), la probabilidad pasa a ser
# 0.5858582, y esto es acorde a lo que los coeficientes del mlp nos decian con respecto a la magnitud
# el consumo de drogas aporta más en la probabilidad de contraer hepatitis.

0.5858582 - 0.1071838

# respuesta: 0.4786744
coef(mlp)

# drogas -> 0.4786744

# Es decir que el coeficiente se puede interpretar como, lo que aporta en la probabilidad de contrar
# hepatitis (1). La probabilidad de contraer hepatitis si consumes drogas aumenta en un 47.86%

f3 <- forecast(mlp,
               newdata = data.frame(gramalcodiario = c(11), drogas = c(0)))

f3

# probabilidad de 0.1170269 con gramos de alcohol en 1 aumento (a 11) vs 0.1071838 (manteniendo 10)
0.1170269 - 0.1071838
# respuesta: 0.0098431
coef(mlp)
# gramalcodiario -> 0.0098431

?read.spss
