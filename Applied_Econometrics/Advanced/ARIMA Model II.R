# Función de autocorrelación simple y parcial: identificando los modelos

# SIMULACIÓN ARIMA Y FUNCIONES DE AUTOCORRELACIÓN (1)



install.packages("tseries")
library(tseries)

# Alfa 1 (o cualquier Alfa) puede ser positivo o negativo, pero en valor absoluto debe ser menor que 
# 1. 
# Y la media movil debe ser menor que Infinito.

ar1 = arima.sim(list(order=c(1,0,0),
                     ar =0.8),
                n =100)
# en esta sentencia, vamos a generar un modelo AR, en donde su coeficiente 
# su Alfa 1 va a ser 0.8
# Lo graficamos
  plot(ar1) # se puede observar una serie centrada.
  
ar2 = arima.sim(list(order=c(1,0,0),
                       ar =0.9),
                  n =100) 
plot(ar2) # si cambiamos el AR a 0.9 podemos obvservar un grafico mas
# pronunciado.

ar3 = arima.sim(list(order=c(1,0,0),
                     ar =0.999),
                n =100)
plot(ar3) # mientras más cercano a 1, el coeficiente AR, mas volatil se vuelve el proceso. 

# ¿Que sucede si le ponemos 1?

ar4 = arima.sim(list(order=c(1,0,0),
                     ar =1),
                n =100)
# NO sale, nos marca un error. No marca que el AR no es estacionario.
# Este tiene que ser si o si menor que 1.

# Para llamar a la Funcion de Autocorrelacion Simple y la funcion de
# autocorrelacion parcial, pondremos el ACF (nombre de la funcion que estamos
# simulando (simulando por que no es una BD real y un AR que nos inventamos
# no uno que hemos calculado, asignamos valores arbitrarios))

acf(ar1) # se observa un plot de la funcion de Autocorrelacion Simple
pacf(ar1) # Y la funcion de Autocorrelacion Parcial luce asi.

# Para verlos ocupamos:

par(mfrow=c(1,2)) # en una fila, 2 graficos.
acf(ar1)  # (simple)
pacf(ar1) # (parcial)

# Nota: Recordemos que la linea azul son los Intervalos de Confianza.

# Podemos notar en la ACF el primer valor de Rho es 1 (primer valor del rezago), 
# consistente con la deficion de ACF, el primer valor de Rho es 1 cuando K = 0.
# luego va decreciendo hacia 0 y se anula.

# Si nosotros observamos que hay valores n en la PACF (Parcial), fuera de los IC
# y en la simple (simultaneamente) vemos valores decreciento, hablamos de un
# modelos autorregresivo de valor p.

# Y si en la PACF SOLO uno se sale de los intervalos de confianza, y es el primero
# es un Autorregresivo de orden 1. De esta manera se leen las PACF.

## -- Medias Moviles --##
ma1 = arima.sim(list(order=c(0,0,1),
                     ma =1.2),
                n =100)
acf(ma1)
