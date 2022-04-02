# Modelos estacionales SARIMA

# Esta es una lección introductoria a los modelos SARIMA, utilizados para el 
# modelamiento de series que se comportan de manera estacional, como por ejemplo:
# Venta de utiles escolares, electrodomésticos, venta flores, depósitos bancarios,
# y demás variables que repiten un patrón de manera sistemática ciertos meses del año.

# Los modelos estacionales: modelos que tinen como objetivo el tratamiento
# variables que tienen como objetivo el tratamiento de variables que se comportan
# de manera estacional: por ejemplo las flores en febrero, venta de juguetes en abril


# Es una variable que repite su comportamiento durante toda su existencia.
# Es donde existen picos o patrones que se repiten a traves del tiempo.
# A esto se le llama Estacional NO Estacionaria. 

# Estas series estacionales no son modelables directamente con modelos
# ARIMA sino con modelos SARIMA.

# Yt = a+a1Yt-1+011Et-1+Ut (ARMA(1,1))

# Yt - Yt-1 = a+a1Yt-1+011Et-1+Ut (ARIMA(1,1,1))
# Arima solo trabaja para series temporales para series estacionarias
# y que no tienen un comportamiento estacional, para los estacionales se
# requieren modelos SARIMA.


# Supongamos que tenemos este modelo

# Yt = a+a1Yt-1 (AR1)

# suponiendo que mi serie tiene una frecuencia de corte mensual (12) yo puedo proponer:

# Yt = a+a1Yt-12

# si en el modelo existe la incorporacion de una variable rezagada, con el numero
# de la frecuencia y que al mismo tiempo es una variable dependiente, estamos ante
# un modelo tipo SAR (1), es un Autorregresivo Estacional de orden 1. De ser...

# Yt = a+a1Yt-12+a2Yt-24+Ut (Es un SAR(2))

# Y estas se puede conjuntar con autorregresivos normales y estacionales:

# Yt = a+a1Yt-1(autorregresivo ordinal)+a2Yt-12(autorregresivo estacional)+Ut 

# La misma logica para las medias moviles (ahora estacionales)

# Yt = 0o+01Et-12 (SMA(1))

# Y estas se puede conjuntar con medias moviles normales y estacionales:

# Yt = 0o+01Et-1(media movil ordinal)+02Et-12(media movil estacional)+Ut

# Cuando se juntan modelos SAR con SMA se conjuntan modelos SARMA
# o un modelo ARIMA con una SARMA se forma un modelo SARIMA. 

# Recordemos que ARIMA (p,d,q)
# Entonces SARIMA (P,D,Q)

# Ejemplo del modelo SARIMA:

# Yt = a0+a1Yt-1-01Et-1+a2Yt-12-02Et-12+Ut

# SARIMA (1,0,1)(1,0,1),12
# Un autorregresivo, 0 diferenciaciones, 1 media movil. 
# Un autorregresivo estacional, 0 diferenciaciones estacionales
# 1 media movil estacional.  


# tenemos que una diferencia ordinal es:

# Yt - Yt-1 (diferencia ordinal) (d)

# Yt-1 - Yt-12 (diferencia estacional) (D)

# SARIMA (p,d,q)(P,D,Q)f.