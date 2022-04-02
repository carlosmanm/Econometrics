# Metodología Box Jenkins: Elaboración de modelos de series temporales 
# paso a paso (1)

# la metodologia Box Jenkins es una serie de pasos..

# 1. metodo grafico

# 2. evaluamos estacionariedad (no raiz unitaria) con nuestros 4 contrastes.

# 3. Determinacion de los procesos FAS-FAP (ARMA y ARIMA)

# 4. Evaluacion del modelo

# 5. Proyeccion

library(forecast)
library(urca)


# -- Cargamos base de datos

file.choose()

data = read.table("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\4. Modelos Econometricos para el Pronostico en Negocios\\Bases de Datos\\PIB.txt")

attach(data)

names(data)

tsdata = ts(as.vector(as.matrix(data)), start=c(2000, 1), 
            frequency = 4)
ts.plot(tsdata)

ts.plot(diff(tsdata, 1))

# Contruimos un ARIMA(d,i,q)

# Evaluar la raiz unitaria.

# Solo ocuparemos 2: PP Y ERS

# PP test

testpp = ur.pp(tsdata, type=c("Z-tau"),
               model=c("trend"),
               lags = c("short"))
summary(testpp)

# Value of test-statistic, type: Z-tau  is: -1.5947 

#Critical values for Z statistics: 
 #                  1pct      5pct     10pct
# critical values -4.134754 -3.493511 -3.175277

# -1.5947 no es mayor en valor absoluto que ninguno de los valores tabla.
# Por lo tanto NO rechazo Ho. La serie en estudio no es estacionaria.

# ERS test

erstest = ur.ers(tsdata, 
                 type=c("DF-GLS"), 
                 model=c("trend"),
                 lag.max = 4)
summary(erstest)

#Value of test-statistic is: -1.6574 

#Critical values of DF-GLS are:
 #                 1pct  5pct 10pct
# critical values -3.58 -3.03 -2.74

# como 1.6574 no es mayor que ninguno de los valores tabla no se rechaza Ho
# y se asume que esta serie temporal presenta problemas de raiz unitaria.

# Ya sabemos que tenemos un problema de raiz unitaria.

# El paquete forecast nos arroja el numero de lags que necesitamos, 
# es decir, cuantas veces tenemos que diferenciar la serie.

# ndiffs(x, alpha = 0.05, test = c("kpss", "adf", "pp"),
# type = c("level", "trend"), max.d = 2, ...)


ndiffs(tsdata,
       test=c("kpss"))
# R nos indica que 1

ndiffs(tsdata,
       test=c("pp"))
# Nos indica que 1 otra vez.

ndiffs(tsdata,
       test=c("adf"))
# Y una vez más nos indica 1 como el numero de diferenciacion. 
# Lo siguiente es la construccion del modelo ARIMA (dado que habra diferenciacion)

dtsdata = diff(tsdata, 1)

# identifiquemos nuestro FAP-FAS

par(mfrow=c(1, 2))
acf(diff(tsdata,1))
pacf(diff(tsdata,1))

# Por definicion el rezago 0 va a tener un valor Rho 1, siempre.
# el rezago 0 no se debe dibujar en el FAS. O no tomarla en cuenta.
# esto se cambia, modificando el xlim. 

# tanto en FAS como en FAP solo 1 valor sobresale los Intervalos de Confianza.
# por lo que construiremos un modelo MA(1) Y AR(1) con una I(1)

modelo1 = arima(tsdata, 
                order = c(1,1,1))
modelo1

# No arroja 
# Coefficients:
        # ar1      ma1
      # 0.8892  -0.3379
# s.e.  0.0881   0.1964

# Nuestros coeficientes de MA y AR son 0.8892  -0.3379. (suma menor a 1)
# lo restante son sus errores.

# Hacemos una prueba de significacia (prueba T) de estos coeficientes.

# Ho: Coeficiente = 0 (queremos rechazar esta)
# H1: Coeficiente!= 0

# Coeficiente/Error

0.8892/ 0.0881
# r: 10.09308
# si T es mayor que 2 para una muestra mayor a 30 datos entonces Ho.
# entonces se rechaza Ho.
-0.3379/ 0.1964
# R: -1.720468 no es mayor que 2
# No se puede rechazar Ho para MA.
# La Media Movil (1), no aporta a la variable en estudio.

modelo2 = arima(tsdata, order=c(1,1,0))
modelo2
# solo nos da un AR.
0.7412/0.0919
# R: 8.065288, rechazo Ho.
# El modelo 2 va bastante bien. 

# Verificar la capacidad predictiva del modelo. Esto lo haremos con la funcion
# accuracy de la paqueteria forecast.
accuracy(modelo2)

# nos da indicadores de predictibilidad, la idea de estos indicadores
# es que sean lo mas pequeños posibles y normalmente se ocupan 3.
# MAPE, MAE y RMSE.

#                   ME     RMSE      MAE       MPE      MAPE    MASE      ACF1
# Training set 39054.11 126842.5 97400.62 0.3079934 0.7929001 0.58768 -0.279324

# El MAPE devuelve el error de pronostico en terminos porcentuales.
# en este caso es 0.79% menor al 1% lo cual es excelente.
# el error (MAE) tiene un valor de 97400.6 para el PIB. (debe ser lo mas chico posible)
# Los indicadores son bastante buenos. Con esto podemos aceptar el modelo 2.

# -- Toca evaluar los residuos.

par(mfrow=c(1,1))
plot(modelo2$residuals)
abline(h=0) # vemos la regularidad

Box.test(modelo2$residuals, lag=1, type = c("Ljung-Box"))

# Ho: Residuos Independientes
# H1: Residuos NO independientes.

# Si p-value menor que 0.05 se rechaza Ho.
# data:  modelo2$residuals
# X-squared = 4.5296, df = 1, p-value = 0.03331
# hay un problema de autocorrelacion. 

par(mfrow=c(1,2))
acf(modelo2$residuals)
pacf(modelo2$residuals)

# Con esto notamos que solo se presenta el problema de autocorrelacion
# en la primera diferencia, las demas estan a salvo.
# eso es normal dado el nivel de agregacion del modelo que es el PIB 
# por lo que podemos seguir adelante con la proyeccion.

f1 = forecast(modelo2, h=4) # h es el numero de proyeccion que queremos.
plot(f1)

f1 # nos arroja las predicciones.
install.packages("highcharter")
hchart(f1)

ts.plot(tsdata, f1$fitted) # nos enseña el modelo original vs el propuesto.

