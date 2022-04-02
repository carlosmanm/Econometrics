# Box Jenkins 2

# Grafico

# Identificacion de Raiz Unitaria

# Identificacion de Proceso Generador de Datos (ARMA) ó (ARIMA)

# Evaluacion de Resultados

# Proyeccion

file.choose()

data = read.table("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\4. Modelos Econometricos para el Pronostico en Negocios\\Bases de Datos\\CONSUMO.txt")

attach(data)

names(data)

ts.plot(data)

tsdata = ts(data/1000, start = c(2004, 8),
            frequency = 12)

# Grafico

ts.plot(tsdata) # definitivamente NO es estacionaria.
ts.plot(diff(tsdata,1))

# Comprobacion de Raiz Unitaria

# pptest

pptest = ur.pp(tsdata, type=c("Z-tau"),
               model=c("trend"),
               lags = c("short"))
summary(pptest)

#Value of test-statistic, type: Z-tau  is: -1.8165 

#Critical values for Z statistics: 
                   # 1pct      5pct     10pct
#critical values -4.036703 -3.447497 -3.148576

# Nuestro valor calculado NO es mayor que ninguno de mis valores tabla por
# lo que no rechazo Ho (Raiz Unitaria)

erstest = ur.ers(tsdata, 
                 type=c("DF-GLS"),
                 model=c("trend"),
                 lag.max = 4)
summary(erstest)

#Value of test-statistic is: -1.0653 

#Critical values of DF-GLS are:
                # 1pct  5pct 10pct
#critical values -3.46 -2.93 -2.64

# Ninguno mayor, no rechazo Ho (Raiz Unitaria)

# Ya sabemos, por 2 contrastes que SI hay Raiz Unitaria, ahora debemos saber
# cuantas veces vamos a diferenciar la serie, para eso ocuparemos ndiffs de
# la paqueteria forecast.


ndiffs(tsdata, test=c("kpss")) # R = 1
ndiffs(tsdata, test=c("adf")) # R = 1
ndiffs(tsdata, test=c("pp")) # R = 1

# Para los 3 contrastes nos indica que con una diferencia es suficiente.
# Entonces se trata de un modelo ARIMA.

# Extraccion del proceso generador de los datos.
## Pacf y Acf vienen en el paquete forecast.

par(mfrow=c(1,2))
acf(diff(tsdata, 1))
pacf(diff(tsdata, 1))

# Para el PACF notamos que es un autorregresivo de orden 6 (AR 6) 
# dado que los autorregresivos que salen de los IC son el 3 y el 6.
# es decir, que solo estos son significativos
# Para el ACF notas que una media movil de orden 6 (MA 6)
# dado que las medias moviles que salen de los IC son el 3 y el 6.
# es decir, que solo estos son significativos

# AR(6) I(1) MA(6)

modelo1 = arima(tsdata, order=c(6,1,6),
                fixed = c(0,0,NA,0,0,NA,0,0,NA,0,0,NA))
modelo1

# Ya los demas coeficientes son 0. Y solo nuestros AR(3 y 6) y MA (3 y 6)
# nos quedan como valores.

# Pruebas de significacia:

# Prueba T

0.3465/0.4999 
# R = 0.6931
# No es mayor que 2. Puede que ese AR NO me aporte. 

0.2968/0.5198
# R = 0.5709
# No es mayor que 2. Puede que ese AR NO me aporte.

-0.2011/0.5093
# R = -0.39485
# No es mayor que 2. Puede que ese MA NO me aporte.
 
-0.0111/0.4852
# R = -0.0228
# No es mayor que 2. Puede que ese MA NO me aporte.

# Esto NO significa que no se tenga que incluir ninguna, sino que la combinacion
# entre estos hace que NO sea significativa. Por lo mismo se pueden llevar a cabo
# otras combinaciones pero entre estas 4 variables MA(3 y 6) y AR(3 y 6)

modelo2 = arima(tsdata, order=c(3,1,3),
               fixed = c(0,0,NA,0,0,NA))

modelo2

# T test

0.87/0.10
# R:8.7

-0.6728/0.1534
# R: -4.385919

# Ya son significativos. Es cuestion de probar las diferentes combiaciones ARIMA.

accuracy(modelo2)

#                   ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
  # Training set 618.7077 5454.465 3595.435 0.4219114 2.014258 0.9052084 0.03834995

plot(modelo2$residuals)


## Identificador problema de 
Box.test(modelo2$residuals)

# p-value = 0.6731
# Si p-value menos que 0.05 rechazo Ho. Aqui no se puede rechazar. 
# NO hay un problema de Autocorrelacion. Los residuos son independientes.

acf(modelo2$residuals)
pacf(modelo2$residuals)
# Ninguna barra sobresale los IC. Este modelo YA es util para replicar la 
# serie y por tanto, pronosticar.

# pronostico

f1 = forecast(modelo2,h=4)
plot(f1)
hchart(f1)

# comparacion

ts.plot(tsdata, f1$fitted, f1$mean, col=c("red", "blue", "green"))
