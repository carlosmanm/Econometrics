# Los modelos SARIMA y los modelos Holt Winters

# Una opción adicional a los modelos SARIMA para modelar series que tienen un 
# comportamiento estacional, son los modelos Holt Winters aditivos y multiplicativos.

# En general, los modelos HW pueden se utilizados para cualquier serie temporal que 
# presente al menos dos componentes pronunciados: Tendencia y Estacionalidad.

# Requerimientos previos...

# Historico de una serie como las ventas, con un minimo de 3 a 5 ciclos completos.

# Conocimiento sobre el comportamiento historico de la serie.

# Son metodos diseñados para construir pronosticos. La estimacion consiste en calculo
# de parametros (tendencia, niveles y factor estacional), cuando los parametros estimados
# son mayores a 1, se les da mayor relevancia. Usualmente el problema de estimacion es
# el valor de arranque: sin embargo R lo estima de forma automatica.

file.choose()

data = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\4. Modelos Econometricos para el Pronostico en Negocios\\Bases de Datos\\base.xlsx",
                 detectDates = T)
View(data)

ventas =ts(data[,2], start = c(2006,1),
              frequency = 12)

# entonces tenemos el comportamiento aditivo (constante) y multiplicativo (creciente)
# tenedencial y estacional (mas o menos contsnate) es un modelo Holt Winters aditivo, 
# tendencial y estacional creciendo a traves del tiempo (Holt Winters multiplicativo.)
# se puede ocupar un SARIMA o un Holt Winters, para modelos estacionales.
# Primero se debe identificar el comportamiento y despues ver que modelo ocupar,
# si es aditivo o el multiplicativo. Todos los HW estiman los mismos 3 parametros.


ts.plot(ventas) # es multiplicativo.

monthplot(log(ventas)) # verificamos que sea estacional por el metodo grafico.

ndiffs(log(tssales), test = c("pp")) # Nos indica que NO necesita ninguna diferenciacion.

# No hay que diferenciarla ni estacional ni ordinariamente. 

# Capturemos la forma del modelo (proceso generador de datos)

par(mfrow=c(1,2))
acf(log(ventas))
pacf(log(ventas)) 

# modelamos.

m1 = arima(log(ventas), 
           order=c(1,0,1),
           seasonal = list(order=c(1,0,1)))
m1

# se hace lo normal; la significancia, el accuracy, independiente de residuos, 
# los ajustes de acf y pacf para residuos, los circulos unitarios y pronosticos.

f1=forecast(m1, h=12)

ts.plot(ventas, exp(f1$fitted),
        exp(f1$mean),
        col=c("red", "green", "brown"))
# Vemos que el modelo NO se ajusta tan correctamente a la serie.

# Lo haremos con Holt Winters como alternativa, se debe hacer la funcion. La cual
# se localiza en Stats Package. Su estructura es:

# HoltWinters(x, alpha = NULL, beta = NULL, gamma = NULL,
# seasonal = c("additive", "multiplicative"),
# start.periods = 2, l.start = NULL, b.start = NULL,
# s.start = NULL, optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
# optim.control = list())

h1 = HoltWinters(log(ventas), 
                 seasonal = c("multiplicative"))
h1

# Llamando a la variable se me arroja:
#  alpha: 0.01948251
# beta : 0
# gamma: 0.4828544

summary(h1)

f2=forecast(h1, h=12)

accuracy(f1) # SARIMA.

accuracy(f2) # el MAPE es mas pequeño en Holt Winters

par(mfrow=c(1,2))

ts.plot(ventas, exp(f1$fitted),
        exp(f1$mean),
        col=c("red", "green", "brown"))

ts.plot(ventas, exp(f2$fitted),
        exp(f2$mean),
        col=c("red", "green", "brown")) ### Se ajusta MUCHO mejor el Holt Winters.
