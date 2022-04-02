# Modelador automático Forecast

# Forecast ofrece en su paqueteria una opcion que le puede generar un posible
# mejor modelo, esto evita el proceso de modelamiento, ya no tendria que
# hacer los contrastes de estacionariedad, ya no tendria que hacer el proceso
# generador de datos (ACF y PACF). Se recomienda trabajar de manera complementaria
# y no definitiva.

file.choose()

data = read.table("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\4. Modelos Econometricos para el Pronostico en Negocios\\Bases de Datos\\PIB.txt")

tsdata = ts(data, start = c(2001,1), 
            frequency=4)
ts.plot(tsdata)

modelo1 = auto.arima(tsdata) # tan sencillo como poner auto.arima(timeseriesbd)

# Nos dice que el mejor modelo de todos seria este:

# ARIMA(1,1,0) with drift 

#Coefficients:
       # ar1      drift
      #0.3821  145012.66
#s.e.  0.1254   24718.58

# sigma^2 estimated as 1.336e+10:  log likelihood=-705.2
# AIC=1416.4   AICc=1416.88   BIC=1422.36

# Un modelo AR(1), I(1), MA(0), con un coefieicnte correspondiente y
# el intercepto. 

accuracy(modelo1)
plot(modelo1)

Box.test(modelo1$residuals, type=c("Ljung-Box"))

# Residuos independientes.
par(mfrow=c(1,2))
acf(modelo1$residuals)
pacf(modelo1$residuals)

# Ninguna sale de IC. El modelo replica bien la serie original.

f1 = forecast(modelo1, h=4)

hchart(f1)
par(mfrow=c(1,1))
ts.plot(tsdata, f1$fitted, f1$mean, col=c("red", "blue", "green"))

