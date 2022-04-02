# Simulación de procesos ARIMA

# No olvidemos que el arima.sim viene de arima simulation.

arma1 = arima.sim(list(order=c(1,0,1), 
                       ar = 0.4, 
                       ma = 0.98), 
                  n=100) 
# El 1, 0, 1 significa que todavia no vamos a diferenciar la serie. Porque esos 3 numeros hacen
# referencia al p, i, q. Entonces esto seria un ARMA (1,1)
plot(arma1)


# Ahora ocupemos una base de datos.

data = read.table("http://www.betametrica.ec/wp-content/uploads/2017/02/PIB.txt")

library(forecast)

attach(data) # La atachamos

names(data)

# La transformamos a series de tiempo.

tsdata = ts(as.vector(as.matrix(data)), start = c(2000, 1),
            end = c(2013, 3), 
            frequency = 4)
ts.plot(tsdata)
# la serie tiene una tendencia pronunciada.

# ¿ Y si le aplicamos una primera diferencia?

plot(diff(tsdata,1)) # aplicamos la primera diferencia y esta aplana el grafico.

arima1 = arima.sim(list(order=c(1,1,1), 
                       ar = 0.45, 
                       ma = 1.2), 
                  n=100) 
# Ahora creamos un modelo con una diferencia (la primera diferencia). YA no es ARMA
# es ARIMA, no olvidar eso.
plot(arima1)

arima2 = arima.sim(list(order=c(2,1,1), 
                        ar =c(0.45, 0.23), 
                        ma = 1.2), 
                   n=100) 
# aqui nosotros simulamos un modelo ARIMA: autorregresivo de orden 2, media movil de 
# orden 1, al poner un AR(2), I(1):(Diferencia), MA(1), el orden va 2, 1, 1. 
# y en AR debemos especificar los coeficientes que NO sumen 1 o mas de 1.
plot(arima2) # si ploteamos varias veces recordar que nos va a arrojar siempre
# distintos graficos porque son valores aleatorios (o simulados)

arima3 = arima.sim(list(order=c(2,1,1), 
                        ar =c(0.45, -0.93),  # te acepta el 0.93, porque la suma en valor
                        ma = 1.2),  # absoluto es menor a 1. Ya que, de ser positivo
                   n=100)       # NO seria estacionario, dado que la suma seria mayor a 1.
plot(arima3)

# Y podemos seguir modificando, digamos que necesitamos una media movil de 2 y un 
# autorregresivo de 3 con una primera diferencia.

arima4 = arima.sim(list(order=c(3,1,2), 
                        ar=c(0.45, -0.93, 0.23), 
                        ma =c(1.2, 0.55)), 
                   n=1000)
plot(arima4)                   
