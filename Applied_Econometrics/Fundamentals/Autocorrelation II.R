# Autocorrelacion 2.

# AUTOCORRELACIÓN

# Causas, consecuencias, identificación y tratamiento

# ------ Cargar librerias. -------- #

library(dynlm)
library(forecast)
library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(zoo)
library(lmtest)

# ------ Cargar Base de Datos. -------- #

file.choose()

exports = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\exportaciones.xlsx")
attach(exports)
names(exports)

# ------ Estimar el Modelo. -------- #
# Crear el modelo y Estimarlo (Regresion con Rezagos, preferentemente)
# -- 1 -- Modelo de Regresion Común. 
modelo = lm(EXUSA~TCR+YPUSA, data=exports)
summary(modelo)
# Parece que el modelo es bastante adecuado, dado que pasa las pruebas T, F y el R es bueno.
# Lo primero que se debe de hacer es desconfiar dado que estamos ocupando series de tiempo
# las cuales comunmente sufren autocorrelacion. 

# -- 2 -- Modelo de Regresion Rezagado, NO hay fechas, asi que hay que asignar de manera aleatoria
# por ejemplo aqui le ponemos que empezo desde el 2003 y por el num. de datos termina en 2009. 
stmodelo = ts(exports, start = c(2003, 2),
              frequency = 12)
stmodelo # Visualizamos
ts.plot(stmodelo) # Graficamos.
ts.plot(stmodelo[,2]) # solo para probar que funcione el time series. 
# Creamos el modelo, la data debe ser la serie de tiempo NO el data frame. 
modelolag = dynlm(EXUSA~TCR+YPUSA+L(EXUSA,1),
                  data = stmodelo) 
summary(modelolag) # Estimamos.

# ------ Identificacion del problema. -------- #
#  (Metodo Grafico, Durbin-Watson, o Breusch Godfrey). 
# -- 1 -- Metodo Grafico
plot(modelo$residuals, type="b")
qplot(x=c(tail(modelo$residuals, -1), 0),
      y=modelo$residuals)
# Podemos visualizar una tendencia creciente en los residuos. (ya podemos identificar una autocorrelacion)

# -- 2 -- Durbin-Watson
dwtest(modelo)
# Ho: No Autocorrelacion
# H1: Autocorrelacion
# DW = 0.68193, p-value = 1.089e-12 
# El P-value es significativo por lo que se RECHAZA la Ho.
# El DW nos da un numero que tiende mas a 0 que a 2, por lo que es Autocorrelacion Positiva

# EL DURBIN WATSON NO SE PUEDE OCUPAR PARA MODELO LAG. 

# -- 3 -- Breusch Godfrey

bgtest(modelo, order = 1) # Para modelo normal


# Ho : No Autocorrelacion de Orden 1
# H1: Autocorrelacion de Orden 1.
# LM test = 34.428, df = 1, p-value = 4.422e-09
# El p-value es MUY significativo, por lo tanto se RECHAZA Ho.
# con un rezago NO fue suficiente, aun hay problema de autocorrelacion. 
# R: Hay Autocorrelacion. 

bgtest(modelolag, order = 1)

# Ho : No Autocorrelacion de Orden 1
# H1: Autocorrelacion de Orden 1.
# LM test = 4.555, df = 1, p-value = 0.03282
# El p-value es MUY significativo, por lo tanto se RECHAZA Ho.

# ------ Atenuar Problema. -------- #
# (Minimos Cuadrados Generalizados o Dewey West)
# -- 1 -- MCG
modelfitted = gls(model = EXUSA~TCR+YPUSA, 
                  correlation = corAR1(), 
                  method = "ML")
summary(modelfitted)
# El Phi es de 0.6652572 y con esto se ha reajustado. Ya no hay Autocorrelacion PERO los coeficientes
# han cambiado.

# No lo haremos para ModeloLag.

# -- 2 -- Dewey West

coeftest(modelo, vcov = NeweyWest(modelo))
# Nos da los mismos betas, con diferentes errores y sin Autocorrelacion. 

# Si se puede ocupar el Dewey Watson con un Rezago, PERO es mejor ocupar 1 de los 2, en este caso si
# el rezago no te funciona el Dewey West con un modelo lo hara, te dara los coeficientes originales.
# Es mas, se tiene entendido que son 2 metodos diferentes:
# O se atenua el problema por el método de los rezagos (no siempre suficiente) y por el método NW. (siempre lo atenua)

# y sin Autocorrelacion. 