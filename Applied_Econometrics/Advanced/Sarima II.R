# SARIMA 2

# tratamiento o abordo del problema de estacionalidad de una serie.

file.choose()

data = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\4. Modelos Econometricos para el Pronostico en Negocios\\Bases de Datos\\base.xlsx")


sales = data.frame(data[,2])

tssales = ts(sales, start = c(2006,1), 
             frequency = 12)

ts.plot(tssales) # se puede apreciar que esta variable tiene una tendencia, y cierto 
#comportamiento estacional, existen patrones a traves del tiempo.

# Test de Raiz Unitaria

monthplot(log(tssales)) # esta funcion nos dibujo segun el mes, por año. digamos si 
# empieza en el 2006 y termina en el 2013, va a dibujar los años, por ejemplo el primero
# es J (January) y nos dibuja el comportamiento de enero para todos los años y asi 
# subsecuentemente. Aqui se trabaja con logaritmos, aunque facilmente se podria trabajar 
# sin logaritmos. En la practica se tendran que realizar modelos con logaritmos y sin 
# logaritmos; en general cuando se ocupan logaritmos naturales en general ciertos indicadores
# mejoran sustancialmente porque se controla la varianza de la serie.

# con monthplot nos damos cuenta que en Abril (o X mes) se vende más que cualquier mes.
# entonces, definitivamente existe un comportamiento estacional. El monthplot calcula la media
# por año segun el mes. (Se puede ver en su media, es la más alta).

# Tambien existen contrastes para saber si a la serie en estudio hay que diferenciarla
# de manera estacional. 

# ARIMA (p,d,q) (P,D,Q)

nsdiffs(log(tssales), test=c("ocsb")) # R: 0 .. No me pide diferenciar la serie.
nsdiffs(log(tssales), test=c("ch")) # R: 0 .. No me pide diferenciar la serie.

# Osborn, Chui, Smith, and Birchenhall (OCSB) test.
# Canova-Hansen test

# Una vez identificada la parte estacional, es importante evaluar los contrastes 
# anteriormente utilizados de la parte ordinaria. 

kpsstest = ur.kpss(log(tssales), type=c("tau"),
                   lags = c("short"))
summary(kpsstest)

# Value of test-statistic is: 0.0418 

# Critical value for a significance level of: 
              #   10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216

# Se rechaza Ho (Raiz Unitaria) mi serie es estacionaria. Tambien se realiza con ERS 
# y PP, los contrastes indican que la serie si es estacionaria. 


ndiffs(log(tssales), test = c("pp")) # Nos indica que NO necesita ninguna diferenciacion.

# No hay que diferenciarla ni estacional ni ordinariamente. 

# Capturemos la forma del modelo (proceso generador de datos)

par(mfrow=c(1,2))
acf(log(tssales)) # SMA y MA (sin diferencias)
pacf(log(tssales)) # SAR y AR (sin diferencias)

# como se trata de una serie de tiempo estacional, es muy comun que el correlograma
# de la serie en la frecuencia (en este caso 12 porque es mensual), la barra sobresalga
# esta inclusive es otra forma de demostrar que la serie es estacional. 

# ¿Que hacemos en este caso que hay demasidas barras fuera de los IC? primero, probar
# con 1's para todo, es decir, SMA(1), SAR(1), AR(1), MA(1), e ir probando cual puede
# ayudarnos a explicar el modelo. Todos modelos más, digamos que tomamos las barras
# más grandes. La recomendacion inicial es empezar con 1's.

m1 = arima(log(tssales), 
           order=c(1,0,1),
           seasonal = list(order=c(1,0,1)))
m1

# se hace lo normal; la significancia, el accuracy, independiente de residuos, 
# pronosticos, etc.

# Coefficients:
         # ar1      ma1    sar1     sma1  intercept
        # 0.9847  -0.8677  0.9092  -0.2721    13.7758
# s.e.  0.0194   0.0776  0.0419   0.1286     0.4447

# Todos son significativos.
accuracy(m1)
#                      ME      RMSE        MAE       MPE      MAPE      MASE      ACF1
# Training set 0.01906134 0.1359341 0.08365181 0.1284045 0.5997905 0.3589133 0.1166357

# El MAPE es muy bueno, y los errores son chicos.

# Evaluar los residuos. 

Box.test(m1$residuals, type=c("Ljung-Box"))
# p-value = 0.2276 ; no rechazo Ho. (Residuos Independientes)

# evaluar que tan bien replica el modelo.

acf(m1$residuals)
pacf(m1$residuals)
# Ninguno sale de IC. El modelo replica muy bien la serie.

# Evaluar los circulos unitarios.
plot(m1)

# Evaluamos el comportamiento de los residuos. 

plot(m1$residuals) # estacional y todo en orden. 

# Pronostico

f1=forecast(m1, h=12)
hchart(f1)

# Pero la serie esta en logaritmos... tenemos que presentarla de manera 
# antilogaritmica (exponencial)

ts.plot(tssales, exp(f1$fitted),
     exp(f1$mean),
     col=c("red", "green", "brown"))

# Nuestro modelo debe tener:

# parametros significativos
# residuos independientes
# menor mae y mape
# siga un sentido coyuntural economico