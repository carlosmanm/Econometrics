# Trabajo Final

# Carlos Manuel Vázquez Muñoz

# Sección A

# Ve a la página del Banco Central de tu país y obtén cualquier variable de tu interés 
# con frecuencia mensual o trimestral, mínimo 60 datos.
# Realiza gráficos necesarios para comprender el comportamiento de la variable en 
# estudio. Comenta en un par de párrafos lo que consideres. Recuerda que debes 
# reportarlos y explicarlos. Realiza los contrastes de raíz unitaria aprendidos.
# Reporta los resultados realiza los respectivos comentarios

# Sección B

# Realiza el análisis de la función de autocorrelación de la variable 
# (original o transformada, según lo que indique las pruebas) y obtén el posible mejor 
# modelo. Considera analizar si la serie tiene algún comportamiento estacional para que
# sea considerado en este literal. Siendo el mejor modelo (ARIMA o SARIMA según sea 
# el caso), valida los resultados. Construye 4 pronósticos fuera de la muestra.
# Con la misma variable, corre un auto.arima y realiza 4 pronósticos fuera de la 
# muestra, con sus intervalos de confianza. Con la misma variable, construye un modelo
# HoltWinters según el comportamiento de la variable y realiza 4 pronósticos fuera de 
# la muestra.En un gráfico de lineas, coloca la variable original, los valores 
# ajustados del modelo final y los valores ajustados del modelo automático, más los 
# resultados del pronóstico usando HoltWinters. Comenta los resultados.Este trabajo 
# es entregable y forma parte de la calificación para que usted tenga el estatus de 
# aprobado. Será revisado en un tiempo de 24-72h conforme vayan llegando las solicitudes.

# Favor enviar los resultados a capacitaciones@betametrica.com.ec con lo siguiente

# Script desarrollado en archivo de r.
# Resultados de lo solicitado con captura de pantalla de forma ordenada en un archivo 
# de WORD.


# Cargar la base

file.choose()

pib =read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\4. Modelos Econometricos para el Pronostico en Negocios\\Bases de Datos\\PIB trimestral - Trabajo Final.xlsx", 
                detectDates = T, 
                sheet = 1)
# En este caso, tomamos el PIB trimestral de México como nuestra Base de Datos.

View(pib)

# Transformamos la base de datos en Series de Tiempo del primer trimestre de 1993 a
# el segundo trimestre del 2019.

tspib = ts(pib/100, start = c(1993,1), 
               frequency = 4)

# Metodo grafico

# Mediate el plot del PIB en series de tiempo podemos observar el comportamiento 
# creciente de la serie.

ts.plot(tspib)
ts.plot(diff(tspib, 1))

monthplot(tspib) # Verificamos si es estacional.

# Verificar Raiz Unitaria

# PP test

pptest = ur.pp(tspib, type=c("Z-tau"),
               model=c("trend"), 
               lags = c("short"))
summary(pptest)

# Ho: Raiz Unitaria
# H1: No Raiz Unitaria


# Value of test-statistic, type: Z-tau  is: -2.6772 

# Critical values for Z statistics: 
                    # 1pct      5pct     10pct
# critical values -4.047655 -3.452684 -3.151616

# El valor calculado (-2.6772 ) NO es mayor que el de tablas(-4.047655) asi que NO 
# se rechaza Ho. (Raiz Unitaria)


# ERS test 

erstest = ur.ers(tspib, 
                 type=c("DF-GLS"),
                 model =c("trend"),
                 lag.max=4)
summary(erstest)

# Ho: Raiz Unitaria
# H1: No Raiz Unitaria


#Value of test-statistic is: -2.7504 

# Critical values of DF-GLS are:
                # 1pct  5pct 10pct
# critical values -3.46 -2.93 -2.64

# El valor calculado (-2.7504) NO es mayor que el de tablas(-3.46) asi que NO 
# se rechaza Ho. (Raiz Unitaria)


# KPSS test

kpsstest = ur.kpss(tspib, 
                   type =c("tau"),
                   lags=c("short"))
summary(kpsstest)

# Ho: Estacionariedad (No Raiz Unitaria)
# H1: No Estacionariedad (Raiz Unitaria)

# Si el valor estadistico es mayor a los valores criticos o de tablas
# rechazo la Ho.

# Value of test-statistic is: 0.143 

# Critical value for a significance level of: 
                 # 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216

# Dado que el estadistico (0.143) en este caso es mayor que el valor de tablas (0.119) 
# se rechaza Ho. NO hay estacionariedad y por lo tanto hay raiz unitaria.
# Rechazo que el PIB es estacionario.


 # Corroborar el numero de diferencias necesarias. 

ndiffs(tspib, test=c("pp")) # R: 1

ndiffs(tspib, test=c("adf")) # R: 1

ndiffs(tspib, test=c("kpss")) # R: 1


# Una vez aqui podemos reafimar que la serie no esta estacional y le pedimos a R que nos
# de el numero de diferencias estacionales, en este caso nos arroja 0 para las 2 
# dado que no se trata de una variable estacional. 

nsdiffs(log(tspib), test=c("ocsb")) # R: 0

nsdiffs(log(tspib), test=c("ch")) # R: 0

# Proceso Generador de Datos

par(mfrow=c(1,2))
acf(diff(tspib, 1)) # Aplicamos 1 diferencia.
pacf(diff(tspib, 1))

modelo1 = arima(tspib, order=c(2,1,1))

modelo1

# Coefficients:

# Coefficients:
          # ar1     ar2     ma1
      # -0.2143  0.2758  0.8158
# s.e.   0.2350  0.1728  0.1996

# A simple vista podemos identificar que los coeficientes AR NO son significantes.
# podemos intentar omitiendo los AR y dejando solo MA(1). Resulta que en nuestros 
# graficos el MA(1) es mucho más pronunciado y el unico que tiene significancia.

modelo2 = arima(tspib, order=c(0,1,1))

modelo2

# Coefficients:
#        ma1
#       0.5619
# s.e.  0.0826

# Prueba T de significancia.

0.5619/0.0826 # R : 6.802663

# Nuestra variable es significativa.

# Ahora crearemos 2 modelos más, uno con Auto Arima y otro con Holt Winters.

# Modelo Auto Arima.

modeloauto = auto.arima(tspib)

modeloauto


# Coefficients:
  #      ma1     drift
#       0.4992  802.6799
# s.e.  0.0908  211.3731
  
# El modelo que el Auto Arima nos arroja es el mismo que el nuestro con un ARIMA(0,1,1)
# solo que agrega un intercepto. Verifiquemos la significancia de ambos. 


# Hacemos una prueba T
  
0.4992/0.0908 # R: 5.4977
802.67/211.37 # R: 3.8009

# Tanto como la Media Movil de Orden 1 como el Intercepto en el Auto Arima 
# tienen significancia.

# Modelo Holt Winters

modelohw = HoltWinters(tspib, alpha = 0.9690624, 
                       beta = 0.0210329, 
                       gamma = 1)
modelohw

# El modelo nos arroja estos parametros.

# alpha: 0.9690624
# beta : 0.0210329
# gamma: 1

summary(modelohw)

# Evaluacion del modelo. 

accuracy(modelo2)

#                ME     RMSE      MAE       MPE      MAPE     MASE        ACF1
# Training set 512.6727 1533.665 1095.291 0.3664797 0.8159088 0.794816 -0.06751716

# El MAPE para el modelo propuesto es pequeño. Es decir, el error de pronostico
# es menor al 1%. Tiene una buena capacidad predictiva. El error (RMSE) tambien es
# pequeño. Mismo caso con el MAE, en general los datos del accuracy son pequeños.

accuracy(modeloauto)

#                ME     RMSE      MAE         MPE      MAPE      MASE         ACF1
# Training set 2.530768 1442.452 943.2401 -0.00506484 0.7046354 0.2078949 -0.005461405

# El MAPE para el modelo Auto Arima es pequeño. Es decir, el error de pronostico
# es menor al 1%. Tiene una buena capacidad predictiva. El error (RMSE) tambien es
# pequeño. Mismo caso con el MAE, en general los datos del accuracy son pequeños.

# El accuracy del modelo Holt Winters lo haremos hasta su forecast.

# Graficos de circulos unitarios.
par(mfrow=c(1,1))
plot(modelo2) # El punto que representa el MA(1) se encuentra dentro del circulo.

plot(modeloauto) # El punto del modelo Auto Arima se encuentra dentro del circulo.

# Un plot del modelo Holt Winters no nos arroja un grafico de circulos unitarios.
# sino un ajuste del modelo con respecto a la serie en estudio, en este caso la 
# replica nos dice que su ajuste es muy bueno. Nos falta ver el grado de error predictivo
# en su forecast, donde podremos comprobar cual es mejor.

plot(modelohw) 

# Evaluacion de los residuos.

Box.test(modelo2$residuals, 
         type=c("Ljung-Box"))

# Ho: Residuos Independientes
# H1: No independientes

# 	Box-Ljung test

# data:  modelo2$residuals
# X-squared = 0.49701, df = 1, p-value = 0.4808

# P value es mayor que 0.05 asi que no puedo rechazar Ho. Los residuos 
# son independientes.

Box.test(modeloauto$residuals, 
         type=c("Ljung-Box"))

# 	Box-Ljung test

# data:  modeloauto$residuals
# X-squared = 0.003252, df = 1, p-value = 0.9545

# P value es mayor que 0.05 asi que no puedo rechazar Ho. Los residuos 
# son independientes.

# En mismo caso, no se puede realizar una evaluacion de residuos para el modelo
# Holt Winters. Asi que la evaluacion de estos, tanto por Box.Test como para 
# el ajuste de los residuos por PACF y ACF solo se realizara para el Auto Arima
# y mi modelo propuesto.
par(mfrow=c(1,2))
acf(modelo2$residuals)
pacf(modelo2$residuals)

# Para el modelo propuesto, ninguna barra sobrepasa los IC, el modelo se ajusta muy
# bien a la serie. Ahora probemos para el modelo Auto Arima.
par(mfrow=c(1,2))
acf(modeloauto$residuals)
pacf(modeloauto$residuals)

# Para el modelo Auto Arima se trata del mismo caso, ninguna barra sobrepasa los IC
# y ademas, se puede visualizar que estas barras estan mucho mas lejanas de los IC
# en este grafico.

# Proyeccion. 

Fmodelo1 = forecast(modelo2, h=4)

Fmodeloauto = forecast(modeloauto, h=4)

Fmodelohw = forecast(modelohw, h=4)

# Evaluacion de los modelos, propuesto, Auto Arima y Holt Winters.
# En este caso solo haremos caso al error de prediccion, MAPE:

accuracy(Fmodelo1) # MAPE : 0.8159088

accuracy(Fmodeloauto) # MAPE : 0.7046354

accuracy(Fmodelohw) # MAPE : 0.8450342

# El mejor MAPE lo ostenta el modelo Auto Arima. Se supone es el mejor modelo predictivo.

# Evaluemos los datos que nos arrojan, es decir los valores de prediccion. Estas 
# predicciones nos arrojan datos del 3er y 4to trimestre del PIB mexicano, ademas del
# 1er y 2do trimestre del 2020. PRONOSTICO FUERA DE LA MUESTRA CON IC.

Fmodelo1

#         Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019 Q3         185424 183449.3 187398.8 182403.9 188444.2
# 2019 Q4         185424 181761.6 189086.5 179822.9 191025.2
# 2020 Q1         185424 180635.8 190212.2 178101.1 192747.0
# 2020 Q2         185424 179728.4 191119.7 176713.3 194134.8

Fmodeloauto

# Point Forecast      Lo 80    Hi 80    Lo 95    Hi 95
# 2019 Q3       185975.5 184100.2 187850.8 183107.4 188843.5
# 2019 Q4       186778.2 183398.6 190157.7 181609.6 191946.7
# 2020 Q1       187580.8 183184.7 191977.0 180857.6 194304.1
# 2020 Q2       188383.5 183165.2 193601.8 180402.9 196364.2


Fmodelohw

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019 Q3       186536.1 184270.5 188801.8 183071.1 190001.2
# 2019 Q4       187363.7 184176.4 190551.0 182489.2 192238.3
# 2020 Q1       187814.2 183890.7 191737.7 181813.8 193814.6
# 2020 Q2       188941.7 184376.4 193506.9 181959.7 195923.6


# Visualizacion en Highcharter para todas las predicciones.

hchart(Fmodelo1)

hchart(Fmodeloauto)

hchart(Fmodelohw)

# Ahora verifiquemos visualmente COMO se ajustan los 3 modelos.


# Modelo Propuesto 

par(mfrow=c(1,1))

ts.plot(tspib, Fmodelo1$fitted,
        Fmodelo1$mean,
        col=c("red", "green", "brown"))

# Modelo Auto Arima

ts.plot(tspib, Fmodeloauto$fitted,
        Fmodeloauto$mean,
        col=c("red", "green", "brown"))

# Modelo Holt Winters

ts.plot(tspib, Fmodelohw$fitted,
        Fmodelohw$mean,
        col=c("red", "green", "brown"))

# Conclusiones: He realizado un analisis en series de tiempo para el PIB de México, 
# este dato viene de manera trimestral y se encuentra disponible en la Pagina del INEGI. 
# He identificado mediante la metodologia Box Jenkins el comportamiento de la variable
# mediante el metodo grafico, la identificacion de una serie estacional (el PIB de México), 
# NO es una serie estacional, sin embargo no presenta estacionariedad, se presento el 
# problema de la Raiz Unitaria, sin embargo, este problema se resolvio aplicando una
# primera diferencia, ademas, eso me hizo identificar que el modelo que debia ocupar
# era un ARIMA (p,d,q), una vez realizado esto, se creo el modelo, se realizaron 2
# pruebas, el segundo modelo propuesto fue el mejor, se hicieron las pruebas de 
# significancia, se evaluaron los residuos, y se realizo la proyeccion, esta misma 
# metodologia, se ocupo tanto para el modelo propuesto, el modelo creado por Auto Arima, 
# en el cual, Forecast nos frece en su paqueteria una opcion que le puede generar un 
# posible mejor modelo, esto evita el proceso de modelamiento, con el cual ya no tuve que
# hacer los contrastes de estacionariedad, ya no tendria que hacer el proceso
# generador de datos (ACF y PACF). Y finalmente el modelo adicional Holt Winters, el cual
# te da la facilidad de que los modelos HW pueden se utilizados para cualquier serie 
# temporal que  presente al menos dos componentes pronunciados: Tendencia y Estacionalidad.

# Finalmente hice la evaluacion, los 3 modelos son MUY buenos, tienen una muy buena capacidad
# predictiva, y finalmente el unico que lleva una ligera ventaja sobre los otros 2
# es el Auto Arima. 
