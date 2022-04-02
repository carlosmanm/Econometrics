options(scipen = 999)

file.choose()

# Cargamos la base 


base = read.xlsx("C:\\Users\\Lenovo\\Desktop\\Base Investigacion.xlsx",
                 sheet = 3, detectDates = T) # Aplicacion de logaritmos
attach(base)

names(base)

str(base)

basets = ts(base, start = c(1996,1), 
            frequency = 4)

# Desestacionalizacion de variables con EVIEWS - TRAMO SEATS

ts.plot(Exportaciones.Petroleras.Totales) # No requirio ser desestacionalizada

ts.plot(Tipo.de.Cambio) # No requirio ser desestacionalizada

ts.plot(PIB.USA) # No requirio ser desestacionalizada

ts.plot(MME) # No requirio ser desestacionalizada

ts.plot(Produccion.Crudo.USA) # Desestacionalizada con TRAMO SEATS


# Graficos explicativos

qplot(Exportaciones.Petroleras.Totales, data = base, geom = "histogram")
qplot(Exportaciones.Petroleras.Totales, data = base, geom = "density")


# Graficos en Series de Tiempo - El comportamiento de las variables a través del tiempo.

ggplot(data = base, aes(x = Fecha, y = Exportaciones.Petroleras.Totales)) + 
  geom_line()

ggplot(data = base, aes(x = Fecha, y = Produccion.Crudo.USA)) + 
  geom_line()

ggplot(data = base, aes(x = Fecha, y = PIB.USA)) + 
  geom_line()

ggplot(data = base, aes(x = Fecha, y = Tipo.de.Cambio)) + 
  geom_line()

ggplot(data = base, aes(x = Fecha, y = MME)) + 
  geom_line()


# Graficos de Dependiente vs Explicativas

qplot(PIB.USA, Exportaciones.Petroleras.Totales, geom = c("point", "smooth"))

qplot(Produccion.Crudo.USA, Exportaciones.Petroleras.Totales, geom = c("point", "smooth"))

qplot(Tipo.de.Cambio, Exportaciones.Petroleras.Totales, geom = c("point", "smooth"))

qplot(MME, Exportaciones.Petroleras.Totales, geom = c("point", "smooth"))

scatterplot(MME, Exportaciones.Petroleras.Totales)

scatter3d(Tipo.de.Cambio, Exportaciones.Petroleras.Totales, 
          Produccion.Crudo.MX)

# Grafico de Corrrelación

ggpairs(base[2:6])


### -------------------------- Modelo ------------------------------###

# Ho: Las exportaciones petroleras de Mexico se han reducido debido a la
# evolucion del sector petrolero en Estados Unidos.

# Original Model

modelo2 = lm(Exportaciones.Petroleras.Totales~Tipo.de.Cambio+
               PIB.USA+Produccion.Crudo.USA+MME, data = basets)

summary(modelo2)

dwtest(modelo2) # Problema de Autocorrelación

# Cargamos la base corregida en EVIEWS y corremos el mismo modelo.

basefitted2 = read.xlsx("C:\\Users\\Lenovo\\Desktop\\Base Investigacion.xlsx",
                 sheet = 4, detectDates = T) 

fittedts = ts(basefitted2, start = c(1996, 1), 
              frequency = 4)

# Fixed Model

modelofitted2 = lm(basefitted2$Exportaciones.Petroleras.Totales.fitted~
                     basefitted2$MME.fitted+basefitted2$PIB.USA.fitted+
                     basefitted2$Tipo.de.Cambio.fitted+basefitted2$Produccion.Crudo.USA, 
                   data = fittedts)

summary(modelofitted2)

# Anova

anova(modelofitted2)

# Autocorrelacion

# Durbin Watson

# Ho: No Autocorrelacion
# H1: Autocorrelacion
# Si p-valor es menor a 0.05, se rechaza Ho.

plot(modelo2$residuals, type = "b")


plot(modelofitted2$residuals)

plot(modelofitted2$residuals, type="b")

qplot(x=c(tail(modelofitted2$residuals, -1), 0),
      y=modelofitted2$residuals)

# Dado que la prueba DW no es aplicable a los modelos con rezago se
# sugiere una prueba alternativa llamada. (la H de Durbin)

# Este Durbin Watson esta corregido para verificar autocorrelacion con variables rezagadas.  

durbinWatsonTest(modelofitted2) # No Autocorrelacion. 

# Breusch Godfrey

# Ho : No Autocorrelacion de Orden 1
# H1: Autocorrelacion de Orden 1.
# Si p-valor es menor a 0.05, se rechaza Ho.

bgtest(modelofitted2, order = 1) # No Autocorrelacion.

# Box-Pierce & Ljung-Box 

Box.test(modelofitted2$residuals) # No Autocorrelacion. 

### Detectar Heterocedasticidad 

# Ho : Homocedasticidad
# H1: Heterocedasticidad
# Si p-valor es menor a 0.05, se rechaza Ho.

bptest(modelofitted2) # Homocedasticidad


### Detectar Multicolinealidad

View(basefitted2)

cor(basefitted2[2:6])
ggpairs(basefitted2[2:6])

vif(modelofitted2) 

### Verificar Especificacion

# Ho: Modelo Correctamente Especificado
# H1: Contrario

# Si P-Value menor que 0.05 se rechaza 0.05

# Queremos que P-Value sea mayor a 0.05 para no rechazar Ho.

resettest(modelofitted2) # Modelo bien especificado

### Verificar normalidad de los errores

# Jarque Bera

# Ho : Normalidad de los Errores
# H1: No Normalidad de los Errores
# Si p-valor es menor a 0.05, se rechaza Ho.

jarque.test(as.vector(modelofitted2$residuals)) # Residuales~N(0,sigma^2)

jarque.test(as.vector(modelo2$residuals))

# Anderson Darling

# Ho : Normalidad de los Errores
# H1: No Normalidad de los Errores
# Si p-valor es menor a 0.05, se rechaza Ho.

ad.test(modelofitted2$residuals) # Se comportan normalmente

### Verificar estabilidad

# sctest es una función genérica para realizar / extraer pruebas de cambio 
# estructural basadas en varios tipos de objetos. 
# Primero, pruebas de cambio estructural basadas en estadísticas F 
# en modelos de regresión lineal (Fstats), procesos de fluctuación empírica 
# en modelos de regresión lineal (efp) y fluctuación empírica generalizada.

# Segundo, interfaces convenientes para llevar a cabo pruebas de cambio 
# estructural en modelos de regresión lineal. 

# Ho: No cambio estructural
# H1: Cambio Estructural
# Si p-value es menor a 0.05 se rechaza Ho

sctest(modelofitted2, type = "OLS-CUSUM") # Es estable

sctest(modelofitted2, type = "OLS-MOSUM")
# Pruebas de estabilidad con parametros

ols = efp(Exportaciones.Petroleras.Totales.fitted~
            MME.fitted+PIB.USA.fitted+
            Tipo.de.Cambio.fitted+Produccion.Crudo.USA, 
          data = fittedts, type = "OLS-CUSUM")
plot(ols)

# Ho: No Cambio Estrural: Es estable en los parametros a traves del tiempo
# H1: Cambio Estructural: No es estable en los parametros a traves del tiempo

sctest(ols) # Structural Change Test: No se rechaza Ho.

# grafico intuitivo, si el modelo NO es estable en los parametros; 
# un modelo siempre es  estable si esta sobre los Intervalos de Confianza 

olms = efp(Exportaciones.Petroleras.Totales.fitted~
             MME.fitted+PIB.USA.fitted+
             Tipo.de.Cambio.fitted+Produccion.Crudo.USA, 
           data = fittedts, type = "OLS-MOSUM") # mobile averange
plot(olms)

# Ho: No Cambio Estrural: Es estable en los parametros a traves del tiempo
# H1: Cambio Estructural: No es estable en los parametros a traves del tiempo

sctest(olms) # No se rechaza Ho.

# Misma interpretacion: se encuentran partes donde sobresalen de los 
# intervalos el modelo NO es estable a traves del tiempo. 
# (tambien por quiebres estructurales)


# Cambio Estructural

breakpoints(Exportaciones.Petroleras.Totales.fitted~
                             1, data = basefitted2)

# Breakpoints at observation number: 13, 34, 58, 74
# correspondientes a: 1999-01-01, 2004-04-01, 2010-01-01 y 2014-04-01.

breakpoints(Produccion.Crudo.USA~1, 
            data = basefitted2)

# Breakpoints at observation number: 15, 37, 52, 66, 80
# correspondientes a: 1999-07-01, 2005-01-01, 2008-10-01, 2012-04-01 y 2015-10-01.


breakpoints(MME~1, data = basefitted2)

breakpoints(Tipo.de.Cambio~1, data = basefitted2)

breakpoints(PIB.USA~1, data = basefitted2)

breakpoints(Exportaciones.Petroleras.Totales~Produccion.Crudo.USA+
              MME+Tipo.de.Cambio+PIB.USA, data = basefitted2)

breakpoints(basefitted2$Exportaciones.Petroleras.Totales.fitted~basefitted2$Produccion.Crudo.USA)

# Breakpoints at observation number: 26 y 64
# correspondientes a : 2002-04-01 y 2011-10-01.

## Criterios de schwartz

BIC(modelofitted2) 

AIC(modelofitted2) 


# Causalidad de Granger

# Ho: Los valores anteriores de X no ayudan a predecir a Y
# H1: Los valores anteriores de X ayudan a predecir a Y

grangertest(Exportaciones.Petroleras.Totales~PIB.USA)

grangertest(Exportaciones.Petroleras.Totales~MME)

grangertest(Exportaciones.Petroleras.Totales~Tipo.de.Cambio, order =2)

grangertest(Exportaciones.Petroleras.Totales, Produccion.Crudo.USA, order=2)

# FORECAST 

accuracy(modelofitted2)

# MAPE: Mean Absolute Percentage Error

# Valores ARIMA:

# FORECAST EXPORTACIONES (MARGEN DE ERROR PREDICTIVO: 1.164431)

#          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019 Q3       8.243597 8.079244 8.407949 7.992241 8.494952
# 2019 Q4       8.222500 7.990071 8.454930 7.867030 8.577971
# 2020 Q1       8.255279 7.970612 8.539946 7.819918 8.690640
# 2020 Q2       8.265041 7.936336 8.593747 7.762330 8.767753

exp(8.243597)^2
exp(8.222500)^2
exp(8.255279)^2
exp(8.265041)^2

# FORECAST EXCHANGE RATE (MARGEN DE ERROR PREDICTIVO: 2.530161)

#         Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019 Q3       1.564321 1.507239 1.621404 1.477021 1.651622
# 2019 Q4       1.571013 1.504318 1.637708 1.469012 1.673014
# 2020 Q1       1.576950 1.506341 1.647559 1.468963 1.684937
# 2020 Q2       1.582509 1.509758 1.655260 1.471246 1.693772 

# FORECAST MME (MARGEN DE ERROR PREDICTIVO: 6.369927)

#         Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019 Q3       2.167350 1.956654 2.378046 1.845118 2.489582
# 2019 Q4       2.153095 1.882524 2.423666 1.739292 2.566897
# 2020 Q1       2.153095 1.859717 2.446473 1.704412 2.601778
# 2020 Q2       2.153095 1.838559 2.467630 1.672054 2.634135


# FORECAST PIB USA (MARGEN DE ERROR PREDICTIVO: 0.03113188)

#         Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019 Q3       12.43919 12.43206 12.44631 12.42829 12.45009
# 2019 Q4       12.44220 12.43305 12.45135 12.42821 12.45619
# 2020 Q1       12.44529 12.43434 12.45623 12.42855 12.46203
# 2020 Q2       12.44836 12.43590 12.46082 12.42930 12.46742

# FORECAST CRUDE PRODUCTION (MARGEN DE ERROR PREDICTIVO: 0.333461)

#        Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019 Q3       5.545584 5.513962 5.577206 5.497223 5.593946
# 2019 Q4       5.557708 5.515952 5.599463 5.493848 5.621567
# 2020 Q1       5.569831 5.518902 5.620760 5.491942 5.647720
# 2020 Q2       5.581954 5.522308 5.641600 5.490734 5.673175

expects = data.frame("Exportaciones.MXX" =c(8.243597, 8.222500, 8.255279, 8.265041),
                     "Produccion.de.Estados.UnidosX"=c( 5.545584, 5.557708, 5.569831, 5.581954))

tsexpects = ts(expects, start = c(2019, 3), frequency = 4)

ts.plot(expects$Produccion.de.Estados.UnidosX, expects$Exportaciones.MXX, data = tsexpects)

scatterplot(basefitted2$Exportaciones.Petroleras.Totales.fitted, basefitted2$Produccion.Crudo.USA)

