# Principales contrastes de raíz unitaria

# hay diferentes contrastes para evaluar estacionariedad o no raiz unitaria en la serie.

# CONTRASTES DE RAÍZ UNITARIA (NO ESTACIONARIEDAD) CASO 1.-

# Cargamos la BD

data = read.table("http://www.betametrica.ec/wp-content/uploads/2017/02/PIB.txt")

# La transformamos a Series de Tiempo

tsdata = ts(as.vector(as.matrix(data)), start = c(2000, 1),
            end = c(2013, 3), 
            frequency = 4)
attach(data)
tsdata      
plot(tsdata)            

ts.plot(tsdata)

# Haremos uso del tests ur.df que significa:
# Dickey-Fuller Unit Root Test
# El cual realiza la prueba de raíz unitaria Dickey-Fuller aumentada.

adftest = ur.df(tsdata, type =c("trend"),
                selectlags = c("BIC"))

# bayesian information criterion: Decimos a R que seleccione el num de rezagos utilizando este criterio
# Contraste que incorando esta tendencia.

summary(adftest)
                
# Regla

# Si el valor calculado es mayor que el valor critico (tabla) se rechaza Ho.

# Ho: Raiz Unitaria
# H1: No Raiz Unitaria

# Value of test-statistic is: -2.0557 6.5667 2.8903 

# Critical values for test statistics: 
  # 1pct  5pct 10pct
# tau3 -4.04 -3.45 -3.15
# phi2  6.50  4.88  4.16
# phi3  8.73  6.49  5.47

# En este caso vemos solo valores absolutos. Y 2.0557 NO es mayor que -4.04 -3.45 -3.15
# entonces NO se rechaza Ho.
# Y con esto decimos que la serie NO es estacionaria.
# Raiz Unitaria: No estacionariedad
# Nosotros queremos que tenga estacionariedad NO debe tener raiz unitaria.

# Ya hemos comprobado la NO estacionariedad (Raiz Unitaria) con Dickey-Fuller
# ahora lo haremos con Phillips Perron. 
# Nota: Unit Root = Raiz Unitaria.

# La estructura es: ur.pp(x, type = c("Z-alpha", "Z-tau"), model = c("constant", "trend"),
# lags = c("short", "long"), use.lag = NULL)

# Donde X es eñ vector probado para Raiz Unitaria, type es el tipo de prueba
# que queremos, model, determina la parte determinista en la regresión de prueba,
# lags son retrasos utilizados para la corrección del término de error y use.lag,
# usa un número de retraso diferente, especificado por el usuario.

pptest = ur.pp(tsdata, type=c("Z-tau"),
               model=c("trend"), lags = c("short"))
summary(pptest)

# Value of test-statistic, type: Z-tau  is: -1.5947 

 # aux. Z statistics
# Z-tau-mu              0.0569
# Z-tau-beta            1.8851

# Critical values for Z statistics: 
#                    1pct      5pct     10pct
# critical values -4.134754 -3.493511 -3.175277

# Nosotros debemos ver el estadistico que nos arroja el test y evaluar 
# en valor absoluto al numero en contra de los valores criticos.
# es decir, aqui comparamos 1.594 vs 4.134754, 3.493511, 3.175277 (tomando
# todos como absolutos)

# Regla Phillips Perron

# si el valor calculado es mayor que los valores criticos se rechaza Ho.

# Ho: Raiz Unitaria
# H1: No Raiz Unitaria

# R: No se rechaza Ho, hay raiz unitaria y NO estacionariedad.

# KPSS: siguiente contraste.

# Estructura: 
# ur.kpss(y, type = c("mu","tau"), lags = c("short", "long", "nil"),
#        use.lag = NULL)
kpsstest = ur.kpss(tsdata, type=c("tau"), lags=c("short"))
summary(kpsstest)

# Value of test-statistic is: 0.151 

# Critical value for a significance level of: 
                # 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216

# La Hipotesis en este contraste es diferente a las demas.

# Ho: Estacionariedad (No Raiz Unitaria)
# H1: No Estacionariedad (Raiz Unitaria)


# Si el valor estadistico es mayor a los valores criticos o de tablas
# rechazo la Ho. 

# Cada valor estadistico corresponde al 90, 95, 97.5 y 99 % como normalmente
# se evalua al 95% de confianza, se evalua el estadistico contra el segundo
# resultado, en este caso es mayor por lo que se rechaza Ho. Y NO hay estacionariedad 
# y por lo tanto hay raiz unitaria.Rechazo que el PIB es estacionario.
# Ya hay 3 contrastes que me dicen que mi serie NO es estacionaria, sino que tiene 
# raiz unitaria.

# Elliott, Rothenberg \& Stock Unit Root Test: Contraste de Alta Potencia.

erstest = ur.ers(tsdata, 
                 type = c("DF-GLS"),
                 model = c("trend"),
                 lag.max = 4)
summary(erstest)

# Value of test-statistic is: -1.6574 

#Critical values of DF-GLS are:
              #    1pct  5pct 10pct
# critical values -3.58 -3.03 -2.74
# Si el estadistico es mayor a alguno de los valores de table rechace Ho.

# Ho: Raiz Unitaria
# H1: No raiz Unitaria

# A todos los niveles de confianza, no se puede rechazar Ho.
# (Una vez mas nos indica que existe un problema de Raiz Unitaria)
