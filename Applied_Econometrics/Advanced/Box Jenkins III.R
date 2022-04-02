# Box Jenkins 3

file.choose()

data = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Betametrica\\4. Modelos Econometricos para el Pronostico en Negocios\\Bases de Datos\\base.xlsx")

View(data)

exports = data.frame(data[,4]) 

attach(exports)

tsexports =ts(exports, start = c(2006,1),
              frequency = 12)
ts.plot(tsexports)
ts.plot(diff(tsexports,1))

# Verificar Raiz Unitaria

pptest = ur.pp(tsexports, type=c("Z-tau"),
               model=c("trend"), 
               lags = c("short"))
summary(pptest)

# Value of test-statistic, type: Z-tau  is: -4.0929 

# Critical values for Z statistics: 
                    # 1pct      5pct    10pct
# critical values -4.049369 -3.453494 -3.15209

# El valor calculado es mayor que al 95% de tablas asi que se rechaza
# Ho. (Raiz Unitaria)

# ERS test 

erstest = ur.ers(tsexports, 
                 type=c("DF-GLS"),
                 model =c("trend"),
                 lag.max=4)
summary(erstest)

# Value of test-statistic is: -3.5529 

# Critical values of DF-GLS are:
                # 1pct  5pct 10pct
# critical values -3.46 -2.93 -2.64

# Dado que -3.5529 es mayor que el de tablas al 95% por lo que se rechaza Ho
# que es raiz unitaria. Aun con presencia de quiebre estructrual, la serie
# es estacionaria. 

# Proceso Generador de Datos

par(mfrow=c(1,2))
acf(diff(tsexports,1))
pacf(diff(tsexports,1))

# Cuando las series presentan tendencia, es decir que se comportan de esta manera
plot(tsexports)
# Generalmente en su funcion de Autocorrelacion Simple, presenta la misma forma
# que podemos observar aqui, decrece lentamente hacia 0. Esto no quiere decir que
# tenemos que incorporar todas estas medias moviles, la funcion de Autocorrelacion Simple
# tiene esta forma cuando se presenta una tendencia pronunciada. Esto comunmente se
# soluciona agregando solo la primera media movil.
# Mientras que AR (4 (omitiendo AR 3)) si se puede identificar facilmente. 


modelo1 = arima(tsexports, order = c(4,0,1),
                fixed = c(NA,NA,0,NA,NA,NA))
summary(modelo1)

# Empezar pruebas de significancia e ir probando coeficientes, hasta que se 
# pueda replicar de la mejor manera el modelo.

modelo2 = arima(tsexports, order = c(4,0,1),
                fixed = c(NA,0,0,NA,NA,NA))
summary(modelo2)

# Hacemos una prueba T (mucho más eficiente que dividir 1 por 1)

coeficientes = as.matrix(subset(modelo2$coef, 
                                           abs(modelo2$coef) > 0))
errores = as.matrix(diag(modelo2$var.coef))

coeficientes/errores        

# ar1        3.288624e+02
# ar4       -5.292619e+01
# ma1       -4.263340e+01
# intercept  2.890599e-05

# Todos los valores son significativos.

accuracy(modelo2) # accuracy means exactitud.

#                    ME     RMSE      MAE        MPE     MAPE      MASE
#   Training set 8925.618 153506.9 119806.2 -0.3226874 7.795407 0.9012988
#                     ACF1
#  Training set -0.04514068

# El error de pronostico es de 7.79% (apropiado). Tiene una buena capacidad predictiva. 
# El error (RMSE) tambien es pequeño.

plot(modelo2) # nos arroja 2 graficos de circulos unitarios, tanto para
# la media movil como para los autorregresivos. Se espera que todos los
# puntos esten dentro del circulo unitario. Si visualizamos 1 fuera, hay que
# tener la certeza de que el proceso es explosivo. Este modelo respeta esta propiedad.

# Toca Evaluar nuestro proceso.
# Box.Test viene en la paqueteria Stats

Box.test(modelo2$residuals, 
         type=c("Ljung-Box"))
  
# Ho: Residuos Independientes
# H1: No independientes

# 	Box-Ljung test

# data:  modelo2$residuals
# X-squared = 0.21809, df = 1, p-value = 0.6405

# P value es mayor que 0.05 asi que no puedo rechazar Ho. Los residuos 
# son independientes.

# Verificar que los residuos tambien se encuentran dentro de los IC

acf(modelo2$residuals)
pacf(modelo2$residuals)

## Proyeccion. 

f1 = forecast(modelo2, h=4)

hchart(f1) # las exportaciones van a la baja. 

par(mfrow=c(1,1))

ts.plot(tsexports, f1$fitted, f1$mean, col=c("red", "blue", "green"))
# comparacion de que tan bien replica la serie mi modelo. 

# Otra opcion que tenemos para modelar este comportamiento de una serie
# tendencial marcado es incorporando una tendencia deterministica a esta serie


### ARIMA TO ARIMAX ###

tendencialineal = seq(0,length(tsexports)-1,1) # la tendencia, generara una frecuencia
# que empiece desde 0, que termine en n-1 (104 obs - 1) y cuente de 1 en 1.
# Length = tamaño

# Copiamos el mismo modelo que estabamos usando (modelo2) y le agregamos un 
# NA (para que calcule la tendencia que estamos agregando)
modelo3 = arima(tsexports, order = c(4,0,1),
                fixed = c(NA,0,0,NA,NA,NA,NA),
                xreg = tendencialineal)

# ¿Porque 7 NA's? porque son 4+1 de MA y AR, el intercepto y la variable
# exogena tendencial, 7 de los cuales debe calcular 5.
# Se convierte de un modelo ARIMA a un modelo ARIMAX porque añade una 
# variable exogena, que es nuestra tendencia lineal. 

modelo3

# Ya sabiamos que todos los coeficientes son significativos y 
# la nueva variable de tendencia tambien lo es.

plot(modelo3) # todos los puntos estan dentro del circulo unitario.

accuracy(modelo3)
# Los datos tiene un grado de error en pronostico muy bajo.

# Verifiquemos que los residuos sean independientes

Box.test(modelo3$residuals,
         type = c("Ljung-Box"))
# p-value de 0.7151 es mayor que 0.05 por lo que no puedo rechazar Ho 
# que dice que los residuos son independientes.

# como el modelo 3 tiene una variable exogena del lado derecho de la ecuacion
# quiero 2 proyecciones hacia adelante, por lo tanto es importante 
# añadir esos valores a la variable.

newdata = data.frame(tendencialineal=c(104,105))

f2 = forecast(modelo3,h=2,xreg=newdata)

par(mfrow=c(1,2))
plot(f1)
plot(f2)
# Comparamos pronosticos tanto de modelo 2 (ARIMA) vs modelo 3 (ARIMAX)
