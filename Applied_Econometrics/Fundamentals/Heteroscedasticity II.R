# Heterocedasticidad 2

# -- Cargar librerias -- #
library(forecast)
library(lmtest)
library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(openxlsx)
file.choose()

data = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\demanda.xlsx")
attach(data)
names(data)

modelo = lm(YD~Q1+P1+P2+P3, data= data)
summary(modelo)
# Hacemos un modelo normal; empezamos a notar que puede existir la presencia de 
# heterocedasticidad, por lo que aplicamos logaritmos a ambos lados para tratar de
# atenuar el problema.(recordemos que los LN reducen los valores grandes)
modelo = lm(log(YD)~log(Q1)
            +log(P1)
            +log(P2)
            +log(P3), data= data)
summary(modelo)
# Notamos que todos los elementos son significativos y que el R2 cuadrado es muy alto
# pero el hecho de que el modelo sea tan bueno y todos los elementos son significativos
# tienes que empezar a sospechar de un problema como la heterocedasticidad

# - Metodo Grafico - #

qplot(x=modelo$fitted.values, 
      y=modelo$residuals)+geom_point()
# Pronostico de valores ajustados (pronosticos) vs residuos.
# Encontramos un patron sistematico tenemos que empezar a sospechar una hetero.
# Habiendo identificado el posible problema de heterocedasticidad se debe ir probando una
# contra una (en el metodo grafico) e identificar CUAL es significativa.
# recordemos que la que lo sea sera la que provoque la heterocedasticidad.

qplot(x=log(data$Q1), 
      y=modelo$residuals)+geom_point()  
# Q1 es la Variable donde mayor patron se identifica

qplot(x=log(data$P1), 
      y=modelo$residuals)+geom_point()

qplot(x=log(data$P2), 
      y=modelo$residuals)+geom_point()

qplot(x=log(data$P3), 
      y=modelo$residuals)+geom_point()

# - BP Test - #

# Ho: Homo
# H1: No Homo (Hetero)

bptest(modelo)

# BP = 2.0751, df = 4, p-value = 0.7219
# Pvalue NO es menor que 0.05 por lo tanto no rechazo la nula (hay homocedasticidad)
# Si bien; el metodo grafico me sugiere que puede haber hetero, el contraste 
# establecido me indica que NO hay.

# Dado que solo es un contraste vamos a utilizar uno más

# Non-Constant Error Variance

ncvTest(modelo)
# Se hace una prueba (a traves de la prueba F si las varianzas son mas o menos constantes)
# Chisquare = 0.6399877, Df = 1, p = 0.42372
# No se rechaza Ho tampoco. 
# Ho: Homo
# H1: No Homo
# Parece que el modelo NO presenta heterocedasticidad

# - Glesjer - #
# Mas comunes : Regresion de residuos en valor absoluto contra la variable
# que creemos crea la heterocedasticidad. En este caso probamos con todas.

modeloajustado = lm(abs(modelo$residuals)~log(Q1),
                    data = data)
summary(modeloajustado)
# log(Q1)     Estime: 0.05846 Error: 0.06882   t value: 0.850    pvalue: 0.403 
# No es significativo, si por el P-Value ni por el T Value.

modeloajustado = lm(abs(modelo$residuals)~log(P1),
                    data = data)
summary(modeloajustado)

# log(P1)     Estime: 0.03494  Error: 0.09081  t value: 0.385  pvalue: 0.703 
# No es significativo, si por el P-Value ni por el T Value.

modeloajustado = lm(abs(modelo$residuals)~log(P2),
                    data = data)
summary(modeloajustado)

# log(P2)     Estime: -0.01985 Error: 0.05200   t value: -0.382   pvalue: 0.706 
# No es significativo, si por el P-Value ni por el T Value.

modeloajustado = lm(abs(modelo$residuals)~log(P3),
                    data = data)
summary(modeloajustado)

# log(P3)     Estime: 0.04576 Error: 0.04457   t value: 1.027    pvalue: 0.313
# No es significativo, si por el P-Value ni por el T Value.

# No encontramos ninguna variable que cree heterocedasticidad

# - Goldfeld - Quantd
# Asumiendo que la data esta ordenada
gqtest(modelo, order.by = ~log(Q1), data = data)
gqtest(modelo, order.by = ~log(P1), data = data)
gqtest(modelo, order.by = ~log(P2), data = data)
gqtest(modelo, order.by = ~log(P3), data = data)

# Ninguna se muestra como significativa

# - Atenuando el problema - # (de haber problema)

# Minimos Cuadrados Ponderados #

modelomcp = lm(log(YD)~log(Q1)
               +log(P1)
               +log(P2)
               +log(P3), data = data, 
               weights = 1/log(Q1))
# Dividimos todas nuestras variables esta la variable que creemos que crea hetero
# en este caso no es ninguna, pero intentemos con Q1.
summary(modelomcp)

# Una forma alternativa a los MCP (invasivos y no recomendables) es el modelo hac

modelohac = coeftest(modelo, vcov = vcovHC(modelo))
modelohac
# nos arroja los errores libres de heterocedastificidad (los verdaderos errores)
# todos los elementos son significativos. 

# ¿Porque es importante tocar los coeficientes? Si nosotros tenemos, por ejemplo,
# 5 materias y en todas tenemos 10. El promedio sera 10 y ese sera el unico
# y mejor promedio. Lo mismo pasa con los coeficientes originales de los betas
# son los mejores promedios posibles a obtener (mejores estimadores insesgados)
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), 
         function(x) sqrt(diag(vcovHC(modelo, type = x)))))

# Con esto, quiero saber a traves de los 4 metodos, a partir de los 4 tipos de 
# correccion de la matriz varianza/covarianza como varian los errores estandar de 
# mi modelo.

# -- Influencia -- #

# Vamos a ver que tanta diferencia muestran los intervalos de confianza
# del primer modelo al modelo corregido (HAC)
# ¿Como hacemos eso? Construimos los IC y vemos que tan diferentes son...

sum1 = summary(modelo)

limitesuperior = sum1$coefficients[2]+1.96*sum1$coefficients[,2][2]
limiteinferior = sum1$coefficients[2]-1.96*sum1$coefficients[,2][2]

limitesuperior1 = modelohac[2]+1.96*modelohac[,2][2]
limiteinferior1 = modelohac[2]-1.96*modelohac[,2][2]

data.frame(limitesuperior, limiteinferior, limitesuperior1, limiteinferior1)
# Vemos las diferentes, el Intervalo de confianza del modelo original es:
# superior: 1.088085   inferior: 0.5613313
# Y para el modelo hac libre de heterocedasticidad:
# superior: 1.211635  inferior: 0.4377806
# Notamos que la amplitud entre las varianzas no difere mucho por lo que el
# problema de heterocedasticidad NO es tan grave. 
