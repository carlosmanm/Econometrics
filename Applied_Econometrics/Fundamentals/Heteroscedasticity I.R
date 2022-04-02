# Heterocedasticidad 1

install.packages("sandwich")

# -- Cargar librerias -- # 

library(lmtest)
library(zoo)
library(sandwich)
library(nlme)
library(ggplot2)
library(openxlsx)
library(car)

# -- Cargar la Base de Datos -- #

file.choose()

data = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\gastocomida.xlsx")
attach(data)
names(data)
# gasto en alimentos y gasto total en 55 familias. 

# dada la hetegeneidad en las familias (en gasto, consumo, ingreso, etc) se puede hacer heterocedasticidad

modelo = lm(galimentos~gtotal, data=data) 
summary(modelo)
# si la varianza esta sobreestimada nuestros contrastes de significacia NO son reales.

# -- Identificacion de Heterocedasticidad -- #

# Metodo Grafico

qplot(x=modelo$fitted.values, 
      y=modelo$residuals)+geom_point()
# Valores Ajustados (X's proyectadas o pronostico de las X's) vs Residuos
# Evaluacion de algun tipo de comportamiento o patron sistematico. (Lo encontramos)

qplot(x=data$gtotal, 
      modelo$residuals)+
  geom_point()
# Nuestro X normal (No proyectado) vs Residuos
# Evaluacion de algun tipo de comportamiento o patron sistematico. (Lo corroboramos)

# -- Contrastes Formalmente establecidos -- #

# Ho: Homocedasticidad
# H1: No homocedasticidad (heterocedasticidad)

# Si p-value se rechaza Ho. 

bptest(modelo)
# Nos arroja 
# data:  modelo
# BP = 6.4293, df = 1, p-value = 0.01123
# Se rechaza Ho. (Se rechaza que hay Homocedasticidad, por lo tanto el problema tiene un 
# problema de Heterocedasticidad)

# Non-Constant Error Variance. 
ncvTest(modelo)
# Variance formula: ~ fitted.values 
# Chisquare = 7.183056, Df = 1, p = 0.0073595
# El p-value es altamente significativo por lo que se debe rechazar Ho (No hay Homocedasticidad)

# -- Glesjer -- # 
# Una vez que se sabe que hay hetecedasticidad, es importante saber CUAL es la que esta afectando.
# Y justo es lo que hace este contraste.

modelof = lm(abs(modelo$residuals)~gtotal,
             data=data)
# Lo que se hace aqui es enfrentar el valor absoluto de los residuos vs variable (en este caso solo tenemos
# una que analizar, pero si tuvieramos por ejemplo, "geducacion", seria evaluar: abs(modelo$residuals)~geducacion)

summary(modelof)
# analizar si la variable que se ocupa en esta regresion es estadisticamente significativa es esta variable la que
# esta afectado y provocando la heterocedasticidad. Si nosotros tuvieramos por ejemplo 4 variables, las 4 las evaluamos.

# -- Goldfeld Quandt -- #
# Contraste generalmente utilizado en libros de econometria (mas ortodoxa) (procedimiento tedioso)

gqtest(modelo, order.by = gtotal,
       data = data)

#GQ = 2.3317, df1 = 26, df2 = 25, p-value = 0.01885
# Invasivo, nos pide ordenar y modificar la data. 

# el p-value indica que hay que rechazar la Ho.

# -- Atenuando el Problema -- #

# - Minimos Cuadrados Ponderados - #

# este modelo supone que nosotros ya conocemos la estructura subyacente de la heterocedasticidad
# misma que puede ser identificada a traves del metodo Glester, por ejemplo, en nuestro contraste nos indico que
# el gtotal es la variable que provoca heterocedasticidad, y con eso sabemos como puede ser tratado 
# (ponderando cada variable entre X o la raiz de X segun sea el caso)

# y1 = Bo+BiXi

# y1/x1 = Bo/x1 + Bi x1/x1

modeloajustado = lm(galimentos~gtotal, data = data, 
                    weights = 1/sqrt(gtotal))
# matematicamente estamos dividiendo 1 sobre la raiz cuadrada de gtotal (variable que crea heterocedasticidad)
summary(modeloajustado)
bptest(modeloajustado)
# persiste el problema. Es muy probable que esa no sea la via, o que el problema de la hetero no sea tan grave
# comp para que nosotros trastoquemos la base de datos, la transformemos y echemos a perder los datos que 
# naturalmente fueron generados (materializacion de evento estadistico)

# La otra manera es la correcion (atenuacion) es obtener los errores estandar libres de hetero. 
# -- Metodo White (HAC) -- #

modelohac = coeftest(modelo, vcov = vcovHAC(modelo))
# comparacion modelo normal vs hac
summary(modelo)
modelohac # se llama sin el summary
# a diferencia los errores ya no estan sobre estimados, pasaron de 50.85 a 44.65 es un error libre de hetero
# todo esto con los mismos coeficientes (no afecta base de datos)
# Nosotros tendremos de HC0 hasta HC3 para muestras pequeñas.
# HC4 para muestras pequeñas pero con presencia de valores atipicos.

t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(modelo, type = x)))))

#HC0    42.46916 0.07289183
#HC1    43.26305 0.07425441
#HC2    43.85880 0.07507972
#HC3    45.31345 0.07736085
#HC4    45.47202 0.07720454

# ocupamos los 4 tipos de transformaciones o correcion de matriz de varianza (por deafult es el HC3)
# y me devuelve los errores estandar libres de heterocedasticidad
# esto implica que el problema de la hetero no es tan grave, no es necesario ocupar metodos invasivos. 