# Contrastes de especificacion 

library(lmtest)

file.choose()

data = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\REG_1.csv")
attach(data)
names(data)

# - estimando el modelo - #

modelo = lm(M1~GP+IPC+TI, data = data)
summary(modelo)

# suponiendo que ya se hizo el prueba F, T y se comprobo un R2 significativo, suponiendo
# que no tiene autocorrelacion, heterocedasticidad o multicolinealidad, o si se tuvieron
# ya se atenuaron, suponiendo que todo esto ya esta validado, entramos en la etapa de validacion
# a traves de las extensiones d elos contrastes o contrastes complementarios.

# 1. Normalidad 
# Esta ya fue aprobada

# -- Contraste de linealidad o especificacion
# recordemos que los modelos de regresion lineal multiple hacen referencia a que los parametros son lineales
# pero tambien, el problema peude ser que el modelo NO sea linea, y por lo tanto 
# hay que hacer algun tipo de transformacion para hacerlo lineal.

# Ocuparemos el contraste:

# Ramsey Resetc (mas común)

# si no esta bien construido el modelo podemos estar cometiendo el error de especificacion.

# Causas:

#  - Variables No linales (se requiere transformacion: cuadratica o cubica por ejemplo)
#  - El modelo esta incorrectamente especificado (variables redundantes u omitidas que producen sesgo)
#  - Autocorrelacion (recordemos que un problema de la autocorrelacion es la mala
# especificacion de las variables, en proporcion a ello, tambien la autocorrelacion.
# Hay ocaciones en las que queremos corregir una autocorrelacion cuando solo se trata de
# un tema de especificacion.)

#  - Ramsey Reset

resettest(modelo)
# Ho: Modelo Correctamente Especificado
# H1: Contrario
# Si P-Value menor que 0.05 se rechaza 0.05

# data:  modelo
# RESET = 27.982, df1 = 2, df2 = 51, p-value = 6.272e-09

# se rechaza Ho: el modelo no esta correctamente especificado.
