# Contrastes de normalidad

install.packages("strucchange") # structure change
install.packages("moments") 
install.packages("nortest") # normality test
library(strucchange)
library(moments) # ejecutar contraste jarque bera
library(nortest) # ejecutar contraste anderson darling

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

# - Contraste de Normalidad
# se supone que la normalidad es un supuesto necesario patra poder realizar pruebas (F, T)
# se supone que noostros tenemos variables que siguen una distribucion nromal, y por ende
# los residuos tambien siguen una distribucion nromal. 
# Aunque, por ejemplo, en el teorema Gauss-Markov no viene nada de esto incluido, es decir
# la nromalidad de todas las variables, sin mebargo, siempre se sugiere que se sigue una 
# distribucion normal. Para validar esta normalidad trabajaremos con contrastes:

# - Jarque Bera

jarque.test(as.vector(modelo$residuals))

# Ho: Normalidad
# H1: No Normalidad
# Si p-value es menor que 0.05 se rechaza la Ho 

# data:  as.vector(modelo$residuals)
# JB = 1.5418, p-value = 0.4626
# alternative hypothesis: greater
# En este caso No se rechaza Ho (Hay normalidad)

# - Anderson Darling

ad.test(modelo$residuals)

# Ho: Normalidad
# H1: No Normalidad
# Si p-value es menor que 0.05 se rechaza la Ho 

# data:  modelo$residuals 
# A = 0.29845, p-value = 0.5753

# En este caso No se rechaza Ho (Hay normalidad.  