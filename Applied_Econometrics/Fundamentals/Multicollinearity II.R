# Multicolinealidad 2

library(car)
library(GGally)
library(ggplot2)
library(openxlsx)

file.choose()
data = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\REG_ACP.xlsx")
attach(data)
names(data)

modelo = lm(INEX~CONS+GPER+GEX, data = data)
summary(modelo)
# Un modelo al parecer muy bueno. 
# POCOS valores T significativos pero un R2 altisimo, clara muestra de 
# multicolinealidad.

# -- Identificando la multicolinealidad -- #

# - Coeficientes de Correlacion. 
cor(data[3:5])

#          CONS      GPER       GEX
# CONS 1.0000000 0.9903797 0.9977693
# GPER 0.9903797 1.0000000 0.9953409
# GEX  0.9977693 0.9953409 1.000000
# La matriz de correlacion nos indica que la correlacion es altisima. 

# - VIF - #

vif(modelo) # Mucho mayor a 10...
#     CONS     GPER      GEX 
# 274.1013 131.3951 564.5714 
# Excesivamente alto. (redundancia por ejemplo consumo con gasto personal.)

pairs(data[3:5])
ggpairs(data[3:5]) # coeficientes de correlacion altisimos (son practicamente lineales)

# -- Atenuando el problema -- #

# Metodos:

# Nada
# Log
# Dif
# Ratios
# ACP
# Ampliando el tamaño muestral
# Sugerencia: comunmente se pide combinar el ampliar el tamaño y aplicar Logaritmos.

# - Analisis de Componentes Principales (tecnica encasillada en estadistica multivariada)
# tecnica previa al Analisis Factorial.
# en resumen: Análisis factorial es una técnica estadística de reducción de datos usada 
# para explicar las correlaciones entre las variables observadas en términos de un número 
# menor de variables no observadas llamadas factores. 


x= cbind(data[3:5])
mcap1 = princomp(x, scores = T, cor =  T)
# modelo de componentes principales: creamos un data frame de modelo con las variables de 
# nuestro interes, como son 3 variables seran 3 componentes. De la matriz X quiero los
# scores y que use la matriz de correlacion. 
summary(mcap1)

scores = data.frame(mcap1$scores)
# se crea un nuevo data frame, estos 3 componentes van a sustituir a nuestros 3 variables
# se dice que estas variables (componentes) YA no tiene multicolinealidad, y que son 
# ortogonales. La ortogonalidad es una condicion que asegura la independencia de los componentes,
# es decir, que ya no hay correalacion entre estas variables. (son indipendientes)

reg = lm(INEX~scores$Comp.1+scores$Comp.2+scores$Comp.3, data = scores)
summary(reg) 
# Ya el problema esta corregido de raiz. El asunto esta sesgado, ya que sabemos que 
# las variables son redundantes. Ya NO son variables originales.
vif(reg)
# scores$Comp.1 scores$Comp.2 scores$Comp.3 
# 1             1             1 
# El problema ha sido resuelto. 
