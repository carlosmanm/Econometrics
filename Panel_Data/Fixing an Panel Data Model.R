

library(plm) #Panel
library(sandwich) #Estimadores EE robustos
library(lmtest) #Contrastes
library(tseries) #Contrastes
library(foreign) #Bases con diversas extensiones
library(gplots) #gráficos
library(memisc) #unión tablas
library(rmarkdown) #automatización de reportes
library(lme4)



file.choose()


Panel <- read.dta("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Betametrica\\Panel 2\\bases\\bases\\data.dta")

fixed <- plm(y~x1, data=Panel,
             index=c("country","year"),
             model="within")

summary(fixed)

# Una vez que hemos evaluado hetero, auto y cross sectional dependence

# Recordemos que en realidad no se corrige, sino se atenua. En ese sentido ocuparemos un metodo de varianzas
# y covarianzas HAC, o tambien conocido como White, que en realidad lo que hace este modelo es conservar los 
# coeficientes del modelo pero con sus errores estandar libre de autocorrelacion y heterocedasticidad.


####  Corrigiendo autocorrelacion y la heterocedasticidad

# Existen distintos tipos de estimadores, este lo haremos con vcovHC (Heteroskedasticity-Consistent Covariance Matrix Estimation)

?coeftest

?vcovHC # Libreria Sandwich. 

# Donde su version para panel es la siguiente: 

?vcovHC.plm

# Y su estructura es la siguiente: 

# vcovHC(
#  x,
#  method = c("arellano", "white1", "white2"),
#  type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
#  cluster = c("group", "time"),

# Donde:

#HC0 Hetocedasticidad consistente y es la opcion por default
#HC1,HC2, HC3 para muestras pequeñas. HC3 para valores atipicos.
#HC4 muestras pequeñas y valores atipicos. 

# Y los metodos:

# white1 para hetero pero no autocorrelacion. Y es recomendado para MEA.
# white2 similar a white1 pero con restricción de varianza y este es común en los grupos. Y es recomendado para los MEA
# Arellano corrige heterocedasticidad y autocorrelacion. Comunmente se ocupa para MEF y tambien es por default. 


summary(fixed)


coeftest(fixed,vcovHC(fixed, 
                       type = "HC0",
                       method="arellano"))

# Mismo coeficiente, error distinto y ya no es significativo 

#Estimar todos los metodos al mismo tiempo: Comparación

t(sapply(c("HC0", "HC1", "HC2", 
           "HC3", "HC4"), 
         function(x) 
           sqrt(diag(vcovHC(fixed, 
                            type = x)))))






