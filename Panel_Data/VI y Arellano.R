# Variables instrumentales y arellando bond

# En los modelos panel donde conjuntamos i (agentes) y t (tiempo) estimando Yit

# Hasta ahora no se habia incorporado la dinamica intertemporal de variables dependientes rezagadas. 

# Es importante incluirla por distintos fenomenos, por ejemplo la inflacion, que no depende unicamente 
# de yt sino de yt-1, yt-2, etc. Sin embargo, esto requiere un trato especial. Pero esto ya no se puede estimar
# mediante MEA, MEF o Pool, debido a que:

# Si se estima un Pool con la variable dependiente rezagada, la esperanzade los residuos dada la variable rezagada 
# es distinta de 0, es decir, provoca correlacion entre variables y residuos. Por ende, las estimaciones seran
# sesgadas e inconsistentes. 

# Si se estima con MEF (within), al momento de calcular la esperanza de yit-1, con respecto a los errores, habrá una
# correlacion. Por ende, las estimaciones seran sesgadas e inconsistentes. 

# Por otro lado, se tiene un problema de inconsistencia: Within sera consistente en la medida en la que T tienda  
# infinito, mientras que los MEA tambien son consistentes dado que basicamente es una transformacion cuasipromedio
# considerada como una media ponderada. 

# Por ello, surge en modelo estimado por el metodo generalizado de los momentos. 
# (recordemos que el MEA se estima por el MGM)

# Si se estima Pool o MEF los errores estandar de los parametros son inconsistentes. 

# Para abordar este tema se acuden a las variables instrumentales a traves del MGM, y una de las metodologias más
# utilizadas de estos modelos es Arellado Bond.

# ¿Que es una variable instrumental? Una variable llamese "Z" es una VI de tal manera que esta correlacionada
# con x (variable exogena) pero no con u. Es decir

# E(Z,X)=!0 pero E(Z,U)=0


# Estos para un modelo:

# Yi,t = a + O Yi,t-1 + BXi,t + ui + vi,t

# Tambien conocida como una variable proxy o similar a la x. 

# Esto es ideal para paneles cortos pero no menor a 2 periodos

# Es necesaria la estacionariedad, es decir que O sea menor que 1 en valor absoluto.

# Los errores no estan serialmente correlacionados. (No autocorrelacion) debe E(X,U)=0


# Arellado Bond (1991)

# Dado el siguiente modelo:

# Yi,t = a + O Yi,t-1 + BXi,t + ui + vi,t

# Trata la variable rezagada aplicando primeras diferencias, es decir:

# (Yi,t - Yi,t-1) = O (Yi,t-1 + Yi,t-2) + B (Xi,t - Xi,t-1) + (vi,t - vi,t-1)

# -D-Yi,t = O-D-Yi,t-1 + B-D-Xi,t + -D-vi,t

# Notese que -D-vi,t es ahora un MA(1)

# La aplicacion de la primera diferencia de un modelo de panel que considera variables dependientes
# rezagadas es la de Arellano Bond. 

# Ejemplo si se tiene una T =3 la estimacion seria:

# (Yi,3 - Yi,2) = O(Yi,2 - Yi,1) + (Vi,3 - Vi,2)

# Para T = 4

# 1) (Yi,4 - Yi,3) = O(Yi,3 - Yi,2) + (Vi,3 - Vi,2)
# 2) (Yi,3 - Yi,2) = O(Yi,2 - Yi,1) + (Vi,3 - Vi,2)

# Donde para la ecuacion 1, los instrumentos son:
# Yi,2 & Yi,1

# Mientras que para la ecuacion 2:
# Yi,1. 

# Es decir se tiene 3 opciones:

# Estimacion con variables instrumentales (reemplazar X's correlacionada con una variable proxy, y que la teoria
# lo sustente, cuestion que es muy complicada en econometria.)
# Estimacion mediante MGM en niveles e incorporando solo rezagos en y
# Estimacion mediante MGM incorporando rezagos en Y & X's (Arellado Bond)

# ¿Como determinar que numero de rezagos es el optimo?

# Test Sargan, donde la Ho: GOF (Bonda de Ajuste) | Tambien se le conoce como una restriccion de sobreidentificacion son validas.


library(plm)
library(foreign)
library(lmtest)

file.choose()


Panel <- read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Betametrica\\Panel 2\\bases\\bases\\nw.csv")

Panel <- Panel[c(-1)] # Ya no tenemos x, una variable contadora.

# firms seria el agente i y year seria t

# Probemos un modelo donde solo hay rezagos en y

?pgmm

?dynformula # Functions of plm

# Generalized Method of Moments (GMM) Estimation for Panel Data

# Con list se controlan los rezagos, solo se pide un rezago para n

names(Panel)
  
z1 <- pgmm(dynformula(n ~ w+k+ys,
                      list(1,0,0,0)), Panel,
           effect="individual",model="twostep",
           gmm.inst=~n,lag.gmm=list(c(2,99)))
summary(z1)

# se obtiene que el rezago de las variable y las demas variables es signifitivo.

# el efecto individual sugiere cuando el panel es largo, mientras que 
# twoways, en lugar de individual, cuando el panel es corto en t. 
# por otro lado si colocamos twoways podemos recuperar las variables dummy.

# Two steps en simplemente el modelo que más se utiliza para arellano bond, a diferencia de 
# one step, es cual es menos común, pero es el que viene por default en pgmm. 

# gmm.inst=~n, indica que la generacion de instrumentos en contra la dependiente y SIEMPRE
# es contra la variable dependiente

# list((c(2,99))) control de los lags de los instrumentos y siempre van del 2 al 99 (esto es estricto)

# probando con rezagos tambien en las X's. 

z2 <- pgmm(dynformula(n ~ w+k+ys,
                      list(1,1,0,0)), Panel,
           effect="individual",model="twostep",
           gmm.inst=~n,lag.gmm=list(c(2,99)))
summary(z2)

# No se pueden evaluar individualmente los rezagos, por ejemplo si elijo un rezago 3 para 
# ys, no me mostrara todos los lags desagregados, para ello vamos a utilizar otra forma para estimar
# el modelo de panel dinamicos. 

z3 <- pgmm(n~lag(n,1:2)+lag(w, 0:0)+
             lag(k, 0:0)+lag(ys, 0:0)|lag(n, 2:99),
           data = Panel, effect="individual",
           model="twostep")
summary(z3)
# En lugar de usar list, se controla la variable por lags
# Ya me muestra los rezagos desagregados. 

# Probando rezagos en variables exogenas con nueva forma de estimar

# Que se coloca (variable, 0:2) por ejemplo, significa que se estime su variable en t-0 (presente), 
# y los rezagos en t-1, y t-2

# Es necesario tener los valores presentes, porq con ellos haremos la interpretacion del modelo.

# Entonces, hagamos un MGM condierando lag en Y y X's pero en la forma correcta

z4 <- pgmm(n~lag(n,1:2)+lag(w, 0:2)+
             lag(k, 0:1)+lag(ys, 0:1)|lag(n, 2:99),
           data = Panel, effect="individual",
           model="twostep")
summary(z4)


# Y si queremos recojer la dinamica del tiempo como en el MEF (donde las fechas eran dummies con valores)
# Cambiamos el modelo de efectos individuales a twoways.

z5<- pgmm(n~lag(n,1:2)+lag(w, 0:2)+
             lag(k, 0:1)+lag(ys, 0:1)|lag(n, 2:99),
           data = Panel, effect="twoways",
           model="twostep")

summary(z5, time.dummies=T)

# Tambien nos arroja los valores de los coeficientes a traves del tiempo.

# ¿Como saber cuantos rezagos incorporar?

# El constraste de Sargan:

# Ho: (Bonda de Ajuste)
# H1: Lo Contrario

# Y este bien directamente en el summary. Donde en nuestro ultimo modelo:

# Sargan test: chisq(25) = 32.75652 (p-value = 0.13727)
# En este test, el p valor no es menor que 0.05, por ende no rechazo la Ho de Bondad de Ajuste. 
# El incoviente es que Sargan, al igual que R2 es una funcion lineal creciente, a medida que
# aumentan variables, aumenta R2, a medida que aumentan instrumentos en Sargan, su valor de probabilidad
# tiende a 1. Esto no se debe olvidar, dado que el criterio es valido siempre y cuando no haya tantos
# instrumentos (rezagos)

# Aunque no a pesar de que Sargan diga que hay bondad de ajuste, no todas las variables son signifitivas
# por lo cual al evaluar el modleo de panel de datos dinamico debe ser una combinacion entre ambos. 

# Evaluamos autocorrelacion serial de los residuos.

?mtest # Arellano-Bond test of Serial Correlation

# Ho: No autocorrelacion
# H1: Lo contrario

mtest(z5, order = 2)

# p-value = 0.8944

# Dada la construccion del modelo se estila que empiece por orden 2 en adelante, 
# y dado que no hay correlacion, es un modelo interpretable y con el cual se puede trabajar. 

