# Multicolinealidad 1

# - Multicolinealidad - #


library(car)
library(GGally)
install.packages("GGally")

# - Cargar Base de Datos - # 

file.choose()
data = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\REG_1.csv")
attach(data)
names(data)

# - Estimar Modelo - #

# En nuestra base podemos identificar que hay variables que estan ciertamente relacionadas
# por ejemplo el IPC y la INF(inflacion), es decir, la inflacion es IPC - IPC-1. Y eso
# Lo podemos notar en nuestra Base de Datos. 

modelo = lm (M1~GP+TI+IPC, data= data)
summary(modelo)
# Parece que el modelo pinta bastante bien, hacemos F, T y R.


# - Identificacion del problema - #

# La multicolinealidad generalmente se asocia a Variables con Baja significancia
# y un R2 alto. En este caso todas las variables son altamente significativas.

# -- Coeficiente de Correlacion de las Variables -- #

cor(data[3:6],
    use = "complete.obs")
# Aqui hacemos una correlacion de la variable 3 a la variable 6, porque el orden va:
# "periodo", "M1", "GP", "IPC", "INF", "TI"
# Nos arroja:
# GP          IPC          INF         TI
# GP   1.00000000  0.794545651  0.039107681 -0.5994705
# IPC  0.79454565  1.000000000 -0.003496827 -0.6760294
# INF  0.03910768 -0.003496827  1.000000000  0.3068127
# TI  -0.59947045 -0.676029403  0.306812715  1.0000000

# encontramos que los datos mas correlacionados son IPC y GP con un indice de correlacion
# de casi 1. Ya identicado cual es el mayor. Si este coeficiente es mayor que 0.90 - 0.91
# podemos concluir categoricamente que existe un problema de multicolinealidad severa
# aqui se tiene que empezar a evaluar como proceder. En este caso, no sucede esto.

# -- Grafico de Coeficientes de Correlacion -- #

pairs(data[3:6])

# Aqui solo podemos identificar la relacion lineal (comportamiento sistematico) de las variables
# GP e IPC.

ggpairs(data[3:6])

# grafico mucho más sofisticado de la libraria de GGally. Podemos ver las distribuciones
# y nos arroja el coeficientes de correlacion.

# -- FVI ó VIF -- #

vif(modelo)
#       GP       TI      IPC 
# 2.765936 1.878126 3.263365 
# NINGUNO supera 10, no hay problema de multicolinealidad

# -- Atenuando la Multicolinealidad -- # (de haber)

# - Ampliar tamaño muestral

# - Aplicar Logaritmo Natural
modelo = lm(log(M1)~log(GP)+log(IPC)+log(TI), data=data)
# - Aplicar Primeras Diferencias
# Para esto, el modelo debe estar en series temporales:
tsdata = ts(data[,2:6], start = c(2003, 1), frequency = 12)
tsdata
difdata = data.frame(diff(tsdata, 1))
# Calculara a toda la matriz la primera diferencia y despues la trasnformara en un data frame.
attach(difdata)
modelo1 = lm (M1~GP+TI+IPC, data= difdata)
summary(modelo1)

# Tenemos nuevos coeficientes y ya no son significativos 2, hemos alterado la base de datos
# y a veces es peor la receta que la enfermedad. 
vif(modelo1)
#       GP       TI      IPC 
# 1.042947 1.028772 1.019602
# El VIF es mas chico, pero no lo vale. 

# - Ratios

# Analisis de Componentes Principales
# Tecnica de la estadistica multivariante. Con esta tecnica desarrollada 
# a traves del Analisis de componentes principales y del Analisis Factorial.

# Las técnicas para atenuar la multi precisamente son para generar estimaciones 
# confiables. El problema es que la interpretación ya no es la misma.
# Por tal motivo generalmente se recomienda no hacer nada, ampliar el tamaño muestral
# y de ahí ver qué otra forma atenuar el problema.
