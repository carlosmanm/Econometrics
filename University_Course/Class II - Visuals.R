# Clase 3 

# Graficos

#............................................................................

#Graficos basicos para datos cualitativos

m = c(3, 2, 5, 1, 3, 1, 5, 6, 2, 2, 2, 1, 3, 5, 2)

pie(table(m), main = "Clase3")

help("pie")

Respuestas = c( "No", "No", "Si", "No", "Si", "No", "No", "Si")

Genero = c("M", "M", "M", "H", "H", "H", "H", "H")

plot(table(Genero, Respuestas), 
     main = "Grafico de Mosaico de las variables \"Genero\" y \"Respuestas\"")

install.packages("vcd")

HairEyeColor


mosaic(HairEyeColor, dir=c("v", "h", "v"), 
       highlighting="Sex", highlighting_fill = c("pink", "lightblue"), 
       main = "Grafico de Mosaico de la tabla HairEyeColor")


#Grafico de barras

x =c(85, 88, 87, 89, 45, 67, 87, 65, 65, 66, 78, 77, 55, 56, 55, 48, 49)
y=c(1.65, 1.77, 1.75, 1.75, 1.88, 1.91, 1.62, 1.66, 1.70, 1.70, 1.82, 1.85, 1.78,
    1.55, 1.54, 1.68, 1.90)


barplot(x)
barplot(x, main = "Edades en Pacientes del Hospital")
barplot(x, main = "Edades en Pacientes del Hospital", col = "red", ylab = "promedio")

#Diferentes tipos de plot con X

summary(x)

help("plot")

plot(x, type = "p", col = "red", main = "Edades", ylab = "promedio")
plot(x, type = "c")
plot(x, type = "b")
plot(x, type = "s")
plot(x, y, type = "l", col = "blue", lty=2, lwd=3, main = "Simulación", 
     ylab = "promedio")
f=function(x){exp(x)}
plot(f)
plot(f, xlim = c(0,10)) 
plot(f, xlim = c(0,10), xaxp = c(0, 10, 10)) 

#Plot y más graficos

f=function(x){x^2}
plot(f, xlim = c(-1,1), ylim = c(-1,1), main = "Estimacion Trimestral")
points(0,0, pch=16)
abline(h=0, col = "green")
curve(-x^2, col = "red", add = TRUE)
text(0,0, labels = "pto. de intersección", pos = 1)
legend("topleft", legend = c(expression(x^2), expression(-x^2), expression(y=0)), col = c("black", "red", "green"), lwd = 3)


#BOXPLOT

boxplot(x)
boxplot(x, y)
boxplot(x, ylab = "peso", col ="red", main = "Concentración de Flujos", notch = TRUE)

mean(x)
#Representación gráfica de datos multidimensional

install.packages("scatterplot3d") #Instalar el package Scatterplot3d 
library(scatterplot3d) 
scatterplot3d(iris[,1:3], pch =20) #Usando el data frame de Iris. 
plot(iris[,1:4], col = iris$Species) #Plot de todos los pares de columnas
install.packages("car") #Instalamos el paquete CAR que contiene 
#la funcion SPM que genera diagramas de dispersion con más informacion.
library(car)
spm(iris[,1:4]) #Todos los diagramas de dispersion contienen lineas de tendencia (regresiones)


# Histogramas

#Ocuparemos el data frame Chile (elecciones 1988 contra Pinochet)

library(car)
data("Chile")
head(Chile)
Chile2 = na.omit(Chile)
k = 14
A = 4
L = min(Chile2$age)-0.5+A*(0:14)
hist(Chile2$age, breaks=L, right = FALSE, main = "Histograma para Chile 1988")
h = hist(Chile2$age, breaks=L, right = FALSE, plot = FALSE )
h$breaks
h$counts
h$density
h$mids

hist_abs(Chile2$age, L)

# Mapas de calor ("Heatmaps")

data("mtcars")
mtcars2 = mtcars[,-c(8,9)]
round(abs(cor(mtcars2)),3)

heatmap(abs(cor(mtcars2)), Rowv = NA, Colv = NA, revC = TRUE)
# con Rowv =NA, impedimos que se añadan al grafico dendogramas en las filas, con Colv, lo mismo por columnas
# con RevC indicamos que el orden de las variables de izquierda a derecha sea el mismo que el de arriba a abajo.
# La funcion corrplot tambien realiza un heatmap pero con mucha mas informacion sobre la correlacion de variables.
install.packages("corrplot")
library(corrplot)
corrplot(cor(mtcars2)) 
#Los circulos mas grandes indican mayor correlacion, al igual que el color (por grados).

dist.mtcars = as.matrix(dist(mtcars2))
heatmap(dist.mtcars, margins = c(9,9), symm = TRUE, #SYMM indica que la matriz es simetrica.
        Rowv = NA, Colv = NA, revC = TRUE)


#Dendrogramas

#Uno de los temas mas tipicos es el agrupamiento de individuos a partir de una matriz de distancias
# Dada una tabla de datos podemos construir una matriz de distancias entre sus individuos. 
# Una vez esto, se puede ir agrupando con el algoritmo "clustering" jerarquico. 
# Un dendograma es una representacion grafica del orden en el que se han ido realizando dichas 
# agrupaciones y de las distancias entre los pares de individuos. (original vs virtual) 
install.packages("cluster.datasets")
data("all.mammals.milk.1956")
AMM = all.mammals.milk.1956
head(AMM)
dist.AMM =dist(AMM[,2:6]) 
round(as.matrix(dist.AMM)[1:6, 1:6],3)
den.AMM = as.dendrogram(hclust(dist.AMM))
plot(den.AMM)

#Nubes de palabras ("bubbles")

# La funcion basica para producirlos en R es "symbols", x e y son vectores de primeras 
# y segundas coordenadas. Parametro_figura indica el tipo de figura que se quiere utilizar 
# y z es el vector de tamaños lineales de las figuras. Ejemplo de datos saving del paquete faraway

install.packages("faraway")
library(faraway) # Nos da acceso a los datos
library(RColorBrewer) # Nos da acceso a paleta de colores
data("savings")
# Colorearemos las burbujas segun una escala de colores que represente el porcentaje de la poblacion 
# menor de 15 años. Burbuja más oscura = mayor porcentaje


porcentaje15 = as.numeric(cut(savings$pop15, 9))
colores = brewer.pal(9, "YlOrBr")[porcentaje15]  
symbols(savings$dpi, savings$sr, circles = savings$ddpi,
        bg = colores, xlab = "Renta Per Capita", ylab = "Tasa de Ahoro")
text(savings$dpi, savings$sr, rownames(savings), cex = 0.75)




#La relacion consumo / tamaño es lineal? es positiva? es negativa? 
install.packages("tidyverse")
data("mpg")

View(mpg)
help(mpg)
# displ : tamaño del motor del coche en litros.
# hwy : numero de millas recorridas en autopista por 
#un galeon de combustible (4,5460902819948 litros (redondeado a 4,5461 litros))

#Grafico con puntos
ggplot(data = mpg) +
  geom_point(mapping = aes (x = displ, y = hwy))

#PLANTILLA PARA HACER UNA REPRESENTACION GRAFICA CON GGPLOT
# ggplot(data = <DATAFRAME>) +
#     <GEOM_FUCTION(mapping) = aes(<mapping>))

ggplot(data = mpg) +
  geom_point(mapping = aes (x = class, y = drv))

ggplot(data = mpg) +
  geom_point(mapping = aes (x = cyl, y = cty))

#Vamos probando el grafico de puntos con ggplot en diferentes variables.

ggplot(data = mpg) +
  geom_point(mapping = aes (x = displ, y = hwy))

#Digamos que queremos diferenciar la clase de automovil en el grafico segun colores, ejecutamos la misma
# instruccion pero agregando color. 

# Color de los Puntos

ggplot(data = mpg) +
  geom_point(mapping = aes (x = displ, y = hwy, color = class))
# Me arroja el mismo grafico pero con puntos multicolor, donde por ejemplo el naranja representa la clase de 
# coche 2seater, o el verde-azul las minivan.

# Tamaño de los Puntos

ggplot(data = mpg) +
  geom_point(mapping = aes (x = displ, y = hwy, size = class))

# Transparencia de los puntos

ggplot(data = mpg) +
  geom_point(mapping = aes (x = displ, y = hwy, alpha = class))

# Forma de los puntos (GGPlot solo permite 6 a la vez)

ggplot(data = mpg) +
  geom_point(mapping = aes (x = displ, y = hwy, shape = class))

