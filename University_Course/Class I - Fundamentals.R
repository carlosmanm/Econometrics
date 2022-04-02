
# Manejo Basico

# Clase 2



install.packages("dplyr")
install.packages("openxlsx")
install.packages("lubridate")
library(dplyr)
library(openxlsx)
library(lubridate)

# Prueba

print("Hello World")


1+1


#..............................................................................

#  Como crear un Data Frame

Sexo = c("Hombre", "Hombre", "Mujer", "Hombre", "Hombre", "Hombre", "Mujer" )

Edad = c(17, 18, 20, 18, 18, 18, 19)

Hermanos = c(2, 0, 0, 1, 1, 1, 0)

dataframe1 = data.frame(Sexo, Edad, Hermanos)

View(dataframe1)

feedbackframe = dataframe1

str(dataframe1) 
dataframe2 = data.frame(Sexo, Edad, Hermanos, stringsAsFactors = FALSE, 
                        row.names = c("E1", "E2", "E3", "E4", "E5", "E6", "E7"))

dataframe2[c("E1", "E2"),] 

fix(dataframe1)

#Modificar data frames

names(dataframe2)=c("S", "E", "H")

rownames(dataframe2)=paste("Alumno", 1:7, sep = "_")

dimnames(dataframe2) = list(c("I", "II", "III", "IV", "V", "VI", "VII"), c("V1", "V2", "V3"))

dataframe2$V2 = as.numeric(dataframe2$V2)

str(dataframe2)

#Cómo añadir filas y columnas a un data frame

nuevainfo = data.frame(Sexo = c("Hombre", "Hombre"), Edad = c(18, 18), Hermanos = c( 1,2))

dataframe2 = rbind(dataframe2, nuevainfo)

dataframe2

#Cómo aplicar una función a un data frame

str(iris)

sapply(iris[,1:4], FUN =mean)


#..............................................................................

# Construyendo una matriz

x = c(1,2,3,4,5,6)
x = c(1:6)

y = c(7:12)

x

y

frame1 = data.frame(y, x)

frame1

summary(frame1)

View(frame1)

#...........................................................................#

# Abrir archivo

install.packages("openxlsx")

file.choose() 

data1 = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso UAM X\\Bases de datos\\DEP_2016_BP.xlsx")

summary(data1)

View(data1)

help("summary")

help("read.xlsx")

data1 = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso UAM X\\Bases de datos\\DEP_2016_BP.xlsx",
                  detectDates = T)


View(data1) # Se valida y efectivamente ya detecto las fechas.

#nombrematriz[fila, columna]

data1[2, 1] # Aqui localizamos del frame1 el valor de la fila 2, columna 1. 

data1[2,] # Solo visualizamos fila 2.
data1[,2] # Solo visualizamos columna 2.

data1[1:3, 1:2] # Visualizamos filas de la 1 a la 3 y columnas de la 1 a la 2.
data1[1:5,] # Visualizamos filas de la 1 a la 5 y todas las columnas.



colnames(data1)

names(data1) 

data1$ENTIDAD  
 
Saldo = data1$SALDO
Saldo = data.frame(data1$SALDO) 

View(Saldo)
summary(Saldo) # Podemos analizarla por si sola.
summary(data1$SALDO) 
View(data1$SALDO)

# Cambiar nombre

names(Saldo)[1] = "Saldo"   

#Categorias de la variable

table(data1$REGION) 

table(data1$ENTIDAD) 

head(data1) 
head(data1,2) 

# Aplicando filtros rapidos con R 


DB.Costa = data1[data1$REGION == "COSTA",] 

DB.Costa = data1[data1$REGION == "INSULAR",]

# Nombre de variable = data[data$region =="Costa", ]

View(DB.Costa) 

table(DB.Costa$REGION)

DB.Costa = data1[data1$REGION=="COSTA" & data1$ENTIDAD == "BP PICHINCHA", ] 

View(DB.Costa)

table(DB.Costa$ENTIDAD, DB.Costa$REGION) 

 

less5clients.cases = data1[data1$NUMERO.DE.CLIENTES<=5, ] 

table(less5clients.cases$NUMERO.DE.CLIENTES) 

View(less5clients.cases)  


min(data1$NUMERO.DE.CLIENTES) 

max(data1$NUMERO.DE.CLIENTES) 



between5n8clients.cases = data1[data1$NUMERO.DE.CLIENTES>=5 & data1$NUMERO.DE.CLIENTES<=8
                                & data1$ENTIDAD=="BP GUAYAQUIL", ] 
table(between5n8clients.cases$NUMERO.DE.CLIENTES) 
View(between5n8clients.cases)


regioncostaorsierra.cases = data1[data1$REGION=="COSTA" | data1$REGION=="SIERRA", ]

View(regioncostaorsierra.cases)
table(regioncostaorsierra.cases$REGION) 


allregionsunlesscosta = data1[data1$REGION!="COSTA",] # Excluimos la COSTA. 
table(allregionsunlesscosta$REGION) 

allregionsunlesscostansierra = data1[data1$REGION!="COSTA" & data1$REGION!="SIERRA",]
table(allregionsunlesscostansierra$REGION) 

 
y = data1[data1$REGION =="COSTA"|data1$REGION=="SIERRA" & data1$ENTIDAD!="BP GUAYAQUIL" &
            data1$ENTIDAD!="BP MACHALA", ] 

table(y$ENTIDAD)

# Combinando operadores logicos y arimeticos. 

lessclientsandnosierra = data1[data1$NUMERO.DE.CLIENTES==min(data1$NUMERO.DE.CLIENTES) 
                               & data1$REGION=="SIERRA",]

table(lessclientsandnosierra$NUMERO.DE.CLIENTES, lessclientsandnosierra$REGION)
# Combinamos los casos de 0 clientes para una entiendad en todas las regiones menos SIERRA.

View(data1)

#.....................................

file.choose()

iowa1 = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso UAM X\\Bases de datos\\Iowa_Liquor_Sales.csv", 
                 stringsAsFactors = FALSE)
names(iowa1) 
View(iowa1)


# SELECT 

selectdata = select(iowa1, Invoice.Item.Number, 
                    Vendor.Name, Category)
View(selectdata)

# Digamos que solo estamos interesado de la variable numero 2 a la variable numero 5. 
#  seleccionando por numero de columna

first1to5numbers = select(iowa1, c(2:5))
View(first1to5numbers)

first1to5names = select(iowa1, Date:Address)
View(first1to5names)


# EXCLUYENDO DATOS CON SELECT

everythingtest = select(iowa1, -Date)


everythingtest = select(iowa1, -Date, -Address, -City)

names(everythingtest) 


everythingtest = select(iowa1, -c(1,3,5))


everythingtest = select(iowa1, -c(1:5)) 

everythingtest = select(iowa1, -c(Address:Vendor.Name))

# Filtrando bases de datos utilizando la función filter

filtertest = filter(iowa1, City=="DAVENPORT")


table(filtertest$City)
table(iowa1$City)


filtertest = filter(iowa1, City=="DAVENPORT", City=="CEDAR RAPIDS")


filtertest = filter(iowa1, City=="DAVENPORT" | City=="CEDAR RAPIDS")

names(iowa1)
filtertest = filter(iowa1,
                    iowa1$City =="DAVENPORT" & 
                      Bottles.Sold >= 6  )

min(filtertest$Bottles.Sold)                      
max(filtertest$Bottles.Sold)

zz = iowa1[iowa1$City=="DAVENPORT" & 
             iowa1$Bottles.Sold >= 6, ]


# Ocuparemos DISTINCT (obtener valores unicos en una columna) 

distinct(iowa1, 
         City)


iowa1$City = sapply(iowa1$City, tolower) # Y listo, ya no hay valores repetidos.

# Tambien con mutate se pudo haber cambiado.

iowa1 = mutate(iowa1, 
               City=sapply(iowa1$City, tolower)) 
 
iowa1 = mutate(iowa1, 
               City=sapply(iowa1$City, toupper))


# UNIENDO BASES considerando que la varible Union NO esta repetida

file.choose()

salesA = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso UAM X\\Bases de datos\\Iowa_Liquor_Sales_A.csv")

salesB = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso UAM X\\Bases de datos\\Iowa_Liquor_Sales_B.csv")


View(salesA)
View(salesB)

names(salesA)
names(salesB)

# La variable Invoice.Item.Number es la variable con la que cruzaremos los datos, ya que estos se repiten en ambas bases.
# Es claro que para esto, no debe haber valores duplicados de estas variables clave.
# Las uniones son las clasicas:
# Inner_Join 
# Left_Join
# Right_Join
# Full_Join  

# Aunque hay otras no tan clasicas como:

# Semi_Join : devuelve todas las filas de x donde hay valores coincidentes en y, manteniendo solo columnas de x. 
# Una unión semi difiere de una unión interna porque una unión interna devolverá una fila de x por cada fila coincidente de y, 
#  donde una unión semi nunca duplicará filas de x.

# Nest_Join: devuelve todas las filas y todas las columnas de x. Agrega una columna de lista de tibbles. Cada tibble contiene todas las filas de y que coinciden con esa fila de x. 
# Cuando no hay coincidencia, la columna de la lista es un tibble de 0 filas con los mismos nombres y tipos de columna que y.

# Anti_Join: devuelve todas las filas de x donde no hay valores coincidentes en y, manteniendo solo columnas de x.


# Aqui sabemos que x & y son las bases de datos.

# Necesitamos que los datos que estan en A (left) se quedan y se unan lo que estan en B (right), entonces ocuparemos un Left_Join
names(salesB)
salesC = left_join(salesA, salesB,by="Invoice.Item.Number") 
# se crea una base de datos con 3 variables nuevas, ya que de las 4 se omite la variable clave 2 
# que hace el match.

file.choose()

DEP.BP.A = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso UAM X\\Bases de datos\\DEP_2016_BP_A.xlsx",
                     detectDates = TRUE)


# Para cambios de variables estan:
# as.factor
# as.character
# as.numeric
# as.integer

DEP.BP.A$OFICINAS = as.numeric(DEP.BP.A$OFICINAS) 

str(DEP.BP.A)