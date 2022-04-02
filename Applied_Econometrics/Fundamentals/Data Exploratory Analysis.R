# Curso de Econometria, Estadistica y Ciencia de Datos
# Modulo: Manejo de Bases de Datos para Principantes con RStudio
# Capitulo 1: INTRODUCCIÓN A LOS COMANDOS BÁSICOS DE R

# Instalar paquete dplyr y ejecutar instrucciones con Ctrl + Enter 

# Guardar step by step con Ctrl + S
# Limpiar consola con Ctrl + L

# Para buscar ciertos fragmentos de nuestro codigo tenemos el Lookup en la parte superior.
      
install.packages("dplyr")
install.packages("openxlsx")
install.packages("lubridate")
library(dplyr)
library(openxlsx)
library(lubridate)
 # Prueba
1+1 

x = c(1:6)
y = c(7:12)

x
y
         # Construyendo una matriz

frame1 = data.frame(y, x)
    # Al seleccionar el frame1 en el global envionment se puede ver el frame y ocupar el filter. 
frame1
summary(frame1)
View(frame1) # Para no seleccionar el enviornment se puede ocupar View

# Abrir archivo

 # Se ejecuta el file.choose y se abre la carpeta del archivo que quieres elegir 

file.choose() # Esta funcion te da la RUTA del archivo con el que aplicaras el read.

data1 = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\DEP_2016_BP.xlsx")

summary(data1)

View(data1)

help("read.xlsx")
  # Estructura de READ.XLSX

#   read.xlsx(xlsxFile, sheet = 1, startRow = 1, ColNames = TRUE, rowNames = FALSE, DetectDates = FALSE
#   skipEmptyRow = TRUE, rows = NULL, cols = NULL, check.name = FALSE, namedRegion = NULL)

 # Por ejemplo, hagamos que nuestro archivo detecte fechas.

data1 = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\DEP_2016_BP.xlsx", 
                  detectDates = TRUE)
View(data1) # Se valida y efectivamente ya detecto las fechas.

# No olvidemos que los frames en R se acomodan de manera matricial (filas, columnas), por ejemplo:
# el frame1 es una matriz de (6, 2) y la de data1 es de (67432, 12) cifras que se pueden ver en el enviornment.
# Situandose de manera matricial es mucho más facil buscar trozos.

                             #nombrematriz[fila, columna]

frame1[2, 1] # Aqui localizamos del frame1 el valor de la fila 2, columna 1. 

frame1[2,] # Solo visualizamos fila 2.
frame1[,2] # Solo visualizamos columna 2.

frame1[1:3, 1:2] # Visualizamos filas de la 1 a la 3 y columnas de la 1 a la 2.
frame1[1:5,] # Visualizamos filas de la 1 a la 5 y todas las columnas.

 # Visualizacion de fragmentos con $
# Por ejemplo, hagamos una nueva matriz


matematicas = c(5.2, 3.4, 7.9, 6.4, 3.2, 4.5, 2.1, 4.2)
calif.teoria = c(6.5, 4.3, 5.4, 3.2, 7.2, 6.4, 9, 6)
# si nos equivocamos de nombre tenemos que borrar la variable, para evitar acumular, de solo cambiar el nombre
# la variable NO se va a borrar.
remove(calif.teoria)
teoria = c(6.5, 4.3, 5.4, 3.2, 7.2, 6.4, 9, 6) # La nombramos de manera correcta.

calificaciones = data.frame(matematicas, teoria)

View(calificaciones)

calificaciones$matematicas # Con el comnado $ podemos visualizar las columnas que querramos.

# Busquemos nombres de variables de nuestra base data1
colnames(data1)  # Nos arroja las variables en ORDEN de fecha, provincia, cuenta, etc
names(data1) # Tambien podemos ocupar este comando.

data1$ENTIDAD  #Visualizemos los datos de la variable Entidad.
# por ejemplo digamos que solo queremos analizar la variable Saldo la cual es
# la columna numero 12, entonces podemos crear una variabla ya sea con [] (numero) o con $ (nombre). 

Saldo = data1$SALDO # Aqui saldo solo es un vector de caracteres largos, para volverlo matriz hay que usar data.frame
Saldo = data.frame(data1$SALDO) # Se ha vuelto una matriz de 67432 filas y 1 columna.
# Al saber que Saldo es la columna 12 pudimos declarar la variable de esta manera: Saldo = data.frame(data1[, 12])
View(Saldo)
summary(Saldo) # Podemos analizarla por si sola.
summary(data1$SALDO) # Aunque si la necesidad de crear una variable tambien se puede hacer con la opcion $.
View(data1$SALDO) # Tambien podemos visualizar los datos de esta manera.

# Ya una vez que tenemos nuestra matriz Saldo, queremos cambiar el horrible nombre por default de la columna
# por uno mucho más apropiado. Entonces realizamos lo siguiente:

names(Saldo)[1] = "Saldo"  # Con esto se ha cambiado el nombre. NOTA: Al colocar el 1 SIN COMAS solo se indican 
# las columnas. Dado que solo es una matriz de una columna con muchas filas, se modifica el nombre de la unica variable que hay. 
  
       #Categorias de la variable

table(data1$REGION) # Aplicando la funcion table me arroja las categorias de Amazonica, Costa, Insular y Sierra
# y el respectivo conteo, que es 4378, 32422, 378 y 30254.
table(data1$ENTIDAD) # Aqui me arroja los respectivos 24 bancos que hay en el data frame.

head(data1) # Te arroja las primeras 6 observaciones de toda la base de datos.
head(data1,2) # Indicamos que solo queremos los primeros 2

# Aplicando filtros rapidos con R 

DB.Costa = data1[data1$REGION == "COSTA",] # Nueva Variable que se compone de la matriz data1, donde todas las filas 
# coinciden con COSTA y se muestran todas las demas columnas. 
View(DB.Costa) # Se visualiza toda la Base de Datos pero los campos donde la region solo sea COSTA.
table(DB.Costa$REGION)

citibank.costa = data1[data1$REGION=="COSTA" & data1$ENTIDAD == "BP CITIBANK", ] # Con & le agregamos más condiciones
# por ejemplo, aqui el match entre COSTA Y CITIBANK se muestran en 85 datos. Podemos verificarlo en Table.
View(citibank.costa)
table(citibank.costa$ENTIDAD, citibank.costa$REGION) # interseccion entre ambas variables.

             # Por ejemplo, queremos ver los casos de Entidades con menos de 5 clientes. 

less5clients.cases = data1[data1$NUMERO.DE.CLIENTES<=5, ] # creamos la variable less5clients y aplicamos el filtro.
table(less5clients.cases$NUMERO.DE.CLIENTES) # verificamos con table en numero de clientes
# se nos nuestra que hay 22470 casos de entidades sin clientes, o 1642 casos de entidades sin clientes.
View(less5clients.cases) # Tambien lo podemos visualizar. 

# Podemos pensar que el 0 es un error, pero verifiquemos buscando el minimo de esta variable.
min(data1$NUMERO.DE.CLIENTES) # Y efectivamente su minimo, nos arroja un 0.
  max(data1$NUMERO.DE.CLIENTES) 
# y su maximo es de 856920 clientes para una entidad.
  
        # Aqui buscamos un intervalo de 5 a 8 clientes.
between5n8clients.cases = data1[data1$NUMERO.DE.CLIENTES>=5 & data1$NUMERO.DE.CLIENTES<=8, ] 
table(between5n8clients.cases$NUMERO.DE.CLIENTES) 
View(between5n8clients.cases)
              # EJEMPLO
# Localizamos lo valores minimos para numero de cientes y de oficinas.

min(data1$NUMERO.DE.CLIENTES) # Nos arroja 0 clientes (como anteriormente lo habia hecho)
min(data1$OFICINAS)  # Nos arroja 1000 oficinas.

lessclientsnoffices.cases = data1[data1$NUMERO.DE.CLIENTES==min(data1$NUMERO.DE.CLIENTES) & 
                                    data1$OFICINAS==min(data1$OFICINAS), ]  # Sin declarar numero, creamos la variables
# con las especificaciones de que, contengan sus minimos valores.

View(lessclientsnoffices.cases) # Visualizamos que solo nos muestra una matriz en la que coinciden 0 clientes y 1000 oficinas.
table(lessclientsnoffices.cases$OFICINAS, lessclientsnoffices.cases$NUMERO.DE.CLIENTES) # Coinciden 683 casos. 
table(lessclientsnoffices.cases$ENTIDAD) # Podemos ver otras variables, por ejemplo, aqui TODOS los bancos
# forman parte de esta seleccion, sin embargo, la cifra de cada uno es mucho más chica, pues la suma da 683.

# La opcion Ó en vez del &.

regioncostaorsierra.cases = data1[data1$REGION=="COSTA"|data1$REGION=="SIERRA", ]
table(regioncostaorsierra.cases$REGION)   # Solo se muestran los casos de SIERRA Ó COSTA.

# La opcion != (excluir ó distinta de)

allregionsunlesscosta = data1[data1$REGION!="COSTA",] # Excluimos la COSTA. 
table(allregionsunlesscosta$REGION) # Nos muestra las demas regiones

allregionsunlesscostansierra = data1[data1$REGION!="COSTA" & data1$REGION!="SIERRA",]
table(allregionsunlesscostansierra$REGION)  # Y podemos seguir excluyendo, más y más. 

# Combinando operadores logicos y arimeticos. 

lessclientsandnosierra = data1[data1$NUMERO.DE.CLIENTES==min(data1$NUMERO.DE.CLIENTES) & data1$REGION!="SIERRA",]
table(lessclientsandnosierra$NUMERO.DE.CLIENTES, lessclientsandnosierra$REGION)
# Combinamos los casos de 0 clientes para una entiendad en todas las regiones menos SIERRA.


                                           # Se aprende haciendo - MBD (1) # TAREA 1

tarea1 = data1[data1$TIPO.DE.ENTIDAD=="BANCOS GRANDES" & data1$REGION=="SIERRA" & data1$TIPO.DE.DEPOSITO>=1000,]
summary(tarea1)
View(tarea1)

# Capitulo 2: MANEJO DE BASES DE DATOS USANDO DPLYR

file.choose()

iowa1 = read.csv("C:\\User
                 s\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\Iowa_Liquor_Sales.csv", 
                 stringsAsFactors = FALSE)
names(iowa1) 
View(iowa1)
# Digamos que de todas las variables que hay solo unos interesan unas pocas, aqui ocupamos "select"

# Estructura: select(data, x1, x2, x3) Pero esto se muestra en consola, asi que lo que usualmente se hace, es asignar
# este select a una nueva base. Se seleccionan variables A PARTIR de su nombre.

selectdata = select(iowa1, Invoice.Item.Number, 
                    Vendor.Name, Category)
View(selectdata)
# Digamos que solo estamos interesado de la variable numero 2 a la variable numero 5. 
#  seleccionando por numero de columna
first1to5numbers = select(iowa1, c(2:5))
View(first1to5numbers)
# seleccionando lo mismo pero por nombre de columna
first1to5names = select(iowa1, Date:Address)
View(first1to5names)
# Digamos que tenemos una variable de interes y necesitamos que esa este en una misma columna, por ejemplo Category
# aqui lo que tenemos que ocupar es la funcion "everything"

everythingtest = select(iowa1, Category, everything())
View(everythingtest)
# y si queremos más variables en primer lugar por ejemplo Pack 

everythingtest = select(iowa1, Category, Pack, everything())
View(everytestt)

# EXCLUYENDO DATOS CON SELECT
# Digamos que queremos excluir Date, solo necesitamos decir la variable y poner un signo de resta antes de escribirla. 
everythingtest = select(iowa1, -Date)
# Y podemos seguir agregando
everythingtest = select(iowa1, -Date, -Address, -City)
names(everythingtest) # verificamos y efectivamente las variables ya no estan aqui.

# Y tambien podemos excluir por numero de columnas
everythingtest = select(iowa1, -c(1,3,5))
# Excluyendo entre vectores de variables, ya sea por numeros.
everythingtest = select(iowa1, -c(1:5))
everythingtest = select(iowa1, -c(Address:Vendor.Name)) # O por nombres.

# Filtrando bases de datos utilizando la función filter

filtertest = filter(iowa1, City=="DAVENPORT")
# Te dice, filtra, usando la base de datos iowa, los casos donde City sea igual a Davenport.

table(filtertest$City)
table(iowa1$City)

# El signo , es sinonimo que el &. 

filtertest = filter(iowa1, City=="DAVENPORT", City=="CEDAR RAPIDS")
# Esto te mostrar 0 datos encontrados. ¿Porque? Por que no pueden ser DAVENPORT y CEDAR RAPIDS a la vez, si es uno, no puede
# ser el otro. Entonces se modifica

filtertest = filter(iowa1, City=="DAVENPORT" | City=="CEDAR RAPIDS")
# El signo | es sinonimo de O (opcional).
    # Ahora podemos convinar filtros.
names(iowa1)
filtertest = filter(iowa1,
                    iowa1$City =="DAVENPORT" & 
                      Bottles.Sold >= 6  )
min(filtertest$Bottles.Sold)                      
max(filtertest$Bottles.Sold)
                    
zz = iowa1[iowa1$City=="DAVENPORT" & 
           iowa1$Bottles.Sold >= 6, ]
# Digamos que solo queremos visualizar ciertas filas (Solo filas, con todas las variables [columnas]), otro recurso es slice

slicetest = slice(iowa1, 350:420) 

                    # Creación de nuevas variables en la misma base de datos o en otra base de datos -MDB-dplyr

# Puedes crear una variable dentro de tu base de datos original usando la función MUTATE
# Puedes crear una variable EN OTRA BASE de datos usando la función TRANSMUTE.

iowa1 = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\Iowa_Liquor_Sales.csv", 
                 stringsAsFactors = FALSE,
                 header = TRUE)
# Con STR podemos identificar de que tipo son nuestras variables, sin son char, num, float, int, etc. 
str(iowa1) 

# por ejemplo, ahi podemos identificar que las ventas en dolares, estan como caracteres, en vez de numeros. 

mean(iowa1$Volume.Sold..Gallons.)
# R = 1.470449 
mean(iowa1$Sale..Dollars.) # esto me va a marcar un error, no podemos sacar su media porque es un valor char. 
# o para crear formulas, por ejemplo ingresos menos costos, o precios por cantidades, tendremos un ingreso, necesitamos transformarlo.
log(mean(iowa1$Volume.Sold..Gallons.))  # Y demas formulas.

# Por ejemplo, state bottle cost esta en formato de caracter (char), no podemos realizar operaciones con el
# y para variar, los datos vienen con un signo de dolar $, entonces realizaremos este cambio en 2 pasos. 

# 1. el primer paso es quitar el caracter que no nos sirve, ocupando la funcion substr, que es equivalente a extrae.
substr(iowa1$State.Bottle.Cost, 2, 15) # Aqui nosotros le indicamos, de state bottle cost tomame en cuenta a partir
# del segundo caracter, con un maximo de 15 caracteres. La estructura es: substr(x, start, stop)

# 2. El paso 2 es transformar estos valores a numericos con la sencilla indicacion de as.numeric
as.numeric(substr(iowa1$State.Bottle.Cost, 2, 15)) # Y listo, nos arroja todos los datos en valor numerico. 

# Aqui ya podemos empezar a realizar operaciones, por ejemplo
as.numeric(substr(iowa1$State.Bottle.Cost, 2, 15))*iowa1$Bottles.Sold # esto me arroja todas las entradas multiplicas

# probemos lo mismo creando 2 nuevas variables.

Bottle.Cost = as.numeric(substr(iowa1$State.Bottle.Cost, 2, 15))
Bottles.Sold = iowa1$Bottles.Sold
Bottle.Cost*Bottles.Sold

# Pero con las funciones mutate o transmutate, esto no sera necesario. Esto sera asi.

iowa1 = mutate(iowa1,
               Total.Cost=
                 as.numeric(substr(iowa1$State.Bottle.Cost, 2, 15))*Bottles.Sold)
str(iowa1$Total.Cost) # Se crea la nueva variable de costo total.

# Hagamos lo mismo, pero ahora con State Bottle Retail.

iowa1 = mutate(iowa1,
               Profits= 
                 as.numeric(substr(iowa1$State.Bottle.Retail, 2, 15))*Bottles.Sold)
# Podemos verificar en el enviorment que efectivamente, ya hay 26 variables, en vez de las 24 iniciales.
# E inclusive la funcion mutate, nos permite crear varias variables a la vez, por ejemplo, trabajemos con la anterior.
iowa1 = mutate(iowa1,
               Profits= 
                 as.numeric(substr(iowa1$State.Bottle.Retail, 2, 15))*Bottles.Sold,
               Total.Income = Profits-Total.Cost)
# Despues de crear la variable Profits, creamos la variable Total Income donde ya hacemos uso de esta. 

# con Transmutate haremos exactmente lo mismo, misma estructura, la diferencia sera que el nombre de la base de datos
# debe ser diferente, dado que lo que se creara sera un nuevo data frame pero con las nuevas variables

iowa2 = transmute(iowa1,
                  Profits= 
                    as.numeric(substr(iowa1$State.Bottle.Retail, 2, 15))*Bottles.Sold,
                  Total.Income = Profits-Total.Cost)
# Se ha creado un nuevo data frame pero con 2 nuevas variables, dado que en esta formula no tenemos la primera. 

# La funcion de ambas es crear variable a traves de funciones, manipulaciones, etc.

# If_else : podemos hacer el uso de condiciones, La estructura es; if_else(condition, true, false, missing = NULL)

iowa1 = mutate(iowa1,
               Results = if_else(Total.Income >0, "Income", "Losses"))
# Aqui le decimos, si, el ingreso es mayor a 0, entonces utilidad, sino, perdida. Se crea la nueva variable, una nueva
# columna que arroja perdidas o utilidades
table(iowa1$Results)
# Aqui vemos que hay 58,375 utilidades y 3 perdidas.
# Ya si queremos esto en una nueva base podemos utilizar trasmute, lo cual no es el caso.

                  # Uniendo bases, uniendo filas y columnas: join, bind_rows, bind_col - MBD

# ¿Trabajas con varios libros de excel a la vez y quieres consolidarlos? ¿Tienes archivos con datos ordenados cronológicamente?
  # puedes utilizar los comandos join, bind_col, bind_row.
file.choose()

# UNIENDO BASES considerando que la varible Union NO esta repetida

salesA = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\Iowa_Liquor_Sales_A.csv")

salesB = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\Iowa_Liquor_Sales_B.csv")

# Vamos a cruzar informacion como en Excel. Con un indice/coincidir.

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
#                Una unión semi difiere de una unión interna porque una unión interna devolverá una fila de x por cada fila coincidente de y, 
#                  donde una unión semi nunca duplicará filas de x.

  # Nest_Join: devuelve todas las filas y todas las columnas de x. Agrega una columna de lista de tibbles. Cada tibble contiene todas las filas de y que coinciden con esa fila de x. 
#              Cuando no hay coincidencia, la columna de la lista es un tibble de 0 filas con los mismos nombres y tipos de columna que y.

  # Anti_Join: devuelve todas las filas de x donde no hay valores coincidentes en y, manteniendo solo columnas de x.

# Y la estructura para todas es la siguiente:
  # any_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

# Aqui sabemos que x & y son las bases de datos.

# Necesitamos que los datos que estan en A (left) se quedan y se unan lo que estan en B (right), entonces ocuparemos un Left_Join
names(salesB)
salesC = left_join(salesA, salesB,by="Invoice.Item.Number") 
# se crea una base de datos con 3 variables nuevas, ya que de las 4 se omite la variable clave 2 que hace el match.
# Pero, por ejemplo aqui, las 2 tienen Date, y esta se repite, ¿Que podemos hacer aqui?

DEP.BP.A = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\DEP_2016_BP_A.xlsx",
                     detectDates = TRUE)

DEP.BP.B = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\DEP_2016_BP_B.xlsx", 
                     detectDates = TRUE)

View(DEP.BP.A)
View(DEP.BP.B)

names(DEP.BP.A)
names(DEP.BP.B)

str(DEP.BP.A)
str(DEP.BP.B)

names(DEP.BP.B)= c("ENTIDAD", "NUMERO_DE_CUENTA", "SALDO") # Cambiamos los nombres

# Para cambios de variables estan:
 # as.factor
 # as.character
 # as.numeric
 # as.integer

DEP.BP.A$OFICINAS = as.numeric(DEP.BP.A$OFICINAS) # Ya hemos cambiado oficinas a un numero. 
# Aqui el caso es diferente, las aqui no hay un ID que los identifique pero si hay un variable es que entidad
# y tambien hay variables repetidas. Para esto ocuparemos la funcion bind_cols.

Binstest = bind_cols(DEP.BP.A, DEP.BP.B)
# se combinan creando un data frame de 13 variables, aunque claro, aqui las variables se repiten. Para eliminar podemos ocupar un select.
View(Binstest)
Binstest = select(Binstest, -ENTIDAD1)
# Con este sencillo paso, ya excluimos la variable desde el mismo resultado. 

# Esta base debe tener el mismo numero de observaciones para que pueda ser unido. ¿Que pasa si no lo son? 
# Mismas variables, mismos datos pero diferente numero de observaciones. Por ejemplo digamos que es un historico que se quiere en una unica base
# se requiere que tengan las misma variables, tanto en uno como en otro. Aqui si lo cumplen, lo que se unira seran las filas.
# otro requerimiento es que tengan el mismo formato.

DEP.BP.C = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\DEP_2016_BP_C.xlsx",
                     detectDates = TRUE)
DEP.BP.D = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\DEP_2016_BP_D.xlsx",
                     detectDates = TRUE)
Binstest = bind_rows(DEP.BP.C, DEP.BP.D)

# Se muestra el error de que oficinas para dep.bp.d esta como numerica. cambiaremos la variable a char.
str(DEP.BP.D)  # Verificamos que nos muestra
class(DEP.BP.D$OFICINAS) # Un camino mucho más rapido es verificarlo de esta manera.

DEP.BP.D$OFICINAS = as.character(DEP.BP.D$OFICINAS) # Y asi es como esto ya se pudo combinar, se convirtio en una base
# con muchas más observaciones pero al igual 12 variables. Automaticamente se ordenan por la fecha.

                     # Ordenando los datos y obteniendo valores únicos MBD-dplyr

# Ocuparemos DISTINCT (obtener valores unicos en una columna) & ARRENGE (ordenar de forma ascendente o descente)
file.choose()

iowa1 = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\Iowa_Liquor_Sales.csv",
                 header = TRUE,
                 stringsAsFactors = F)
View(iowa1)

distinct(iowa1, 
         City)
# Nos da los valores unicos que se encuentran en esa columna (Quita Duplicados). Aqui vemos un problema, 
# las ciudades se repiten como nuevas variables pero en minuscula. Vamos a cambiarlo todo a minuscula para que se combine. 

# SAPPLY para cambiar los valores.

iowa1$City = sapply(iowa1$City, tolower) # Y listo, ya no hay valores repetidos.

# Tambien con mutate se pudo haber cambiado.

iowa1 = mutate(iowa1, 
               City=sapply(iowa1$City, tolower)) # ToLower = Minusculas

# Y lo mismo para cambiar a mayusculas. 
iowa1 = mutate(iowa1, 
               City=sapply(iowa1$City, toupper)) # ToUpper = Mayusculas
arrange(iowa1, City) # Ordenamos la variable city, y la agregamos al data frame.

iowa1$City = arrange(iowa1, City) # Por default lo hace de manera ascendente. Si lo queremos de manera descendente, tenemos que agregarlo a la formula.

iowa1$City = arrange(iowa1, desc(City)) # Ya losa acomodo de manera descendente.

# Y podemos seguir agregando en el campo.

iowa1 = arrange(iowa1,Sale..Dollars.,
                Bottles.Sold) # Y asi se va ordenando y agregando. 

# TAREA 2 : 

file.choose()
tarea2 = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\DEP_2016_BP.xlsx")
View(tarea2)
str(tarea2)
names(tarea2)
tarea3 = filter(tarea2, TIPO.DE.ENTIDAD == "BANCOS GRANDES", REGION == "SIERRA", SALDO>=1000) # filtros.

distinct(tarea2, TIPO.DE.ENTIDAD) # verificamos datos

summary(tarea3) # calculamos medidas de tendencia central.

iowa3 = select(iowa1, c(1, 2, 3)); arrange(iowa3, City)


# Modulo: EXTENSIONES MANEJO DE BASES DE DATOS

# Resumiendo la información: las poderosas funciones (Agrupar informacion) group_by & (Resumir informacion )summarise 

iowa1 = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\BASES_DATOS_BASES\\Iowa_Liquor_Sales.csv",
                 header = TRUE,
                 stringsAsFactors = F)

mean(iowa1$Sale..Dollars.) # Nos marca error porque no es numerica. Asi que la trasformamos. 

as.numeric(substr(iowa1$Sale..Dollars., 2, 15))

iowa1 = mutate(iowa1, Sale..Dollars. = 
                 as.numeric(substr(iowa1$Sale..Dollars., 2, 15)))
class(iowa1$Sale..Dollars.)
mean(iowa1$Sale..Dollars.) # Ahora si nos arroja la media. 

# Digamos que ahora lo que queremos saber es la media de ventas de dolar (Sales Dollars) por ciudad, para eso tenemos que crear una 
# agrupacion.  Estructura : group_by(.data, ..., add = FALSE, .drop = group_by_drop_default(.data))

iowa.group1 = group_by(iowa1, City) # Le pido que mi base de datos este agrupada a partir de City.

summarise(iowa.group1, 
          media = mean(Sale..Dollars.)) # Nos arroja 6 medias para 6 ciudades cuando solo deben ser 3, asi que las transformamos.

distinct(iowa1, City)

iowa1$City = sapply(iowa1$City, tolower)
iowa1$Category.Name = sapply(iowa1$Category.Name, tolower)

summarise(iowa.group1, 
          media = mean(Sale..Dollars., na.rm = T)) # Ahora si nos muestra solo 3 medias. Colocamos na.rm=TRUE solo para
# asegurarnos que no cuente lo valores faltantes.

# Group_by: agrupa por categoria que nosotros indiquemos
# summarise: utiliza la variable agrupada, y le empieza a calcular funciones. 

summarise(iowa1, 
          media1 = mean(Sale..Dollars., na.rm = T)) # Por ejemplo si lo intentamos con iowa1, la base NO agrupada
# no nos arroja nada mas que una media unica, mientras con grupo nos arrojara TODO para las categorias.(Ciudades)

# Y podemos seguir trabajando sobre la misma variable.

summarise(iowa.group1, 
          media = mean(Sale..Dollars., na.rm = T),
          maximo = max(Sale..Dollars.), 
          minimo = min(Sale..Dollars.), 
          mediana = median(Sale..Dollars.),
          PQ = quantile(Sale..Dollars., 0.25),
          UQ = quantile(Sale..Dollars.,0.75),
          contar=n(),
          perdidos = sum(is.na(Sale..Dollars.)))
# Sacamos el maximo, minimo, mediana, primer, tercer cuartil, el conteo de cada variable y el numero de datos perdidos.
# Y todo esto se puede mandar a un data frame de resumen.

resumen = data.frame(summarise(iowa.group1, 
                               media = mean(Sale..Dollars., na.rm = T),
                               maximo = max(Sale..Dollars.), 
                               minimo = min(Sale..Dollars.), 
                               mediana = median(Sale..Dollars.),
                               PQ = quantile(Sale..Dollars., 0.25),
                               UQ = quantile(Sale..Dollars., 0.75),
                               contar=n(),
                               contar.6 = sum(Bottles.Sold<6),
                               conteo.neto = contar - contar.6,
                               perdidos = sum(is.na(Sale..Dollars.))))


# El operador "pipe": construyendo filtros complejos

# El operador pipe utiliza esto "%>%"
# y sirve para continuar una linea sin ejecutarla, compactando el calculo. 

iowa1%>%
  group_by(City, Category.Name)%>%   #No olvidar que se puede agrupar por varias variables.
  summarise(media=mean(Sale..Dollars.))
# PIPE sirve para no tener que seleccionar todo y darle ejecutar, sino que aqui se indica; utiliza la BD, CONTINUA, 
# agrupa por ciudad, CONTINUA y hazme un summarise de esa agrupacion para la media de Sale Dollars.
# con esto TAMPOCO se generan objetos en el enviornment.

## FIN DEL CURSO.