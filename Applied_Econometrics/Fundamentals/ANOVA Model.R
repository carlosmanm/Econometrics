# Modelo ANOVA

# - No hay librerias especiales

# -- Cargamos la Base de Datos

file.choose()

anova = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\modelo anova.csv")
attach(anova)
names(anova)
## Nos muestra los siguientes nombres: Estate, Salay, Spending, D2, D3 y D4, estos ultimos 3 correspondientes
# a nuestras variables cualitativas dicotomicas.

View(anova) # de hecho, podemos visualizar la tabla, y verificar que en estas ultimas 3 columnas solo hay 0's y 1's.
str(anova)
# y visualizar las caracteriticas de cada variable. 

### Variable Dummy: En estadística y econometría, particularmente en el análisis de regresión, una variable ficticia 
# es aquella que toma el valor 0 o 1 para indicar la ausencia o presencia de algún efecto categórico que se puede 
# esperar que cambie el resultado.


# Tenemos que empezar a diseñar estas variables.

# D2 = 1 si es noreste o norte-centro, 0 ECOP

# D3 = 1 si es sur, 0 ECOP

# D4 = 1 si es oeste, 0 ECOP

## No se encuentra en D1 en la base por razones que se explicaran en el modelo. 

# -- Estimar el modelo

modelo.anova = lm(Salary~ D2+D3+D4, data = anova)
summary(modelo.anova)
# Nos arroja un NA para D4, lo que sucede aqui es que, si yo agrego D4 al modelo
# este es redundante, aqui ocurre la trampa de la variable ficticia.

# La trampa de la variable ficticia: simplemente respetar una formula que es m-1
# m hace referencia a las categorias, por ejemplo, si nosotros tenemos 4 categorias, norte, sur, este y oeste.
# nosotros SOLO debemos incorporar 3 dicotomicas, debido a que m-1.
# por ejemplo, si nosotros tenemos 1er trimestre, 2do trimestre, 3er trimestre y 4to trimestre como categorias
# en nuestro modelo solo debemos incorporar 3 categorias. 
# Si nosotros tenemos Masculino y Fememino, nosotros solo podemos incorporar 1 categoria. 
# Al numero de categorias que tenemos, siempre debemos restarle 1 para incorporar las variables dicotomicas.

# Nosotros al incorporar D4 estoy tomando todas las categorias. Por ese motivo sale NA porque D4 esta redundando.

# En este caso, en este modelo hay Multicolinealidad Perfecta. 
modelo.anova = lm(Salary~ D2+D3+D4, data = anova)
summary(modelo.anova) 

# Correción de la trampa de la variable dummy

modelo.anova1 = lm(Salary~D2+D3, data = anova)
summary(modelo.anova1)


# El intercepto nos indica que el Intercepto (Bo) que guarda el valor de la variable omitida, es de  48015 
# es decir es ese el salario promedio de las personas que viven en el Oeste (D4)

# Y TODO el analisis que ahora se haga tiene que ir en referencia con este intercepto, por lo mismo el nombre es 
# 'Variable de Interes', y los resultados se interpretaran asi.

# El intercepto es 48015, entonces el valor de Oeste (D4) es 48,015. Y para D2 que es Norte y D3 que es Sur
# se interpreta asi: El Salario promedio de los trabajadores del Norte es 1524 unidades mayor que el Oeste (D4)
# Y en el Sur, los salario en promedio son -1721 unidades menor que en el Oeste (D4).
# TODO el analisis estara rodeando este intercepto. Nosotros, de querer los coeficientes, pues bien podemos 
# realizar la suma o la resta respectiva, es decir el coeficiente de Norte seria
48015+1524
# Es decir, 49,539 (D2)
# O para sur realizamos la operacion
48015-1721
# Es decir, 46,294 (D3)
# O bien podemos crearlo en el modelo.

modelo.anova2 = lm(Salary~D2+D3+D4 - 1,
                   data = anova)
summary(modelo.anova2)
# Ya nos muestra los respectivos coeficientes. Todo el analisis esta enfocada en la varible que NO incorporamos
# en el modelo.

# Esto tiene muchos usos, un ejemplo:

# Probabilidad de que NO pague un credito ~ Educacion+Genero+Geografia+Error

# Este tipo de modelos se basa de regresar una Y contra variables dicotimicas, es decir, variables
# que responden a 1 y a 0.
# Estos modelos si son usados, pero no tan sencillos como los hemos estado ocupando, la mayoria de los modelos
# tambien ocupan tanto cualitativas como cuantitativas. No todos los modelos estan en funcion de 1 y 0's.
# por ejemplo, si nosotros estamos modelando las ventas, tal vez podemos agregar variables cualitativas dicotomicas
# como la estacion del año, o la ubicacion geografica, pero tambien influyen otras variables como el salario
# o por ejemplo los gastos en publicidad, es muy extraño que solo ocupemos cualitativas dicotomicas.

install.packages("openxlsx")
library(openxlsx)
file.choose()

anova = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\salarios.xlsx")

attach(anova)

names(anova)

modelo.anova = lm(w~female, 
                  data = anova)

summary(modelo.anova)

# El proceso no es realmente dificil. Lo dificil es la interpretacion. Ahora, ¿Que nos dicen los coeficientes?
# En la variable Female, hay 2 opciones, 1 si es mujer 0 si es hombre, en este caso, la variable que NO
# hemos incorporado es Hombre, por lo cual, su valor lo guardara el Intercepto que es 7.0995, y es justo en el que
# pondremos foco, dado que es nuestra variable de interes. El valor para mujer que es -2.5118 significa que en 
# promedio las mujeres ganan -2.5118 dolares menos con respecto al salario de los hombres.

# ¿como sabemos esto? Cuando es 1 es Mujer y cuando es 0 es Hombre.
# Si el modelo esta : W = Bo + B1Female+u
# Y si Female = 1 entonces B1*1 = B1 que es -2.5118: W = 7.0995-2.5118+u
# Pero si es Hombre = 0 entonces B1*0 = 0 entonces solo queda: W = Bo + U
# y Bo es igual a 7.009 = Hombre. 


#¿Queremos el coeficiente? pues podemos realizarlo sin problema, restando un 1 al modelo y SOLO quedandonos con
# el valor del regresor, dado que ese -1 elimina el Intercepto y solo nos deja con ese valor.

modelo.anova = lm(w~female -1, 
                  data = anova)

summary(modelo.anova) # Nos arroja el coeficiente de las mujeres: 4.5877

# Es muy probable que nosotros No ocupes estos podemos debido a su sencilles. Puede que las podamos ocupar
# es una buena tecnica para desestacionalizar.

# Cual llegaria a ser la diferencia entre el modelo ANOVA Y el modelo ANCOVA ?
# Que el Modelo ANOVA solo utiliza variables cualitativas dicotomicas, mientras en el ANCOVA tiene variables
# cualitativas dicotomicas MÁS variables cuantitativas. 

# Cual llegaria a ser la diferencia entre el modelo ANOVA Y el modelo LOGIY O PROBIT ?
# Un modelo ANOVA considera la Y como cuantitativa, y sus x son cualitativas dicotómicas. 
# Un logit probit considera la Y como cualitativa dicotómica, y sus x pueden ser cualitativas dicotómicas, 
# o cuantitativas.
