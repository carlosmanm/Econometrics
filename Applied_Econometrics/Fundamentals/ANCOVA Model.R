# Modelo ANCOVA

# Un modelo ANCOVA es una extension de los ANOVA. Estos modelos los cuales se conocen como Analisis de Covariables 
# o Analisis de Covarianzas, y funciona igual que los modelos ANOVA solo que ahora se incorporan variables 
# cuantitativas. Aqui podemos poner, Salario : f( Ubicacion Geografica+Gasto en Transporte) o 
# Ventas : f (Clima+Gastos en Publicidad)

install.packages("lattice")
install.packages("openxlsx")
library(lattice)
library(openxlsx)
file.choose()

anova = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\salarios.xlsx")
attach(anova)
names(anova)

## Construimos el modelo

modelo.ancova = lm(w~female+educ, 
                  data = anova)
summary(modelo.ancova)
# Aqui tenemos una variable cualitativa dicotomica que es el genero, 1 si es mujer, 0 si es hombre y una variable
# cuantitativa que es el numero de años cursados. 
# Pasemos con la interpretacion: para el coeficiente de años cursados, que nos arroja 0.50645, se tiene que 
# interpretar de esta manera; el incremento de 1 año de educacion dara como resultado un incremento de 0.50645
# unidades de dolares en el salario. (resultado de un modelo de regresion comun y silvestre)
# Para la variable dicotomica: las mujeres en promedio ganan -2.27336 menos que los varones, ¿cuanto ganan los
# varones? pues el intercepto, 0.62282.

## Graficando la regresion.

xyplot(w ~ educ|female, data = anova, 
      panel = function(x, y, ...)
        { 
        panel.xyplot(x, y, ...)
        panel.lmline(x,y, ...)
        }
      )
# Aqui ocupamos la paqueteria Lattice. Aqui visualizamos diferencias en las regresiones, si es masculino o si es
# femenino, se puede apreciar en la pendiente de la recta. 

##### Modelos multiplicativos.
# ¿De que se trata un modelo ANCOVA multiplicativo? 
# En el ejercicio anterior donde mediamos W=f(female+education), pero nosotros tambien podemos incorporar en nuestra
# ecuacion una variable de tipo multiplicativo, por ejemplo, "educ:female", que haga las veces de variable de
# control. En general a estas variables que agregamos en el modelo se les conoce como Variables de Control, dado que
# son multiplicativas. Es decir, nos interesa saber o controlar, en ese mismo momento, los años de educacion
# por (o según) el genero.  

# y = a1+a2+b1+b2

# alfa 1 (sera Bo)


# alfa 2 va a ser el termino diferencial, es decir, female (o es hombre o es mujer).  
# alfa 2 (intercepto diferencial), la pregunta asociada a esto es, ¿Las 2 regresiones tienen el mismo intercepto?
# es decir, el primer intercepto sera para los hombres, y el segundo para las mujeres. ¿Tienen el mismo?
# ¿Porque se habla de 2 regresiones? Recordemos que hay 2 regresiones porque hay 2 posibles resultados, que sea
# masculino o que sea femenino. 


# b1 (esta asociado a education (educ))
# b2 esta asociado  a la parte multiplicativa, tambien conocido como(coeficiente de pendiente diferencial-alterador)
# la pregunta asociada a este coeficiente es, ¿Las 2 regresiones tienen la misma pendiente? 
# dado que estamos trabajando con 2 variables dicotomicas, hay 2 regresiones, para los hombres y para las mujeres,
# la situaciones es que estas regresiones NO tengan el mismo coeficiente de pendiente. Puede que para las mujeres
# tenga mayor inclinacion que para los hombres, o viceversa.
# Esto permite diferenciar entre los coeficientes de las pendientes de las 2 categorias. Mientras el intercepto 
# diferencial permite diferenciar entre los interceptos de las 2 categorias.(conocidas como regresiones concurrentes)

ancova.multiplicativo = lm(w~female+educ + educ:female, data =anova)
summary(ancova.multiplicativo)
# hacemos un modelo de regresion, de el salario (w) en funcion del genero (female) más la educacion (educ), 
# más la educacion controlada por el genero. 

# Interpretamos:

# a1 =  0.20050
# a2 = -1.19852 
# b1 =  0.53948
# b2 = -0.08600
# La teoria dice, si alfa2 (intercepto diferencial), es significativo entonces las regresiones NO tienen el mismo 
# intercepto, pero en este caso NO lo es, para Alfa2 que es female, el t value NO es mayor que 2 absolto, (-0.905)
# y su p value NO es menor a 0.05 (0.366), puedo decir que las 2 regresiones TIENEN el mismo intercepto.
# La teoria dice que si Beta2 (coeficiente de pendiente diferencial), si este es significativo RECHAZO que las 2
# regresiones tienen la misma pendiente, pero en este caso NO son significativas, dado que b2 tiene un t value
# de -0.830 no mayor al 2 absoluto y un p value de 0.407 NO menor a 0.05. Entonces se acepta que TIENEN
# la misma pendiente. 

# al parecer las 2 regresiones asociadas al genero tienen el mismo intercepto y la misma pendiente. 
# No olvidar que si  nos piden los coeficientes: todo va con respecto al intercepto, por ejemplo si nos piden el
# coeficiente de female, pues es 0.20050 (intercepto) -  -1.19852  = 
0.20050-1.19852 # R : -0.99802
# y el del multiplicativo o de control es 0.20050 (intercepto) - 0.08600  =
0.20050 -0.08600 # R: 0.1145 