# Caso 4: Modelo de Regresión Múltiple

library(forecast)
file.choose()
base = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\2. Econometria Aplicada 1\\material\\REG_1.csv")

# Construir modelo de regresion multiple

# y = bo + b1x1 + b2x2

attach(base)

names(base)

modelo1 = lm(M1~GP+TI, data=base)
summary(modelo1)
# Se muestra un modelo mix: La oferta monetaria M1 (en millones) esta en funcion de el Gasto Publico GP (en millones) 
# y la tasa de interes TI (en porcentaje)

# Interpretar los resultados:
# 1) Evaluar los signos esperados:

# se sabe de la teoria macroeconomica que la tasa de interes TI y la oferta monetaria M1 son inversamente proporcional, 
# cuado sube TI baja el M1, y viceversa. Cuando la tasa de interes baja la liquidez. El signo del coeficiente debe ser negativo
# y lo es: -1283.6208, asi que esta en lo correcto.
# por otro lado el GP cuando aumenta, el M1 tambien. Y el coeficiente es 1.6697, la relacion es positiva, asi que esta bien los signos

# 2) Interpretacion de los coeficientes:
# un aumento de 1 unidad en gasto publico provocara un aumento en 1.6697 de M1 
# un aumento 1 % en la tasa de interes da como resultado una reduccion de 1283.62 en la M1.

# 3) prueba de significancia individual:

# Ho: bk=0
# H1: bk =!0
# 3 criterios:  p-value <0.05, regla 2t: >2 absoluto y las estrellas: coeficientes con mas estrellas, son significativos.

# 2t: se rechaza Ho cuando la t es mayor que 2 en valor absoluto.
# aqui 8.355 para B1 es mayor a 2, entonces se rechaza Ho, y en B2 es -5.448, pero como se toma valor absoluto
# (es decir que no se toman valores negativos) es 5.448 lo cual es mayor a 2, entonces se rechaza Ho. 

# El valor P. Los valores correspondientes de B1 y B2 son: 2.64e-11 y 2.64e-11, los cuales son MUY inferiores a 0.05

# 4) Prueba de significacion conjunta (Prueba F):

# si p value conjunto es < 0.05 rechazo Ho.

# Ho : b1=b2=bk= 0
# H1 : b1=b2=bk=!0
# es pvalue es: p-value: < 2.2e-16 lo cual es MUCHO menor que 0.05, se rechaza contundentemente la Ho.

# 3) coefiente de determinacion (R2) mientras mas cercano a 1, mejor ajustado el modelo. 
# Multiple R-squared:  0.8166; el 81% del modelo esta explicado en el modelo, el residuo esta explicado en variables
# no tomadas en cuenta. 
# Recordemos que es R2 es una funcion lineal creciente; a mas variables R2 mas alto. 
# para poder analizar el modelo de manera más eficaz, es mejor tomar en cuenta el R ajustado:
# Adjusted R-squared:  0.8098  dado que no se considera una funcion lineal creciente y mucho más acida. 


