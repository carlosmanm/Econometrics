

## Modelo de Regresion Lineal

file.choose()

data = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\material\\REG_1.csv")
names(data)
# M1 : Oferta Monetaria

# GP: Gasto Publico

# IPC: Indice de Precios al Consumidor

# INF: Inflacion

# TI: Tasa de Interes

# Construir un modelo de regresion simple que explique la Oferta Monetaria (M)

attach(data) # Aqui solo se le indica a R que se va a estar trabajando con esta base

# Nuestro modelo es: M1 = Bo + B1*GP

modelo1 = lm(M1~GP, data = data)
summary(modelo1)

# Y esto nos arroja:

# Intercepto (Bo) = 4264.7478
# Error Estandar (u) = 376.7869
# La Prueba T =  11.32
# P Value = 5.49e-16
# Coeficiente asociado al GP, es decir al B1 = 2.3223
# Con su error estandar (u), prueba T y P Value correspondiente: 0.1973, 11.77, < 2e-16.
# Y el R2 del modelo es que = 0.7107

# ¿Como se interpreta esto?

# Bo: $4264.7478 millones de dolares es la oferta monetaria basica sin intervencion de ninguna otra variable
# B1: Incremento en 1 unidad en gasto publico dara como resultado un incremento de 2.32 unidades monetarias en M1

# prueba de significancia individual