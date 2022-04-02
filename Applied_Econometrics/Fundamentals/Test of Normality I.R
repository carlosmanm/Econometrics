# Contrastes de estabilidad


file.choose()

data = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\3. Econometria Aplicada 2\\Bases de Datos\\REG_1.csv")
attach(data)
names(data)

# - estimando el modelo - #

modelo = lm(M1~GP+IPC+TI, data = data)
summary(modelo)

# suponiendo que ya se hizo el prueba F, T y se comprobo un R2 significativo, suponiendo
# que no tiene autocorrelacion, heterocedasticidad o multicolinealidad, o si se tuvieron
# ya se atenuaron, suponiendo que todo esto ya esta validado, entramos en la etapa de validacion
# a traves de las extensiones d elos contrastes o contrastes complementarios.

# 1. Normalidad 
# Esta ya fue aprobada
# 2. Especificacion
# Esta ya fue aprobada

# -- Contraste de Estabilidad -- #
# se supone que cuando nosotros construimos un modelo econometrico estos tienen que ser estables
# a traves del tiempo. Un modelo que no tenga parametros estables es un modelo explosivo
# y no puede ser tomado para toma de decisiones. 

# pruebas de estabilida de parametros:

# CUSUM: Cumulative Sums Standard Residuals
# Autores: Brown (1975)
# Importantisimo que lo añadamos a nuestras notas tecnicas.

ols = efp(modelo, data = data, type = "OLS-CUSUM")
plot(ols)
# grafico intuitivo, si el modelo NO es estable en los parametros; un modelo siempre es 
# estable si esta sobre los lineas rojas (Intervalos de Confianza): cuando esta fluctuando
# dentro del intervalo tampoco es tan conveniente: aqui podemos observar un quiebre 
# estructural, por lo tanto las variables NO son estables. 

olms = efp(modelo, data = data, type = "OLS-MOSUM") # mobile averange
plot(olms)
# Misma interpretacion: se encuentran partes donde sobresalen de los intervalos
# el modelo NO es estable a traves del tiempo. (tambien por quiebres estructurales)

# Ho: No cambio estructural
# H1: Cambio Estructural
# Si p-value es menor a 0.05 se rechaza Ho


sctest(modelo, type = "OLS-CUSUM", data = data)
# data:  modelo
# f(efp) = 2.018, p-value = 0.002321
# se rechaza Ho (Cambio Estrural: No es estable en los parametros a traves del tiempo)
