## TAREA TASAS DE CRECIMIENTO

# Abre un nuevo script de RStudio, carga la base de datos REG.csv y el archivo Ventas.csv

# 1. Calcule la tasa de crecimiento relativa compuesta de: Ventas y SueldosySalarios (LOG-LIN)

# 2. Calcule la tasa de crecimiento absoluta entre Ventas y SueldosYSarlarios (LIN-LOG)

library(forecast)
file.choose()

base1 = read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\2. Econometria Aplicada 1\\material\\ventas.csv")
attach(base1)
names(base1)

# 1. Calcule la tasa de crecimiento relativa compuesta de: Ventas y SueldosySalarios (LOG-LIN)

# La normalidad: y = bo+b1x1

# Modelo Log-Lin: ln(y) = bo + b1TREND 

timeseriesbase1 = ts(base1, start = c(2006, 1), frequency = 12)
timeseriesbase1
modelologlinventas = tslm(log(VENTAS_NETAS) ~ trend, data = timeseriesbase1)
summary(modelologlinventas)

(exp(7.801e-03 )-1)*100
# La tasa de crecimiento relativa compuesta de Ventas es de 0.7831507

modelologlinsalarios = tslm(log(GSueldosySalarios) ~ trend, data = timeseriesbase1)
summary(modelologlinsalarios)
(exp(4.000e-03)-1)*100
# La tasa de crecimiento relativa compuesta de Sueldos Y Salarios es de 0.4008011


## 2. Calcule la tasa de crecimiento absoluta entre Ventas y SueldosYSarlarios (LIN-LOG)

modelolinlog = lm(VENTAS_NETAS~log(GSueldosySalarios), data = base1)
summary(modelolinlog)
# el incremento en 1% del Sueldos y Salarios dara como resultado un incremento de  1,360,750 millones en Ventas Netas.
