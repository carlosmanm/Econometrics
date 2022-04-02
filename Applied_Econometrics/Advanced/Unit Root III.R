# Raiz Unitaria 3

# Forecast, openxlsx, URCA.

file.choose()
base = read.xlsx("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Curso Econometria - Betametrica\\4. Modelos Econometricos para el Pronostico en Negocios\\Bases de Datos\\base.xlsx", detectDates = TRUE)
attach(base)
names(base)
exports = base[,4]
tsexports = ts(exports, 
               start =c(2006, 1), 
               frequency = 12)
ts.plot(tsexports)

# Contrastes de Raiz Unitaria
# Dickey-Fuller: Baja Potencia.
dftest = ur.df(tsexports, 
               type = c("trend"),
               selectlags = c("BIC"))
summary(dftest)
# Value of test-statistic is: -2.729 2.9143 3.7244 

# Critical values for test statistics: 
        #  1pct  5pct 10pct
# tau3 -3.99 -3.43 -3.13
# phi2  6.22  4.75  4.07
# phi3  8.43  6.49  5.47

# Pero -2.729 no es mayor que ninguno de estos: -3.99 -3.43 -3.13
# por lo que NO se rechaza Ho que dice que hay Raiz Unitaria. No es Estacionaria.

# Phillips Perron

pptest = ur.pp(tsexports, 
               type=c("Z-tau"),
               model=c("trend"), 
               lags = c("short"))
summary(pptest)
#Value of test-statistic, type: Z-tau  is: -4.0929 

# Critical values for Z statistics: 
                    # 1pct      5pct    10pct
# critical values -4.049369 -3.453494 -3.15209
# El valor calculado es mayor a TODOS los valores, se rechaza Ho.
# La Ho es Raiz Unitaria: considerando Phillips Perron, dice que la serie
# no tiene raiz unitaria, si es estacionaria.

# KPSS

kpsstest = ur.kpss(tsexports, 
                   type = c("tau"), 
                   lags = c("short"))
summary(kpsstest)
#Value of test-statistic is: 0.1121 

#Critical value for a significance level of: 
                 # 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216

# Ho: Estacionariedad
# H1: No estacionariedad

# No es mayor en ninguno de los valores de tabla NO se rechaza Ho
# Mi serie de tiempo ES estacionaria.

# Phillips Perron y KPSS nos dice que SI es estacionaria, Dickey Fuller es 
# sensible a quiebres estructurales y valores atipicos.

#ERS
# DF-GLS = Dickey-Fuller - Minimos Cuadrados Generalizados
erstest = ur.ers(tsexports, 
                 type = c("DF-GLS"),
                 model=c("trend"),
                 lag.max = 4)
summary(erstest)
# Value of test-statistic is: -3.5529 

#Critical values of DF-GLS are:
 # 1pct  5pct 10pct
# critical values -3.46 -2.93 -2.64
# supera el valor en 95%
# se rechaza Ho que dice que hay Raiz Unitaria, entonces mi serie
# no presenta problemas de No Estacionariedad.

# Ultimo Contraste: Plus

# Zivot & Andrews (diseñados en presencia de quiebres estructurales en incepcepto, tendencia y ambos)
zatest = ur.za(tsexports, 
               model=c("both"))
summary(zatest)
# Ho : Raiz Unitaria
# H1: No Raiz Unitaria
# Teststatistic: -6.4571 
# Critical values: 0.01= -5.57 0.05= -5.08 0.1= -4.82 

# Como -6.4571  es mayor que 0.05= -5.08 (95%) entonces se rechaza Ho.
# Nuestra serie es Estacionaria.