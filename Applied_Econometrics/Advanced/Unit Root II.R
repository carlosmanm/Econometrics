# Raiz Unitaria 2
data = read.table("https://www.betametrica.ec/wp-content/uploads/2017/02/consumo.txt")
ts.plot(data)
tsdata1 = ts(data, start = c(2004, 8), 
            end = c(2014, 8),
            frequency = 12)
ts.plot(tsdata)
dfuellertest = ur.df(tsdata,
                     type=c("trend"),
                     selectlags = c("BIC"))
summary(dfuellertest)

# Value of test-statistic is: -1.774 5.919 3.7095 

# Critical values for test statistics: 
     #  1pct  5pct 10pct
# tau3 -3.99 -3.43 -3.13
# phi2  6.22  4.75  4.07
# phi3  8.43  6.49  5.47

# si el valor calculado (primer estadistico) es mayor que el valor tabla o critico
# rechazo Ho.(tau3)
# ¿Es mayor el -1.774 que estos: -3.99 -3.43 -3.13? No entonces no se rechaza Ho
# y la H1 dice que hay Raiz Unitaria. 

# Phillips Perron

pptest = ur.pp(tsdata, 
               type=c("Z-tau"),
               model = c("trend"),
               lags = c("short"))
summary(pptest)

# Value of test-statistic, type: Z-tau  is: -1.8165 

# Critical values for Z statistics: 
                    # 1pct      5pct     10pct
# critical values -4.036703 -3.447497 -3.148576

# El -1.8165  no es mayor que ninguno de los valores criticos, no se puede
# rechazar Ho, que dice que hay Raiz Unitaria. (la serie no es estacionaria)

# KPSS test

kpsstest = ur.kpss(tsdata, 
                   type =c("tau"),
                   lags=c("short"))
summary(kpsstest)

# Value of test-statistic is: 0.2748 

#Critical value for a significance level of: 
                # 10pct  5pct 2.5pct  1pct
#critical values 0.119 0.146  0.176 0.216

#El valor al 95% e mayor que el critico o de tabla por lo que No se rechaza Ho.
# y recordemos que la Ho de KPSS es diferente de las demas, aqui es al reves
# aqui se rechaza Ho que dice que NO hay estacionariedad.

# Elliott, Rothenberg \& Stock Unit Root Test: Alta Potencia.
# menos sensible a quiebres estructurales o valores atipicos.
erstest = ur.ers(tsdata, 
                 type=c("DF-GLS"),
                 model=c("trend"),
                 lag.max = 4)
summary(erstest)

# Value of test-statistic is: -1.0653 

# Critical values of DF-GLS are:
              #  1pct  5pct 10pct
#critical values -3.46 -2.93 -2.64
# dado que -1.0653  no es mayor que valores criticos No se rechaza Ho, 
# hay Raiz Unitaria.
