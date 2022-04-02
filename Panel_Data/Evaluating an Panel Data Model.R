library(plm) #Panel
library(sandwich) #Estimadores EE robustos
library(lmtest) #Contrastes
library(tseries) #Contrastes
library(foreign) #Bases con diversas extensiones
library(gplots) #gráficos
library(memisc) #unión tablas
library(rmarkdown) #automatización de reportes


file.choose()

Panel <-  read.csv("C:\\Users\\Lenovo\\Documents\\R\\Proyectos\\Betametrica\\Panel 2\\bases\\bases\\ocde2.csv",
                   sep=";")



fixed <-plm(EV ~ MED+GS,
            data=Panel,
            index=c("COUNTRY", "ANIO"), 
            model="within")
summary(fixed)




## evaluando autocorrelación.


#Ho: No cross sectional dependece.
#estoy motivado a no rechazar la HO
#si el valor p es menor que 0.05
#Rechazo Ho.

pcdtest(fixed, test = c("lm"))

pcdtest(fixed, test = c("cd"))


#Heterocedasticidad

#estoy motivado a no rechazar la HO.

#ho: homocedasticidad



#si el valor p es menor que 0.05
#Rechazo Ho.
bptest(EV ~ MED+GS, 
       data = Panel, 
       studentize=F)




#correlación serial

#Ho: No correlación serial.
#estoy motivado a no rechazar la HO
#si el valor p es menor que 0.05
#Rechazo Ho.

pbgtest(fixed)

  
#------------ Con efectos aleatorios #---------

random <-plm(EV ~ MED+GS, 
             data=Panel, 
             index=c("COUNTRY", "ANIO"), 
             model="random")

summary(random)





## evaluando autocorrelación.


#Ho: No cross sectional dependece.
#estoy motivado a no rechazar la HO
#si el valor p es menor que 0.05
#Rechazo Ho.

pcdtest(random, test = c("lm"))

pcdtest(random, test = c("cd"))


#Heterocedasticidad

#estoy motivado a no rechazar la HO.

#ho: homocedasticidad



#si el valor p es menor que 0.05
#Rechazo Ho.
bptest(EV ~ MED+GS, 
       data = Panel, 
       studentize=F)




#correlación serial

#Ho: No correlación serial.
#estoy motivado a no rechazar la HO
#si el valor p es menor que 0.05
#Rechazo Ho.

pbgtest(random)
