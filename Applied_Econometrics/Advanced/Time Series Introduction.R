# Series Temporales en R 

# Propiedades:

  # Tendencial
  # Ciclico  
  # Estacional
  # Estocastico (irregular)

data = read.table("http://www.betametrica.ec/wp-content/uploads/2017/02/PIB.txt")

tsdata = ts(as.vector(as.matrix(data)), start = c(2000, 1),
            end = c(2013, 3), 
            frequency = 4)
      
tsdata      
plot(tsdata)            
ts.plot(tsdata)
abline(v=2008) 
abline(v=2004)
abline(v=2012) # segmentamos con lineas.


hchart(tsdata) # Mejor visoria; primer grafico dinamico.
# Podemos observar que NO es una serie estacionaria dado que su media y su varianza
# No son constantes a traves del tiempo. Viola el supuesto de la 
# estacionariedad debil. Dado esta violacion, es imposible ocupar los modelos ARMA.

 
     # Aspectos Visuales


par(bg=c("lightgreen"))  # relleno de hoja a color lightgreen
plot(tsdata/1000, main = "PIB", sub = "Elaboracion por Carlos Muñoz", 
     xlab = "Tiempo", ylab = "Nivel", col = "blue")
# Acomodo de etiquetas y demas.


