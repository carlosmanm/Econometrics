# Clase 4

# Matematicas/Estadistica

#Medidas de Tendencia Central Basica

x=c(24,76,87,6, 76, 54,98,56,98,66,55,48, 91, 33, 44, 67, 66, 42, 41, 34)
summary(x) #Resumen 
cumsum(x) #Sumatoria
table(x) #Frecuencia Absoluta
cumsum(table(x)) #Frecuencia Absoluta Acumulada
prop.table(table(x)) #Frecuencia Relativa
cumsum(prop.table(table(x))) #Frecuencia Relativa Acumulada
mean(x) #Media
median(x) #Mediana
as.numeric(names(which(table(x)==max(table(x))))) #Moda
quantile(x, 0.5) #Cuantiles
sapply(x, mean)
scale(x)

# Algebra Lineal

#Construccion de matrices:
w=1:12
matrix(w, nrow = 3, byrow = TRUE)
matrix(w, ncol = 4, byrow = TRUE)
D= matrix(c(31, 25, 35, 7), nrow=2) # La forma clasica de crear matrices. 
k=1:4
h=c(1,3,5,7)
v=rep(0,4)
rbind(k, h, v) # concatenados por filas
cbind(k, h, v) # concatenados por columnas
g=diag(4) # Matriz unidad con 4 unos en la diagonal. 
rbind(k, h, v, g)

install.packages("expm")
A = matrix(1:4, nrow=2) # Declaracion de matrices
B = matrix(1, nrow = 2, ncol = 2)
C = matrix(1:6, nrow = 2)
A + B # Suma
A - B # Resta
5*C # 5 por C       
A%*%B # El producto de A por B
t(C) # La traspuesta de C
det(A) # El determinante de A
det(C) # El determinate de C NO existe porque no es una matriz cuadrada 
solve(A) # Inversa de A
solve(B) # La inversa de B no existe porque no es invertible dado que su rango es 1 y no 2.
qr(B)$rank # Confirmamos que su rango es de 1 y no de 2. 
B%^%20 # Calculamos la potencia vigesiva de B. 
mean(A) # Media de A
# Esto resuelve sistema de ecuaciones por ejemplo aqui el sistema seria: x+3y=1 y 2x+4y=3
solve(A, c(1,3))  # esta solucion nos arroja de x=2.5 e y=-0.5
solve(D, c(30,30)) # esta solucion nos arroja x=1.2765 e y=-0.2735

#Regresion Lineal

edad = c(1, 2, 3, 5, 7, 9, 11, 13)
altura = c(76.11, 86.45, 95.27, 109.18, 122.03, 133.73, 143.73, 156.41)
datos1 = data.frame(edad, altura)
plot(datos1)
reg_edad_altura = lm(altura~edad, data = datos1)
reg_edad_altura
abline(reg_edad_altura)
summary(reg_edad_altura)

#Covarianzas y Correlacion 

X = data.frame(V1= c(1, 1, 2, 3), 
               V2 = c(-1, 0, 3, 0), 
               V3 = c(3, 3, 0, 1))
View(X) #Visualizacion
cov(X$V1, X$V2)
cov(X$V1, X$V2)*3/4
cov(X) #Covarianza muestral
n=dim(X)[1]
((n-1)/n)*cov(X) #Covarianza poblacional
cor(X)
cov(scale(X))
