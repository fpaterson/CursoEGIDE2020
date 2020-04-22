library(MASS)
library(ISLR)

#Conjunto de datos #Boston, que registra medv (mediana del valor de un inmueble) para 506 
#inmuebles alrededor de Boston

Boston <- MASS::Boston
names(MASS::Boston)

View(Boston)

#Ahora graficaremos medv e lstat junto con la regresión de mínimos cuadrados
#línea usando las funciones plot () y abline ()

lstat <- Boston$lstat
medv <- Boston$medv

plot(lstat,medv)


#Comenzaremos utilizando la función lm () para ajustar una regresión lineal simple 
#El modelo tiene medv como la respuesta y lstat como el predictor



lm.fit=lm(medv~lstat,data=Boston)


#invocando al modelo "lm.fit"  se genera información básica sobre el modelo

lm.fit

#Como el modelo lm.fit es de su propia clase, y esta clase en sí es una lista, entonces podemos 
# usar la función de names() para descubrir qué otra información se almacenan en lm.fit.

class(lm.fit)
names(lm.fit)


#Para ver los coeficientes
lm.fit$coefficients

#Para ver los residuos
lm.fit$residuals

#Para ver toda la regresión
summary(lm.fit)


#######VER DETALLES DE SUMMARY con $ y GRAFICAR

plot(lstat,medv)
abline(lm.fit, col="red")



#Para hacer una estimación con lstat=5

predict (lm.fit, data.frame("lstat"=5))


#para hacer varias estimaciones, con estimacion por intervalos
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")

#para hacer varias estimaciones, con estimacion por predicción de la variable
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")





