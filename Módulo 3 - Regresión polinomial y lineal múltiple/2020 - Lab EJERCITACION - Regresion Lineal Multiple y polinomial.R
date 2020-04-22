#Para ajustar un modelo de regresión lineal múltiple utilizando mínimos cuadrados,
#de nuevo use la función lm (). La sintaxis lm (y~x1 + x2 + x3) se usa para ajustar un
#modelo con tres predictores, x1, x2 y x3. La función summary() ahora
#genera los coeficientes de regresión para todos los predictores.

library(MASS)
library(ISLR)

#Conjunto de datos #Boston, que registra medv (mediana del valor de un inmueble) para 506 
#inmuebles alrededor de Boston

Boston <- MASS::Boston
names(MASS::Boston)

lm.fit=lm(medv~lstat+age,data=Boston)



#Eldataet  de Boston contiene 13 variables, por lo que sería engorroso
#tener que escribirlos a todos  para realizar una regresión usando todos los
#predictores. En cambio, podemos usar el sig atajo:

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)




#Como habíamos ya visto antes podemos acceder a los componentes individuales 
#de un objeto del tipo summary  por sus respectivos  nombres (escriba ?summary.lm para ver los elementos
#disponibles. Por ejemplo, summary(lm.fit)$r.sq nos da el R2 
#y summary(lm.fit)$sigma nos da el RSE.


# -->  ACTIVIDAD 1: hacer summary, ver la formula de regresión y preparar 
# un dataframe con los resultados de la regresion









#Qué pasa si queremos probar la regresión múltiple pero sin un campo?
# -->  ACTIVIDAD 2: SACAR UN CAMPO Y ACTUALIZAR EL DATAFRAME CON LAS REGRESIONES













####################################################
# Interacción entre términos ############
####################################################

#Es fácil incluir términos de interacción en un modelo lineal utilizando la función lm().
#La sintaxis lstat: age le dice a R que incluya un término de interacción entre
#lstat y age  (o sea lstat x age). La sintaxis lstat*age incluye simultáneamente lstat, age,
#y el término de interacción lstat × age como predictores; es una forma abreviada de
#lstat + age + lstat: age.

lm.fit2 <-lm(medv~lstat*age,data=Boston)
summary(lm.fit2)





# -->  ACTIVIDAD 3:  ACTUALIAR EL DATAFRAME CON LAS REGRESIONES








####################################################
# Transformaciones no lineales de los predictores
####################################################

#dado un predictor X, podemos crear un predictor X^2
#usando I (X ^ 2). La función I () es necesaria ya que ^ tiene un significado especial

lm.fit3=lm(medv~lstat+I(lstat^2),data=Boston)
summary(lm.fit3)

attach(Boston)


# y las predicciones
# -->  ACTIVIDAD 4:  ARMAR VECTOR DE PREDICCIONES






# grafico ahora las predicciones para cada valor de lstat
par(mfrow=c(1,1))

# -->  ACTIVIDAD 5:  PLOTEAR Y FUNCION POINTS












#las prediccines están ordenadas en función de sort(lstat)
head(lstat)
head(sort(lstat))
head(pred.fit3)

# -->  ACTIVIDAD 6:  PLOTEAR EN FORMA CORRECTA











# probemos con un plinomio de grado 4
# aquí pudo omitir data=Boston porque cargué el dataset con attach

lm.fit4 <- lm(medv ~ poly(lstat, 4))

# -->  ACTIVIDAD 7: ARMAR PREDICTOR, PLOTEAR, Y CARGAR EN DATFRAME DE REGRESION










# -->  ACTIVIDAD 8: COMPLETO pOLINOMIO DE GRADO 8 Y GRADO 16








###############################################
# polinomial múltiple
###############################################

lm.pol.mult <- lm(medv ~ poly(lstat, 2)+poly(age,3))

# -->  ACTIVIDAD 9: ARMAR PREDICTOR, PLOTEAR, Y CARGAR EN DATFRAME DE REGRESION










# -->  ACTIVIDAD 10: CORREGIR Y PLOTTEAR




#Por supuesto, no estamos de ninguna manera restringidos al uso de transformaciones polinomiales
#de los predictores. Aquí intentamos una transformación logaritimca.

lm.log = lm(medv~log(rm),data=Boston)


# y también,porque no, aplicar una transformación sobre la variable objetivo
lm.log.obj = lm(log(medv)~poly(lstat, 4),data=Boston)

# -->  ACTIVIDAD 11: ARMAR PREDICTOR, PLOTEAR, Y CARGAR EN DATFRAME DE REGRESION
