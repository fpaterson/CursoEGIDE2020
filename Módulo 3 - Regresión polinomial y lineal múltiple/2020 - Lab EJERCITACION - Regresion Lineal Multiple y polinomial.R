#Para ajustar un modelo de regresi�n lineal m�ltiple utilizando m�nimos cuadrados,
#de nuevo use la funci�n lm (). La sintaxis lm (y~x1 + x2 + x3) se usa para ajustar un
#modelo con tres predictores, x1, x2 y x3. La funci�n summary() ahora
#genera los coeficientes de regresi�n para todos los predictores.

library(MASS)
library(ISLR)

#Conjunto de datos #Boston, que registra medv (mediana del valor de un inmueble) para 506 
#inmuebles alrededor de Boston

Boston <- MASS::Boston
names(MASS::Boston)

lm.fit=lm(medv~lstat+age,data=Boston)



#Eldataet  de Boston contiene 13 variables, por lo que ser�a engorroso
#tener que escribirlos a todos  para realizar una regresi�n usando todos los
#predictores. En cambio, podemos usar el sig atajo:

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)




#Como hab�amos ya visto antes podemos acceder a los componentes individuales 
#de un objeto del tipo summary  por sus respectivos  nombres (escriba ?summary.lm para ver los elementos
#disponibles. Por ejemplo, summary(lm.fit)$r.sq nos da el R2 
#y summary(lm.fit)$sigma nos da el RSE.


# -->  ACTIVIDAD 1: hacer summary, ver la formula de regresi�n y preparar 
# un dataframe con los resultados de la regresion


names(summary(lm.fit))
summary(lm.fit)$adj.r.squared

mod.reg = as.character(summary(lm.fit)$call)[2]
mod.r2_aj = summary(lm.fit)$adj.r.squared

r2.ajustado = data.frame(modelo = mod.reg, r2.aj = mod.r2_aj, StringsAsFactors = F)

#Qu� pasa si queremos probar la regresi�n m�ltiple pero sin un campo?
# -->  ACTIVIDAD 2: SACAR UN CAMPO Y ACTUALIZAR EL DATAFRAME CON LAS REGRESIONES













####################################################
# Interacci�n entre t�rminos ############
####################################################

#Es f�cil incluir t�rminos de interacci�n en un modelo lineal utilizando la funci�n lm().
#La sintaxis lstat: age le dice a R que incluya un t�rmino de interacci�n entre
#lstat y age  (o sea lstat x age). La sintaxis lstat*age incluye simult�neamente lstat, age,
#y el t�rmino de interacci�n lstat � age como predictores; es una forma abreviada de
#lstat + age + lstat: age.

lm.fit2 <-lm(medv~lstat*age,data=Boston)
summary(lm.fit2)





# -->  ACTIVIDAD 3:  ACTUALIAR EL DATAFRAME CON LAS REGRESIONES








####################################################
# Transformaciones no lineales de los predictores
####################################################

#dado un predictor X, podemos crear un predictor X^2
#usando I (X ^ 2). La funci�n I () es necesaria ya que ^ tiene un significado especial

lm.fit3=lm(medv~lstat+I(lstat^2),data=Boston)
summary(lm.fit3)

attach(Boston)


# y las predicciones
# -->  ACTIVIDAD 4:  ARMAR VECTOR DE PREDICCIONES






# grafico ahora las predicciones para cada valor de lstat
par(mfrow=c(1,1))

# -->  ACTIVIDAD 5:  PLOTEAR Y FUNCION POINTS












#las prediccines est�n ordenadas en funci�n de sort(lstat)
head(lstat)
head(sort(lstat))
head(pred.fit3)

# -->  ACTIVIDAD 6:  PLOTEAR EN FORMA CORRECTA











# probemos con un plinomio de grado 4
# aqu� pudo omitir data=Boston porque cargu� el dataset con attach

lm.fit4 <- lm(medv ~ poly(lstat, 4))

# -->  ACTIVIDAD 7: ARMAR PREDICTOR, PLOTEAR, Y CARGAR EN DATFRAME DE REGRESION










# -->  ACTIVIDAD 8: COMPLETO pOLINOMIO DE GRADO 8 Y GRADO 16








###############################################
# polinomial m�ltiple
###############################################

lm.pol.mult <- lm(medv ~ poly(lstat, 2)+poly(age,3))

# -->  ACTIVIDAD 9: ARMAR PREDICTOR, PLOTEAR, Y CARGAR EN DATFRAME DE REGRESION










# -->  ACTIVIDAD 10: CORREGIR Y PLOTTEAR




#Por supuesto, no estamos de ninguna manera restringidos al uso de transformaciones polinomiales
#de los predictores. Aqu� intentamos una transformaci�n logaritimca.

lm.log = lm(medv~log(rm),data=Boston)


# y tambi�n,porque no, aplicar una transformaci�n sobre la variable objetivo
lm.log.obj = lm(log(medv)~poly(lstat, 4),data=Boston)

# -->  ACTIVIDAD 11: ARMAR PREDICTOR, PLOTEAR, Y CARGAR EN DATFRAME DE REGRESION
