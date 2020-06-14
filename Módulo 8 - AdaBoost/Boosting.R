##############################################################################
# Boosting en Boston
##############################################################################

#  GBM = GRADIENT BOOSTED MACHINES
#install.packages("gbm")
library(gbm)
library(MASS)

str(Boston)

set.seed(1)
# train = sample(1:nrow(Boston), nrow(Boston)/2)
train = sample(nrow(Boston), 0.8*nrow(Boston))

# EL argumento "dsitribution" es Gaussain, porque la funci?n de p?rdida es la suma de los cuadrados del error
#  por ser una estimaci?n de una variable cuantitativa (regresi?n)
#  interaction.depth limita la profunidad (niveles de Particiones/ramificaciones) 
# de cada ?rbol.

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,
                 interaction.depth=4)

#  la funci?n summary nos d? el ranking de variables y las grafica
summary(boost.boston)
str(summary(boost.boston))
View(summary(boost.boston))
plot(summary(boost.boston)$var,summary(boost.boston)$rel.inf, xlab="variable", ylab="relevancia", type = "p", col="blue" )


boost.boston_50=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=50,
                 interaction.depth=4)

#  la funci?n summary nos d? el ranking de variables y las grafica
summary(boost.boston_50)


#  predicciones en el set de validaci?n
y.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)

#NOTA DEL HELP: "predict.gbm produces predicted values for each observation in newdata 
#using the the first n.trees iterations of the boosting sequence.

mean((y.boost - Boston[-train,"medv"])^2)

# El MSE de prueba obtenido es bueno; similar a la MSE de validaci?n para RF
# y mejor  al de bagging. Si queremos, podemos correr el alg de boosting
# con un valor diferente del par?metro de regularizaci?n lambda . El valor por defecto
# es 0.01. Probemos con lambda = 0.2.

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)

y.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)

mean((y.boost-Boston[-train,"medv"])^2)

# con un valor diferente del par?metro de regularizaci?n lambda . El valor por defecto
# es 0.1. Probemos con lambda = 0.1, pero con 5000 ?rboles

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=5000,interaction.depth=4,shrinkage=0.1,verbose=F)

y.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)

mean((y.boost-Boston[-train,"medv"])^2)

#  la funci?n predict de gbm permite probar con distinta cantidad de 
#  ?rboles, por lo que no hay que armar el loop for


#  Vamos a probar con diferente cantidad de ?rboles
cant.arb =seq(100, 10000, by=100 )

matriz.pred = predict(boost.boston,newdata = Boston[-train,],n.trees = cant.arb)

matriz.pred[,1:5]

cme = apply((matriz.pred - Boston[-train,"medv"])^2,2,FUN=mean)

matplot(cant.arb,cme,  pch=19, col="red")

names(cme[which.min(cme)])

##############################################################################
# Boosting en churn
##############################################################################

#  GBM = GRADIENT BOOSTED MACHINES
# install.packages("gbm")
library(gbm)

ds = read.csv("Churn_Modelling.csv")

# quito campos que no uso  factorizo la variable objetivo
ds = ds[-c(1:3)]
str(ds)


set.seed(1)
train = sample(nrow(ds), 0.8*nrow(ds))



# EL argumento "distribution" es adaboost, porque la funci?n de p?rdida es la suma de  error de clasificaci?n
#  por ser una estimaci?n de una variable cualitativa 
#  interaction.depth limita la profundidad (niveles de Particiones/ramificaciones) 
# de cada ?rbol.

boost=gbm(Exited~.,data=ds[train,],distribution="adaboost",n.trees=2000)

#  predicciones en el set de validaci?n
pred.boost.hx=predict(boost,newdata = ds[-train,],n.trees=2000)
pred.boost.prob=predict(boost,newdata = ds[-train,],n.trees=2000, type = "response")

head(pred.boost.hx)
head(pred.boost.prob)

pred.clase.hx <- ifelse(pred.boost.hx > 0, 1, 0)
pred.clase.prob <- ifelse(pred.boost.prob > 0.5, 1, 0)

head(pred.clase.hx)
head(pred.clase.prob)


#######################################
#  performance del modelo umbral=0.5
#######################################

set.seed(151)
train = sample(nrow(ds), 0.8*nrow(ds))

#  Vamos a probar con diferente cantida de ?rboles
cant.arb =seq(1, 5000, by=5 )

boost=gbm(Exited~.,data=ds[train,],distribution="adaboost",n.trees=5000, 
          interaction.depth=4)

matriz.pred = predict(boost,newdata = ds[-train,],n.trees = cant.arb, type="response")

#  lo convierto a clase {0,1}

matriz.pred.clase <- ifelse(matriz.pred>0.5,1,0)

acc.vector = double(ncol(matriz.pred))

for (i in 1:ncol(matriz.pred)){
  acc.vector[i] = sum(diag(table(matriz.pred.clase[,i],ds[-train,"Exited"])))/nrow(matriz.pred)
  }


matplot(cant.arb,1-acc.vector,  type="line", col="red")


matriz.pred.train = predict(boost,newdata = ds[train,],n.trees = cant.arb, type="response")

matriz.pred.train.clase <- ifelse(matriz.pred.train>0.5,1,0)
acc.vector.train = double(ncol(matriz.pred.train))

for (i in 1:ncol(matriz.pred.train)){
  acc.vector.train[i] = sum(diag(table(matriz.pred.train.clase[,i],ds[train,"Exited"])))/nrow(matriz.pred.train)

  sum(diag(table(matriz.pred.train.clase[,i],ds[train,"Exited"])))/nrow(matriz.pred.train)  
}


matplot(cant.arb,1-acc.vector.train,  type="l", pch=1, col="red")
lines(cant.arb,1-acc.vector,  pch=1, col="blue")

matplot(cant.arb[1:100],1-acc.vector.train[1:100],  type="l", pch=1, col="red")
lines(cant.arb[1:100],1-acc.vector[1:100],  pch=1, col="blue")

matplot(cant.arb[1:20],1-acc.vector.train[1:20],  type="l", pch=1, col="red")
lines(cant.arb[1:20],1-acc.vector[1:20],  pch=1, col="blue")



# CON MENOS VARIANZA Interaction.depth=2
boost=gbm(Exited~.,data=ds[train,],distribution="adaboost",n.trees=5000, 
          interaction.depth=2)

cant.arb =seq(1, 5000, by=5 )
matriz.pred = predict(boost,newdata = ds[-train,],n.trees = cant.arb, type="response")

#  lo convierto a clase {0,1}

matriz.pred.clase <- ifelse(matriz.pred>0.5,1,0)

acc.vector = double(ncol(matriz.pred))

for (i in 1:ncol(matriz.pred)){
  acc.vector[i] = sum(diag(table(matriz.pred.clase[,i],ds[-train,"Exited"])))/nrow(matriz.pred)
}


matplot(cant.arb,1-acc.vector,  pch=1, col="red")


# CON MENOS VARIANZA Interaction.depth=1
boost=gbm(Exited~.,data=ds[train,],distribution="adaboost",n.trees=5000, 
          interaction.depth=1)

cant.arb =seq(1, 5000, by=5 )
matriz.pred = predict(boost,newdata = ds[-train,],n.trees = cant.arb, type="response")

#  lo convierto a clase {0,1}

matriz.pred.clase <- ifelse(matriz.pred>0.5,1,0)

acc.vector = double(ncol(matriz.pred))

for (i in 1:ncol(matriz.pred)){
  acc.vector[i] = sum(diag(table(matriz.pred.clase[,i],ds[-train,"Exited"])))/nrow(matriz.pred)
}


matplot(cant.arb,1-acc.vector,  pch=1, col="red")





#  NO PUEDE SER !!! AHORA CON 100.000 ARBOLES

boost=gbm(Exited~.,data=ds[train,],distribution="adaboost",n.trees=40000,
          interaction.depth=2)

#  de a 10

cant.arb =seq(5, 40000, by=5 )

matriz.pred = predict(boost,newdata = ds[-train,],n.trees = cant.arb, type="response")
                      

matriz.pred.clase <- ifelse(matriz.pred>0.5,1,0)


err.vector = double(ncol(matriz.pred))

for (i in 1:ncol(matriz.pred)){
  err.vector[i] = 1-sum(diag(table(matriz.pred.clase[,i],ds[-train,"Exited"])))/nrow(matriz.pred)
}

min(acc.vector)
cant.arb[which(acc.vector==min(acc.vector))]

matplot(cant.arb,err.vector,  pch=10, col="red", xlab="Cant de ?rboles", ylab="Error")
