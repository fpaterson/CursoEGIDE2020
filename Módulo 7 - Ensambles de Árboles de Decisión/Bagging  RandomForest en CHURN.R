# Bagging and Random Forests
install.packages("randomForest")
library(randomForest)


ds = read.csv("C:/Users/charly/Desktop/UBA/UBA curso data Science/+++ curso completo POR CLASES - 1 CUATR 2019/CLASE 6 - Árboles de Decisión - Matriz Benef/Churn_Modelling.csv")


# quito campos que no uso  factorizo la variabnle objetivo
ds = ds[-c(1:3)]
ds$Exited = factor(ds$Exited, levels = c(0, 1))
str(ds)


set.seed(1)
train = sample(nrow(ds), 0.8*nrow(ds))




# Ahora probemos con Random Forest. El único argumento que modificaremos es la cant de predictores
# a utilizar para entrenar cada árbol:  mtry. Por defecto, randomForest ()
# usa  p/3 variables al construir un RF de árboles de regresión, y
# raizcuadrada(p) al construir un RF de árboles de clasificación. Nosotros usaremos
# mtry = 6.


set.seed(1)
rf=randomForest(Exited~., data=ds , subset=train , mtry=3 , importance=TRUE , ntree=1000)

importance(rf)
help(importance)

# Se utilizan dos medidas de performance por variable predictora. La primera , %IncMSE
# es el promedio de la disminución de la exactitud (accuracy) en las predicciones 
# cuando la  variable en cuestión se excluye del modelo. La segunda mide 
# la disminución total en la impureza del nodo que resulta de las particiones realizadas 
# sobre esa variable, promediada en todos los árboles.
# En el caso de árboles de regresión, la impureza del nodo se mide por el SCR, y 
# para árboles de clasificación por la desviación. 

importance(rf)[order(importance(rf)[,4], decreasing = TRUE),]                                             

help(randomForest)


# probemos con diferebntes tamaño de árboles
cant.arb =seq(100, 1000, by=100 )
acc.vec <- double(10)


for (i in 1:length(cant.arb)) {
  rf=randomForest(Exited~., data=ds , subset=train , mtry=3 , importance=TRUE,  ntree=cant.arb[i])
  pred.rf.prob = predict(rf,newdata=ds[-train,], type="prob", ntree=cant.arb[i])
  umbral=0.5
  pred.umbral <- ifelse(pred.rf.prob[,2] > umbral, 1, 0)
  acc.vec[i] = sum(diag(table(pred.umbral,ds[-train,"Exited"])))/nrow(ds[-train,])
}

plot(cant.arb,acc.vec)
lines(cant.arb,acc.vec)

#  ahora evaluando los  errores de clasificación al variar la cantidad de variables prdictoras
cant.mtry =seq(1, 10)

error.vec <- double(length(cant.mtry))

for (i in cant.mtry) {
  rf=randomForest(Exited~.,data=ds,subset=train,mtry=i,importance=TRUE, ntree=300)
  pred.rf.prob = predict(rf,newdata=ds[-train,], type="prob", ntree=300)
  umbral=0.5
  pred.umbral <- ifelse(pred.rf.prob[,2] > umbral, 1, 0)
  error.vec[i] = 1-sum(diag(table(pred.umbral,ds[-train,"Exited"])))/nrow(ds[-train,])
}


matplot(cant.mtry,error.vec, pch=19, col=c("red","blue"), type = "b",
        xlab = "Cant Var Predictoras", ylab="Error")

mtry_min=which.min(error.vec)

rf_optimo=randomForest(Exited~.,data=ds,subset=train,mtry=mtry_min,importance=TRUE, ntree=300)

# Se utilizan dos medidas de performance por variable predictora. La primera , %IncMSE
# es el promedio de la disminución de la exactitud (accuracy) en las predicciones 
# cuando la  variable en cuestión se excluye del modelo. La segunda mide 
# la disminución total en la impureza del nodo que resulta de las particiones realizadas 
# sobre esa variable, promediada en todos los árboles.
# En el caso de árboles de regresión, la impureza del nodo se mide por el SCR, y 
# para árboles de clasificación por la desviación. 

# ordenada por MeanDecreaseAccuracy
importance(rf)[rev(order(importance(rf_optimo)[,3])),]

# ordenada por MeanDecreaseGini
importance(rf)[rev(order(importance(rf_optimo)[,4])),]

# Lo grafico
varImpPlot(rf_optimo)



  #######################################
#  performance del modelo umbral=0.5

umbral=0.5
pred.umbral <- ifelse(pred.rf.prob[,2] > umbral, 1, 0)


mc = table(pred.umbral,ds[-train,"Exited"])
mc

VP=mc[2,2]; 
VN=mc[1,1]; 
FP=mc[2,1]; 
FN=mc[1,2]; 

p_VP=VP/(VP+FN)
p_VN=VN/(VN+FP)
p_FP=FP/(VN+FP)
p_FN=FN/(VP+FN)

acc = (VP+VN)/(VP+VN+FP+FN)

p_FN
acc

#######################################
#  Costo - beneficio 

# MATRIZ DE BENEFICIOS
b_VP = 950
b_VN = 0
c_FP = -50
c_FN = -500

#  prob a priori
clase = table(ds[-train,"Exited"])
p_POS= clase[2]/nrow(ds[-train])
p_NEG= clase[1]/nrow(ds[-train])

#  funciónal de beneficio esperado
# B.esp = p_POS*(p_VP*b_VP + p_FN*c_FN)+p_NEG*(p_VN*b_Vn + p_FP*c_FP)

B.esp = p_POS*(p_VP*b_VP + p_FN*c_FN)+p_NEG*(p_VN*b_VN + p_FP*c_FP)
B.esp



#######################################
#  performance del modelo umbral=0.2

umbral=0.2
pred.umbral <- ifelse(pred.rf.prob[,2] > umbral, 1, 0)


mc = table(pred.umbral,ds[-train,"Exited"])
mc

VP=mc[2,2]; 
VN=mc[1,1]; 
FP=mc[2,1]; 
FN=mc[1,2]; 

p_VP=VP/(VP+FN)
p_VN=VN/(VN+FP)
p_FP=FP/(VN+FP)
p_FN=FN/(VP+FN)

acc = (VP+VN)/(VP+VN+FP+FN)

