library("neuralnet")
library("MASS")

#cargo el dataset

boston <- MASS::Boston
str(boston)

#veo los primeros diez regfistros del dataset
head(boston)

#analisis descriptivo de los datos
summary(boston)

# estandarizo
boston_n <-  apply(boston, 2, scale)

#preparo una muestra del 80 % de los registros
muestra <- sample(nrow(boston),0.8*nrow(boston))

#preparo el train set y el validation/test set
train <- boston_n[muestra,]
test <- boston_n[-muestra,]

summary(train)
summary(test)

##############################################################
#ENTRENO EL MODELO de NN
##############################################################

formula_ann <- paste("medv ~ ",paste(names(boston[-length(boston)]),collapse = " + "))

modelo <- neuralnet(formula_ann, data=train, hidden=2, rep=2, threshold=0.01, linear.output = TRUE)

#  mas simple, solo tres var predictoras
modelo_1 <- neuralnet(medv ~ lstat+dis+age, data=train, hidden=2, rep=2, threshold=0.01, linear.output = TRUE)

#  mas simple, solo dos var predictoras
modelo_2 <- neuralnet(medv ~ lstat+dis, data=train, hidden=2, rep=1, threshold=0.01, linear.output = TRUE)

plot(modelo)
plot(modelo_1)

modelo_1$net.result
modelo_1$weights
modelo_1$call

#  corro una prediccion con nuevo dato : "lstat"=12 ; "dis"=4
#  lo estandarizo antes
nuevo_dato_n = data.frame("lstat"=(12-mean(boston[,"lstat"]))/sd(boston[,"lstat"]),
                          "dis"=(4-mean(boston[,"dis"]))/sd(boston[,"dis"]))

resultado_n = compute(modelo_2, nuevo_dato_n)
resultado_n$net.result

# desestandarizo
resultado_n$net.result * sd(boston[,"medv"]) + mean(boston[,"medv"])

#en el test con el modelo completo :
#   "medv ~  crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat"
testeo <- compute(modelo, test[,-ncol(test)])

#resultados en el test
res_test <- testeo$net.result

str(res_test)
#des_estandarizo
res_des = res_test*sd(boston[,"medv"])+mean(boston[,"medv"])

# perfromance
ECM <- sum((res_des - boston[-muestra,"medv"])^2/nrow(test))

