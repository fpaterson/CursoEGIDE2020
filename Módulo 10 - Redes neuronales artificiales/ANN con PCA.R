##########################################
# PCA
##########################################

# importo el dataset
ds = read.csv("C:/Users/charly/Desktop/UBA/UBA curso data Science/CLASE 12 - ANN/ENVIO ALUMNOS/OnlineNewsPopularity.csv")
str(ds)

# estandariizo
ds_n <-  data.frame(apply(ds[-1], 2, scale))
str(ds_n)

#  PCA
library(e1071)
library(factoextra)

#  lo dejo para quye haga las p componentes ppales
pca = prcomp(x = ds_n)


#  veo los coeficientes de cada PC
summary(pca)

screeplot(pca, type="barplot", npcs=30)
fviz_eig(pca,ncp=30)


#  me quedo en 4
pca9 = prcomp(x = ds_n, rank=4)


# ahora el train expresado en PC
set.seed(123)
train =sample(nrow(ds),0.8*nrow(ds))
training_set <- ds_n[train,]
test_set <- ds_n[-train,]

train_set_pca = predict(pca9, training_set)

# le agrego la variable respuesta "shares" al nuevo train set
train_set_pca = data.frame(cbind(train_set_pca,shares=training_set$shares))

head(train_set_pca)

# el test expresado en PC y le agrego la variable respuesta "shares" al nuevo test set
test_set_pca = data.frame(cbind(predict(pca9, test_set),shares=test_set$shares))
head(test_set_pca)


###############################################
#ENTRENO EL MODELO de NN
###############################################
library(neuralnet)

formula_ann <- paste("shares ~ ",paste(names(train_set_pca[-10]),collapse = " + "))

modelo <- neuralnet(formula_ann, data=train_set_pca[1:10000,], hidden=2, 
                   threshold=0.8,linear.output = TRUE)
# algorithm="rprop+",

plot(modelo)

modelo$net.result
modelo$weights
modelo$call
modelo$result.matrix


#en el test
testeo <- compute(modelo, test_set_pca[,-9])

#resultados en el test
res_test <- testeo$net.result


# str(res_test)
#des_estandarizo
 res_des = res_test*sd(ds[,"shares"])+mean(ds[,"shares"])

# perfromance
ECM <- sum((res_des - ds_n[-train,"shares"])^2/nrow(test_set_pca))
ECM
