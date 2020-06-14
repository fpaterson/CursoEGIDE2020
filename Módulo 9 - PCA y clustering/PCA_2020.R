##########################################
# PCA
##########################################

# importo el dataset
ds = read.csv("C:/Users/charly/Desktop/UBA/UBA curso data Science/+++ curso completo POR CLASES - 1 CUATR 2019/CLASE 10 - PCA y Clustering/Wine.csv")
str(ds)

ds= read.csv("C:/Users/charly/Desktop/UCEMA/Ciencia de Datos en Negocios/Clases 1 Cuatr 2020/Clase 3/Churn_Modelling.csv")
ds = ds[-c(1:3)]
# factorizo
ds$Exited = factor(ds$Exited, levels = c(0, 1))

#  numerico 2 y 3
ds$Geography = as.integer(ds$Geography)
ds$Gender = as.integer(ds$Gender)

str(ds)

# train y test

set.seed(123)
train =sample(nrow(ds),0.8*nrow(ds))
training_set <- ds[train,]
test_set <- ds[-train,]

# estandarizo las varaibles
training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

summary(training_set)


#  PCA
library(e1071)



#  pasa de tener 13 variables a tener 3 que  representen la mayor cantidad de la variable total
pca = prcomp(x = training_set[-11], rank=3)

#  veo los coeficientes de cada PC
pca


# ahora el train expresado en PC
train_set_pca = predict(pca, training_set)

# le agrego la variable respuesta "Exited" al nuevo train set
train_set_pca = cbind(train_set_pca,Exited=training_set$Exited)
colnames(train_set_pca)

# el test expresado en PC y le agrego la variable respuesta "segmento" al nuevo test set
test_set_pca = cbind(predict(pca, test_set),Exited=test_set$Exited)
colnames(test_set_pca)

# AHORA VOY A APLICAR un algoritmo de clasificación con los datasets con PC
#  -----> regresión LOGISTICA


# glm(formula = Exited ~ .,family = binomial,data = training_set)
mod_glm = glm(formula = factor(Exited) ~ .,data = data.frame(train_set_pca),family = binomial)

# las predicciones 
pred = predict(mod_glm, newdata = data.frame(test_set_pca[,-ncol(test_set_pca)]),type = 'response')

View(pred)

y_pred = ifelse(pred > 0.5, 1, 0)
head(y_pred)


# matriz de confusion
cm = table(pred=y_pred,real=test_set_pca[,ncol(test_set_pca)])
cm

 
