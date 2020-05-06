#  NAIVE BAYES

install.packages("e1071")
library(e1071)

ds <- read.csv2("C:/Users/Usuario/Desktop/UBA - Clases/+++ curso completo POR CLASES -1 CUATR 2019/CLASE 5 - Naive Bayes - Matriz de Confusión/adult.csv")
str(ds)

ds <- na.omit(ds)

train <- sample(nrow(ds), 0.8*nrow(ds))

ds_train <- ds[train,]
ds_test <- ds[-train,]

mod_bayes <- naiveBayes(ing ~ ., data=ds_train)

str(mod_bayes)



#Para evaluar el clasificador NB, debemos probar sus predicciones en los datos de prueba

test_pred <- predict(mod_bayes, ds_test[-ncol(ds_test)], type="raw")


head(test_pred)



umbral <- .9


pred <- c()
pred[test_pred[,2]>=umbral] <- " >50K"
pred[test_pred[,2]<umbral] <- " <=50K"


mc <- table(pred, ds_test$ing, dnn=c("Pred", "Real"))
mc

exactitud <- sum(diag(mc))/sum(mc)

recall.sensitividad <- mc[2,2]/sum(mc[,2])

precision <- mc[2,2]/sum(mc[2,])


F1 <- 2*precision*recall.sensitividad/(precision + recall.sensitividad)

umbral
exactitud
precision
recall.sensitividad
F1


#Veamos la función la función CrossTable ()
#en el paquete gmodels. 


install.packages("gmodels")
library(gmodels)

CrossTable(pred, ds_test$ing,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'real'))




