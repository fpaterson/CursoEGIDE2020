#############################################################################################################
# K-Nearest Neighbors
#############################################################################################################

library(class)
library(ISLR)

# ATENCION: PARA CADA UBICACION DEL dataset ORIGINAL SERA DISTINTO EL PATH
# ds = read.csv(--path al dataset con "/" como separador de directorios y entre comillas--- /Churn_Modelling.csv')

ds = read.csv('C:/Users/charly/Desktop/UBA/UBA curso data Science/CLASE 5/Churn_Modelling.csv')

View(ds)

# saco los campos innecesarios: RowNumber, CustomerId, Surname
ds <- ds[-c(1:3)]

ds$Exited <- as.factor(ds$Exited)

str(ds)

set.seed(123)
train <- sample(10000, 8000)

# covnierto a numeric

# ds$CreditScore <- as.numeric(ds$CreditScore)
# ds$Age <- as.numeric(ds$Age)
# ds$Tenure <- as.numeric(ds$Tenure)
# ds$NumOfProducts <- as.numeric(ds$NumOfProducts )                          
# ds$HasCrCard  <- as.numeric(ds$HasCrCard)                           
# ds$IsActiveMember <- as.numeric(ds$IsActiveMember )                           


ds$Geography <- as.numeric(ds$Geography)
ds$Gender <- as.numeric(ds$Gender)
# ds$Age <- as.numeric(ds$Age)


str(ds)




knn.pred=knn(ds[train,-11],ds[-train,-11],ds$Exited[train],k=3)

str(test)

knn.pred.puntual=knn(ds[train,-11],c(635,2,2,39,6,88856,2,0,1,74941),ds$Exited[train],k=3)
knn.pred.puntual


#############################################################################################################
# LR  Churn Modelling 
#############################################################################################################

# Impo
ds_reg = read.csv('C:/Users/charly/Desktop/UBA/UBA curso data Science/CLASE 5/Churn_Modelling.csv')
ds_reg = ds_reg[-c(1:3)]

# factorizo
ds_reg$Exited = factor(ds_reg$Exited, levels = c(0, 1))


# Train y test
str(ds_reg)
set.seed(5656)
train <- sample(10000, 8000)

training_set = ds_reg[train,]
test_set = ds_reg[-train,]

# modelo
classifier = glm(formula = Exited ~ .,family = binomial,data = training_set)
coef(classifier)

# Prediccion en el test
prob_pred = predict(classifier, type = 'response', newdata = test_set[-11])

class(prob_pred)
head(prob_pred)

y_pred = ifelse(prob_pred > 0.5, 1, 0)
head(y_pred)
y_pred[1:100]



#############################################################################################################
# Cross Validation
#############################################################################################################

# CV
library(ISLR)
library(boot)
?cv.glm

attach(Auto)
plot(mpg,horsepower)

# 10 CV
modelo <- glm(mpg ~ poly(horsepower, 4),data=Auto)
cv <- cv.glm(Auto, modelo,K=10)

?cv.glm
str(cv)
error <- cv$delta[1]


pruebas=5
cv.error <- c()
for (i in 1:pruebas){
  glm.fit <- glm(mpg ~ poly(horsepower, i),data=Auto,)
  cv.error <- c(cv.error,cv.glm(Auto, glm.fit,K=10)$delta[1])
}

cv.error

#  ploteo la evolución
plot(1:pruebas,cv.error,xlab="grado", ylab="error", type = "b", col="blue" )



# ahora pruebo con polinomios de hasta 10 grados
pruebas=10
cv.error <- c()
for (i in 1:pruebas){
  glm.fit <- glm(mpg ~ poly(horsepower, i),data=Auto,)
  cv.error <- c(cv.error,cv.glm(Auto, glm.fit,K=10)$delta[1])
}

cv.error

#  ploteo la evolución
plot(1:pruebas,cv.error,xlab="grado", ylab="error", type = "b", col="blue" )



# ahora pruebo con polinomios de hasta 12
pruebas=12
cv.error <- c()
for (i in 1:pruebas){
  glm.fit <- glm(mpg ~ poly(horsepower, i),data=Auto,)
  cv.error <- c(cv.error,cv.glm(Auto, glm.fit,K=10)$delta[1])
}

cv.error

#  ploteo la evolución
plot(1:pruebas,cv.error,xlab="grado", ylab="error", type = "b", col="blue" )
