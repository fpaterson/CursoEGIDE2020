library(h2o)

#cluster H20
local.h2o <- h2o.init(nthreads=-1)

ds <- read.csv("C:/Users/charly/Desktop/UNTREF/CURSO PARA DOCENTES J.J. PRIMOSICH/codigo en R y datasets/mnist_train.csv")


View(ds[1:20,])

########################################
#    Función para visualizar el dígito
########################################

#  tomo todos los datos de los pixels de la imagen de un digito (un registro)

img.mn <- function(data, row_index){
  
  #la fila como un vector
  r <- as.numeric(data[row_index, 2:785])
  
  #matriz vacia de 28*28
  im <- matrix(nrow = 28, ncol = 28)
  
  #relelcno la matriz con los datos correctos
  j <- 1
  for(i in 28:1){
    
    im[,i] <- r[j:(j+27)]
    
    j <- j+28
    
  }  
  
  #veo la imagen completa
  image(x = 1:28, 
        y = 1:28, 
        z = im, 
        col=gray((0:255)/255), 
        main = paste("Number:", data[row_index, 1]))
}



#  la pruebo con el registro 2383 (o cualquier otra)
img.mn(ds,2383)



################################################
# Entrenamiento del modelo
################################################

str(summary(ds))
ds[1:20,1]
names(ds)

set.seed(2)
muestra <- sample(nrow(ds),0.8*nrow(ds))

train <- ds[muestra,]
test <- ds[-muestra,]

ss=rbind(train, test)

# CONVIERTO EL CAMPO DEL DIGITO  A FACTOR
train[,1] <- as.factor(train[,1])
test[,1] <- as.factor(test[,1])


# paso los dataframes al cluster H20 en mi máquina
trData<-as.h2o(train)
tsData<-as.h2o(test)

#  entreno el modelo de DL
modelo1 <- h2o.deeplearning(x = 2:785, 
                            y = 1, 
                            training_frame =trData, 
                            validation_frame =tsData,
                            distribution = "multinomial",
                            activation = "RectifierWithDropout",
                            hidden = c(20,20),
                            hidden_dropout_ratios = c(0.1, 0.1,0),
                            epochs = 10,
                            export_weights_and_biases=T)


#  primer layer oculto, neuronas 1 y 2 , pesos p1-n1 a p10.n1 y p2.n1 a p2.n10
h2o.weights(modelo1, matrix_id=1)[1:2,1:10]

plot(modelo1)

# le agrego L1 DE 0.001 (IMPORTANTE!)
modelo2 <- h2o.deeplearning(x = 2:785, 
                            y = 1, 
                            training_frame =trData, 
                            validation_frame =tsData,
                            distribution = "multinomial",
                            activation = "RectifierWithDropout",
                            hidden = c(20,20,20),
                            hidden_dropout_ratios = c(0.1, 0.1,0),
                            l1 = 1e-3,
                            epochs = 10,
                            export_weights_and_biases=T)

par(mfrow=c(2,2))
plot(modelo1)
plot(modelo2)


##############################
# Análisis de los resultados
##############################

# parámetros de los modelos
modelo2@parameters

# performance completa del modelo
modelo2

# métricas en el training set
h2o.performance(modelo2, train = TRUE)

# métricas en el validation set
h2o.performance(modelo2, valid = TRUE)

#  sólo el  CME (MSE)
h2o.mse(modelo2, valid = TRUE)


########################################
#  N FOLD CROSS VALIDATION
########################################

# modelo 3
modelo_cv <- h2o.deeplearning(x = 2:785, 
                              y = 1, 
                              training_frame =trData, 
                              # validation_frame =tsData,
                              distribution = "multinomial",
                              activation = "RectifierWithDropout",
                              hidden = c(20,20,20),
                              input_dropout_ratio = 0.2,
                              l1 = 1e-5,
                              epochs = 10,
                              export_weights_and_biases=T,
                              nfolds = 5
)


plot(modelo_cv)

#######################
# PREDICCIONES  #######
#######################

pred <- h2o.predict(modelo2, newdata = tsData)

#  variables importantes
modelo2_vi <- h2o.deeplearning(x = 2:785, 
                              y = 1, 
                              training_frame =trData, 
                              distribution = "multinomial",
                              activation = "RectifierWithDropout",
                              hidden = c(20,20,20),
                              hidden_dropout_ratios = c(0.1, 0.1,0),
                              l1 = 1e-3,
                              epochs = 10,
                              variable_importances = TRUE
)

#  lo vemos
h2o.varimp(modelo2_vi)


###########################################
#  BUSQEUDA COMBINADA (GRID SEARCH)  #######
###########################################

#  permite seleccionar un rangos de valores para diferentes parámetros y así comparar la performance 
# de cada combinación

#  cantidad de neuronas en cada capa oculta
hidden_opt <- list(c(40,40), c(10,30,10), c(50,50,50))

#  valor del lambda de regularización L1
l1_opt <- c(1e-5,1e-7)

#  corro la busqueda del mejor modelo

modelo_grid <- h2o.grid("deeplearning",
                        hyper_params = list(hidden = hidden_opt, l1 = l1_opt),
                        x = 2:785, 
                        y = 1, 
                        training_frame =trData, 
                        validation_frame =tsData,
                        distribution = "multinomial",
)
# el resultado
modelo_grid


# veamos los MSE  de cada modelo

str(modelo_grid)
mod.ds <- data.frame(Modelo=character(0), MSE=numeric(0))
names(mod.ds)


for (i in modelo_grid@model_ids){
  mod_i = h2o.getModel(i)
  mod.ds<- rbind(mod.ds, data.frame(Modelo=i, MSE=h2o.mse(mod_i, valid = TRUE)))
}

