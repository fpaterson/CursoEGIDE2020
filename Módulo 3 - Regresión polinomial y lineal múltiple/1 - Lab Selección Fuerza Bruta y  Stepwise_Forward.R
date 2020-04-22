Boston <- MASS::Boston
attach(MASS::Boston)

plot(lstat,medv, col="grey")
names(Boston)

############################################
#de a dos fuerza bruta
###########################################
Boston <- MASS::Boston
campos <- names(Boston)[-ncol(Boston)]
campo_max <- campos[1]
max.r2 <- 0
primer_pasada <- TRUE


for (i in 1:length(campos)){
  formula_ant <- "medv ~"
  for (j in 1:length(campos)){
    if (j != i){
        formula <- paste(formula_ant,campos[i],"+",campos[j]) 
        print(formula)
        lm.fit <- lm(formula, data=Boston)
        r2 <- summary(lm.fit)$adj.r.squared
        
        if (r2 > max.r2) {
          max.r2 <- r2
          formula_max <- formula
          campo_max <- c
        }
    }
  }
}
    
#resultados    
max.r2
formula_max


############################################
#de a tresfuerza bruta
###########################################













############################################
#de a cuatro fuerza bruta
###########################################


















############################################
#de a cinco fuerza bruta
###########################################





















######################################## 
#### ORIGINAL FORWARD STEPWISE
######################################## 


campos <- names(Boston)[-ncol(Boston)]
campo_max <- campos[1]
max.r2 <- 0
formula_ant <- "medv ~"
primer_pasada <- TRUE

modelos = data.frame(modelo=as.character(), r2aj=as.numeric(), stringsAsFactors = FALSE)

for (f in campos){
  for (c in campos){
      if (primer_pasada) {
          formula <- paste(formula_ant,c)
          }
          else{
              formula <- paste(formula_ant,"+",c)    
              }
      lm.fit <- lm(formula, data=Boston)
      
      r2 <- summary(lm.fit)$adj.r.squared
      #print(paste(formula,r2))
      modelos = rbind(modelos, data.frame("modelo"=formula, "r2aj"=r2))
      
    
      if (r2 > max.r2) {
            max.r2 <- r2
            formula_max <- formula
      }
      
 }
  #campos <- campos[!(campos==campo_max)]
  campos <- campos[campos!=campo_max]
  formula_ant <- formula_max
  primer_pasada <- FALSE
  
}
modelos


max.r2
formula_max





