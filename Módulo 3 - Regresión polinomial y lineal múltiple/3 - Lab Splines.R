install.packages("ISLR")
library(ISLR)
attach(Wage)
View(Wage)


#############
#  SPLINES
############

library(splines)
fit <- lm(wage ~ bs(age, knots = c(25,40,60)), data=Wage)

# corro sobre los datos de entrenamiento
pred <- predict(fit,list(age=sort(age)))


# gr{afico con los datos de edad wage
plot(age, wage, col="blue")

# dibuja los splines!!!!!
  lines(sort((age)),pred,col="red", lwd=3)

#dibijo los limites de los splines
abline(v=c(25,40,60), col="green")

summary(fit)

