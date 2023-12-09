install.packages("neuralnet")
library(neuralnet)
set.seed(1234)

# hay otros paquetes:
# nnet
# RSNNS

###################################
# Modelando la fuerza del cemento #
###################################

# La fuerza del cemento varia debido al uso de los ingredientes
# que interactuan de forma compleja entre si.
# https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength
####################
# Data preparation #
####################

# Usaremos un dataset del UCI Machine Learning repository
concrete <- read.csv("/Users/fcastanf/Desktop/stuff/Clases/UNIR/Videos/3_NN_data_examples/concrete.csv")
str(concrete)

# Necesitamos escalar las variables de entrada.
# Si los datos siguen una distribución normal se puede
# utilizar la funcion scale() de R.
# Por el contrario si los datos siguen una distribución uniforme
# o muy poco normal es mejor llevar a cabo una normalizacion
# entre 0 y 1.

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))  
}

concrete.norm <- as.data.frame(lapply(concrete, normalize))

concrete.scale <- as.data.frame(scale(concrete))

hist(concrete$cement)
hist(concrete.norm$cement)
hist(concrete.scale$cement)

hist(concrete$slag)
hist(concrete.norm$slag)

hist(concrete$ash)
hist(concrete.norm$ash)

hist(concrete$water)
hist(concrete.norm$water)

hist(concrete$superplastic)
hist(concrete.norm$superplastic)

hist(concrete$coarseagg)
hist(concrete.norm$coarseagg)

hist(concrete$fineagg)
hist(concrete.norm$fineagg)

hist(concrete$age)
hist(concrete.norm$age)

hist(concrete$strength)
hist(concrete.norm$strength)

summary(concrete.norm$strength)
summary(concrete$strength)

# Generamos los conjuntos de training (75%) y validation (25%)
concrete.train <- concrete.norm[1:773,]
concrete.test <- concrete.norm[774:1030,]

#########################
# Vamos a entrenar una multilayer feedforward network.
#########################
set.seed(1234)
concrete.model <- neuralnet(strength ~ cement + slag + ash + water 
                            + superplastic + coarseagg + fineagg 
                            + age, 
                            data = concrete.train
                            #act.fct = 'tanh',
                            #, algorithm = "backprop", learningrate = 1e-3
                            )

plot(concrete.model)

model.results <- compute(concrete.model, concrete.test[,c(1:8)])
strength.predictions <- model.results$net.result

# Evaluamos el modelo
cor(strength.predictions, concrete.test$strength)

RMSE <- sqrt(mean((concrete.test$strength - strength.predictions)^2))
RMSE
MSE <- mean((concrete.test$strength - strength.predictions)^2)
MSE

#
# Mejorado la precision....
#
set.seed(1234)
concrete.model.2 <- neuralnet(strength ~ cement + slag + ash
                              + water + superplastic + coarseagg + fineagg + age,
                              data = concrete.train, hidden = c(2, 2, 2),
                              act.fct = 'tanh')
plot(concrete.model.2)

model.results.2 <- compute(concrete.model.2, concrete.test[,1:8])
strength.predictions.2 <- model.results.2$net.result

cor(strength.predictions.2, concrete.test$strength)
RMSE <- sqrt(mean((concrete.test$strength - strength.predictions.2)^2))
RMSE

# Probamos una regresion lineal
lm.model <- lm(strength ~ cement + slag + ash
               + water + superplastic + coarseagg + fineagg + age,
               data = concrete.train)

pred.lm <- predict(lm.model, concrete.test[, 1:8])
cor(pred.lm, concrete.test$strength)
RMSE <- sqrt(mean((concrete.test$strength - pred.lm)^2))
RMSE

# Visualizamos las tres predicciones
par(mfrow=c(1,3))
plot(concrete.test$strength,strength.predictions,col='red'
     ,main='Real vs predicho NN 1',pch=18,cex=0.7)
abline(0,1,lwd=1)

plot(concrete.test$strength,strength.predictions.2,col='red',main='Real vs predicho NN 2',
     pch=18,cex=0.7)
abline(0,1,lwd=2)

plot(concrete.test$strength,pred.lm,col='red',main='Real vs predicho lm',pch=18,cex=0.7)
abline(0,1,lwd=2)

#
model.results.2.tr <- compute(concrete.model.2, concrete.train[,1:8])
strength.predictions.2.tr <- model.results.2.tr$net.result

plot(concrete.train$strength,strength.predictions.2.tr,col='red',main='Real vs predicho NN 2',
     pch=18,cex=0.7)
abline(0,1,lwd=2)

# Otra metrica
RMSLE <- sqrt(mean(( log(concrete.test$strength+1) - 
                      log(strength.predictions.2+1))^2))
RMSLE

