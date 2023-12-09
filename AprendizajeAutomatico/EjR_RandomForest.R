#
# Veamos como funciona el random forest con el dataset de credit 
# https://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29 
#
library(randomForest)
library(gmodels)
set.seed(1234)

input.folder <- "/Users/fcastanf/Desktop/stuff/Clases/UNIR/Videos/5_RF_data_examples/"
credit.data <- read.csv(paste0(input.folder, "credit.csv"))

str(credit.data)
credit.data$default <- credit.data$default-1

credit.rand  <- credit.data[order(runif(1000)),]
credit.train <- credit.rand[1:900, ]
credit.test  <- credit.rand[901:1000, ]

prop.table(table(credit.train$default))
prop.table(table(credit.test$default))

credit.train$default <- as.factor(credit.train$default)
rf.model <- randomForest(x = credit.train[, -21],
                         y = credit.train$default,
                         data = credit.train, 
                         ntree = 500,
                         do.trace= T)
# El OOB es un estimador razonable de error en test/validation
rf.model
plot(rf.model)

# Podemos ver la importancia de las variables
varImpPlot(rf.model)

# hacemos una prediccion en test
rf.pred <- predict(rf.model, credit.test)
head(rf.pred)
     
CrossTable(credit.test$default, rf.pred, prop.chisq = F, prop.c = F, prop.r = T, prop.t = F,  
           dnn = c("actual default", "predicted default"))

# Recordemos la distribucion orginal de las clases
prop.table(table(credit.train$default))

# Entrenamos con un conjunto balanceado
credit.train.pos <- credit.train[credit.train$default == 1,]
credit.train.neg <- credit.train[credit.train$default == 0,]

set.seed(1234)
credit.train.neg.bal <- credit.train.neg[sample(1:nrow(credit.train.neg), 
                                                nrow(credit.train.pos)), ]
credit.train.bal <- rbind(credit.train.pos, credit.train.neg.bal)

rf.model.2 <- randomForest(default ~., data = credit.train.bal, ntree = 1000,
                           do.trace = T)

plot(rf.model.2)
varImpPlot(rf.model.2)

# hacemos una nueva prediccion en test
rf.pred.2 <- predict(rf.model.2, credit.test)

CrossTable(credit.test$default, rf.pred.2, prop.chisq = F, prop.c = F, prop.r = T, prop.t = F,  
           dnn = c("actual default", "predicted default"))
#
# Hagamos un ensemble de randomForest y los combinamos
#
set.seed(1234)
credit.train.neg.bal <- credit.train.neg[sample(1:nrow(credit.train.neg), 
                                                nrow(credit.train.pos)), ]
credit.train.bal <- rbind(credit.train.pos, credit.train.neg.bal)
rf.model.3 <- randomForest(default ~., data = credit.train.bal, ntree = 1000,
                           do.trace = T)

credit.train.neg.bal <- credit.train.neg[sample(1:nrow(credit.train.neg), 
                                                nrow(credit.train.pos)), ]
credit.train.bal <- rbind(credit.train.pos, credit.train.neg.bal)
rf.model.4 <- randomForest(default ~., data = credit.train.bal, ntree = 1000,
                           do.trace = T)

credit.train.neg.bal <- credit.train.neg[sample(1:nrow(credit.train.neg), 
                                                nrow(credit.train.pos)), ]
credit.train.bal <- rbind(credit.train.pos, credit.train.neg.bal)
rf.model.5 <- randomForest(default ~., data = credit.train.bal, ntree = 1000,
                           do.trace = T)

plot(rf.model.3)
plot(rf.model.4)
plot(rf.model.5)

varImpPlot(rf.model.3)
varImpPlot(rf.model.4)
varImpPlot(rf.model.5)

rf.model.all <- combine(rf.model.3, rf.model.4, rf.model.5)

# hacemos una nueva prediccion en test
rf.pred.all <- predict(rf.model.all, credit.test)#, type = "prob")

CrossTable(credit.test$default, rf.pred.all, prop.chisq = F, prop.c = F, prop.r = T, prop.t = F,  
           dnn = c("actual default", "predicted default"))
