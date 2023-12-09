install.packages("e1071")
library(e1071)

#############################
# Support Vector Classifier #
#############################

# Es posible especificar un argumento de cost.
# Cuando el argumento es pequeño los margin son amplios y muchos support 
# vectors estaran en el margen.
# Cuando el argumento es grande el margin es pequeño y habra pocos vectores 
# de soporte en el margen.

# Usaremos la funncion svm() para ajustar el clasificador a un valor de 
# coste C especifico.

set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] <- x[y==1,] + 1

# Comprobemos si las clases son separables linealmente
plot(x, col = (3-y))

# Para usar svm como clasificacion y no como regresion debemos
# codificar la variable respuesta como factor.
train.data <- data.frame(x = x, y = as.factor(y))

svm.fit <- svm(y ~ x.1 + x.2, data = train.data, kernel = "linear", 
               cost = 10, scale = F)

plot(svm.fit, train.data)
# Los vectores de soporte se pintan con x y los demas puntos con 
# circulos

# Para ver cuales son
svm.fit$index

# podemos obtener informacion acerca de ellos con summary
summary(svm.fit)

# Vamos a utilizar un valor de cost C mas pequeño
svm.fit <- svm(y ~ ., data = train.data, kernel = "linear", 
               cost = .1, scale = F)
plot(svm.fit, train.data)
svm.fit$index

# Con un valor mas pequeño obtenemos un mayor numero de vectores 
# de soporte porque el margen es mas ancho

# El paquete e1071 tiene una funcion tune() que permite realizar cross-validation.
# y probar diferentes valores del parametro C
set.seed(1)
tune.out <- tune(svm, y ~ ., data = train.data, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)),
                 #ranges = list(cost = c(seq(0.1, 1, by = 0.1))),
                 #tunecontrol = tune.control(sampling = "fix", fix = 2/3)
                 tunecontrol = tune.control(sampling = "cross", cross = 5))

# podemos acceder a la infor de estos modelos
summary(tune.out)

# Podemos obtener el mejor modelo de la siguiente forma
best.model <- tune.out$best.model
summary(best.model)

# Usemos la funcion predict() para predecir en un conjunto de test
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1,] <- xtest[ytest == 1, ] + 1
plot(x, col = (3-y), main = "Datos de Entrenamiento")
plot(xtest, col = (3-ytest), main = "Datos de Test")

test.data <- data.frame(x = xtest, y = as.factor(ytest))

y.pred <- predict(best.model, test.data)
table(predict = y.pred, truth = test.data$y)

# Ahora consideremos un caso en que las dos clases sean linealmente separables
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y+5)/2, pch = 19)

# vamos a ajustar vector classifier con un valor grande de cost
data.train.2 <- data.frame(x = x, y = as.factor(y))
svm.fit.2 <- svm(y ~ ., data = data.train.2, kernel = "linear", cost = 1e5)
summary(svm.fit.2)
plot(svm.fit.2, data.train.2)

# No ha habido errores en training y solo se han necesitado 3 vectores de soporte
# Los margenes son muy estrechos porque las observaciones que no son vectores de
# soporte (marcados como circulos) estan muy cerca de la frontera de decision
# Parece poco probable que este modelo funcione bien con validation data

# Probemos un valor menor de C
svm.fit.3 <- svm(y ~ ., data = data.train.2, kernel = "linear", cost = 1)
summary(svm.fit.3)
plot(svm.fit.3, data.train.2)

# Ahora cometemos error en una clasificacion pero obtenemos un margen mayor
# y se utilizan 7 vectores de soporte. Parece que este modelo se puede comportar
# mejor en datos de validation que el anterior.

###########################
# Support Vector Machines #
###########################

# vamos a utilizar un kernel radial y utilizaremos el valor de gamma
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
data.train.3 <- data.frame(x = x, y = as.factor(y))

# pintando los datos se ve claramente que no hay una frontera lineal
plot(x, col = y, pch = 19)

sample.rows <- sample(200, 100)
svm.fit.4 <- svm(y ~ ., data = data.train.3[sample.rows, ], 
                 kernel = "radial", gamma = 1, 
                 cost = 1)
plot(svm.fit.4, data.train.3[sample.rows,])
summary(svm.fit.4)

# podemos ver que hay un numero alto de errores de entrenamiento.
# Si incrementamos el valor de cost podemos reducir el numero
# de errores de entrenamiento. Sin embargo la frontera de decision se puede
# sobre ajustar a los datos.
svm.fit.5 <- svm(y ~ ., data = data.train.3[sample.rows, ], kernel = "radial", gamma = 1, 
                 cost = 1e5)
plot(svm.fit.5, data.train.3[sample.rows,])

# hagamos cross validation con tune
tune.out.2 <- tune(svm, y ~ ., data = data.train.3[sample.rows, ], 
                   kernel = "radial", 
                   range = list(cost = c(0.1, 1, 10, 100, 1000), 
                   gamma = c(0.5, 1, 2, 3)))

summary(tune.out.2)

table(true = data.train.3[-sample.rows, "y"], 
      pred = predict(tune.out.2$best.model, newx = data.train.3[-sample.rows,]))

##############
# Curvas ROC #
##############
#install.packages("ROCR")
library(ROCR)

rocplot <- function(pred, truth, ...) {
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}

# Por defecto SVMs genera como salida las etiquetas de las clase de cada
# observacion. Si se desea obtener lo valores utilizados para obtener las
# etiquetas de las clases hay que utilizar decision.values = TRUE
svm.fit.opt <- svm(y ~ ., data = data.train.3[sample.rows, ], 
                   kernel = "radial", gamma = 2,
                   cost = 1, decision.values = T)

prediccion <- predict(svm.fit.opt, data.train.3[sample.rows, ], decision.values = T)

fitted <- attributes(prediccion)$decision.values

# pintando la curva ROC
par(mfrow = c(1, 2))
rocplot(fitted, data.train.3[sample.rows, "y"], main = "Training data")

# Incrementando el valor de gamma conseguimos un ajuste mas felxible y
# generamos mejoras en la precision.
svm.fit.flex <- svm(y ~ ., data = data.train.3[sample.rows, ], kernel = "radial",
                    gamma = 50, cost = 1, decision.values = T)

fitted <- attributes(predict (svm.fit.flex, data.train.3[sample.rows, ], 
                              decision.values = T))$decision.values

rocplot(fitted, data.train.3[sample.rows, "y"], add = T, col = "red")

#
# Vamos a pintar las curvas ROC en los datos de validacion
#
fitted <- attributes(predict(svm.fit.opt, data.train.3[-sample.rows, ], 
                             decision.values = T))$decision.values

rocplot(fitted, data.train.3[-sample.rows, "y"], main = "Test Data")

fitted <- attributes(predict(svm.fit.flex, data.train.3[-sample.rows, ], 
                             decision.values = T))$decision.values

rocplot(fitted, data.train.3[-sample.rows, "y"], add = T, col = "red")

############################
# SVM con multiples clases #
############################
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
data.train <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x, col = (y+1))

svm.fit.6 <- svm(y ~ ., data = data.train, kernel = "radial", cost = 10, gamma = 1, probability = TRUE)
plot(svm.fit.6, data.train)

svm.pred <- predict(svm.fit.6, data.train, probability = TRUE, decision.values = TRUE)
# svm.pred.probs <- attributes(svm.pred)$decision.values

# La libreria e1071 tambien puede llevar a cabo support vector regression 
# si el vector de respuesta es numerico en lugar de factor.
