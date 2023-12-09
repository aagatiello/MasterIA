# 2. En este ejemplo, vamos a utilizar el dataset llamado Carseats
# que contiene informacion sobre la venta de sillas de coches
# en 400 tiendas diferentes.
library(tree)
set.seed(1)

load("/Users/fcastanf/Desktop/stuff/Clases/UNIR/Videos/4_DT_data_examples//Carseats.RData")

summary(Carseats)

# Discretizamos la variable Sales
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

# Vamos a construir un arbol de clasificacion para predecir High
# utilizando todas las variables, excepto Sales
tree.1 <- tree(High ~ .-Sales, Carseats)
summary(tree.1)

# El training error es del 9% (Misclassifcation error rate)
plot(tree.1)
text(tree.1, pretty = 0)

# El indicador mas importante para las ventas parace ser la localizacion del estante.

# Si ejecutamos el nombre del arbol, R nos muestra cada una de las ramas del arbol
# junto con el criterio de split, el numero de observaciones en cada rama, la desviacion,
# la prediccion completa para la rama (yes o no) y la fraccion de observaciones que en esa
# rama toman valores si o no.
tree.1

# Vamos a evaluar el arbol con conjuntos de train/test

train <- sample(1:nrow(Carseats), 200)
test <- Carseats[-train, ]
High.test <- High[-train]

tree.2 <- tree(High ~ .-Sales, Carseats, subset = train)
plot(tree.2)
text(tree.2, pretty = 0)

tree.pred <- predict(tree.2, test, type = "class")
table(tree.pred, High.test)
prop.table(table(tree.pred, High.test), margin = 2)

# Precision
precision <- (98 + 56) / 200 
precision

# Veamos si podando el arbol se puede mejorar. 
# la funcion cv.tree() realiza cross-validation para determinar el nivel optimo
# de la complejidad del arbol
# con FUN=prune.misclass indicamos que el criterio a optimizar es el error
# en clasificacion. 
# Por defecto K == 10

cv.carseats <- cv.tree(tree.2, FUN = prune.misclass)
names(cv.carseats)
cv.carseats

# El atributo dev indica el cross-validation error. Vamos a pintar el 
# error rate como una funcion de size y k
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k,    cv.carseats$dev, type = "b")

# Ahora aplicamos la funcion prune.misclass() para podar el arbol y 
# obtener aquel con 10 nodos

prune.carseats <- prune.misclass(tree.2, best = 5)

par(mfrow = c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# Veamos el comportamiento de este arbol en el test data
tree.pred <- predict(prune.carseats, test, type = "class")
table(tree.pred, High.test)
prop.table(table(tree.pred, High.test), margin = 2)

precision <- (92+56)/200
precision
