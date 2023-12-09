# 1. Ejemplo de regresion trees utilizando el paquete tree
install.packages("tree")
library(tree)
set.seed(1)

# Cargamos el paquete MASS que contiene el valor de las casas en 
# las afueras de Boston
library(MASS)
?Boston
str(Boston)
summary(Boston)

train <- sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston <- tree(medv ~ . , Boston[train, ]) 

summary(tree.boston)

# Vemos que solo se han utilizado 3 variables: lstat, rm, dis
plot(tree.boston)
text(tree.boston, pretty = 0)

# la variable lstat mide el porcentaje de individuos con status 
# socio-economico bajo
# El arbol indica que el valor mas bajo de 
# lstat corresponde con las casas mas caras.

# usemos cvtree() para ver si podando el arbol se mejora
cv.boston <- cv.tree(tree.boston, K = 10)
plot(cv.boston$size, cv.boston$dev, type = 'b')

# vamos a podar el arbol
prune.boston <- prune.tree(tree.boston, best = 7)
plot(prune.boston)
text(prune.boston, pretty = 0)

# hagamos una prediccion con el arbol sin podar
yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test, main = "arbol sin podar")
abline(0, 1)

# Prediccion con arbol podado
yhat.prune <- predict(prune.boston, newdata = Boston[-train, ])
plot(yhat.prune, boston.test, main = "arbol podado")
abline(0, 1)

# Mean Square Error (MSE)
mean((yhat - boston.test)^2)

mean((yhat.prune - boston.test)^2)

# EL MSE del conjunto de test con el arbol de regresiones 25.05
# El sqrt(MSE) es 5.005 lo que indica que las predicciones con este
# modelo estan alrededor de $ %.005 del valor mediano real.
