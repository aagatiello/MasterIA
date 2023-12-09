####################################
# Ejemplo Clasificador Naive Bayes #
####################################

#install.packages("tm")
library(tm)
#install.packages("e1071")
library(e1071)
# Vamos a utilizar un dataset de mensajes SMS con 5575 instancias
# de las cuales 4831 instancias son ham y 747 spam
###########################
# 1. Preparacion de datos #
###########################

# Vamos a utilizar una representacion bag-of-words que ignora el orden
# en que aperecen las palabras y proporciona una variable indicando 
# si aparece

sms_data <- read.csv("/Users/fcastanf/Desktop/stuff/Clases/UNIR/Videos//NB_example/sms_data.csv", 
                     stringsAsFactors = FALSE)

# observamos el formato del dataset
head(sms_data)
tail(sms_data)

# hacemos que la columna type sea factor
sms_data$type <- as.factor(sms_data$type)

# veamos cuantas instancias tenemos de cada clase
prop.table(table(sms_data$type))

# vamos a modificar el texto de los mensajes
# para que sea mas facil crear el clasificador

# creamos un corpus a partir de una coleccion
# de documentos
sms_corpus <- Corpus(VectorSource(sms_data$text))

# veamos los tres primeros documentos
head(sms_corpus[[1]]$content)
head(sms_corpus[[2]]$content)
head(sms_corpus[[3]]$content)

# convertimos todo el texto a lower-case y eliminamos numeros
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
head(corpus_clean[[1]]$content)

corpus_clean <- tm_map(corpus_clean, content_transformer(removeNumbers))
head(corpus_clean[[3]]$content)

# eliminamos las stop-words
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
head(corpus_clean[[3]]$content)

# eliminamos simbolos de puntuacion
corpus_clean <- tm_map(corpus_clean, removePunctuation)
head(corpus_clean[[3]]$content)

# eliminamos espacios adicionales
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
head(corpus_clean[[3]]$content)

# veamos los tres primeros documentos
# inspect(corpus_clean[1:3])
head(corpus_clean[[1]])
head(corpus_clean[[2]])
head(corpus_clean[[3]])

# Dividimos el corpus en componentes individuales y creamos la matriz term-document
sms_td <- DocumentTermMatrix(corpus_clean)

##############################################
# 2. Creacion de datos de entrenamiento/test #
##############################################
sms_raw_train <- sms_data[1:4500, ]
sms_raw_test  <- sms_data[4501:5574, ]

sms_corpus_train <- corpus_clean[1:4500]
sms_corpus_test  <- corpus_clean[4501:5574]

sms_td_train <- sms_td[1:4500, ] 
sms_td_test  <- sms_td[4501:5574, ]

# Observamos que se mantienen las proporciones
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

##########################################
# 3. Creacion de features para el modelo #
##########################################

# Es necesario transformar la matrix term-document en una estructura
# que pueda ser utilizada por el clasificador NB
# Vamos a eliminar aquellas palabras que aparecen en menos de 10 mensajes

sms_td_train <- sms_td[1:4500, ] 
sms_td_test  <- sms_td[4501:5574, ]
sms_freq_terms <- findFreqTerms(sms_td, 10)

sms_filtered_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_freq_terms))
sms_filtered_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_freq_terms))

# El clasificador NB necesita variables categoricas.
# convertimos las variables a factor
convert_values <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  x <- as.factor(x)
  return(x)
}

# utilizando apply() la aplicamos sobre todas las filas de la matriz
sms_train <- apply(sms_filtered_train, MARGIN = 2, convert_values)
sms_test  <- apply(sms_filtered_test, MARGIN = 2,  convert_values)
  
# Entrenamos un clasificador NB.
# El modelo utiliza la presencia o ausencia de una palabra para estimar la probabilidad de que
# un mensaje determinado sea spam
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)

# Predecimos la clase mas probable
sms_test_pred <- predict(sms_classifier, sms_test, type = "class")

# Matriz de confusion
table(sms_test_pred, sms_raw_test$type)
prop.table(table(sms_test_pred, sms_raw_test$type), margin = 2)

# Mensajes Originales donde se ha equivocado el modelo
sms_raw_test[sms_raw_test$type != sms_test_pred,]

# Predecimos con probabilidades y Pintamos el histograma
sms_test_pred <- predict(sms_classifier, sms_test, type = "raw")

sms_test_pred <- as.data.frame(sms_test_pred)
hist(sms_test_pred$spam)
