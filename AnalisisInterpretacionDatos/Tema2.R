rm(list=ls())
##################Tema 1#############################################

requiredPackages <- c("arsenal", "car", "corrplot", "fdth","gapminder","dplyr","DescTools", "foreign", "e1071", "expss", "GGally", "ggplot2", "haven", 
                      "knitr","plotly", "remotes", "summarytools","ggridges","table1", "tableone", "tidyverse", "SmartEDA")

sesion1 <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

sesion1(requiredPackages)
######################################################
##LOAD DATA
#Data Lending Club -https://www.kaggle.com/wordsforthewise/lending-club. Factores que determinan el Default en los créditos. Modelo de riesgo
Datalc<-read.csv("https://raw.githubusercontent.com/millerjanny/Custom_UNIR/main/Data_LendingClub.csv")
Datalc$Default=recode_factor(Datalc$Default, `1` = "Default", `0` = "Non-default")
# Exploración inicial

View(Datalc)
head(Datalc)
glimpse(Datalc)
names(Datalc)
str(Datalc)
dim(Datalc)
sapply(Datalc, function(x) sum(is.na(x)))
######################################################
#Tabla de frecuencias variable categórica
table(Datalc$home_ownership_n)
freq(Datalc$home_ownership_n, style = "rmarkdown")

#Tabla de frecuencias variable continua
Datalc$int_rate_cat <- factor(cut(Datalc$int_rate,
                       breaks=nclass.Sturges(Datalc$int_rate),
                       include.lowest=TRUE,
                       dig.lab=4))
freq(Datalc$int_rate_cat, style = "rmarkdown")

(dist_int_rate_cat <- fdt(Datalc$int_rate,breaks="Sturges"))#breaks="Sturges",c(“Sturges”, “Scott”, “FD”)
####################################################

#Pie chart
  #Somos malos para juzgar el tamaño de los ángulos, que es lo que requieren los gráficos de tarta.
  #Cambiar los colores de las porciones de la tarta hace que diferentes porciones parezcan más grandes o más pequeñas.
  #Girar la tarta hace que las diferentes porciones parezcan más grandes o más pequeñas.
  #Cuando hay pocas categorías, desperdician espacio.
  #Cuando hay muchas categorías, son ilegibles.

df <- as.data.frame(table(Datalc$home_ownership_n)/length(Datalc$home_ownership_n))
colnames(df) <- c("class", "relative_freq")
pie <- ggplot(df, aes(x = "", y=relative_freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Home ownership", 
       caption="Source: Lending Club")
pie + coord_polar(theta = "y", start=0)

#Barras
options(warn = -1)
ggplot(data=Datalc, aes(home_ownership_n)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ylab("Percent")+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..), accuracy = 0.01),
                 y= (..count..)/sum(..count..),accuracy = 0.01), stat= "count", vjust = -.5)+
  scale_y_continuous(labels = scales::percent)

#comparación de Barras por variable cualitatitva
ggplot(data=Datalc, aes(x=home_ownership_n)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100),fill="steelblue") + 
  facet_wrap(~Default)+
  ylab("Percent")

#Barras por variable cualitativa en el mismo gráfico. 
ggplot(Datalc, aes(x=home_ownership_n)) +   
  geom_bar(aes(y =(..count..)/sum(..count..)*100,accuracy = 0.01, fill = Default), 
           position = "dodge")+
  ylab("Percent")

#Barras apiladas-variable cualitativa
ggplot(Datalc, aes(x = home_ownership_n, fill = Default)) + 
  geom_bar(aes(y =(..count..)/sum(..count..)*100,accuracy = 0.01))+
  ylab("Percent")

#Histograma-Poligono de frecuencias-continua
ggplot(Datalc, aes(int_rate)) + 
  geom_histogram(mapping=aes(x=int_rate, y=..count../sum(..count..)*100),color="white",fill="darkblue", 
                 bins=nclass.Sturges(Datalc$int_rate))+
  geom_freqpoly(mapping=aes(x=int_rate, y=..count../sum(..count..)*100),
                bins=nclass.Sturges(Datalc$int_rate))+
  ggtitle("Distribution of Interest rate") +
  xlab("Int Rate") +
  ylab("Percent")


#Grafico de frecuencias acumuladas
ggplot(Datalc, aes(int_rate))+
  geom_histogram(aes(y=cumsum(..count../sum(..count..)*100)),bins=nclass.Sturges(Datalc$int_rate),color='white',fill="darkblue")+
  stat_bin(aes(y=cumsum(..count../sum(..count..)*100),bins=nclass.Sturges(Datalc$int_rate)),geom="line",color="green")+
  xlab("Int Rate") +
  ylab("Percent")

dist_int_rate <- fdt(Datalc$int_rate,breaks="Sturges")#k=10
plot(dist_int_rate, type="fh",main='Histograma de frecuencias acumuladas')
plot(dist_int_rate, type="cfh",main='Histograma de frecuencias acumuladas')
plot(dist_int_rate,type="rfp",main='poligono de frecuencias')
plot(dist_int_rate, type="cfpp",main='Ojiva %')

#comparar Grafico de frecuencias acumuladas por variable cualitativa
ggplot(Datalc, aes(int_rate, color = Default)) +                      
  stat_ecdf(geom = "point")+
  ylab("Relative frequency")


#Comparar distrbuciones de variable cuanti por variable cualitativa-crestas
ggplot(Datalc, aes(x = int_rate, y = Default)) +
  geom_density_ridges(aes(fill = Default)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

ggplot(Datalc, aes(x = `int_rate`, y = `home_ownership_n`)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
                       name = "Interest rate")+
  labs(title = 'Distribution of Interest rate by Home ownership') 
#####################################################################
#El paquete gapminder contiene un fichero de datos de población, esperanza de vida y renta per cápita de los países del mundo entre 1952 y 2007.
#La fundación Gapminder es una organización sin fines de lucro con sede en Suecia que promueve el desarrollo global mediante el uso de estadísticas.
library(gapminder)
# Descripción de variables
# country: factor with 142 levels	
# continent: factor with 5 levels	
# year: 1952-2007	
# lifeExp: life expectancy at birth
# pop: total population
# gdpPercap: per-capita GDP

#Gráfico de dispersión
gap=data.frame(gapminder)
ggplot(gap, aes(y=lifeExp, x=log(gdpPercap))) + 
  geom_point()+
  geom_smooth(method=lm)

#Gráfico de linea-variable temporal
spain <- gapminder %>%
  filter(country == "Spain")

ggplot(spain, aes(x = year, y = lifeExp)) +
  geom_line(color = "#0099f9", size = 1) +
  geom_point(color = "#0099f9", size = 2) +
  labs(title = "Average life expectancy in Spain",
    subtitle = "Data from 1952 to 2007",
    caption = "Source: Gapminder dataset") +
  theme(plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0))

######################################################
######################################################
COVID_19 <-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                    na.strings = "", fileEncoding = "UTF-8-BOM")

COVID_19$dateRep <- as.POSIXct( COVID_19$dateRep, format="%d/%m/%Y")#variable date

Totalcases_in_All_Countries<-COVID_19 %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(Cases = sum(cases))

# Comparison between different countries, using a line plot (x = time, y=number of cases). 
cv <- ggplot(COVID_19, aes(x = dateRep, y= cases)) 
cv + geom_line()
  

COVID_19s<-COVID_19 %>% group_by(countryterritoryCode)%>%
  filter(sum(cases)>500000)
cv2 <- ggplot(COVID_19s, aes(x = dateRep, y= cases)) 
cv2 + geom_col(aes(fill=countryterritoryCode)) 
