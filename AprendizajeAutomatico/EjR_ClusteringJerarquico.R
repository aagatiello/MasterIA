require(graphics)

### Ejemplo 1, Ratio de crimenes violentos en US por estado
View(USArrests)

d <- dist(USArrests)
hc <- hclust(d_2, "ave")
plot(hc)
plot(hc, hang = -1)

# Hacemos lo mismo con el clustering de centroids y la distancia *squared* Euclidea.
# Cortamos el arbol en 10 clusters y reconstruimos la parte alta del arbol con 
# los centroides de los clusters.

hc <- hclust(dist(USArrests)^2, "cen")
plot(hc)
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)

### Ejemplo 2: distancias de linea-recta entre 10 ciudades US
#   y comparar el resultado de los algoritmos "ward.D" y "ward.D2"

data(UScitiesD)

mds2 <- -cmdscale(UScitiesD)
plot(mds2, type="n", axes=FALSE, ann=FALSE)
text(mds2, labels=rownames(mds2), xpd = NA)

hcity.D  <- hclust(UScitiesD, "ward.D") # "wrong"
hcity.D2 <- hclust(UScitiesD, "ward.D2")
opar <- par(mfrow = c(1, 2))
plot(hcity.D,  hang=-1)
plot(hcity.D2, hang=-1)
par(opar)