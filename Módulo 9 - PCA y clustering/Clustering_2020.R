######################################
# K-Means Clustering
######################################


ds = read.csv("C:/Users/charly/Desktop/UBA/UBA curso data Science/+++ curso completo POR CLASES - 1 CUATR 2019/CLASE 10 - PCA y Clustering/Mall_Customers.csv")

#  nos interesan solo las columnas 4 y 5
ds = ds[4:5]

library(caTools)


# método del codo (elbow) para enconterR LA CANTIDAD ÓPTIMA DE CLUSTERS
set.seed(6)
wcss = vector()

for (i in 1:10){
  mod.i=kmeans(ds, i)
  wcss[i] = sum(mod.i$withinss)
}
plot(1:10,wcss,type = 'b',pch=19,main = paste('Método Elbow para determinar cant de Clusters'),
     xlab = 'Cant. de clusters',ylab = 'WCSS')

#  vemos que la cantidad óptima de cluester es 5

# Modelo K-Means en ds mall
#  parámetros: maximas iteraciones (le indico hasta cuando probar si hay menor disrtancia) 
#  y cantidad de valores asignaciones (o sea de iteraciones del  entrenamiento) a los 
#  5 clusters de paso 1 del algoritno kNN 

kmeans = kmeans(x = ds, centers = 5, iter.max = 300, nstart = 10)
y_kmeans = kmeans$cluster

table(y_kmeans)


# LISTA CON los clusters y las observaciones del ds en cada uno
clusters = list()
for (i in 1:5) clusters[[i]] = which(y_kmeans==i)

clusters

# Visualización de los clusters
library(cluster)

# parámetros: lines=0 para que no aparezcan lineas de distancia. shaded=T PARA QUE LOS CLUESTERS SE SOMBREEN EN FC DE SU DENSIDAD
 
clusplot(ds,y_kmeans,lines = 0,shade = TRUE, color = TRUE,labels = 2,plotchar = FALSE,
         span = TRUE,cex.txt=0.6, main = paste('Clusters de clientes'),xlab = 'Ingreso Anual',
         ylab = 'Score de consumo')


#########################################
# CLUSTERING JERARQUICO
#########################################

# DENDOGRAMA CON DISTANCIA EUCLIDEA Y disimitud Ward 
hc = hclust(d = dist(ds, method = 'euclidean'), method="ward.D")
plot(hc,main = paste('Dendrograma'),xlab = 'Clientes',
     ylab = 'Euclidean distances', cex=0.5)

# ALGORITMO para determinando la cantidad óptima de clusters

#  función que calcule la suma de cuadrados dentro de un cluster (WSS)
wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}

#  función que calcule la suma de cuadrados dentro de cada cluster de un modelo de clustering
calc_wss <- function(i, hc, x) {
  cl <- cutree(hc, i)
  spl <- split(x, cl)
  wss <- sum(sapply(spl, wss))
  wss
}

# armo el vector con todos los wss para cada cantidad de clusters
wss_vec = vector()
for (i in 1:10) wss_vec[i] = calc_wss(i,hc,ds)

plot(1:10,wss_vec,type = 'b',pch=19,main = paste('Método Elbow en Clustering Jerárquico'),
     xlab = 'Cant. de clusters',ylab = 'WSS')

#  los valores cortando el dendograma en 5 clusters
y_hc = cutree(hc, 5)

# LISTA CON los clusters y las observaciones del ds en cada uno
clusters_hc = list()
for (i in 1:5) clusters_hc[[i]] = which(y_hc==i)
clusters_hc

#  Visualizando los clusters
library(cluster)
clusplot(ds, y_hc,lines = 0, shade = TRUE,color = TRUE,labels= 2,
         plotchar = FALSE,span = TRUE,main = paste('Clusters de Clientes'),
         xlab = 'Ingreso Anual', ylab = 'Score de Consumos')

#  ahora quiero ver los cluesters en el dendograma
plot(hc, cex = 0.6)
rect.hclust(hc, k = 5, border = 2:5)




