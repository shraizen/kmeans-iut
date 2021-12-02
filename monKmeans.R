
# Les etoiles jaunes sont les centre finaux. Les centres initiaux sont representés par des triangles. Chaque cluster a une couleur propre.

seuil = 0.1 
donnees <- read.table("Xtrain.txt")


monKmeans<- function(donnees, k, EchellePlot) #donnees est le jeu de données utilisé , K le nombre de clusters , EchellePlot pour ajuster l'échelle du plot en x et en y
{
  nombrepoints=nrow(donnees) # nombre de points
  
  x <- donnees[, 1]
  y <- donnees[, 2]
  d=matrix(data=NA, ncol=0, nrow=0)
  for(i in 1:k)                       #on met les points des données dans une matrice
    d <-  c(d, c(x[i], y[i]))         #on met les points des données dans une matrice
  
  matrice <- matrix(d, ncol=2, byrow=TRUE)
  
  plotTitle <- paste("projet kmeans ,  K = ", k)
  plot(donnees, xlim=c(1,EchellePlot), ylim=c(1,EchellePlot), xlab="X", ylab="Y", pch=20, #on initialise le plot 
       main=plotTitle)                        
  par(new=T)                                                                              #on initialise le plot 
  plot(matrice, pch=2, xlim=c(1,EchellePlot), ylim=c(1,EchellePlot), xlab="X", ylab="Y") #on initialise le plot 
  par(new=T)                                                                             #on initialise le plot 
  oldCentres <- matrice
  oldCentres 
  clustering <- Clustering(donnees, oldCentres)
  clustering
  Centres <- UpdateCentres(donnees, clustering, k)
  
  thr <- Triangle(oldCentres, Centres)
  iterations <- 1
  while(thr > seuil)
  {
    clustering <- Clustering(donnees, Centres)
    oldCentres <- Centres
    Centres <- UpdateCentres(donnees, clustering, k)
    thr <- Triangle(oldCentres, Centres)
    iterations <- iterations+1
  }
  clustering
  thr
  Centres
  iterations
  
  for(km in 1:k)
  {
    group <- which(clustering == km)
    
    plot(donnees[group,],axes=F, col=km, xlim=c(1,EchellePlot), ylim=c(1,EchellePlot), pch=20, xlab="X", ylab="Y")
    par(new=T)
  }
  
  plot(Centres, axes=F, pch=8, col=15, xlim=c(1,EchellePlot), ylim=c(1,EchellePlot), xlab="X", ylab="Y")
  par(new=T)
  
  
}

######################### fin kCentres
#fonction distance
Distance <- function(x,y)
{
  d<-sqrt( sum((x - y) **2 ))
}

CreerMatriceCentre <- function(d)
{
  matrix(d, ncol=2, byrow=TRUE)
}

euclide <- function(a,b){
  d<-sqrt(a**2 + b**2)
}
euclide2 <- function(a){
  d<-sqrt(sum(a**2))
}

#calculer difference entre les nouveaux et anciens clusters
Triangle <- function(oldCentres, newCentres)
{
  a <- newCentres - oldCentres    
  max(euclide(a[, 1], a[, 2]))
}

Clustering <- function(donnees, Centres)
{
  clusters = c()
  nombrepoints <- nrow(donnees)
  for(i in 1:nombrepoints)
  {
    distances = c()
    k <- nrow(Centres)
    for(j in 1:k)
    {
      di <- donnees[i,] - Centres[j,]
      ds<-euclide2(di)
      distances <- c(distances, ds)
    }
    minDist <- min(distances)
    clustering <- match(minDist, distances)
    clusters <- c(clusters, clustering)    
  }
  return (clusters)
}

UpdateCentres <- function(donnees, clustering, k)
{
  Centres <- c()
  for(c in 1:k)
  {
    # obtenir le point du cluster c
    group <- which(clustering == c)
    
    # calculer le point moyen de tous les points de la grappe c
    mt1 <- mean(donnees[group,1])
    mt2 <- mean(donnees[group,2])
    vMean <- c(mt1, mt2)
    Centres <- c(Centres, vMean)
  }
  Centres <- CreerMatriceCentre(Centres)
  return(Centres)
}


