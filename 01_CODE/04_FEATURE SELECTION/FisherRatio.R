
#Fisher Score function
#Inputs:
# 1.- X -> Input peaks matrix. Columns are variables, Rows observations (Pixels).
# 2.- Kvalue -> Value corresponding to the numbers of clusters which has to be used for k-means calculation.
#
#Output:
# 1.- Fisher Ratio value.


f_score <- function (X, Kvalue) 
{
  #With the provided data, kmeans is calculated.
  
  #If input X is only one variable and it is sent as an array,
  #convert it into matrix.
  if(is.null(dim(X)))
  {
    X<-as.matrix(X)
  }
  
  #Compute k-means.
  Kdata <- kmeans(X, Kvalue, iter.max = 5000)
  #If warning appears, try again with a higher limit for
  #iterations.
  while(Kdata$ifault == 4) 
  { 
    Kdata <- kmeans(X, Kvalue, iter.max = 10000);
    if(Kdata$ifault != 4)
    {
      print("DEBUG - Warning detected/solved.")
    }
  }
  
  #Assign centers and clusters obtained from k-means.
  Centers <- Kdata$center
  Clusters <- Kdata$cluster
  
  #Between-group variability calculation.
  Overall_means <- colMeans(Centers)
  Means_dist <- matrix(data = NA, ncol = ncol(Centers), nrow = nrow(Centers))
  for(rowindex in 1:nrow(Means_dist))
  {
    Means_dist[rowindex, ] <- (Centers[rowindex, ]-Overall_means)^2
  }
  Means_sum <- rowSums(Means_dist)
  Observations <- table(Clusters)
  Den_BGVar <- sum(Means_sum*Observations)
  Num_BGVar <- Kvalue - 1
  
  BGVar <- Den_BGVar/Num_BGVar
  
  #Within-group variability
  WGVar <- 0
  valor <- 1
  while(valor <= Kvalue)
  {
    #print(valor)
    Clust_Observations <- X[-which(Clusters != valor),]
    if(is.null(dim(Clust_Observations)))
    {
      Clust_Observations<-as.matrix(Clust_Observations)
      Clust_Observations<-t(Clust_Observations)
    }
    Dist_Observations <- matrix(data = NA, nrow = nrow(Clust_Observations), 
                                ncol = ncol(Clust_Observations))
    for(rowindex in 1:nrow(Dist_Observations))
    {
      Dist_Observations[rowindex, ]<- (Clust_Observations[rowindex, ]-Centers[valor, ])^2
    }
    Dist_Result <- rowSums(Dist_Observations)/(nrow(X)-Kvalue)
    WGVar <- WGVar + sum(Dist_Result)
    valor <- valor + 1
  }
  WGVar <- WGVar/ncol(X)
  
  Fvalue <- BGVar/WGVar
  
  return(Fvalue)
}