
#Hierarchical clustering function
#Inputs:
# 1.- NormalizedData
#       |-> $intensity -> it contains normalized intensities.
#       |-> $pos -> it contains the x,y coordinates to generate image.
# 2.- method -> It can be "agglomerative" or "divisive"
#
#Output:
# 1.- hc <- resulting list with hierarchical clustering elements.
#
#

HClustering <- function (NormalizedData, method = "agglomerative")
{
  #Assign intensity matrix to an internal variable.
  Mat_Intensity <- NormalizedData$intensity
  
  #Obtain trasposed matrix of intensities.
  Mat_TrasIntensity <- t(Mat_Intensity)
  
  #Generate R-Pearson matrix
  Mat_Pearson <- matrix(data = NA, nrow = nrow(Mat_TrasIntensity), ncol = nrow(Mat_TrasIntensity))
  for(i in 1:nrow(Mat_Pearson))
  {
    for(j in 1:ncol(Mat_Pearson))
    {
      Mat_Pearson[i,j] <- cor(Mat_TrasIntensity[i,],Mat_TrasIntensity[j,])
    }
  }
  
  #Get Euclidean distance from Mat_Pearson
  D <- dist(Mat_Pearson, method = "euclidean")
  #Convert distance array into Matrix
  Mat_D <- as.matrix(D)
  
  if(method == "agglomerative")
  {
    #Get hierarchical clustering from distance array
    hc <- hclust(D, method = "complete")
  }
  else
  {
    if(method == "divisive")
    {
      hc <- diana(D)
    }
    else
    {
      warning("No valid method selected.")
    }
  }
  
  return(hc)
}