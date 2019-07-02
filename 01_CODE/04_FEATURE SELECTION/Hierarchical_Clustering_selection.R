
#Hierarchical clustering variable selection function
#Inputs:
# 1.- NormalizedData
#       |-> $intensity -> it contains normalized intensities.
#       |-> $pos -> it contains the x,y coordinates to generate image.
# 2.- method -> It can be "agglomerative" or "divisive"
# 3.- varNum -> Number of variables expected.
#
#Output:
# 1.- Selected vars -> List with dataset of selected variables, and information relative to the selected
# variables.
#

#Required function to convert from polar to cartesian coordinates
cart.to.polar <- function (cartesian_coord)
{
  polar_coord <- matrix(0, nrow = nrow(cartesian_coord),
                        ncol = ncol(cartesian_coord))
  for (val in 1:nrow(cartesian_coord)) 
  {
    polar_coord[val,]<-cart2pol(as.vector(cartesian_coord[val,]))
  }
  return(polar_coord)
}

#Required function to calculate module from centroid to variable coordinates
Func_Module <- function(x, y, z, center)
{
  result <- vector()
  for(val in 1:length(x))
  {
    module <- (x[val]-center[1])^2 + (y[val]-center[2])^2 + (z[val]-center[3])^2
    result[val] <- sqrt(module)
  }
  return(result)
}

HClust_VarSelection <- function (NormalizedData, method = "agglomerative", varNum)
{
  library(ggplot2)
  library(factoextra)
  library(pracma)
  library(magrittr) # for pipe %>%
  library(dplyr)   # everything else
  
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
  
  hc_result <- HClustering(NormalizedData, method)
  
  #Split hierarchical clustering into groups
  groups = cutree(hc, k = varNum)
  groups_num <- table(groups)
  
  #Get principal components of Pearson matrix.
  pc1 <- prcomp(Mat_Pearson)
  
  #Individual coordinates
  pc1.pca <- get_pca_ind(pc1)
  
  #Calculate centroids
  centroids <- pc1.pca$coord %>%
    as_data_frame() %>%
    select(Dim.1, Dim.2, Dim.3) %>%
    mutate(cluster_groups = groups) %>%
    group_by(cluster_groups) %>%
    summarise(
      Dim.1 = mean(Dim.1),
      Dim.2 = mean(Dim.2),
      Dim.3 = mean(Dim.3)
    )
  
  #Represent centroids as coordinates classified by groups
  coord.groups <- pc1.pca$coord %>%
    as_data_frame() %>%
    select(Dim.1, Dim.2, Dim.3) %>%
    mutate(cluster_groups = groups) %>%
    group_by(cluster_groups)
  
  #Classify subgroups information.
  Subgroups <- list()
  for(val in 1:length(groups_num))
  {
    Subgroups$Groups[[val]] <- which(groups == val)
    Subgroups$Centroid[[val]] <- c(centroids$Dim.1[val],
                                   centroids$Dim.2[val],
                                   centroids$Dim.3[val])
    Subgroups$Dim1[[val]] <- coord.groups$Dim.1[which(val == coord.groups$cluster_groups)]
    Subgroups$Dim2[[val]] <- coord.groups$Dim.2[which(val == coord.groups$cluster_groups)]
    Subgroups$Dim3[[val]] <- coord.groups$Dim.3[which(val == coord.groups$cluster_groups)]
    Subgroups$Module[[val]] <- Func_Module(Subgroups$Dim1[[val]],
                                           Subgroups$Dim2[[val]],
                                           Subgroups$Dim3[[val]],
                                           Subgroups$Centroid[[val]])
    Subgroups$Nearest[[val]] <- Subgroups$Groups[[val]][which.min(Subgroups$Module[[val]])]
  }
  
  Mat_Selected <- matrix(NA, nrow = nrow(Mat_Intensity), ncol = length(Subgroups$Nearest))
  for(val in 1:length(groups_num))
  {
    Mat_Selected[,val]<-Mat_Intensity[,Subgroups$Nearest[val]]
  }
  
  Selected_vars <- list()
  Selected_vars[["Dataset"]] <- Mat_Selected
  Selected_vars[["Subgroups"]] <- Subgroups
  
  return(Selected_vars)
}

