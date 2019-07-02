rm(list = ls())
myPeaks <- rMSIproc::LoadPeakMatrix("d:/Usuarios/Admin/METSE/TFM/Projects/Test/mergeddata-peaks.zip")

#In case of using Logarithmic Scaling add 1 to all positions to avoid zeros.
#myPeaks$intensity <- myPeaks$intensity + 1

colfunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))

filename <- "HClust_K7.png"
#png(filename, width = 14.92, height = 8.89, units = "cm", res = 300)
#png(filename, width = 14.78, height = 23.47, units = "cm", res = 300)
png(filename, width = 14.78, height = 4.71, units = "cm", res = 300)
#png(filename, width = 4.99, height = 2.85, units = "cm", res = 300)
par(mfrow=c(1,2))
par(mar=c(1.5,1.5,1.5,1.5))

#Parameter declaration
#--------------------------------------------------------------------
Kmeans <- 7

#--------------------------------------------------------------------



#Data normalization strategy
Normalized_Data <- TIC_Normalization(myPeaks)
#Normalized_Data <- TICAcq_Normalization(myPeaks)

#Scaling strategy
#Normalized_Data <- AutoScaling(NormalizedData = Normalized_Data)
#Normalized_Data <- Log_Scaling(NormalizedData = Normalized_Data)

#Hierarchical Clustering
# AgglomerativeHC <- HClustering(NormalizedData = Normalized_Data, method = "agglomerative")
# HCDendrogram <- as.dendrogram(AgglomerativeHC)
# plot(HCDendrogram, main = "Agglomerative Hierarchical Clustering Dendrogram")
# plot(HCDendrogram,  xlab = "Height", ylim = c(181,254),nodePar = nodePar, horiz = TRUE,
#      main = "Agglomerative Hierarchical\n Clustering Dendrogram [181:254]")
# 
# DivisiveHC <- HClustering(NormalizedData = Normalized_Data, method = "divisive")
# HCDendrogram <- as.dendrogram(DivisiveHC)
# #plot(DivisiveHC, main = "Agglomerative Hierarchical Clustering Dendrogram")
# plot(HCDendrogram,  xlab = "Height", ylim = c(181,254),nodePar = nodePar, horiz = TRUE,
#      main = "Divisive Hierarchical\n Clustering Dendrogram [181:254]")

#Apply K-means to dataset:
Arr_kmeansTIC <- kmeans(Normalized_Data$intensity, Kmeans)
#Arr_kmeansTIC <- kmeans(Best_Solutions$Solutions[[105]]$Dataset, Kmeans)
Val_kmeansMax <- max(Arr_kmeansTIC$cluster)

#Convert clustering result into image matrix.
Mat_Image <- matrix(NaN, max(Normalized_Data$pos$y), max(Normalized_Data$pos$x))

for (val in 1:(length(Arr_kmeansTIC$cluster)))
{
  if(!(is.nan(Normalized_Data$pos$y[val])))
  {
    Mat_Image[Normalized_Data$pos$y[val],Normalized_Data$pos$x[val]] <- Arr_kmeansTIC$cluster[val]
  }
  
}

Dens_vals <- table(Mat_Image)
Vals_sorted <- labels(sort(Dens_vals, decreasing = TRUE))
Vals_sorted <- as.numeric(Vals_sorted$Mat_Image)


for (i in 1:nrow(Mat_Image))
{
  for (j in 1:ncol(Mat_Image))
  {
    Mat_Image[i,j] <- match(Mat_Image[i,j], Vals_sorted)
  }
}

Dens_vals2 <- table(Mat_Image)
Dens_vals
Dens_vals2

#title <- paste("K = ", toString(Kmeans),sep = " ")
title <- "All ions."
title <- "Selected variables: 40."
image(Mat_Image, axes = FALSE, col = colfunc(Kmeans), main = title)



dev.off()