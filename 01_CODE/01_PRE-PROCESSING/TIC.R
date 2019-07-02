rm(list = ls())
myPeaks <- rMSIproc::LoadPeakMatrix("d:/Usuarios/Admin/METSE/TFM/Projects/Test/mergeddata-peaks.zip")
#myPeaks <- rMSIproc::LoadPeakMatrix("d:/Usuarios/Admin/METSE/TFM/region7/mergeddata-peaks.zip")

colfunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))
#Assignment of values


Kmeans<-11
Kval <- 2
# for (Kval in 2:Kmeans)
# {
Mat_Intensity <- myPeaks$intensity
Mat_TICNormalized <- matrix(0,nrow = nrow(Mat_Intensity),ncol = ncol(Mat_Intensity))
Arr_TICNormalization <- myPeaks$normalizations$TIC
Arr_PixelPosition <- as.data.frame(myPeaks$pos)
maxx <- max(Arr_PixelPosition$x)
maxy <- max(Arr_PixelPosition$y)
PixelPosition <- list(Arr_PixelPosition = Arr_PixelPosition, maxx = maxx, maxy = maxy)

#Cleaning smaller values than 1
for (val in 1:length(Arr_TICNormalization))
{
  if(Arr_TICNormalization[val] < 10)
  {
    Arr_TICNormalization[val]<-0
  }
}


#Cleaning pixels of values removed from TIC normalization
for(val in 1:length(Arr_TICNormalization))
{
  if(Arr_TICNormalization[val] == 0)
  {
    Mat_Intensity[val,]<-NA
    PixelPosition$Arr_PixelPosition$x[val]<-NA
    PixelPosition$Arr_PixelPosition$y[val]<-NA
    #Arr_TICNormalization[val] <- NA
  }
}

# Arr_TICNormalization <- na.omit(Arr_TICNormalization)
# histogram <- hist(Arr_TICNormalization, breaks = 50)
# histogram$density = histogram$counts/sum(histogram$counts)*100
# plot(histogram, freq = FALSE, ylab = "Density (%)")

#Normalization after removing values from TIC normalization
for(column in 1:ncol(Mat_Intensity))
{
  for(row in 1:nrow(Mat_Intensity))
  {
    Mat_TICNormalized[row,column]<-Mat_Intensity[row,column]/Arr_TICNormalization[row]
    if(is.infinite(Mat_TICNormalized[row,column]))
    {
      Mat_TICNormalized[row,column]<-NA
    }
    if(is.nan(Mat_TICNormalized[row,column]))
    {
      Mat_TICNormalized[row,column]<-NA
    }
    if(Arr_TICNormalization[row] != 0)
    {
      Mat_TICNormalized[row,column]<-Mat_Intensity[row,column]/Arr_TICNormalization[row]
      if(is.infinite(Mat_TICNormalized[row,column]))
      {
        Mat_TICNormalized[row,column]<-NA
      }
      if(is.nan(Mat_TICNormalized[row,column]))
      {
        Mat_TICNormalized[row,column]<-NA
      }
    }
    else
    {
      Mat_TICNormalized[row,column]<-NA
    }
  }
}

#Matrix removing NaN rows
Mat_FiltIntensity <- na.omit(Mat_TICNormalized)
Arr_FiltPosition <- na.omit(PixelPosition$Arr_PixelPosition)

#K-means over the resulting matrix
Arr_kmeansTIC <- kmeans(Mat_FiltIntensity, Kmeans)
# Arr_kmeansTIC <- kmeans(Best_Solutions$Solutions[[39]]$Dataset,Best_Solutions$Kvalue, iter.max = 500)
#Arr_kmeansTIC <- kmeans(Mat_FiltIntensity,Best_Solutions$Kvalue)
#Arr_kmeansTIC <- kmeans(X, Best_Solutions$ClustersNum)
Val_kmeansMax <- max(Arr_kmeansTIC$cluster)

#Convert cluster resulting array into matrix.
Mat_Image <- matrix(NaN, PixelPosition$maxy, PixelPosition$maxx)

for ( val in 1:(nrow(Mat_FiltIntensity)) )
{
  if(!(is.nan(Arr_FiltPosition$y[val])))
  {
    Mat_Image[Arr_FiltPosition$y[val],Arr_FiltPosition$x[val]] <- Arr_kmeansTIC$cluster[val]
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

#Generate TIC image
#graphics::image(Mat_Image, col = rainbow(Val_kmeansMax))
#title <- paste("Groups:", toString(length(Subgroups$Nearest)),"     ","Kmeans:",toString(K_iteration), sep = " ")
# title <- paste("Mouse brain sample.TIC Normalization.","Kmeans: k =",toString(Best_Solutions$Kvalue), "Number of variables:", 
#                toString(length(Best_Solutions$Solutions[[42]]$Variables)), sep = " ")
# image(Mat_Image,
#       axes = FALSE,
#       col = colfunc(Kmeans)
#       )
#image.rota


title <- paste("K = ", toString(Kmeans),sep = " ")
image(Mat_Image, axes = FALSE, col = colfunc(Kmeans), main = title)
filename <- paste("K_",toString(Kmeans),".png", sep = "")
png(filename, width = 189, height = 108, units = "px", res = 10)
image(Mat_Image, axes = FALSE, col = colfunc(Kmeans))
dev.off()
# }