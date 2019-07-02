rm(list = ls())
myPeaks <- rMSIproc::LoadPeakMatrix("d:/Usuarios/Admin/METSE/TFM/Projects/Test/mergeddata-peaks.zip")

colfunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))

#Assignment of values
Kval <- 15
Mat_Intensity <- myPeaks$intensity
Mat_Intensity<-Mat_Intensity[,-152]
Mat_TICNormalized <- matrix(0,nrow = nrow(Mat_Intensity),ncol = ncol(Mat_Intensity))
Arr_TICNormalization <- myPeaks$normalizations$TIC
Mat_AcqTICNormalized <- matrix(0,nrow = nrow(Mat_Intensity),ncol = ncol(Mat_Intensity))
Arr_AcqTICNormalization <- myPeaks$normalizations$AcqTic
Arr_PixelPosition <- as.data.frame(myPeaks$pos)
maxx <- max(Arr_PixelPosition$x)
maxy <- max(Arr_PixelPosition$y)
PixelPosition <- list(Arr_PixelPosition = Arr_PixelPosition, maxx = maxx, maxy = maxy)

#TODO:
# 1. Remove undesired values from Mat_Intensity, Arr_AcqTICNormalization, and Arr_PixelPosition
#    (using Arr_TICNormalization array).
# 2. Apply normalization using TIC Acquired array for the resulting positions.
# 3. Apply k-means clustering over the resulting matrix and build image.

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
    Arr_AcqTICNormalization[val]<-NA
    Mat_Intensity[val,]<-NA
    PixelPosition$Arr_PixelPosition$x[val]<-NA
    PixelPosition$Arr_PixelPosition$y[val]<-NA
  }
}

#Normalization after removing values from TIC normalization
#Normalization after removing values from TIC normalization
for(column in 1:ncol(Mat_Intensity))
{
  for(row in 1:nrow(Mat_Intensity))
  {
    if(Arr_TICNormalization[row] != 0)
    {
      Mat_AcqTICNormalized[row,column]<-Mat_Intensity[row,column]/Arr_AcqTICNormalization[row]
      if(is.infinite(Mat_AcqTICNormalized[row,column]))
      {
        Mat_AcqTICNormalized[row,column]<-NA
      }
      if(is.nan(Mat_AcqTICNormalized[row,column]))
      {
        Mat_AcqTICNormalized[row,column]<-NA
      }
    }
    else
    {
      Mat_AcqTICNormalized[row,column]<-NA
    }
  }
}

#Matrix removing NaN rows
Mat_FiltIntensity <- na.omit(Mat_AcqTICNormalized)
Arr_FiltPosition <- na.omit(PixelPosition$Arr_PixelPosition)

#K-means over the resulting matrix
Arr_kmeansTIC <- kmeans(Mat_FiltIntensity,Kval)
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

title <- paste("Mouse brain sample.TIC Acquired Normalization.","Kmeans: k =",toString(Kval), sep = " ")
image(Mat_Image,
      axes = FALSE,
      col = colfunc(Kval),
      main = title
)

#Generate TIC image
image(Mat_Image,
      axes = FALSE)
