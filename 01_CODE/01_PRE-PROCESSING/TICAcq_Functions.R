
#TIC Acquired normalization function
#Inputs:
# 1.- rMSIprocPeakMatrix
#The object rMSIprocPeakMatrix must contain TIC and TIC Acquired normalizations.
#
#Output:
# 1.- TICAcqnormalization
#       |-> $intensity -> it contains normalized intensities.
#       |-> $pos -> it contains the x,y coordinates to generate image.

TICAcq_Normalization <- function(PeakMatrix)
{
  #Assign Intensity matrix to internal variable.
  Mat_Intensity <- PeakMatrix$intensity
  Mat_Intensity <- Mat_Intensity[,-152]
  #Create matrix that will contain intensity normalization.
  Mat_AcqTICNormalized <- matrix(0,nrow = nrow(Mat_Intensity),ncol = ncol(Mat_Intensity))
  #Assign TIC coefficients array into internal variable.
  Arr_TICNormalization <- PeakMatrix$normalizations$TIC
  #Assign TIC Acquired coefficients array into internal variable.
  Arr_AcqTICNormalization <- PeakMatrix$normalizations$AcqTic
  #Assign pixel positions array to intermediate variables.
  Arr_PixelPosition <- as.data.frame(PeakMatrix$pos)
  maxx <- max(Arr_PixelPosition$x)
  maxy <- max(Arr_PixelPosition$y)
  PixelPosition <- list(Arr_PixelPosition = Arr_PixelPosition, maxx = maxx, maxy = maxy)
  
  #Needed in Mouse brain sample.
  #Clean smaller values than 10 in TIC coefficients array.
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
      #Assign NA to all pixels/intensities/TIC Acquired coefficients where TIC coefficient is 0.
      Arr_AcqTICNormalization[val]<-NA
      Mat_Intensity[val,]<-NA
      PixelPosition$Arr_PixelPosition$x[val]<-NA
      PixelPosition$Arr_PixelPosition$y[val]<-NA
    }
  }
  
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
  
  #Generate output dataset
  TICAcqnormalization <- list()
  TICAcqnormalization[["intensity"]] <- Mat_FiltIntensity
  TICAcqnormalization[["pos"]]<- Arr_FiltPosition
  
  return(TICAcqnormalization)
}