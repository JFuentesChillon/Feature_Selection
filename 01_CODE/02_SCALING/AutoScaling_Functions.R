
#Autoscaling function
#Inputs:
# 1.- NormalizedData
#       |-> $intensity -> it contains normalized intensities.
#       |-> $pos -> it contains the [x,y] coordinates to generate image.
#
#Output:
# 1.- NormalizedData
#       |-> $intensity -> it contains scaled intensities.
#       |-> $pos -> it contains the x,y coordinates to generate image.

AutoScaling <- function(NormalizedData)
{
  #Create matrix of same dimensions as input intensity matrix.
  Mat_ScaledIntensity <- matrix(NaN, nrow = nrow(NormalizedData$intensity), ncol = ncol(NormalizedData$intensity))
  #Create vector with Autoscaling coefficients.
  Vect_AutoScaling <- vector("numeric", length = ncol(NormalizedData$intensity))
  
  for(column in 1:ncol(NormalizedData$intensity))
  {
    Vect_AutoScaling[column] <- max(NormalizedData$intensity[,column])
    NormalizedData$intensity[,column]<-NormalizedData$intensity[,column]/(Vect_AutoScaling[column])
  }
  
  return(NormalizedData)
}