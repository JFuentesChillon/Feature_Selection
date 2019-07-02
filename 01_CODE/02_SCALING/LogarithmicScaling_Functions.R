
#Logarithmic Scaling function
#Inputs:
# 1.- NormalizedData
#       |-> $intensity -> it contains normalized intensities.
#       |-> $pos -> it contains the [x,y] coordinates to generate image.
#
#Output:
# 1.- ScaledData
#       |-> $intensity -> it contains scaled intensities.
#       |-> $pos -> it contains the x,y coordinates to generate image.

Log_Scaling <- function(NormalizedData)
{
  #Add 1 to all values in intensity matrix to avoid infinites.
  #NormalizedData$intensity <- NormalizedData$intensity + 1
  
  for(column in 1:ncol(NormalizedData$intensity))
  {
    for(row in 1:nrow(NormalizedData$intensity))
    {
        NormalizedData$intensity[row,column]<-log(NormalizedData$intensity[row,column])
    }
  }
  
  return(NormalizedData)
}