
#Step Forward regression algorithm
#Inputs:
# 1.- X -> Input peaks matrix. Columns are variables, Rows observations (Pixels).
# 2.- maxSolutions -> Maximum number of solutions desired.
# 3.- Kvalue -> Value corresponding to the numbers of clusters which has to be used for k-means calculation.
#
#Output:
# 1.- Best_Solutions -> list containing the best solutions found at each iteration. 
#

Step_Forward <- function (X, maxSolutions, Kvalue)
{
  Sample_Vect <- c(1:ncol(X))
  iteration <- 1
  numSolutions <- 1
  vect_results <- vector("numeric", length = maxSolutions)
  X_selected <- matrix()
  
  Best_Solutions <- list()
  Best_Solutions[["Kvalue"]]<-Kvalue
  Best_Solutions[["Results"]]<- vect_results
  Best_Solutions[["Solutions"]]<-list()
  
  Selected_Vars<-vector()
  
  
  while (numSolutions <= maxSolutions)
  {
    
    F_vector <- vector(mode = "numeric", length = length(Sample_Vect))
    
    if(numSolutions == 1)
    {
      for(position in 1:length(Sample_Vect))
      {
        X_temp <- as.matrix(X[ , position])
        F_vector[position] <- f_score(X = X_temp, Kvalue = Kvalue)
      }
      X_selected <- as.matrix(X[ , which.max(F_vector)])
      X<-X[,-which.max(F_vector)]
    }
    else
    {
      for(position in 1:length(Sample_Vect))
      {
        X_temp <- cbind(X_selected, X[ , position])
        F_vector[position] <- f_score(X = X_temp, Kvalue = Kvalue)
      }
      X_selected<- cbind(X_selected, X[,which.max(F_vector)])
      X<-X[,-which.max(F_vector)]
    }
    print(which.max(F_vector))
    vect_results[numSolutions]<-F_vector[which.max(F_vector)]
    plot(vect_results)
    Selected_Vars<-append(Selected_Vars,Sample_Vect[which.max(F_vector)])
    Sample_Vect<-Sample_Vect[-which.max(F_vector)]
    
    Solution <- list()
    Solution[["Variables"]] <- Selected_Vars
    Solution[["Fvector"]]<- F_vector
    Solution[["Dataset"]]<- X_selected
    Best_Solutions$Solutions[[numSolutions]]<-Solution
    Best_Solutions$Results <- vect_results
    
    numSolutions <- numSolutions+1
    
  }
  
  return(Best_Solutions)
}
  
