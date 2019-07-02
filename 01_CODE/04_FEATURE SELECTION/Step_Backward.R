
#Step Backward regression algorithm
#Inputs:
# 1.- X -> Input peaks matrix. Columns are variables, Rows observations (Pixels).
# 2.- maxIterations -> Maximum number of iterations allowed.
# 3.- Kvalue -> Value corresponding to the numbers of clusters which has to be used for k-means calculation.
#
#Output:
# 1.- Best_Solutions -> list containing the best solutions found at each iteration. 
#

Step_Backward <- function(X, maxIterations, Kvalue)
{
  Sample_Vect <- c(1:ncol(X))
  
  F_val <- f_score(X = X, Kvalue = Kvalue)
  X_temp <- X
  X_Best <- X
  flag_stop <- 0
  iteration <- 1
  numSolutions <- 0
  vect_results <- vector("numeric", length = maxIterations)
  
  Best_Solutions <- list()
  Best_Solutions[["Kvalue"]]<-Kvalue
  Best_Solutions[["Results"]]<- vect_results
  Best_Solutions[["Solutions"]]<-list()
  
  
  while(iteration < maxIterations)
  {
    if((length(Sample_Vect) == 0) || (length(X_Best) == 0))
    {
      iteration <- iteration-2
      Sample_Vect<-Best_Solutions$Solutions[[iteration]]$Variables
      X_Best <- Best_Solutions$Solutions[[iteration]]$Dataset
    }
    
    F_vector <- vector(mode = "numeric", length = length(Sample_Vect))
    
    for(position in 1:length(F_vector))
    {
      
      X_temp <- X_Best[ , -position]
      F_vector[position] <- f_score(X = X_temp, Kvalue = Kvalue)
      
      par(mfrow=c(1,2))
      plot(F_vector, pch = 1)
      plot(vect_results, pch = 2)
    }
    
    Var_Max <- which.max(F_vector)
    F_best <- F_vector[Var_Max]
    vect_results[iteration] <- F_best
    X_temp <- X_Best[ , -which.max(F_vector)]
    Sample_Vect <- Sample_Vect[-which.max(F_vector)]
    
    Solution <- list()
    Solution[["Variables"]] <- Sample_Vect
    Solution[["Fvector"]]<-F_vector
    Solution[["Dataset"]]<- X_temp
    Best_Solutions$Solutions[[iteration]]<-Solution
    Best_Solutions$Results <- vect_results
    X_Best <- X_temp
    
    iteration <- iteration + 1
  }
  
  return(Best_Solutions)
}

