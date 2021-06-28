#' Entropy of the dataset.
#'
#' Provides the contribution of each element that is taken as a divisor in the ALR transformation to the entropy of total information. 
#' Code developed from the original code from J.Buxeda i Garrigos.
#' Entropy: It includes the value of the entropy of the information (in base logarithms 2) (H2)
#' and the relative value that this entropy has with respect to the maximum that can reach for the number of dimensions (elements) that are take into account (H2%).
#' The entropy is calculated on the τ.i value, once the total variation (vt) has been subtracted.
#' Probability: provides the probabilities of each of the divisors estimated from the relative frequency 
#' that each value τ.i represents with respect to the sum of all these values.
#' 
#' @param df_chem a dataframe only with chemical data

#' @return Provides two dataframes based on \code{df_chem}: Entropy and Probability. 

#' @import ggplot2 ggthemes devEMF
#' @references Aitchison, J. (1986). The Statistical Analysis of Compositional Data. 
#'     \url{https://doi.org/10.1007/978-94-009-4109-0}
#'     Buxeda i Garrigós, J., & Kilikoglou, V. (2003). 
#'     Total Variation as a Measure of Variability in Chemical Data Sets. Patterns and Process. 
#'     A Festchrift in Honor to Dr. Edward Sayre, March, 185–198.
#' @export




"arch_entropy" <-   function(df_chem)
  {
 
  #n_individuals  of rows (individuals)
  n_individuals <- nrow(df_chem) 
  
  # number of columns
  n_variables <- ncol(df_chem) 

  #dataframe for entropy
  varmat <- matrix(0, n_individuals, n_variables + 2) 

  #dataframe for probability
  varmat2 <- matrix(0,n_individuals, n_variables) 
 
  {
    # normalize to 1 all values in the dataframe
    varmat2<-as.matrix(sweep(df_chem,1,apply(df_chem,1,sum),FUN="/"))
  }
  
  for (j in 1:n_individuals) #for every row
  {
    for (k in 1:n_variables) #for evert column
    {
      if (varmat2[j,k]==0) 
           varmat[j,k]<-0 
      else varmat[j,k]<- log((1/varmat2[j,k]))/log(2) #add the values 
        varmat[j,n_variables +1] <- varmat[j,n_variables +1]+ (varmat[j,k]*varmat2[j,k])
    }
  }
  
  varmat[,n_variables +2]<-varmat[,n_variables +1]/(log(n_variables )/log(2))
  
  dimnames(varmat) <-list(c(dimnames(df_chem)[[1]]),
                          c(dimnames(df_chem)[[2]], 
                            "H2", "H2%"))
  
  list(Entropy=as.data.frame(varmat),
      Probability= as.data.frame(varmat2)) 
}






