#' Classify by signaries.
#'
#' Creates signary matrix to obtain balances to plot in a dendrogram in a evenness analysis of the variables
#'
#' @param dendroMVC Dendrogram of compositional variation matrix (see \code{arch_evennes} function)
#
#' @return A list of dataframes with \code{output}, \code{signary}, \code{f1}, \code{f2}, \code{m}, \code{des} and \code{zeros} (values that are used in arch_evennes)


#' @export


"arch_classify" <- function(dendroMVC)
  {
      
    n <- length(dendroMVC[[4]])

    output <- matrix(0,n-1,n)
    signary <- matrix(-2,n-1,n) #-2 to avoid confusions with possible values
    zeros <- matrix(-2,n-1,n) #calculates a matrix based on the zeros 
   

    #check the fusions
    for (i in 1:n-1) {
      output[i,]<-cutree(dendroMVC,k=i+1)
    }

    # resolve line by line, the first is special
    # The first only has two groups. The last only has two individuals.
   
    for (i in 1:n-1){
      if (i==1) {
        
        posar <- which(output[i,]==1)
        signary[i,posar] <- 1
    
    # Check if it is leaf and include zeros below
        if (length(posar)==1) {
          signary[c((i+1):(n-1)),posar] <- 0
          zeros[c((i+1):(n-1)),posar] <- 0
        }
        posar<-which(output[i,]==2)
        signary[i,posar]<- -1
        
        if (length(posar)==1) {
          signary[c((i+1):(n-1)),posar] <- 0
          zeros[c((i+1):(n-1)),posar] <- 0
        }
      }
      if (i>1) {
        if (i<(n-1)) {
         

      # prepares a f1 vector where the zeros are included to be transformed into factors 
      # of row i-1 and a vector f2 to be a factor of row i

          f1 <- output[(i-1),]
          f2 <- output[i,]
          posar <- which(zeros[(i-1),]==0)
          
          if (length(posar)>0) {f1[posar] <- 0}
          posar <- which(zeros[i,]==0)
          if (length(posar)>0) {f2[posar] <- 0}
      
      #turn f1 and f2 into factors

          f1<-as.factor(t(f1))
          f2<-as.factor(t(f2))
      
      #Create a matrxi to assess whether the groups are different

          nf1 <- nlevels(f1)
          nf2 <- nlevels(f2)
          m <- matrix(TRUE,nf1,nf2)
          if (levels(f2)[1]== "0") {des <- 2} else {des <- 1}
          for (j in 1:nf1) {
            for (jj in des:nf2) {
              posarf1 <- which(f1==levels(f1)[j])
              posarf2 <- which(f2==levels(f2)[jj])
              m[j,jj] <- identical(posarf1,posarf2)
            }
          }
      
      #A matrix for evaluating. Counter to look for two columns of F of 1 and -1 if the groups are different
          
          counter<-1
          for (jj in 1:nf2) {
            if (any(m[,jj])==FALSE) {
              if (levels(f2)[jj]!= "0") {
                posarf2<-which(f2==levels(f2)[jj])
                signary[i,posarf2]<-counter
                counter<--1
              }
            }
            if (any(m[,jj])==TRUE) {
              posarf2<-which(f2==levels(f2)[jj])
              signary[i,posarf2]<-0
            }
          }
      
      #put zeros in the clades

          posar<-which(signary[i,]==1)
          
          if (length(posar)==1) {
            signary[c((i+1):(n-1)),posar] <- 0
            zeros[c((i+1):(n-1)),posar] <- 0
          }
          posar<-which(signary[i,]==-1)
          if (length(posar)==1) {
            signary[c((i+1):(n-1)),posar] <- 0
            zeros[c((i+1):(n-1)),posar] <- 0
          }
        }

        if (i==(n-1)) {
          f2<-output[i,]
          posar<-which(zeros[i,]==0)
          if (length(posar)>0) {f2[posar] <- 0}
          counter=1
          for (jj in 1:n) {
            if (f2[jj]!=0) {
              f2[jj] <- counter
              counter <--1
            }
          }
          signary[i,] <- f2
        }
      } 
    }
    dimnames(output)[[2]] <- dendroMVC[[4]]
    list(output=as.data.frame(output), signary=as.data.frame(signary), f1=f1, f2=f2, m=as.data.frame(m), des=des, zeros=as.data.frame(zeros))
  }

