#' Centered log-ratio transformation. 
#'
#' Performs centered log-ratio transformation of the \code{df_chem}
#' Based on M.J. Baxter observations on compositional data

#' @param df_chem A dataframe with chemical data


#' @return Centered log-ratio transformed dataframe. 
#' @export


"logcenter.tran"<-
  function(df_chem)
  {
    # versiÛ normalitzada a marÁ de 2015
    if(min(df_chem) < 0) {
      cat("Negative data - terminating.")
      return()
    }
    as.data.frame(sweep(log(df_chem),1,apply(log(df_chem),1,mean),FUN="-"))
  }
