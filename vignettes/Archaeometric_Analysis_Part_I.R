## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## ----warning=FALSE, cache=FALSE-----------------------------------------------
if(!require(devtools)) install.packages("devtools",repos = "http://cran.us.r-project.org")

install_github("esteful/ArchFlow")
library("ArchFlow")

## -----------------------------------------------------------------------------
if(!require(archdata)) install.packages("archdata",repos = "http://cran.us.r-project.org")
library("archdata")

## -----------------------------------------------------------------------------
  data(RBPottery)
  knitr::kable(RBPottery)

## -----------------------------------------------------------------------------
  row.names(RBPottery) <- RBPottery[,1]
  df_raw <- RBPottery  #we are calling to the whole dataset
  df_chem <- RBPottery[,-c(1:3)] #ignore all the columns containing categorical data, 9 in this case. 

## -----------------------------------------------------------------------------
  str(df_raw)

## -----------------------------------------------------------------------------
  str(df_chem)

## -----------------------------------------------------------------------------
  for (i in 1:ncol(df_chem)){
  .datt <- data.frame(REGION=df_raw$Region, i= df_chem[,i], KILN=df_raw$Kiln)
  plot(ggplot2::ggplot(.datt, ggplot2::aes(x= KILN, y = i, fill=REGION)) + ggplot2::geom_boxplot() + ggplot2::ylab(colnames(df_chem)[i]))
  }

## -----------------------------------------------------------------------------
  df_raw$Calcium <- c() #Create the desired column
  
  "Low-Calcareous(CaO<6%)" ->   df_raw$Calcium[df_raw$CaO < 6]
  "Calcareous (6%<CaO<20%)" ->       df_raw$Calcium[df_raw$CaO> 6]
  "High Calcareous (CaO>%20)" -> df_raw$Calcium[df_raw$CaO > 20]
   
  df_raw$Calcium <- as.factor(df_raw$Calcium)  #bring back to factors

## -----------------------------------------------------------------------------
df_raw[,c("CaO", "Calcium")]
#select(df_raw,c(CaO,Calcareous)) 

## -----------------------------------------------------------------------------
arch_varmat(df_chem)

## ----warning=FALSE------------------------------------------------------------
arch_evenness(df_chem)

## -----------------------------------------------------------------------------
  #select this with the variables to use 
  vars <- c("MnO","CaO","Na2O", "TiO2","BaO", "Al2O3") 

  #alr conversion
  compositions::alr(df_chem,ivar = 2)-> df_alr
  cbind(df_raw[,1:3],df_alr) -> df_alr

  arch_scatter_matrix(df_raw=df_alr, vars, color = "Region", shape = "Kiln", title= "RBPottery")
  #a pdf file is saved in the working directory
  ##Ggcally package can offer similar kinds of visualizations

## -----------------------------------------------------------------------------
arch_dendro(df_chem = df_chem, df_raw = df_raw, printDendro = FALSE, nplot=c(2,3))

## ----warning=FALSE------------------------------------------------------------
arch_PCA(df_chem, df_raw =df_raw, printPCA= FALSE, labels = FALSE, nplot = c(2,3), shape_cat_number = 2)

## -----------------------------------------------------------------------------
#arch_heatmap(df_chem)

## -----------------------------------------------------------------------------
SiO2 <- 0
total <- 98


if (!"SiO2" %in% colnames(df_chem) == TRUE)  {
  
      SiO2 <- 1
    
      df_chem$SiO2 <- c(rep(0, nrow(df_chem)))
    
  for (i in 1:nrow(df_chem)){
      
      df_chem$SiO2[i] <- total - sum(df_chem[i,])

      }
      
cbind(df_raw[,c(1:3)], df_chem) -> df_raw

}


## ----fig.height=6, fig.width=6------------------------------------------------
arch_triangles(df_raw, plot.category = 3, rounded_circle = ) #grup = indicates the column from which the factors will be for the legend

## ----fig.height=7, fig.width=7------------------------------------------------
#Remove the column of estimated SiO2
if (SiO2 == 1){
  df_chem[,-c(ncol(df_chem))] -> df_chem
  df_raw[,-c(ncol(df_raw))] -> df_raw
  SiO2 <- 0
}


## ----echo = FALSE-------------------------------------------------------------
message("This is the first step of the ArchFlow workflow")

## ---- fig.show='hold'---------------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(mtcars, 10))

