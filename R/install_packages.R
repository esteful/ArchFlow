#All the packages required to run ArchFlow

#First of all, make sure the working directory is the correct one. The same as the .Rproj file.
getwd() -> current_wd
setwd(current_wd)


packages <- c("archdata",
                      "plyr",
                      "dplyr",
                      "RcmdrMisc",
                      "compositions",
                      "ggplot2",
                      "ggfortify",
                      "ggthemes",
                      "plotrix",
                      "dendextend",
                      #"d3heatmap",
                      "stargazer",
                      "devEMF",
                      "devtools",
                      "rmarkdown")
            
            
            new <- packages[!(packages %in% installed.packages()[,"Package"])]
            #watch out the difference between installed.packages and install.packages
            if(length(new)) install.packages(new)
            lapply(packages, require, character.only=TRUE)
            
          
            #install package d3heatmap (otherwise gives an R)
            devtools::install_github("rstudio/d3heatmap")

            
            rm(new)
            rm(packages)
            
  


message("The following folder is the working directory of this project:")
print(getwd())

message("#All the packages were installed and loaded succesfully! #All the functions were loaded to the system succesfully! # The ArchFlow rmd files are ready to run!")

#sessionInfo() 






