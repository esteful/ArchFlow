
#' Hierarchical Clustering Analysis.
#'
#' Plots hierarchical clustering of clr transformed data using centroid algorithm and squared euclidean distance.
#'
#' @param df_chem chemical data
#' @param df_raw categorical data and chemical data
#' @param nplot number of plots to display
#' @param printDendro when TRUE creates a pdf file with the dendrogram
#' @param nplot number of plots to display
#' @param cex font size

#' @return A dendrogram including \code{df_chem} and \code{df_raw} data.

#' @export


"arch_dendro" <- function(df_chem, df_raw, nplot, printDendro = TRUE, cex=0.4){


  df_chem -> x

# transform the numeric variables to log centered ratio

  x.clr <- logcenter.tran(x)

# perform the cluster analysis

  HClust <-  hclust(d = dist(x.clr)^2, method = "cen")

#save in global environment for using cut tree

  assign(".HClust", HClust,.GlobalEnv)

#save the dendrogram

  my_dend <- as.dendrogram(HClust) #format for the dendrogram

#nplot vector indicates the chosen categorical columns to plot

  for (i in nplot)
    {
      #plot options
        my_dend <- set(my_dend, "labels_cex", cex) #labels text size
        par(mar = c(2,2,2,2)) #set legend scale

     #add colors to labels
        labels_colors(my_dend) <-rainbow(nlevels(as.factor(df_raw[,i])))[as.factor(df_raw[,i])][order.dendrogram(my_dend)]

        #MORE COLOR PALETTES: heat.colors, rainbow, terrain.colors, cm.colors, topo.colors

    #create the plot
        plot(my_dend, main= "Hierarchical Clustering", horiz =  FALSE)
        legend("topright",cex = 0.5, legend = unique(as.factor(df_raw[,i])),
        fill = unique(rainbow(nlevels(as.factor(df_raw[,i])))[as.factor(df_raw[,i])]))



     #save the plot
        dendro <-recordPlot(my_dend)

     if (printDendro == TRUE){
       emf("Dendrogram.emf")
       replayPlot(dendro)
       dev.off()
       pdf("Dendrogram.pdf")
       replayPlot(dendro)
       dev.off()
     }
    }

 assign("my_dend", my_dend,.GlobalEnv)
 print(paste("HCA using:", paste(noquote(colnames(df_chem)), collapse = ", ")))
 print(paste("Samples:", nrow(df_chem)))

}
