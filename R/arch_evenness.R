#' Chemical Evenness of the dataset.
#'
#' Provides chemical evenness plot, dendrogram of compositional variation matrix (MVC) and CoDa Dendrogram.
#' Code developed from the original code from J.Buxeda i Garrigos.
#' Based on the observations from Aitchison and Buxeda on compositional data.
#'
#'
#' @param df_chem a dataframe only with chemical data
#'
#' @return Provides evenness plot, MVC dendrogram and CoDa Dendrogram based on \code{df_chem} data.
#'
#' @import compositions
#' @import ggplot2 ggthemes devEMF
#'
#' @references Aitchison, J. (1986). The Statistical Analysis of Compositional Data.
#'     \url{https://doi.org/10.1007/978-94-009-4109-0}
#'     Buxeda i Garrigós, J., & Kilikoglou, V. (2003).
#'     Total Variation as a Measure of Variability in Chemical Data Sets. Patterns and Process.
#'     A Festchrift in Honor to Dr. Edward Sayre, March, 185–198.
#' @export





"arch_evenness" <- function(df_chem)

{
  # number of columns
  n_variables <- ncol(df_chem)

  # create a matrix with column number and equal row number
  varmat <- matrix(0, n_variables, n_variables)

  # same matrix with 4 more rows
  varmat2<-matrix(0, n_variables + 4, n_variables)

  #Fill the matrix with values

  # calculate the diagonal matrix with the variances of the
  # log x divided by the value of the corresponding variable
  for(i in 1:n_variables) {
    varmat[, i] <-
      diag(var(log(df_chem/df_chem[, i])))
  }

  #calculate total variation
  totvar <- sum(varmat)/(2 * n_variables)

  #"t.i": the sum of the individual variabilities for each value in a given column
  varsum  <- apply(varmat, 2, sum)

  # vt/t.i (ratio of total variation and individual variation)
  varprop <- totvar/varsum

  #"r v,t" calculate the correlation between individual and total variation
  varcor  <- vector(mode="numeric", length= n_variables)


  #Correlation vector

  for(i in 1:n_variables) {
    varcor[i] <-
      compositions::cor(varmat[-c(i),i], varsum[-i])
  }

  #add values to the empty matrix
  for(i in 1:n_variables)
  {
    varmat2[i,]  <-  varmat[i,]  #add previously calculated values
    varmat2[n_variables+1,]<-  varsum      #add the sum of all variabilities
    varmat2[n_variables+2,]<-  varprop     #add the ratio of total variation and individual variation
    varmat2[n_variables+3,]<-  varcor      #add correlation values
    varmat2[n_variables+4,1]<- totvar      #add the total variation, only one value in the first column
  }

  #set the names
  dimnames(varmat2) <-
    list(c(dimnames(df_chem)[[2]],"t.i","vt/t.i","r v,t","vt"),c(dimnames(df_chem)[[2]]))

  #vt/t.i values by order
  ord <- order(varprop)

  #get the name of the less varying variable (disable)
  #colnames(df_chem)[ord[n_variables]] -> lvar

  #set names to show after in the MVC plot
  names(varsum) <-  colnames(df_chem) #
  varsum[order(varsum, decreasing = T)] -> varsum_ordered_vec
  as.data.frame(varsum_ordered_vec) -> df_varsum

  ##For MVC plot (set oxide names when these are in the dataset)
  for (i in 1:length(row.names(df_varsum))) {
    if (regexpr ("^Fe2O3$",as.character(row.names(df_varsum)[i]))==T) row.names(df_varsum)[i]<-expression("Fe"["2"]*"O"["3"])
    if (regexpr ("^Al2O3$", as.character(row.names(df_varsum)[i]))==T) row.names(df_varsum)[i]<-expression("Al"["2"]*"O"["3"])
    if (regexpr ("^P2O5$", as.character(row.names(df_varsum)[i]))==T) row.names(df_varsum)[i]<-expression("P"["2"]*"O"["5"])
    if (regexpr ("^TiO2$", as.character(row.names(df_varsum)[i]))==T) row.names(df_varsum)[i]<-expression("TiO"["2"])
    if (regexpr ("^Na2O$", as.character(row.names(df_varsum)[i]))==T) row.names(df_varsum)[i]<-expression("Na"["2"]*"O")
    if (regexpr ("^K2O$", as.character(row.names(df_varsum)[i]))==T) row.names(df_varsum)[i]<-expression("K"["2"]*"O")
    if (regexpr ("^SiO2$", as.character(row.names(df_varsum)[i]))==T) row.names(df_varsum)[i]<-expression("SiO"["2"])
  }

  row.names(df_varsum) <- sub(pattern = "Fe2O3",replacement =  expression("Fe"["2"]*"O"["3"]), x=row.names(df_varsum))






  ##For MVC plot and its cuts, and also MVC dendro
  n_varplus1 <-n_variables + 1            # all variables + 1
  mat <- matrix(0, n_varplus1,1)       # a matrix with all variables + 1

  #Create a dataframe
  MVC<-as.data.frame(varmat2)               #convert varmat2 matrix to data frame
  tMVC<-as.data.frame(t(MVC))               #transpose the dataframe


  ordered_varsum_vec<-order(varsum, decreasing=T) #order the variables according to the varsum decreasingly and get

  tMVC<-as.data.frame(tMVC[ordered_varsum_vec,])  #save the matrix ordered as dataframe


  #MVC plot

  for(i in 1:(n_variables)) {
    mat[i,1] <- tMVC[i, n_varplus1]
  }

  mat[n_varplus1,1] <- MVC[n_varplus1+3,1]  #total variation (vt)
  dimnames(mat)[[1]]<-c(dimnames(tMVC)[[1]], "vt")


  #calculate h2, h2p using arch_entropy function
  tmat <- t(mat[c(1:n_varplus1-1),])
  tmatentro <- arch_entropy(tmat-mat[n_varplus1,1]) #entropia function

    #information entropy
    h2 <- round(tmatentro$Entropy[1,n_varplus1],digits=2)

    #percentage of information entropy over the maximum possible
    h2p <- round(tmatentro$Entropy[1,n_varplus1+1]*100,digits=2)

  #total variation
  vt<-round(mat[n_varplus1,1],digits=2)

  #add doted lines
  cuts=c(0.3, 0.5, 0.9) #vertical doted axis
  vec3<- length(cuts)            # 3
  matrix3<- matrix(0,vec3,1)     # (0,3,1)

  for (i in 1:vec3) {
    matrix3[i,1] <-
      length(which(MVC[n_varplus1+1,]<cuts[i])) #matrix with n observation below cuts
  }




  ##CREATE THE MVC PLOT

  MVC_plot <-
    ggplot2::ggplot(df_varsum,
            ggplot2::aes(x=row.names(df_varsum),  #especify data to plot x axis
               y=varsum_ordered_vec)) +  #especify data to plot y axis
    ggplot2::geom_line(ggplot2::aes(group=1)) + #add the line between dots
    ggplot2::geom_point(size=3, shape=20) +  #add the dots (change size or shape)

    #change theme
    #theme_few(base_size = 7, base_family = "sans")

    #adjust margin
     ggplot2::theme(plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"), text = ggplot2::element_text(size=ggplot2::rel(4))) +


    ggplot2::scale_x_discrete(limits = c(row.names(df_varsum)), #order elements from max to min varsum
                    labels = arch_chemLabels(names(varsum_ordered_vec))) + #include labels with subscripts using chemLabels function
     ggplot2::theme(axis.text.x =  ggplot2::element_text(angle = 90)) + #rotate x labels

    #use this to add padding between the last element and right part of the plot
    #expand_limits(x = 30) +

    #optional: use this to include labels over the dots instead of the x axis
        #geom_text(
         # data = df_varsum,
          #label = row.names(df_varsum),
          #parse = TRUE,
          #nudge_x = 0.5, #change this for label position in x axis
          #nudge_y = (ifelse(test = nrow(df_chem)>20, #change this for label position in y axis
          #                  yes = ncol(df_chem)*0.03,
          #                  no = 0.05)),  #if more than 20 individuals change the label position
          #check_overlap = FALSE,
          #na.rm = FALSE,
          #show.legend = NA,
          #inherit.aes = TRUE,
          #angle = 69 #change the angle of the labels
        #) +

    ggplot2::theme(plot.title =  ggplot2::element_text(hjust = 0, size = ggplot2::rel(4)))+  #Graphic title
    ggplot2::ggtitle(paste0("Data (n = ", nrow(df_chem),")", collapse = NULL))+  #paste0 removes the space

    ggplot2::labs(x = NULL,
         y = expression(tau[.i]),
         subtitle = bquote(""*"H"[2]*" = "*.(h2)*" Sh   " ~
                            "H"[2]*" % = "*.(h2p) ~
                             "    "*"vt = "*.(vt))) +
          ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 1, size = 12)) +



    ##Add data and dotted lines to the MVC plot

    #0.3 line + text
      ggplot2::annotate("segment", x = matrix3[1]+0.5,
                          xend = matrix3[1]+0.5,
                          y = 0.3,
                          yend = max(varsum_ordered_vec)/1.5,
                          linetype="dashed",
                          colour = "black",
                          size =0.3) +
      ggplot2::annotate("text", x = matrix3[1]+0.5, y = 0, label = "0.3")+

    #0.5 line + text
      ggplot2::annotate("segment", x = matrix3[2]+0.5,
                          xend = matrix3[2]+0.5,
                          y = 0.3,
                          yend = max(varsum_ordered_vec)/1,5,
                          linetype="dashed",
                          colour = "black",
                          size =0.3) +
      ggplot2::annotate("text", x = matrix3[2]+0.5, y = 0, label = "0.5") +


    #0.9 line + text
      ggplot2::annotate("segment", x = matrix3[3]+0.5,
                          xend = matrix3[3]+0.5,
                          y = 0.3,
                          yend = max(varsum_ordered_vec)/1.5,
                          linetype="dashed",
                          colour = "black",
                          size =0.3) +
      ggplot2::annotate("text", x = matrix3[3]+0.5, y = 0, label = "0.9") +

    # horizontal line vt
      ggplot2::annotate("segment",
                x = 0,
                xend =dim(df_varsum)[1],
                y = min(df_varsum),
                yend = min(df_varsum),
                linetype="dashed", colour = "black", size =0.3)


  #save the plot
  ggplot2::ggsave("MVC_plot.pdf",MVC_plot)

  #visualize the plot
  plot(MVC_plot)



  ############################    2 DENDROGRAM OF MVC  ######################################################

  dendroMVC<-hclust(as.dist(MVC[c(1:(dim(MVC)[2])),]),"ave") #hclust with average method

  for (i in 1:(dim(MVC)[2])) {
    if (regexpr ("^Fe2O3$",as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Fe"["2"]*"O"["3"])
    if (regexpr ("^Al2O3$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Al"["2"]*"O"["3"])
    if (regexpr ("^P2O5$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("P"["2"]*"O"["5"])
    if (regexpr ("^TiO2$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("TiO"["2"])
    if (regexpr ("^Na2O$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Na"["2"]*"O")
    if (regexpr ("^K2O$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("K"["2"]*"O")
    if (regexpr ("^SiO2$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("SiO"["2"])
  }

  #plot
  plot(as.dendrogram(dendroMVC), main= "MVC Dendrogram")

  #save the plot as pdf
  MVCdendro <-recordPlot(dendroMVC)
  pdf("MVC_Dendrogram.pdf")
  replayPlot(MVCdendro)
  dev.off()


  ##############################################################################################################



  ############################    3. CoDadendrogram   #######################################################


  ##CoDadendrogram

  mida.boxplot= 30 #for coDadendro
  rang.boxplot=c(-4,4) #for coDadendro


  classi<- arch_classify(dendroMVC) #arch_clasify function is external

  Signary<- as.data.frame(t(classi$signary))

  dimnames(Signary)[[1]]<-dimnames(df_chem)[[2]]

  df_acomp <- compositions::acomp(df_chem)

  #compositions::CoDaDendrogram(X = df_acomp, signary=Signary, border="red4",col="goldenrod3",type="boxplot",box.space=mida.boxplot, range=rang.boxplot)



  #save the plot of CoDaDendrogram
    #CodaDendroPlot<- recordPlot()
    #MVCdendro <- recordPlot(CodaDendroPlot)
    #pdf("CoDaDendrogram.pdf")
    #replayPlot(CodaDendroPlot)
    #dev.off()

  #To save the nummeric data (enable/disable with CodaDendrogram)
  bases <- compositions::gsi.buildilrBase(Signary)
  Fitxer_ilr <- compositions::ilr(df_chem,bases)
  par(mar=c(5,4,4,2)+0.1,mgp=c(3,1,0))
  list(MVC=MVC,
       Entropia=tmatentro$Entropia,
       Probabilitat= tmatentro$Probabilitat,
       Signary= Signary,
       Bases= bases,
       Fitxer_ilr= Fitxer_ilr)


  ####################################################################################################


}



