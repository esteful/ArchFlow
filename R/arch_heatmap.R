
#' Heatmap of the chemical composition.
#'
#' Provides a heatmap showing the chemical outlying values.

#' @param df_chem chemical data


#' @return A heatmap that is interactive in the html version.
#' @import d3heatmap
#' @export



"arch_heatmap" <- function(df_chem){

  # Heatmap of the chemical composition ####
    mat_div <- t(t(df_chem)/colMeans(df_chem))

  #get the relative values of each composition
    d3heatmap::d3heatmap(mat_div,       #colors are based on these values
            cellnote=df_chem,       #show the raw chemical data
            labRow= row.names(df_chem),width = 1050, height = 900)   #the names of the labels.
}
