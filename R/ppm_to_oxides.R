#' Conversion from ppm of elemental form to oxides wt\% values

#' @param df_chem A dataframe with chemical data in ppm
#'
#' @param round A number of digits to use in the rounding
#'
#' @return A \code{dataframe} with the oxide \%wt values converted (only for major elements)
#' @export
#'


"ppm_to_oxides" <- function(df_chem, round= 2){

  df_chem -> df_ppm

  Major = c("Al","NA","NA","NA","NA","NA","NA","Ca","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","Fe","NA","NA","NA","NA","NA","NA","K","NA","NA","NA","Mg","NA","Mn","NA","NA","Na","NA","NA","NA","P","NA","NA","NA","NA","NA","NA","Si","NA","NA","NA","NA","NA","NA","NA","Ti","NA","NA","NA","NA","NA","NA","NA","NA","NA")

  ppm = c("Al","As","Au","B","Ba","Be","C","Ca","Ce","Ce","Co","Cr","Cs","Cu","Dy","Er","Eu","Eu","Fe","Fe","Ga","Gd","Ge","H","Hf","Ho","K","La","Li","Lu","Mg","Mn","Mn","Mn","Mo","Na","Nb","Nd","Ni","P","Pb","Pr","Rb","S","Sb","Sc","Si","Sm","Sn","Sr","Ta","Tb","Th","Ti","Ti","Tm","U
","U1","V","W","Y","Yb","Zn","Zr")

  Oxide_form = c("Al2O3","As2O3","Au2O","B2O3","BaO","BeO","CO2","CaO","CeO2","Ce2O3","CoO","Cr2O3","Cs2O","CuO","Dy2O3","Er2O3","EuO","Eu2O3","FeO","Fe2O3","Ga2O3","Gd2O3","GeO2","H2O","HfO2","Ho2O3","K2O","La2O3","Li2O","Lu2O3","MgO","MnO","MnO2","Mn3O4","MoO3","Na2O","Nb2O5","Nd2O3","NiO","P2O5","PbO","Pr2O3","Rb2O","SO3","Sb2O3","Sc2O3","SiO2","Sm2O3","SnO2","SrO","Ta2O5","Tb2O3","ThO2","TiO2","Ti2O3","Tm2O3","UO2","U3O8","V2O5","WO3","Y2O3","Yb2O3","ZnO","ZrO2")

  Conversion_Factor = c(0.529251, 0.7574, 0.961, 0.310551, 0.89566, 0.36032, 0.272916, 0.714701, 0.814089, 0.85377, 0.786483, 0.684202, 0.943226, 0.798865, 0.871318, 0.87452, 0.904742, 0.86361, 0.777311, 0.699433, 0.743925, 0.867591, 0.694051, 0.111694, 0.847979, 0.872973, 0.830147, 0.85268, 0.46457, 0.879383, 0.603036, 0.774457, 0.63193, 0.720304, 0.6665, 0.741857, 0.699044, 0.857351, 0.785845, 0.436421, 0.928318, 0.854469, 0.914412, 0.400459, 0.83534, 0.65196, 0.467439, 0.86239, 0.78765, 0.845595, 0.818967, 0.868803, 0.878809, 0.599508, 0.666211, 0.875609, 0.881498, 0.848002, 0.560166, 0.793, 0.78744, 0.878201, 0.803397, 0.740309)

  cf_df <- data.frame(Major, ppm, Oxide_form, Conversion_Factor)


  for (i in cf_df[,1]){
    if(length(grep(i, colnames(df_ppm)))>0){ #look for the oxide in column 1, if match = length > 1
      col_ox <- grep(i, colnames(df_ppm)) #grep the position of the column in df_ppm
      row_ox <- grep(i,cf_df[,1])
      df_ppm[col_ox] <- round(df_ppm[col_ox] / cf_df[row_ox,4] /10000,digits = round)

      colnames(df_ppm)[which(names(df_ppm) == "Al")] <- "Al2O3"
      colnames(df_ppm)[which(names(df_ppm) == "Fe")] <- "Fe2O3"
      colnames(df_ppm)[which(names(df_ppm) == "K")] <- "K2O"
      colnames(df_ppm)[which(names(df_ppm) == "P")] <- "P2O5"
      colnames(df_ppm)[which(names(df_ppm) == "Na")] <- "Na2O"
      colnames(df_ppm)[which(names(df_ppm) == "Ti")] <- "TiO2"
      colnames(df_ppm)[which(names(df_ppm) == "Ca")] <- "CaO"
      colnames(df_ppm)[which(names(df_ppm) == "Mn")] <- "MnO"


      print(i)
    }
  }
  return(df_ppm)
}



