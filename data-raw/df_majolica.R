# data-raw/df_majolcia.R
# Data import and processing pipeline

if(!require(openxlsx)) install.packages("openxlsx",repos = "http://cran.us.r-project.org")

df_majolica <-  read.xlsx("data-raw/df_majolica_def.xlsx", rowNames = TRUE)

usethis::use_data(df_majolica, overwrite = TRUE, compress = "xz")

