#' Scatter Matrix.
#'
#' Provides a scatter matrix showing the correlations among different elements of the dataset.
#'
#' @param df_raw Dataframe with the chemical composition and the catagorical values
#' @param vars Variables to use in the scatter matrix (chemical elements or compounds)
#' @param title The title to be displayed
#' @param color Categorical values to be distinguided by colors (e.g. "Region", "Kiln", "Provenance"...)
#' @param color_title The title to be displayed
#' @param shape Categorical values to be distinguided by shapes (e.g. "Region", "Kiln", "Provenance"...)
#' @param shape_title The title to be displayed

#' @return A facet grid scatter matrix showing correlations among selected variables.
#' @export

arch_scatter_matrix <- function(df_raw, vars,title, color, color_title ="Color Title", shape = color, shape_title = "Shape title"){

##based on ggplot and ggthems
#vars
#@example arch_scatter_matrix(df_raw= df_alr, vars, color = "Region", shape = "Kiln", title= "RBPottery")


  ggthemes_data <- ggthemes::ggthemes_data #obtain theme

  x <- c()
  y <- c()
  z <- c()
  w <- c()

  #Prepare the dataframe
  .df <- df_raw[c(vars)] #create a dataset only with the variables to plot (MnO, TiO2,...)
  .grid <- expand.grid(x = 1:ncol(.df), y = 1:ncol(.df)) #create a df with x and y
  .grid <- subset(.grid, x != y) #remove coinciding values

  #Fill the dataframe with the values
  .all <- do.call("rbind", lapply(1:nrow(.grid), function(i) {
    xcol <- .grid[i, "x"];
    ycol <- .grid[i, "y"];
    data.frame(xvar = names(.df)[ycol], yvar = names(.df)[xcol],
               x = .df[, xcol], y = .df[, ycol], .df)
  }))

  .all$xvar  <- factor(.all$xvar, levels = names(.df))
  .all$yvar  <- factor(.all$yvar, levels = names(.df))

  #Calculate the densities
  .densities <- do.call("rbind", lapply(1:ncol(.df), function(i) {
    .tmp <- as.data.frame(density(x = .df[, i])[c("x", "y")]);
    .tmp$y <- .tmp$y/max(.tmp$y)*diff(range(.tmp$x)) + min(.tmp$x);
    data.frame(xvar = names(.df)[i], yvar = names(.df)[i],
               x = .tmp$x, y = .tmp$y)
  }))

  #add a column with the group 1 (column z in .all)
  .all <- data.frame(.all, z = rep(df_raw[,eval(parse(text = "color"))], length = nrow(.all)))

  #add a column with the group 2 (column w in .all)
  .all <- data.frame(.all, w = rep(df_raw[,eval(parse(text = "shape"))], length = nrow(.all)))

  #create the column z in densities (required)
  .densities$z <- NA
  .densities$w <- NA


  plot <-
   ggplot2::ggplot(.all, aes(x = x, y = y, colour = z, shape = w)) + #here are set the color (z) categories and the shape (w)
    facet_grid(xvar ~ yvar, scales = "free") +
    geom_point(aes(colour=z), na.rm = TRUE, alpha=0.8)+
    geom_point(size=0.5) + ##change the point size here
    geom_line(aes(x = x, y = y), data = .densities, colour = "grey") +
    scale_y_continuous(expand = c(0.01, 0)) +

    xlab(NULL) +
    ylab(NULL) +

    labs(colour = color_title, shape = shape_title) +
    labs(title = title) +
    ggthemes::theme_base(base_size = 10, base_family = "sans") +
    theme(legend.position = "right")

    plot(plot)

    ggplot2::ggsave(filename = "scatter_plot.pdf", plot = plot)

  rm(ggthemes_data, .grid, .all, .densities)
}
