#' Contains auxiliary functions used in other scripts

if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(gridExtra)
packageVersion("gridExtra")
# [1] ‘2.2.1’

if(!require(grid)){
  install.packages("grid",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(grid)
packageVersion("grid")
# [1] ‘3.3.0’

#' Arranges plots in a grid with a shared legend.
#' 
#' Arranges the plots in a grid and adds a shared legend
#' extracted from the first plot passed
#' 
#' @param ... plots to arrange
#' @param ncol (integer) number of columns in the grid
#' @param nrow (integer) number of rows in the grid
#' @param position (character) legend position
#' @return (gtable) with combined plots and a shared legend
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 3, position = c("bottom", "right")) {
  
  # Put the plots in a list
  
  plots <- list(...)
  
  # Get position
  position <- match.arg(position)
  
  # Arrange the legend to the position requested
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  
  # get the legend
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  
  #get legend dimensions
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  
  # Remove legends
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  
  # Prepare arguments for arrangeGrob call
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  # Arrange the plots in a grid and then add the legend according to the position requested
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  # Plot the new graph
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


#' Extracts p-value from a model
#' 
#' @param model (lm) model fitted
#' @return (numeric) p-value
extract_p_value <- function(model) {
  
  # Get f statistic
  fstat <- summary(model)$fstatistic
  
  # Compute p-value
  
  pf(fstat[1],fstat[2],fstat[3],lower.tail = FALSE)
  
}
