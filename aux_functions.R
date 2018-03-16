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
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}