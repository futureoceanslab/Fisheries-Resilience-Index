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


#' Computes the p-value of lm(Resilience_Index ~ x_column) for each class
#' in column_name
#' 
#' @param data (data.frame) the data
#' @param column_name (character) the column name with the classes
#' @param x_column (character) column name to be used as x in lm
#' @return (data.frame) with p-values for each class
p_values_for_column <- function(data,column_name,x_column){
  
  # Get the classes
  
  classes <- data %>% filter_at(vars(one_of(column_name)),all_vars(!is.na(.))) %>% pull(column_name) %>% unique
  
  # Compute p-values for each class
  
  p_values_class <- classes %>% lapply(function(a_class){
    
    
    # data for dimension used to compute trend line Resilience vs LAT. Note, trend-lin in graphs is computed as LAT ~ Resilience_Index. 
    
    to.compute <- data %>% 
      filter_at(vars(one_of(c(column_name,"Resilience_Index",x_column))),all_vars(!is.na(.))) %>% # remove NAs
      filter_at(vars(one_of(column_name)),all_vars(.==a_class)) # keep only data for a_class
    
    
    # compute p-values
    
    model_formula <- as.formula(paste("Resilience_Index ~",x_column))
    
    
    model <-  lm(formula=model_formula,data=to.compute) 
    
    p<- extract_p_value(model)
    
    
    result <- data.frame(var=a_class,p=p)
    
    names(result)[1] <- column_name
    
    result
    
  })  %>% bind_rows() %>% spread(column_name,p)
  
}

