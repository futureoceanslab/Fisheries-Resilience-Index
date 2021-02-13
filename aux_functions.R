#' Contains auxiliary functions used in other scripts


if(!require(flextable)){
  install.packages('flextable',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(flextable)
packageVersion("flextable")
# [1] ‘0.4.4’


if(!require(officer)){
  install.packages('officer',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(officer)
packageVersion("officer")
# [1] ‘0.3.0’

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

##### PLOTS #####

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

#' Formats longitud values to be used as labels in a graph.
#' 
#' The format is X ºW for negative longitude values and X ºE por positive longitude values.
#' 0 longitude is formatted as 0º
#' 
#' @param l (integer) longitude
#' @return (character) longitude formatted
longitude_formatter <- function(l) {
  
  
  ifelse(l<0,paste0(as.character(abs(l)),intToUtf8(176),"W"),ifelse(l>0,paste0(as.character(abs(l)),intToUtf8(176),"E"),paste0("0",intToUtf8(176))))
  
}

#' Formats latitude values to be used as labels in a graph.
#' 
#' The format is X ºS for negative latitude values and X ºN por positive latitude values.
#' 0 latitude is formatted as 0º
#' 
#' @param l (integer) latitude
#' @return (character) latitude formatted
latitude_formatter <- function(l) {
  
  
  ifelse(l<0,paste0(as.character(abs(l)),intToUtf8(176),"S"),ifelse(l>0,paste0(as.character(abs(l)),intToUtf8(176),"N"),paste0("0",intToUtf8(176))))
  
}

##### P-VALUES #####

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
#' @param class_column_name (character) the column name with the classes
#' @param x_column (character) column name to be used as x in lm
#' @return (data.frame) with p-values for each class
p_values_for_classes_in_column <- function(data,class_column_name,x_column){
  
  # Get the classes
  
  classes <- data %>% filter_at(vars(one_of(class_column_name)),all_vars(!is.na(.))) %>% pull(class_column_name) %>% unique
  
  # Compute p-values for each class
  
  p_values_class <- classes %>% lapply(function(a_class){
    
    
    # data for dimension used to compute trend line Resilience vs LAT. Note, trend-lin in graphs is computed as LAT ~ Resilience_Index. 
    
    to.compute <- data %>% 
      filter_at(vars(one_of(c(class_column_name,"Resilience_Index",x_column))),all_vars(!is.na(.))) %>% # remove NAs
      filter_at(vars(one_of(class_column_name)),all_vars(.==a_class)) # keep only data for a_class
    
    
    # compute p-values
    
    model_formula <- as.formula(paste("Resilience_Index ~",x_column))
    
    
    model <-  lm(formula=model_formula,data=to.compute) 
    
    p<- extract_p_value(model)
    
    
    result <- data.frame(var=a_class,p=p)
    
    names(result)[1] <- class_column_name
    
    result
    
  })  %>% bind_rows() %>% spread(class_column_name,p)
  
}

#' Computes the p-value of lm(Resilience_Index ~ x_column) for each class
#' in column_name and each x_column in column names
#' 
#' @param data (data.frame) the data
#' @param column_names (character) column names to be used as x in lm
#' @param class_column_name (character) the column name with the classes
#' @return (data.frame) with p-values for each class (columns) and column in column_names (rows)
p_values_for_columns_and_classes_in_column <- function(data,column_names,class_column_name){
  
  p_values_column <- column_names %>% lapply(function(column_name){
    
    # data to plot
    
    to.compute <- data %>% 
      filter_at(vars(one_of(c(column_name,"Resilience_Index"))),all_vars(!is.na(.))) # remove NAs
    
    
    # compute p-values
    
    
    
    
    p_values <- p_values_for_classes_in_column(to.compute,class_column_name,column_name) %>% mutate(Var=column_name) 
    
    
    p_values %>% select(Var,one_of(names(p_values)))
    
  }) %>% bind_rows() %>% data.frame
  
  
}


##### WORD TABLES #####

#' Formats a table to be printed in a word document
#' 
#' @param data (data.frame) data to be formatted
#' @return (FlexTable) table formatted
format_table <- function(data){
  # Prepare the table
  
  # Convert data into a flex table, no rownames
  Ft <- flextable(data) %>% border_remove() %>%
    hline_bottom(border=fp_border(),part="header") %>%
    vline(border=fp_border(),j=1) %>%
    hline_bottom(border=fp_border())
  
  # Table header format: font size 10, italic, center alignment. Only bottom border.
  
  Ft %<>% italic(part="header") %>% 
    fontsize(size=10,part="header") %>%
    align(align="center",part="header") 
  
  #Ft[to="header"] <- textProperties(font.size = 10,font.style = "italic")
  
  # Ft[to="header"] <- parProperties(text.align = "center")
  
  #Ft[to="header"] <- cellProperties(border.right.style =  "none",border.top.style = "none",border.left.style = "none")
  
  # General table format: font size 10, center alignment
  
  Ft %<>% 
    fontsize(size=10,part="all") %>%
    align(align="center",part="all") 
  
  #Ft[] <- textProperties(font.size = 10)
  
  #Ft[] <- parProperties(text.align = "center")
  
  # First column format: font size 10, italic, left alignment. No left border, bottom border only in last column
  
  # Ft[,1] <- textProperties(font.size = 10,font.style   = "italic")
  
  Ft %<>% 
    italic(j=1) %>%
    align(align="right",j=1) 
  
  #Ft[,1] <- parProperties(text.align = "right")
  
  #Ft[1:(nrow(data)),1] <- cellProperties(border.bottom.style =  "none",border.top.style = "none",border.left.style = "none")
  
  #Ft[nrow(data),1] <- cellProperties(border.bottom.style =  "solid",border.top.style = "none",border.left.style = "none")
  
  # Table body format: No borders, odd rows background is grey
  
  #Ft[,2:ncol(data)] <- cellProperties(border.style="none")
  
  Ft %<>% bg(i=seq(1,nrow(data),2),bg="gray90",j=2:ncol(data))
  
  #Ft[seq(1,nrow(data),2),2:ncol(data)] <- cellProperties(background.color = "gray90",border.style="none")
  
  #Ft[nrow(data),2:ncol(data)] <- cellProperties(background.color = ifelse(nrow(data)%%2==0,"white","gray90"),border.bottom.style = "solid",border.right.style =  "none",border.top.style = "none",border.left.style = "none")
  
  
  Ft
}

#' Writes a table to a docx document
#' 
#' @param Ft (FlexTable) table formated using format_table
#' @param title (character) Table title
#' @param outfile (character) file path to save the word document
#' @param landscape (logical) if TRUE, prints the table in a landscape section in the document.
#' @return NULL
write_doc <- function(Ft,title,outfile,landscape=FALSE){
  
  # New document
  doc <- read_docx()
  
  
  # Empty line
  doc %<>% body_add_par("")
  
  # Table title
  
  doc %<>% body_add_par(title,style = "table title")
  
  # Add table  
  
  doc %<>% body_add_flextable(autofit(Ft,add_w = 0))
  
  # Close landscape section if the table is printed landscape
  if(landscape){
    doc %<>% body_end_section_landscape()
  }
  
  # Save the document
  print(doc,target=outfile)
  
  NULL
}

##### TABLE FORMATTING #####

# Order in which counties, species and stocks should appear in the Word tables.
countries_order <- c("BE","DK","DE","EE","IE","ES","FR","LV","LT","NL","PL","PT","FI","SE")
species_order <- c("European hake","Atlantic cod")
stocks_order1 <- c("HAKENRTN","HAKESOTH","CODCOASTNOR_CODNEAR","CODFAPL","CODICE","CODBA2532","CODKAT","CODIS","CODVIa","CODNS")
stocks_order2 <- c("HAKENRTN","HAKESOTH","CODCOASTNOR","CODNEAR","CODFAPL","CODICE","CODBA2532","CODKAT","CODIS","CODVIa","CODNS")

#' Arranges table according to the order of countries, species,and stocks
arrange_table <- function(df,stocks_order=stocks_order1) {
  df %>% 
    mutate(SPECIES=factor(SPECIES,levels=species_order),STOCK=factor(STOCK,levels=stocks_order),COUNTRIES=factor(COUNTRIES,levels=countries_order)) %>% 
    arrange(COUNTRIES,SPECIES,STOCK) %>% 
    mutate(SPECIES=as.character(SPECIES),STOCK=as.character(STOCK),COUNTRIES=as.character(COUNTRIES))
}

#' Replaces long species name with short name
species_sort_name <- function(x) {
  case_when(x=="Atlantic cod" ~ "cod",
            x=="European hake" ~ "hake",
            TRUE ~ x
  )
}  
#' Replaces short species name with long name
species_long_name <- function(x) {
  case_when(x=="cod" ~"Atlantic cod",
            x=="hake" ~"European hake",
            TRUE ~ x
  )
}
##### NORMALIZATION #####

normalize_positive <-function(x) {
  (x - min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))
}

normalize_negative <-function(x) {
  (max(x,na.rm = TRUE)-x)/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))
}



###PARTIAL PLOT 2###

#partialplot function from 
#https://github.com/srisatish/randomForest/blob/master/R/partialPlot.R
#changing col parameter from blue to grey

partialPlot2 <- function(x, ...) UseMethod("partialPlot")

partialPlot.default <- function(x, ...)
  stop("partial dependence plot not implemented for this class of objects.\n")

partialPlot.randomForest <-
  function (x, pred.data, x.var, which.class, w, plot=TRUE, add=FALSE,
            n.pt = min(length(unique(pred.data[, xname])), 51), rug = TRUE,
            xlab=deparse(substitute(x.var)), ylab="",
            main=paste("Partial Dependence on", deparse(substitute(x.var))),
            ...)
  {
    classRF <- x$type != "regression"
    if (is.null(x$forest))
      stop("The randomForest object must contain the forest.\n")
    x.var <- substitute(x.var)
    xname <- if (is.character(x.var)) x.var else {
      if (is.name(x.var)) deparse(x.var) else {
        eval(x.var)
      }
    }
    xv <- pred.data[, xname]
    n <- nrow(pred.data)
    if (missing(w)) w <- rep(1, n)
    if (classRF) {
      if (missing(which.class)) {
        focus <- 1
      }
      else {
        focus <- charmatch(which.class, colnames(x$votes))
        if (is.na(focus))
          stop(which.class, "is not one of the class labels.")
      }
    }
    if (is.factor(xv) && !is.ordered(xv)) {
      x.pt <- levels(xv)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] > 0,
                                              pr[, focus], .Machine$double.eps)) -
                                     rowMeans(log(ifelse(pr > 0, pr, .Machine$double.eps))),
                                   w, na.rm=TRUE)
        } else y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        
      }
      if (add) {
        points(1:length(x.pt), y.pt, type="h", lwd=2, ...)
      } else {
        if (plot) barplot(y.pt, width=rep(1, length(y.pt)), col="grey",
                          xlab = xlab, ylab = ylab, main=main,
                          names.arg=x.pt, ...)
      }
    } else {
      if (is.ordered(xv)) xv <- as.numeric(xv)
      x.pt <- seq(min(xv), max(xv), length = n.pt)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- rep(x.pt[i], n)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] == 0,
                                              .Machine$double.eps, pr[, focus]))
                                   - rowMeans(log(ifelse(pr == 0, .Machine$double.eps, pr))),
                                   w, na.rm=TRUE)
        } else {
          y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        }
      }
      if (add) {
        lines(x.pt, y.pt, ...)
      } else {
        if (plot) plot(x.pt, y.pt, type = "l", xlab=xlab, ylab=ylab,
                       main = main, ...)
      }
      if (rug && plot) {
        if (n.pt > 10) {
          rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
        } else {
          rug(unique(xv, side = 1))
        }
      }
    }
    invisible(list(x = x.pt, y = y.pt))
  }