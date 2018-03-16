#' This script reads data/final_index.csv and plots
#' Figure 3 in the paper.

##### 1. LOAD PACKAGES AND DISPLAY VERSIONS #####

version                           
# platform       x86_64-apple-darwin13.4.0   
# arch           x86_64                      
# os             darwin13.4.0                
# system         x86_64, darwin13.4.0        
# status                                     
# major          3                           
# minor          3.0                         
# year           2016                        
# month          05                          
# day            03                          
# svn rev        70573                       
# language       R                           
# version.string R version 3.3.0 (2016-05-03)
# nickname       Supposedly Educational          

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’

source("aux_functions.R")

##### 2. READ DATA #####

# Read the data

final_index <- read_csv("data/final_index.csv")

##### 3. PLOT ######

# Columns to plot and their x label

x_labels <- c(GDP.2016="GDP 2016",
              OHI.2016="OHI 2016",
              OHI.eco="OHI economic 2016",
              Tech..develop..2013="Technical Development (number per country)",
              Inclusion.of.Requirements.2010="Compilance (scores)",
              Readiness="Readiness",
              Vulnerability="Vulnerability")



# Plot the graphs and save each graph in a list for later

graphs <- 1:length(x_labels) %>% lapply(function(i){
  # i <-1
  # Get the column name to plot
  column_name <- names(x_labels)[i]
  
  # Get the x label
  x_label <- x_labels[column_name]
  
  # A letter for the subtitle
  subtitle_letter <- LETTERS[i]
  
  # Define point shapes
  point_shapes <- case_when(column_name=="Inclusion.of.Requirements.2010" ~ c(c(1,2)),
                      TRUE ~ c(16,1))
  # data to plot
  
  to.plot <- final_index %>% 
    filter_at(vars(one_of(c(column_name,"Resilience_Index"))),all_vars(!is.na(.))) # remove NAs
  
  # subset data for trend lines.
  
  not.cod.data <- to.plot %>% filter(SPECIE!="Cod")
  not.hake.data <- to.plot %>% filter(SPECIE!="Hake")
  
  # Plot
  g <- ggplot (to.plot, aes_string(column_name, "Resilience_Index", col = "SPECIE", linetype = "SPECIE")) +
    geom_point(aes(shape=SPECIE)) +
    geom_smooth(data = not.cod.data,method = lm, se = TRUE, linetype = "dotted")+
    geom_smooth(data = not.hake.data,method = lm, se = TRUE)+
    scale_shape_manual(values=point_shapes)+
    scale_color_manual(values=c("steelblue","steelblue"))+
    ylab("R.I")+
    xlab(x_label)+
    labs(subtitle = subtitle_letter)+
    theme_classic()+
    theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black"),
          axis.text = element_text(size=12, color="black"),
          axis.title = element_text(size=14, color="black"),
          legend.text = element_text(size=12,color = "black"),
          legend.title = element_text(size=14,color="black"))
  g
})


# Arrange in grid with shared legend and save to file.

png("Figures/Figure 3.png",width=9.38,height=7.99,units="in",res=300)

do.call(grid_arrange_shared_legend,c(graphs, list(nrow = 4, ncol = 2)))

dev.off()


