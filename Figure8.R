#' This script reads data/final_index.csv and plots
#' Figure 8 in the paper.

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

# SPECIES to plot. Vector names define legend title.

species_to_plot <- c(COD="Cod",HAKE="Hake")

# Note: We are modifying the x-axis for some graphs, but trend lines are computed using all the data available.

graphs <- names(species_to_plot) %>% lapply(function(species_name){
  # species_name <- "HAKE"
  # Species
  
  species <- species_to_plot[species_name]
  
  
  # Filter species data
  
  final_index_species <- final_index %>% filter(SPECIE==species)
  
  # Plot the graphs and save each graph in a list for later
  
  graphs_for_specie <- 1:length(x_labels) %>% lapply(function(i){
    # i <-6
    # Get the column name to plot
    column_name <- names(x_labels)[i]
    
    # Get the x label
    x_label <- x_labels[column_name]
    
    # A letter for the subtitle
    subtitle_letter <- LETTERS[i]
    
    # data to plot
    
    to.plot <- final_index_species %>% 
      filter_at(vars(one_of(c(column_name,"Resilience_Index"))),all_vars(!is.na(.))) # remove NAs
    
    # subset data for trend lines.
    
    
    # Plot
    g <-  ggplot(to.plot, aes_string(column_name, "Resilience_Index", col = "DIMENSION")) +
      geom_point(aes(shape=DIMENSION)) +
      geom_smooth(se = TRUE, method = "lm", fill= "gray77", size= 1)+
      scale_shape_manual(values=c(1,5,6))+
      scale_color_manual(values=c("green4","navy","purple"))+
      ylab("R.I")+
      scale_fill_discrete(name=species_name)+
      xlab(x_label)+
      theme_classic()+
      labs(subtitle = subtitle_letter, color = paste0(species_name,"\n"), shape = paste0(species_name,"\n"))+
      theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"),
            axis.text = element_text(size=12, color="black"),
            axis.title = element_text(size=14, color="black"),
            legend.text = element_text(size=12,color = "black"),
            legend.title = element_text(size=14,color="black"))
    
    # Remove y label on the right side column of graphs
    
    if(i %% 2==0){
      g <- g+ theme(axis.title.y = element_blank())
      
    }
    
    # Limiting x-axis range for three cases
    
    # if(species_name=="HAKE"){
    #   g <-switch (column_name,
    #               GDP.2016 = g + coord_cartesian(xlim=c(28,50)),   
    #               Readiness = g + coord_cartesian(xlim=c(0.58,0.74)),
    #               Vulnerability = g + coord_cartesian(xlim=c(0.300,0.351)),
    #               OHI.eco = g + coord_cartesian(xlim = c(70,100)),
    #               Inclusion.of.Requirements.2010= g + coord_cartesian(xlim = c(17,24)),
    #               g
    #               ) 
    #    
    # }
    
    
    g
  })
  
  
  do.call(grid_arrange_shared_legend,c(graphs_for_specie, list(nrow = 4, ncol = 2)))
  
})



png("Figures/Figure 8.png",width=10,height=20,units="in",res=300)

do.call(grid.arrange,c(graphs, list(ncol = 1)))

dev.off()
