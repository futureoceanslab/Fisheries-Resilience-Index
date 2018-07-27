#' This script reads data/sp.csv and plots
#' Fig dimensions.png in the paper.

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

sp <- fread("data/final_index.csv") %>% select(COUNTRIES,DIMENSION,Resilience_Index,SPECIE) %>% 
  mutate(DIMENSION=c(ecological="Resilience_Index_E",socioeconomic="Resilience_Index_S",institutional="Resilience_Index_I")[DIMENSION]) %>%
  spread(DIMENSION,Resilience_Index)

 #<- read_csv("data/sp.csv")


##### 3. PLOT #####

# Define the grahps: xvar, y var, title, x-axis label, y-axis label for each graph

graph_defs <- matrix(c(
  "Resilience_Index_S","Resilience_Index_I","RESILIENCE INDEX socioeconomic-institutional","Resilience Index S","Resilence Index I",
  "Resilience_Index_E","Resilience_Index_I","RESILIENCE INDEX ecological-institutional","Resilience Index E","Resilence Index I",
  "Resilience_Index_E","Resilience_Index_S","RESILIENCE INDEX ecological-socioeconomic","Resilience Index E", "Resilence Index S"
        ),ncol=5,byrow = TRUE) %>% data.frame(stringsAsFactors = FALSE)

names(graph_defs) <- c("x","y","title","xlab","ylab")

# Plot the graphs

graphs <- 1:nrow(graph_defs) %>% lapply(function(i){
  
  
  
  x_var <- graph_defs[i,"x"] # x variable
  y_var <- graph_defs[i,"y"] # y variable
  title <- graph_defs[i,"title"] # title
  xlab <- graph_defs[i,"xlab"] # x label
  ylab <- graph_defs[i,"ylab"] # y label
  
  # Plot
  
  g <- ggplot (sp, aes_string(x=x_var, y=y_var , col = "SPECIE", linetype = "SPECIE")) +
    geom_point(aes(shape=SPECIE)) +
    geom_smooth(se = TRUE, method = lm)+
    scale_shape_manual(values=c(16,1))+
    scale_color_manual(values=c("steelblue","steelblue"))+
    ggtitle(title) +
    xlab(xlab)+
    ylab(ylab)+
    theme_classic()+
    theme(plot.title = element_text(size=13, face="bold", color="black"),
          axis.text = element_text(size=12, color="black"),
          axis.title = element_text(size=14, color="black"),
          legend.text = element_text(size=12,color = "black"),
          legend.title = element_text(size=14,color="black"))
  
  g
  
})

# Arrange in grid with shared legend and save to file.

tiff("Figures/Fig dimensions.tiff",width=9.5,height=8,units="in",res=300)

do.call(grid_arrange_shared_legend,c(graphs, list(nrow = 2, ncol = 2)))

dev.off()

