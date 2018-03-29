#' This script reads data/final_index.csv and plots
#' Figures/Figure 3.png

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

# Columns to plot and their x label

x_labels <- c(GDP.2016="GDP 2016",
              OHI.2016="OHI 2016",
              OHI.eco="OHI economic 2016",
              Tech..develop..2013="Technical Development (number per country)",
              Inclusion.of.Requirements.2010="Compilance (scores)",
              Readiness="Readiness",
              Vulnerability="Vulnerability")

##### 3. p-values #####


p_values <- p_values_for_columns_and_classes_in_column(final_index,names(x_labels),"SPECIE")



##### 4. PLOT ######





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
  
  cod.data <- to.plot %>% filter(SPECIE=="Cod")
  hake.data <- to.plot %>% filter(SPECIE=="Hake")
  
  # Plot
  g <- ggplot(to.plot, aes_string(column_name, "Resilience_Index", col = "SPECIE", linetype = "SPECIE")) +
    geom_point(aes(shape=SPECIE)) +
    geom_smooth(data = hake.data,method = lm, se = TRUE, linetype = "dotted")+
    geom_smooth(data = cod.data,method = lm, se = TRUE)+
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
  
  # Locate p-values at the top left of the graph.
  x_center <- to.plot[,column_name] %>% min(na.rm=TRUE)
  y_center <- to.plot[,"Resilience_Index"] %>% max(na.rm=TRUE) %>% add(0.1)
  
  
  # get the p-values
  p <-  p_values %>% filter(Var==column_name) %>% select(-Var) %>% gather(var,p) %>% filter(p<0.05) %>% mutate(p=ifelse(p<0.01,"<0.01",sprintf("%0.2f",p))) %>% mutate(x=x_center,y=y_center,hjust=c(-2,-3.2)[1:nrow(.)])
  
  names(p)[1] <- "SPECIE"
  
  # plot the p-values
  
  
  if(nrow(p) >0){
    g<- g + geom_text(data=p,aes_string(x="x",y="y",label="p",col="SPECIE",hjust="hjust"),show.legend = FALSE,vjust=0.5) +
      geom_text(label="p-value",col="black",x=x_center,y=y_center,vjust=0.5,hjust=0)
    
  }
  
  g
  
  
})


# Arrange in grid with shared legend and save to file.

png("Figures/Figure 3.png",width=9.38,height=7.99,units="in",res=300)

do.call(grid_arrange_shared_legend,c(graphs, list(nrow = 4, ncol = 2)))

dev.off()


