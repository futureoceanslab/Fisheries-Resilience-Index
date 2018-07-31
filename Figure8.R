
#' This script reads data/final_index.csv, plots
#' Figures/Figure 8.png and creates Tables/Fig8_p_values.docx


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

if(!require(magrittr)){
  install.packages('magrittr',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(magrittr)
packageVersion("magrittr")
# [1] ‘1.5’

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

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’

if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(gridExtra)
packageVersion("gridExtra")
# [1] ‘2.2.1’


source("aux_functions.R")

##### 2. READ DATA #####

# Read the data

final_index <- read_csv("data/final_index.csv")


# SPECIES to plot. Vector names define legend title.

species_to_plot <- c(COD="Cod",HAKE="Hake")

# Columns to plot and their x label

x_labels <- c(GDP.2016="GDP 2016",
              OHI.fisheries="OHI fisheries",
              OHI.economic="OHI economic",
              Readiness="Readiness",
              Vulnerability="Vulnerability")



##### 3. P-VALUES #####

# Compute p-values for plot and tables

p_values_species<- names(species_to_plot) %>% lapply(function(species_name){
  # species_name <- "HAKE"
  # Species
  
  species <- species_to_plot[species_name]
  
  
  # Filter species data
  
  final_index_species <- final_index %>% filter(SPECIE==species)
  
  # Compute the p-value for each colum name and create table
  
  table_for_specie <- p_values_for_columns_and_classes_in_column(final_index_species,names(x_labels),"DIMENSION")
    
  
  table_for_specie %<>% mutate(Var=x_labels[Var])
    
  
})


names(p_values_species) <- names(species_to_plot)


##### 4. PLOT ######


graphs <- names(species_to_plot) %>% lapply(function(species_name){
  # species_name <- "HAKE"
  # Species
  
  species <- species_to_plot[species_name]
  
  
  # Filter species data
  
  final_index_species <- final_index %>% filter(SPECIE==species)
  
  # Plot the graphs and save each graph in a list for later
  
  graphs_for_specie <- 1:length(x_labels) %>% lapply(function(i){

    # i <-5

    # Get the column name to plot
    column_name <- names(x_labels)[i]
    
    # Get the x label
    x_label <- x_labels[column_name]
    
    # A letter for the subtitle
    subtitle_letter <- LETTERS[i]
    
    # data to plot
    
    to.plot <- final_index_species %>% 
      filter_at(vars(one_of(c(column_name,"Resilience_Index"))),all_vars(!is.na(.))) # remove NAs

    
    
    
    
    
    # Plot
    g <- ggplot(to.plot, aes_string(column_name, "Resilience_Index", col = "DIMENSION")) +
      geom_point(aes(shape=DIMENSION)) +
      geom_smooth(se = TRUE, method = "lm", fill= "gray77", size= 1,alpha=0.25)+
      scale_shape_manual(values=c(1,5,6))+
      scale_color_manual(values=c("seagreen4","cornsilk3","yellow3"))+
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
    
    
    # plot p-values

  
    # Locate p-values at the bottom center of the graph. y_center may fail with different ggplot versions
    x_center <- to.plot[,column_name] %>% range %>% mean
    y_center <- ggplot_build(g)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]] %>% min
    
    # get the p-values
    p <-  p_values_species[[species_name]] %>% gather(DIMENSION,p,-Var) %>% filter(Var==x_label)  %>% filter(p<0.05) %>% mutate(p=ifelse(p<0.01,"<0.01",sprintf("%0.2f",p))) %>% mutate(x=x_center,y=y_center,hjust=c(1.2,0,-1.2)[1:nrow(.)])

    # plot the p-values
        

    if(nrow(p) >0){
      g<- g + geom_text(data=p,aes(x=x,y=y,label=p,col=DIMENSION,hjust=hjust),show.legend = FALSE,vjust=-0.1) +
      geom_text(label="p-value",col="black",x=x_center,y=y_center,vjust=-0.1,hjust=2)
    }
    # Remove y label on the right side column of graphs
    
    if(i %% 2==0){
      g <- g+ theme(axis.title.y = element_blank())
      
    }
    

    g
  })
  
  #Arrange all panels

  
  do.call(grid_arrange_shared_legend,c(graphs_for_specie, list(nrow = 4, ncol = 2)))
  
})


# Sae the two graphs in one file

tiff("Figures/Figure 8.tiff",width=10,height=20,units="in",res=300, compression = "lzw")

do.call(grid.arrange,c(graphs, list(ncol = 1)))

dev.off()


##### 5. TABLES #####

doc <- read_docx()

for(species_name in names(species_to_plot)){
  
  # Species
  
  species <- species_to_plot[species_name]
  
  title <-  paste0(species,": p-values for trend lines in Fig 8")

  
  # Empty line
  doc %<>% body_add_par("")
  
  # Table title
  
  doc %<>% body_add_par(title,style = "table title")
  
  # Get the p-values
  
 
  table_for_specie <-  p_values_species[[species_name]] %>% gather(DIMENSION,p,-Var) %>% 
    mutate(p=ifelse(p<0.01,"<0.01",sprintf("%0.2f",p))) %>% 
    spread(DIMENSION,p)

  Ft <- format_table(table_for_specie)
  
  # Add table  
  
  doc %<>% body_add_flextable(autofit(Ft,add_w = 0))
  
  
  
}

# Write document

print(doc,target="Tables/Fig8_p_values.docx")

