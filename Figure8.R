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

if(!require(magrittr)){
  install.packages('magrittr',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(magrittr)
packageVersion("magrittr")
# [1] ‘1.5’

if(!require(ReporteRs)){
  install.packages('ReporteRs',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(ReporteRs)
packageVersion("ReporteRs")
# [1] ‘0.8.8’

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

    
    
    
    
    
    # Plot
    g <- ggplot(to.plot, aes_string(column_name, "Resilience_Index", col = "DIMENSION")) +
      geom_point(aes(shape=DIMENSION)) +
      geom_smooth(se = TRUE, method = "lm", fill= "gray77", size= 1,alpha=0.25)+
      scale_shape_manual(values=c(1,5,6))+
      scale_color_manual(values=c("yellow3","seagreen4","cornsilk3"))+
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
    
    
    
    model_formula <- as.formula(paste("Resilience_Index ~",column_name))
    
    dimension_names <- to.plot$DIMENSION %>% unique %>% sort
    
    x_center <- to.plot[,column_name] %>% range %>% mean
    y_center <- ggplot_build(g)[["layout"]][["panel_ranges"]][[1]][["y.range"]] %>% min
    
    
    p <-   dimension_names %>% lapply(function(x){
      
      # x <- "institutional"
      d <- to.plot %>% filter(DIMENSION==x)
      
      model <-  lm(formula=model_formula,data=d) 
      
      p<- extract_p_value(model)
      
      
      data.frame(DIMENSION=x,p=p)
      
    }) %>% bind_rows()   %>% filter(p<0.05) %>% mutate(p=ifelse(p<0.01,"<0.01",sprintf("%0.2f",p))) %>% mutate(x=x_center,y=y_center,hjust=c(1.2,0,-1.2)[1:nrow(.)])
    
    
    
    
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
  
  
  do.call(grid_arrange_shared_legend,c(graphs_for_specie, list(nrow = 4, ncol = 2)))
  
})



png("Figures/Figure 8.png",width=10,height=20,units="in",res=300)

do.call(grid.arrange,c(graphs, list(ncol = 1)))

dev.off()


##### 4. TABLES #####

doc <- docx()

for(species_name in names(species_to_plot)){
  # species_name <- "HAKE"
  # Species
  
  species <- species_to_plot[species_name]
  
  
  # Filter species data
  
  final_index_species <- final_index %>% filter(SPECIE==species)
  
  # Plot the graphs and save each graph in a list for later
  
  table_for_specie <- 1:length(x_labels) %>% lapply(function(i){
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
    
    
    
    
    
    
  
    # compute p-values
    
    
    
    model_formula <- as.formula(paste("Resilience_Index ~",column_name))
    
    dimension_names <- to.plot$DIMENSION %>% unique %>% sort
    
    
    
    p <-   dimension_names %>% lapply(function(x){
      
      # x <- "institutional"
      d <- to.plot %>% filter(DIMENSION==x)
      
      model <-  lm(formula=model_formula,data=d) 
      
      p<- extract_p_value(model)
      
      
      data.frame(Var=x_labels[column_name],DIMENSION=x,p=p)
      
    }) %>% bind_rows() %>% mutate(p=ifelse(p<0.01,"<0.01",sprintf("%0.2f",p))) %>% spread(DIMENSION,p)
    
    
    
    
    
    
    p
    
  }) %>% bind_rows() %>% data.frame
  
  
  
  doc %<>% addParagraph("")
  
  title <-  paste0(species,": p-values for trend lines in Fig 8")
  
  doc %<>% addParagraph(title,stylename = "En-tte")
  
  
  Ft <- FlexTable(table_for_specie,add.rownames = FALSE)
  
  Ft[to="header"] <- textProperties(font.size = 12,font.weight = "bold")
  Ft[to="header"] <- parProperties(text.align = "center")
  
  
  Ft[] <- textProperties(font.size = 12)
  
  Ft[,1] <- textProperties(font.size = 12,font.weight = "bold")
  
  Ft[] <- parProperties(text.align = "center")
  
  Ft[,1] <- parProperties(text.align = "left")
  
  
  doc %<>% addFlexTable(Ft,offx=-1)
  
  #footer <- paste0("Notes: All models include year fixed effects.  Reported results are the predicted change in the probability of reporting poor health for a ",units," change in the state ",measure," [the sum of β(",measure,") + β(",measure," * income quintile)].  In the baseline model, for example, a ",units," increase in the state ",measure," predicts an increase of ",export.tbl[[i]][1,2] %>% strsplit("\\n") %>% `[[`(1) %>% `[`(1) %>% as.numeric()*100," percentage points in the probability of reporting poor health among people in the poorest income quintile in their state.  Robust 95% confidence intervals. N = ",export.tbl[[i]][6,2],". ")
  
  
  
  #doc %<>% addParagraph(pot(footer,format = textProperties(font.size=10)),stylename = "Pieddepage")
  
  
  
  
  
}

writeDoc(doc,file=paste0("Tables/Fig8_p_values.docx"))
