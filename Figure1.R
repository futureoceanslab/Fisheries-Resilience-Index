#' This script reads data/final_index.csv and plots
#' Figure 1 in the paper.
#' 
#' NOTE: As of 2018-03-21, CRAN version of ggplot2 does not include geom_sf.
#' If you run the script and get an error that says that geom_sf is not found, then you
#' need the dev version in github. See bellow to see how to install the github version.


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

if(!require(sf)){
  install.packages("sf",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(sf)
packageVersion("sf")
# [1] ‘0.6.0’

# NOTE: As of 2018-03-21, CRAN version of ggplot2 does not include geom_sf.
# If you run the script and get an error that says that geom_sf is not found. Then uncomment and run
# the next lines

# if(!require(devtools)){
#   install.packages("devtools",dependencies = TRUE,repos='http://cran.us.r-project.org')
# }
# require(devtools)
# install_github("tidyverse/ggplot2") 



if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’


source("aux_functions.R")


##### 2. PREPARE MAP #####

download.file(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip",
              destfile = "data/world.zip", mode = "wb")

unzip("data/world.zip", exdir = "data")

world_area <- sf::st_read("data/TM_WORLD_BORDERS_SIMPL-0.3.dbf", quiet = TRUE) ## sf:: es para que sepa que la funcion es del package sf, sino sale masked como error.
class(world_area)        

file.remove(c("data/Readme.txt","data/world.zip",list.files("data",pattern = "TM_WORLD.*",full.names = TRUE)))

a<-filter(world_area, grepl('EG|AL|AD|AT|BY|BE|BA|BG|HR|CY|CZ|DK|EE|FO|FI|FR|DE|GI|GR|HU|IS|IE|IM|IT|RS|LV|LI|LT|LU|MK|MT|MD|MC|ME|NL|NO|PL|PT|RO|SM|RS|SK|SI|ES|SE|CH|UA|GB|VA|RS|MA|TN|DZ|LY|EH', ISO2)) %>% #get the dat with your 3 cuntries per caracter variable with a name
  mutate(value = AREA*SUBREGION) %>% rename(COUNTRIES=ISO2)  ## add a column called "value" multipliying area*region

##### 3. READ DATA #####

final_index <- read_csv("data/final_index.csv")

##### 4. PLOT #####

# Plot one species per panel

graphs <- c("COD","HAKE") %>% lapply(function(specie_name){
  
  # Filter to keep data for one specie, then merge with map data
  
  to.plot <- final_index %>% 
    arrange(COUNTRIES,DIMENSION)  %>% 
    mutate(SPECIE=toupper(SPECIE))  %>%  
    filter(SPECIE==specie_name) %>% 
    group_by(COUNTRIES) %>%
    summarise(Resilience_Index=mean(Resilience_Index,na.rm = TRUE)) %>%
    ungroup() %>%
    merge(a, ., by="COUNTRIES", all.x=T) 
  
  # Plot
  
  ggplot(to.plot) +
    geom_sf(aes(fill = Resilience_Index)) +
    scale_colour_brewer() +
    coord_sf(xlim = c(-30,30), ylim = c(29,70)) +
    scale_fill_distiller(type = "seq", direction = 1, name="Resilience Index")+
    labs(subtitle = specie_name)+
    theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
          panel.grid.major = element_line(color="white"),
          legend.text = element_text(size=12,color = "black"),
          legend.title = element_text(size=14,color="black"))
  
  
})

# Arrange all the panels in one graph with a common legend

png("Figures/Figure 1.png",width=9,height=7,units="in",res=300)

do.call(grid_arrange_shared_legend,c(graphs, list(nrow = 1, ncol = 2)))

dev.off()


