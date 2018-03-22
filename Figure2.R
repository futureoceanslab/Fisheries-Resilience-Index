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

if(!require(sf)){
  install.packages("sf",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(sf)
packageVersion("sf")
# [1] ‘0.6.0’

require(ggplot2)
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’


source("aux_functions.R")

library(grid)
library(gridExtra)
library(sf)
library(ggplot2)
library(devtools)
library(tidyverse)
library(RColorBrewer)
##### 2. WORLD MAP #####

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


plot_definition <- data.frame(DIMENSION=c("institutional","institutional","socioeconomic","socioeconomic","ecological","ecological"),SPECIES=c("Cod","Hake","Cod","Hake","Cod","Hake"),stringsAsFactors = FALSE)

graphs <- 1:nrow(plot_definition) %>% lapply(function(i){
  # i <- 1
  dimension <- plot_definition$DIMENSION[i]
  specie <- plot_definition$SPECIES[i]

  to.plot <-   final_index  %>% filter(DIMENSION==dimension,SPECIE==specie) %>% data.frame() %>% merge(a,.,by="COUNTRIES",all.x=T)
  
  
  
  ggplot(to.plot) +
    geom_sf(aes(fill = Resilience_Index)) +
    scale_colour_brewer() +
    coord_sf(xlim = c(-30,30), ylim = c(29,70)) +
    scale_fill_distiller(type = "seq", direction = 1,name="Resilence Index")+
    labs(subtitle = LETTERS[i])+
    theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
          panel.grid.major = element_line(color="white"),
          legend.text = element_text(size=12,color = "black"),
          legend.title = element_text(size=14,color="black"))
  
    
})


png("Figures/Figure 2.png",width=10,height=10,units="in",res=300)

do.call(grid_arrange_shared_legend,c(graphs, list(nrow = 3, ncol = 2)))

dev.off()



