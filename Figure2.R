#' This script reads data/final_index.csv and plots
#' Figure 2 in the paper.


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


if(!require(broom)){
  install.packages("broom",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(broom)
packageVersion("broom")
# [1] ‘0.4.2’

if(!require(ggmap)){
  install.packages("ggmap",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(ggmap)
packageVersion("ggmap")
# [1] ‘2.6.1’

if(!require(sp)){
  install.packages("sp",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(sp)
packageVersion("sp")
# [1] ‘1.2.7’


if(!require(rgdal)){
  install.packages("rgdal",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(rgdal)
packageVersion("rgdal")
# [1] ‘1.2.20’


if(!require(rnaturalearthdata)){
  install.packages("rnaturalearthdata",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(rnaturalearthdata)
packageVersion("rnaturalearthdata")
# [1] ‘0.1.0’

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’


source("aux_functions.R")


##### 2. PREPARE MAP #####

# Get country borders and select contries

countries_to_plot <- c("EG","AL","AD","AT","BY","BE","BA","BG","HR","CY","CZ","DK","EE","FO","FI","FR","DE","GI","GR","HU","IS","IE","IM","IT","RS","LV","LI","LT","LU","MK","MT","MD","MC","ME","NL","NO","PL","PT","RO","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA","RS","MA","TN","DZ","LY","EH")

world <- rnaturalearthdata::countries50

# Transform to use with ggmap

europe <- world[world$iso_a2 %in% countries_to_plot,]

europe@data$id <- rownames(europe@data)

europe <- spTransform(europe, CRS("+proj=longlat"))

##### 3. READ DATA #####

final_index <- read_csv("data/final_index.csv")


##### 4. PLOT #####

# Define which dimension and species will be shown in each panel
plot_definition <- data.frame(DIMENSION=c("institutional","institutional","socioeconomic","socioeconomic","ecological","ecological"),
                              SPECIES=c("Cod","Hake","Cod","Hake","Cod","Hake"),
                              stringsAsFactors = FALSE)


# Plot each panel
graphs <- 1:nrow(plot_definition) %>% lapply(function(i){
  
  # Get dimension and species for this panel
  dimension <- plot_definition$DIMENSION[i]
  specie <- plot_definition$SPECIES[i]

  # Filter data for that dimension and specie.
  
  to.plot <-   final_index  %>% 
    filter(DIMENSION==dimension,SPECIE==specie)
  
  # Plot

  
  # Join map with index and get ready to plot
  
  map_a <- europe
  
  map_a@data <- left_join(map_a@data,to.plot,by=c(iso_a2="COUNTRIES"))
  
  map <- tidy(map_a)
  
  map <- left_join(map,map_a@data,by="id")
  
  #Plot
  
  ggplot() + 
    geom_map(data=map, map=map,
                      aes(x=long,y=lat, map_id=id, group=group),
                      fill="gray50", color="black")+
  geom_map(data=map, map=map, color="black", size=0.15,
           aes(fill=Resilience_Index, group=id, map_id=id)) +
    scale_colour_brewer() +
    coord_equal(xlim = c(-30,30), ylim = c(29,70),ratio=1.5) +
    scale_x_continuous(labels =longitude_formatter )+
    scale_y_continuous(labels =latitude_formatter )+
    scale_fill_distiller(type = "seq", direction = 1,name="Resilence Index",limits=range(final_index$Resilience_Index))+
    labs(subtitle = LETTERS[i])+
    theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
          panel.grid.major = element_line(color="white"),
          legend.text = element_text(size=12,color = "black"),
          legend.title = element_text(size=14,color="black"),
          legend.key.width=unit(2,"cm"),
          legend.position = "bottom",
          axis.title = element_blank())
    

  
    
})

# Arrange all the panels in one graph with a common legend

tiff("Figures/Figure 2.tiff",width=10,height=10,units="in",res=300)

do.call(grid_arrange_shared_legend,c(graphs, list(nrow = 3, ncol = 2)))

dev.off()



