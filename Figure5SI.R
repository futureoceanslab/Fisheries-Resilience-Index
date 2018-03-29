#' This script reads data/final_index.csv and plots
#' Figures/Fig 5 SI.


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

if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(gridExtra)
packageVersion("gridExtra")
# [1] ‘2.2.1’

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’


source("aux_functions.R")


##### 2. GET LONGITUDE AND LATITUDES #####

download.file(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip",
              destfile = "data/world.zip", mode = "wb")

unzip("data/world.zip", exdir = "data")

world_area <- sf::st_read("data/TM_WORLD_BORDERS_SIMPL-0.3.dbf", quiet = TRUE) ## sf:: es para que sepa que la funcion es del package sf, sino sale masked como error.


file.remove(c("data/Readme.txt","data/world.zip",list.files("data",pattern = "TM_WORLD.*",full.names = TRUE)))

a<-filter(world_area, grepl('EG|AL|AD|AT|BY|BE|BA|BG|HR|CY|CZ|DK|EE|FO|FI|FR|DE|GI|GR|HU|IS|IE|IM|IT|RS|LV|LI|LT|LU|MK|MT|MD|MC|ME|NL|NO|PL|PT|RO|SM|RS|SK|SI|ES|SE|CH|UA|GB|VA|RS|MA|TN|DZ|LY|EH', ISO2)) %>% #get the dat with your 3 cuntries per caracter variable with a name
  mutate(value = AREA*SUBREGION) %>% rename(COUNTRIES=ISO2)  ## add a column called "value" multipliying area*region


##### 3. READ DATA #####

final_index <- read_csv("data/final_index.csv")

to.plot <- final_index %>% 
  data.frame() %>% 
  merge(a,.,by="COUNTRIES",all.x=T) # IMPORTANT!!!!: Cannot use full_join because sf structure is lost and map is not correlctly rendered

dimensions <- to.plot %>% filter(!is.na(DIMENSION)) %>% pull("DIMENSION") %>% unique

species <- to.plot %>% filter(!is.na(SPECIE)) %>% pull("SPECIE") %>% unique



# Compute the p-value for each dimension name and create table

p_values_dimensions  <- p_values_for_column(to.plot,"DIMENSION","LAT")

# Compute the p-value for each species name and create table

p_values_species <- p_values_for_column(to.plot,"SPECIE","LAT")





##### 4. PLOT #####


#' Plots p-values < 0.05 in the upper left_corner of the panel
#' 
#' @param g (ggplot) graph
#' @param data (data.frame) data used to plot g
#' @param p_values (data.frame) p_values for the trend lines displayed
#' @param column_name (character) column name of the classe used to plot the trend lines
#' @return (ggplot) graph with the p-values
plot_p_values <- function(g,data,p_values,column_name){
  # Locate p-values at the bottom center of the graph. y_center may fail with different ggplot versions
  x_center <- data[,"Resilience_Index"] %>% min(na.rm=TRUE)
  y_center <- data[,"LAT"] %>% max(na.rm=TRUE)
  
  
  # get the p-values
  p <-  p_values %>% gather(var,p) %>% filter(p<0.05) %>% mutate(p=ifelse(p<0.01,"<0.01",sprintf("%0.2f",p))) %>% mutate(x=x_center,y=y_center,hjust=c(0,-1.2,-2.4)[1:nrow(.)])
  
  names(p)[1] <- column_name
  
  # plot the p-values
  
  
  if(nrow(p) >0){
    g<- g + geom_text(data=p,aes_string(x="x",y="y",label="p",col=column_name,hjust="hjust"),show.legend = FALSE,vjust=2) +
      geom_text(label="p-value",col="black",x=x_center,y=y_center,vjust=0,hjust=0)
    
  }
  
  g
  
}

# By dimension

LatDim <- ggplot (na.omit(to.plot), aes(Resilience_Index, LAT, col = DIMENSION)) +
  geom_point(aes(shape=DIMENSION)) +
  geom_smooth(se = TRUE, method = "lm", size= 1,alpha=0.2)+
  scale_color_manual(values=c("seagreen4","cornsilk3","yellow3"))+
  xlab("R.I")+
  ylab("Latitude (º)") +
  theme_classic() + 
  theme(axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=20, color="black"),
        legend.text = element_text(size=12,color = "black"),
        legend.title = element_text(size=14,color="black")) 

LatDim <- plot_p_values(LatDim, na.omit(to.plot),p_values_dimensions,"DIMENSION") # plot p-values
LatDim






# By species

LatSp <- ggplot (na.omit(to.plot), aes(Resilience_Index, LAT, col = SPECIE, linetype = SPECIE)) +
  geom_point(aes(shape=SPECIE)) +
  geom_smooth(se = TRUE, method = "lm", size= 1,alpha=0.2)+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","steelblue4"))+
  xlab("R.I")+
  ylab("Latitude (º)") +
  theme_classic() + 
  theme(axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=20, color="black"),
        legend.text = element_text(size=12,color = "black"),
        legend.title = element_text(size=14,color="black"))

LatSp <- plot_p_values(LatSp, na.omit(to.plot),p_values_species,"SPECIE") # plot p-values


LatSp

# Arrange both panels in one graph and save

png("Figures/Fig 5 SI.png",width=9,height=9,units="in",res=300)

grid.arrange(ggplotGrob(LatDim), ggplotGrob(LatSp), layout_matrix = cbind(c(1,2)))

dev.off()
