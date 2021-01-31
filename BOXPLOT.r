#' This script reads data/final_index.csv and plots
#' Figure GLM (3) in the paper.
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


library(mgcv)
#library(gam)

## To create Latex tables with GAM summary 
install.packages("itsadug")
library(itsadug)

##### 2. PREPARE lAT #####


download.file(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip",
              destfile = "data/world.zip", mode = "wb")

unzip("data/world.zip", exdir = "data")

world_area <- sf::st_read("data/TM_WORLD_BORDERS_SIMPL-0.3.dbf", quiet = TRUE) ## sf:: es para que sepa que la funcion es del package sf, sino sale masked como error.
class(world_area)        

file.remove(c("data/Readme.txt","data/world.zip",list.files("data",pattern = "TM_WORLD.*",full.names = TRUE)))

a<-filter(world_area, grepl('EG|AL|AD|AT|BY|BE|BA|BG|HR|CY|CZ|DK|EE|FO|FI|FR|DE|GI|GR|HU|IS|IE|IM|IT|RS|LV|LI|LT|LU|MK|MT|MD|MC|ME|NL|NO|PL|PT|RO|SM|RS|SK|SI|ES|SE|CH|UA|GB|VA|RS|MA|TN|DZ|LY|EH', ISO2)) %>% #get the dat with your 3 cuntries per caracter variable with a name
  mutate(value = AREA*SUBREGION) %>% rename(COUNTRIES=ISO2)  ## add a column called "value" multipliying area*region

##### 3. READ DATA #####

final_index <- read.csv("data/final_index.csv",sep=",", header=TRUE)  ## ";" in PC?

joined <- merge(a, final_index, by="COUNTRIES", all.x=T)



## Create factor variables 
joined$sp=as.factor(joined$SPECIE)
joined$dim=as.factor(joined$DIMENSION)
Data=split(joined,f=joined$dim)


################## 	BOXPLOTS ##################
## Can be useful to supprt the comparison between species made in the first paragraph of Results.
species_average <- final_index %>% 
  arrange(COUNTRIES,DIMENSION)  %>% 
  mutate(SPECIE=toupper(SPECIE))  %>%  
  group_by(COUNTRIES,SPECIE) %>%
  summarise(Resilience_Index=mean(RI,na.rm = TRUE)) %>%
  ungroup()
	
Fdata=species_average

png(filename = "Figures/Boxplots_RI.png",
    width = 20, height =8, units = "cm", pointsize = 8,
    bg = "white", res = 450, family = "", restoreConsole = TRUE)


	par(mfrow=c(1,4))
	ylim=c(0,1)
	ldim=levels(joined$dim)
	boxplot(Resilience_Index~SPECIE, ylim=ylim, ylab="Resilience Index", main="Overal", data=Fdata)
	for(i in 1:3)
	{
		boxplot(RI~sp, ylim=ylim, ylab="Resilience Index", main=ldim[i], data=Data[[i]])
	}

dev.off()
