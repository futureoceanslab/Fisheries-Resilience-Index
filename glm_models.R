#' This script reads data/final_index.csv and plots
#' Figure X in the paper.
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

install.packages("sjPlot",repos='http://cran.us.r-project.org')
install.packages("MASS",repos='http://cran.us.r-project.org')
install.packages("gamlss",repos='http://cran.us.r-project.org')
install.packages("sjmisc",repos='http://cran.us.r-project.org')
install.packages("sjlabelled",repos='http://cran.us.r-project.org')
library(gamlss)
library(MASS)
library(mgcv)
library(gam)
library(sjPlot)
library(ggplot2)
library(sjmisc)
library(sjlabelled)


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

final_index <- read_csv("data/final_index.csv")

joined <- merge(a, final_index, by="COUNTRIES", all.x=T)



##### 4. RUN MODELS (SPECIE) #####

#model 0 --> the RI depends on LAT by dimensions? 

model0e<- glm(Resilience_Index~LAT, family = "quasibinomial", data = joined[joined$DIMENSION=="ecological",])
summary(model0e)
model0s<- glm(Resilience_Index~LAT, family = "quasibinomial",data = joined[joined$DIMENSION=="socioeconomic",])
summary(model0s)
model0i<- glm(Resilience_Index~LAT, family = "quasibinomial",data = joined[joined$DIMENSION=="institutional",])
summary(model0i)

sjt.glm(model0e, model0s, model0i, depvar.labels = c("Model0 ecological", "Model0 socioeconomic", "Model0 institutional"), 
        p.numeric = FALSE, show.chi2 = TRUE, show.se = TRUE, show.dev = TRUE, exp.coef = FALSE)


#model 1 --> the RI depends on LAT by dimension, classify by sp? 

model1e <- glm(Resilience_Index~SPECIE+LAT,family = "quasibinomial", data=joined[joined$DIMENSION=="ecological",])
summary(model1e)

model1s <- glm(Resilience_Index~SPECIE+LAT,family = "quasibinomial",data=joined[joined$DIMENSION=="socioeconomic",])
summary(model1s)

model1i <- glm(Resilience_Index~SPECIE+LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="institutional",])
summary(model1i)

sjt.glm(model1e, model1s, model1i, depvar.labels = c("Model1 ecological", "Model1 socioeconomic", "Model1 institutional"), 
        p.numeric = FALSE, show.chi2 = TRUE, show.se = TRUE, show.dev = TRUE, exp.coef = FALSE)




#model 2 --> the RI depends on LAT by dimension, classify by sp? 

model2e <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial" ,data=joined[joined$DIMENSION=="ecological",])
summary(model2e)

model2s <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="socioeconomic",])
summary(model2s)

model2i <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="institutional",])
summary(model2i)

sjt.glm(model2e, model2s, model2i, depvar.labels = c("Model2 ecological", "Model2 socioeconomic", "Model2 institutional"), p.numeric = FALSE, separate.ci.col = FALSE,
        show.aic = F, show.family = TRUE, show.r2 = TRUE, exp.coef = FALSE)

