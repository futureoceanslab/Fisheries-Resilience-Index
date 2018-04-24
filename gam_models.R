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
# [1] â€˜0.6.0â€™

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
# [1] â€˜1.2.1â€™


source("aux_functions.R")

install.packages("sjPlot")
library(mgcv)
library(gam)
library(sjPlot)
library(ggplot2)


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

model0e<- glm(Resilience_Index~LAT, family = "quasibinomial", data = joined[joined$DIMENSION=="ecological",])
summary(model0e)
anova(model0e)
model0s<- glm(Resilience_Index~LAT, family = "quasibinomial",data = joined[joined$DIMENSION=="socioeconomic",])
summary(model0s)
anova(model0s)
model0i<- glm(Resilience_Index~LAT, family = "quasibinomial",data = joined[joined$DIMENSION=="institutional",])
summary(model0i)
anova(model0i)
model1e <- glm(Resilience_Index~SPECIE+LAT,family = "quasibinomial", data=joined[joined$DIMENSION=="ecological",])
summary(model1e)
anova(model1e)
model1s <- glm(Resilience_Index~SPECIE+LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="socioeconomic",])
summary(model1s)
anova(model1s)
model1i <- glm(Resilience_Index~SPECIE+LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="institutional",])
summary(model1i)
anova(model1i)
model2e <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="ecological",])
summary(model2e)
anova(model2e)
model2s <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="socioeconomic",])
summary(model2s)
anova(model2s)
model2i <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="institutional",])
summary(model2i)
anova(model2i)

anova(model0e, model1e, model2e)

##### RUN MODELS (DIMENSION) #####

model0h<- glm(Resilience_Index~LAT, family = "quasibinomial", data = joined[joined$SPECIE=="Hake",])
summary(model0h)
anova(model0h)
model0c<- glm(Resilience_Index~LAT, family = "quasibinomial",data = joined[joined$SPECIE=="Cod",])
summary(model0c)
anova(model0c)
model1h <- glm(Resilience_Index~DIMENSION+LAT,family = "quasibinomial", data=joined[joined$SPECIE=="Hake",])
summary(model1h)
anova(model1h)
model1c <- glm(Resilience_Index~DIMENSION+LAT,family = "quasibinomial", data=joined[joined$SPECIE=="Cod",])
summary(model1c)
anova(model1c)
model2h <- glm(Resilience_Index~DIMENSION*LAT, family = "quasibinomial",data=joined[joined$SPECIE=="Hake",])
summary(model2h)
anova(model2h)
model2c <- glm(Resilience_Index~DIMENSION*LAT, family = "quasibinomial",data=joined[joined$SPECIE=="Cod",])
summary(model2c)
anova(model2c)

##### RUN MODELS (OTHER INDEX - SPECIE) #####

#Hake
model0Gh<- glm(Resilience_Index~GDP.2016, family = "quasibinomial", data = joined[joined$SPECIE=="Hake",])
summary(model0Gh)
model1GhD <- glm(Resilience_Index~DIMENSION+GDP.2016,family = "quasibinomial", data=joined[joined$SPECIE=="Hake",])
summary(model1GhD)
model2Ghd <- glm(Resilience_Index~DIMENSION*GDP.2016, family = "quasibinomial",data=joined[joined$SPECIE=="Hake",])
summary(model2Ghd)

model0Oh<- glm(Resilience_Index~OHI.2016, family = "quasibinomial", data = joined[joined$SPECIE=="Hake",])
summary(model0Oh)
model1OhD <- glm(Resilience_Index~DIMENSION+OHI.2016,family = "quasibinomial", data=joined[joined$SPECIE=="Hake",])
summary(model1OhD)
model2Ohd <- glm(Resilience_Index~DIMENSION*OHI.2016, family = "quasibinomial",data=joined[joined$SPECIE=="Hake",])
summary(model2Ohd)

model0oh<- glm(Resilience_Index~OHI.eco, family = "quasibinomial", data = joined[joined$SPECIE=="Hake",])
summary(model0oh)
model1ohD <- glm(Resilience_Index~DIMENSION+OHI.eco,family = "quasibinomial", data=joined[joined$SPECIE=="Hake",])
summary(model1ohD)
model2ohd <- glm(Resilience_Index~DIMENSION*OHI.eco, family = "quasibinomial",data=joined[joined$SPECIE=="Hake",])
summary(model2ohd)

model0Rh<- glm(Resilience_Index~Readiness, family = "quasibinomial", data = joined[joined$SPECIE=="Hake",])
summary(model0Rh)
model1RhD <- glm(Resilience_Index~DIMENSION+Readiness,family = "quasibinomial", data=joined[joined$SPECIE=="Hake",])
summary(model1RhD)
model2Rhd <- glm(Resilience_Index~DIMENSION*Readiness, family = "quasibinomial",data=joined[joined$SPECIE=="Hake",])
summary(model2Rhd)

model0Vh<- glm(Resilience_Index~Vulnerability, family = "quasibinomial", data = joined[joined$SPECIE=="Hake",])
summary(model0Vh)
model1VhD <- glm(Resilience_Index~DIMENSION+Vulnerability,family = "quasibinomial", data=joined[joined$SPECIE=="Hake",])
summary(model1VhD)
model2Vhd <- glm(Resilience_Index~DIMENSION*Vulnerability, family = "quasibinomial",data=joined[joined$SPECIE=="Hake",])
summary(model2Vhd)

#Cod
model0Gc<- glm(Resilience_Index~GDP.2016, family = "quasibinomial", data = joined[joined$SPECIE=="Cod",])
summary(model0Gc)
model1GcD <- glm(Resilience_Index~DIMENSION+GDP.2016,family = "quasibinomial", data=joined[joined$SPECIE=="Cod",])
summary(model1GcD)
model2Gcd <- glm(Resilience_Index~DIMENSION*GDP.2016, family = "quasibinomial",data=joined[joined$SPECIE=="Cod",])
summary(model2Gcd)

model0Oc<- glm(Resilience_Index~OHI.2016, family = "quasibinomial", data = joined[joined$SPECIE=="Cod",])
summary(model0Oc)
model1OcD <- glm(Resilience_Index~DIMENSION+OHI.2016,family = "quasibinomial", data=joined[joined$SPECIE=="Cod",])
summary(model1OcD)
model2Ocd <- glm(Resilience_Index~DIMENSION*OHI.2016, family = "quasibinomial",data=joined[joined$SPECIE=="Cod",])
summary(model2Ocd)

model0oc<- glm(Resilience_Index~OHI.eco, family = "quasibinomial", data = joined[joined$SPECIE=="Cod",])
summary(model0oc)
model1ocD <- glm(Resilience_Index~DIMENSION+OHI.eco,family = "quasibinomial", data=joined[joined$SPECIE=="Cod",])
summary(model1ocD)
model2ocd <- glm(Resilience_Index~DIMENSION*OHI.eco, family = "quasibinomial",data=joined[joined$SPECIE=="Cod",])
summary(model2ocd)

model0Rc<- glm(Resilience_Index~Readiness, family = "quasibinomial", data = joined[joined$SPECIE=="Cod",])
summary(model0Rc)
model1RcD <- glm(Resilience_Index~DIMENSION+Readiness,family = "quasibinomial", data=joined[joined$SPECIE=="Cod",])
summary(model1RcD)
model2Rcd <- glm(Resilience_Index~DIMENSION*Readiness, family = "quasibinomial",data=joined[joined$SPECIE=="Cod",])
summary(model2Rcd)

model0Vc<- glm(Resilience_Index~Vulnerability, family = "quasibinomial", data = joined[joined$SPECIE=="Cod",])
summary(model0Vc)
model1VcD <- glm(Resilience_Index~DIMENSION+Vulnerability,family = "quasibinomial", data=joined[joined$SPECIE=="Cod",])
summary(model1VcD)
model2Vcd <- glm(Resilience_Index~DIMENSION*Vulnerability, family = "quasibinomial",data=joined[joined$SPECIE=="Cod",])
summary(model2Vcd)

##### PLOTS #####

lateco <-  ggplot (na.omit(joined[joined$DIMENSION=="ecological",]), aes(LAT, Resilience_Index, col = SPECIE, linetype = SPECIE)) +
  geom_point(aes(shape=SPECIE)) +
  geom_smooth(method = "glm", family="quasibinomial")+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","steelblue4"))+
  xlab("Lat (º)")+
  ylab("R.I")
lateco

latdim <-  ggplot (na.omit(joined[joined$SPECIE=="Hake",]), aes(LAT, Resilience_Index, col = DIMENSION)) +
  geom_point(aes(shape=DIMENSION)) +
  geom_smooth(method = "glm", family="quasibinomial")+
  scale_shape_manual(values=c(1,5,6))+
  scale_color_manual(values=c("seagreen4","cornsilk3","yellow3"))+
  xlab("Lat (º)")+
  ylab("R.I")+
  ggtitle("Hake")
latdim


latdimc <-  ggplot (na.omit(joined[joined$SPECIE=="Cod",]), aes(LAT, Resilience_Index, col = DIMENSION)) +
  geom_point(aes(shape=DIMENSION)) +
  geom_smooth(method = "glm", family="quasibinomial")+
  scale_shape_manual(values=c(1,5,6))+
  scale_color_manual(values=c("seagreen4","cornsilk3","yellow3"))+
  xlab("Lat (º)")+
  ylab("R.I")+
  ggtitle("Cod")
latdimc






#table(joined$GDP.2016)

#data$INdesin<- apply(data [ ,3:10], 1, mean) #media por fila para o indice 