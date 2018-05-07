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

#model 0 --> the RI depends on LAT by dimensions? 

model0e<- glm(Resilience_Index~LAT, family = "quasibinomial", data = joined[joined$DIMENSION=="ecological",])
model0e
plot (model0e)
#sjp.glm(model0e)
model0s<- glm(Resilience_Index~LAT, family = "quasibinomial",data = joined[joined$DIMENSION=="socioeconomic",])
model0s
#sjp.glm (model0s)
model0i<- glm(Resilience_Index~LAT, family = "quasibinomial",data = joined[joined$DIMENSION=="institutional",])
model0i
#anova(model0i)
sjt.glm(model0e, model0s, model0i, p.numeric = FALSE, string.est = "Estimate",
        show.aic = F, show.family = TRUE)

#model 1 --> the RI depends on LAT by dimension, classify by sp? 

model1e <- glm(Resilience_Index~SPECIE+LAT,family = "quasibinomial", data=joined[joined$DIMENSION=="ecological",])
summary(model1e)
sjp.glm(model1e)
model1s <- glm(Resilience_Index~SPECIE+LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="socioeconomic",])
model1s
anova(model1s)
model1i <- glm(Resilience_Index~SPECIE+LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="institutional",])
model1i
anova(model1i)
sjt.glm(model1e, model1s, model1i, p.numeric = FALSE, separate.ci.col = FALSE,
        show.aic = F, show.family = TRUE, show.r2 = TRUE)

#model 1 --> the RI depends on LAT by dimension, classify by sp? 

model2e <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="ecological",])
model2e
anova(model2e)
model2s <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="socioeconomic",])
model2s
anova(model2s)
model2i <- glm(Resilience_Index~SPECIE*LAT, family = "quasibinomial",data=joined[joined$DIMENSION=="institutional",])
model2i
anova(model2i)
sjt.glm(model2e, model2s, model2i, p.numeric = FALSE, separate.ci.col = FALSE,
        show.aic = F, show.family = TRUE, show.r2 = TRUE)



sjt.glm(model0e, model1e, model2e, depvar.labels = c("Model0 eco", "Model1 eco", "Model2 eco"),  p.numeric = FALSE, group.pred = FALSE)
sjt.glm(model0s, model1s, model2s, depvar.labels = c("Model0 socio", "Model1 socio", "Model2 socio"), p.numeric = FALSE, group.pred = FALSE)
sjt.glm(model0i, model1i, model2i, depvar.labels = c("Model0 ins", "Model1 ins", "Model2 ins"), p.numeric = F, group.pred = FALSE)
sjt.glm(model1e, model1s, model1i, depvar.labels = c("Model1 eco", "Model1 soci", "Model1 ins"), string.est = "Estimate",
        show.dev = TRUE, p.numeric = FALSE, group.pred = FALSE, show.chi2 = TRUE, show.se = TRUE)


LatDim <- ggplot (na.omit(joined), aes(LAT,Resilience_Index, col = DIMENSION)) +
  geom_point(aes(shape=DIMENSION)) +
  geom_smooth(se = TRUE, method = "lm", size= 1,alpha=0.2)+
  scale_color_manual(values=c("seagreen4","cornsilk3","yellow3"))+
  xlab("Latitude (º)")+
  ylab("R.I") +
  theme_classic() + 
  theme(axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=20, color="black"),
        legend.text = element_text(size=12,color = "black"),
        legend.title = element_text(size=14,color="black")) 
LatDim


LatSp <- ggplot (na.omit(joined), aes(LAT, Resilience_Index, col = SPECIE, linetype = SPECIE)) +
  geom_point(aes(shape=SPECIE)) +
  geom_smooth(se = TRUE, method = "lm", size= 1,alpha=0.2)+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","steelblue4"))+
  xlab("Latitude (º)")+
  ylab("R.I") +
  theme_classic() + 
  theme(axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=20, color="black"),
        legend.text = element_text(size=12,color = "black"),
        legend.title = element_text(size=14,color="black"))
LatSp


##### RUN MODELS (DIMENSION) #####

model0h<- glm(Resilience_Index~LAT, family = "quasibinomial", data = joined[joined$SPECIE=="Hake",])
summary(model0h)
sjp.glm(model0h)
model0c<- glm(Resilience_Index~LAT, family = "quasibinomial",data = joined[joined$SPECIE=="Cod",])
model0c
anova(model0c)
model1h <- glm(Resilience_Index~DIMENSION+LAT,family = "quasibinomial", data=joined[joined$SPECIE=="Hake",])
summary(model1h)
plot_model(model1h)
plot(model1h)
model1c <- glm(Resilience_Index~DIMENSION+LAT,family = "quasibinomial", data=joined[joined$SPECIE=="Cod",])
model1c
anova(model1c)
model2h <- glm(Resilience_Index~DIMENSION*LAT, family = "quasibinomial",data=joined[joined$SPECIE=="Hake",])
model2h
anova(model2h)
model2c <- glm(Resilience_Index~DIMENSION*LAT, family = "quasibinomial",data=joined[joined$SPECIE=="Cod",])
model2c
anova(model2c)


##### PLOTS #####

lateco <-  ggplot (na.omit(joined[joined$DIMENSION=="ecological",]), aes(LAT, Resilience_Index, col = SPECIE, linetype = SPECIE)) +
  geom_point(aes(shape=SPECIE)) +
  geom_smooth(method = "glm", family="quasibinomial")+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","steelblue4"))+
  xlab("Lat (�)")+
  ylab("R.I")
lateco

latdim <-  ggplot (na.omit(joined[joined$SPECIE=="Hake",]), aes(LAT, Resilience_Index, col = DIMENSION)) +
  geom_point(aes(shape=DIMENSION)) +
  geom_smooth(method = "glm", family="quasibinomial")+
  scale_shape_manual(values=c(1,5,6))+
  scale_color_manual(values=c("seagreen4","cornsilk3","yellow3"))+
  xlab("Lat (�)")+
  ylab("R.I")+
  ggtitle("Hake")
latdim


latdimc <-  ggplot (na.omit(joined[joined$SPECIE=="Cod",]), aes(LAT, Resilience_Index, col = DIMENSION)) +
  geom_point(aes(shape=DIMENSION)) +
  geom_smooth(method = "glm", family="quasibinomial")+
  scale_shape_manual(values=c(1,5,6))+
  scale_color_manual(values=c("seagreen4","cornsilk3","yellow3"))+
  xlab("Lat (�)")+
  ylab("R.I")+
  ggtitle("Cod")
latdimc






#sjp.glm para plotear. sjPlot paquete




table(joined$GDP.2016)

data$INdesin<- apply(data [ ,3:10], 1, mean) #media por fila para o indice 