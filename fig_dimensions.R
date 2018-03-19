#' This script reads data/sp.csv and plots
#' Fig dimensions.png in the paper.

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

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’

source("aux_functions.R")

##### 2. READ DATA #####

# Read the data

sp <- read_csv("data/sp.csv")

matrix(c("Resilience_Index_E","Resilience_Index_S","RESILIENCE INDEX ecological-socioeconomic","Resilience Index S",
        "Resilience_Index_E","Resilience_Index_S","RESILIENCE INDEX ecological-socioeconomic","Resilience Index S",
        "Resilience_Index_E","Resilience_Index_S","RESILIENCE INDEX ecological-socioeconomic","Resilience Index S"),ncol=4) %>% data.frame()

data.frame(x=c(),
           y=c(),
           title=c())

p1 <- ggplot (sp, aes(Resilience_Index_E, Resilience_Index_S , col = SPECIE, linetype = SPECIE)) +
  geom_point(aes(shape=SPECIE)) +
  geom_smooth(se = TRUE, method = lm)+
  #scale_x_continuous(limits = c(0,1),
  #                   expand = c(0,0))+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","steelblue"))+
  ggtitle("RESILIENCE INDEX ecological-socioeconomic") +
  xlab("Resilience_Index_E")+
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1))
p1

####**Ecolgical Resilience vs Institutional Resilience**

p2 <- ggplot (sp, aes(Resilience_Index_E, Resilience_Index_I , col = SPECIE, linetype = SPECIE)) +
  geom_point(aes(shape=SPECIE)) +
  geom_smooth(se = TRUE, method = lm)+
  #scale_x_continuous(limits = c(0,1),
  #                   expand = c(0,0))+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","steelblue"))+
  ggtitle("RESILIENCE INDEX ecological-institutional") +
  xlab("Resilience_Index_E")+
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1))
p2

####**Socioeconomic Resilience vs Institutional Resilience**

p3 <- ggplot (sp, aes(Resilience_Index_S, Resilience_Index_I , col = SPECIE, linetype = SPECIE)) +
  geom_point(aes(shape=SPECIE)) +
  geom_smooth(se = TRUE, method = lm)+
  #scale_x_continuous(limits = c(0,1),
  #                   expand = c(0,0))+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","steelblue"))+
  ggtitle("RESILIENCE INDEX socioeconomic-institutional") +
  xlab("Resilience_Index_S")+
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1))
p3

