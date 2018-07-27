#' This script reads data/final_index.csv and plots
#' Figures/Fig 3 SI.png in the paper.

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
  install.packages("magrittr",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(magrittr)
packageVersion("magrittr")


if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’

##### 2. READ DATA #####

# Read the data

final_index <- read_csv("data/final_index.csv")

to.plot <- final_index %>% mutate(SPECIE=toupper(SPECIE)) %>% complete(SPECIE,COUNTRIES,DIMENSION)

# Compute average Resilience index per specie and country

aggdata <- to.plot %>% group_by(COUNTRIES,SPECIE) %>% summarise(Resilience_Index=mean(Resilience_Index,na.rm=TRUE)) %>% ungroup()


##### 3. PLOT #####

ggplot(to.plot, aes(x=COUNTRIES, y=Resilience_Index))+
  geom_bar(aes(fill = DIMENSION), stat="identity",col=NA)+
  scale_fill_manual(values=c("seagreen4","cornsilk3","yellow3"))+
  geom_point(data= aggdata, aes(x=COUNTRIES, y= Resilience_Index,group=SPECIE),col="black",size=3)+
  coord_flip()+
  facet_wrap(~ SPECIE,ncol=2,scales="free_y")+
  ylab("Resilience Index")+
  theme_classic()+
  theme(legend.position = "bottom",
        strip.text = element_text(size=18,color="black",face="bold",hjust=0),
        strip.background = element_blank(),
        axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=20, color="black"),
        legend.text = element_text(size=22,color = "black"),
        legend.title = element_text(size=24,color="black"))

# Save

ggsave("Figures/Fig 3 SI.tiff",width = 10,height = 8,units = "in")
