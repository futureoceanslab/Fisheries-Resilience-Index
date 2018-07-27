#' Reads data/eco_country_cod.csv, data/eco_country_hake.csv, 
#' data/institutional_factors_cod.csv, data/institutional_factors_hake.csv,
#' data/socioeconomic_factors_cod.csv  and data/socioeconomic_factors_hake.csv 
#' and plots Figures/Fig 2 SI.png

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
# [1] ‘1.5’

if(!require(data.table)){
  install.packages("data.table",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(data.table)
packageVersion("data.table")
# [1] ‘1.10.4’

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’

source("aux_functions.R")

##### 2. READ DATA #####

# Read the data
# 
# eco_countryCOD <- fread("data/eco_country_cod.csv",check.names = TRUE)
# eco_countryHAKE <- fread("data/eco_country_hake.csv",check.names = TRUE)
# 
# ins_factors <- bind_rows(fread("data/institutional_factors_hake.csv",check.names = TRUE),
#                          fread("data/institutional_factors_cod.csv",check.names = TRUE))
# 
# 
# soc_factors <- bind_rows(fread("data/socioeconomic_factors_hake.csv",check.names = TRUE),
#                          fread("data/socioeconomic_factors_cod.csv",check.names = TRUE))
# 
# eco_countryCOD$SPECIES<-"Atlantic Cod"
# eco_countryCOD %<>% gather(FACTOR, VALUE, ABUNDANCE:RECOVERY, factor_key=TRUE)
# eco_countryHAKE$SPECIES<-"European Hake"
# eco_countryHAKE %<>% gather(FACTOR, VALUE, ABUNDANCE:RECOVERY, factor_key=TRUE)
# 
# eco_factors <- rbind(eco_countryCOD, eco_countryHAKE) %>% rename(COUNTRIES=COUNTRY) %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR))) %>% mutate(FACTOR=factor(FACTOR,levels = c("ABUNDANCE","TEMPERATURE","OVEREXPLOITATION","RECOVERY")))
# 
# 
# soc_factors %<>% gather(FACTOR, VALUE, ADAPTIVE.MNG:FLEET.MOBILITY, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("ADAPTIVE MNG","CATCH DEP","FLEET MOBILITY")))
# 
# ins_factors %<>% gather(FACTOR, VALUE, DEVELOPMENT:CO.MANAGEMENT, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR))) %>% mutate(FACTOR=factor(FACTOR,levels = c("DEVELOPMENT","QUOTAS","PROPERTY RIGHTS","CO MANAGEMENT")))

eco_factors <- fread("data/ecological_factors_country.csv") %>% gather(FACTOR, VALUE, -SPECIES,-COUNTRIES, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("ABUNDANCE","TEMPERATURE","OVEREXPLOITATION","RECOVERY")))

soc_factors <- fread("data/socioeconomic_factors_country.csv") %>% gather(FACTOR, VALUE, -SPECIES,-COUNTRIES, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("ADAPTIVE MNG","CATCH DEP","FLEET MOBILITY")))

ins_factors <- fread("data/institutional_factors_country.csv") %>% gather(FACTOR, VALUE, -SPECIES,-COUNTRIES, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("STRENGTH","QUOTAS","PROPERTY RIGHTS","CO MANAGEMENT")))

##### 3. PLOT #####

dimensions <- c("eco_factors","soc_factors","ins_factors")
  
dimension_colors <- list(eco_factors=c("seagreen3","seagreen4"),
                         soc_factors=c("yellow3","yellow4"),
                         ins_factors=c("cornsilk3","cornsilk4"))
                         
graphs <- dimensions  %>% lapply(function(dimension){
  # dimension <- "soc_factors"
  x<- get(dimension)

  g <- ggplot(data=na.omit(x), aes(x= COUNTRIES , y=VALUE, fill=SPECIES, group=SPECIES))+
    geom_bar(stat = "identity", position=position_dodge(width=0.4), width=0.3)+
    scale_fill_manual(values=dimension_colors[[dimension]],name="SPECIES")+
    facet_grid(~FACTOR)+
    coord_flip()+
    theme_classic()+
    ylab("")+
    xlab("COUNTRY")+
    theme(axis.text.y = element_text(size=20, color="black"),
          axis.text.x = element_text(size=16,color="black",angle=50,hjust=1),
          legend.text = element_text(size=22,color = "black"),
          legend.title = element_text(size=24,color="black"),
          panel.spacing.x = unit(1,"lines"),
          strip.text = element_text(color="black",face="bold",size=11))
  
  if(dimension!="eco_factors"){
    
    g <- g + theme(axis.title.y = element_blank())
  }else{
    g <- g + theme(axis.title.y = element_text(size=26, color="black"))
    
  }
   g 
})


tiff("Figures/Fig 2 SI.tiff",width=25,height=12,units="in",res=300)

do.call(grid_arrange_shared_legend,c(graphs, list(nrow = 1, ncol = 3)))

dev.off()


