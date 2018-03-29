
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
  install.packages('magrittr',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(magrittr)
packageVersion("magrittr")
# [1] ‘1.5’

if(!require(data.table)){
  install.packages('data.table',dependencies = TRUE,repos='http://cran.us.r-project.org')
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


GDP <- fread("data/GDP.csv",check.names = TRUE)
OHI <- fread("data/OHI.csv",check.names = TRUE)
OHIeco <- fread("data/OHIeco.csv",check.names = TRUE)
Tech <- fread("data/Tech.csv",check.names = TRUE)
Requi <- fread("data/Requirements.csv",check.names = TRUE)
Readiness <- fread("data/Readiness.csv",check.names = TRUE)
Vulnerability <- fread("data/Vulnerability.csv",check.names = TRUE)


final_index <- reduce(list(GDP,OHI,OHIeco,Tech,Requi,Readiness,Vulnerability),full_join,by="COUNTRY") %>% rename(COUNTRIES=COUNTRY)


preprocess_indicators <- function(filepath,column_name){
  
  fread(filepath,check.names = TRUE) %>%
    group_by(SPECIES,COUNTRIES) %>%
    summarise_at(c(column_name),funs(mean=mean(.,na.rm = TRUE))) %>%
    ungroup() %>%
    group_by(SPECIES) %>%
    mutate(norm=(mean-min(mean,na.rm=TRUE))/(max(mean,na.rm = TRUE)-min(mean,na.rm=TRUE))) %>%
    ungroup() %>%
    rename_at(c("mean","norm"),funs(paste0(column_name,.)))
  
  
}

ins_indicators <- preprocess_indicators("data/institutional_indicators.csv","TAC")  %>% select(SPECIES,COUNTRIES,QUOTAS=TACnorm)

soc_indicators <- preprocess_indicators("data/socioeconomic_indicators.csv","Stockdep.sp") %>% select(SPECIES,COUNTRIES,CATCH.DEP=Stockdep.spnorm)

soc_factors <- fread("data/socioeconomic_factors.csv",check.names = TRUE) %>% 
  select(COUNTRIES,SPECIES,STOCK,ADAPTIVE.MNG,FLEET.MOBILITY) %>% 
  left_join(soc_indicators,by=c("COUNTRIES","SPECIES")) %>%
  gather(FACTOR,VALUE,-COUNTRIES,-SPECIES,-STOCK,factor_key = TRUE) %>%
  group_by(SPECIES,COUNTRIES,FACTOR) %>%
  summarise(VALUE=mean(VALUE,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(DIMENSION="socioeconomic")

ins_factors <- fread("data/institutional_factors.csv",check.names = TRUE) %>%
  select(COUNTRIES,SPECIES,STOCK,DEVELOPMENT,PROPERTY.RIGHTS,CO.MANAGEMENT) %>% 
  left_join(ins_indicators,by=c("COUNTRIES","SPECIES")) %>%
  gather(FACTOR,VALUE,-COUNTRIES,-SPECIES,-STOCK,factor_key = TRUE) %>%
  group_by(SPECIES,COUNTRIES,FACTOR) %>%
  summarise(VALUE=mean(VALUE,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(DIMENSION="institutional")



ecountriesCOD <- fread("data/eco_country_cod.csv",check.names = TRUE) %>% 
  gather(FACTOR,VALUE,-COUNTRY,factor_key = TRUE) %>%
  mutate(SPECIES="Atlantic cod",DIMENSION="ecological") %>%
  rename(COUNTRIES=COUNTRY)
  
dim_country <- bind_rows(ecountriesCOD,ins_factors,soc_factors) %>% 
  group_by(SPECIES,DIMENSION,COUNTRIES) %>% 
  summarise(Resilience_Index=mean(VALUE,na.rm=TRUE)) %>%
  ungroup()

write.csv(dim_country,file="data/dim_country.csv",row.names = FALSE)

final_index %<>% left_join(dim_country,by=c("COUNTRIES")) %<>% mutate(SPECIES=c(`Atlantic cod`="Cod",`European hake`="Hake")[SPECIES]) %>% rename(SPECIE=SPECIES)

write.csv(final_index,file="data/final_index.csv",row.names = FALSE)
