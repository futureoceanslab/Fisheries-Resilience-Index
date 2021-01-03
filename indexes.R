#' Reads:
#' 
#' data/institutional_factors_country.csv
#' data/socioeconomic_factors_country.csv
#' data/ecological_factors_country.csv
#' data/Other_index.csv
#' 
#' and produces
#' 
#' data/final_index.csv



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



##### 1. Read factors #####


eco_factors <- fread("data/ecological_factors_country.csv") %>% mutate(DIMENSION="ecological")

eco_factors_stock <- fread("data/ecological_factors.csv") %>% mutate(DIMENSION="ecological")

ins_factors <- fread("data/institutional_factors_country.csv")  %>% mutate(DIMENSION="institutional")

soc_factors <- fread("data/socioeconomic_factors_country.csv")  %>% mutate(DIMENSION="socioeconomic")

##### 2. Compute Resilience Index #####

##compute resilience index per stock, only ecological


eco_res_stock <- bind_rows(eco_factors_stock) %>% 
  gather(FACTOR,VALUE,-STOCK,-SPECIES,-DIMENSION,factor_key = TRUE) %>%
  filter(complete.cases(.)) %>%
  group_by(SPECIES,DIMENSION,STOCK) %>% 
  summarise(Eco_Res_Index=mean(VALUE,na.rm=TRUE)) %>%
  ungroup()


# Compute resilience index. For that put together all the factors computed above and compute 
# their mean by species, dimension and country

resilience_index <- bind_rows(eco_factors,ins_factors,soc_factors) %>% 
  gather(FACTOR,VALUE,-COUNTRIES,-SPECIES,-DIMENSION,factor_key = TRUE) %>%
  filter(complete.cases(.)) %>%
  group_by(SPECIES,DIMENSION,COUNTRIES) %>% 
  summarise(Resilience_Index=mean(VALUE,na.rm=TRUE)) %>%
  ungroup()


###### 3. Merge other indexes #####

# Read other indexes and merge with the resilience index


other_index <- fread("data/Other_index.csv",check.names = TRUE) %>% rename(OHI.fisheries=OHI.wild.caught)

# merge with resilience index

final_index <- other_index %>% 
  left_join(resilience_index,by=c("COUNTRIES")) %>% 
  mutate(SPECIES=c(`Atlantic cod`="Cod",`European hake`="Hake")[SPECIES]) %>% rename(SPECIE=SPECIES) %>% 
  arrange(SPECIE,COUNTRIES,DIMENSION) %>% select(SPECIE,COUNTRIES,DIMENSION,everything())


# Save final index. This file is used by several scripts to produce most figures in the repository. 

write.csv(final_index,file="data/final_index.csv",row.names = FALSE)
