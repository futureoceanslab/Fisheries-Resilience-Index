#' Reads:
#' 
#' data/institutional_indicators_hake.csv
#' data/institutional_indicators_cod.csv
#' data/socioeconomic_indicators_hake.csv
#' data/socioeconomic_indicators_cod.csv
#' data/institutional_factors_hake.csv
#' data/institutional_factors_cod.csv
#' data/socioeconomic_factors_hake.csv
#' data/socioeconomic_factors_cod.csv
#' data/eco_country_cod.csv
#' data/eco_country_hake.csv
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


##### 1. Compute Resilience Index #####

#' Reads data file and preprocess indicators data
#' 
#' Computes mean of column_name by species/country and normalizes for each species using
#' (mean-min(mean))/(max(mean)-min(mean)). Returns column mean and normalized values by species/country
#' 
#' @param (filepath) path of the file to preprocess
#' @param (column_name) column name to preprocess
preprocess_indicators <- function(filepath,column_name){
  
  fread(filepath,check.names = TRUE) %>% # Read data
    group_by(SPECIES,COUNTRIES) %>% # Group by species and country
    summarise_at(c(column_name),funs(mean=mean(.,na.rm = TRUE))) %>% # Compute mean for each species/country
    ungroup() %>%
    group_by(SPECIES) %>%  # Group by species
    mutate(norm=(mean-min(mean,na.rm=TRUE))/(max(mean,na.rm = TRUE)-min(mean,na.rm=TRUE))) %>% # Normalize
    ungroup() %>%
    rename_at(c("mean","norm"),funs(paste0(column_name,.))) # Rename
  
  
}

# Preprocess institutional indicators

ins_indicators <- bind_rows(preprocess_indicators("data/institutional_indicators_hake.csv","TAC"),
                            preprocess_indicators("data/institutional_indicators_cod.csv","TAC")) %>% select(SPECIES,COUNTRIES,QUOTAS=TACnorm)

# Preprocess socioeconomic indicators

soc_indicators <- bind_rows(preprocess_indicators("data/socioeconomic_indicators_hake.csv","Stockdep.sp") %>% select(SPECIES,COUNTRIES,CATCH.DEP=Stockdep.spnorm),
                            preprocess_indicators("data/socioeconomic_indicators_cod.csv","Stockdep.sp") %>% select(SPECIES,COUNTRIES,CATCH.DEP=Stockdep.spnorm))

# Preprocess insitutional factors

ins_factors <- bind_rows(fread("data/institutional_factors_hake.csv",check.names = TRUE),
                         fread("data/institutional_factors_cod.csv",check.names = TRUE)) %>% # Read data
  select(COUNTRIES,SPECIES,STOCK,DEVELOPMENT,PROPERTY.RIGHTS,CO.MANAGEMENT) %>% 
  left_join(ins_indicators,by=c("COUNTRIES","SPECIES")) %>% # merge with institutional indicators
  gather(FACTOR,VALUE,-COUNTRIES,-SPECIES,-STOCK,factor_key = TRUE) %>% # Gather all factors in two columns
  group_by(SPECIES,COUNTRIES,FACTOR) %>% # Group by species, country and factor
  summarise(VALUE=mean(VALUE,na.rm=TRUE)) %>%  # Compute mean
  ungroup() %>%
  mutate(DIMENSION="institutional") # Add dimension name

# Preprocess socioeconomic factors.

soc_factors <- bind_rows(fread("data/socioeconomic_factors_hake.csv",check.names = TRUE),
                         fread("data/socioeconomic_factors_cod.csv",check.names = TRUE)) %>% # Read data
  select(COUNTRIES,SPECIES,STOCK,ADAPTIVE.MNG,FLEET.MOBILITY) %>% 
  left_join(soc_indicators,by=c("COUNTRIES","SPECIES")) %>% # merge with socioeconomic indicator
  gather(FACTOR,VALUE,-COUNTRIES,-SPECIES,-STOCK,factor_key = TRUE) %>% # Gather all factors in two columns
  group_by(SPECIES,COUNTRIES,FACTOR) %>% # Group by species, country and factor
  summarise(VALUE=mean(VALUE,na.rm=TRUE)) %>%  # Compute mean
  ungroup() %>%
  mutate(DIMENSION="socioeconomic") # Add dimension name




# Preprocess ecologic data

ecountriesCOD <- fread("data/eco_country_cod.csv",check.names = TRUE) %>% # Read data
  gather(FACTOR,VALUE,-COUNTRY,factor_key = TRUE) %>% # Gather colums in factors
  mutate(SPECIES="Atlantic cod",DIMENSION="ecological") %>% # add species name and dimension
  rename(COUNTRIES=COUNTRY)

ecountriesHAKE <- fread("data/eco_country_hake.csv",check.names = TRUE) %>% # Read data
  gather(FACTOR,VALUE,-COUNTRY,factor_key = TRUE) %>% # Gather colums in factors
  mutate(SPECIES="European hake",DIMENSION="ecological") %>% # add species name and dimension
  rename(COUNTRIES=COUNTRY)

ecountries <- bind_rows(ecountriesHAKE,ecountriesCOD)

# Compute resilience index. For that put together all the factors computed above and compute 
# their mean by species, dimension and country

resilience_index <- bind_rows(ecountries,ins_factors,soc_factors) %>% 
  group_by(SPECIES,DIMENSION,COUNTRIES) %>% 
  summarise(Resilience_Index=mean(VALUE,na.rm=TRUE)) %>%
  ungroup()


###### 2. Include other variables #####

# Read other indexes and merge with the resilience index


other_index <- fread("data/Other_index.csv",check.names = TRUE) %>% rename(OHI.2016=OHI.wild.caught)

# merge with resilience index

final_index <- other_index %>% 
  left_join(resilience_index,by=c("COUNTRIES")) %<>% 
  mutate(SPECIES=c(`Atlantic cod`="Cod",`European hake`="Hake")[SPECIES]) %>% rename(SPECIE=SPECIES) %>% 
  arrange(SPECIE,COUNTRIES,DIMENSION) %>% select(SPECIE,COUNTRIES,DIMENSION,everything())


# Save final index. This file is used by several scripts to produce most figures in the repository. 

write.csv(final_index,file="data/final_index.csv",row.names = FALSE)

