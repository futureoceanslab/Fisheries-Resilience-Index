

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

if(!require(flextable)){
  install.packages('flextable',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(flextable)
packageVersion("flextable")
# [1] ‘0.4.4’


if(!require(officer)){
  install.packages('officer',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(officer)
packageVersion("officer")
# [1] ‘0.3.0’

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

if(!require(viridis)){
  install.packages("viridis",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(viridis)
packageVersion("viridis")

source("aux_functions.R")

# Disable scientific notation
scipen <- getOption("scipen")
options(scipen=999)



data=read.csv("data/AllFactors.csv",sep=",", header=TRUE)


data <- data %>% 
        select(-DIMENSION) %>%
        rename(Resilience_Index_Dim=Resilience_Index) %>%
        group_by(SPECIES, COUNTRIES) %>%
        mutate(Resilience_Index=mean(Resilience_Index_Dim)) %>%
        select(-Resilience_Index_Dim)
dim(data)

data <- unique(data)
 
write_excel_csv(data,"data/AllFactorsRI.csv") 
  



