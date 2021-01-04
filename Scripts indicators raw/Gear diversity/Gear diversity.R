##gear diversity calculation per country
##jan 2021 Elena Ojea and Juan Bueno

##load packages and libraries

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

source("aux_functions.R")

# Disable scientific notation
scipen <- getOption("scipen")
options(scipen=999)


##load raw data files

gear_cod  <- fread("Scripts indicators raw/Gear diversity/SAU_Taxa_cod.csv",check.names = TRUE)
gear_hake  <- read.csv("Scripts indicators raw/Gear diversity/SAU_Taxa_hake.csv", sep = ",")

###
countries_order <- c("BE","DK","DE","EE","IE","ES","FR","LV","LT","NL","PL","PT","FI","SE")

###number of gears per species and per country

Table_gear_hake <- gear_hake %>% 
  select(fishing_entity,catch_type, gear_type) %>%
  filter(catch_type == "Landings") %>%
  filter(!is.na(fishing_entity)) %>%
  group_by(fishing_entity) %>% 
  mutate(gear_num=(count = n_distinct(gear_type))) %>% # number of fishing gears per country
  ungroup() %>%
  select(fishing_entity, gear_num) %>%
  distinct ()

Table_gear_cod <- gear_cod %>% 
  select(fishing_entity,catch_type, gear_type) %>%
  filter(catch_type == "Landings") %>%
  filter(!is.na(fishing_entity)) %>%
  group_by(fishing_entity) %>% 
  mutate(gear_num=(count = n_distinct(gear_type))) %>% # number of fishing gears per country
  ungroup() %>%
  select(fishing_entity, gear_num) %>%
  distinct ()


Table_gear_hake [which(Table_gear_hake$gear_num == min(Table_gear_hake$gear_num)), ] ## 1
Table_gear_hake [which(Table_gear_hake$gear_num == max(Table_gear_hake$gear_num)), ] ## 16 ireland

Table_gear_cod [which(Table_gear_cod$gear_num == min(Table_gear_cod$gear_num)), ] ## 1
Table_gear_cod [which(Table_gear_cod$gear_num == max(Table_gear_cod$gear_num)), ] ## 22 sweden
