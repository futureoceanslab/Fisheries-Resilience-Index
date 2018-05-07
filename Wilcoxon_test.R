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

library(MASS) 

##### 2. READ DATA #####

final_index_1 <- read_csv("data/final_index_1.csv")
final_index_2 <- read_csv("data/final_index_2.csv")
final_index_above <- read_csv("data/final_index_above_TAC.csv")

##### 3. WILCOXON TEST #####

wilcox.test(final_index_1$Resilience_Index, final_index_2$Resilience_Index, paired=TRUE)

wilcox.test(final_index_1$Resilience_Index, final_index_1$Resilience_Index, paired=TRUE)

wilcox.test(final_index_1$Resilience_Index, final_index_above$Resilience_Index, paired=TRUE)






