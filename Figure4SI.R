#' Reads data/ecological_indicators.csv, data/institutional_indicators.csv and 
#' data/socioeconomic_indicators.csv. Computes correlation matrices for each set of
#' indicators and plots the correlation matrices in Figures/Fig 4 SI.png

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

if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(gridExtra)
packageVersion("gridExtra")
# [1] ‘2.2.1’

if(!require(corrplot)){
  install.packages("corrplot",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(corrplot)
packageVersion("corrplot")
# [1] ‘0.77’

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



##### 2. READ DATA #####

# Read the data

eco_indicators <- fread("data/ecological_indicators.csv")
                            
ins_indicators <- fread("data/institutional_indicators 2.csv")
                            
soc_indicators <- fread("data/socioeconomic_indicators.csv")

##### 3. PLOT #####

png("Figures/Fig 4 SI.png",width=12.5,height = 4.5,units = "in",res=300)

par(mfrow = c(1, 3))

# socioeconomic correlations

soc_to_correlate <- soc_indicators %>% select(-SPECIES,-COUNTRIES,-STOCK)
soc_correlations <- cor(soc_to_correlate, use="pairwise.complete.obs")
p1 <- corrplot(soc_correlations, order ="AOE")


# institutional correlations

ins_to_correlate <- ins_indicators %>% select(-SPECIES,-COUNTRIES,-STOCK)
ins_correlations <- cor(ins_to_correlate, use="pairwise.complete.obs")
p2 <- corrplot(ins_correlations, order ="AOE")


# Ecological correlations

eco_to_correlate <- eco_indicators %>%
  mutate(SSBrecent=B_SSBrecent/SSB.average,SSBhistoric=B_SSBhistoric/SSB.average,Ftrend=B_Ftrend/F.average,Rtrend=B_Rtrend/R.average)  %>% 
  select(-SPECIES,-STOCK,-starts_with("B_"),-ends_with(".average"))
eco_correlations <- cor(eco_to_correlate, use="pairwise.complete.obs")
p3 <- corrplot(eco_correlations, order ="AOE")

par(mfrow=c(1,1))

dev.off()
