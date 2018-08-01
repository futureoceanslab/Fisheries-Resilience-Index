


if(!require(randomForest)){
  install.packages('randomForest',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(randomForest)

rf_classifier = randomForest(Species ~ ., data=training, ntree=100, mtry=2, importance=TRUE)



##### 1. Read factors #####


eco_factors <- fread("data/ecological_factors_country.csv") %>% mutate(DIMENSION="ecological")

ins_factors <- fread("data/institutional_factors_country.csv")  %>% mutate(DIMENSION="institutional")

soc_factors <- fread("data/socioeconomic_factors_country.csv")  %>% mutate(DIMENSION="socioeconomic")

##### 2. Compute Resilience Index #####

# Compute resilience index. For that put together all the factors computed above and compute 
# their mean by species, dimension and country

resilience_index <-cbind (eco_factors, soc_factors, by=COUNTRIES)


resilience_index <- bind_rows(eco_factors,ins_factors,soc_factors) %>% 
  gather(FACTOR,VALUE,-COUNTRIES,-SPECIES,-DIMENSION,factor_key = TRUE) %>%
  filter(complete.cases(.)) %>%
  group_by(SPECIES,DIMENSION,COUNTRIES) %>% 
  summarise(Resilience_Index=mean(VALUE,na.rm=TRUE)) %>%
  ungroup()