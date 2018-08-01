


if(!require(randomForest)){
  install.packages('randomForest',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
if(!require(ggRandomForests)){
  install.packages('ggRandomForests',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
if(!require(MASS)){
  install.packages('MASS',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(MASS)
library(randomForest)
library("ggRandomForests") 
library(ggplot2)



##### 1. Read factors #####


eco_factors <- fread("data/ecological_factors_country.csv") %>% mutate(DIMENSION="ecological")
eco_factors<- eco_factors[,3:6]

ins_factors <- fread("data/institutional_factors_country.csv")  %>% mutate(DIMENSION="institutional")
ins_factors<- ins_factors[,3:6]

soc_factors <- fread("data/socioeconomic_factors_country.csv")  %>% mutate(DIMENSION="socioeconomic")
soc_factors<- soc_factors[,3:5]

resilience_index <- fread("data/final_index.csv") 
resilience_index<- resilience_index[,c(9)]


##### 2. Compute dataframe #####

rf1 <- cbind(eco_factors, ins_factors)
str(rf1)
rf2 <- cbind(rf1, soc_factors)
str(rf2)
rf <- cbind(rf2, resilience_index)
str(rf)

##### 3. Random Forest #####
dim(rf)
#[1] 66 12

train=sample(1:nrow(rf),20) #training Sample with 20 observations
index.rf=randomForest(Resilience_Index ~ . , data = rf , subset = train, na.action=na.roughfix, calibrate = TRUE)
index.rf
plot(index.rf)


plot(gg_vimp(index.rf))


train=sample(1:nrow(rf),50) #training Sample with 50 observations
index.rf=randomForest(Resilience_Index ~ . , data = rf , subset = train, na.action=na.roughfix, calibrate = TRUE)
index.rf
plot(index.rf)


plot(gg_vimp(index.rf))


#no training
index.rf=randomForest(Resilience_Index ~ . , data = rf ,  na.action=na.roughfix)
index.rf
plot(index.rf)


plot(gg_vimp(index.rf))


#BOSTON data

data("Boston")
str(Boston)
dim(Boston)
trainB=sample(1:nrow(Boston),300) #training Sample with 50 observations
a=randomForest(medv ~ . , data = Boston , subset = trainB, na.action=na.roughfix)
a
plot(a)


plot(gg_vimp(a))
