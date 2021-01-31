

if(!require(randomForest)){
  install.packages('randomForest',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
if(!require(tree)){
  install.packages('tree',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
if(!require(ggRandomForests)){
  install.packages('ggRandomForests',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
if(!require(MASS)){
  install.packages('MASS',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
if(!require(gbm)){
  install.packages('gbm',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
if(!require(e1071)){
  install.packages('e1071',dependencies = TRUE,repos='http://cran.us.r-project.org')
}

library(e1071)
library(MASS)
library(randomForest)
library(ggRandomForests) 
library(ggplot2)
library(gbm)
library(tree)
library(tidyverse)
library(data.table)


##### 1. Read factors #####


eco_factors <- fread("data/ecological_factors_country.csv")
colnames(eco_factors)[1] = "COUNTRIES"
colnames(eco_factors)[2] = "SPECIES"

ins_factors <- fread("data/institutional_factors_country.csv") 
#ins_factors<- ins_factors[,3:6]

soc_factors <- fread("data/socioeconomic_factors_country.csv") 
#soc_factors<- soc_factors[,3:5]

resilience_index <- fread("data/final_index.csv") 
resilience_index<- resilience_index[,c(1:3,9)]
colnames(resilience_index)[1] = "SPECIES"



resilience_index <- resilience_index %>% 
  mutate(SPECIES = ifelse(as.character(SPECIES) == "Hake", "European hake", as.character(SPECIES))) %>%
  mutate(SPECIES = ifelse(as.character(SPECIES) == "Cod", "Atlantic cod", as.character(SPECIES)))



##### 2. Compute dataframe #####
TableFactors <- reduce(
  list(
    eco_factors,
    soc_factors,
    ins_factors,
    resilience_index
  ),full_join,by = c("COUNTRIES", "SPECIES"))

write.csv(TableFactors,file="data/AllFactors.csv",row.names = FALSE)

##### 3. Random Forest #####
dim(TableFactors)

TableFactors$COUNTRIES <- as.factor(TableFactors$COUNTRIES)
TableFactors$SPECIES <- as.factor(TableFactors$SPECIES)
TableFactors$DIMENSION <- as.factor(TableFactors$DIMENSION)

TableFactors <- TableFactors[ ,c(-1, -2, -15)]  #droping dim species and country coluns for RF


#[1] 66 12
set.seed(1)
train=sample(1:nrow(TableFactors),20) #training Sample with 20 observations
index.rf=randomForest(Resilience_Index ~ . , data = TableFactors , subset = train, importance=TRUE, na.action=na.roughfix, calibrate = TRUE)

importance(index.rf)
importanceSD(index.rf)




plot(gg_vimp(index.rf))



#'Mean Decrease Accuracy (%IncMSE) - 
#'This shows how much our model accuracy decreases if we leave out that variable

png(file="Figures/RandomForest.png",
    width=600, height=350)
varImpPlot(index.rf,type=1, main="country resilience index") # let's save the varImp object
dev.off()




########extra code############
# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$var_categ <- rep(1:2, 5) # random var category
ggplot(imp, aes(x=reorder(varnames, IncNodePurity), y=IncNodePurity, color=as.factor(var_categ))) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=IncNodePurity)) +
  scale_color_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  coord_flip()

