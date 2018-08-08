


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

library("e1071")
library(MASS)
library(randomForest)
library("ggRandomForests") 
library(ggplot2)
library(gbm)
library(tree)


##### 1. Read factors #####


eco_factors <- fread("data/ecological_factors_country.csv")
eco_factors<- eco_factors[,3:6]

ins_factors <- fread("data/institutional_factors_country.csv") 
ins_factors<- ins_factors[,3:6]

soc_factors <- fread("data/socioeconomic_factors_country.csv") 
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
write.csv(rf,file="data/rf.csv",row.names = FALSE)

##### 3. Random Forest #####
dim(rf)
#[1] 66 12
set.seed(1)
train=sample(1:nrow(rf),20) #training Sample with 20 observations
index.rf=randomForest(Resilience_Index ~ . , data = rf , subset = train, na.action=na.roughfix,calibrate = TRUE)
index.rf
plot(index.rf)


plot(gg_vimp(index.rf))

set.seed(1)
train = sample(1:nrow(rf), nrow(rf)/2) 
index.rf=randomForest(Resilience_Index ~ . , data = rf , subset = train, na.action=na.roughfix,calibrate = TRUE)
index.rf
plot(index.rf)


plot(gg_vimp(index.rf))


set.seed(1)
train = sample(1:nrow(rf), nrow(rf)/2)
tree.rf=tree(Resilience_Index~.,rf,subset=train, na.action=na.roughfix)
summary(tree.rf)
plot(tree.rf)
text(tree.rf,pretty=0)
yh=predict(tree.rf,newdata=rf[-train,])
rf.test=rf[-train,"Resilience_Index"]
#plot(yhat,boston.test)
#abline(0,1)
mean((yh-rf.test)^2)

set.seed(1)
bag.boston=randomForest(Resilience_Index~.,data=rf,subset=train,mtry=11,importance=TRUE, na.action=na.roughfix)
bag.boston

set.seed(1)
train1=sample(1:nrow(rf),50) #training Sample with 50 observations
index.rf1=randomForest(Resilience_Index ~ . , data = rf , subset = train1, na.action=na.roughfix, calibrate = TRUE)
index.rf1
plot(index.rf1)


plot(gg_vimp(index.rf1))

set.seed(1)
train5=sample(1:nrow(rf),40) #training Sample with 40 observations
index.rf5=randomForest(Resilience_Index ~ . , data = rf , subset = train5, na.action=na.roughfix, calibrate = TRUE)
index.rf5
plot(index.rf5)


plot(gg_vimp(index.rf1))

 #NO training Sample 
set.seed(1)
index.rf2=rfsrc(Resilience_Index ~ . , data = rf )
index.rf2
plot(index.rf2)


plot(gg_vimp(index.rf2))

#Predictions (training)
set.seed(1)
train=sample(1:nrow(rf),20) #training Sample with 20 observations
index.rfa=rfsrc(Resilience_Index ~ . , data = rf, subset = train, importance = TRUE)
index.rfa
plot(index.rfa)


plot(gg_vimp(index.rfa))

#Predictions (no training)
set.seed(1)
index.rf3=rfsrc(Resilience_Index ~ . , data = rf, importance = TRUE)
index.rf3
plot(index.rf3)


plot(gg_vimp(index.rf3))

#Plot predicted median home values.
plot(gg_rfsrc(index.rf), alpha=.5)





#BOSTON data
library(MASS)
set.seed(1)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
#plot(yhat,boston.test)
#abline(0,1)
mean((yhat-boston.test)^2)

data("Boston")
str(Boston)
dim(Boston)
set.seed(1)
trainB=sample(1:nrow(Boston),300) #training Sample with 50 observations

a=randomForest(medv ~ . , data = Boston , subset = trainB, na.action=na.roughfix)
a
#plot(a)

set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

plot(gg_vimp(a))
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)


##### 4. SVM #####
head(iris, 5)
attach(iris)
x<- subset(iris, select=-Species)
y<- Species
svm_model <- svm(Species ~ ., data=iris)
summary(svm_model)
pred <- predict(svm_model,x)
system.time(pred <- predict(svm_model,x))
table(pred, y)


rf<- rf[,c(1,12)]

attach(rf)
x1<- subset(rf, select=-Resilience_Index)
y1<- Resilience_Index
na.pass(rf)
svm_model1 <- svm(Resilience_Index ~ ., data=rf)
summary(svm_model1)

pred <- predict(svm_model1,x1)
system.time(pred <- predict(svm_model1,x1))
table(pred, y1)



