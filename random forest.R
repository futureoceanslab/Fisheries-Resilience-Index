

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
if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(gridExtra)
packageVersion("gridExtra")
# [1] ‘2.2.1’

if(!require(magrittr)){
  install.packages('magrittr',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(magrittr)
packageVersion("magrittr")
# [1] ‘1.5’

library(car) #for VIFs
library(randomForest)
source("aux_functions.R")
library(gridExtra)


##### 1. Read factors #####

#Datos
d <- read.csv("Data/AllFactorsRI.csv")

#droping species and country colums for RF
d <- d[ ,-c(1:2)]  

#sustituyo NAs por la mediana en los datos directamente (así evito errores en partialplot)
#he puesto mediana porque lo teniaas asi en el RF
d$CATCH.DEP[is.na(d$CATCH.DEP)] <- median(d$CATCH.DEP, na.rm = T)
d$FLEET.MOBILITY[is.na(d$FLEET.MOBILITY)] <- median(d$FLEET.MOBILITY, na.rm = T)
d$GEAR.DIVERSITY[is.na(d$GEAR.DIVERSITY)] <- median(d$GEAR.DIVERSITY, na.rm = T)
d$ORGANIZATION[is.na(d$ORGANIZATION)] <- median(d$ORGANIZATION, na.rm = T)

#Mirar colinearidad. Con la mediana mejora.
v <- lm(Resilience_Index ~. , d)
as.data.frame(vif(v)) #OVEREX valor alto

#RF
#Con todaas las variables:
#Al final he dejado valores por defecto. No sé si habría que probar k-fold cross validation
set.seed(123)
rf <- randomForest(Resilience_Index ~., data = d, 
                   importance = T, 
                   ntree = 1000); rf
#variables explain 44% of the data variance
varImpPlot(rf, type = 1, main ="Resilience Index")
importance(rf)

#Plots del efecto marginal de las variables versus el index
png("Figures/RF1.png") 
partialPlot2(rf, d, OVEREXPLOITATION,
             main = "Partial Dependence on Overexploitation", xlab = "Overexploitation"
)
dev.off() 

png("Figures/RF2.png") 
partialPlot2(rf, d, TEMPERATURE,
             main = "Partial Dependence on temperature",xlab = "Temperature (?C)"
)#sólo hay dos valores diferentes de temperatura en los datos!
dev.off() 

png("Figures/RF3.png") 
partialPlot2(rf, d, ABUNDANCE)
dev.off() 

png("Figures/RF4.png") 
partialPlot2(rf, d, CATCH.DEP)
dev.off() 

png("Figures/RF5.png") 
partialPlot2(rf, d, ADAPTIVE.MNG)
dev.off() 

#Evolucion del error con el numero de arboles
png("Figures/RF6.png") 
plot(rf, main = paste("Pseudo R2 (",round(rf[["rsq"]][1000], 2),")"))
dev.off() 

##to save results random forest

varImpPlot(rf, type = 1, main ="Resilience Index")

png("Figures/RandomForest.png",width=9,height=7,units="in",res=600)

dev.off()
