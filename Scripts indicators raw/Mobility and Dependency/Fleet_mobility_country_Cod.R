#ICES area where each country is fishing

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/FOL/OneDrive/CLOCK_STUDENTS/Elena Fontan/Resilience Index Work/R scripts/Historical Landings Data R/Per country catch trends")#setwd("~/OneDrive/CLOCK_STUDENTS/Elena Fontan/TFM/R scripts/R per countries")


Belgium_Cod <- read.csv("Belgium_Cod example.csv", sep=";")
Belgium_Cod[Belgium_Cod=="-"] <- NA
Belgium_Cod[Belgium_Cod=="."] <- NA
Belgium_Cod[Belgium_Cod=="<0.5"] <- NA
Belgium_Cod[5:65] <- lapply(Belgium_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Belgium_Cod)
Belgium<- Belgium_Cod[, 5:65] #new matrix col 5:65 only
Belgium[Belgium>0]<- 1  #where data is >0 name 1
Belgium<- Belgium_Cod$Factor*Belgium #multiply Factor * the new dataframe
nareasBelgium<- t(as.data.frame(colSums(Belgium, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasBelgium)<- "Belgium"
#nareasBelgium$Year <- rownames(nareasBelgium)
#nareasBelgium$Country <-  "Belgium"



Denmark_Cod <- read.csv("Denmark_Cod.csv", sep=";")
Denmark_Cod[Denmark_Cod=="-"]<- NA
Denmark_Cod[Denmark_Cod=="."]<- NA
Denmark_Cod[Denmark_Cod=="<0.5"]<- NA
Denmark_Cod[5:65] <- lapply(Denmark_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Denmark_Cod)
Denmark<- Denmark_Cod[, 5:65] #new matrix col 5:65 only
Denmark[Denmark>0]<- 1  #where data is >0 name 1
Denmark<- Denmark_Cod$Factor*Denmark #multiply Factor * the new dataframe
nareasDenmark<- t(as.data.frame(colSums(Denmark, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasDenmark)<- "Denmark"
#nareasDenmark$Year <- rownames(nareasDenmark)
#nareasDenmark$Country <-  "Denmark"


Germany_Cod <- read.csv ("Germany_Cod.csv", sep=";")
Germany_Cod[Germany_Cod=="-"]<- NA
Germany_Cod[Germany_Cod=="."]<- NA
Germany_Cod[Germany_Cod=="<0.5"]<- NA
Germany_Cod[5:65] <- lapply(Germany_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Germany_Cod)
Germany<- Germany_Cod[, 5:65] #new matrix col 5:65 only
Germany[Germany>0]<- 1  #where data is >0 name 1
Germany<- Germany_Cod$Factor*Germany #multiply Factor * the new dataframe
nareasGermany<- t(as.data.frame(colSums(Germany, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasGermany)<- "Germany"

Estonia_Cod <- read.csv("Estonia_Cod.csv", sep = ";")
Estonia_Cod[Estonia_Cod=="-"]<- NA
Estonia_Cod[Estonia_Cod=="."]<- NA
Estonia_Cod[Estonia_Cod=="<0.5"]<-NA
Estonia_Cod[5:65] <- lapply(Estonia_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Estonia_Cod)
Estonia<- Estonia_Cod[, 5:65] #new matrix col 5:65 only
Estonia[Estonia>0]<- 1  #where data is >0 name 1
Estonia<- Estonia_Cod$Factor*Estonia #multiply Factor * the new dataframe
nareasEstonia<- t(as.data.frame(colSums(Estonia, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasEstonia)<- "Estonia"


Spain_Cod<- read.csv("Spain_Cod.csv", sep = ";")
Spain_Cod[Spain_Cod=="-"]<-NA
Spain_Cod[Spain_Cod=="."]<-NA
Spain_Cod[Spain_Cod=="<0.5"]<-NA
Spain_Cod[5:65] <- lapply(Spain_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Spain_Cod)
Spain<- Spain_Cod[, 5:65] #new matrix col 5:65 only
Spain[Spain>0]<- 1  #where data is >0 name 1
Spain<- Spain_Cod$Factor*Spain #multiply Factor * the new dataframe
nareasSpain<- t(as.data.frame(colSums(Spain, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasSpain)<- "Spain"

France_Cod<- read.csv("France_Cod.csv", sep = ";")
France_Cod[France_Cod=="-"]<- NA
France_Cod[France_Cod=="."]<- NA
France_Cod[France_Cod=="<0.5"]<- NA
France_Cod[5:65] <- lapply(France_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(France_Cod)
France<- France_Cod[, 5:65] #new matrix col 5:65 only
France[France>0]<- 1  #where data is >0 name 1
France<- France_Cod$Factor*France #multiply Factor * the new dataframe
nareasFrance<- t(as.data.frame(colSums(France, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasFrance)<- "France"

Latvia_Cod<- read.csv("Latvia_Cod.csv", sep = ";")
Latvia_Cod[Latvia_Cod=="-"]<-NA
Latvia_Cod[Latvia_Cod=="."]<-NA
Latvia_Cod[Latvia_Cod=="<0.5"]<-NA
Latvia_Cod[5:65] <- lapply(Latvia_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Latvia_Cod)
Latvia<- Latvia_Cod[, 5:65] #new matrix col 5:65 only
Latvia[Latvia>0]<- 1  #where data is >0 name 1
Latvia<- Latvia_Cod$Factor*Latvia #multiply Factor * the new dataframe
nareasLatvia<- t(as.data.frame(colSums(Latvia, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasLatvia)<- "Latvia"


Lithuania_Cod<- read.csv("Lithuania_Cod.csv", sep = ";")
Lithuania_Cod[Lithuania_Cod=="-"]<-NA
Lithuania_Cod[Lithuania_Cod=="."]<-NA
Lithuania_Cod[Lithuania_Cod=="<0.5"]<-NA
Lithuania_Cod[5:65] <- lapply(Lithuania_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Lithuania_Cod)
Lithuania<- Lithuania_Cod[, 5:65] #new matrix col 5:65 only
Lithuania[Lithuania>0]<- 1  #where data is >0 name 1
Lithuania<- Lithuania_Cod$Factor*Lithuania #multiply Factor * the new dataframe
nareasLithuania<- t(as.data.frame(colSums(Lithuania, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasLithuania)<- "Lithuania"


Portugal_Cod<- read.csv("Portugal_Cod.csv", sep = ";")
Portugal_Cod[Portugal_Cod=="-"]<- NA
Portugal_Cod[Portugal_Cod=="."]<- NA
Portugal_Cod[Portugal_Cod=="<0.5"]<- NA
Portugal_Cod[5:65] <- lapply(Portugal_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Portugal_Cod)
Portugal<- Portugal_Cod[, 5:65] #new matrix col 5:65 only
Portugal[Portugal>0]<- 1  #where data is >0 name 1
Portugal<- Portugal_Cod$Factor*Portugal #multiply Factor * the new dataframe
nareasPortugal<- t(as.data.frame(colSums(Portugal, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasPortugal)<- "Portugal"


Netherlands_Cod<- read.csv("Netherlands_Cod.csv", sep = ";")
Netherlands_Cod[Netherlands_Cod=="-"]<- NA
Netherlands_Cod[Netherlands_Cod=="."]<- NA
Netherlands_Cod[Netherlands_Cod=="<0.5"]<- NA
Netherlands_Cod[5:65] <- lapply(Netherlands_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Netherlands_Cod)
Netherlands<- Netherlands_Cod[, 5:65] #new matrix col 5:65 only
Netherlands[Netherlands>0]<- 1  #where data is >0 name 1
Netherlands<- Netherlands_Cod$Factor*Netherlands #multiply Factor * the new dataframe
nareasNetherlands<- t(as.data.frame(colSums(Netherlands, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasNetherlands)<- "Netherlands"


Poland_Cod<- read.csv("Poland_Cod.csv", sep = ";")
Poland_Cod[Poland_Cod=="-"]<- NA
Poland_Cod[Poland_Cod=="."]<- NA
Poland_Cod[Poland_Cod=="<0.5"]<- NA
Poland_Cod[5:65] <- lapply(Poland_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Poland_Cod)
Poland<- Poland_Cod[, 5:65] #new matrix col 5:65 only
Poland[Poland>0]<- 1  #where data is >0 name 1
Poland<- Poland_Cod$Factor*Poland #multiply Factor * the new dataframe
nareasPoland<- t(as.data.frame(colSums(Poland, na.rm=TRUE)))  #Suma las columnas por anos para ver cuantas veces aparece pescando en un area.
rownames(nareasPoland)<- "Poland"
#nareasPoland$Country<- rownames(Poland)



allcountries<- rbind(nareasBelgium, nareasDenmark, nareasEstonia, nareasFrance, nareasGermany, nareasLatvia, nareasSpain, nareasLithuania, nareasNetherlands, nareasPoland, nareasPortugal)
colnames(allcountries) <- c(1950:2010)
#all<- t(allcountries)
allcountries<- as.data.frame(allcountries)
allcountries$country <- rownames(allcountries)

library(reshape2)
allcountries1 <- melt(allcountries, id.vars = "country")
colnames(allcountries1)<- c("country", "year", "nareas")
allcountries1[allcountries1==0]<-NA
allcountries1$year<-as.numeric(as.character(allcountries1$year))


#summaries and subsetsy
allcountries1$groupEU[allcountries1$year <= 1982]<- 0
allcountries1$groupEU[allcountries1$year >= 1983]<- 1
meansEU <- aggregate( nareas~country+groupEU, allcountries1, mean )
write.csv(meansEU, "meansEU.csv")

library(dplyr)

meanvalues<- group_by(allfive, country) %>% summarize(m = mean(nareas)) #numero de areas por pais  en los ultimos 5 anos


ggplot(data=allfive, aes(x=year, y=nareas, group=factor(country), color=factor(country))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))



ggplot(data=subset(allcountries1, country=="Germany"), aes(x=year, y=nareas)) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))