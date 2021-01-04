#ICES area where each country is fishing

library(dplyr)
library(tidyr)
library(ggplot2)


setwd("C:/Users/FOL/OneDrive/CLOCK_STUDENTS/Elena Fontan/Resilience Index Work/R scripts/Historical Landings Data R/Per country catch trends")

Belgium_Cod <- read.csv("Belgium_Cod example.csv", sep=";")
Belgium_Cod[Belgium_Cod=="-"] <- NA
Belgium_Cod[Belgium_Cod=="."] <- NA
Belgium_Cod[Belgium_Cod=="<0.5"] <- NA
Belgium_Cod[5:65] <- lapply(Belgium_Cod[5:65], function(x) as.numeric(as.character(x))) 
str(Belgium_Cod)

Belgium<- Belgium_Cod[, 5:65] #new matrix col 5:65 only
Belgium[Belgium>0]<- 1  #where data is >0 name 1
Belgium<- Belgium_Cod$Factor*Belgium #multiply Factor * bel (the new dataframe)
Belgium<- na.omit(Belgium) 
nareasBelgium<- colSums(Belgium)  #Suma las columnas por años para ver cuantas veces aparece pescando en un área.
nareasBelgium$country <- "Belgium"

Denmark_Cod <- read.csv("Denmark_Cod.csv", sep=";")
Denmark_Cod[Denmark_Cod=="-"]<- NA
Denmark_Cod[Denmark_Cod=="."]<- NA
Denmark_Cod[Denmark_Cod=="<0.5"]<- NA

Germany_Cod <- read.csv ("Germany_Cod.csv", sep=";")
Germany_Cod[Germany_Cod=="-"]<- NA
Germany_Cod[Germany_Cod=="."]<- NA
Germany_Cod[Germany_Cod=="<0.5"]<- NA

Estonia_Cod <- read.csv("Estonia_Cod.csv", sep = ";")
Estonia_Cod[Estonia_Cod=="-"]<- NA
Estonia_Cod[Estonia_Cod=="."]<- NA
Estonia_Cod[Estonia_Cod=="<0.5"]<-NA

Spain_Cod<- read.csv("Spain_Cod.csv", sep = ";")
Spain_Cod[Spain_Cod=="-"]<-NA
Spain_Cod[Spain_Cod=="."]<-NA
Spain_Cod[Spain_Cod=="<0.5"]<-NA

France_Cod<- read.csv("France_Cod.csv", sep = ";")
France_Cod[France_Cod=="-"]<- NA
France_Cod[France_Cod=="."]<- NA
France_Cod[France_Cod=="<0.5"]<- NA

Latvia_Cod<- read.csv("Latvia_Cod.csv", sep = ";")
Latvia_Cod[Latvia_Cod=="-"]<-NA
Latvia_Cod[Latvia_Cod=="."]<-NA
Latvia_Cod[Latvia_Cod=="<0.5"]<-NA

Lithuania_Cod<- read.csv("Lithuania_Cod.csv", sep = ";")
Lithuania_Cod[Lithuania_Cod=="-"]<-NA
Lithuania_Cod[Lithuania_Cod=="."]<-NA
Lithuania_Cod[Lithuania_Cod=="<0.5"]<-NA

Portugal_Cod<- read.csv("Portugal_Cod.csv", sep = ";")
Portugal_Cod[Portugal_Cod=="-"]<- NA
Portugal_Cod[Portugal_Cod=="."]<- NA
Portugal_Cod[Portugal_Cod=="<0.5"]<- NA

Netherlands_Cod<- read.csv("Netherlands_Cod.csv", sep = ";")
Netherlands_Cod[Netherlands_Cod=="-"]<- NA
Netherlands_Cod[Netherlands_Cod=="."]<- NA
Netherlands_Cod[Netherlands_Cod=="<0.5"]<- NA

Poland_Cod<- read.csv("Poland_Cod.csv", sep = ";")
Poland_Cod[Poland_Cod=="-"]<- NA
Poland_Cod[Poland_Cod=="."]<- NA
Poland_Cod[Poland_Cod=="<0.5"]<- NA





