###Code for country and Catch historic data ICES
##15/May/2017


setwd("C:/Users/FOL/OneDrive/CLOCK_STUDENTS/Elena Fontan/Resilience Index Work/R scripts/Historical Landings Data R/Per country catch trends")

#Read database
historicdatatotal <- read.csv("ICES_1950-2010.csv", sep=",")
historicdatacod <- subset(historicdatatotal, historicdatatotal$Species=="Atlantic cod") 


#install.packages("ggplot2", "dplyr", "tidyr")
library(dplyr)
library(tidyr)
library(ggplot2)


#

historicdatacod[historicdatacod=="-"] <- NA
historicdatacod[historicdatacod=="."] <- NA
historicdatacod[historicdatacod=="<0.5"] <- 0.5

colnames(historicdatacod) <- c("Country", "Species", "Division", c(1950:2010))   #to delete the X before the year
A <- c("Belgium", "Bulgaria", "Denmark","Estonia", "Faeroe Islands", "Finland", "France", "Germany", "Greenland", "Iceland", "Ireland", "Latvia", "Lithuania", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russian Federation", "Spain", "Sweden")
B <- c("Belgium", "Bulgaria", "Denmark","Estonia", "Faeroe Islands", "Finland", "France", "Germany", "Greenland", "Iceland", "Ireland", "Latvia", "Lithuania", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russian Federation", "Spain", "Sweden")
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  texto <- B[i]
  
  {
    country <-historicdatacod[grep(texto, historicdatacod$Country),]
    n <- dim(country)[1]
    country$country <- rep(texto,n)
  }
  AreaMatrix[[i]] <- country
}

Belgium <- AreaMatrix[[1]]
Bulgaria <- AreaMatrix[[2]]
Denmark <- AreaMatrix[[3]]
Estonia <- AreaMatrix[[4]]
Faroe <- AreaMatrix[[5]]
Finland <- AreaMatrix[[6]]
France <- AreaMatrix[[7]]
Germany <- AreaMatrix[[8]]
Greenland <- AreaMatrix[[9]]
Iceland <- AreaMatrix[[10]]
Ireland <- AreaMatrix[[11]]
Latvia <- AreaMatrix[[12]]
Lithuania <- AreaMatrix[[13]]
Netherlands<- AreaMatrix[[14]]
Norway <- AreaMatrix[[15]]
Poland <- AreaMatrix[[16]]
Portugal <- AreaMatrix[[17]]
Romania <- AreaMatrix[[18]]
Russia <- AreaMatrix[[19]]
Spain <- AreaMatrix[[20]]
Sweden <- AreaMatrix[[21]]

#Belgium catches
Belgium[4:64] <- lapply(Belgium[4:64], function(x) as.numeric(as.character(x))) 
catchesBelgium<-colSums(Belgium[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataBelgium <- as.data.frame(catchesBelgium)           #to get a data frame for the catches per year per country
dataBelgium$Year <- rownames(dataBelgium)              #to add the years
dataBelgium$Country <- "Belgium"           ##add the country (It does not work)
dataBelgium <-spread(dataBelgium, Year,catchesBelgium)      #to have data in rows

#Graphic Belgium
dataBelgium <- gather(dataBelgium, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataBelgium, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataBelgium, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataBelgium, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataBelgium$Year<- as.numeric(as.character(dataBelgium$Year, na.omit=TRUE))
str(dataBelgium)
abnBelgium <- lm(formula = Catches ~Year, data=dataBelgium)
summary(abnBelgium)
abnBelgium1 <- lm(formula = Catches ~ Year, data=subset(dataBelgium, Year >= 1990))
summary(abnBelgium1)
abnBelgium2 <- lm(formula = Catches ~ Year, data=subset(dataBelgium, Year >= 2000))
summary(abnBelgium2)


#Bulgaria catches
Bulgaria[4:64] <- lapply(Bulgaria[4:64], function(x) as.numeric(as.character(x))) 
catchesBulgaria<-colSums(Bulgaria[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataBulgaria <- as.data.frame(catchesBulgaria)           #to get a data frame for the catches per year per country
dataBulgaria$Year <- rownames(dataBulgaria)              #to add the years
dataBulgaria$Country <- "Bulgaria"           ##add the country (It does not work)
dataBulgaria <-spread(dataBulgaria, Year,catchesBulgaria)      #to have data in rows

#Graphic Bulgaria
dataBulgaria <- gather(dataBulgaria, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataBulgaria, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataBulgaria, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataBulgaria, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataBulgaria$Year<- as.numeric(as.character(dataBulgaria$Year, na.omit=TRUE))
str(dataBelgium)
abnBulgaria <- lm(formula = Catches ~Year, data=dataBulgaria)
summary(abnBulgaria)
abnBulgaria1 <- lm(formula = Catches ~ Year, data=subset(dataBulgaria, Year >= 1990))
summary(abnBulgaria1)
abnBulgaria2 <- lm(formula = Catches ~ Year, data=subset(dataBulgaria, Year >= 2000))
summary(abnBulgaria2)

#Denmark catches
Denmark[4:64] <- lapply(Denmark[4:64], function(x) as.numeric(as.character(x))) 
catchesDenmark<-colSums(Denmark[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataDenmark <- as.data.frame(catchesDenmark)           #to get a data frame for the catches per year per country
dataDenmark$Year <- rownames(dataDenmark)              #to add the years
dataDenmark$Country <- "Denmark"           ##add the country (It does not work)
dataDenmark <-spread(dataDenmark, Year,catchesDenmark)      #to have data in rows

#Graphic Denmark
dataDenmark <- gather(dataDenmark, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataDenmark, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataDenmark, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataDenmark, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataDenmark$Year<- as.numeric(as.character(dataDenmark$Year, na.omit=TRUE))
str(dataBelgium)
abnDenmark <- lm(formula = Catches ~Year, data=dataDenmark)
summary(abnDenmark)
abnDenmark1 <- lm(formula = Catches ~ Year, data=subset(dataDenmark, Year >= 1990))
summary(abnDenmark1)
abnDenmark2 <- lm(formula = Catches ~ Year, data=subset(dataDenmark, Year >= 2000))
summary(abnDenmark2)

#Estonia catches
Estonia[4:64] <- lapply(Estonia[4:64], function(x) as.numeric(as.character(x))) 
catchesEstonia<-colSums(Estonia[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataEstonia <- as.data.frame(catchesEstonia)           #to get a data frame for the catches per year per country
dataEstonia$Year <- rownames(dataEstonia)              #to add the years
dataEstonia$Country <- "Estonia"           ##add the country (It does not work)
dataEstonia <-spread(dataEstonia, Year,catchesEstonia)      #to have data in rows

#Graphic Estonia
dataEstonia <- gather(dataEstonia, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataEstonia, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataEstonia, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataEstonia, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataEstonia$Year<- as.numeric(as.character(dataEstonia$Year, na.omit=TRUE))
str(dataBelgium)
abnEstonia <- lm(formula = Catches ~Year, data=dataEstonia)
summary(abnEstonia)
abnEstonia1 <- lm(formula = Catches ~ Year, data=subset(dataEstonia, Year >= 1990))
summary(abnEstonia1)
abnEstonia2 <- lm(formula = Catches ~ Year, data=subset(dataEstonia, Year >= 2000))
summary(abnEstonia2)

#Faroe catches
Faroe[4:64] <- lapply(Faroe[4:64], function(x) as.numeric(as.character(x))) 
catchesFaroe<-colSums(Faroe[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataFaroe <- as.data.frame(catchesFaroe)           #to get a data frame for the catches per year per country
dataFaroe$Year <- rownames(dataFaroe)              #to add the years
dataFaroe$Country <- "Faroe"           ##add the country (It does not work)
dataFaroe <-spread(dataFaroe, Year,catchesFaroe)      #to have data in rows

#Graphic Faroe
dataFaroe <- gather(dataFaroe, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataFaroe, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataFaroe, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataFaroe, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataFaroe$Year<- as.numeric(as.character(dataFaroe$Year, na.omit=TRUE))
str(dataBelgium)
abnFaroe <- lm(formula = Catches ~Year, data=dataFaroe)
summary(abnFaroe)
abnFaroe1 <- lm(formula = Catches ~ Year, data=subset(dataFaroe, Year >= 1990))
summary(abnFaroe1)
abnFaroe2 <- lm(formula = Catches ~ Year, data=subset(dataFaroe, Year >= 2000))
summary(abnFaroe2)

#Finland catches
Finland[4:64] <- lapply(Finland[4:64], function(x) as.numeric(as.character(x))) 
catchesFinland<-colSums(Finland[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataFinland <- as.data.frame(catchesFinland)           #to get a data frame for the catches per year per country
dataFinland$Year <- rownames(dataFinland)              #to add the years
dataFinland$Country <- "Finland"           ##add the country (It does not work)
dataFinland <-spread(dataFinland, Year,catchesFinland)      #to have data in rows

#Graphic Finland
dataFinland <- gather(dataFinland, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataFinland, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataFinland, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataFinland, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataFinland$Year<- as.numeric(as.character(dataFinland$Year, na.omit=TRUE))
str(dataBelgium)
abnFinland <- lm(formula = Catches ~Year, data=dataFinland)
summary(abnFinland)
abnFinland1 <- lm(formula = Catches ~ Year, data=subset(dataFinland, Year >= 1990))
summary(abnFinland1)
abnFinland2 <- lm(formula = Catches ~ Year, data=subset(dataFinland, Year >= 2000))
summary(abnFinland2)

#France catches
France[4:64] <- lapply(France[4:64], function(x) as.numeric(as.character(x))) 
catchesFrance<-colSums(France[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataFrance <- as.data.frame(catchesFrance)           #to get a data frame for the catches per year per country
dataFrance$Year <- rownames(dataFrance)              #to add the years
dataFrance$Country <- "France"           ##add the country (It does not work)
dataFrance <-spread(dataFrance, Year,catchesFrance)      #to have data in rows

#Graphic France
dataFrance <- gather(dataFrance, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataFrance, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataFrance, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataFrance, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataFrance$Year<- as.numeric(as.character(dataFrance$Year, na.omit=TRUE))
str(dataBelgium)
abnFrance <- lm(formula = Catches ~Year, data=dataFrance)
summary(abnFrance)
abnFrance1 <- lm(formula = Catches ~ Year, data=subset(dataFrance, Year >= 1990))
summary(abnFrance1)
abnFrance2 <- lm(formula = Catches ~ Year, data=subset(dataFrance, Year >= 2000))
summary(abnFrance2)

#Germany catches
Germany[4:64] <- lapply(Germany[4:64], function(x) as.numeric(as.character(x))) 
catchesGermany<-colSums(Germany[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataGermany <- as.data.frame(catchesGermany)           #to get a data frame for the catches per year per country
dataGermany$Year <- rownames(dataGermany)              #to add the years
dataGermany$Country <- "Germany"           ##add the country (It does not work)
dataGermany <-spread(dataGermany, Year,catchesGermany)      #to have data in rows

#Graphic Germany
dataGermany <- gather(dataGermany, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataGermany, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataGermany, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataGermany, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataGermany$Year<- as.numeric(as.character(dataGermany$Year, na.omit=TRUE))
str(dataBelgium)
abnGermany <- lm(formula = Catches ~Year, data=dataGermany)
summary(abnGermany)
abnGermany1 <- lm(formula = Catches ~ Year, data=subset(dataGermany, Year >= 1990))
summary(abnGermany1)
abnGermany2 <- lm(formula = Catches ~ Year, data=subset(dataGermany, Year >= 2000))
summary(abnGermany2)

#Greenland catches
Greenland[4:64] <- lapply(Greenland[4:64], function(x) as.numeric(as.character(x))) 
catchesGreenland<-colSums(Greenland[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataGreenland <- as.data.frame(catchesGreenland)           #to get a data frame for the catches per year per country
dataGreenland$Year <- rownames(dataGreenland)              #to add the years
dataGreenland$Country <- "Greenland"           ##add the country (It does not work)
dataGreenland <-spread(dataGreenland, Year,catchesGreenland)      #to have data in rows

#Graphic Greenland
dataGreenland <- gather(dataGreenland, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataGreenland, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataGreenland, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataGreenland, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataGreenland$Year<- as.numeric(as.character(dataGreenland$Year, na.omit=TRUE))
str(dataBelgium)
abnGreenland <- lm(formula = Catches ~Year, data=dataGreenland)
summary(abnGreenland)
abnGreenland1 <- lm(formula = Catches ~ Year, data=subset(dataGreenland, Year >= 1990))
summary(abnGreenland1)
abnGreenland2 <- lm(formula = Catches ~ Year, data=subset(dataGreenland, Year >= 2000))
summary(abnGreenland2)

#Iceland catches
Iceland[4:64] <- lapply(Iceland[4:64], function(x) as.numeric(as.character(x))) 
catchesIceland<-colSums(Iceland[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataIceland <- as.data.frame(catchesIceland)           #to get a data frame for the catches per year per country
dataIceland$Year <- rownames(dataIceland)              #to add the years
dataIceland$Country <- "Iceland"           ##add the country (It does not work)
dataIceland <-spread(dataIceland, Year,catchesIceland)      #to have data in rows

#Graphic Iceland
dataIceland <- gather(dataIceland, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataIceland, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataIceland, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataIceland, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataIceland$Year<- as.numeric(as.character(dataIceland$Year, na.omit=TRUE))
str(dataBelgium)
abnIceland <- lm(formula = Catches ~Year, data=dataIceland)
summary(abnIceland)
abnIceland1 <- lm(formula = Catches ~ Year, data=subset(dataIceland, Year >= 1990))
summary(abnIceland1)
abnIceland2 <- lm(formula = Catches ~ Year, data=subset(dataIceland, Year >= 2000))
summary(abnIceland2)

#Ireland catches
Ireland[4:64] <- lapply(Ireland[4:64], function(x) as.numeric(as.character(x))) 
catchesIreland<-colSums(Ireland[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataIreland <- as.data.frame(catchesIreland)           #to get a data frame for the catches per year per country
dataIreland$Year <- rownames(dataIreland)              #to add the years
dataIreland$Country <- "Ireland"           ##add the country (It does not work)
dataIreland <-spread(dataIreland, Year,catchesIreland)      #to have data in rows

#Graphic Ireland
dataIreland <- gather(dataIreland, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataIreland, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataIreland, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataIreland, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataIreland$Year<- as.numeric(as.character(dataIreland$Year, na.omit=TRUE))
str(dataBelgium)
abnIreland <- lm(formula = Catches ~Year, data=dataIreland)
summary(abnIreland)
abnIreland1 <- lm(formula = Catches ~ Year, data=subset(dataIreland, Year >= 1990))
summary(abnIreland1)
abnIreland2 <- lm(formula = Catches ~ Year, data=subset(dataIreland, Year >= 2000))
summary(abnIreland2)

#Latvia catches
Latvia[4:64] <- lapply(Latvia[4:64], function(x) as.numeric(as.character(x))) 
catchesLatvia<-colSums(Latvia[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataLatvia <- as.data.frame(catchesLatvia)           #to get a data frame for the catches per year per country
dataLatvia$Year <- rownames(dataLatvia)              #to add the years
dataLatvia$Country <- "Latvia"           ##add the country (It does not work)
dataLatvia <-spread(dataLatvia, Year,catchesLatvia)      #to have data in rows

#Graphic Latvia
dataLatvia <- gather(dataLatvia, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataLatvia, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataLatvia, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataLatvia, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataLatvia$Year<- as.numeric(as.character(dataLatvia$Year, na.omit=TRUE))
str(dataBelgium)
abnLatvia <- lm(formula = Catches ~Year, data=dataLatvia)
summary(abnLatvia)
abnLatvia1 <- lm(formula = Catches ~ Year, data=subset(dataLatvia, Year >= 1990))
summary(abnLatvia1)
abnLatvia2 <- lm(formula = Catches ~ Year, data=subset(dataLatvia, Year >= 2000))
summary(abnLatvia2)

#Lithuania catches
Lithuania[4:64] <- lapply(Lithuania[4:64], function(x) as.numeric(as.character(x))) 
catchesLithuania<-colSums(Lithuania[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataLithuania <- as.data.frame(catchesLithuania)           #to get a data frame for the catches per year per country
dataLithuania$Year <- rownames(dataLithuania)              #to add the years
dataLithuania$Country <- "Lithuania"           ##add the country (It does not work)
dataLithuania <-spread(dataLithuania, Year,catchesLithuania)      #to have data in rows

#Graphic Lithuania
dataLithuania <- gather(dataLithuania, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataLithuania, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataLithuania, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataLithuania, Year >= 1990), method=lm) +
  geom_smooth(data=subset(dataLithuania, Year >= 1987),method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataLithuania$Year<- as.numeric(as.character(dataLithuania$Year, na.omit=TRUE))
str(dataBelgium)
abnLithuania <- lm(formula = Catches ~Year, data=dataLithuania)
summary(abnLithuania)
abnLithuania1 <- lm(formula = Catches ~ Year, data=subset(dataLithuania, Year >= 1990))
summary(abnLithuania1)
abnLithuania2 <- lm(formula = Catches ~ Year, data=subset(dataLithuania, Year >= 2000))
summary(abnLithuania2)

#Netherlands catches
Netherlands[4:64] <- lapply(Netherlands[4:64], function(x) as.numeric(as.character(x))) 
catchesNetherlands<-colSums(Netherlands[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataNetherlands <- as.data.frame(catchesNetherlands)           #to get a data frame for the catches per year per country
dataNetherlands$Year <- rownames(dataNetherlands)              #to add the years
dataNetherlands$Country <- "Netherlands"           ##add the country (It does not work)
dataNetherlands <-spread(dataNetherlands, Year,catchesNetherlands)      #to have data in rows

#Graphic Netherlands
dataNetherlands <- gather(dataNetherlands, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataNetherlands, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataNetherlands, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataNetherlands, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataNetherlands$Year<- as.numeric(as.character(dataNetherlands$Year, na.omit=TRUE))
str(dataBelgium)
abnNetherlands <- lm(formula = Catches ~Year, data=dataNetherlands)
summary(abnNetherlands)
abnNetherlands1 <- lm(formula = Catches ~ Year, data=subset(dataNetherlands, Year >= 1990))
summary(abnNetherlands1)
abnNetherlands2 <- lm(formula = Catches ~ Year, data=subset(dataNetherlands, Year >= 2000))
summary(abnNetherlands2)

#Norway catches
Norway[4:64] <- lapply(Norway[4:64], function(x) as.numeric(as.character(x))) 
catchesNorway<-colSums(Norway[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataNorway <- as.data.frame(catchesNorway)           #to get a data frame for the catches per year per country
dataNorway$Year <- rownames(dataNorway)              #to add the years
dataNorway$Country <- "Norway"           ##add the country (It does not work)
dataNorway <-spread(dataNorway, Year,catchesNorway)      #to have data in rows

#Graphic Norway
dataNorway <- gather(dataNorway, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataNorway, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataNorway, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataNorway, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataNorway$Year<- as.numeric(as.character(dataNorway$Year, na.omit=TRUE))
str(dataBelgium)
abnNorway <- lm(formula = Catches ~Year, data=dataNorway)
summary(abnNorway)
abnNorway1 <- lm(formula = Catches ~ Year, data=subset(dataNorway, Year >= 1990))
summary(abnNorway1)
abnNorway2 <- lm(formula = Catches ~ Year, data=subset(dataNorway, Year >= 2000))
summary(abnNorway2)

#Poland catches
Poland[4:64] <- lapply(Poland[4:64], function(x) as.numeric(as.character(x))) 
catchesPoland<-colSums(Poland[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataPoland <- as.data.frame(catchesPoland)           #to get a data frame for the catches per year per country
dataPoland$Year <- rownames(dataPoland)              #to add the years
dataPoland$Country <- "Poland"           ##add the country (It does not work)
dataPoland <-spread(dataPoland, Year,catchesPoland)      #to have data in rows

#Graphic Poland
dataPoland <- gather(dataPoland, Year, Catches, 2:62)   #to put data in long format needed for ggplot

ggplot(data=dataPoland, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataPoland, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataPoland, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataPoland$Year<- as.numeric(as.character(dataPoland$Year, na.omit=TRUE))
str(dataBelgium)
abnPoland <- lm(formula = Catches ~Year, data=dataPoland)
summary(abnPoland)
abnPoland1 <- lm(formula = Catches ~ Year, data=subset(dataPoland, Year >= 1990))
summary(abnPoland1)
abnPoland2 <- lm(formula = Catches ~ Year, data=subset(dataPoland, Year >= 2000))
summary(abnPoland2)

#Portugal catches
Portugal[4:64] <- lapply(Portugal[4:64], function(x) as.numeric(as.character(x))) 
catchesPortugal<-colSums(Portugal[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataPortugal <- as.data.frame(catchesPortugal)           #to get a data frame for the catches per year per country
dataPortugal$Year <- rownames(dataPortugal)              #to add the years
dataPortugal$Country <- "Portugal"           ##add the country (It does not work)
dataPortugal <-spread(dataPortugal, Year,catchesPortugal)      #to have data in rows

#Graphic Portugal
dataPortugal <- gather(dataPortugal, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataPortugal, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataPortugal, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataPortugal, Year >= 1990), method=lm) +
  geom_smooth(data=subset(dataPortugal, Year >= 1972), method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataPortugal$Year<- as.numeric(as.character(dataPortugal$Year, na.omit=TRUE))
str(dataBelgium)
abnPortugal <- lm(formula = Catches ~Year, data=dataPortugal)
summary(abnPortugal)
abnPortugal1 <- lm(formula = Catches ~ Year, data=subset(dataPortugal, Year >= 1990))
summary(abnPortugal1)
abnPortugal2 <- lm(formula = Catches ~ Year, data=subset(dataPortugal, Year >= 2000))
summary(abnPortugal2)

#Romania catches
Romania[4:64] <- lapply(Romania[4:64], function(x) as.numeric(as.character(x))) 
catchesRomania<-colSums(Romania[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataRomania <- as.data.frame(catchesRomania)           #to get a data frame for the catches per year per country
dataRomania$Year <- rownames(dataRomania)              #to add the years
dataRomania$Country <- "Romania"           ##add the country (It does not work)
dataRomania <-spread(dataRomania, Year,catchesRomania)      #to have data in rows

#Graphic Romania
dataRomania <- gather(dataRomania, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataRomania, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataRomania, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataRomania, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataRomania$Year<- as.numeric(as.character(dataRomania$Year, na.omit=TRUE))
str(dataBelgium)
abnRomania <- lm(formula = Catches ~Year, data=dataRomania)
summary(abnRomania)
abnRomania1 <- lm(formula = Catches ~ Year, data=subset(dataRomania, Year >= 1990))
summary(abnRomania1)
abnRomania2 <- lm(formula = Catches ~ Year, data=subset(dataRomania, Year >= 2000))
summary(abnRomania2)

#Russia catches
Russia[4:64] <- lapply(Russia[4:64], function(x) as.numeric(as.character(x))) 
catchesRussia<-colSums(Russia[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataRussia <- as.data.frame(catchesRussia)           #to get a data frame for the catches per year per country
dataRussia$Year <- rownames(dataRussia)              #to add the years
dataRussia$Country <- "Russia"           ##add the country (It does not work)
dataRussia <-spread(dataRussia, Year,catchesRussia)      #to have data in rows

#Graphic Russia
dataRussia <- gather(dataRussia, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataRussia, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataRussia, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataRussia, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataRussia$Year<- as.numeric(as.character(dataRussia$Year, na.omit=TRUE))
str(dataBelgium)
abnRussia <- lm(formula = Catches ~Year, data=dataRussia)
summary(abnRussia)
abnRussia1 <- lm(formula = Catches ~ Year, data=subset(dataRussia, Year >= 1990))
summary(abnRussia1)
abnRussia2 <- lm(formula = Catches ~ Year, data=subset(dataRussia, Year >= 2000))
summary(abnRussia2)

#Spain catches
Spain[4:64] <- lapply(Spain[4:64], function(x) as.numeric(as.character(x))) 
catchesSpain<-colSums(Spain[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataSpain <- as.data.frame(catchesSpain)           #to get a data frame for the catches per year per country
dataSpain$Year <- rownames(dataSpain)              #to add the years
dataSpain$Country <- "Spain"           ##add the country (It does not work)
dataSpain <-spread(dataSpain, Year,catchesSpain)      #to have data in rows

#Graphic Spain
dataSpain <- gather(dataSpain, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataSpain, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataSpain, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataSpain, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataSpain$Year<- as.numeric(as.character(dataSpain$Year, na.omit=TRUE))
str(dataBelgium)
abnSpain <- lm(formula = Catches ~Year, data=dataSpain)
summary(abnSpain)
abnSpain1 <- lm(formula = Catches ~ Year, data=subset(dataSpain, Year >= 1990))
summary(abnSpain1)
abnSpain2 <- lm(formula = Catches ~ Year, data=subset(dataSpain, Year >= 2000))
summary(abnSpain2)

#Sweden catches
Sweden[4:64] <- lapply(Sweden[4:64], function(x) as.numeric(as.character(x))) 
catchesSweden<-colSums(Sweden[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataSweden <- as.data.frame(catchesSweden)           #to get a data frame for the catches per year per country
dataSweden$Year <- rownames(dataSweden)              #to add the years
dataSweden$Country <- "Sweden"           ##add the country (It does not work)
dataSweden <-spread(dataSweden, Year,catchesSweden)      #to have data in rows

#Graphic Sweden
dataSweden <- gather(dataSweden, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataSweden, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataSweden, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataSweden, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))


dataSweden$Year<- as.numeric(as.character(dataSweden$Year, na.omit=TRUE))
str(dataBelgium)
abnSweden <- lm(formula = Catches ~Year, data=dataSweden)
summary(abnSweden)
abnSweden1 <- lm(formula = Catches ~ Year, data=subset(dataSweden, Year >= 1990))
summary(abnSweden1)
abnSweden2 <- lm(formula = Catches ~ Year, data=subset(dataSweden, Year >= 2000))
summary(abnSweden2)

#All countries together in one plot

datacatchescoutries <- rbind (dataBelgium, dataBulgaria, dataDenmark, dataEstonia, dataFaroe, dataFinland, dataFrance, dataGermany, dataGreenland, dataIceland, dataIreland, dataLatvia, dataLithuania, dataNetherlands, dataNorway, dataPoland, dataPortugal, dataRomania, dataRussia, dataSpain, dataSweden)

#Graphic countries
datacatchescoutries <- gather(datacatchescoutries, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=datacatchescoutries, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes cod")+
  theme(axis.text.x = element_text(angle = 90, size = 8))