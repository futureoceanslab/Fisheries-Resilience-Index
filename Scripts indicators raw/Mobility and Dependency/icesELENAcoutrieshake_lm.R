###Code for country and Catch historic data ICES
##15/May/2017


#setwd("C:/Users/FOL/Ele/Datos e R/datos R ICES para TFM/Historical Landings Data R/Por países")
setwd("C:/Users/FOL/OneDrive/CLOCK_STUDENTS/Elena Fontan/Resilience Index Work/R scripts/Historical Landings Data R/Per country catch trends")

#Read database
historicdatatotal <- read.csv("ICES_1950-2010.csv", sep=",")
historicdatahake <- subset(historicdatatotal, historicdatatotal$Species=="European hake") 


#install.packages("ggplot2", "dplyr", "tidyr")
library(dplyr)
library(tidyr)
library(ggplot2)


#

historicdatahake[historicdatahake=="-"] <- NA
historicdatahake[historicdatahake=="."] <- NA
historicdatahake[historicdatahake=="<0.5"] <- 0.5

colnames(historicdatahake) <- c("Country", "Species", "Division", c(1950:2010))   #to delete the X before the year
A <- c("Belgium", "Denmark", "Faeroe Islands", "France", "Germany", "Iceland", "Ireland", "Italy", "Netherlands", "Norway", "Poland", "Portugal", "Spain", "Sweden", "Un. Sov. Soc. Rep.")
B <- c("Belgium", "Denmark", "Faeroe Islands", "France", "Germany", "Iceland", "Ireland", "Italy", "Netherlands", "Norway", "Poland", "Portugal", "Spain", "Sweden", "USSR")
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  texto <- B[i]
  
  {
    country <-historicdatahake[grep(texto, historicdatahake$Country),]
    n <- dim(country)[1]
    country$country <- rep(texto,n)
  }
  AreaMatrix[[i]] <- country
}

Belgium <- AreaMatrix[[1]]
Denmark <- AreaMatrix[[2]]
Faroe <- AreaMatrix[[3]]
France <- AreaMatrix[[4]]
Germany <- AreaMatrix[[5]]
Iceland <- AreaMatrix[[6]]
Ireland <- AreaMatrix[[7]]
Italy <- AreaMatrix[[8]]
Netherlands<- AreaMatrix[[9]]
Norway <- AreaMatrix[[10]]
Poland <- AreaMatrix[[11]]
Portugal <- AreaMatrix[[12]]
Spain <- AreaMatrix[[13]]
Sweden <- AreaMatrix[[14]]
USSR <- AreaMatrix[[15]]

#Belgium catches
Belgium[4:64] <- lapply(Belgium[4:64], function(x) as.numeric(as.character(x))) 
catchesBelgium<-colSums(Belgium[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataBelgium <- as.data.frame(catchesBelgium)           #to get a data frame for the catches per year per country
dataBelgium$Year <- rownames(dataBelgium)              #to add the years
dataBelgium$Country <- "Belgium"           ##add the country 
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
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataBelgium$Year<- as.numeric(as.character(dataBelgium$Year, na.omit=TRUE))
str(dataBelgium)
abnBelgium <- lm(formula = Catches ~Year, data=dataBelgium)
summary(abnBelgium)
abnBelgium1 <- lm(formula = Catches ~ Year, data=subset(dataBelgium, Year >= 1990))
summary(abnBelgium1)
abnBelgium2 <- lm(formula = Catches ~ Year, data=subset(dataBelgium, Year >= 2000))
summary(abnBelgium2)

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
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataDenmark$Year<- as.numeric(as.character(dataDenmark$Year, na.omit=TRUE))
str(dataBelgium)
abnDenmark <- lm(formula = Catches ~Year, data=dataDenmark)
summary(abnDenmark)
abnDenmark1 <- lm(formula = Catches ~ Year, data=subset(dataDenmark, Year >= 1990))
summary(abnDenmark1)
abnDenmark2 <- lm(formula = Catches ~ Year, data=subset(dataDenmark, Year >= 2000))
summary(abnDenmark2)


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
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataFaroe$Year<- as.numeric(as.character(dataFaroe$Year, na.omit=TRUE))
str(dataBelgium)
abnFaroe <- lm(formula = Catches ~Year, data=dataFaroe)
summary(abnFaroe)
abnFaroe1 <- lm(formula = Catches ~ Year, data=subset(dataFaroe, Year >= 1990))
summary(abnFaroe1)
abnFaroe2 <- lm(formula = Catches ~ Year, data=subset(dataFaroe, Year >= 2000))
summary(abnFaroe2)


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
  ylab("Catches in tonnes hake")+
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
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataGermany$Year<- as.numeric(as.character(dataGermany$Year, na.omit=TRUE))
str(dataBelgium)
abnGermany <- lm(formula = Catches ~Year, data=dataGermany)
summary(abnGermany)
abnGermany1 <- lm(formula = Catches ~ Year, data=subset(dataGermany, Year >= 1990))
summary(abnGermany1)
abnGermany2 <- lm(formula = Catches ~ Year, data=subset(dataGermany, Year >= 2000))
summary(abnGermany2)

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
  ylab("Catches in tonnes hake")+
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
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataIreland$Year<- as.numeric(as.character(dataIreland$Year, na.omit=TRUE))
str(dataBelgium)
abnIreland <- lm(formula = Catches ~Year, data=dataIreland)
summary(abnIreland)
abnIreland1 <- lm(formula = Catches ~ Year, data=subset(dataIreland, Year >= 1990))
summary(abnIreland1)
abnIreland2 <- lm(formula = Catches ~ Year, data=subset(dataIreland, Year >= 2000))
summary(abnIreland2)

#Italy catches
Italy[4:64] <- lapply(Italy[4:64], function(x) as.numeric(as.character(x))) 
catchesItaly<-colSums(Italy[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataItaly <- as.data.frame(catchesItaly)           #to get a data frame for the catches per year per country
dataItaly$Year <- rownames(dataItaly)              #to add the years
dataItaly$Country <- "Italy"           ##add the country (It does not work)
dataItaly <-spread(dataItaly, Year,catchesItaly)      #to have data in rows

#Graphic Italy
dataItaly <- gather(dataItaly, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataItaly, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  geom_smooth(data=subset(dataItaly, Year >= 2000), method=lm) +
  geom_smooth(data=subset(dataItaly, Year >= 1990), method=lm) +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
dataItaly$Year<- as.numeric(as.character(dataItaly$Year, na.omit=TRUE))
str(dataBelgium)
abnItaly <- lm(formula = Catches ~Year, data=dataItaly)
summary(abnItaly)
abnItaly1 <- lm(formula = Catches ~ Year, data=subset(dataItaly, Year >= 1990))
summary(abnItaly1)
abnItaly2 <- lm(formula = Catches ~ Year, data=subset(dataItaly, Year >= 2000))
summary(abnItaly2)

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
  ylab("Catches in tonnes hake")+
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
  ylab("Catches in tonnes hake")+
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
  ylab("Catches in tonnes hake")+
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
  geom_smooth(method=lm) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataPortugal$Year<- as.numeric(as.character(dataPortugal$Year, na.omit=TRUE))
str(dataBelgium)
abnPortugal <- lm(formula = Catches ~Year, data=dataPortugal)
summary(abnPortugal)
abnPortugal1 <- lm(formula = Catches ~ Year, data=subset(dataPortugal, Year >= 1990))
summary(abnPortugal1)
abnPortugal2 <- lm(formula = Catches ~ Year, data=subset(dataPortugal, Year >= 2000))
summary(abnPortugal2)

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
  ylab("Catches in tonnes hake")+
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
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

dataSweden$Year<- as.numeric(as.character(dataSweden$Year, na.omit=TRUE))
str(dataBelgium)
abnSweden <- lm(formula = Catches ~Year, data=dataSweden)
summary(abnSweden)
abnSweden1 <- lm(formula = Catches ~ Year, data=subset(dataSweden, Year >= 1990))
summary(abnSweden1)
abnSweden2 <- lm(formula = Catches ~ Year, data=subset(dataSweden, Year >= 2000))
summary(abnSweden2)

#USSR catches
USSR[4:64] <- lapply(USSR[4:64], function(x) as.numeric(as.character(x))) 
catchesUSSR<-colSums(USSR[4:64], na.rm = T)  #sum columns of years, not consider NAs
dataUSSR <- as.data.frame(catchesUSSR)           #to get a data frame for the catches per year per country
dataUSSR$Year <- rownames(dataUSSR)              #to add the years
dataUSSR$Country <- "USSR"           ##add the country (It does not work)
dataUSSR <-spread(dataUSSR, Year,catchesUSSR)      #to have data in rows

#Graphic USSR
dataUSSR <- gather(dataUSSR, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=dataUSSR, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))


#All countries together in one plot

datacatchescoutries <- rbind (dataBelgium, dataDenmark, dataFaroe, dataFrance, dataGermany, dataIceland, dataIreland, dataItaly, dataNetherlands, dataNorway, dataPoland, dataPortugal, dataSpain, dataSweden)

#Graphic countries
datacatchescoutries <- gather(datacatchescoutries, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=datacatchescoutries, aes(x=Year, y=Catches, group=factor(Country), color=factor(Country))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes hake")+
  theme(axis.text.x = element_text(angle = 90, size = 8))