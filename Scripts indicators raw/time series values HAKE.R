## Jan2020
##extracting the SSB, F and R indices from the RAM legacy database for hake
#source file: HAKE_COD_RAM, from RAMLDB v4.491

library(dplyr)
library(tidyr)
library(ggplot2)

#Read database
timedata <- read.csv("Data/time_series_values_hake.csv", sep=";", na.strings ="")
str(timedata)
timedata$SSB<- as.numeric(as.character(timedata$SSB, na.omit=TRUE))    #omit NA col SSB. and change FACTOR per num.
timedata$R<- as.numeric(as.character(timedata$R, na.omit=TRUE))
timedata$F<- as.numeric(as.character(timedata$F, na.omit=TRUE))
timedata$TSYEAR <- as.numeric(as.character(timedata$TSYEAR))
str(timedata)


A <- c("WGBIE-HAKENRTN-1978-2018-ICESIMP2018", "WGBIE-HAKESOTH-1982-2018-ICESIMP2018")
B <- c("WGBIE-HAKENRTN-1978-2018-ICESIMP2018", "WGBIE-HAKESOTH-1982-2018-ICESIMP2018")
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  texto <- B[i]
  
  {
    ASSESSID <-timedata[grep(texto, timedata$ASSESSID),]
    n <- dim(ASSESSID)[1]
    ASSESSID$ASSESSID <- rep(texto,n)
  }
  AreaMatrix[[i]] <- ASSESSID
}

HAKENRTN <- AreaMatrix[[1]]
HAKESOTH <- AreaMatrix[[2]]

#all stocks SSB
allstock <- rbind(HAKENRTN, HAKESOTH) #sum all dataframe

ggplot(data=allstock, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  geom_smooth(data=subset(allstock, TSYEAR >= 1995), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock1 <- lm(formula = SSB ~TSYEAR, data=subset(allstock, TSYEAR >= 1980))
abnallstock <- lm(formula = SSB ~TSYEAR, data=allstock)
summary(abnallstock)
summary (abnallstock1)


##all stocks SSB average 1980-most recent year

allstock1980 <- subset(allstock, TSYEAR>=1980)

SSB_mean <- allstock1980 %>%
  group_by(STOCKID) %>%
  summarise(mean=mean(SSB), sd=sd(SSB))


#Abundance trend calculation for each stock
#1 SSB
ggplot(data=HAKENRTN, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  geom_smooth(data=subset(HAKENRTN, TSYEAR >= 1995), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnHAKENRTN1 <- lm(formula = SSB ~TSYEAR, data=subset(HAKENRTN, TSYEAR >= 1980))
abnHAKENRTN <- lm(formula = SSB ~TSYEAR, data=HAKENRTN)
summary(abnHAKENRTN)
summary(abnHAKENRTN1)

#2 SSB
ggplot(data=HAKESOTH, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  geom_smooth(data=subset(HAKESOTH, TSYEAR >= 1995), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnHAKESOTH1 <- lm(formula = SSB ~TSYEAR, data=subset(HAKESOTH, TSYEAR >= 1980))
abnHAKESOTH <- lm(formula = SSB ~TSYEAR, data=HAKESOTH)
summary(abnHAKESOTH)
summary(abnHAKESOTH1)

#all stocks R
allstock <- rbind(HAKENRTN, HAKESOTH) #sum all dataframe

ggplot(data=allstock, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock <- lm(formula = R ~TSYEAR, data=allstock)
summary(abnallstock)


#Abundance trend calculation for each stock

#abundance average 1980 - present

R_mean <- allstock1980 %>%
  group_by(STOCKID) %>%
  summarise(mean=mean(R), sd=sd(R))

#1 R
ggplot(data=HAKENRTN, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnHAKENRTN <- lm(formula = R ~TSYEAR, data=HAKENRTN)
summary(abnHAKENRTN)

#2 R
ggplot(data=HAKESOTH, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnHAKESOTH <- lm(formula = R ~TSYEAR, data=HAKESOTH)
summary(abnHAKESOTH)


#Fishing mortality figures
#fishing mortality average 1980 - present

F_mean <- allstock1980 %>%
  group_by(STOCKID) %>%
  summarise(mean=mean(F, na.rm = T), sd=sd(F, na.rm = T))

#all stocks F
allstock <- rbind(HAKENRTN, HAKESOTH) #sum all dataframe

ggplot(data=allstock, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock <- lm(formula = F ~TSYEAR, data=allstock)
summary(abnallstock)


#Abundance trend calculation for each stock
#1 F
ggplot(data=HAKENRTN, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  #ggtitle("F mortality")
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnHAKENRTN <- lm(formula = F ~TSYEAR, data=HAKENRTN)
summary(abnHAKENRTN)

#2 F
ggplot(data=HAKESOTH, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnHAKESOTH <- lm(formula = F ~TSYEAR, data=HAKESOTH)
summary(abnHAKESOTH)





##get the recovery time

#stock HAKENRTN
SSBlimit1 <- 32000

HAKENRTN$TSYEAR[which(HAKENRTN$SSB <  (SSBlimit1))]
HAKENRTN$TSYEAR[which(HAKENRTN$SSB >  (1.5*SSBlimit1))]
#15 years from 1994 to 2009

##2007-1992= 15 years taking into consideration the first year that the biomass goes under SSB limit and the first year it goes over 1.5*SSB

#stock HAKESOTH
SSBlimit2 <- 8000

HAKESOTH$TSYEAR[which(HAKESOTH$SSB <  (SSBlimit2))]
HAKESOTH$TSYEAR[which(HAKESOTH$SSB >  (1.5*SSBlimit2))]

#12 years from 1995 to 2007




