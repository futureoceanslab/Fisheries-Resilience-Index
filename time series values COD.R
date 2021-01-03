## Jan2020
##extracting the SSB, F and R indices from the RAM legacy database for cod
#source file: HAKE_COD_RAM, from RAMLDB v4.491

library(dplyr)
library(tidyr)
library(ggplot2)

#Read database
timedata <- read.csv("Data/time_series_values_cod.csv", sep=";", na.strings ="")
str(timedata)
timedata$SSB<- as.numeric(as.character(timedata$SSB, na.omit=TRUE))    #omit NA col SSB. and change FACTOR per num.
timedata$R<- as.numeric(as.character(timedata$R, na.omit=TRUE))
timedata$F<- as.numeric(as.character(timedata$F, na.omit=TRUE))
timedata$TSYEAR <- as.numeric(as.character(timedata$TSYEAR))
str(timedata)


A <- c("AFWG-CODNEAR-1943-2018-ICESIMP2018", "AFWG-CODNEARNCW-1984-2017-ICESIMP2018", "NWWG-CODFAPL-1958-2018-ICESIMP2018", "NWWG-CODICE-1952-2018-ICESIMP2018", "WGBFAS-CODBA2532-1965-2017-ICESIMP2018", "WGBFAS-CODKAT-1996-2018-ICESIMP2018", "WGCSE-CODIS-1968-2017-ICESIMP2018", "WGCSE-CODVIa-1980-2017-ICESIMP2018", "WGNSSK-CODIIIaW-IV-VIId-1962-2018-ICESIMP2018")
B <- c("AFWG-CODNEAR-1943-2018-ICESIMP2018", "AFWG-CODNEARNCW-1984-2017-ICESIMP2018", "NWWG-CODFAPL-1958-2018-ICESIMP2018", "NWWG-CODICE-1952-2018-ICESIMP2018", "WGBFAS-CODBA2532-1965-2017-ICESIMP2018", "WGBFAS-CODKAT-1996-2018-ICESIMP2018", "WGCSE-CODIS-1968-2017-ICESIMP2018", "WGCSE-CODVIa-1980-2017-ICESIMP2018", "WGNSSK-CODIIIaW-IV-VIId-1962-2018-ICESIMP2018")
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

CODNEAR <- AreaMatrix[[1]]
CODNEARNCW <- AreaMatrix[[2]]
CODFAPL <- AreaMatrix[[3]]
CODICE <- AreaMatrix [[4]]
CODBA2532 <- AreaMatrix [[5]]
CODKAT <- AreaMatrix [[6]]
CODIS <- AreaMatrix [[7]]
CODVIa <- AreaMatrix [[8]]     
CODIIIaW <- AreaMatrix [[9]]

#all stocks SSB
allstock <- rbind(CODNEAR, CODNEARNCW, CODFAPL, CODICE, CODBA2532, CODKAT, CODIS, CODVIa, CODIIIaW)  #sum all dataframe

ggplot(data=allstock, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(allstock, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock <- lm(formula = SSB ~TSYEAR, data=allstock)
summary(abnallstock)

##all stocks SSB average 1980-most recent year

allstock1980 <- subset(allstock, TSYEAR>=1980)

SSBmean <- allstock1980 %>%
  group_by(STOCKID) %>%
  summarise(mean=mean(SSB, na.rm = T), sd=sd(SSB, na.rm = T))

#Abundance trend calculation for each stock SSB


#1SSB
ggplot(data=CODNEAR, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODNEAR, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEAR1<- lm(formula = SSB ~TSYEAR, data=CODNEAR)
abnCODNEAR <- lm(formula = SSB ~TSYEAR, data=subset(CODNEAR, TSYEAR >= 1980))
summary(abnCODNEAR)
summary(abnCODNEAR1)

#2 SSB
ggplot(data=CODNEARNCW, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODNEARNCW, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEARNCW1 <- lm(formula = SSB ~TSYEAR, data=CODNEARNCW)
abnCODNEARNCW <- lm(formula = SSB ~TSYEAR, data=subset(CODNEARNCW, TSYEAR >= 1980))
summary(abnCODNEARNCW)
summary(abnCODNEARNCW1 )

#3 SSB
ggplot(data=CODFAPL, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODFAPL, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODFAP1 <- lm(formula = SSB ~TSYEAR, data=CODFAPL)
abnCODFAPL <- lm(formula = SSB ~TSYEAR, data=subset(CODFAPL, TSYEAR >= 1980))
summary(abnCODFAPL)
summary(abnCODFAP1)

#4 SSB
ggplot(data=CODICE, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODICE, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODICE1 <- lm(formula = SSB ~TSYEAR, data=CODICE)
abnCODICE <- lm(formula = SSB ~TSYEAR, data=subset(CODICE, TSYEAR >= 1980))
summary(abnCODICE)
summary(abnCODICE1)

#5 SSB
ggplot(data=CODBA2532, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODBA2532, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODBA25321 <- lm(formula = SSB ~TSYEAR, data=CODBA2532)
abnCODBA2532 <- lm(formula = SSB ~TSYEAR, data=subset(CODBA2532, TSYEAR >= 1980))
summary(abnCODBA2532)
summary(abnCODBA25321)

#6 SSB
ggplot(data=CODKAT, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODKAT, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODKAT1 <- lm(formula = SSB ~TSYEAR, data=CODKAT)
abnCODKAT <- lm(formula = SSB ~TSYEAR, data=subset(CODKAT, TSYEAR >= 1980))
summary(abnCODKAT)
summary(abnCODKAT1)

#7 SSB
ggplot(data=CODIS, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODIS, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODIS1 <- lm(formula = SSB ~TSYEAR, data=CODIS)
abnCODIS <- lm(formula = SSB ~TSYEAR, data=subset(CODIS, TSYEAR >= 1980))
summary(abnCODIS)
summary(abnCODIS1)

#8 SSB
ggplot(data=CODVIa, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODVIa, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODVIa1 <- lm(formula = SSB ~TSYEAR, data=CODVIa)
abnCODVIa <- lm(formula = SSB ~TSYEAR, data=subset(CODVIa, TSYEAR >= 1980))
summary(abnCODVIa)
summary(abnCODVIa1)

#9 SSB
ggplot(data=CODIIIaW, aes(x=TSYEAR, y=SSB, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_line()+
  geom_smooth(data=subset(CODIIIaW, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODIIIaW1 <- lm(formula = SSB ~TSYEAR, data=CODIIIaW)
abnCODIIIaW <- lm(formula = SSB ~TSYEAR, data=subset(CODIIIaW, TSYEAR >= 1980))
summary(abnCODIIIaW)
summary(abnCODIIIaW1)

#Abundance trend calculation for each stock F
#fishing mortality average 1980 - present

F_mean <- allstock1980 %>%
  group_by(STOCKID) %>%
  summarise(mean=mean(F, na.rm = T), sd=sd(F, na.rm = T))

#all stocks F

ggplot(data=allstock, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock <- lm(formula = F ~TSYEAR, data=allstock)
summary(abnallstock)




#1F
ggplot(data=CODNEAR, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEAR <- lm(formula = F ~TSYEAR, data=CODNEAR)
summary(abnCODNEAR)

#2 F
ggplot(data=CODNEARNCW, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEARNCW <- lm(formula = F ~TSYEAR, data=CODNEARNCW)
summary(abnCODNEARNCW)

#3 F
ggplot(data=CODFAPL, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODFAPL <- lm(formula = F ~TSYEAR, data=CODFAPL)
summary(abnCODFAPL)

#4 F
ggplot(data=CODICE, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODICE <- lm(formula = F ~TSYEAR, data=CODICE)
summary(abnCODICE)

#5 F
ggplot(data=CODBA2532, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODBA2532 <- lm(formula = F ~TSYEAR, data=CODBA2532)
summary(abnCODBA2532)

#6 F
ggplot(data=CODKAT, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODKAT <- lm(formula = F ~TSYEAR, data=CODKAT)
summary(abnCODKAT)

#7 F
ggplot(data=CODIS, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODIS <- lm(formula = F ~TSYEAR, data=CODIS)
summary(abnCODIS)

#8 F
ggplot(data=CODVIa, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODVIa <- lm(formula = F ~TSYEAR, data=CODVIa)
summary(abnCODVIa)

#9 F
ggplot(data=CODIIIaW, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODIIIaW <- lm(formula = F ~TSYEAR, data=CODIIIaW)
summary(abnCODIIIaW)

#Recruitment trend calculation for each stock R
#recruitment average 1980 - present

R_mean <- allstock1980 %>%
  group_by(STOCKID) %>%
  summarise(mean=mean(R, na.rm = T), sd=sd(R, na.rm = T))

#all stocks R

ggplot(data=allstock, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock <- lm(formula = R ~TSYEAR, data=allstock)
summary(abnallstock)

#1R
ggplot(data=CODNEAR, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEAR <- lm(formula = R ~TSYEAR, data=CODNEAR)
summary(abnCODNEAR)

#2 R
ggplot(data=CODNEARNCW, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEARNCW <- lm(formula = R ~TSYEAR, data=CODNEARNCW)
summary(abnCODNEARNCW)

#3 R
ggplot(data=CODFAPL, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODFAPL <- lm(formula = R ~TSYEAR, data=CODFAPL)
summary(abnCODFAPL)

#4 R
ggplot(data=CODICE, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODICE <- lm(formula = R ~TSYEAR, data=CODICE)
summary(abnCODICE)

#5 R
ggplot(data=CODBA2532, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODBA2532 <- lm(formula = R ~TSYEAR, data=CODBA2532)
summary(abnCODBA2532)

#6 R
ggplot(data=CODKAT, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODKAT <- lm(formula = R ~TSYEAR, data=CODKAT)
summary(abnCODKAT)

#7 R
ggplot(data=CODIS, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODIS <- lm(formula = R ~TSYEAR, data=CODIS)
summary(abnCODIS)

#8 R
ggplot(data=CODVIa, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODVIa <- lm(formula = R ~TSYEAR, data=CODVIa)
summary(abnCODVIa)

#9 R
ggplot(data=CODIIIaW, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODIIIaW <- lm(formula = R ~TSYEAR, data=CODIIIaW)
summary(abnCODIIIaW)







##get the recovery time CODNEAR, CODNEARNCW CODFAPL, CODICE, CODBA2532, CODKAT, CODIS, CODIIIaW


#stock CODNEAR
SSBlimit2 <- 220000

CODNEAR$TSYEAR[which(CODNEAR$SSB <  (SSBlimit2))]
CODNEAR$TSYEAR[which(CODNEAR$SSB >  (1.5*SSBlimit2))]
#11 years 1963-1972

#stock CODNEARNCW *uses an estimate of SSb lim vased on the tration SSBmng for the rest of stocks
SSBlimit1 <- 39408

CODNEARNCW$TSYEAR[which(CODNEARNCW$SSB <  (SSBlimit1))]
CODNEARNCW$TSYEAR[which(CODNEARNCW$SSB >  (1.5*SSBlimit1))]
#18 2015-1997

#stock CODFAPL
SSBlimit3 <- 21000

CODFAPL$TSYEAR[which(CODFAPL$SSB <  (SSBlimit3))]
CODFAPL$TSYEAR[which(CODFAPL$SSB >  (1.5*SSBlimit3))]
# 12 years from 2006-2018

#stock CODICE
SSBlimit1 <- 125000

CODICE$TSYEAR[which(CODICE$SSB <  (SSBlimit1))]
CODICE$TSYEAR[which(CODICE$SSB >  (1.5*SSBlimit1))]
#4 years, from 1993-1997


#stock CODBA2532
SSBlimit5 <- 63000

CODBA2532$TSYEAR[which(CODBA2532$SSB <  (SSBlimit5))]
CODBA2532$TSYEAR[which(CODBA2532$SSB >  (1.5*SSBlimit5))]
# 3 years from 2005 to 2008


#stock CODKAT
SSBlimit6 <- 6400

CODKAT$TSYEAR[which(CODKAT$SSB <  (SSBlimit6))]
CODKAT$TSYEAR[which(CODKAT$SSB >  (1.5*SSBlimit6))]#21 from 1997 to 2018


#stock CODIS
SSBlimit7 <- 6000

CODIS$TSYEAR[which(CODIS$SSB <  (SSBlimit7))]
CODIS$TSYEAR[which(CODIS$SSB >  (1.5*SSBlimit7))]
##24 from 1993 to 2017

#stock CODVIa
SSBlimit9 <- 14000

CODVIa$TSYEAR[which(CODVIa$SSB <  (SSBlimit9))]
CODVIa$TSYEAR[which(CODVIa$SSB >  (1.5*SSBlimit9))]
#25 from 1992 until 2017 (not recovered) 

#stock CODIIIaW
SSBlimit8 <- 107000

CODIIIaW$TSYEAR[which(CODIIIaW$SSB <  (SSBlimit8))]
CODIIIaW$TSYEAR[which(CODIIIaW$SSB >  (1.5*SSBlimit8))]
#25 from 1989 until today (not recovered) 