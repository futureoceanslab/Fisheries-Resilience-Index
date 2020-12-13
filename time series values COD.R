### SSB, F and R during years of a stock. RAM legacy database. 
## 16 Mayo 2017


setwd("C:/Users/FOL/OneDrive/CLOCK_STUDENTS/Elena Fontan/Resilience Index Work/R scripts/RAM legacy data (SSB_F_R)")#install.packages("ggplot2", "dplyr", "tidyr")
library(dplyr)
library(tidyr)
library(ggplot2)

#Read database
timedata <- read.csv("time series values cod.csv", sep=";", na.strings ="")
str(timedata)
timedata$SSB<- as.numeric(as.character(timedata$SSB, na.omit=TRUE))    #omit NA col SSB. and change FACTOR per num.
timedata$R<- as.numeric(as.character(timedata$R, na.omit=TRUE))
timedata$F<- as.numeric(as.character(timedata$F, na.omit=TRUE))
timedata$TSYEAR <- as.numeric(as.character(timedata$TSYEAR))
str(timedata)


A <- c("AFWG-CODCOASTNOR-1982-2006-MINTO", "AFWG-CODNEAR-1943-2006-MINTO", "NWWG-CODFAPL-1959-2006-MINTO", "NWWG-CODICE-1952-2006-MINTO", "WGBFAS-CODBA2224-1969-2007-JENNINGS", "WGBFAS-CODKAT-1970-2006-MINTO", "WGNSDS-CODIS-1968-2006-MINTO", "WGNSDS-CODVIa-1977-2006-MINTO", "WGNSSK-CODNS-1962-2007-MINTO")
B <- c("AFWG-CODCOASTNOR-1982-2006-MINTO", "AFWG-CODNEAR-1943-2006-MINTO", "NWWG-CODFAPL-1959-2006-MINTO", "NWWG-CODICE-1952-2006-MINTO", "WGBFAS-CODBA2224-1969-2007-JENNINGS", "WGBFAS-CODKAT-1970-2006-MINTO", "WGNSDS-CODIS-1968-2006-MINTO", "WGNSDS-CODVIa-1977-2006-MINTO", "WGNSSK-CODNS-1962-2007-MINTO")
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

CODCOASTNOR <- AreaMatrix[[1]]
CODNEAR <- AreaMatrix[[2]]
CODFAPL <- AreaMatrix[[3]]
CODICE <- AreaMatrix [[4]]
CODBA2532 <- AreaMatrix [[5]]
CODKAT <- AreaMatrix [[6]]
CODIS <- AreaMatrix [[7]]
CODVIa <- AreaMatrix [[8]]      #No data
CODNS <- AreaMatrix [[9]]

#all stocks SSB
allstock <- rbind(CODCOASTNOR, CODNEAR, CODFAPL, CODICE, CODBA2532, CODKAT, CODIS, CODNS)  #sum all dataframe

ggplot(data=allstock, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  geom_smooth(data=subset(allstock, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock <- lm(formula = SSB ~TSYEAR, data=allstock)
summary(abnallstock)


#Abundance trend calculation for each stock SSB
#1SSB
ggplot(data=CODCOASTNOR, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  geom_smooth(data=subset(CODCOASTNOR, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODCOASTNOR1<- lm(formula = SSB ~TSYEAR, data=CODCOASTNOR)
abnCODCOASTNOR <- lm(formula = SSB ~TSYEAR, data=subset(CODCOASTNOR, TSYEAR >= 1980))
summary(abnCODCOASTNOR)
summary(abnCODCOASTNOR1)

#2 SSB
ggplot(data=CODNEAR, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  geom_smooth(data=subset(CODNEAR, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEAR1 <- lm(formula = SSB ~TSYEAR, data=CODNEAR)
abnCODNEAR <- lm(formula = SSB ~TSYEAR, data=subset(CODNEAR, TSYEAR >= 1980))
summary(abnCODNEAR)
summary(abnCODNEAR1 )

#3 SSB
ggplot(data=CODFAPL, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
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
ggplot(data=CODICE, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
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
ggplot(data=CODBA2532, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
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
ggplot(data=CODKAT, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
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
ggplot(data=CODIS, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
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
ggplot(data=CODVIa, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
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
ggplot(data=CODNS, aes(x=TSYEAR, y=SSB, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  geom_smooth(data=subset(CODNS, TSYEAR >= 1980), method=lm) +
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Spawing Stock Biomass MT")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNS1 <- lm(formula = SSB ~TSYEAR, data=CODNS)
abnCODNS <- lm(formula = SSB ~TSYEAR, data=subset(CODNS, TSYEAR >= 1980))
summary(abnCODNS)
summary(abnCODNS1)

#Abundance trend calculation for each stock F
#all stocks F
allstock <- rbind(CODCOASTNOR, CODNEAR, CODFAPL, CODICE, CODBA2532, CODKAT, CODIS, CODNS)  #sum all dataframe

ggplot(data=allstock, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock <- lm(formula = F ~TSYEAR, data=allstock)
summary(abnallstock)

#1F
ggplot(data=CODCOASTNOR, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODCOASTNOR <- lm(formula = F ~TSYEAR, data=CODCOASTNOR)
summary(abnCODCOASTNOR)

#2 F
ggplot(data=CODNEAR, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEAR <- lm(formula = F ~TSYEAR, data=CODNEAR)
summary(abnCODNEAR)

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
ggplot(data=CODNS, aes(x=TSYEAR, y=F, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("F 1/yr")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNS <- lm(formula = F ~TSYEAR, data=CODNS)
summary(abnCODNS)

#Abundance trend calculation for each stock R
#all stocks R
allstock <- rbind(CODCOASTNOR, CODNEAR, CODFAPL, CODICE, CODBA2532, CODKAT, CODIS, CODNS)  #sum all dataframe

ggplot(data=allstock, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnallstock <- lm(formula = R ~TSYEAR, data=allstock)
summary(abnallstock)

#1R
ggplot(data=CODCOASTNOR, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODCOASTNOR <- lm(formula = R ~TSYEAR, data=CODCOASTNOR)
summary(abnCODCOASTNOR)

#2 R
ggplot(data=CODNEAR, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNEAR <- lm(formula = R ~TSYEAR, data=CODNEAR)
summary(abnCODNEAR)

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
ggplot(data=CODNS, aes(x=TSYEAR, y=R, group=factor(ASSESSID), color=factor(ASSESSID))) +
  geom_line()+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("R E03")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

abnCODNS <- lm(formula = R ~TSYEAR, data=CODNS)
summary(abnCODNS)








##get the recovery time CODCOASTNOR, CODNEAR, CODFAPL, CODICE, CODBA2532, CODKAT, CODIS, CODNS

#stock CODCOASTNOR
#SSBlimit1 <- 100000

#CODCOASTNOR$TSYEAR[which(CODCOASTNOR$SSB <  (SSBlimit1))]

#CODCOASTNOR$TSYEAR[which(CODCOASTNOR$SSB >  (1.5*SSBlimit1))]


#stock CODNEAR
SSBlimit2 <- 220000

CODNEAR$TSYEAR[which(CODNEAR$SSB <  (SSBlimit2))]

CODNEAR$TSYEAR[which(CODNEAR$SSB >  (1.5*SSBlimit2))]

#stock CODFAPL
SSBlimit3 <- 21000

CODFAPL$TSYEAR[which(CODFAPL$SSB <  (SSBlimit3))]

CODFAPL$TSYEAR[which(CODFAPL$SSB >  (1.5*SSBlimit3))]

#stock CODICE
#SSBlimit1 <- 100000

#CODICE$TSYEAR[which(CODICE$SSB <  (SSBlimit1))]

#CODICE$TSYEAR[which(CODICE$SSB >  (1.5*SSBlimit1))]

#stock CODBA2532
SSBlimit5 <- 160000

CODBA2532$TSYEAR[which(CODBA2532$SSB <  (SSBlimit5))]

CODBA2532$TSYEAR[which(CODBA2532$SSB >  (1.5*SSBlimit5))]

#stock CODKAT
SSBlimit6 <- 6400

CODKAT$TSYEAR[which(CODKAT$SSB <  (SSBlimit6))]

CODKAT$TSYEAR[which(CODKAT$SSB >  (1.5*SSBlimit6))]

#stock CODIS
SSBlimit7 <- 6000

CODIS$TSYEAR[which(CODIS$SSB <  (SSBlimit7))]

CODIS$TSYEAR[which(CODIS$SSB >  (1.5*SSBlimit7))]

#stock CODNS
SSBlimit8 <- 70000

CODNS$TSYEAR[which(CODNS$SSB <  (SSBlimit8))]

CODNS$TSYEAR[which(CODNS$SSB >  (1.5*SSBlimit8))]