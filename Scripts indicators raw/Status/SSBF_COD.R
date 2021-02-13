##plot F/Flim and SSB/SSBLIM
#Elena F. 19/05/2017
#Elena O. Dec 2020

library(dplyr)
library(tidyr)
library(ggplot2)
library (kobe)
library(gridExtra)

#Read database
SSBFCOD <- read.csv("Data/SSBLIM_FLIM_COD.csv", sep=";", na.strings ="")
str(SSBFCOD)
SSBFCOD$SSB<- as.numeric(as.character(SSBFCOD$SSB, na.omit=TRUE))    #omit NA col SSB. and change FACTOR per num.
SSBFCOD$SSB.SSBLIM <- as.numeric(as.character(SSBFCOD$SSB.SSBLIM, na.omit=TRUE))
SSBFCOD$Fm<- as.numeric(as.character(SSBFCOD$Fm, na.omit=TRUE))
SSBFCOD$F.FLIM<- as.numeric(as.character(SSBFCOD$F.FLIM, na.omit=TRUE))


A <- c("AFWG-CODNEAR-1943-2018-ICESIMP2018", "AFWG-CODNEARNCW-1984-2017-ICESIMP2018", "NWWG-CODFAPL-1958-2018-ICESIMP2018", "NWWG-CODICE-1952-2018-ICESIMP2018", "WGBFAS-CODBA2532-1965-2017-ICESIMP2018", "WGBFAS-CODKAT-1996-2018-ICESIMP2018", "WGCSE-CODIS-1968-2017-ICESIMP2018", "WGCSE-CODVIa-1980-2017-ICESIMP2018", "WGNSSK-CODIIIaW-IV-VIId-1962-2018-ICESIMP2018")
B <- c("AFWG-CODNEAR-1943-2018-ICESIMP2018", "AFWG-CODNEARNCW-1984-2017-ICESIMP2018", "NWWG-CODFAPL-1958-2018-ICESIMP2018", "NWWG-CODICE-1952-2018-ICESIMP2018", "WGBFAS-CODBA2532-1965-2017-ICESIMP2018", "WGBFAS-CODKAT-1996-2018-ICESIMP2018", "WGCSE-CODIS-1968-2017-ICESIMP2018", "WGCSE-CODVIa-1980-2017-ICESIMP2018", "WGNSSK-CODIIIaW-IV-VIId-1962-2018-ICESIMP2018")
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  texto <- B[i]
  
  {
    ASSESSID <-SSBFCOD[grep(texto, SSBFCOD$ASSESSID),]
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


# plots
ggplot(data=SSBFCOD, aes(x=SSB.SSBLIM, y=F.FLIM, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_point(size=3)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  #scale_color_gradient(low="#132B43", high="#56B1F7")+
  xlab("SSB/SSBlimit") +
  ylab("F/Flimit") +
  ggtitle("SSBFCOD")
  #theme(axis.text.x = element_text(angle = 90, size = 8))

##no sufficient data for CODBA2532
p1 <- ggplot(data=CODBA2532, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODBA2532") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODBA2532")
  #theme(axis.text.x = element_text(angle = 90, size = 8))
p1

p2 <- ggplot(data=CODFAPL, aes(x=CODFAPL$SSB.SSBLIM, y=CODFAPL$F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODFAP") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODFAPL")
  #theme(axis.text.x = element_text(angle = 90, size = 8))
p2

p3 <- ggplot(data=CODIS, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODIS") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODIS")
  #theme(axis.text.x = element_text(angle = 90, size = 8))
p3

p4 <- ggplot(data=CODKAT, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODKAT") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODKAT")
  #theme(axis.text.x = element_text(angle = 90, size = 8))
p4 #no data for kobe

p5 <- ggplot(data=CODNEAR, aes(x=CODNEAR$SSB.SSBLIM, y=CODNEAR$F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODNEAR") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODNEAR")
  #theme(axis.text.x = element_text(angle = 90, size = 8))
p5 


p6 <- ggplot(data=CODNEARNCW, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODNEARNCW") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODNEARNCW")
  #theme(axis.text.x = element_text(angle = 90, size = 8))
p6 #no data for kobe

p7 <- ggplot(data=CODVIa, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODVIa") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODVIa")
  #theme(axis.text.x = element_text(angle = 90, size = 8))
p7

p8 <- ggplot(data=CODICE, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODICE") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODICE")
#theme(axis.text.x = element_text(angle = 90, size = 8))
p8

p9 <- ggplot(data=CODIIIaW, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  scale_colour_gradient(low="blue", high="green") +
  #geom_line(x=1, size=1)+
  xlab("SSB/SSBlimit CODIIIaW") +
  ylab("F/Flimit") +
  xlim(0, 8) +
  ylim(0.25, 1.8) +
  ggtitle("CODIIIaW")
#theme(axis.text.x = element_text(angle = 90, size = 8))
p9

grid.arrange(p2, p3, p5, p7, p8, p9)
