##plot F/Flim and SSB/SSBLIM
#Elena F. 19/05/2017
#Elena O. Dec 2020

library(dplyr)
library(tidyr)
library(ggplot2)
library (kobe)
library(gridExtra)

#Read database
SSBFHAKE <- read.csv("Data/SSBLIM_FLIM_HAKE.csv", sep=";", na.strings ="")
str(SSBFHAKE)
SSBFHAKE$SSB<- as.numeric(as.character(SSBFHAKE$SSB, na.omit=TRUE))    #omit NA col SSB. and change FACTOR per num.
SSBFHAKE$SSB.SSBLIM<- as.numeric(as.character(SSBFHAKE$SSB.SSBLIM, na.omit=TRUE))
SSBFHAKE$Fm<- as.numeric(as.character(SSBFHAKE$Fm, na.omit=TRUE))
SSBFHAKE$F.FLIM<- as.numeric(as.character(SSBFHAKE$F.FLIM, na.omit=TRUE))
str(SSBFHAKE)


A <- c("WGBIE-HAKENRTN-1978-2018-ICESIMP2018", "WGBIE-HAKESOTH-1982-2018-ICESIMP2018")
B <- c("WGBIE-HAKENRTN-1978-2018-ICESIMP2018", "WGBIE-HAKESOTH-1982-2018-ICESIMP2018")
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  texto <- B[i]
  
  {
    ASSESSID <-SSBFHAKE[grep(texto, SSBFHAKE$ASSESSID),]
    n <- dim(ASSESSID)[1]
    ASSESSID$ASSESSID <- rep(texto,n)
  }
  AreaMatrix[[i]] <- ASSESSID
}

HAKENRTN <- AreaMatrix[[1]]
HAKESOTH <- AreaMatrix[[2]]



# plots
ggplot(data=SSBFHAKE, aes(x=SSB.SSBLIM, y=F.FLIM, group=factor(STOCKID), color=factor(STOCKID))) +
  geom_point(size=3)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  #scale_color_gradient(low="#132B43", high="#56B1F7")+
  xlab("SSB/SSBlimit") +
  ylab("F/Flimit") +
  ggtitle("SSBHAKE")
#theme(axis.text.x = element_text(angle = 90, size = 8))

p1 <- ggplot(data=HAKENRTN, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  #geom_line(x=1, size=1)+
  scale_colour_gradient(low="blue", high="green") +
  xlab("SSB/SSBlimit") +
  ylab("F/Flimit") +
  ggtitle("Hake North")+
  xlim(0, 3) +
  ylim(0.5, 1.5) +
  theme(axis.text.x = element_text(angle = 90, size = 8))
p1

p2 <- ggplot(data=HAKESOTH, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
  geom_point(size=5)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  #geom_line(x=1, size=1)
  scale_colour_gradient(low="blue", high="green") +
  ggtitle("Hake South")+
  xlab("SSB/SSBlimit") +
  ylab("F/Flimit") +
  xlim(0, 3) +
  ylim(0.5, 1.5) +
  theme(axis.text.x = element_text(angle = 90, size = 8))
p2

graphs <- grid.arrange(p1, p2)


# Save

png("Figures/Fig 6a SI.png",width=9,height=7,units="in",res=600)

do.call(grid.arrange,c(graphs))

dev.off()
