###Hake for Catch historic data ICES
##2/May/2017


#Read database
historicdatatotal <- read.csv("ICES_1950-2010.csv", sep=",")
historicdata <- subset(historicdatatotal, historicdatatotal$Species=="European hake") 

Hake_I<- read.csv("Hake_I.csv", sep=";")
Hake_II<- read.csv("Hake_II.csv", sep=";")
Hake_III<- read.csv("Hake_III.csv", sep=";")
Hake_IV<- read.csv("Hake_IV.csv", sep=";")
Hake_V<- read.csv("Hake_V.csv", sep=";")
Hake_VI<- read.csv("Hake_VI.csv", sep=";")
Hake_VII<- read.csv("Hake_VII.csv", sep=";")
Hake_VIII<- read.csv("Hake_VIII.csv", sep=";")
Hake_X<- read.csv("Hake_X.csv", sep=";")
Hake_XII<- read.csv("Hake_XII.csv", sep=";")

#install.packages("ggplot2", "dplyr", "tidyr")
library(dplyr)
library(tidyr)
library(ggplot2)

#HAKE area 1
Hake_I[Hake_I=="-"] <- NA
Hake_I[Hake_I=="."] <- NA
Hake_I[Hake_I=="<0.5"] <- 0.5
colnames(Hake_I) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("XII")
B <- c(1)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_I[grep(texto, Hake_XII$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}
area1 <- AreaMatrix[[1]]
area1[4:64] <- lapply(area1[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches1<-colSums(area1[4:64], na.rm = T)  #sum columns of years, not consider NAs
data1 <- as.data.frame(catches1)           #to get a data frame for the catches per year per area
data1$Year <- rownames(data1)              #to add the years
data1$Area <- 1                            #to add the area number
data1 <-spread(data1, Year, catches1)      #to have data in rows

#Graphic area 1
data1 <- gather(data1, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data1, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))


#Hake area 2
Hake_II[Hake_II=="-"] <- NA
Hake_II[Hake_II=="."] <- NA
Hake_II[Hake_II=="<0.5"] <- 0.5
colnames(Hake_II) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("II")
B <- c(2)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_II[grep(texto, Hake_II$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}

area2 <- AreaMatrix[[1]]
area2[4:64] <- lapply(area2[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches2<-colSums(area2[4:64], na.rm = T)  #sum columns of years, not consider NAs
data2 <- as.data.frame(catches2)           #to get a data frame for the catches per year per area
data2$Year <- rownames(data2)              #to add the years
data2$Area <- 2                            #to add the area number
data2 <-spread(data2, Year, catches2)      #to have data in rows

#Graphic area 2
data2 <- gather(data2, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data2, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))


#Hake area 3
Hake_III[Hake_III=="-"] <- NA
Hake_III[Hake_III=="."] <- NA
Hake_III[Hake_III=="<0.5"] <- 0.5
colnames(Hake_III) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("III")
B <- c(3)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_III[grep(texto, Hake_III$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}

area3 <- AreaMatrix[[1]]
area3[4:64] <- lapply(area3[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches3<-colSums(area3[4:64], na.rm = T)  #sum columns of years, not consider NAs
data3 <- as.data.frame(catches3)           #to get a data frame for the catches per year per area
data3$Year <- rownames(data3)              #to add the years
data3$Area <- 3                            #to add the area number
data3 <-spread(data3, Year, catches3)      #to have data in rows

#Graphic area 3
data3 <- gather(data3, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data3, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#Hake area 4
Hake_IV[Hake_IV=="-"] <- NA
Hake_IV[Hake_IV=="."] <- NA
Hake_IV[Hake_IV=="<0.5"] <- 0.5
colnames(Hake_IV) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("IV")
B <- c(4)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_IV[grep(texto, Hake_IV$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}

area4 <- AreaMatrix[[1]]
area4[4:64] <- lapply(area4[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches4<-colSums(area4[4:64], na.rm = T)  #sum columns of years, not consider NAs
data4 <- as.data.frame(catches4)           #to get a data frame for the catches per year per area
data4$Year <- rownames(data4)              #to add the years
data4$Area <- 4                            #to add the area number
data4 <-spread(data4, Year, catches4)      #to have data in rows

#Graphic area 4
data4 <- gather(data4, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data4, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#Hake area 5
Hake_V[Hake_V=="-"] <- NA
Hake_V[Hake_V=="."] <- NA
Hake_V[Hake_V=="<0.5"] <- 0.5
colnames(Hake_V) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("V")
B <- c(5)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_V[grep(texto, Hake_V$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}
area5 <- AreaMatrix[[1]]
area5[4:64] <- lapply(area5[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches5<-colSums(area5[4:64], na.rm = T)  #sum columns of years, not consider NAs
data5 <- as.data.frame(catches5)           #to get a data frame for the catches per year per area
data5$Year <- rownames(data5)              #to add the years
data5$Area <- 5                            #to add the area number
data5 <-spread(data5, Year, catches5)      #to have data in rows

#Graphic area 5
data5 <- gather(data5, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data5, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#Hake area 6
Hake_VI[Hake_VI=="-"] <- NA
Hake_VI[Hake_VI=="."] <- NA
Hake_VI[Hake_VI=="<0.5"] <- 0.5
colnames(Hake_VI) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("VI")
B <- c(6)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_VI[grep(texto, Hake_VI$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}
area6 <- AreaMatrix[[1]]
area6[4:64] <- lapply(area6[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches6<-colSums(area6[4:64], na.rm = T)  #sum columns of years, not consider NAs
data6 <- as.data.frame(catches6)           #to get a data frame for the catches per year per area
data6$Year <- rownames(data6)              #to add the years
data6$Area <- 6                            #to add the area number
data6 <-spread(data6, Year, catches6)      #to have data in rows

#Graphic area 6
data6 <- gather(data6, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data6, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#Hake area 7
Hake_VII[Hake_VII=="-"] <- NA
Hake_VII[Hake_VII=="."] <- NA
Hake_VII[Hake_VII=="<0.5"] <- 0.5
colnames(Hake_VII) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("VII")
B <- c(7)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_VII[grep(texto, Hake_VII$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}

area7 <- AreaMatrix[[1]]
area7[4:64] <- lapply(area7[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches7<-colSums(area7[4:64], na.rm = T)  #sum columns of years, not consider NAs
data7 <- as.data.frame(catches7)           #to get a data frame for the catches per year per area
data7$Year <- rownames(data7)              #to add the years
data7$Area <- 7                            #to add the area number
data7 <-spread(data7, Year, catches7)      #to have data in rows

#Graphic area 7
data7 <- gather(data7, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data7, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#Hake area 10
Hake_X[Hake_X=="-"] <- NA
Hake_X[Hake_X=="."] <- NA
Hake_X[Hake_X=="<0.5"] <- 0.5
colnames(Hake_X) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("X")
B <- c(10)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_X[grep(texto, Hake_X$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}
area10 <- AreaMatrix[[1]]
area10[4:64] <- lapply(area10[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches10<-colSums(area10[4:64], na.rm = T)  #sum columns of years, not consider NAs
data10 <- as.data.frame(catches10)           #to get a data frame for the catches per year per area
data10$Year <- rownames(data10)              #to add the years
data10$Area <- 10                            #to add the area number
data10 <-spread(data10, Year, catches10)      #to have data in rows

#Graphic area 10
data10 <- gather(data10, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data10, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#Hake area 12
Hake_XII[Hake_XII=="-"] <- NA
Hake_XII[Hake_XII=="."] <- NA
Hake_XII[Hake_XII=="<0.5"] <- 0.5
colnames(Hake_XII) <- c("Country", "Species", "Division", c(1950:2010))
A <- c("XII")
B <- c(12)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-Hake_XII[grep(texto, Hake_XII$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}
area12 <- AreaMatrix[[1]]
area12[4:64] <- lapply(area12[4:64], function(x) as.numeric(as.character(x)))
#calculate the total catches per year in the area (dataX)
catches12<-colSums(area12[4:64], na.rm = T)  #sum columns of years, not consider NAs
data12 <- as.data.frame(catches12)           #to get a data frame for the catches per year per area
data12$Year <- rownames(data12)              #to add the years
data12$Area <- 12                            #to add the area number
data12 <-spread(data12, Year, catches12)      #to have data in rows

#Graphic area 12
data12 <- gather(data12, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data12, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))


#Para as ?reas 8, 9 e 14 usamos a matriz historicdata.

historicdata[historicdata=="-"] <- NA
historicdata[historicdata=="."] <- NA
historicdata[historicdata=="<0.5"] <- 0.5

colnames(historicdata) <- c("Country", "Species", "Division", c(1950:2010))   #to delete the X before the year
A <- c("VIII", "IX", "XIV")
B <- c(8, 9, 14)
Na <- length(A)
AreaMatrix <- list()

for (i in 1:Na)
{
  texto <- A[i]
  numero <- B[i]
  
  {
    area <-historicdata[grep(texto, historicdata$Division),]
    n <- dim(area)[1]
    area$area <- rep(numero,n)
  }
  AreaMatrix[[i]] <- area
}

area8 <- AreaMatrix[[1]]
area9 <- AreaMatrix[[2]]
area14 <- AreaMatrix[[3]]

#Hake area 8
area8[4:64] <- lapply(area8[4:64], function(x) as.numeric(as.character(x))) 
catches8<-colSums(area8[4:64], na.rm = T)  #sum columns of years, not consider NAs
data8 <- as.data.frame(catches8)           #to get a data frame for the catches per year per area
data8$Year <- rownames(data8)              #to add the years
data8$Area <- 8                            #to add the area number
data8 <-spread(data8, Year, catches8)      #to have data in rows

#Graphic area 8
data8 <- gather(data8, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data8, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#Hake aera 9
area9[4:64] <- lapply(area9[4:64], function(x) as.numeric(as.character(x))) 
catches9<-colSums(area9[4:64], na.rm = T)  #sum columns of years, not consider NAs
data9 <- as.data.frame(catches9)           #to get a data frame for the catches per year per area
data9$Year <- rownames(data9)              #to add the years
data9$Area <- 9                            #to add the area number
data9 <-spread(data9, Year, catches9)      #to have data in rows

#Graphic area 9
data9 <- gather(data9, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data9, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#Hake area 14
area14[4:64] <- lapply(area14[4:64], function(x) as.numeric(as.character(x))) 
catches14<-colSums(area14[4:64], na.rm = T)  #sum columns of years, not consider NAs
data14 <- as.data.frame(catches14)           #to get a data frame for the catches per year per area
data14$Year <- rownames(data14)              #to add the years
data14$Area <- 14                            #to add the area number
data14 <-spread(data14, Year, catches14)      #to have data in rows

#Graphic area 14
data14 <- gather(data14, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=data14, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line()+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

##put all the areas together in the same dataframe (example with two areas but you can add the rest)

datacatches <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data12, data14) ##ir engadindo todas as areas

##graph of landings per year per area

#datacatches1 <- gather(datacatches, Year, Catches, 2:62)   #to put data in long format needed for ggplot


ggplot(data=datacatches, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line(size=1)+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes 'hake'")+
  theme(axis.text.x = element_text(angle = 90, size = 8))        

########


#rescale table areas 3, 4, 6, 7, 8, 9
datacatchestop <- rbind(data3,data4, data6, data7, data8, data9)
ggplot(data=datacatchestop, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line(size=1)+
  scale_color_manual(values=c("chocolate", "goldenrod1", "green", "blue", "orchid4", "violet"))+
  xlab(NULL) +
  ylab("Catches in tonnes 'hake'")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

#rescale table areas 2, 10, 5
datacatchesrest <- rbind(data2, data10, data5)
ggplot(data=datacatchesrest, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line(size=1)+
  #scale_color_manual(values=c("#CC6666", "#9999CC"))+
  xlab(NULL) +
  ylab("Catches in tonnes 'hake'")+
  theme(axis.text.x = element_text(angle = 90, size = 8)) 


#rescale table areas 1, 14, 12
datacatchesdown <- rbind(data1, data14, data12)
ggplot(data=datacatchesdown, aes(x=Year, y=Catches, group=factor(Area), color=factor(Area))) +
  geom_line(size=1)+
  scale_color_manual(values=c("chocolate", "goldenrod1", "green", "blue", "orchid4"))+
  xlab(NULL) +
  ylab("Catches in tonnes 'hake'")+
  theme(axis.text.x = element_text(angle = 90, size = 8))

