
---
title: "ICES rectangles map"
author: "Raquel Ruiz-Díaz"
date: "21/04/2020"
output: pdf_document
---
  
# Running packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

# usage
packages <- c("tidyverse", 
              "sf", # for loading shapefiles
              "sp",
              "tools", # for loading shapefiles
              "here", # for easy paths
              "rnaturalearth",
              "viridis" # color-blind friendly pallets
)

ipak(packages)


# Upload ICES rectangles and merge with stock database.
#ICESmap <- sf::st_read("Data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.dbf", quiet = TRUE)
#ggplot() +
  #geom_sf(data=ICESmap, (aes(fill = factor(Ecoregion), color=Ecoregion)))

ICESmap <- sf::st_read("Data/ICES_StatRec_mapto_ICES_Areas/StatRec_map_Areas_Full_20170124.dbf", quiet = TRUE)
class(ICESmap)

stock <- read.csv("Data/Stock-data.csv", sep = ";")

ICES.stocks <- merge(ICESmap, stock , by="Area_27", all.x=TRUE)


# Upload Resilience data

Res <- read.csv("Data/Lat_RI_Stock2.csv", sep = ";")
Res$FRI.x.Stock<- as.factor(Res$RI.x.Stock)

Res.cod<- Res%>% 
  filter(SPECIES == "Atlantic cod")

Res.hake<- Res%>% 
  filter(SPECIES == "European hake")

# Upload land sf.

## Descarla el mapa 
land10 <- ne_download(scale = 10, 
                      type = 'land', 
                      category = 'physical')
                       
#transform to sf.file
land10_sf <- st_as_sf(land10)
                      
# cut the area we are interested on (in this case North Atlantic) 
dims <- c(xmin=-35,
          xmax=35, 
          ymin=25,
          ymax=75)
                       
NAtlantic <- st_crop(x=land10_sf, 
                       y=dims)


# Make the plots
library(ggplot2)
library(ggrepel)

## Cod stocks
jpeg(file="Cod-ICESrec2.jpeg", height=8, width= 10, units="in", res=300)

library(RColorBrewer)
myColors <- brewer.pal(8,"Set1")
names(myColors) <- levels(Res.cod$Stock_name)
colScale <- scale_colour_manual(name = "grp",values = myColors)

jpeg(file="Cod-ICESrecr.jpeg", height=8, width= 10, units="in", res=300)
ICES.stocks%>% 
  filter(Species == "Cod") %>% 
                       ggplot() +
                       geom_sf(aes(fill = factor(Stock_name), color=Stock_name)) +
                       geom_sf(data = NAtlantic, aes(color = "grey"), color="grey47") +
  geom_label(data=Res.cod, (aes(x = Long, y = Lat, label=FRI.x.Stock)), color = "black", fontface = "bold") +
  guides(color=FALSE)+
  labs(fill= "Cod Stock")+
  theme_minimal()
dev.off()


  geom_label(data=Res.cod, aes(
    x = Long,
    y = Lat), label = Res.cod$FRI.x.Stock, box.padding = 0.5)  +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(fill= "Cod Stock")
  
  
  geom_label(aes(fill = factor(cyl)), colour = "white", fontface = "bold")
  
  
                       
dev.off()

labs(fill= "Cod Stock")+

geom_sf_text(aes(label=factor(RI.x.Stock), color = "white")) +

## Hake stocks
jpeg(file="Hake-ICESrec2.jpeg", height=8, width= 10, units="in", res=300)      
ICES.stocks%>% 
  filter(Species == "Hake") %>% 
                      ggplot() +
                      geom_sf(aes(fill = factor(Stock_name), color=Stock_name)) +
                      geom_sf(data = NAtlantic, aes(color = "grey"), color="grey47") +
                      labs(fill= "Hake Stock")+
                      theme_minimal() 
dev.off()
                      
                       
                      