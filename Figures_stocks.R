
---
#title: "ICES rectangles map"
#author: "Raquel Ruiz-D?az", modified by "Juan Bueno"
#date: "21/04/2020"
#output: pdf_document
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
ICESmap <- sf::st_read("data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.dbf", quiet = TRUE)
ggplot() +
  geom_sf(data=ICESmap, (aes(fill = factor(Area_27), color=Area_27)))

library(rgdal)
shp <- readOGR ("data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp")
glimpse(shp)

library(sp)
shp <- spTransform(shp,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


library(broom)
shp_tidy <- tidy(shp)
head(shp_tidy)

#ggplot(shp_tidy, aes(x = long, y = lat, group = group)) +
#  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
#  coord_equal() +
#  theme_minimal()

shp$id <- row.names(shp)
shp_tidy <- left_join(shp_tidy, shp@data)

## Add extra info for the regions
stock <- read.csv("Data/Stock-data.csv", sep = ";")
colnames(stock) [which (colnames(stock) == "?..Species")] <- "Species"

shp_tidy <- merge(shp_tidy, stock , by="Area_27", all.x=TRUE)


##################################################
# Upload Resilience data

Res <- read.csv("Data/Lat_RI_Stock2.csv", sep = ";")
Res$FRI.x.Stock<- as.factor(Res$RI.x.Stock)

colnames(Res) [which (colnames(Res) == "SPECIES")] <- "Species"

Res.cod<- Res%>% 
  filter(Species == "Atlantic cod")

Res.hake<- Res%>% 
  filter(Species == "European hake")



###################################################


matching.names <- match(stock$Stock_name, Res$Stock_name)
matching.names [1] <- 9
matching.names [2] <- 10

stock_res <- Res$FRI.x.Stock [matching.names]
stock.res <- data.frame (stock, Res=stock_res)

stock.res$Area_27 <- gsub(" ", "", stock.res$Area_27)


shp_tidy_res <- stock.res$Res [match(shp_tidy$Area_27, stock.res$Area_27)]
shp_tidy_sp  <- stock.res$Species [match(shp_tidy$Area_27, stock.res$Area_27)]
shp_tidy_stock  <- stock.res$Stock_name [match(shp_tidy$Area_27, stock.res$Area_27)]

shp_tidy.res <- data.frame (shp_tidy, Res=shp_tidy_res, Species=shp_tidy_sp, Stock_name=shp_tidy_stock)

shp_tidy.res$Res <- as.numeric(as.character(shp_tidy.res$Res))

##################################################

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


##################################################

png("Figures/Figure_Cod.png",width=9,height=7,units="in",res=300)
shp_tidy.res%>% 
  filter(Species == "Cod") %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Res)) +
  scale_fill_distiller(palette = "Spectral", 
                       values = scales::rescale((1:10), c(0,1)),
                       type = "seq", 
                       direction = 1, name="Resilence Index", limits=range(shp_tidy.res$Res))+
  geom_sf(data = NAtlantic, aes(color = "grey"), color="grey47") +
  guides(color=FALSE)+
  labs(fill= "Cod Stock")+
  theme_minimal()
dev.off()

png("Figures/Figure_Hake.png",width=9,height=7,units="in",res=300)
shp_tidy.res%>% 
  filter(Species == "Hake") %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Res)) +
  scale_fill_distiller(palette = "Spectral", 
                       values = scales::rescale((1:10), c(0,1)),
                       type = "seq", 
                       direction = 1, name="Resilence Index", limits=range(shp_tidy.res$Res))+
  geom_sf(data = NAtlantic, aes(color = "grey"), color="grey47") +
  guides(color=FALSE)+
  labs(fill= "Hake Stock")+
  theme_minimal()
dev.off()



