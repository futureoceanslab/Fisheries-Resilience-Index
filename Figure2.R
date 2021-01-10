#' This script reads data/final_index.csv and plots
#' Figure 2 in the paper.


if(!require(broom)){
  install.packages("broom",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(broom)
packageVersion("broom")
# [1] ‘0.4.2’

if(!require(ggmap)){
  install.packages("ggmap",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(ggmap)
packageVersion("ggmap")
# [1] ‘2.6.1’

if(!require(sp)){
  install.packages("sp",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(sp)
packageVersion("sp")
# [1] ‘1.2.7’


if(!require(rgdal)){
  install.packages("rgdal",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(rgdal)
packageVersion("rgdal")
# [1] ‘1.2.20’


if(!require(rnaturalearthdata)){
  install.packages("rnaturalearthdata",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(rnaturalearthdata)
packageVersion("rnaturalearthdata")
# [1] ‘0.1.0’

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’


source("aux_functions.R")


##### 2. PREPARE MAP #####

# Get country borders and select contries

countries_to_plot <- c("EG","AL","AD","AT","BY","BE","BA","BG","HR","CY","CZ","DK","EE","FO","FI","FR","DE","GI","GR","HU","IS","IE","IM","IT","RS","LV","LI","LT","LU","MK","MT","MD","MC","ME","NL","NO","PL","PT","RO","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA","RS","MA","TN","DZ","LY","EH")

world <- rnaturalearthdata::countries50

# Transform to use with ggmap

europe <- world[world$iso_a2 %in% countries_to_plot,]

europe@data$id <- rownames(europe@data)

europe <- spTransform(europe, CRS("+proj=longlat"))

##### 3. READ DATA #####

final_index <- read_csv("data/final_index.csv")

##### 4. PLOT #####

dimensions.yaxis <- c("Institutional","Institutional","Socioeconomic","Socioeconomic")

# Define which dimension and species will be shown in each panel
plot_definition <- data.frame(DIMENSION=c("institutional","institutional","socioeconomic","socioeconomic"),
                              SPECIES=c("Cod","Hake","Cod","Hake"),
                              stringsAsFactors = FALSE)



i<-1
# Get dimension and species for this panel
dimension <- plot_definition$DIMENSION[i]
specie <- plot_definition$SPECIES[i]
# Filter data for that dimension and specie.
to.plot <-   final_index  %>% 
  filter(DIMENSION==dimension,SPECIE==specie)
# Plot
# Join map with index and get ready to plot
map_a <- europe
map_a@data <- left_join(map_a@data,to.plot,by=c(iso_a2="COUNTRIES"))
map <- tidy(map_a)
map <- left_join(map,map_a@data,by="id")

#Plot
plot1 <- ggplot() +
  theme(plot.title=element_text(color="black", size=15, face="bold.italic", hjust = 0.5, margin=margin(0,0,-15,0)))+
  ggtitle(specie)+
  
  geom_map(data=map, map=map,
           aes(x=long,y=lat, map_id=id, group=group),
           fill="gray50", color="black")+
  geom_map(data=map, map=map, color="black", size=0.15,
           aes(fill=Resilience_Index, group=id, map_id=id)) +
  scale_colour_brewer() +
  coord_equal(xlim = c(-30,30), ylim = c(29,70),ratio=1.5) +
  scale_x_continuous(labels = I )+
  scale_y_continuous(labels = I )+
  scale_fill_distiller(palette = "Spectral", type = "seq", direction = 1,name="Resilence Index",limits=range(final_index$Resilience_Index))+
  labs(subtitle = LETTERS[i], y=dimensions.yaxis[i], x="", cex=5)+
  theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
        panel.grid.major = element_line(color="white"),
        legend.text = element_text(size=14,color = "black"),
        legend.title = element_text(size=14,color="black"),
        legend.key.width=unit(2,"cm"),
        legend.position = "bottom",
        plot.margin=unit(c(0,0,0,0), "cm"))

i<-2
# Get dimension and species for this panel
dimension <- plot_definition$DIMENSION[i]
specie <- plot_definition$SPECIES[i]
# Filter data for that dimension and specie.
to.plot <-   final_index  %>% 
  filter(DIMENSION==dimension,SPECIE==specie)
# Plot
# Join map with index and get ready to plot
map_a <- europe
map_a@data <- left_join(map_a@data,to.plot,by=c(iso_a2="COUNTRIES"))
map <- tidy(map_a)
map <- left_join(map,map_a@data,by="id")

#Plot
plot2 <- ggplot() +
  theme(plot.title=element_text(color="black", size=15, face="bold.italic", hjust = 0.5, margin=margin(0,0,-15,0)))+
  ggtitle(specie)+
  
  geom_map(data=map, map=map,
           aes(x=long,y=lat, map_id=id, group=group),
           fill="gray50", color="black")+
  geom_map(data=map, map=map, color="black", size=0.15,
           aes(fill=Resilience_Index, group=id, map_id=id)) +
  scale_colour_brewer() +
  coord_equal(xlim = c(-30,30), ylim = c(29,70),ratio=1.5) +
  scale_x_continuous(labels =I )+
  scale_y_continuous(labels =I )+
  scale_fill_distiller(palette = "Spectral", type = "seq", direction = 1,name="Resilence Index",limits=range(final_index$Resilience_Index))+
  labs(subtitle = LETTERS[i], y=dimensions.yaxis[i], x="", cex=5)+
  theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
        panel.grid.major = element_line(color="white"),
        legend.text = element_text(size=15,color = "black"),
        legend.title = element_text(size=14,color="black"),
        legend.key.width=unit(2,"cm"),
        legend.position = "bottom",
        plot.margin=unit(c(0,0,0,0), "cm"))

i<-3
# Get dimension and species for this panel
dimension <- plot_definition$DIMENSION[i]
specie <- plot_definition$SPECIES[i]
# Filter data for that dimension and specie.
to.plot <-   final_index  %>% 
  filter(DIMENSION==dimension,SPECIE==specie)
# Plot
# Join map with index and get ready to plot
map_a <- europe
map_a@data <- left_join(map_a@data,to.plot,by=c(iso_a2="COUNTRIES"))
map <- tidy(map_a)
map <- left_join(map,map_a@data,by="id")

#Plot
plot3 <- ggplot() +
  theme(plot.title=element_text(color="black", size=15, face="bold.italic", hjust = 0.5, margin=margin(0,0,-15,0)))+
  ggtitle(specie)+
  
  geom_map(data=map, map=map,
           aes(x=long,y=lat, map_id=id, group=group),
           fill="gray50", color="black")+
  geom_map(data=map, map=map, color="black", size=0.15,
           aes(fill=Resilience_Index, group=id, map_id=id)) +
  scale_colour_brewer() +
  coord_equal(xlim = c(-30,30), ylim = c(29,70),ratio=1.5) +
  scale_x_continuous(labels =I )+
  scale_y_continuous(labels =I )+
  scale_fill_distiller(palette = "Spectral", type = "seq", direction = 1,name="Resilence Index",limits=range(final_index$Resilience_Index))+
  labs(subtitle = LETTERS[i], y=dimensions.yaxis[i], x="", cex=5)+
  theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
        panel.grid.major = element_line(color="white"),
        legend.text = element_text(size=15,color = "black"),
        legend.title = element_text(size=14,color="black"),
        legend.key.width=unit(2,"cm"),
        legend.position = "bottom",
        plot.margin=unit(c(0,0,0,0), "cm"))


i<-4
# Get dimension and species for this panel
dimension <- plot_definition$DIMENSION[i]
specie <- plot_definition$SPECIES[i]
# Filter data for that dimension and specie.
to.plot <-   final_index  %>% 
  filter(DIMENSION==dimension,SPECIE==specie)
# Plot
# Join map with index and get ready to plot
map_a <- europe
map_a@data <- left_join(map_a@data,to.plot,by=c(iso_a2="COUNTRIES"))
map <- tidy(map_a)
map <- left_join(map,map_a@data,by="id")

#Plot
plot4 <- ggplot() +
  theme(plot.title=element_text(color="black", size=15, face="bold.italic", hjust = 0.5, margin=margin(0,0,-15,0)))+
  ggtitle(specie)+
  
  geom_map(data=map, map=map,
           aes(x=long,y=lat, map_id=id, group=group),
           fill="gray50", color="black")+
  geom_map(data=map, map=map, color="black", size=0.15,
           aes(fill=Resilience_Index, group=id, map_id=id)) +
  scale_colour_brewer() +
  coord_equal(xlim = c(-30,30), ylim = c(29,70),ratio=1.5) +
  scale_x_continuous(labels =I )+
  scale_y_continuous(labels =I )+
  scale_fill_distiller(palette = "Spectral", type = "seq", direction = 1,name="Resilence Index",limits=range(final_index$Resilience_Index))+
  labs(subtitle = LETTERS[i], y=dimensions.yaxis[i], x="", cex=5)+
  theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
        panel.grid.major = element_line(color="white"),
        legend.text = element_text(size=15,color = "black"),
        legend.title = element_text(size=14,color="black"),
        legend.key.width=unit(2,"cm"),
        legend.position = "bottom",
        plot.margin=unit(c(0,0,0,0), "cm"))

################################################################################################################
###################################################################################################################
#################################################################################################################


#title: "ICES rectangles map"
#author: "Raquel Ruiz-D?az", modified by "Juan Bueno"
#date: "21/04/2020"
#output: pdf_document


# Running packages (IF ERROR, RUN TWICE)
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

library(rgdal)
shp <- readOGR ("Data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp")
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

#shp_tidy <- merge(shp_tidy, stock , by="Area_27", all.x=TRUE)


##################################################
# Upload Resilience data

Res <- read.csv("Data/Lat_RI_Stock.csv", sep = ";")
Res$FRI.x.Stock<- as.factor(Res$RI.x.Stock)

colnames(Res) [which (colnames(Res) == "SPECIES")] <- "Species"

Res.cod<- Res%>% 
  filter(Species == "Atlantic cod")

Res.hake<- Res%>% 
  filter(Species == "European hake")



###################################################


matching.names <- match(stock$Stock_name, Res$Stock_name )
#matching.names [1] <- 9
#matching.names [2] <- 10

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
shp_cod <- shp_tidy.res%>% filter(Species == "Cod")
shp_hake <- shp_tidy.res%>% filter(Species == "Hake")


plot5 <- ggplot() +
  theme(plot.title=element_text(color="black", size=15, face="bold.italic", hjust = 0.5, margin=margin(0,0,-15,0)))+
  ggtitle("Cod")+
  
  geom_polygon(aes(x=shp_cod$long, y=shp_cod$lat, group=shp_cod$group, fill=shp_cod$Res)) +
  scale_fill_distiller(palette = "Spectral", 
                       values = scales::rescale((1:10), c(0,1)),
                       type = "seq", 
                       direction = 1, name="Resilence Index", limits=c(0.2, 0.8))+
  
  geom_map(data=map, map=map,
           aes(x=long,y=lat, map_id=id, group=group),
           fill="gray50", color="black")+
  #geom_map(data=map, map=map, color="black", size=0.15,
  #         aes(fill=Resilience_Index, group=id, map_id=id)) +
  scale_colour_brewer() +
  coord_equal(xlim = c(-30,30), ylim = c(29,70),ratio=1.5) +
  scale_x_continuous(labels =I )+
  scale_y_continuous(labels =I )+
  scale_fill_distiller(palette = "Spectral", type = "seq", direction = 1,name="Resilence Index",limits=range(final_index$Resilience_Index))+
  labs(subtitle = "E", y="Stocks", x="", cex=5)+
  theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
        panel.grid.major = element_line(color="white"),
        legend.text = element_text(size=15,color = "black"),
        legend.title = element_text(size=14,color="black"),
        legend.key.width=unit(2,"cm"),
        legend.position = "bottom",
        plot.margin=unit(c(0,0,0,0), "cm"))


plot6 <- ggplot() +
  theme(plot.title=element_text(color="black", size=15, face="bold.italic", hjust = 0.5, margin=margin(0,0,-15,0)))+
  ggtitle("Hake")+
  
  geom_polygon(aes(x=shp_hake$long, y=shp_hake$lat, group=shp_hake$group, fill=shp_hake$Res)) +
  scale_fill_distiller(palette = "Spectral", 
                       values = scales::rescale((1:10), c(0,1)),
                       type = "seq", 
                       direction = 1, name="Resilence Index", limits=c(0.2, 0.8))+
  
  geom_map(data=map, map=map,
           aes(x=long,y=lat, map_id=id, group=group),
           fill="gray50", color="black")+
  #geom_map(data=map, map=map, color="black", size=0.15,
  #         aes(fill=Resilience_Index, group=id, map_id=id)) +
  scale_colour_brewer() +
  coord_equal(xlim = c(-30,30), ylim = c(29,70),ratio=1.5) +
  scale_x_continuous(labels =I )+
  scale_y_continuous(labels =I )+
  scale_fill_distiller(palette = "Spectral", type = "seq", direction = 1,name="Resilence Index",limits=range(final_index$Resilience_Index))+
  labs(subtitle = "F", y="Stocks", x="", cex=5)+
  theme(plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black"),
        panel.grid.major = element_line(color="white"),
        legend.text = element_text(size=15,color = "black"),
        legend.title = element_text(size=14,color="black"),
        legend.key.width=unit(2,"cm"),
        legend.position = "bottom",
        plot.margin=unit(c(0,0,0,0), "cm"))







###############################################################################################################

library(ggpubr)
png("Figures/Figure 2.png",width=10,height=8,units="in",res=300)

ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=3, ncol=2, common.legend = TRUE, legend="bottom")
          
dev.off()


