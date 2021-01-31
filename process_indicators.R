#' this script produces ecological, socioeconomic and insititutional 
#' factors from the corresponding indicators. 
#' Also, creates tables for "SI 2. INDICATORS AND FACTORS"
#' 
#' Reads:
#' 
#' data/institutional_indicators.csv
#' data/socioeconomic_indicators.csv
#' data/ecological_indicators.csv
#' 
#' and produces
#' 
#' data/ecological_indicators_country.csv
#' data/ecological_factors.csv
#' data/socioeconomic_factors.csv
#' data/institutional_factors.csv
#' data/ecological_factors_country.csv
#' data/socioeconomic_factors_country.csv
#' data/institutional_factors_country.csv
#' data/ecologicalRI_country.csv
#' 
#' Tables/Table1SI.docx
#' Tables/Table2SI.docx
#' Tables/Table3SI.docx
#' Tables/Table4SI.docx
#' Tables/Table5SI.docx
#' Tables/Table6SI.docx
#' Tables/Table8SI.docx
#' Tables/Table9SI.docx dropped
#' Tables/Table10SI.docx
#' Tables/Table11SI.docx
#' Tables/Table12SI.docx
#' Tables/Table12SI.docx
#' Tables/Table14SI.docx
#' Tables/Table15SI.docx
#' Tables/Table16SI.docx
#' Tables/Table17SI.docx
#' Tables/Table18SI.docx
#' Tables/Table19SI.docx
#' Tables/Table20SI.docx
#' Tables/Table21SI.docx



##### 1. LOAD PACKAGES AND DISPLAY VERSIONS #####

version                           
# platform       x86_64-apple-darwin13.4.0   
# arch           x86_64                      
# os             darwin13.4.0                
# system         x86_64, darwin13.4.0        
# status                                     
# major          3                           
# minor          3.0                         
# year           2016                        
# month          05                          
# day            03                          
# svn rev        70573                       
# language       R                           
# version.string R version 3.3.0 (2016-05-03)
# nickname       Supposedly Educational          

if(!require(flextable)){
  install.packages('flextable',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(flextable)
packageVersion("flextable")
# [1] ‘0.4.4’


if(!require(officer)){
  install.packages('officer',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(officer)
packageVersion("officer")
# [1] ‘0.3.0’

if(!require(magrittr)){
  install.packages('magrittr',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(magrittr)
packageVersion("magrittr")
# [1] ‘1.5’

if(!require(data.table)){
  install.packages('data.table',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(data.table)
packageVersion("data.table")
# [1] ‘1.10.4’

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’

if(!require(viridis)){
  install.packages("viridis",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(viridis)
packageVersion("viridis")

source("aux_functions.R")

# Disable scientific notation
scipen <- getOption("scipen")
options(scipen=999)



##### 1.1 DEFINE COUNTRY DEPENDENCY #####

# Define which species are captured in each stock

stock_by_species <- list(`Atlantic cod`=c("CODNEAR","CODNEARNCW","CODFAPL","CODICE","CODBA2532","CODKAT","CODIS","CODVIa","CODIIIaW"),
                         `European hake`=c("HAKENRTN","HAKESOTH")) 

suppressWarnings(stock_by_species %<>% lapply(`length<-`, max(lengths(stock_by_species))) %>% data.frame(check.names = FALSE) %>% gather(SPECIES,STOCK) %>% filter(complete.cases(.)))

# Which countries fish  each stock
countries_fishing <- list(HAKENRTN=c("BE","DK","DE","ES","FR","NL","PT","SE"),
                          HAKESOTH=c("ES","FR","PT"),
                          CODNEARNCW=c("DK","DE","EE","IE","ES","FR","PL","PT"),
                          CODNEAR=c("DK","DE","EE","IE","ES","FR","PL","PT"),
                          CODFAPL=c("DE"),
                          CODICE=c("DE"),
                          CODBA2532=c("DK","DE","EE","LV","LT","PL","FI","SE"),
                          CODKAT=c("DK","DE","FR","NL","PT","SE"),
                          CODIS=c("BE","IE","FR"),
                          CODVIa=c("DE","IE","FR"),
                          CODIIIaW=c("FR"))

# Define country dependence for each species

countries_dependence <- suppressWarnings(c(countries_fishing,list(CODNEARNCW_CODNEAR=countries_fishing$CODNEARNCW)) %>%
                                           lapply(`length<-`, max(lengths(countries_fishing))) %>% 
                                           as.data.frame() %>% 
                                           gather(STOCK,COUNTRIES) %>% 
                                           filter(complete.cases(.)) %>%
                                           full_join(stock_by_species,by="STOCK") %>%
                                           select(-STOCK) %>% distinct %>%
                                           mutate(dependence=TRUE))

countries_dependence <- countries_dependence[-(23:30),]


### STOCK FRACTION BY COUNTRY  ###############

country_areas=read.csv("data/country_areas.csv",sep=";", header=TRUE)
country_areas$Chake=apply(country_areas[,3:4],1,sum)
country_areas$Ccod=apply(country_areas[,5:12],1,sum)
for (i in 3:4)
{
  x=country_areas[,i]
  country_areas[x>0,i]=country_areas[x>0,i]/country_areas$Chake[x>0]
}	
for (i in 5:12)
{
  x=country_areas[,i]
  country_areas[x>0,i]=country_areas[x>0,i]/country_areas$Ccod[x>0]
}

Careas=country_areas[,-1]
tab=as.matrix(Careas[,-1])
rownames(tab)=Careas[,1]

stock_areas=read.csv("data/stock_areas.csv",sep=";", header=TRUE)
SCareas=stock_areas[1:14,]							   
hake.area=as.matrix(SCareas[,3:4])
cod.area=SCareas[,5:12]
rownames(hake.area)=country_areas$country_code[-15]
rownames(cod.area)=country_areas$country_code[-15]

###################################################################



# Table 8

# Prepare for word

stocks_fished <-  lapply(countries_fishing, function(x) countries_order %in% x) %>% 
  data.frame() %>% t %>% data.frame()  %>% 
  set_names(countries_order) %>% mutate(STOCK=rownames(.)) %>% 
  select(STOCK,one_of(names(.)))

Table8 <- stocks_fished %>% mutate_if(is.logical,funs(ifelse(.,"yes","-")))


to.plot <- Table8 %>% data.frame

# Save to word

Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 8. Stocks that are being fished by EU countries (2006-2010).",
          "Tables/Table8SI.docx")


##### 2. ECOLOGICAL INDICATORS PROCESSING #####

eco_indicators <- fread("data/ecological_indicators.csv",check.names = TRUE)


###### 2.1 AREA (E1) #####

# See section "AREA (E1)" in 1.A in "SI 2. Indicators and Factors" for details. TO REVISE

# Table 1
#normalization is between -1 and 1 so we sum a 1 to the value of AreaChange and teh normaliztion range to do the normalization between 0 and 1
area_min <- 0 # area in 2100 is the same or below the area projected in 2006  in García-Molinos et al. (2015)
area_max <- 2 # area in 2100 is the same as the area projected in 2006  in García-Molinos et al. (2015)

Table1 <- eco_indicators  %>% 
  select(SPECIES,AreaChange) %>% 
  distinct() %>%
  mutate(AreaChange_positive=(AreaChange+1),
         AreaChange_norm=(AreaChange_positive-area_min)/(area_max-area_min)
         # Normalization positive
         # Normalization positive
  )%>% 
  rowwise() %>%  # AREA factor is the mean of the normalized indicators above for each species (row)
  mutate(AREA=AreaChange_norm,
         area_min=area_min,
         area_max=area_max)

# Prepare for word
to.plot <- Table1 %>% 
  data.frame %>% 
  select(SPECIES,AreaChange_positive,area_min,area_max,AreaChange_norm,AREA) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame

to.plot[is.na(to.plot)] <- "-"


# Save to word

Ft<- format_table(to.plot)

Ft %<>% set_header_labels(SPECIES="SPECIES",AreaChange_positive="AreaChange", area_min="Min area change",area_max="Max area change",AreaChange_norm="Area Change\nNormalized",AREA="AREA")

write_doc(Ft,
          "Table1. Area indicators and factor.",
          "Tables/Table1SI.docx",landscape = TRUE)



###### 2.2 ABUNDANCE (E2) #####

# See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details

# Table 2

# Create table
Table2 <- eco_indicators %>% 
  select(STOCK,B_SSBrecent,B_SSBhistoric,B_F=B_Ftrend,B_R=B_Rtrend,SSB.average,F.average,R.average)

# Prepare for word
to.plot <- Table2 %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame


# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 2. Value of the coefficients (ß) from the linear models of SSB historic (full time series), SSB recent (1980-2018), R and F in RAM legacy stocks. Significance at the 0.001 (***), 0.01 (**), 0.05 (*) and 0.1 levels.",
          "Tables/Table2SI.docx",landscape=TRUE)

# Table 3

Table3 <- eco_indicators  %>% 
  select(SPECIES,STOCK,B_SSBrecent,B_SSBhistoric,B_Ftrend,B_Rtrend,SSB.average,F.average,R.average)%>%
  mutate(SSBrecent=B_SSBrecent/SSB.average, # Divide slopes by average
         SSBhistoric=B_SSBhistoric/SSB.average,
         Ftrend=B_Ftrend/F.average,
         Rtrend=B_Rtrend/R.average) %>%
  mutate(SSBrecent_norm=normalize_positive(SSBrecent), # Normalize positive
         SSBhistoric_norm=normalize_positive(SSBhistoric), # Positive
         Ftrend_norm=normalize_negative(Ftrend), # Negative
         Rtrend_norm=normalize_positive(Rtrend) # Positive
  ) %>%
  rowwise %>% # ABUNDANCE factor is the mean of the normalized indicators above for each stock (row)
  mutate(ABUNDANCE=mean(c(SSBrecent_norm,Ftrend_norm),na.rm=TRUE)) %>% #droping SSBhistoric and Rtrend due to correlation
  ungroup() %>% 
  select(-starts_with("B_"),-ends_with(".average")) # Remove slopes and averages from the table

# Prepare for word
to.plot <- Table3 %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame



# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 3. Value of the abundance indicators and overexploitation. Value of indicators normalized from Table SI2",
          "Tables/Table3SI.docx",landscape=TRUE)


###### 2.3 TEMPERATURE (E3) #####

# See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details.

# Table 4

Trange_2 <- 0 # 2nd percentile of the distribution of temperature range. Cheung et al. (2010)
Trange_98 <- 11 # 98th percentile of the distribution of temperature range. Cheung et al. (2010)
T50_2 <- 0 # 2nd percentile of the distribution of median temperature.  Cheung et al. (2010)
T50_98 <- 28 # 98th percentile of the distribution of median temperature.  Cheung et al. (2010)

Table4 <- eco_indicators  %>% 
  select(SPECIES,T50,Trange)%>% 
  distinct() %>%
  mutate(Trange_norm=(Trange-Trange_2)/(Trange_98-Trange_2), # Normalization
         T50_norm=(T50-T50_2)/(T50_98-T50_2) # Normalization
  ) %>% 
  mutate(TEMPERATURE=Trange_norm, # TEMPERATURE factor is normalized Trange, droping T50 due to correlation with T50 and AreaChange
         Trange_2=Trange_2,
         Trange_98=Trange_98,
         T50_2=T50_2,
         T50_98=T50_98)

# Prepare for word
to.plot <- Table4 %>% 
  mutate_at(vars(c("Trange_norm","T50_norm","TEMPERATURE")),funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  mutate_at(vars(c("T50","Trange","Trange_2","Trange_98","T50_2","T50_98")),funs(ifelse(is.na(.),"-",sprintf("%d",as.integer(.))))) %>% # Numbers to string
  data.frame %>% 
  select(SPECIES,Trange,T50,Trange_2,Trange_98,T50_2,T50_98,Trange_norm,T50_norm,TEMPERATURE)

# Save to word

Ft<- format_table(to.plot)

Ft %<>% set_header_labels(SPECIES="SPECIES",Trange="temperature\nrange\n(ºC)",T50="mean\ntemp\n(ºC)",Trange_2="Trang\ne2%\n(ºC)",Trange_98="Trang\ne98%\n(ºC)",T50_2="T50\ne2%\n(ºC)",T50_98="T50\ne98%\n(ºC)",Trange_norm="Trange'\normalized",T50_norm="T50'\nnormalized",TEMPERATURE="TEMPERAT.")

write_doc(Ft,
          "Table 4. Temperature indicators, normalization and factor.",
          "Tables/Table4SI.docx",landscape = TRUE)



###### 2.4 OVEREXPLOITATION (E4) #####

# See section "Overexploitation (E4)"" in 1.A in "SI 2. Indicators and Factors" for details

# Table5

Table5 <- eco_indicators  %>% 
  select(SPECIES,STOCK,OverMSY,Status) %>% 
  mutate(OverMSY_norm=normalize_negative(OverMSY), # Normalize negative
         Status_norm=normalize_positive(Status) # Normalize positive
  ) %>% 
  rowwise() %>% # OVEREXPLOITATION factor is the mean of the normalized indicators above for each stock (row)
  mutate(OVEREXPLOITATION=mean(c(OverMSY_norm,Status_norm),na.rm=TRUE)) %>%
  ungroup()

# Prepare for word
to.plot <- Table5 %>% 
  select(-SPECIES) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  select(STOCK,OverMSY,Status,OverMSY_norm,Status_norm,OVEREXPLOITATION) %>%
  data.frame




# Save to word
Ft<- format_table(to.plot)

Ft %<>% set_header_labels(STOCK="STOCK",OverMSY="OverMSY",Status="Status",OverMSY_norm="OverMSY'\normalized",Status_norm="Status'\nnormalized",OVEREXPLOITATION="OVEREXPLOITATION")

write_doc(Ft,
          "Table 5. Overexploitation indicators, normalization and factor.",
          "Tables/Table5SI.docx")



##### 2.5 RECOVERY (E5) #####

# See section "Recovery (E5)"" in 1.A in "SI 2. Indicators and Factors" for details

# Table 6

recovery_2 <- 1 # 2% recovery time of sample on Neubawer et al., 2013
recovery_98 <- 43.34 # 98% recovery time of sample on Neubawer et al., 2013

Table6 <- eco_indicators  %>% 
  select(SPECIES,STOCK,Recovery) %>% 
  mutate(Recovery_norm=(recovery_98-Recovery)/(recovery_98-recovery_2)) %>% # Normalization
  mutate(RECOVERY=Recovery_norm,recovery_2=recovery_2,recovery_98=recovery_98)  # RECOVERY factor is the normalized recovery

# Prepare for word

to.plot <- Table6 %>% 
  select(-SPECIES) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.2f",.)))) %>% # Numbers to string
  select(STOCK,Recovery,recovery_2,recovery_98,Recovery_norm,RECOVERY) %>%
  data.frame


# Save to word

Ft<- format_table(to.plot)

Ft %<>% set_header_labels(STOCK="STOCK",Recovery="recovery",recovery_2="recovery 2%",recovery_98="recovery 98%",Recovery_norm="Recovery'\nnormalized",RECOVERY="RECOVERY")

write_doc(Ft,
          "Table 6. Recovery indicator, normalization and factor.",
          "Tables/Table6SI.docx")

# Table 7 - ECOLOGICAL FACTORS PER STOCK

# Merge all the factors in one table except AREA due to correlation

Table7 <- reduce(list(Table3 %>% select(SPECIES,STOCK,ABUNDANCE),
                      Table5 %>% select(SPECIES,STOCK,OVEREXPLOITATION),
                      Table6 %>% select(SPECIES,STOCK,RECOVERY)),full_join,by=c("SPECIES","STOCK")) %>% 
  full_join(Table4 %>% select(SPECIES,TEMPERATURE),by="SPECIES")

# Prepare for word

to.plot <- Table7 %>% 
  select(STOCK,ABUNDANCE,TEMPERATURE,OVEREXPLOITATION,RECOVERY) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame

# Save to word

Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 7. Ecological factors per stock.",
          "Tables/Table7SI.docx")

# Save ecological_factors.csv

Table7 %>% 
  select(SPECIES,STOCK,ABUNDANCE,TEMPERATURE,OVEREXPLOITATION,RECOVERY) %>%
  write_excel_csv("data/ecological_factors.csv")



##################################################################		  
#########		 eco_indicators by country 					######
##################################################################	
CAmat=as.matrix(Careas[,-1])
EcoArray=array(NA, dim=c(14,ncol(eco_indicators)-1,2))
rownames(EcoArray)=country_areas$country_code[1:14]
colnames(EcoArray)=names(eco_indicators)[-1]	  
colnames(EcoArray)[1]="country"	  
ecomat=data.frame(eco_indicators)

EcoHake=as.data.frame(EcoArray[,,1])
EcoHake$country=rownames(EcoArray)
EcoHake$AreaChange=ecomat$AreaChange[10]
EcoHake$Trange=ecomat$Trange[10]
EcoHake$T50=ecomat$T50[10]

for (i in 1:14)
{
  x=CAmat[i,1:2]
  for (j in 5:ncol(EcoArray))
  {
    k=j+1
    y=ecomat[10:11,k]	
    aux=x*y
    EcoHake[i,j]=sum(aux)
  }
}

EcoCod=as.data.frame(EcoArray[,,2])
EcoCod$country=rownames(EcoArray)
EcoCod$AreaChange=ecomat$AreaChange[1]	
EcoCod$Trange=ecomat$Trange[1]
EcoCod$T50=ecomat$T50[1]

for (i in 1:14)
{
  x=as.numeric(CAmat[i,3:10])
  for (j in 5:ncol(EcoArray))
  {
    k=j+1
    y=ecomat[1:9,k]	
    y=y[-2]
    aux=x*y
    EcoCod[i,j]=sum(aux)
    EcoCod[i,j]=sum(na.omit(aux))
  }
}
species=c(rep("Hake",14),rep("Cod",14))

Ecoind_country=rbind(EcoHake,EcoCod)
Ecoind_country$species=species
Ecoind_country$country=as.factor(Ecoind_country$country)
Ecoind_country$species=as.factor(Ecoind_country$species)
Ecoind_country <- Ecoind_country[-c(4:5, 8:9, 11,13), ] #to drop countries with no catches for hake
Ecoind_country[Ecoind_country == 0] <- NA

##correcting abundance indicators by average values

X=Ecoind_country
X$SSBrecent=X$B_SSBrecent/X$SSB.average
X$SSBhistoric=X$B_SSBhistoric/X$SSB.average
X$Ftrend=X$B_Ftrend/X$F.average
X$Rtrend=X$B_Rtrend/X$R.average


X %>% 
  select(-B_SSBrecent,-B_SSBhistoric,-B_Ftrend,-B_Rtrend,-SSB.average,-F.average, -R.average) %>%
  write_excel_csv("data/ecological_indicators_country.csv")


##Normalization and calculation of Ecological Factors by country


#ABUNDANCE FACTOR
X$SSBrecent_norm=normalize_positive(X$SSBrecent)
X$SSBhistoric_norm=normalize_positive(X$SSBhistoric)
X$Ftrend_norm=normalize_negative(X$Ftrend)
X$Rtrend_norm=normalize_positive(X$Rtrend)

X$ABUNDANCE=(X$SSBrecent_norm+X$Ftrend_norm)/2


##TEMPERATURE FACTOR
X$Trange_norm=(X$Trange-Trange_2)/(Trange_98-Trange_2)
X$T50_norm=(X$T50-T50_2)/(T50_98-T50_2)
X$TEMPERATURE=X$T50_norm


#OVEREXPLOITATION FACTOR
X$OverMSY_norm=normalize_negative(X$OverMSY)
X$Status_norm=normalize_positive(X$Status)
X$OVEREXPLOITATION=apply(X[,20:21],1,mean, na.rm=TRUE)

#RECOVERY FACTOR
X$Recovery_norm=(recovery_98-X$Recovery)/(recovery_98-recovery_2) # Normalization
X$RECOVERY=X$Recovery_norm

##saving Ecological factors by country


eco_countries <-   X    %>% 
  select("country","species","ABUNDANCE","TEMPERATURE","OVEREXPLOITATION","RECOVERY") %>%
  write_excel_csv("data/ecological_factors_country.csv")



##saving ecological RI by country
Emat=cbind(X$ABUNDANCE,	X$TEMPERATURE,X$OVEREXPLOITATION,X$RECOVERY) 
Ecoind_country$RI=apply(Emat,1,mean, na.rm=TRUE)
Ecoind_country$RIdef= Ecoind_country$RI
Ecoind_country$RIdef[rid==0]=NA #not working
summary(Ecoind_country)
Ecoind_country$species=species


write.table(Ecoind_country,"Data/EcologicalRI_country.csv",sep=";", row.names = FALSE) 


##### 3. SOCIOECONOMIC INDICATORS #####

soc_indicators <- fread("data/socioeconomic_indicators.csv")

# Remove countries that do not depend on a given species.

soc_indicators %<>% 
  left_join(countries_dependence,by = c("COUNTRIES", "SPECIES")) %>% 
  filter(dependence) %>% select(-dependence)


##### 3.1 GEAR.DIVERSITY #####

# See "GEAR.DIVERSITY (S1)" in 2.A in "SI 2. Indicators and Factors" for details

# Table 10

Table10 <- soc_indicators %>% 
  select(COUNTRIES,SPECIES,SPgear) %>% 
  distinct %>% 
  mutate(SPgear_norm=normalize_positive(SPgear), # Normalize positive
         GEAR.DIVERSITY=SPgear_norm) # GEAR.DIVERSITY factor is the SPgear normalized.

# Prepare for word

to.plot <- Table10 %>% mutate(SPECIES=tools::toTitleCase(species_sort_name(SPECIES))) %>%
  data.frame

# Save to word

Ft <- format_table(to.plot)
Ft %<>% set_header_labels(COUNTRIES="Fishing Countries",SPgear="Gear Diversity",SPECIES="Species",SPgear="Gear Diversity",SPgear_norm="NormalizedGearDiv",GEAR.DIVERSITY="GEAR DIVERSITY")

country_rows <- rle(to.plot$COUNTRIES)$lengths %>% cumsum()

Ft %<>% merge_v(j = 1) %>% vline(j=2,border = fp_border()) %>% 
  align(align = "left",j=2,part="body") %>% 
  bg(bg="white",j=2) %>%
  hline(country_rows,border=fp_border())


write_doc(Ft,
          "Table 10. Values and normalization of Gear Diversity.",
          "Tables/Table10SI.docx")

###### 3.2 FLEET.MOBILITY #####

# See "FLEET MOBILITY (S2)" in 2.A in "SI 2. Indicators and Factors" for details

# Table 11

Table11 <- soc_indicators %>% 
  select(COUNTRIES,ICESareas5,ICESareasEU) %>% 
  distinct %>% 
  mutate(ICESareas5_norm=normalize_positive(ICESareas5), # Normalization positive
         ICESareasEU_norm=normalize_positive(ICESareasEU) # Normalization positive
  ) %>% 
  rowwise() %>% # FLEET.MOBILITY factor is the mean of the normalized indicators above for each country (row)
  mutate(FLEET.MOBILITY=mean(c(ICESareas5_norm,ICESareasEU_norm),na.rm=TRUE)) %>% 
  ungroup()

# Prepare for word

to.plot <- Table11 %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame


to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

Ft %<>% set_header_labels(COUNTRIES="COUNTRIES",ICESareas5="ICESareas5",ICESareasEU="ICESareasEU",ICESareas5_norm="Normalized\nICESareas5",ICESareasEU_norm="Normalized\nICESareasEU",FLEET.MOBILITY="FLEET MOBILITY")

write_doc(Ft,
          "Table 11. Indicators and normalization of Fleet Mobility factor.",
          "Tables/Table11SI.docx")


##### 3.3 CATCH.DEP #####

# See "CATCH DEPENDENCY (S3)" in 2.A in "SI 2. Indicators and Factors" for details

# Table 12

Table12 <- soc_indicators %>% 
  complete(nesting(SPECIES,STOCK),COUNTRIES) %>%
  arrange(COUNTRIES,SPECIES,STOCK) %>% 
  select(SPECIES,COUNTRIES,STOCK,Stockdep.sp,Stockdep.total) %>%
  mutate(Stockdep.sp = na_if(Stockdep.sp, 0),         #convert 0 dependence on a stock to NA, so that there is no catch dep calculated
         Stockdep.total = na_if(Stockdep.total, 0)
  ) %>%      
  mutate(Stockdep.sp_norm=normalize_negative(Stockdep.sp), # Normalization negative
         Stockdep.total_norm=normalize_negative(Stockdep.total) # Normalization negative
  ) %>%
  rowwise() %>% # CATCH.DEP factor is the mean of the normalized indicators above for each stock (row)
  mutate(CATCH.DEP=mean(c(Stockdep.sp_norm,Stockdep.total_norm),na.rm=TRUE)) %>% 
  ungroup() 
#%>% arrange_table()


# Prepare for word

to.plot <- Table12 %>% 
  select(-SPECIES) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame

# Save to word

Ft<- format_table(to.plot)


Ft %<>%  set_header_labels(COUNTRIES="COUNTRIES",STOCK="STOCK",Stockdep.sp="Sockdep.sp",Stockdep.total="Stockdep.total",Stockdep.sp_norm="normalized\nspecie",Stockdep.total_norm="normalized\ntotal","CATCH.DEP")

country_rows <- rle(to.plot$COUNTRIES)$lengths %>% cumsum()

Ft %<>% merge_v(j = 1) %>% vline(j=2,border = fp_border()) %>% 
  align(align = "left",j=2,part="body") %>% 
  bg(bg="white",j=2) %>%
  hline(country_rows,border=fp_border())


write_doc(Ft,
          "Table 12. Catch dependency of countries on stocks.",
          "Tables/Table12SI.docx",landscape = TRUE)

# Table 13

Table13 <- Table12 %>% 
  group_by(SPECIES,COUNTRIES) %>%
  summarise(CATCH.DEP=mean(CATCH.DEP,na.rm=TRUE)) %>%  # Average catch dependency for each country
  ungroup() 

# Prepare for word

to.plot <- Table13  %>%
  spread(SPECIES,CATCH.DEP) %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame(check.names = TRUE)

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

to.plot[is.na(to.plot)] <- "-"

# Save to word

Ft<- format_table(to.plot)

header_labels <- paste0("CATCH.DEP\n",tools::toTitleCase(species_sort_name(names(to.plot) %>% gsub("\\."," ",.)))) %>% as.list

names(header_labels) <- names(to.plot)

Ft <- do.call(set_header_labels,c(list(x=Ft),header_labels))


write_doc(Ft,
          "Table 13. Catch dependency factors per country.",
          "Tables/Table13SI.docx")


###### 3.4 ADAPTIVE MANAGEMENT #####

# See "ADAPTIVE MANAGEMENT (S4)" in 2.A in "SI 2. Indicators and Factors" for details.

# Table 14

Table14 <- soc_indicators %>% 
  select(COUNTRIES,Research,Management) %>%
  distinct %>%
  mutate(Research_norm=normalize_positive(Research), # Normalization positive
         Management_norm=normalize_positive(Management) # Normalization positive
  ) %>%
  rowwise() %>% # ADAPTIVE.MNG factor is the mean of the normalized indicators above for each country (row)
  mutate(ADAPTIVE.MNG=mean(c(Research_norm,Management_norm),na.rm=TRUE)) %>% 
  ungroup()




# Prepare for word

to.plot <- Table14 %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame %>%
  set_names(c("COUNTRIES","Research","Management","normalizedResearch","normalizedMng","ADAPTIVE.MNG"))

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 14. Adaptive Management indicator values and normalization.",
          "Tables/Table14SI.docx")


##### 3.5 SOCIOECONOMIC FACTORS #####

# See section 2.B in "SI 2. Indicators and Factors" for details.

# Table 15

# Merge tables 10, 11, 13 and 14.

Table15 <- reduce(
  list(
    Table10 %>% select(SPECIES,COUNTRIES, GEAR.DIVERSITY) %>% 
      mutate(SPECIES=paste0("GEAR.DIV\n",species_sort_name(SPECIES))) %>% # One column for each species
      spread(SPECIES,GEAR.DIVERSITY),
    Table11 %>% select(COUNTRIES,FLEET.MOBILITY),
    Table13 %>% select(COUNTRIES,SPECIES,CATCH.DEP) %>%
      mutate(SPECIES=paste0("CATCH.DEP\n",species_sort_name(SPECIES))) %>% # One column for each species
      spread(SPECIES,CATCH.DEP),
    Table14 %>% select(COUNTRIES,ADAPTIVE.MNG)
  ),full_join,by="COUNTRIES")

# Prepare for word
to.plot <- Table15 %>%
  mutate_at(vars(starts_with("CATCH")),funs(round(.,digits = 3))) %>%
  mutate_at(vars(starts_with("ADAP"),starts_with("GEAR"), starts_with("FLEET")),funs(round(.,digits = 3))) %>% 
  select(COUNTRIES,starts_with("GEAR"),FLEET.MOBILITY,starts_with("CATCH"),ADAPTIVE.MNG) %>% 
  data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 15. Socioeconomic Factors",
          "Tables/Table15SI.docx")

# Merge tables 11, 12, 10 and 14 to produce socioeconomic factors per stock: socioeconomic_factors.csv

#Adding the SPECIES column to tables 11 and 14 to be able to properly join
Table14 <- as.data.frame(lapply(Table14, rep,2))
SPECIES_col <- c(rep("European hake",14),  rep("Atlantic cod",14))
Table14$SPECIES <- SPECIES_col

Table11 <- as.data.frame(lapply(Table11, rep,2))
SPECIES_col <- c(rep("European hake",14),  rep("Atlantic cod",14))
Table11$SPECIES <- SPECIES_col


reduce( # Merge tables 11, 12 and 14
  list(
    Table10 %>% select(SPECIES,COUNTRIES,GEAR.DIVERSITY),
    Table11 %>% select(SPECIES,COUNTRIES,FLEET.MOBILITY),
    Table12 %>% select(SPECIES,STOCK,COUNTRIES,CATCH.DEP),
    Table14 %>% select(SPECIES,COUNTRIES,ADAPTIVE.MNG)
  ),full_join,by = c("COUNTRIES", "SPECIES")) %>%
  left_join(countries_dependence,by = c("COUNTRIES", "SPECIES")) %>% # Keep only countries that depend on each species
  filter(dependence) %>% select(-dependence) %>%
  distinct()  %>%
  select("SPECIES","COUNTRIES","STOCK","ADAPTIVE.MNG","CATCH.DEP","FLEET.MOBILITY","GEAR.DIVERSITY") %>% # Organize columns
  write_excel_csv("data/socioeconomic_factors.csv") # Save to csv

# Merge tables 11, 13, 10 and 14 to produce socioeconomic factors per country: socioeconomic_factors_country.csv

reduce( # Merge tables 11, 13 and 14
  list(
    Table10 %>% select(SPECIES, COUNTRIES,GEAR.DIVERSITY),
    Table11 %>% select(SPECIES, COUNTRIES,FLEET.MOBILITY),
    Table13 %>% select(SPECIES,COUNTRIES,CATCH.DEP),
    Table14 %>% select(SPECIES, COUNTRIES,ADAPTIVE.MNG)
  ),full_join,by = c("COUNTRIES", "SPECIES")) %>%
  left_join(countries_dependence,by = c("COUNTRIES", "SPECIES")) %>% # Keep only countries that depend on each species
  filter(dependence) %>% select(-dependence) %>%
  select("SPECIES","COUNTRIES","ADAPTIVE.MNG","CATCH.DEP","FLEET.MOBILITY","GEAR.DIVERSITY") %>% # Organize columns
  write_excel_csv("data/socioeconomic_factors_country.csv") # Save to csv


##### 4 INSTITUTIONAL INDICATORS #####

ins_indicators <- fread("data/institutional_indicators.csv")

# Remove countries that do not depend on a given species.

ins_indicators %<>% 
  left_join(countries_dependence,by = c("COUNTRIES", "SPECIES")) %>% 
  filter(dependence) %>% select(-dependence)

##### 4.1 ORGANIZATION (I1) #####

# See "ORGANIZATION (I1)" in 3.A in "SI 2. Indicators and Factors" for details

# Table 16

Table16 <-ins_indicators %>% 
  select(COUNTRIES,Norganizations) %>%
  distinct %>%
  filter(!is.na(Norganizations)) %>%
  mutate(Norganizations_norm=normalize_positive(Norganizations)) %>% # Normalization positive
  rowwise() %>%
  mutate(ORGANIZATION=Norganizations_norm) %>% # ORGANIZATION is Norganizations normalized
  ungroup()

# Prepare for word

to.plot <- Table16 %>%
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame 

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

Ft %<>% set_header_labels(COUNTRIES="",Norganizations="Norganizations\n2017",Norganizations_norm="Norganizations'\n(normalized)",ORGANIZATION="ORGANIZATION")

write_doc(Ft,
          "Table 16. Values, normalization and ORGANIZATION factor.",
          "Tables/Table16SI.docx")

##### 4.2 PROPERTY.RIGHTS (I2) #####

# See "PROPERTY.RIGHTS (I2)" in 3.A in "SI 2. Indicators and Factors" for details

# Table 17

Table17 <- ins_indicators %>% 
  select(COUNTRIES,Swaps) %>%
  distinct %>%
  filter(!is.na(Swaps)) %>%
  mutate(Swaps_norm=normalize_positive(Swaps)) %>% # Normalization positive
  rowwise() %>%
  mutate(PROPERTY.RIGHTS=Swaps_norm) %>% # PROPERTY.RIGHTS factor equals Swaps normalized
  ungroup()

# Prepare for word

to.plot <- Table17 %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

header_labels <- c("COUNTRIES","Swaps\n(million € 2000 - 2006)","Swaps'\n(normalized)","PROPERTY RIGHTS")

names(header_labels) <- names(to.plot)

Ft <- do.call(set_header_labels,c(list(x=Ft),header_labels))

write_doc(Ft,
          "Table 17. Indicators and normalization of Property Rights.",
          "Tables/Table17SI.docx")


##### 4.3 CATCH QUOTAS (I3) #####

# See "CATCH QUOTAS (I3)" in 3.A in "SI 2. Indicators and Factors" for details

#Table 18

Table18 <- ins_indicators  %>% 
  select(COUNTRIES,STOCK,TAC) %>% 
  spread(COUNTRIES,TAC)

# Prepare for word

to.plot <- Table18 %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[,match(c("STOCK",countries_order),names(to.plot))]

to.plot$STOCK <- gsub("_","/\n",to.plot$STOCK)

# Save to word

Ft<- format_table(to.plot)

Ft %<>% fontsize(j=1,size=10) %>% italic(j=1)


write_doc(Ft,
          "Table 18. TAC (million tons) per stock and country (2015)",
          "Tables/Table18SI.docx",landscape = TRUE)

# Prepare for word

#to.plot <- Table14 %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame %>%
#  set_names(c("COUNTRIES","Research","Management","normalizedResearch","normalizedMng","ADAPTIVE.MNG"))

#to.plot[is.na(to.plot)] <- "-"

#to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Table 19

Table19p <- ins_indicators %>% 
  select(SPECIES,STOCK,COUNTRIES,Above_advice)

Table19 <- Table19p %>%
  group_by(COUNTRIES,SPECIES) %>% 
  summarise(Above_advice=sum(Above_advice, na.rm = TRUE)) %>% # Sum stocks by country and species
  ungroup()%>%
  mutate(Above_advice_norm=normalize_negative(Above_advice) # Normalization negative
  ) %>% 
  rowwise() %>%
  mutate(QUOTAS=Above_advice_norm,na.rm=TRUE)


# Prepare for word

to.plot <- Table19 %>% select(SPECIES,COUNTRIES,QUOTAS) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"0.000",sprintf("%0.3f",.)))) %>% # Numbers to string
  spread(SPECIES,QUOTAS) %>% data.frame(check.names = TRUE)


to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

header_labels <- c("COUNTRIES",paste0("QUOTAS\n",names(to.plot)[2:3] %>% gsub("\\."," ",.) %>% species_sort_name %>% toupper))

names(header_labels) <- names(to.plot)

Ft <- do.call(set_header_labels,c(list(x=Ft),header_labels))

write_doc(Ft,
          "Table 19. Factor Quota values per country.",
          "Tables/Table19SI.docx")


##### 4.4 INSTITUTIONAL STRENGTH (I4) #####

# See "INSTITUTIONAL STRENGTH (I4)" in 3.A in "SI 2. Indicators and Factors" for details

# Table 20

Table20 <-ins_indicators %>% 
  select(COUNTRIES,Compliance) %>%
  distinct %>%
  mutate(Compliance_norm=normalize_negative(Compliance)) %>% # Normalization negative
  rowwise() %>%
  mutate(STRENGTH=Compliance_norm) %>% # DEVELOPMENT factos is HDI normalized
  ungroup()

# Prepare for word

to.plot <- Table20 %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),] 

# Save to word

Ft<- format_table(to.plot)

header_labels <- c("COUNTRIES","Compliance", "Compliance'\n(normalized)", "STRENGTH")

names(header_labels) <- names(to.plot)

Ft <- do.call(set_header_labels,c(list(x=Ft),header_labels))

write_doc(Ft,
          "Table 20. Development indicator and factor.",
          "Tables/Table20SI.docx")


##### 4.5 INSTITUTIONAL FACTORS #####

# See section 3.B in "SI 2. Indicators and Factors" for details

# Table 21. Merge tables 16, 17, 19 and 20

Table21 <- reduce(list(Table16 %>% select(COUNTRIES,ORGANIZATION),
                       Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
                       Table19 %>% select(SPECIES,COUNTRIES,QUOTAS),
                       Table20 %>% select(COUNTRIES,STRENGTH)),full_join,by="COUNTRIES")

# Prepare for word

to.plot <- Table21 %>% 
  mutate(SPECIES=paste0("QUOTAS\n",species_sort_name(SPECIES))) %>% 
  spread(SPECIES,QUOTAS) %>% 
  mutate_at(vars(starts_with("QUOTAS")),funs(ifelse(is.na(.),"0.000",sprintf("%0.3f",.)))) %>% # Numeric to string
  mutate_at(vars(starts_with("ORG"),starts_with("PROPERTY")),funs(ifelse(is.na(.),"-",sprintf("%0.2f",.)))) %>%  # Numeric to string
  mutate_at(vars(starts_with("STRENGTH")),funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% data.frame  # Numeric to string

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 21. List of Institutional factors.",
          "Tables/Table21SI.docx")


# Merge tables 16, 17, 19 not normalized and 20 to produce institutional factors per stock: institutional_factors.csv

reduce(list(Table16 %>% select(COUNTRIES,ORGANIZATION),
            Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
            Table19p %>% select(SPECIES, STOCK,COUNTRIES),
            Table20 %>% select(COUNTRIES,STRENGTH)),full_join,by="COUNTRIES") %>%
  select(SPECIES,COUNTRIES,STOCK,STRENGTH,PROPERTY.RIGHTS,ORGANIZATION) %>%
  left_join(countries_dependence,by = c("COUNTRIES", "SPECIES")) %>% # Keep only countries that depend on each species
  filter(dependence) %>% select(-dependence) %>%
  write_excel_csv("data/institutional_factors.csv")

# Merge tables 16, 17, 19 and 20 to produce institutional factors per country: institutional_factors_country.csv

reduce(list(Table16 %>% select(COUNTRIES,ORGANIZATION),
            Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
            Table19 %>% select(SPECIES, COUNTRIES,QUOTAS),
            Table20 %>% select(COUNTRIES,STRENGTH)),full_join,by="COUNTRIES") %>%
  select(SPECIES,COUNTRIES,STRENGTH,QUOTAS,PROPERTY.RIGHTS,ORGANIZATION) %>%
  left_join(countries_dependence,by = c("COUNTRIES", "SPECIES")) %>% # Keep only countries that depend on each species
  filter(dependence) %>% select(-dependence) %>%
  write_excel_csv("data/institutional_factors_country.csv")



# Enable scientific notation again

options(scipen=scipen)



