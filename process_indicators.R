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
#' data/socioeconomic_factors.csv
#' data/institutional_factors.csv
#' data/ecological_factors.csv
#' data/socioeconomic_factors_country.csv
#' data/institutional_factors_country.csv
#' data/ecological_factors_country.csv
#' 
#' Tables/Table2SI.docx
#' Tables/Table3SI.docx
#' Tables/Table4SI.docx
#' Tables/Table5SI.docx
#' Tables/Table6SI.docx
#' Tables/Table8SI.docx
#' Tables/Table9SI.docx
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

if(!require(ReporteRs)){
  install.packages('ReporteRs',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(ReporteRs)
packageVersion("ReporteRs")
# [1] ‘0.8.10’

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

source("aux_functions.R")

# Disable scientific notation
scipen <- getOption("scipen")
options(scipen=999)
##### 2. ECOLOGICAL INDICATORS #####

eco_indicators <- fread("data/ecological_indicators.csv",check.names = TRUE)


###### 2.1 AREA (E1) #####

# See section "AREA (E1)" in 1.A in "SI 2. Indicators and Factors" for details.

# Table 1

area_2 <- 261895.6 # 2nd percentile of the distribution of area ranges in García-Molinos et al. (2015)
area_98 <- 83378496 # 98th percentile of the distribution of area ranges in García-Molinos et al. (2015)

Table1 <- eco_indicators  %>% 
  select(SPECIES,area2006,area2100) %>% 
  distinct() %>%
  mutate(area2006_norm=(area2006-area_2)/(area_98-area_2), # Normalization positive
         area2100_norm=(area2100-area_2)/(area_98-area_2) # Normalization positive
  ) %>% 
  rowwise() %>%  # AREA factor is the mean of the normalized indicators above for each species (row)
  mutate(AREA=mean(area2006_norm,area2100_norm),
         area_2=area_2,
         area_98=area_98)

# Prepare for word
to.plot <- Table1 %>% 
  data.frame %>% 
  select(SPECIES,area2006,area2100,area_2,area_98,area2006_norm,area2100_norm,AREA) %>%
  set_names(c("SPECIES","Area2006\n(km^2)","Area2100\n(km^2)","area 2%\n(km^2)","area 98%\n(km^2)","Area06'\nNormalized","Area100\nNormalized","AREA"))

to.plot[is.na(to.plot)] <- "-"


# Save to word

Ft<- format_table(to.plot)
write_doc(Ft,
          "Table1. Area indicators and factor.",
          "Tables/Table1SI.docx")



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
          "Table 2. Value of the coefficients (ß) from the linear models of SSB historic (1950- 2010), SSB recent (1980-2010), R and F in ICES data series. Significance at the 0.001 (***), 0.01 (**) and 0.05 (*) levels.",
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
  mutate(ABUNDANCE=mean(SSBhistoric_norm,SSBrecent_norm,Ftrend_norm,Rtrend_norm,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(-starts_with("B_"),-ends_with(".average")) # Remove slopes and averages from the table

# Prepare for word

to.plot <- Table3%>% 
  select(-SPECIES) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame

# Save to word
Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 3. Normalization of Abundance indicators and Abundance factor calculation.",
          "Tables/Table3SI.docx",landscape=TRUE)



###### 2.3 TEMPERATURE (E3) #####

# See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details.

# Table 4

Trange_2 <- 0 # 2nd percentile of the distribution of temperature range. Cheung et al. (2010)
Trange_98 <- 11 # 98th percentile of the distribution of temperature range. Cheung et al. (2010)
T50_2 <- 0 # 2nd percentile of the distribution of temperature range. Cheung et al. (2010)
T50_98 <- 28 # 98th percentile of the distribution of temperature range. Cheung et al. (2010)

Table4 <- eco_indicators  %>% 
  select(SPECIES,T50,Trange)%>% 
  distinct() %>%
  mutate(Trange_norm=(Trange-Trange_2)/(Trange_98-Trange_2), # Normalization
         T50_norm=(T50-T50_2)/(T50_98-T50_2) # Normalization
  ) %>% 
  mutate(TEMPERATURE=T50_norm, # TEMPERATURE factor is normalized T50
         Trange_2=Trange_2,
         Trange_98=Trange_98,
         T50_2=T50_2,
         T50_98=T50_98)

# Prepare for word
to.plot <- Table4 %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame %>% 
  select(SPECIES,Trange,T50,Trange_2,Trange_98,T50_2,T50_98,Trange_norm,T50_norm,TEMPERATURE) %>%
  set_names(c("SPECIES","temperature\nrange\n(ºC)","mean\ntemp\n(ºC)","Trang\ne2%\n(ºC)","Trang\ne98%\n(ºC)","T50\ne2%\n(ºC)","T50\ne98%\n(ºC)","Trange'\normalized","T50'\nnormalized","TEMPERATURE"))




# Save to word

Ft<- format_table(to.plot)
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
  data.frame %>% 
  set_names(c("STOCK","OverMSY","Status","OverMSY'\normalized","Status'\nnormalized","OVEREXPLOITATION"))



# Save to word
Ft<- format_table(to.plot)

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
  data.frame %>%
  set_names(c("STOCK","recovery","recovery 2%","recovery 98%","Recovery'\nnormalized","RECOVERY"))


# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 6. Recovery indicator, normalization and factor.",
          "Tables/Table6SI.docx")


##### 2.6 ECOLOGICAL FACTORS #####

# See section 1.B in "SI 2. Indicators and Factors" for details

# Table 7

# Merge all the factors in one table

Table7 <- reduce(list(Table3 %>% select(SPECIES,STOCK,ABUNDANCE),
                      Table5 %>% select(SPECIES,STOCK,OVEREXPLOITATION),
                      Table6 %>% select(SPECIES,STOCK,RECOVERY)),full_join,by=c("SPECIES","STOCK")) %>% 
  full_join(Table4 %>% select(SPECIES,TEMPERATURE),by="SPECIES")

# Prepare for word

to.plot <- Table7 %>% 
  select(STOCK,ABUNDANCE,TEMPERATURE,OVEREXPLOITATION,RECOVERY) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.4f",.)))) %>% # Numbers to string
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


# Table 8

# Which countries fish  each stock
countries_fishing <- list(HAKENRTN=c("BE","DK","DE","ES","FR","NL","PT","SE"),
                          HAKESOTH=c("ES","FR","PT"),
                          CODCOASTNOR=c("DK","DE","EE","EI","ES","FR","PL","PT"),
                          CODNEAR=c("DK","DE","EE","EI","ES","FR","PL","PT"),
                          CODFAPL=c("DE"),
                          CODICE=c("DE"),
                          CODBA2532=c("DK","DE","EE","LV","LT","PL","FI","SE"),
                          CODKAT=c("DK","DE","FR","NL","PT","SE"),
                          CODIS=c("BE","IE","FR"),
                          CODVIa=c("DE","IE","FR"),
                          CODNS=c("FR"))


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


# Table 9

# Compute mean factor for each country. 

eco_countries <- lapply(countries_order, function(country){ # For each country
  
  
  stocks_fished %>% 
    filter_at(vars(one_of(country)),all_vars(.)) %>% # Keep the data for that country 
    select(STOCK) %>% 
    left_join(Table7,by="STOCK") %>% # Merge with data by stock
    select(-STOCK) %>% 
    group_by(SPECIES) %>% # For each species
    summarise_all(funs(mean(.,na.rm=TRUE))) %>% # Compute the mean of each factor
    ungroup() %>% 
    mutate(COUNTRIES=country)
  
  
}) %>% bind_rows() %>% # put all countries together
  arrange(SPECIES,COUNTRIES) %>% # ORder by species and countries
  select(COUNTRIES,SPECIES,everything()) # Reorder columns

# Prepare for word

Table9 <- eco_countries %>% 
  gather(FACTOR,VALUE,-SPECIES,-COUNTRIES) %>% 
  unite(SP_FACTOR,SPECIES,FACTOR) %>% 
  spread(SP_FACTOR,VALUE) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  set_names(gsub("Atlantic cod_|European hake_","",names(.)))

to.plot <- Table9[match(countries_order,Table9$COUNTRIES),]


# Save to word

Ft<- format_table(to.plot)

# Extra header
Ft<- addHeaderRow(Ft,c("","COD","HAKE"),c(1,4,4),first = TRUE, cell.properties = cellProperties(border.top.style = "none",border.left.style = "none",border.right.style = "none"),par.properties = parProperties(text.align = "center"))

write_doc(Ft,
          "Table 9. Ecological Factors per fishing country and species.",
          "Tables/Table9SI.docx",landscape = TRUE)


# Save factors by county to ecological_factors_country.csv

eco_countries %>%  mutate_if(is.numeric,funs(round(.,digits = 9))) %>% 
  select(SPECIES,COUNTRIES,"ABUNDANCE","TEMPERATURE","OVEREXPLOITATION","RECOVERY") %>%
  write_excel_csv("data/ecological_factors_country.csv")

##### 3. SOCIOECONOMIC INDICATORS #####

soc_indicators <- fread("data/socioeconomic_indicators.csv")

##### 3.1 GEAR.DIVERSITY #####

# See "GEAR.DIVERSITY (S1)" in 2.A in "SI 2. Indicators and Factors" for details

# Table 10

Table10 <- soc_indicators %>% 
  select(SPECIES,SPgear) %>% 
  distinct %>% 
  mutate(SPgear_norm=normalize_positive(SPgear), # Normalize positive
         GEAR.DIVERSITY=SPgear_norm) # GEAR.DIVERSITY factor is the SPgear normalized.

# Prepare for word

to.plot <- Table10 %>% mutate(SPECIES=tools::toTitleCase(species_sort_name(SPECIES))) %>%
  data.frame %>%
  set_names("","Gear Diversity","NormalizedGearDiv","GEAR.DIVERSITY")

# Save to word

Ft <- format_table(to.plot)

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
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.2f",.)))) %>% # Numbers to string
  data.frame %>%
  set_names(c("COUNTRIES","ICESareas5","ICESareasEU","Normalized\nICESareas5","Normalized\nICESareasEU","FLEET.MOBILITY"))


to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

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
  mutate(Stockdep.sp_norm=normalize_negative(Stockdep.sp), # Normalization negative
         Stockdep.total_norm=normalize_negative(Stockdep.total) # Normalization negative
  ) %>%
  rowwise() %>% # CATCH.DEP factor is the mean of the normalized indicators above for each stock (row)
  mutate(CATCH.DEP=mean(c(Stockdep.sp_norm,Stockdep.total_norm),na.rm=TRUE)) %>% 
  ungroup() %>% arrange_table()

# Prepare for word

to.plot <- Table12 %>% 
  select(-SPECIES) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.4f",.)))) %>% # Numbers to string
  data.frame %>%
  set_names(c("COUNTRIES","STOCK","Sockdep.sp","Stockdep.total","normalized\nspecie","normalized\ntotal","CATCH.DEP"))


# Save to word

Ft<- format_table(to.plot)

Ft <- spanFlexTableRows(Ft,1,runs = to.plot[,1])

Ft[,2] <- parProperties(text.align = "left")

country_rows <- rle(to.plot$COUNTRIES)$lengths %>% cumsum()

Ft[,2] <- cellProperties(border.bottom.style = "none",border.top.style = "none",border.left.style = "solid",border.right.style = "solid")

Ft[country_rows,1:2] <- cellProperties(border.bottom.style = "solid",border.top.style = "none",border.left.style = "none")
for(country_row in country_rows){

  Ft[country_row,3:ncol(to.plot)] <- cellProperties(background.color = ifelse(country_row%%2==0,"white","gray90"),border.bottom.style = "solid",border.right.style =  "none",border.top.style = "none",border.left.style = "none")
    
}


write_doc(Ft,
          "Table 12. Catch dependency of countries on stocks.",
          "Tables/Table12SI.docx")

# Table 13

Table13 <- Table12 %>% 
  group_by(SPECIES,COUNTRIES) %>%
  summarise(CATCH.DEP=mean(CATCH.DEP,na.rm=TRUE)) %>%  # Average catch dependency for each country
  ungroup() 

# Prepare for word

to.plot <- Table13 %>%
  mutate(SPECIES=paste0("CATCH.DEP\n",tools::toTitleCase(species_sort_name(SPECIES)))) %>%
  spread(SPECIES,CATCH.DEP) %>% mutate_if(is.numeric,funs(round(.,digits = 3)))

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

to.plot[is.na(to.plot)] <- "-"

# Save to word

Ft<- format_table(to.plot)

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

to.plot <- Table14 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame %>%
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

# Merge tables 11, 13 and 14.

Table15 <- reduce(
  list(
    Table11 %>% select(COUNTRIES,FLEET.MOBILITY),
    Table13 %>% select(SPECIES,COUNTRIES,CATCH.DEP) %>%
      mutate(SPECIES=paste0("CATCH.DEP\n",species_sort_name(SPECIES))) %>% # One column for each species
      spread(SPECIES,CATCH.DEP) ,
    Table14 %>% select(COUNTRIES,ADAPTIVE.MNG)
  ),full_join,by="COUNTRIES")


# Merge table 10 with all the others
t10 <- Table10 %>% select(SPECIES,GEAR.DIVERSITY) %>% 
  mutate(SPECIES=paste0("GEAR.DIV\n",species_sort_name(SPECIES))) %>% # One column for each species
  spread(SPECIES,GEAR.DIVERSITY)

Table15 <- bind_cols(Table15,t10[rep(1,nrow(Table15)),])

# Prepare for word
to.plot <- Table15 %>% 
  mutate_at(vars(starts_with("CATCH")),funs(round(.,digits = 3))) %>%
  mutate_at(vars(starts_with("ADAP"),starts_with("FLEET")),funs(round(.,digits = 2)))%>% 
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

reduce( # Merge tables 11, 12 and 14
  list(
    Table11 %>% select(COUNTRIES,FLEET.MOBILITY),
    Table12 %>% select(SPECIES,STOCK,COUNTRIES,CATCH.DEP),
    Table14 %>% select(COUNTRIES,ADAPTIVE.MNG)
  ),full_join,by="COUNTRIES") %>%
  full_join(Table10 %>% select(SPECIES,GEAR.DIVERSITY), by="SPECIES") %>% # Merge table 10
  select("SPECIES","COUNTRIES","STOCK","ADAPTIVE.MNG","CATCH.DEP","FLEET.MOBILITY","GEAR.DIVERSITY") %>% # Organize columns
  write_excel_csv("data/socioeconomic_factors.csv") # Save to csv

# Merge tables 11, 13, 10 and 14 to produce socioeconomic factors per country: socioeconomic_factors_country.csv

reduce( # Merge tables 11, 13 and 14
  list(
    Table11 %>% select(COUNTRIES,FLEET.MOBILITY),
    Table13 %>% select(SPECIES,COUNTRIES,CATCH.DEP),
    Table14 %>% select(COUNTRIES,ADAPTIVE.MNG)
  ),full_join,by="COUNTRIES") %>%
  full_join(Table10 %>% select(SPECIES,GEAR.DIVERSITY), by="SPECIES") %>% # Merge table 10
  select("SPECIES","COUNTRIES","ADAPTIVE.MNG","CATCH.DEP","FLEET.MOBILITY","GEAR.DIVERSITY") %>% # Organize columns
  write_excel_csv("data/socioeconomic_factors_country.csv") # Save to csv


##### 4 INSTITUTIONAL INDICATORS #####

ins_indicators <- fread("data/institutional_indicators 2.csv")


##### 4.1 CO.MANAGEMENT (I1) #####

# See "CO.MANAGEMENT (I1)" in 3.A in "SI 2. Indicators and Factors" for details

# Table 16

Table16 <-ins_indicators %>% 
  select(COUNTRIES,Norganizations) %>%
  distinct %>%
  filter(!is.na(Norganizations)) %>%
  mutate(Norganizations_norm=normalize_positive(Norganizations)) %>% # Normalization positive
  rowwise() %>%
  mutate(CO.MANAGEMENT=Norganizations_norm) %>% # CO.MANAGEMENT is Norganizations normalized
  ungroup()

# Prepare for word

to.plot <- Table16 %>%
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.2f",.)))) %>% # Numbers to string
  data.frame 

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),] %>%
  set_names(c("","Norganizations\n2017","Norganizations'\n(normalized)","CO.MANAGEMENT"))

# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 16. Values, normalization and Co-Management factor.",
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
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.2f",.)))) %>% # Numbers to string
  data.frame

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),] %>%
  set_names(c("COUNTRIES","Swaps\n(million € 2000 - 2006)","Swaps'\n(normalized)","PROPERTY.RIGHTS"))

# Save to word

Ft<- format_table(to.plot)

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

Ft[,1] <- textProperties(font.size = 10,font.style = "italic")

write_doc(Ft,
          "Table 18. TAC (million tons) per stock and country (2015)",
          
          "Tables/Table18SI.docx",landscape = TRUE)

# Prepare for word

to.plot <- Table14 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame %>%
  set_names(c("COUNTRIES","Research","Management","normalizedResearch","normalizedMng","ADAPTIVE.MNG"))

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Table 19

Table19p <- ins_indicators %>% 
  select(SPECIES,STOCK,COUNTRIES,TAC, Above_advice)

Table19 <- Table19p %>%
  group_by(COUNTRIES,SPECIES) %>% 
  summarise(TAC=sum(TAC,na.rm=TRUE), Above_advice=sum(Above_advice, na.rm = TRUE)) %>% # Sum stocks by country and species
  ungroup()%>%
  mutate(TAC_norm=normalize_positive(TAC), # Normalize positive
         Above_advice_norm=normalize_negative(Above_advice) # Normalization negative
         ) %>% 
  rowwise() %>%
  mutate(QUOTAS=mean(c(TAC_norm,Above_advice_norm),na.rm=TRUE))
  

# Prepare for word

to.plot <- Table19 %>% select(SPECIES,COUNTRIES,QUOTAS) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"0.000",sprintf("%0.3f",.)))) %>% # Numbers to string
  mutate(SPECIES=paste0("QUOTAS\n",species_sort_name(SPECIES))) %>%
  spread(SPECIES,QUOTAS)


to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 19. Factor Quota values per country.",
          "Tables/Table19SI.docx")


##### 4.4 DEVELOPMENT (I4) #####

# See "DEVELOPMENT (I4)" in 3.A in "SI 2. Indicators and Factors" for details

# Table 20

Table20 <-ins_indicators %>% 
  select(COUNTRIES,HDI) %>%
  distinct %>%
  filter(!is.na(HDI)) %>%
  mutate(HDI_norm=normalize_positive(HDI)) %>% # Normalization positive
  rowwise() %>%
  mutate(DEVELOPMENT=HDI_norm) %>% # DEVELOPMENT factos is HDI normalized
  ungroup()

# Prepare for word

to.plot <- Table20 %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% # Numbers to string
  data.frame

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),] %>%
  set_names(c("COUNTRIES","HDI","HDI'\n(normalized)","DEVELOPMENT"))

# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 20. Development indicator and factor.",
          "Tables/Table20SI.docx")


##### 4.5 INSTITUTIONAL FACTORS #####

# See section 3.B in "SI 2. Indicators and Factors" for details

# Table 21. Merge tables 16, 17, 19 and 20

Table21 <- reduce(list(Table16 %>% select(COUNTRIES,CO.MANAGEMENT),
                       Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
                       Table19 %>% select(SPECIES,COUNTRIES,QUOTAS),
                       Table20 %>% select(COUNTRIES,DEVELOPMENT)),full_join,by="COUNTRIES")

# Prepare for word

to.plot <- Table21 %>% 
  mutate(SPECIES=paste0("QUOTAS\n",species_sort_name(SPECIES))) %>% 
  spread(SPECIES,QUOTAS) %>% 
  mutate_at(vars(starts_with("QUOTAS")),funs(ifelse(is.na(.),"0.000",sprintf("%0.3f",.)))) %>% # Numeric to string
  mutate_at(vars(starts_with("CO.MANAG"),starts_with("PROPERTY")),funs(ifelse(is.na(.),"-",sprintf("%0.2f",.)))) %>%  # Numeric to string
  mutate_at(vars(starts_with("DEVELOPMENT")),funs(ifelse(is.na(.),"-",sprintf("%0.3f",.)))) %>% data.frame  # Numeric to string

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

# Save to word

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 21. List of Institutional factors.",
          "Tables/Table21SI.docx")


# Merge tables 16, 17, 19 not normalized and 20 to produce institutional factors per stock: institutional_factors.csv

reduce(list(Table16 %>% select(COUNTRIES,CO.MANAGEMENT),
            Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
            Table19p %>% select(SPECIES, STOCK,COUNTRIES,TAC),
            Table20 %>% select(COUNTRIES,DEVELOPMENT)),full_join,by="COUNTRIES") %>%
  select(SPECIES,COUNTRIES,STOCK,DEVELOPMENT,TAC,PROPERTY.RIGHTS,CO.MANAGEMENT) %>%
  write_excel_csv("data/institutional_factors.csv")

# Merge tables 16, 17, 19 and 20 to produce institutional factors per country: institutional_factors_country.csv

reduce(list(Table16 %>% select(COUNTRIES,CO.MANAGEMENT),
            Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
            Table19 %>% select(SPECIES, COUNTRIES,QUOTAS),
            Table20 %>% select(COUNTRIES,DEVELOPMENT)),full_join,by="COUNTRIES") %>%
  select(SPECIES,COUNTRIES,DEVELOPMENT,QUOTAS,PROPERTY.RIGHTS,CO.MANAGEMENT) %>%
  write_excel_csv("data/institutional_factors_country.csv")



# Enable scientific notation again

options(scipen=scipen)

