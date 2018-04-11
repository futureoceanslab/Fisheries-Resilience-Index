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


format_table <- function(data){
  # Prepare the table
  
  Ft <- FlexTable(data,add.rownames = FALSE)
  
  # Table header format
  Ft[to="header"] <- textProperties(font.size = 10,font.weight = "bold")
  Ft[to="header"] <- parProperties(text.align = "center")
  
  # General table format
  Ft[] <- textProperties(font.size = 10)
  
  Ft[] <- parProperties(text.align = "center")
  
  # First column format
  Ft[,1] <- textProperties(font.size = 10,font.style   = "italic")
  
  Ft[to="header"] <- textProperties(font.style = "italic")
  
  Ft[,1] <- parProperties(text.align = "left")
  
  Ft[,2:ncol(data)] <- cellProperties(border.style="none")
  
  Ft[1:(nrow(data)),1] <- cellProperties(border.bottom.style =  "none",border.top.style = "none",border.left.style = "none")
  
  Ft[to="header"] <- cellProperties(border.right.style =  "none",border.top.style = "none",border.left.style = "none")
  
  Ft[seq(1,nrow(data),2),2:ncol(data)] <- cellProperties(background.color = "gray90",border.style="none")
  
  Ft[nrow(data),2:ncol(data)] <- cellProperties(background.color = ifelse(nrow(data)%%2==0,"white","gray90"),border.bottom.style = "solid",border.right.style =  "none",border.top.style = "none",border.left.style = "none")
  
  Ft[nrow(data),1] <- cellProperties(border.bottom.style =  "solid",border.top.style = "none",border.left.style = "none")
  
  Ft
}

write_doc <- function(Ft,title,outfile,landscape=FALSE){
  
  doc <- docx()
  
  if(landscape){
    doc %<>% addSection(landscape=TRUE)
  }
  
  # Empty line
  doc %<>% addParagraph("")
  
  # Table title
  
  
  
  doc %<>% addParagraph(title,stylename = "En-tte")
  
  
  
  
  # Add table  
  
  doc %<>% addFlexTable(Ft,offx=-1)
  
  if(landscape){
    doc %<>% addSection()
  }
  
  writeDoc(doc,file=outfile)
  
}

countries_order <- c("BE","DK","DE","EE","IE","ES","FR","LV","LT","NL","PL","PT","FI","SE")
species_order <- c("European hake","Atlantic cod")
stocks_order1 <- c("HAKENRTN","HAKESOTH","CODCOASTNOR_CODNEAR","CODFAPL","CODICE","CODBA2532","CODKAT","CODIS","CODVIa","CODNS")


arrange_table <- function(df,stocks_order=stocks_order1) df %>% 
  mutate(SPECIES=factor(SPECIES,levels=species_order),STOCK=factor(STOCK,levels=stocks_order),COUNTRIES=factor(COUNTRIES,levels=countries_order)) %>% 
  arrange(COUNTRIES,SPECIES,STOCK) %>% 
  mutate(SPECIES=as.character(SPECIES),STOCK=as.character(STOCK),COUNTRIES=as.character(COUNTRIES))

normalize_positive <-function(x) (x - min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))

normalize_negative <-function(x) (max(x,na.rm = TRUE)-x)/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))

species_sort_name <- function(x) case_when(x=="Atlantic cod" ~ "cod",
                                           x=="European hake" ~ "hake",
                                           TRUE ~ x
)

species_long_name <- function(x) case_when(x=="cod" ~"Atlantic cod",
                                           x=="hake" ~"European hake",
                                           TRUE ~ x
)


##### 2. ECOLOGICAL INDICATORS #####

# Preprocess ecologic data

# ab <- fread("../abundance_indicators.csv",check.names = TRUE,na.strings = ".") %>% select(STOCK,B_SSBrecent=SSBrecent,B_SSBhistoric=SSBhistoric,B_Ftrend=Ftrend,B_Rtrend=Rtrend,ends_with(".average"))
# 
# eco_indicators <- bind_rows(fread("data/ecological_indicators_hake.csv",check.names = TRUE),
#                             fread("data/ecological_indicators_cod.csv",check.names = TRUE))
# 
# eco_indicators %>% select(-SSBhistoric,-SSBrecent,-Ftrend,-Rtrend) %>% left_join(ab,by="STOCK") %>% write_excel_csv("data/ecological_indicators.csv")

eco_indicators <- fread("data/ecological_indicators.csv",check.names = TRUE)





###### 2.1 ABUNDANCE (E2) #####

Table2 <- eco_indicators %>% select(STOCK,B_SSBrecent,B_SSBhistoric,B_F=B_Ftrend,B_R=B_Rtrend,SSB.average,F.average,R.average)

to.plot <- Table2 %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)



write_doc(Ft,
          "Table 2. Value of the coefficients (ß) from the linear models of SSB historic (1950- 2010), SSB recent (1980-2010), R and F in ICES data series. Significance at the 0.001 (***), 0.01 (**) and 0.05 (*) levels.",
          "Tables/Table2SI.docx",landscape=TRUE)


Table3 <- eco_indicators  %>% select(SPECIES,STOCK,B_SSBrecent,B_SSBhistoric,B_Ftrend,B_Rtrend,SSB.average,F.average,R.average)%>%
  mutate(SSBrecent=B_SSBrecent/SSB.average,SSBhistoric=B_SSBhistoric/SSB.average,Ftrend=B_Ftrend/F.average,Rtrend=B_Rtrend/R.average) %>%
  mutate(SSBrecent_norm=normalize_positive(SSBrecent),SSBhistoric_norm=normalize_positive(SSBhistoric),Ftrend_norm=normalize_negative(Ftrend),Rtrend_norm=normalize_positive(Rtrend)) %>%
  rowwise %>% mutate(ABUNDANCE=mean(SSBhistoric_norm,SSBrecent_norm,Ftrend_norm,Rtrend_norm,na.rm=TRUE)) %>% 
  ungroup() %>% select(-starts_with("B_"),-ends_with(".average"))


to.plot <- Table3%>% select(-SPECIES) %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)



write_doc(Ft,
          "Table 3. Normalization of Abundance indicators and Abundance factor calculation.",
          "Tables/Table3SI.docx",landscape=TRUE)



###### 2.2 TEMPERATURE (E3) #####

Trange_2 <- 0
Trange_98 <- 11
T50_2 <- 0
T50_98 <- 28

Table4 <- eco_indicators  %>% select(SPECIES,T50,Trange)%>% distinct() %>%
  mutate(Trange_norm=(Trange-Trange_2)/(Trange_98-Trange_2),T50_norm=(T50-T50_2)/(T50_98-T50_2)) %>% 
  mutate(TEMPERATURE=T50_norm,Trange_2=Trange_2,Trange_98=Trange_98,T50_2=T50_2,T50_98=T50_98)

to.plot <- Table4 %>% 
  mutate_if(is.numeric,funs(round(.,digits = 3))) %>% 
  data.frame %>% 
  select(SPECIES,Trange,T50,Trange_2,Trange_98,T50_2,T50_98,Trange_norm,T50_norm,TEMPERATURE) %>%
  set_names(c("SPECIES","temperature\nrange\n(ºC)","mean\ntemp\n(ºC)","Trang\ne2%\n(ºC)","Trang\ne98%\n(ºC)","T50\ne2%\n(ºC)","T50\ne98%\n(ºC)","Trange'\normalized","T50'\nnormalized","TEMPERATURE"))

to.plot[is.na(to.plot)] <- "-"

to.plot

Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 4. Temperature indicators, normalization and factor.",
          "Tables/Table4SI.docx",landscape = TRUE)



###### 2.3 OVEREXPLOITATION (E4) #####

Table5 <- eco_indicators  %>% select(SPECIES,STOCK,OverMSY,Status) %>% 
  mutate(OverMSY_norm=normalize_negative(OverMSY),Status_norm=normalize_positive(Status)) %>% 
  rowwise() %>%
  mutate(OVEREXPLOITATION=mean(c(OverMSY_norm,Status_norm),na.rm=TRUE))

to.plot <- Table5 %>% 
  select(-SPECIES) %>% 
  mutate_if(is.numeric,funs(round(.,digits = 3))) %>%
  select(STOCK,OverMSY,Status,OverMSY_norm,Status_norm,OVEREXPLOITATION) %>%
  data.frame %>% 
  set_names(c("STOCK","OverMSY","Status","OverMSY'\normalized","Status'\nnormalized","OVEREXPLOITATION"))
  

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 5. Overexploitation indicators, normalization and factor.",
          "Tables/Table5SI.docx")



##### 2.4 RECOVERY (E5) #####

recovery_2 <- 1
recovery_98 <- 43.34

Table6 <- eco_indicators  %>% select(SPECIES,STOCK,Recovery) %>% 
  mutate(Recovery_norm=1-(Recovery-recovery_2)/(recovery_98-recovery_2)) %>% 
  rowwise() %>%
  mutate(RECOVERY=Recovery_norm,recovery_2=recovery_2,recovery_98=recovery_98)

to.plot <- Table6 %>% 
  select(-SPECIES) %>% 
  mutate_if(is.numeric,funs(round(.,digits = 2))) %>% 
  select(STOCK,Recovery,recovery_2,recovery_98,Recovery_norm,RECOVERY) %>%
  data.frame %>%
  set_names(c("STOCK","recovery","recovery 2%","recovery 98%","Recovery'\nnormalized","RECOVERY"))

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 6. Recovery indicator, normalization and factor.",
          "Tables/Table6SI.docx")


##### 2.5. ECOLOGICAL FACTORS #####

# Table 7

Table7 <- reduce(list(Table3 %>% select(SPECIES,STOCK,ABUNDANCE),
                      Table5 %>% select(SPECIES,STOCK,OVEREXPLOITATION),
                      Table6 %>% select(SPECIES,STOCK,RECOVERY)),full_join,by=c("SPECIES","STOCK")) %>% 
  full_join(Table4 %>% select(SPECIES,TEMPERATURE),by="SPECIES")

to.plot <- Table7 %>% 
  select(STOCK,ABUNDANCE,TEMPERATURE,OVEREXPLOITATION,RECOVERY) %>% 
  mutate_if(is.numeric,funs(round(.,digits = 4))) %>% 
  data.frame

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 7. Ecological factors per stock.",
          "Tables/Table7SI.docx")


Table7 %>% 
  select(SPECIES,STOCK,ABUNDANCE,TEMPERATURE,OVEREXPLOITATION,RECOVERY) %>%
  write_excel_csv("data/ecological_factors.csv")
  

# Table 8

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




stocks_fished <-  lapply(countries_fishing, function(x) countries_order %in% x) %>% 
  data.frame() %>% t %>% data.frame()  %>% 
  set_names(countries_order) %>% mutate(STOCK=rownames(.)) %>% 
  select(STOCK,one_of(names(.)))

Table8 <- stocks_fished %>% mutate_if(is.logical,funs(ifelse(.,"yes","-")))


to.plot <- Table8 %>% data.frame


Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 8. Stocks that are being fished by EU countries (2006-2010).",
          "Tables/Table8SI.docx")


# Table 9


eco_countries <- lapply(countries_order, function(country){
  
  #country <- "PT" 
  
  stocks_fished %>% 
    filter_at(vars(one_of(country)),all_vars(.)) %>% 
    filter(STOCK!="CODNEAR") %>% 
    select(STOCK) %>% 
    left_join(Table7,by="STOCK") %>% 
    select(-STOCK) %>% 
    group_by(SPECIES) %>%
    summarise_all(funs(mean(.,na.rm=TRUE))) %>% 
    ungroup() %>% 
    mutate(COUNTRIES=country)
  
  
}) %>% bind_rows() %>% arrange(SPECIES,COUNTRIES) %>% select(COUNTRIES,SPECIES,one_of(names(.)))


Table9 <- eco_countries %>% gather(FACTOR,VALUE,-SPECIES,-COUNTRIES) %>% 
  unite(SP_FACTOR,SPECIES,FACTOR) %>% 
  spread(SP_FACTOR,VALUE) %>% 
  mutate_if(is.numeric,funs(round(.,digits = 3))) %>% 
  set_names(gsub("Atlantic cod_|European hake_","",names(.)))
  

to.plot <- Table9[match(countries_order,Table9$COUNTRIES),]

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)

Ft<- addHeaderRow(Ft,c("","Cod","Hake"),c(1,4,4),first = TRUE)

write_doc(Ft,
          "Table 9. Ecological Factors per fishing country and species.",
          "Tables/Table9SI.docx",landscape = TRUE)



eco_countries %>%  mutate_if(is.numeric,funs(round(.,digits = 9))) %>% 
  select(SPECIES,COUNTRIES,"ABUNDANCE","TEMPERATURE","OVEREXPLOITATION","RECOVERY") %>%
  write_excel_csv("data/ecological_factors_country.csv")

##### 3. SOCIOECONOMIC INDICATORS #####

# soc_indicators <- bind_rows(fread("data/socioeconomic_indicators_cod.csv",check.names = TRUE),
#                             fread("data/socioeconomic_indicators_hake.csv",check.names = TRUE)) %>%
#   arrange(COUNTRIES,STOCK)
# 
# 
# write_excel_csv(soc_indicators,"data/socioeconomic_indicators.csv")

soc_indicators <- fread("data/socioeconomic_indicators.csv")

##### 3.1 GEAR.DIVERSITY #####


Table10 <- soc_indicators %>% 
  select(SPECIES,SPgear) %>% 
  distinct %>% 
  mutate(SPgear_norm=normalize_positive(SPgear), GEAR.DIVERSITY=SPgear_norm)

to.plot <- Table10 %>% mutate(SPECIES=tools::toTitleCase(species_sort_name(SPECIES))) %>%
  data.frame %>%
  set_names("","Gear Diversity","NormalizedGearDiv","GEAR.DIVERSITY")

Ft <- format_table(to.plot)

write_doc(Ft,
          "Table 10. Values and normalization of Gear Diversity.",
          "Tables/Table10SI.docx")

###### 3.2 FLEET.MOBILITY #####

Table11 <- soc_indicators %>% 
  select(COUNTRIES,ICESareas5,ICESareasEU) %>% 
  distinct %>% 
  mutate(ICESareas5_norm=normalize_positive(ICESareas5),ICESareasEU_norm=normalize_positive(ICESareasEU)) %>% 
  rowwise() %>%
  mutate(FLEET.MOBILITY=mean(c(ICESareas5_norm,ICESareasEU_norm),na.rm=TRUE)) %>% 
  ungroup()

to.plot <- Table11 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame %>%
  set_names(c("COUNTRIES","ICESareas5","ICESareasEU","Normalized\nICESareas5","Normalized\nICESareasEU","FLEET.MOBILITY"))

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)


write_doc(Ft,
          "Table 11. Indicators and normalization of Fleet Mobility factor.",
          "Tables/Table11SI.docx")


##### 3.3 CATCH.DEP #####

Table12 <- soc_indicators %>% arrange(COUNTRIES,SPECIES,STOCK) %>% 
  select(SPECIES,COUNTRIES,STOCK,Stockdep.sp,Stockdep.total) %>%
  mutate(Stockdep.sp_norm=normalize_negative(Stockdep.sp),Stockdep.total_norm=1-normalize_positive(Stockdep.total)) %>%
  rowwise() %>%
  mutate(CATCH.DEP=mean(c(Stockdep.sp_norm,Stockdep.total_norm),na.rm=TRUE)) %>% 
  ungroup() %>% arrange_table()



to.plot <- Table12 %>% select(-SPECIES) %>% mutate_if(is.numeric,funs(round(.,digits = 4))) %>% data.frame %>%
  set_names(c("COUNTRIES","STOCK","Sockdep.sp","Stockdep.total","normalized\nspecie","normalized\ntotal","CATCH.DEP"))

to.plot[is.na(to.plot)] <- "-"

Ft<- format_table(to.plot)

Ft <- spanFlexTableRows(Ft,1,runs = to.plot[,1])

Ft[,2] <- parProperties(text.align = "left")

write_doc(Ft,
          "Table 12. Catch dependency of countries on stocks.",
          "Tables/Table12SI.docx")

Table13 <- Table12 %>% 
  group_by(SPECIES,COUNTRIES) %>% 
  summarise(CATCH.DEP=mean(CATCH.DEP,na.rm=TRUE)) %>% 
  
  ungroup() 

to.plot <- Table13 %>%
  mutate(SPECIES=paste0("CATCH.DEP\n",tools::toTitleCase(species_sort_name(SPECIES)))) %>%
  spread(SPECIES,CATCH.DEP) %>% mutate_if(is.numeric,funs(round(.,digits = 3)))

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

to.plot[is.na(to.plot)] <- "-"

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 13. Catch dependency factors per country.",
          "Tables/Table13SI.docx")


###### 3.4 ADAPTIVE MANAGEMENT #####

Table14 <- soc_indicators %>% 
  select(COUNTRIES,Research,Management) %>%
  distinct %>%
  mutate(Research_norm=normalize_positive(Research),Management_norm=normalize_positive(Management)) %>%
  rowwise() %>%
  mutate(ADAPTIVE.MNG=mean(c(Research_norm,Management_norm),na.rm=TRUE)) %>% 
  ungroup()

to.plot <- Table14 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame %>%
  set_names(c("COUNTRIES","Research","Management","normalizedResearch","normalizedMng","ADAPTIVE.MNG"))

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 14. Adaptive Management indicator values and normalization.",
          "Tables/Table14SI.docx")


##### 3.5 SOCIOECONOMIC FACTORS #####

Table15 <- reduce(list(Table11 %>% select(COUNTRIES,FLEET.MOBILITY),
                       Table13 %>% select(SPECIES,COUNTRIES,CATCH.DEP) %>%
                         mutate(SPECIES=paste0("CATCH.DEP\n",species_sort_name(SPECIES))) %>%
                         spread(SPECIES,CATCH.DEP) ,
                       Table14 %>% select(COUNTRIES,ADAPTIVE.MNG)),full_join,by="COUNTRIES")




t10 <- Table10 %>% select(SPECIES,GEAR.DIVERSITY) %>% mutate(SPECIES=paste0("GEAR.DIV\n",species_sort_name(SPECIES))) %>%
  spread(SPECIES,GEAR.DIVERSITY)

Table15 <- bind_cols(Table15,t10[rep(1,nrow(Table15)),])

to.plot <- Table15 %>% mutate_at(vars(starts_with("CATCH")),funs(round(.,digits = 3))) %>%
  mutate_at(vars(starts_with("ADAP"),starts_with("FLEET")),funs(round(.,digits = 2)))%>% 
  select(COUNTRIES,starts_with("GEAR"),FLEET.MOBILITY,starts_with("CATCH"),ADAPTIVE.MNG) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 15. Socioeconomic Factors",
          "Tables/Table15SI.docx")


reduce(list(Table11 %>% select(COUNTRIES,FLEET.MOBILITY),
            Table12 %>% select(SPECIES,STOCK,COUNTRIES,CATCH.DEP),
            Table14 %>% select(COUNTRIES,ADAPTIVE.MNG)),full_join,by="COUNTRIES") %>%
  full_join(Table10 %>% select(SPECIES,GEAR.DIVERSITY), by="SPECIES") %>% 
  select("SPECIES","COUNTRIES","STOCK","ADAPTIVE.MNG","CATCH.DEP","FLEET.MOBILITY","GEAR.DIVERSITY") %>%
  write_excel_csv("data/socioeconomic_factors.csv")

reduce(list(Table11 %>% select(COUNTRIES,FLEET.MOBILITY),
            Table13 %>% select(SPECIES,COUNTRIES,CATCH.DEP),
            Table14 %>% select(COUNTRIES,ADAPTIVE.MNG)),full_join,by="COUNTRIES") %>%
  full_join(Table10 %>% select(SPECIES,GEAR.DIVERSITY), by="SPECIES") %>% 
  select("SPECIES","COUNTRIES","ADAPTIVE.MNG","CATCH.DEP","FLEET.MOBILITY","GEAR.DIVERSITY") %>%
  write_excel_csv("data/socioeconomic_factors_country.csv")


##### 4 INSTITUTIONAL INDICATORS #####


# ins_indicators <- bind_rows(fread("data/institutional_indicators_cod.csv",check.names = TRUE),
#                             fread("data/institutional_indicators_hake.csv",check.names = TRUE)) %>% 
#   arrange(COUNTRIES,STOCK)
# 
# quota <- readxl::read_excel("../institutionalfactors.xlsx","QUOTA",na = "NA") %>% select(1:4) %>% mutate(SPECIES=species_long_name(SPECIES))
# 
# ins_indicators %<>% select(-TAC) %>% full_join(quota,by=c("SPECIES","COUNTRIES","STOCK"))
# 
# write_excel_csv(ins_indicators,"data/institutional_indicators.csv")

ins_indicators <- fread("data/institutional_indicators.csv")


##### 4.1 CO.MANAGEMENT (I1) #####


Table16 <-ins_indicators %>% 
  select(COUNTRIES,Norganizations) %>%
  distinct %>%
  mutate(Norganizations_norm=normalize_positive(Norganizations)) %>%
  rowwise() %>%
  mutate(CO.MANAGEMENT=Norganizations_norm) %>% 
  ungroup()

to.plot <- Table16 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 16. Values, normalization and Co-Management factor.",
          "Tables/Table16SI.docx")

##### 4.2 PROPERTY.RIGHTS (I2) #####

Table17 <- ins_indicators %>% 
  select(COUNTRIES,Swaps) %>%
  distinct %>%
  mutate(Swaps_norm=normalize_positive(Swaps)) %>%
  rowwise() %>%
  mutate(PROPERTY.RIGHTS=Swaps_norm) %>% 
  ungroup()

to.plot <- Table17 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 17. Indicators and normalization of Property Rights.",
          "Tables/Table17SI.docx")


##### 4.3 CATCH QUOTAS (I3) #####

Table18 <- ins_indicators  %>% 
  select(COUNTRIES,STOCK,TAC) %>% 
  spread(COUNTRIES,TAC)

to.plot <- Table18 %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[,match(c("STOCK",countries_order),names(to.plot))]

Ft<- format_table(to.plot)

Ft[,1] <- textProperties(font.size = 8,font.weight = "bold")

write_doc(Ft,
          "Table 18. TAC (million tons) per stock and country (2015)",
          
          "Tables/Table18SI.docx",landscape = TRUE)

Table19p <- ins_indicators %>% 
  select(SPECIES,STOCK,COUNTRIES,TAC)

Table19 <- Table19p %>%
  group_by(COUNTRIES,SPECIES) %>% 
  summarise(TAC=sum(TAC,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(TAC_norm=normalize_positive(TAC)) %>%
  mutate(QUOTAS=TAC_norm)


to.plot <- Table19 %>% select(SPECIES,COUNTRIES,QUOTAS) %>% 
  mutate_if(is.numeric,funs(round(.,digits = 3))) %>% 
  mutate(SPECIES=paste0("QUOTAS\n",species_sort_name(SPECIES))) %>% 
  spread(SPECIES,QUOTAS)


to.plot[is.na(to.plot)] <- 0.0

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)



write_doc(Ft,
          "Table 19. Factor Quota values per country.",
          "Tables/Table19SI.docx")

##### 4.4 DEVELOPMENT (I4) #####

Table20 <-ins_indicators %>% 
  select(COUNTRIES,HDI) %>%
  distinct %>%
  mutate(HDI_norm=normalize_positive(HDI)) %>%
  rowwise() %>%
  mutate(DEVELOPMENT=HDI_norm) %>% 
  ungroup()

to.plot <- Table20 %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 20. Development indicator and factor.",
          "Tables/Table20SI.docx")


##### 4.5 INSTITUTIONAL FACTORS #####

Table21 <- reduce(list(Table16 %>% select(COUNTRIES,CO.MANAGEMENT),
                       Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
                       Table19 %>% select(SPECIES,COUNTRIES,QUOTAS),
                       Table20 %>% select(COUNTRIES,DEVELOPMENT)),full_join,by="COUNTRIES")



to.plot <- Table21 %>% 
  mutate(SPECIES=paste0("QUOTAS\n",species_sort_name(SPECIES))) %>% 
  spread(SPECIES,QUOTAS) %>% 
  mutate_at(vars(starts_with("QUOTAS")),funs(ifelse(is.na(.),0.0,round(.,digits = 3)))) %>% 
  mutate_at(vars(starts_with("CO.MANAG"),starts_with("PROPERTY")),funs(round(.,digits = 2))) %>%
  mutate_at(vars(starts_with("DEVELOPMENT")),funs(round(.,digits = 3))) %>% data.frame



to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(countries_order,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)



write_doc(Ft,
          "Table 21. List of Institutional factors.",
          "Tables/Table21SI.docx")



reduce(list(Table16 %>% select(COUNTRIES,CO.MANAGEMENT),
            Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
            Table19p %>% select(SPECIES, STOCK,COUNTRIES,TAC),
            Table20 %>% select(COUNTRIES,DEVELOPMENT)),full_join,by="COUNTRIES") %>%
  select(SPECIES,COUNTRIES,STOCK,DEVELOPMENT,TAC,PROPERTY.RIGHTS,CO.MANAGEMENT) %>%
  mutate_if(is.numeric,funs(round(.,digits = 4))) %>% 
  write_excel_csv("data/institutional_factors.csv")

reduce(list(Table16 %>% select(COUNTRIES,CO.MANAGEMENT),
            Table17 %>% select(COUNTRIES,PROPERTY.RIGHTS),
            Table19 %>% select(SPECIES, COUNTRIES,QUOTAS),
            Table20 %>% select(COUNTRIES,DEVELOPMENT)),full_join,by="COUNTRIES") %>%
  select(SPECIES,COUNTRIES,DEVELOPMENT,QUOTAS,PROPERTY.RIGHTS,CO.MANAGEMENT) %>%
  mutate_if(is.numeric,funs(round(.,digits = 4))) %>% 
  write_excel_csv("data/institutional_factors_country.csv")