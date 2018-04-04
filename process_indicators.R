#' Reads:
#' 
#' data/institutional_indicators_hake.csv
#' data/institutional_indicators_cod.csv
#' data/socioeconomic_indicators_hake.csv
#' data/socioeconomic_indicators_cod.csv
#' data/ecological_indicators_hake.csv
#' data/ecological_indicators_cod.csv
#' 
#' and produces
#' 
#' data/socioeconomic_factors.csv
#' data/institutional_factors.csv
#' data/ecological_factors.csv
#' data/eco_country.csv
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
  Ft[,1] <- textProperties(font.size = 10,font.weight = "bold")
  
  Ft[,1] <- parProperties(text.align = "left")
  
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

normalize <-function(x) (x - min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))

species_sort_name <- function(x) case_when(x=="Atlantic cod" ~ "cod",
                                           x=="European hake" ~ "hake",
                                           TRUE ~ x
                                           )

##### 2. ECOLOGICAL INDICATORS #####

# Preprocess ecologic data

eco_indicators <- bind_rows(fread("data/ecological_indicators_hake.csv",check.names = TRUE),
                            fread("data/ecological_indicators_cod.csv",check.names = TRUE))

###### 2.1 ABUNDANCE (E2) #####

Table3 <- eco_indicators  %>% select(SPECIES,STOCK,SSBrecent,SSBhistoric,Ftrend,Rtrend)%>%
  mutate(SSBrecent_norm=normalize(SSBrecent),SSBhistoric_norm=normalize(SSBhistoric),Ftrend_norm=normalize(Ftrend),Rtrend_norm=normalize(Rtrend)) %>%
  rowwise %>% mutate(ABUNDANCE=mean(SSBhistoric_norm,SSBrecent_norm,Ftrend_norm,Rtrend_norm,na.rm=TRUE)) %>% 
  ungroup()


to.plot <- Table3%>% select(-SPECIES) %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)



write_doc(Ft,
          "Table 3. Normalization of Abundance indicators and Abundance factor calculation.",
          "Tables/Table3SI.docx",landscapt=TRUE)



###### 2.2 TEMPERATURE (E3) #####

Trange_2 <- 0
Trange_98 <- 11
T50_2 <- 0
T50_98 <- 28

Table4 <- eco_indicators  %>% select(SPECIES,T50=Trange,Trange=T50)%>% distinct() %>%
  mutate(Trange_norm=(Trange-Trange_2)/(Trange_98-Trange_2),T50_norm=(T50-T50_2)/(T50_98-T50_2)) %>% 
  mutate(TEMPERATURE=T50_norm)

to.plot <- Table4 %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 4. Temperature indicators, normalization and factor.",
          "Tables/Table4SI.docx")



###### 2.3 OVEREXPLOITATION (E4) #####

Table5 <- eco_indicators  %>% select(SPECIES,STOCK,OverMSY,Status) %>% 
  mutate(OverMSY_norm=1-normalize(OverMSY),Status_norm=normalize(Status)) %>% 
  rowwise() %>%
  mutate(OVEREXPLOITATION=mean(c(OverMSY_norm,Status_norm),na.rm=TRUE))

to.plot <- Table5 %>% select(-SPECIES) %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame

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
  mutate(RECOVERY=Recovery_norm)

to.plot <- Table6 %>% select(-SPECIES) %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 6. Recovery indicator, normalization and factor.",
          "Tables/Table6SI.docx")


##### 2.5. ECOLOGICAL FACTORS BY COUNTRY #####

# Table 7

Table7 <- reduce(list(Table3 %>% select(SPECIES,STOCK,ABUNDANCE),
                       Table5 %>% select(SPECIES,STOCK,OVEREXPLOITATION),
                      Table6 %>% select(SPECIES,STOCK,RECOVERY)),full_join,by=c("SPECIES","STOCK")) %>% 
  full_join(Table4 %>% select(SPECIES,TEMPERATURE),by="SPECIES")

to.plot <- Table7 %>% select(-SPECIES) %>% mutate_if(is.numeric,funs(round(.,digits = 4))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 7. Ecological factors per stock.",
          "Tables/Table7SI.docx")

write.csv(Table7%>% mutate_if(is.numeric,funs(round(.,digits = 3))),file="data/ecological_factors.csv",row.names = FALSE)

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

all_countries <- c("BE","DK","DE","EE","IE","ES","FR","LV","LT","NL","PL","PT","FI","SE")



stocks_fished <-  lapply(countries_fishing, function(x) all_countries %in% x) %>% data.frame() %>% t %>% data.frame()  %>% set_names(all_countries) %>% mutate(STOCK=rownames(.)) %>% select(STOCK,one_of(names(.)))

Table8 <- stocks_fished %>% mutate_if(is.logical,funs(ifelse(.,"yes","-")))


to.plot <- Table8 %>% data.frame




Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 8. Stocks that are being fished by EU countries (2006-2010).",
          "Tables/Table8SI.docx")


# Table 9


eco_countries <- lapply(all_countries, function(country){
  
  #country <- "PT" 
  
  stocks_fished %>% filter_at(vars(one_of(country)),all_vars(.)) %>% filter(STOCK!="CODNEAR") %>% 
    select(STOCK) %>% left_join(Table7,by="STOCK") %>% select(-STOCK) %>% group_by(SPECIES) %>%
    summarise_all(funs(mean(.,na.rm=TRUE))) %>% ungroup() %>% mutate(COUNTRIES=country)
  
  
}) %>% bind_rows() %>% arrange(SPECIES,COUNTRIES) %>% select(COUNTRIES,SPECIES,one_of(names(.)))


Table9 <- eco_countries %>% gather(FACTOR,VALUE,-SPECIES,-COUNTRIES) %>% unite(SP_FACTOR,SPECIES,FACTOR) %>% spread(SP_FACTOR,VALUE) %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% set_names(gsub("Atlantic cod_|European hake_","",names(.)))

to.plot <- Table9[match(all_countries,Table9$COUNTRIES),]

to.plot[is.na(to.plot)] <- "-"


Ft<- format_table(to.plot)
write_doc(Ft,
          "Table 9. Ecological Factors per fishing country and species.",
          "Tables/Table9SI.docx")


write.csv(eco_countries,file="data/eco_country.csv",row.names = FALSE)

##### 3. SOCIOECONOMIC INDICATORS #####

soc_indicators <- bind_rows(fread("data/socioeconomic_indicators_cod.csv",check.names = TRUE),
                            fread("data/socioeconomic_indicators_hake.csv",check.names = TRUE)) %>% 
  arrange(COUNTRIES,STOCK)


##### 3.1 GEAR.DIVERSITY #####


Table10 <- soc_indicators %>% 
  select(SPECIES,SPgear) %>% 
  distinct %>% 
  mutate(SPgear_norm=normalize(SPgear), GEAR.DIVERSITY=SPgear_norm)

write_doc(format_table(Table10),
          "Table 10. Values and normalization of Gear Diversity.",
          "Tables/Table10SI.docx")

###### 3.2 FLEET.MOBILITY #####

Table11 <- soc_indicators %>% 
  select(COUNTRIES,ICESareas5,ICESareasEU) %>% 
  distinct %>% 
  mutate(ICESareas5_norm=normalize(ICESareas5),ICESareasEU_norm=normalize(ICESareasEU)) %>% 
  rowwise() %>%
  mutate(FLEET.MOBILITY=mean(c(ICESareas5_norm,ICESareasEU_norm),na.rm=TRUE)) %>% 
  ungroup()

to.plot <- Table11 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)


write_doc(Ft,
          "Table 11. Indicators and normalization of Fleet Mobility factor.",
          "Tables/Table11SI.docx")


##### 3.3 CATCH.DEP #####

Table12 <- soc_indicators %>% arrange(COUNTRIES,SPECIES,STOCK) %>% 
  select(SPECIES,COUNTRIES,STOCK,Stockdep.sp,Stockdep.total) %>%
  mutate(Stockdep.sp_norm=1-normalize(Stockdep.sp),Stockdep.total_norm=1-normalize(Stockdep.total)) %>%
  rowwise() %>%
  mutate(CATCH.DEP=mean(c(Stockdep.sp_norm,Stockdep.total_norm),na.rm=TRUE)) %>% 
  ungroup()



to.plot <- Table12 %>% select(-SPECIES) %>% mutate_if(is.numeric,funs(round(.,digits = 4))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

Ft<- format_table(to.plot)

Ft <- spanFlexTableRows(Ft,1,runs = to.plot[,1])

write_doc(Ft,
          "Table 12. Catch dependency of countries on stocks.",
          "Tables/Table12SI.docx")

Table13 <- Table12 %>% 
  group_by(SPECIES,COUNTRIES) %>% 
  summarise(CATCH.DEP=mean(CATCH.DEP,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(SPECIES=paste0("CATCH.DEP\n",SPECIES)) %>%
  spread(SPECIES,CATCH.DEP)

to.plot <- Table13 %>% mutate_if(is.numeric,funs(round(.,digits = 4)))

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

to.plot[is.na(to.plot)] <- "-"

Ft<- format_table(to.plot)

write_doc(format_table(to.plot),
          "Table 13. Catch dependency factors per country.",
          "Tables/Table13SI.docx")


###### 3.4 ADAPTIVE MANAGEMENT #####

Table14 <- soc_indicators %>% 
  select(COUNTRIES,Research,Management) %>%
  distinct %>%
  mutate(Research_norm=normalize(Research),Management_norm=normalize(Management)) %>%
  rowwise() %>%
  mutate(ADAPTIVE.MNG=mean(c(Research_norm,Management_norm),na.rm=TRUE)) %>% 
  ungroup()

to.plot <- Table14 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 14. Adaptive Management indicator values and normalization.",
          "Tables/Table14SI.docx")


##### 3.5 SOCIOECONOMIC FACTORS #####

Table15 <- reduce(list(Table11 %>% select(COUNTRIES,FLEET.MOBILITY),
                       Table13 %>% select(COUNTRIES,`CATCH.DEP
European hake`,`CATCH.DEP
Atlantic cod`),
                       Table14 %>% select(COUNTRIES,ADAPTIVE.MNG)),full_join,by="COUNTRIES")

t10 <- Table10 %>% select(SPECIES,GEAR.DIVERSITY) %>% mutate(SPECIES=paste0("GEAR.DIV\n",SPECIES)) %>%
  spread(SPECIES,GEAR.DIVERSITY)

Table15 <- bind_cols(Table15,t10[rep(1,nrow(Table15)),])

to.plot <- Table15 %>% mutate_at(vars(starts_with("CATCH")),funs(round(.,digits = 3))) %>%
  mutate_at(vars(starts_with("ADAP"),starts_with("FLEET")),funs(round(.,digits = 2)))%>% 
  select(COUNTRIES,starts_with("GEAR"),FLEET.MOBILITY,starts_with("CATCH"),ADAPTIVE.MNG) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 15. Socioeconomic Factors",
          "Tables/Table15SI.docx")


##### 4 INSTITUTIONAL INDICATORS #####


ins_indicators <- bind_rows(fread("data/institutional_indicators_cod.csv",check.names = TRUE),
                            fread("data/institutional_indicators_hake.csv",check.names = TRUE)) %>% 
  arrange(COUNTRIES,STOCK)

##### 4.1 CO.MANAGEMENT (I1) #####


Table16 <-ins_indicators %>% 
  select(COUNTRIES,Norganizations) %>%
  distinct %>%
  mutate(Norganizations_norm=normalize(Norganizations)) %>%
  rowwise() %>%
  mutate(CO.MANAGEMENT=Norganizations_norm) %>% 
  ungroup()

to.plot <- Table16 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 16. Values, normalization and Co-Management factor.",
          "Tables/Table16SI.docx")

##### 4.2 PROPERTY.RIGHTS (I2) #####

Table17 <- ins_indicators %>% 
  select(COUNTRIES,Swaps) %>%
  distinct %>%
  mutate(Swaps_norm=normalize(Swaps)) %>%
  rowwise() %>%
  mutate(PROPERTY.RIGHTS=Swaps_norm) %>% 
  ungroup()

to.plot <- Table17 %>% mutate_if(is.numeric,funs(round(.,digits = 2))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)

write_doc(Ft,
          "Table 17. Indicators and normalization of Property Rights.",
          "Tables/Table17SI.docx")


##### 4.3 CATCH QUOTAS (I3) #####

Table18 <- ins_indicators %>% 
  select(COUNTRIES,STOCK,TAC) %>% 
  spread(COUNTRIES,TAC,-STOCK)

to.plot <- Table18 %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[,match(c("STOCK",all_countries),names(to.plot))]

Ft<- format_table(to.plot)

Ft[,1] <- textProperties(font.size = 8,font.weight = "bold")

write_doc(Ft,
          "Table 18. TAC (million tons) per stock and country (2015)",
          "Tables/Table18SI.docx")


Table19 <- ins_indicators %>% 
  select(SPECIES,COUNTRIES,TAC) %>%
  group_by(SPECIES,COUNTRIES) %>% 
  summarise(TAC=mean(TAC,na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(SPECIES) %>%
  mutate(TAC_norm=normalize(TAC)) %>%
  ungroup() %>%
  mutate(QUOTAS=TAC_norm)


to.plot <- Table19 %>% select(SPECIES,COUNTRIES,QUOTAS) %>% 
  mutate_if(is.numeric,funs(round(.,digits = 3))) %>% 
  mutate(SPECIES=paste0("QUOTAS\n",species_sort_name(SPECIES))) %>% 
  spread(SPECIES,QUOTAS)


to.plot[is.na(to.plot)] <- 0.0

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)



write_doc(Ft,
          "Table 19. Factor Quota values per country.",
          "Tables/Table19SI.docx")

##### 4.4 DEVELOPMENT (I4) #####

Table20 <-ins_indicators %>% 
  select(COUNTRIES,HDI) %>%
  distinct %>%
  mutate(HDI_norm=normalize(HDI)) %>%
  rowwise() %>%
  mutate(DEVELOPMENT=HDI_norm) %>% 
  ungroup()

to.plot <- Table20 %>% mutate_if(is.numeric,funs(round(.,digits = 3))) %>% data.frame

to.plot[is.na(to.plot)] <- "-"

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

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

to.plot <- to.plot[match(all_countries,to.plot$COUNTRIES),]

Ft<- format_table(to.plot)



write_doc(Ft,
          "Table 21. List of Institutional factors.",
          "Tables/Table21SI.docx")

