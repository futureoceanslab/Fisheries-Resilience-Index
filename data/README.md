# Input files

## ecological_indicators.csv

Columns:

- **SPECIES** (character): Species

- **STOCK** (character): Stock

- **area2006** (integer): Area indicator. See section "Area (E1)"" in 1.A in "SI 2. Indicators and Factors" for details.

- **area2100** (integer): Area indicator. See section "Area (E1)"" in 1.A in "SI 2. Indicators and Factors" for details.

- **T50** (numeric): Temperature indicator. Species temperature 50% range. Cheung et al. 2015?. See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details.

- **Trange** (numeric): Temperature indicator. Species temperature 75%-25% range. Cheung et al. 2015?. See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details.

- **OverMSY** (numeric): Overexploitation indicator. Costello et al., 2012. Neubauer et al.  See section "Overexploitation (E4)"" in 1.A in "SI 2. Indicators and Factors" for details.

- **Status** (numeric): Overexploitation indicator. See section "Overexploitation (E4)"" in 1.A in "SI 2. Indicators and Factors" for details.

- **Recovery** (numeric): Recovery indicator. Recovery time. YEARS UNDER THE SSBLIMIT.   See section "Recovery (E5)"" in 1.A in "SI 2. Indicators and Factors" for details.

- **B_SSBrecent** (numeric): Abundance indicator. Biomass recent. Trend SSG 1980-2010/SSB stock average. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation. 

- **B_SSBhistoric** (numeric): Abundance indicator. Biomass historic. Trend SSG 1950-2010/SSB stock average. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation.

- **B_Ftrend** (numeric): Abundance indicator. Mortality. Trend F 19??-20??/F stock average. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation. 

- **B_Rtrend** (numeric): Abundance indicator. Recruitment. Trend R 19??-20??/R stock average. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation. 

## socioeconomic_indicators.csv

Columns:

- **SPECIES** (character): Species

- **COUNTRIES** (character): ISO Alpha-2 Country code.

- **STOCK** (character): Stock

- **SPgear** (integer): Gear diversity indicator. Number of different gear types fisherman can use. ICES Stock Annex. See "GEAR.DIVERSITY (S1)" in 2.A in "SI 2. Indicators and Factors" for details

- **ICESareas5** (numeric): Fleet mobility indicator. Distance fleet can do in last 5 years average. ICES database. See "FLEET MOBILITY (S2)" in 2.A in "SI 2. Indicators and Factors" for details

- **ICESareasEU** (numeric): Fleet mobility indicator. Distances that fleet can do compare before and after EU. ICES database. See "FLEET MOBILITY (S2)" in 2.A in "SI 2. Indicators and Factors" for details

- **Stockdep.sp** (numeric): Catch dependency indicator. Total catches of species dependent of total catch of stock. ICES database, Eurostat, RAM's Legacy. See "CATCH DEPENDENCY (S3)" in 2.A in "SI 2. Indicators and Factors" for details

- **Stockdep.total** (numeric): Catch dependency indicator. Total landings dependent of stock catches. ICES database,
Eurostat, RAM's Legacy. See "CATCH DEPENDENCY (S3)" in 2.A in "SI 2. Indicators and Factors" for details

- **Research** (integer): Adaptive management indicator. Total subsidies in research Amount US$ '000 for specified year (2009). Fisheries Research investment. Fisheries research and development in http://www.seaaroundus.org/data/#/subsidy/192. See "ADAPTIVE MANAGEMENT (S4)" in 2.A in "SI 2. Indicators and Factors" for details

- **Management** (integer): Adaptive management indicator. Total subsidies in management. Fisheries management programs and services in http://www.seaaroundus.org/data/#/subsidy/193.  See "ADAPTIVE MANAGEMENT (S4)" in 2.A in "SI 2. Indicators and Factors" for details

## institutional_indicators.csv

Columns:

- **SPECIES** (character): Species

- **COUNTRIES** (character): ISO Alpha-2 Country code.

- **STOCK** (character): Stock

- **HDI** (numeric): Development indicator. Human Development Index. UNDP. See "INSTITUTIONAL STRENGTH (I4)" in 3.A in "SI 2. Indicators and Factors" for details

- **Compliance** (integer): Compliance scores by country 2010. Inclusion of requirements by country. See "INSTITUTIONAL STRENGTH (I4)" in 3.A in "SI 2. Indicators and Factors" for details

- **TAC** (integer): Catch quotas indicator. Totall allowable catch per country and stock. europa.eu. See "CATCH QUOTAS (I3)" in 3.A in "SI 2. Indicators and Factors" for details

- **Above_TAC** (integer): Catches above advised TAC. Country catches above TAC. SSMM Carpenter et al., 2016 See "CATCH QUOTAS (I3)" in 3.A in "SI 2. Indicators and Factors" for details

- **Swaps** (numeric): Property rights indicator. Monetary exchange of quota in a country. Total ITQ per country and stock 2000-2006. Andersen et al., 2009. See "PROPERTY.RIGHTS (I2)" in 3.A in "SI 2. Indicators and Factors" for details

- **Norganizations** (integer): Co-Management indicator. Number of recognized producer organizations in the fishery and aquaculture sector. europa.eu. See "CO.MANAGEMENT (I1)" in 3.A in "SI 2. Indicators and Factors" for details

## Other_index.csv

Columns: 

- **COUNTRIES** (character): ISO Alpha-2 Country code.

- **GDP.2016** (numeric): GDP 2016. International Monetary Bank

- **OHI.fisheries** (numeric): OHI 2016. OHI webpage wild caught fisheries

- **OHI.economic** (numeric): OHI eco 2016. Coastal Livelihoods and Economies

- **Readiness** (numeric): ND-Gain Index: Readiness. 2016

- **Vulnerability** (numeric): ND-Gain Index: Vulnerability 2016

# Output files

## ecological_factors.csv

Ecological factors per stock and country. Computed in process_indicators.R

Columns:

- **SPECIES** (character): Species

- **STOCK** (character): Stock

- **ABUNDANCE** (numeric): Abundance factor. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation.

- **TEMPERATURE** (numeric): Temperature factor. See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details on computation.

- **OVEREXPLOITATION** (numeric): Overexploitation factor. See section "Overexploitation (E4)"" in 1.A in "SI 2. Indicators and Factors" for details on computation.

- **RECOVERY** (numeric): Recovery factor. See section "Recovery (E5)"" in 1.A in "SI 2. Indicators and Factors" for details on computation.

## ecological_factors_country.csv

Ecological factors per stock are converted to factors per country based on whether the stock is being fished by the EU member country, based on the data from ICES Historical Nominal Catches 1950-2010 (http://www.ices.dk/marine-data/dataset- collections/Pages/Fish-catch-and-stock-assessment.aspx) See table 8 in "SI 2. Indicators and Factors".

The values of ecological factors in countries are calculated as the mean of the values of ecological factors in stocks that these countries fish. Computed in process_indicators.R


Columns:

- **SPECIES** (character): Species

- **ABUNDANCE** (numeric): Abundance factor. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation.

- **TEMPERATURE** (numeric): Temperature factor. See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details on computation.

- **OVEREXPLOITATION** (numeric): Overexploitation factor. See section "Overexploitation (E4)"" in 1.A in "SI 2. Indicators and Factors" for details on computation.

- **RECOVERY** (numeric): Recovery factor. See section "Recovery (E5)"" in 1.A in "SI 2. Indicators and Factors" for details on computation.


## socioeconomic_factors.csv

Socioeconomic factors per stock and country. Computed in process_indicators.R

Columns: 

- **SPECIES** (character): Species

- **COUNTRIES** (character): ISO Alpha-2 Country code.

- **STOCK** (character): Stock

- **ADAPTIVE.MNG** (numeric): Adaptive management factor. See "ADAPTIVE MANAGEMENT (S4)" in 2.A in "SI 2. Indicators and Factors" for details on computation.

- **CATCH.DEP** (numeric): Catch dependency factor. See "CATCH DEPENDENCY (S3)" in 2.A in "SI 2. Indicators and Factors" for details on computation.

- **FLEET.MOBILITY** (numeric): Fleet mobility factor. See "FLEET MOBILITY (S2)" in 2.A in "SI 2. Indicators and Factors" for details on computation.

- **GEAR.DIVERSITY** (numeric): Gear diversity factor. See "GEAR.DIVERSITY (S1)" in 2.A in "SI 2. Indicators and Factors" for details on computation.

## socioeconomic_factors_country.csv

Socioeconomic factors per country. Computed in process_indicators.R

Columns: 

- **SPECIES** (character): Species

- **COUNTRIES** (character): ISO Alpha-2 Country code.

- **ADAPTIVE.MNG** (numeric): Adaptive management factor. See "ADAPTIVE MANAGEMENT (S4)" in 2.A in "SI 2. Indicators and Factors" for details on computation.

- **CATCH.DEP** (numeric): Catch dependency factor. See "CATCH DEPENDENCY (S3)" in 2.A in "SI 2. Indicators and Factors" for details on computation.

- **FLEET.MOBILITY** (numeric): Fleet mobility factor. See "FLEET MOBILITY (S2)" in 2.A in "SI 2. Indicators and Factors" for details on computation.

- **GEAR.DIVERSITY** (numeric): Gear diversity factor. See "GEAR.DIVERSITY (S1)" in 2.A in "SI 2. Indicators and Factors" for details on computation.


## institutional_factors.csv

Institutional factors per stock and country. Computed in process_indicators.R

Columns: 

- **SPECIES** (character): Species

- **COUNTRIES** (character): ISO Alpha-2 Country code.

- **STOCK** (character): Stock

- **DEVELOPMENT** (numeric): Development factor. See "DEVELOPMENT (I4)" in 3.A in "SI 2. Indicators and Factors" for details on computation.

- **QUOTAS** (numeric): Catch quotas factor. See "CATCH QUOTAS (I3)" in 3.A in "SI 2. Indicators and Factors" for details on computation.

- **PROPERTY.RIGHTS** (numeric): Property rights factor. See "PROPERTY.RIGHTS (I2)" in 3.A in "SI 2. Indicators and Factors" for details on computation.

- **CO.MANAGEMENT** (numeric): Co-management factor. See "CO.MANAGEMENT (I1)" in 3.A in "SI 2. Indicators and Factors" for details on computation.

## institutional_factors_country.csv

Institutional factors per country. Computed in process_indicators.R

Columns: 

- **SPECIES** (character): Species

- **COUNTRIES** (character): ISO Alpha-2 Country code.

- **STOCK** (character): Stock

- **INSTITUTIONAL STRENGHT** (numeric): Development factor. See "INSTITUTIONAL STRENGHT (I4)" in 3.A in "SI 2. Indicators and Factors" for details on computation.

- **QUOTAS** (numeric): Catch quotas factor. See "CATCH QUOTAS (I3)" in 3.A in "SI 2. Indicators and Factors" for details on computation.

- **PROPERTY.RIGHTS** (numeric): Property rights factor. See "PROPERTY.RIGHTS (I2)" in 3.A in "SI 2. Indicators and Factors" for details on computation.

- **CO.MANAGEMENT** (numeric): Co-management factor. See "CO.MANAGEMENT (I1)" in 3.A in "SI 2. Indicators and Factors" for details on computation.




## final_index.csv

Resilience index and other indexes. Computed in indexes.R

Columns:

- **SPECIES** (character): Species

- **COUNTRIES** (character): ISO Alpha-2 Country code.

- **DIMENSION** (character): ecological, socioeconomic or institutional.

- **GDP.2016** (numeric): GDP 2016. International Monetary Bank

- **OHI.fisheries** (numeric): OHI 2016. OHI webpage wild caught fisheries

- **OHI.economic** (numeric): OHI eco 2016. Coastal Livelihoods and Economies

- **Readiness** (numeric): ND-Gain Index: Readiness. 2016

- **Vulnerability** (numeric): ND-Gain Index: Vulnerability 2016

- **Resilience_Index** (numeric): Resilience index computed in the script indexes.R

