## ecological_indicators_cod.csv and ecological_indicators_hake.csv

Columns:

- SPECIES (character): Species
- STOCK (character): Stock
- area2006 (integer): Area indicator. See section "Area (E1)"" in 1.A in "SI 2. Indicators and Factors" for details.
- area2100 (integer): Area indicator. See section "Area (E1)"" in 1.A in "SI 2. Indicators and Factors" for details.
- SSBrecent (numeric): Abundance indicator. Biomass recent. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation. 
- SSBhistoric (numeric): Abundance indicator. Biomass historic. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation.
- Ftrend (numeric): Abundance indicator. Mortality. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation. 
- Rtrend (numeric): Abundance indicator. Recruitment. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation. 
- T50 (numeric): Temperature indicator. Mean temperature. See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details.
- Trange (numeric): Temperature indicator. Temperature range. See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details.
- OverMSY (numeric): Overexploitation indicator. See section "Overexploitation (E4)"" in 1.A in "SI 2. Indicators and Factors" for details.
- Status (numeric): Overexploitation indicator. See section "Overexploitation (E4)"" in 1.A in "SI 2. Indicators and Factors" for details.
- Recovery (numeric): Recovery indicator. Recovery time.  See section "Recovery (E5)"" in 1.A in "SI 2. Indicators and Factors" for details.


## ecological_factors_cod.csv and ecological_factors_hake.csv

Columns:

- SPECIES (character): Species
- STOCK (character): Stock
- ABUNDANCE (numeric): Abundance factor. See "Abundance (E2)" in 1.A in "SI 2. Indicators and Factors" for details on computation.
- TEMPERATURE (numeric): Temperature factor. See section "Temperature (E3)" in 1.A in "SI 2. Indicators and Factors" for details on computation.
- OVEREXPLOITATION (numeric): Overexploitation factor. See section "Overexploitation (E4)"" in 1.A in "SI 2. Indicators and Factors" for details on computation.
- RECOVERY (numeric): Recovery factor. See section "Recovery (E5)"" in 1.A in "SI 2. Indicators and Factors" for details on computation.

## eco_country_cod.csv and eco_country_hake.csv

Ecological factors per stock are converted to factors per country based on whether the stock is being fished by the EU member country, based on the data from ICES Historical Nominal Catches 1950-2010 (http://www.ices.dk/marine-data/dataset- collections/Pages/Fish-catch-and-stock-assessment.aspx) See table 8 in "SI 2. Indicators and Factors".

The values of ecological factors in countries are calculated as the mean of the values of ecological factors in stocks that these countries fish. Therefore, the values in these files are computed from ecological_factors_cod.csv and ecological_factors_hake.csv.

Columns:

- COUNTRY (character): ISO Alpha-2 Country code.
- ABUNDANCE (numeric): Abundance factor.
- TEMPERATURE (numeric): Temperature factor. computation.
- OVEREXPLOITATION (numeric): Overexploitation factor. details on computation.
- RECOVERY (numeric): Recovery factor.


## socioeconomic_indicators_cod.csv and socioeconomic_indicators_hake.csv

Columns:

- SPECIES (character): Species
- COUNTRIES (character): ISO Alpha-2 Country code.
- STOCK (character): Stock
- SPgear (integer): Gear diversity indicator.See "GEAR.DIVERSITY (S1)" in 2.A in "SI 2. Indicators and Factors" for details
- ICESareas5 (numeric): Fleet mobility indicator.See "FLEET MOBILITY (S2)" in 2.A in "SI 2. Indicators and Factors" for details
- ICESareasEU (numeric): Fleet mobility indicator. See "FLEET MOBILITY (S2)" in 2.A in "SI 2. Indicators and Factors" for details
- Stockdep.sp (numeric): Catch dependency indicator. See "CATCH DEPENDENCY (S3)" in 2.A in "SI 2. Indicators and Factors" for details
- Stockdep.total (numeric): Catch dependency indicator. See "CATCH DEPENDENCY (S3)" in 2.A in "SI 2. Indicators and Factors" for details
- Research (integer): Adaptive management indicator. Fisheries Research investment. See "ADAPTIVE MANAGEMENT (S4)" in 2.A in "SI 2. Indicators and Factors" for details
- Management (integer): Adaptive management indicator. Fisheries Management investment. See "ADAPTIVE MANAGEMENT (S4)" in 2.A in "SI 2. Indicators and Factors" for details

## socioeconomic_factors_cod.csv and socioeconomic_factors_hake.csv

Columns: 

- SPECIES (character): Species
- COUNTRIES (character): ISO Alpha-2 Country code.
- STOCK (character): Stock
- ADAPTIVE.MNG (numeric): Adaptive management factor. See "ADAPTIVE MANAGEMENT (S4)" in 2.A in "SI 2. Indicators and Factors" for details on computation.
- CATCH.DEP (numeric): Catch dependency factor. See "CATCH DEPENDENCY (S3)" in 2.A in "SI 2. Indicators and Factors" for details on computation.
- FLEET.MOBILITY (numeric): Fleet mobility factor. See "FLEET MOBILITY (S2)" in 2.A in "SI 2. Indicators and Factors" for details on computation.
- GEAR.DIVERSITY (numeric): Gear diversity factor. See "GEAR.DIVERSITY (S1)" in 2.A in "SI 2. Indicators and Factors" for details on computation.

## institutional_indicators_cod.csv and institutional_indicators_hake.csv

Columns:

- SPECIES (character): Species
- COUNTRIES (character): ISO Alpha-2 Country code.
- STOCK (character): Stock
- HDI (numeric): Development indicator. Human Development Index. See "DEVELOPMENT (I4)" in 3.A in "SI 2. Indicators and Factors" for details
- TAC (integer): Catch quotas indicator. Totall allowable catch. See "CATCH QUOTAS (I3)" in 3.A in "SI 2. Indicators and Factors" for details
- Swaps (numeric): Property rights indicator. Monetary exchange of quota in a country. See "PROPERTY.RIGHTS (I2)" in 3.A in "SI 2. Indicators and Factors" for details
- Norganizations (integer): Co-Management indicator. See "CO.MANAGEMENT (I1)" in 3.A in "SI 2. Indicators and Factors" for details

## institutional_factors_cod.csv and institutional_factors_hake.csv

Columns: 

- SPECIES (character): Species
- COUNTRIES (character): ISO Alpha-2 Country code.
- STOCK (character): Stock
- DEVELOPMENT (numeric): Development factor. See "DEVELOPMENT (I4)" in 3.A in "SI 2. Indicators and Factors" for details on computation.
- QUOTAS (numeric): Catch quotas factor. See "CATCH QUOTAS (I3)" in 3.A in "SI 2. Indicators and Factors" for details on computation.
- PROPERTY.RIGHTS (numeric): Property rights factor. See "PROPERTY.RIGHTS (I2)" in 3.A in "SI 2. Indicators and Factors" for details on computation.
- CO.MANAGEMENT (numeric): Co-management factor. See "CO.MANAGEMENT (I1)" in 3.A in "SI 2. Indicators and Factors" for details on computation.

## Other_index.csv

Columns: 

- COUNTRIES (character): ISO Alpha-2 Country code.
- GDP.2016 (numeric): GDP 2016. International Monetary Bank
- OHI.wild.caught (numeric): OHI 2016. OHI webpage wild caught fisheries
- OHI.eco (numeric): OHI eco 2016. Coastal Livelihoods and Economies
- Tech..develop..2013 (numeric): Tech Develop 2013. number of inventions OECD.stat
- Inclusion.of.Requirements.2010 (numeric): Inclusion of Requirements 2010. Member state for inclusion of required elements in annual reports. 
- Readiness (numeric): ND-Gain Index: Readiness. 2016
- Vulnerability (numeric): ND-Gain Index: Vulnerability 2016

## final_index.csv

- SPECIES (character): Species
- COUNTRIES (character): ISO Alpha-2 Country code.
- DIMENSION (character): ecological, socioeconomic or institutional.
- GDP.2016 (numeric): GDP 2016. International Monetary Bank
- OHI.2016 (numeric): OHI 2016. OHI webpage wild caught fisheries
- OHI.eco (numeric): OHI eco 2016. Coastal Livelihoods and Economies
- Tech..develop..2013 (numeric): Tech Develop 2013. number of inventions OECD.stat
- Inclusion.of.Requirements.2010 (numeric): Inclusion of Requirements 2010. Member state for inclusion of required elements in annual reports. 
- Readiness (numeric): ND-Gain Index: Readiness. 2016
- Vulnerability (numeric): ND-Gain Index: Vulnerability 2016
- Resilience_Index (numeric): Resilience index computed in the script indexes.R

