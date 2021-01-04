#' This script reads data/final_index.csv and plots
#' Figure GLM (3) in the paper.
#' 
#' NOTE: As of 2018-03-21, CRAN version of ggplot2 does not include geom_sf.
#' If you run the script and get an error that says that geom_sf is not found, then you
#' need the dev version in github. See bellow to see how to install the github version.


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

if(!require(sf)){
  install.packages("sf",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(sf)
packageVersion("sf")
# [1] ‘0.6.0’

# NOTE: As of 2018-03-21, CRAN version of ggplot2 does not include geom_sf.
# If you run the script and get an error that says that geom_sf is not found. Then uncomment and run
# the next lines

# if(!require(devtools)){
#   install.packages("devtools",dependencies = TRUE,repos='http://cran.us.r-project.org')
# }
# require(devtools)
# install_github("tidyverse/ggplot2") 



if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’


source("aux_functions.R")


library(mgcv)
#library(gam)

## To create Latex tables with GAM summary 
install.packages("itsadug")
library(itsadug)

##### 2. PREPARE lAT #####


download.file(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip",
              destfile = "data/world.zip", mode = "wb")

unzip("data/world.zip", exdir = "data")

world_area <- sf::st_read("data/TM_WORLD_BORDERS_SIMPL-0.3.dbf", quiet = TRUE) ## sf:: es para que sepa que la funcion es del package sf, sino sale masked como error.
class(world_area)        

file.remove(c("data/Readme.txt","data/world.zip",list.files("data",pattern = "TM_WORLD.*",full.names = TRUE)))

a<-filter(world_area, grepl('EG|AL|AD|AT|BY|BE|BA|BG|HR|CY|CZ|DK|EE|FO|FI|FR|DE|GI|GR|HU|IS|IE|IM|IT|RS|LV|LI|LT|LU|MK|MT|MD|MC|ME|NL|NO|PL|PT|RO|SM|RS|SK|SI|ES|SE|CH|UA|GB|VA|RS|MA|TN|DZ|LY|EH', ISO2)) %>% #get the dat with your 3 cuntries per caracter variable with a name
  mutate(value = AREA*SUBREGION) %>% rename(COUNTRIES=ISO2)  ## add a column called "value" multipliying area*region

##### 3. READ DATA #####

final_index <- read_csv("data/final_index.csv")

joined <- merge(a, final_index, by="COUNTRIES", all.x=T)

##### GAM MODELS #####

## Create factor variables 
joined$sp=as.factor(joined$SPECIE)
joined$dim=as.factor(joined$DIMENSION)
Data=split(joined,f=joined$dim)

##################	 	ECOLOGICAL DIMENSION 			#################
g0e=gam(Resilience_Index~sp+s(LAT,bs="ts"),family = "quasibinomial", data=Data[[1]])
summary(g0e) ## p.value for smooth term >0.1 -> non significant effect of latitude 
gamtabs(g0e) ## Latex Table

###########################################################

Parametric coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.67882    0.09297   7.302 9.09e-07 ***
  spHake      -0.39215    0.15174  -2.584   0.0188 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
  edf Ref.df     F p-value
s(LAT) 2.104      9 0.543   0.111

R-sq.(adj) =  0.363   Deviance explained = 45.8%
GCV = 0.03208  Scale est. = 0.026477  n = 22
#########################################################

g1e=gam(Resilience_Index~sp+s(LAT,bs="ts", by=sp),family = "quasibinomial", data=Data[[1]])
summary(g1e) ## Significant effect of latitude for cod.
gambtab(g1e)

##########################################################
Parametric coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.67534    0.08109   8.328 2.26e-07 ***
  spHake      -0.42683    0.12878  -3.314  0.00414 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
  edf Ref.df     F p-value  
s(LAT):spCod  3.163e+00      9 1.425  0.0174 *
  s(LAT):spHake 1.469e-05      6 0.000  0.5148  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.523   Deviance explained = 61.8%
GCV = 0.025531  Scale est. = 0.01971   n = 22	

########################################################################	

### marginal model for cod 

anova(ge_cod) # p-value = 0.113, non-significant effect fo latitude.



##################		INSTITUTIONAL DIMENSION 			#################	

g0i=gam(Resilience_Index~sp+s(LAT,bs="ts"),family = "quasibinomial", data=Data[[2]])
summary(g0i) 
gamtabs(g0i)


g1i=gam(Resilience_Index~sp+s(LAT,bs="ts", by=sp),family = "quasibinomial", data=Data[[2]])
summary(g1i)
gamtabs(g0i) # Different effect of latitude for cod and hake.

###################################################################################	
Parametric coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.67534    0.08109   8.328 2.26e-07 ***
  spHake      -0.42683    0.12878  -3.314  0.00414 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
  edf Ref.df     F p-value  
s(LAT):spCod  3.163e+00      9 1.425  0.0174 *
  s(LAT):spHake 1.469e-05      6 0.000  0.5148  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.523   Deviance explained = 61.8%
GCV = 0.025531  Scale est. = 0.01971   n = 22

###################################################################################		

## Fit marginal models to plot	
gi_cod=gam(Resilience_Index~s(LAT, bs="ts"), family="quasibinomial", data=SIdata[[1]])
gi_hake=gam(Resilience_Index~s(LAT, bs="ts"), family="quasibinomial", data=SIdata[[2]])

## create predictions 	
data=SIdata[[1]]
rlat=range(data$LAT)
ncod=data.frame(LAT=seq(rlat[1], rlat[2], length=100))
p=predict(gi_cod,newdata=ncod, type="response",se=TRUE)
pred=p$fit
se=p$se.fit
up=pred+1.96*se
low=pred-1.96*se

data=SIdata[[2]]
rlat=range(data$LAT)
nhake=data.frame(LAT=seq(rlat[1], rlat[2], length=100))
p=predict(gi_hake,newdata=nhake, type="response",se=TRUE)
pred2=p$fit
se=p$se.fit
up2=pred2+1.96*se
low2=pred2-1.96*se


png(filename = "Figures/INSTITUTIONALgam.png",
    width = 20, height =8, units = "cm", pointsize = 8,
    bg = "white", res = 450, family = "", restoreConsole = TRUE)

par(mfrow=c(1,2))
par(mar=c(4,4.5, 4, 2))


plot(ncod$LAT, pred,ylim=c(0,1), type="l",col=1, lty=2,xlab="LAT", ylab="Institutional R.I", main="COD")
x=ncod$LAT
polygon(x=c(x,rev(x)),y=c(up,rev(low)), col="grey75")
lines(x,pred, col=1, lwd=2)
abline(h=mean(data$Resilience_Index), col=2, lty=2)

x=nhake$LAT

plot(nhake$LAT, pred2,ylim=c(0,1), type="l",col=1, lty=2,xlab="LAT", ylab="Institutional R.I", main="HAKE")
x=nhake$LAT
polygon(x=c(x,rev(x)),y=c(up2,rev(low2)), col="grey75")
lines(x,pred2, col=1, lwd=2)
abline(h=mean(data$Resilience_Index), col=2, lty=2)

dev.off()	


##################			SOCIOECONOMIC DIMENSION 			#################	

g0s=gam(Resilience_Index~sp+s(LAT,bs="ts"),family = "quasibinomial", data=Data[[3]])
summary(g0s) 
gamtabs(g0s)
anova(g0s)# p-value =  0.183

g1s=gam(Resilience_Index~sp+s(LAT,bs="ts", by=sp),family = "quasibinomial", data=Data[[3]])
summary(g1s) # Non-sginficant effect of LAT.
gamtabs(g1s)

##################################################################################


Parametric coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept)   0.4577     0.1224   3.739  0.00204 **
  spHake       -1.1338     0.9271  -1.223  0.24061   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
  edf Ref.df     F p-value
s(LAT):spCod  6.272e-06      9 0.000   0.879
s(LAT):spHake 5.294e+00      7 1.376   0.166

R-sq.(adj) =  0.565   Deviance explained = 69.1%
GCV = 0.075979  Scale est. = 0.049783  n = 22

##################################################################################


################## 	BOXPLOTS ##################
## Can be useful to supprt the comparison between species made in the first paragraph of Results.




png(filename = "Figures/Boxplots_RI.png",
    width = 20, height =8, units = "cm", pointsize = 8,
    bg = "white", res = 450, family = "", restoreConsole = TRUE)


par(mfrow=c(1,4))
ylim=c(0,1)
ldim=levels(joined$dim)
boxplot(Resilience_Index~sp, ylim=ylim, ylab="Resilience Index", main="Overal", data=Fdata)
for(i in 1:3)
{
  boxplot(Resilience_Index~sp, ylim=ylim, ylab="Resilience Index", main=ldim[i], data=Data[[i]])
}

dev.off()
