#Get data

setwd("C:/Users/FOL/OneDrive/CLOCK_STUDENTS/Elena Fontan/TFM/DATA FLEET/R scrip")

getDATRAS <- function(record, survey, startyear, endyear, quarters,
                      parallel = FALSE, cores = NULL, keepTime = FALSE) {
  # library(XML)
  # library(doParallel)
  # library(parallel)
  # library(foreach)
  # library(data.table)
  if(keepTime == TRUE) strt <- Sys.time()
  #
  seqYear <- startyear:endyear
  #
  if(!record %in% c("HL", "HH", "CA")) stop("Please specify record type:
                                            HH (haul meta-data),
                                            HL (Species length-based data),
                                            CA (species age-based data)")
  getURL <- apply(expand.grid(record, survey, seqYear, quarters),
                  1,
                  function(x) paste0("http://datras.ices.dk/WebServices/",
                                     "DATRASWebService.asmx/get", x[1],
                                     "data?survey=", x[2],
                                     "&year=", x[3],
                                     "&quarter=", x[4]))
  #
  if(parallel == TRUE) {
    if(missing("cores")) stop("Please specify the number of cores.")
    #
    unregister <- function() {
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
    } # close unregister
    #
    cl <- makeCluster(cores)
    registerDoParallel(cores = cl)
    
#
    getDATA <- foreach(temp = getURL,
                       .combine = function(...) rbindlist(list(...), fill = TRUE),
                       .multicombine = T,
                       .inorder = F,
                       .maxcombine = 1000,
                       .packages = c("XML", "data.table")) %dopar% {
                         data.table(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T,
                                                                     options = HUGE,
                                                                     useInternalNodes = T)),
                                                function(x) xmlSApply(x, xmlValue))))
                       } # close foreach %dopar%
    
    stopCluster(cl)
    unregister()
  } # close parallel == TRUE
  #
  if(parallel == FALSE) {
    getDATA <- foreach(temp = getURL,
                       .combine = function(...) rbindlist(list(...), fill = TRUE),
                       .multicombine=T,
                       .inorder=F,
                       .maxcombine=1000,
                       .packages = c("XML", "data.table")) %do% {
                         data.table(t(xmlSApply(xmlRoot(xmlTreeParse(temp, isURL = T,
                                                                     options = HUGE,
                                                                     useInternalNodes = T)),
                                                function(x) xmlSApply(x, xmlValue))))
                       } # close foreach %do%
  } # close parallel == FALSE
  if(keepTime == TRUE) print(Sys.time()-strt)
  return(getDATA)
} # close function

library (XML, quietly=TRUE)
library(doParallel, quietly=TRUE)
library(foreach, quietly=TRUE)
library(data.table, quietly=TRUE)

#

ageData<- getDATRAS(record="CA",
                    survey="EVHOE",
                    startyear=1999,
                    endyear=1999,
                    quarters=c(1:4),
                    parallel=TRUE,
                    cores=4)
dim(ageData)
