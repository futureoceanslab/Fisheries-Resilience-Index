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

if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(gridExtra)
packageVersion("gridExtra")
# [1] ‘2.2.1’


if(!require(magrittr)){
  install.packages("magrittr",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(magrittr)
packageVersion("magrittr")
# [1] ‘1.5’

if(!require(data.table)){
  install.packages("data.table",dependencies = TRUE,repos='http://cran.us.r-project.org')
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



##### 2. READ DATA #####

# Read the data

SSBFCOD <- fread("data/SSBLIM_FLIM_COD.csv",check.names = TRUE)
SSBFHAKE <- fread("data/SSBLIM_FLIM_HAKE.csv",check.names = TRUE)

##### 3. PLOT #####

plot_fig6si_species <- function(data,ASSESSID_to_keep,filename,xlim=c(0,1),ylim=c(0,1),fig.width=14,fig.height=7) {
  
  graphs <- names(ASSESSID_to_keep) %>% lapply(function(ASSESSID_name){
    
    ASSESSID_to_keep <- ASSESSID_to_keep[ASSESSID_name]
    
    to.plot <- data %>% filter(ASSESSID.1 == ASSESSID_to_keep)  
    
    ggplot(data=to.plot, aes(x=SSB.SSBLIM, y=F.FLIM, color=TSYEAR)) +
      geom_point(size=5)+
      geom_vline(xintercept = 1)+
      geom_hline(yintercept = 1)+
      scale_colour_gradient(low="blue", high="green",name="YEAR") +
      xlab(paste("SSB/SSBlimit",ASSESSID_name)) +
      ylab("F/Flimit") +
      xlim(xlim) +
      ylim(ylim) +
      theme_classic()+
      ggtitle(ASSESSID_name)+
      theme(axis.text = element_text(size=14, color="black"),
            axis.title = element_text(size=16, color="black"),
            legend.text = element_text(size=12,color = "black"),
            legend.title = element_text(size=14,color="black"),
            plot.title = element_text(size=14,color="black",face="bold"),
            panel.grid.major = element_line(size=0.5,color="gray",linetype = "dotted"))
    
  })
  
  
  png(filename,width=fig.width,height=fig.height,units="in",res=300)
  
  do.call(grid.arrange,c(graphs))
  
  dev.off()
  
}


##### 3.1. PLOT HAKE (Fig 6SIa) #####


ASSESSID_to_keep_HAKE <- c(`Hake North`="WGHMM-HAKENRTN-1977-2007-JENNINGS", `Hake South`="WGHMM-HAKESOTH-1982-2007-JENNINGS")



plot_fig6si_species(data=SSBFHAKE,
                    ASSESSID_to_keep=ASSESSID_to_keep_HAKE,
                    filename="Figures/Fig 6 SIa.png",
                    xlim=c(0,3),
                    ylim=c(0.5,1.5),
                    fig.width = 13,
                    fig.height = 8)


##### 3.2. PLOT COD (Fig 6SIb) #####

ASSESSID_to_keep_COD <- c(CODBA2532="WGBFAS-CODBA2532-1964-2007-JENNINGS", 
                      CODFAPL="NWWG-CODFAPL-1959-2006-MINTO", 
                      CODIS="WGNSDS-CODIS-1968-2006-MINTO", 
                      CODKAT="WGBFAS-CODKAT-1970-2006-MINTO", 
                      CODNEAR="AFWG-CODNEAR-1943-2006-MINTO", 
                      CODNS="WGNSSK-CODNS-1962-2007-MINTO",
                      CODVIa="WGNSDS-CODVIa-1977-2006-MINTO" )



plot_fig6si_species(data=SSBFCOD,
                    ASSESSID_to_keep=ASSESSID_to_keep_COD,
                    filename="Figures/Fig 6 SIb.png",
                    xlim=c(0,8),
                    ylim=c(0.25,1.8),
                    fig.width = 14,
                    fig.height = 7)

