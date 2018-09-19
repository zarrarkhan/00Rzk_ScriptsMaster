#***********
# Contact: Zarrar Khan (zarrar.khan@pnnl.gov)
# Project: General
# *********

#______________________
# A.2 Libraries
#______________________

# If you don't have dev tools
#install.packages("devtools")
# Rtools https://cran.r-project.org/bin/windows/Rtools/
# devtools::install_github("hadley/devtools"))

# Install libraries

if("spDataLarge" %in% rownames(installed.packages()) == FALSE){install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')}

packagesX<-c("ggplot2","RColorBrewer","reshape2","magrittr","plyr","dplyr","tools","scales","rgdal",
             "rgeos","raster","tmap","animation","tis","tibble","here","classInt","tidyr",
             "assertthat","readr","data.table","devtools","ncdf4","zoo")
for(i in packagesX){if(i %in% rownames(installed.packages()) == FALSE) {install.packages(i)};library(i,character.only = TRUE)}

#Additional Libraries
if("rgcam" %in% rownames(installed.packages()) == FALSE) {devtools::install_github('JGCRI/rgcam')};library(rgcam)


#--------------------
# Conversions
#--------------------

# Emissions Convert to CO2eq
# GWP conversions - uses 100-yr GWPs from IPPC AR4 and AR5
# https://www.ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf
# Does not include all covnersions. Add them if they are tracked in GCAM

GWP<- tribble(
  ~ghg, ~GWPSAR, ~GWPAR4,~GWPAR5,
  "CO2",44/12,44/12,44/12,
  "CH4",21,25,28,
  "N20",310,298,265,
  "C2F6",9.2,12.2,11.1,
  "CF4",6.5,7.39,6.63,
  "HFC125",2.8,3.5,3.17,
  "HFC134a",1.3,1.43,1.3,
  "HFC245fa",1.03,1.03,0.858,
  "HFC143a",3.8,4.47,4.8,
  "HFC152a",0.140,0.124,0.138,
  "HFC227ea",2.9,3.22,3.35,
  "HFC23",11.7,14.8,12.4,
  "HFC236fa",6.3,9.81,8.06,
  "HFC32",0.650,0.675,0.677,
  "HFC365mfc",0.794,0.794,0.804,
  "SF6",23.9,22.8,23.5
)%>%as.data.frame;head(GWP)

convertGgTgMTC<- tribble(
  ~Units,~Convert,
  "Gg",0.001,
  "Tg",1,
  "MTC",1
)%>%as.data.frame;head(convertGgTgMTC)

GWPType<-"GWPAR4" #GWPSAR,GWPAR4,GWPAR5

convEJ2TWh<<-277.77777777778
convEJ2GW<<-convEJ2TWh*1000/8760
conv1975USDperGJ22017USDperMWh<<-3.62/0.2777778    # Deflators 1975 to 2017 from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
conv1975USDperGJ22017USDperMBTU<<-3.62/0.947       # Deflators 1975 to 2017 from World Bank https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US&view=chart
