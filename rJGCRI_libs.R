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
             "rgeos","raster","tmap","animation","tis","tibble","here","classInt","tidyr")
for(i in packagesX){if(i %in% rownames(installed.packages()) == FALSE) {install.packages(i)};library(i,character.only = TRUE)}

#Additional Libraries
if("rgcam" %in% rownames(installed.packages()) == FALSE) {devtools::install_github('JGCRI/rgcam')};library(rgcam)
# Download Pakcage ncdf4 zip file from http://cirrus.ucsd.edu/~pierce/ncdf/
if(!file.exists("D:/software/ncdf4_1.12.zip")){print("ncdf zip package not available. Donwload and keep in folder.")}
if("ncdf4" %in% rownames(installed.packages()) == FALSE){install.packages("D:/software/ncdf4_1.12.zip", repos = NULL)};library(ncdf4)
