#***********
# Contact: Zarrar Khan (zarrar.khan@pnnl.gov)
# Project: IDB
# File: rgcam_Figures.R
# *********

#____________________________________________
#____________________________________________
# Contents
#____________________________________________
#____________________________________________

# To be written


#______________________
# Initial setup
#______________________

rm(list=ls()) # Clear all old variables
gc()
graphics.off()

wdsp<<-"D:/00SpatialData"  # Spatial Data folder Check in rgcam_FiguresMASTER.R to see if all spatial files are in this folder
wdXanthosRuns<<-"D:/00XanthosRuns/pm_abcd_mrtm"  # Xanthos runoff runs
wdScripts<<-"D:/rJGCRI_ChartsMaps"
source(paste(wdScripts,"/rJGCRI_libs.R",sep=""))

#______________________
# Select parameters and regions
#______________________

reReadData     <<- 0  #Read gcamdata queryData.proj again?
reReadMaps     <<- 0  #Read gcamdata queryData.proj again?
deleteOldFigs  <<- 0  # To completely delete old figures select 1, else it will only overwrite
animationsOn   <<- 1 # 1 Yes, 0 No

meanYearOnly   <<- 1
selectFigsOnly <<- 0  # If you want all figures to be produced then choose 0
bySubBasin     <<- 0
runDiffPlots   <<- 0
runGCAMCharts  <<- 0
runTethysMaps  <<- 1
runDemeterMaps <<- 1
runXanthosMaps <<- 1
runScarcity    <<- 1
runScarcityImpacts <<- 1


regionAnalysis<<-c("India")
scenariosAnalysis<<-c("IMFRecessionLampRef")
gcamDatabaseNameAnalysis<<-"database_basexdb"

#Get the Scenarios in the Data Base and Set names etc.
myDB<<-gcamDatabaseNameAnalysis        # Name of database
connx<<- localDBConn(paste(dirname(getwd()),"/gcam/output",sep=""),myDB) # Connect to database

scenNameRefOrig<<-"IMFRecessionLampRef";scenNameRef<-"Ref"
scenName1Orig<<-"Reference_IMFRecession_LAMPOn";scenName1<-"GCAM_Orig"
scenName2Orig<<-"LocalData";scenName2<-"Local Data"
scenName3Orig<<-"x";scenName3<-"x"
scenariosAnalysis<<-c(scenNameRefOrig,scenName1Orig,scenName2Orig)
scenariosIndvAnalysis<<-c(scenName1Orig)

paramsDiff<<-c("pop","gdp","gdpGrowthRate","aggLandAlloc","co2emissionByEndUse","ghgEmissByGHGGROUPS",
               "elecByTech","agProdByCropNoPasture","watConsumBySec","watWithdrawBySec",
               "agProdbyIrrRfd","landIrrRfd","watBioPhysCons")

#______________________
# Source Scripts
#______________________
source(paste(wdScripts,"/color_schemes.R",sep=""))  # From the GCAm R diagnostics package
source(paste(wdScripts,"/rJGCRI_ChartsMaps.R",sep=""))
source(paste(wdScripts,"/rJGCRI_gis.R",sep=""))

#______________________
# Main Script
#______________________

source(paste(wdScripts,"/rJGCRI_IDBFiguresProcess.R",sep=""))

