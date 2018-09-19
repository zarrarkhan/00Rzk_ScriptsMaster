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

#rm(list=ls()) # Not used because this file is now sourced
#graphics.off()
memory.limit(size=1E+10)


#______________________
# Sources
#______________________

# Hydro Basins layers   - ~\cite{lehner2013_HYDROBASINS} , B. Lehner and G. Grill, Global river hydrography and network routing: Baseline data and new approaches to study the world’s large river systems, Hydrological Processes, vol. 27, no. 15, pp. 2171-2186, 2013. Data is available online: https://www.hydrosheds.org
# GADM Layer - GADM data, Robert Hijmans Environmental Science and Policy, Geography Graduate Group UC Davis, Hijmans Lab
# NaturalEarth - State Boundaires and Provinces for a simpler smaller file http://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-1-states-provinces/
# PNNL Basins/Regions - Chris Vernon Data - Chris.vernon@pnnl.gov From: /pic/projects/GCAM/gcam_hydrology/boundaries


#______________________
# Select parameters and regions
#______________________

region_i<<-regionAnalysis
scenariosIndv<<-scenariosIndvAnalysis
scenariosComp<<-scenariosAnalysis
gcamDatabaseName<<-gcamDatabaseNameAnalysis

rangeX<<-seq(2005,2050,by=5)
yearsX<<-paste("X",rangeX,sep="")  # To put in format that is read in from tethys and Demeter
years_analysisXanthos<<-seq(1980,2005,by=5) # For Xanthos
yearsXanthos<<-paste("X",years_analysisXanthos,sep="")  # To put in format that is read in from tethys


delay<<-60 # Animation delay
colx<<-(brewer.pal(5,"YlOrRd")) #------Choose color palette

# Chart Details
titleOn<<-0 # 1 for yes, 0 for no
useNewLabels<<- 1 # 1 for new modified labels without letter prefix and capitalized
prettyBreaksyMaster<<-5
breakx_majMaster<<-10
breakx_minMaster<<-5

# PDF details
figWidth_InchMaster<<-13
figWidth_StateCharts_Multplier<<-1.5
figWidth_FreeScale_Multplier<<-1.2
figHeight_InchMaster<<-9
pdfpng='png'  #'pdf', 'png', or 'both'

# Map PDF details
mapWidthInch<<-10
mapHeightInch<<-8


#______________________
# New Functions
#______________________

# source function file
source(paste(wdScripts,"/rJGCRI_ChartsMaps.R",sep=""))
source(paste(wdScripts,"/rJGCRI_gis.R",sep=""))


#______________________
# Select Report figures
#______________________

selectFigsparams<<-c()
## NOTE: Region will be substituted for each region analyzed
selectFigsparams<<-c("map_basemaps_m5_Region_regionalMap")
selectFigsparams<<-gsub("Region",region_i,selectFigsparams);
selectedFigs<<-c()

#______________________
# Working Direcotries and Data
#______________________

wd0<<-getwd();wd0
wdfigsOut<<-paste(wd0,"/fig_outputs",sep="");wdfigsOut;
if(deleteOldFigs==1){if(dir.exists(wdfigsOut)){unlink(wdfigsOut,recursive=T)}}
if(!dir.exists(wdfigsOut)){dir.create(wdfigsOut)}  # Output Directory

# GCAM, Tethys, Demeter, Xanthos Data outputs
wddb<<-paste(dirname(wd0),"/gcam/output",sep="");wddb    # GCAM Database directories
wdtethys<<-paste(dirname(wd0),"/Modules/Water/Tethys/Output/",sep="") # Tethys data directory
wdDemeter<<-paste(dirname(wd0),"/Modules/Land/Demeter/outputs/",sep="") # Demeter data directory
wdXanthos<<-paste(dirname(wd0),"/Modules/Water/Xanthos/output/",sep="") # Xanthos data directory

# Supporting shapefiles directories (Admin/Basin)
wdspne10mAdmin0<<-paste(wdsp,"/naturalEarth/ne_10m_admin_0_countries_lakes",sep="");wdspne10mAdmin0
wdspPNNL_CV<<-paste(wdsp,"/boundaries_PNNLChrisVernon/shp",sep="");wdspPNNL_CV 
wdspGADM<<-paste(wdsp,"/gadm",sep="");wdspGADM
wdspSIAChydrozones<<-paste(wdsp,"/Zonificacion_hidrografica_2013",sep="");wdspSIAChydrozones
wdspHydroBasins<<-paste(wdsp,"/HydroBASINS/hydrobasins_processed",sep="");wdspHydroBasins

#______________________
# Connecting to GCAM https://github.com/JGCRI/rgcam
#______________________

myDB<<-gcamDatabaseName          # Name of database
connx<<- localDBConn(wddb, myDB)    # Connect to database

# Add scenarios to the project
if(reReadData==1){
if(file.exists("queryData.proj")){file.remove("queryData.proj")} # Delete old project file
for (scenario_i in scenariosComp){
queryData.proj<<-addScenario(conn=connx, proj="queryData.proj",scenario=scenario_ix,queryFile=paste(wdScripts,'/analysis_queriesIDB.xml',sep=""))  # Check your queries file
}}else{
queryData.proj<<-loadProject("queryData.proj")  # Use already saved database
}

lscen<<-listScenarios(queryData.proj);lscen
lquer<<-listQueries(queryData.proj);lquer
#getQuery(queryData.proj,"Primary Energy Consumption (Direct Equivalent")   # Check a Query


#______________________
# Read in Spatial Data
#______________________

if(1 %in% c(runTethysMaps,runDemeterMaps,runXanthosMaps,runScarcity)){

# Natural Earth Country Boundaries (Simplified Level 0 for quick plotting)
shp_wdspne10mAdmin0<<-readOGR(paste(wdspne10mAdmin0,sep=""),"ne_10m_admin_0_countries_lakes",use_iconv=T,encoding='UTF-8')

#GADM 
shp_gadm36L1<<-readOGR(paste(wdspGADM,sep=""),"gadm36_1",use_iconv=T,encoding='UTF-8')

# PNNL Regions and Basins
shp_PNNL32Reg<<-readOGR(paste(wdspPNNL_CV,sep=""),"region32_0p5deg",use_iconv=T,encoding='UTF-8')
shp_PNNL235CLM5ArcMin_multi<<-readOGR(paste(wdspPNNL_CV,sep=""),"Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')

LookupTable_PNNL32Region<<-data.frame(reg32_id=c(0:32),
                                     GCAM_region=c("0","USA","Africa_Eastern","Africa_Northern","Africa_Southern",
                                                   "Africa_Western","Australia_NZ","Brazil","Canada",
                                                   "Central America and Caribbean","Central Asia","China","EU-12",
                                                   "EU-15","Europe_Eastern","Europe_Non_EU","European Free Trade Association",
                                                   "India","Indonesia","Japan","Mexico",
                                                   "Middle East","Pakistan","Russia","South Africa",
                                                   "South America Northern","South America_Southern","South Asia","South Korea",
                                                   "Southeast Asia","Taiwan","Argentina","Colombia"))

shp_PNNL32Reg@data <-join(shp_PNNL32Reg@data,LookupTable_PNNL32Region,by=c("reg32_id")); head(shp_PNNL32Reg@data)

#SubBasins

# HydroBasins Levels SubBasins
shp_HydroBasinsLev3<<-readOGR(paste(wdspHydroBasins,sep=""),"hydrobasins_level_3",use_iconv=T,encoding='UTF-8')
shp_HydroBasinsLev4<<-readOGR(paste(wdspHydroBasins,sep=""),"hydrobasins_level_4",use_iconv=T,encoding='UTF-8')

# Colombia Sub-Basins
shp_SIACHydroZones<<-readOGR(paste(wdspSIAChydrozones,sep=""),"Zonificacion_hidrografica_2013",use_iconv=T,encoding='UTF-8')

#______________________
# Set projection layer
#______________________

projX<<-proj4string(shp_PNNL235CLM5ArcMin_multi) # Setting to HydroBASINS layer
shp_wdspne10mAdmin0<<-spTransform(shp_wdspne10mAdmin0, CRS(projX))
shp_PNNL32Reg<<-spTransform(shp_PNNL32Reg, CRS(projX))
shp_PNNL235CLM5ArcMin_multi<<-spTransform(shp_PNNL235CLM5ArcMin_multi, CRS(projX))
shp_SIACHydroZones<<-spTransform(shp_SIACHydroZones, CRS(projX))
shp_HydroBasinsLev3<<-spTransform(shp_HydroBasinsLev3, CRS(projX))
shp_HydroBasinsLev4<<-spTransform(shp_HydroBasinsLev4, CRS(projX))
shp_gadm36L1<<-spTransform(shp_gadm36L1, CRS(projX))

# https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
shp_HydroBasinsLev4 <- gBuffer(shp_HydroBasinsLev4, byid=TRUE, width=0)
shp_HydroBasinsLev4<<-spTransform(shp_HydroBasinsLev4, CRS(projX))
# Check any bad polys?
#sum(gIsValid(shp_HydroBasinsLev4, byid=TRUE)==FALSE)  


#--------------------------
# Main Mapping function
#--------------------------

maps_ReAggregate <<- function(region_i=region_i,scenario_i=scenario_ix,
                              moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                              moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                              moduleRemove=moduleRemove){

  if(FALSE){ # Setting variables for manual run
    region_i=region_i;scenario_i=scenario_ix;moduleName=moduleName;moduleParam=moduleParam;
    moduleUnits=moduleUnits;moduleTitleText=moduleTitleText;moduleAggType=moduleAggType;
    moduleRemove=moduleRemove;}

  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  # BY YEAR
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  
  yearsOriginal<<-years
  if(meanYearOnly==1){years<-years[length(years)]}
  
  for (year_i in years) {
    
   
    df1<-subset(df,select=c(lat,lon,get(year_i),Type))
    df1<-dcast(df1,lat+lon~Type,value.var=year_i,fun.aggregate=sum,na.rm=F);head(df1)
    
    # Convert to Spatial Point Data Frames
    df1 = SpatialPointsDataFrame(SpatialPoints(coords=(cbind(df1$lon,df1$lat))),data=df1)
    proj4string(df1)<-projX
    
    #____________________
    # GRIDDED PLOTS
    #____________________
    
    # Gridded
    # Crop to the Regional file shpa boundary
    df2<-raster::intersect(df1,b1);plot(df2)  # Crop to Layer shpa
    dfxtra<-raster::intersect(df1,b1);plot(dfxtra) #create larger boundary extents for plotting in tmap
    df3<-df2
    gridded(df3)<-TRUE  # Create Gridded Raster Data
    gridded(dfxtra)<-TRUE
    dfxtra<<-dfxtra
    
    dfCommonScaleYears<<-df3@data%>%dplyr::select(-c(lat,lon,moduleRemove))
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<<-1};
    head(dfCommonScaleYears) ;max(dfCommonScaleYears)
    
    #---------------------------------------------
    #----- For each year compare Demands by Sector
    #---------------------------------------------
    
    dfx<-df3
    dfx@data<-subset(dfx@data,select=c(unique(df$Type)))  # Choose the Sectors
    if(!is.null(moduleRemove)){dfx@data<-dfx@data%>%dplyr::select(-moduleRemove)}
    
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_raster(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        m8+if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i),sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_rasterKMeans(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        m8+ if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i),sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #---------------------------------------------
    #----- For each year compare Demands by Sector Free Scale
    #---------------------------------------------
    
    dfx<-df3
    dfx@data<-subset(dfx@data,select=c(unique(df$Type)))  # Choose the Sectors
    #dfx@data<-subset(dfx@data,select=-c(Total))  # Remove
    
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_rasterFreeScale(rasterBoundary=dfxtra,data=dfx) + m8+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i),"\nFree Scale",sep=""))}
      map 
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_rasterFreeScaleKMeans(rasterBoundary=dfxtra,data=dfx) + m8+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i),"\nFree Scale",sep=""))}
      map 
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #____________________
    # By Admin Region
    #____________________
    
    head(df3)
    dxpbyAdmin<<-spatAgg_gridded2shape(gridded=df3,shape=shpa1,byLev="NAME_1",boundBox=b1,moduleAggType=moduleAggType)
    head(dxpbyAdmin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    write.csv(dxpbyAdmin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorByAdminRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
    }
    
    dfCommonScaleYears<<-dxpbyAdmin%>%dplyr::select(-c(NAME_1,moduleRemove))
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<<-1}
    head(dfCommonScaleYears) ;max(dfCommonScaleYears)
    
    #---------------------------------------------
    #----- For each year_i compare Demands by Sector (By Admin Region)
    #---------------------------------------------
    
    shpa.x<-shpa1
    shpa.x@data<-join(shpa.x@data,dxpbyAdmin,by=c("NAME_1")) %>% 
      subset(select=c(unique(df$Type),"NAME_1")) %>%dplyr::select(-c(NAME_1));
    if(!is.null(moduleRemove)){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fill(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Provinces",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Provinces",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #---------------------------------------------
    #----- For each year_i compare Demands by Sector Free Scale
    #---------------------------------------------
    
    shpa.x<-shpa1
    shpa.x@data<-join(shpa.x@data,dxpbyAdmin,by=c("NAME_1")) %>% 
      subset(select=c(unique(df$Type),"NAME_1")) %>% subset(select=-c(NAME_1))
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fillFreeScale(data=dfx)+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Provinces Free Scale",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fillFreeScaleKMeans(data=dfx)+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Provinces Free Scale",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #____________________
    # By Basin Region
    #____________________
    
    # Basin Boundary
    head(df3)
    dxpbyBasin<<-spatAgg_gridded2shape(gridded=df3,shape=shpbasin1,byLev="basin_name",boundBox=b1,moduleAggType=moduleAggType)
    head(dxpbyBasin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbyBasin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorByBasinRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
    }
    
    dfCommonScaleYears<<-dxpbyBasin%>%dplyr::select(-c(basin_name,moduleRemove))
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<<-1}
    
    #---------------------------------------------
    #----- For each year_i compare Demands by Sector (By Basin Region)
    #---------------------------------------------
    
    shpa.x<-shpbasin1
    shpa.x@data<-join(shpa.x@data,dxpbyBasin,by=c("basin_name")) %>% 
      subset(select=c(unique(df$Type),"basin_name")) %>% subset(select=-c(basin_name));
    if(!is.null(moduleRemove)){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fill(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Basins",sep=""))}
      map
      print_PDFPNG(map,dir=dir,fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Basins",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #---------------------------------------------
    #----- For each year_i compare Demands by Sector by Basin Free Scale 
    #---------------------------------------------
    
    shpa.x<-shpbasin1
    shpa.x@data<-join(shpa.x@data,dxpbyBasin,by=c("basin_name")) %>% 
      subset(select=c(unique(df$Type),"basin_name")) %>% subset(select=-c(basin_name))
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{get("fname")})){
      map<-m7+mapX_fillFreeScale(data=dfx)+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Basin Free Scale",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fillFreeScaleKMeans(data=dfx)+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Basin Free Scale",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    
    if(bySubBasin==1){
      #____________________
      # By subBasin Region
      #____________________
      
      # subBasin Boundary
      head(df3)
      
      dxpbysubBasin<<-spatAgg_gridded2shape(gridded=df3,shape=shpsubBasin1,byLev="subBasin_name",boundBox=b1,moduleAggType=moduleAggType)
      head(dxpbysubBasin)
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbysubBasin,file=paste(dir,"/subBasin/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorBysubBasinRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
      }
      
      dfCommonScaleYears<<-dxpbysubBasin%>%dplyr::select(-c(subBasin_name,moduleRemove))
      if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<<-1}
      
      #---------------------------------------------
      #----- For each year_i compare Demands by Sector (By Admin Region)
      #---------------------------------------------
      
      shpa.x<-shpsubBasin1
      shpa.x@data<-join(shpa.x@data,dxpbysubBasin,by=c("subBasin_name")) %>% 
        subset(select=c(unique(df$Type),"subBasin_name")) %>% subset(select=-c(subBasin_name));
      if(!is.null(moduleRemove)){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
      dfx<-shpa.x
      
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-m7+mapX_fill(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
          if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," subBasins",sep=""))}
        map
        print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
      #KMEANS
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-m7+mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
          if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," subBasins",sep=""))}
        map
        print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
      #---------------------------------------------
      #----- For each year_i compare Demands by Sector by subBasin Free Scale 
      #---------------------------------------------
      
      shpa.x<-shpsubBasin1
      shpa.x@data<-join(shpa.x@data,dxpbysubBasin,by=c("subBasin_name")) %>% 
        subset(select=c(unique(df$Type),"subBasin_name")) %>% subset(select=-c(subBasin_name))
      dfx<-shpa.x
      
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-m7+mapX_fillFreeScale(data=dfx)+
          if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," subBasin Free Scale",sep=""))}
        map
        print_PDFPNG(map,dir=dir,filename=fname,
                     figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
      #KMEANS
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),"_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-m7+mapX_fillFreeScaleKMeans(data=dfx)+
          if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," subBasin Free Scale",sep=""))}
        map
        print_PDFPNG(map,dir=dir,filename=fname,
                     figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
    } # Close bySubBasin loop
    
  } # End Year Loop
  
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  # BY SECTOR 
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  
  
  # Common scales Across all years removing totals or other non-wanted categories

    #------------------
    # Admin 1 Scale from Maximum Values across Types
    #----------------
  
  years<-yearsOriginal
    
    
    # Gridded Boundary
    df1<-subset(df,select=c("lat","lon",years,"Type"))
    # Convert to Spatial Point Data Frames
    df1 = SpatialPointsDataFrame(SpatialPoints(coords=(cbind(df1$lon,df1$lat))),data=df1)
    proj4string(df1)<-projX
    # Crop to the Regional file shpa boundary
    df3<-raster::intersect(df1,b1);
    gridded(df3)<-TRUE
    
    dfCommonScaleYearsAllTypesTotal<-df3@data
    dfCommonScaleYearsAllTypes<-dfCommonScaleYearsAllTypesTotal%>%dplyr::filter(!(Type %in% moduleRemove))
    
    df3@data<-df3@data%>%dplyr::filter(!(Type %in% moduleRemove))
    
    # GRIDDED 1 SCALE (Maximum values across grid cells for all selected Types)
    df1ScaleGridLatLon<<- df3@data%>%dplyr::select(-c(Type))%>%
      group_by(lat,lon) %>% 
      summarise_all(funs(max(.,na.rm=T))) %>% as.data.frame
    head(df1ScaleGridLatLon)
    
    df1ScaleGrid<<-df1ScaleGridLatLon %>% dplyr::select(-c(lat,lon))
    head(df1ScaleGrid);max(df1ScaleGrid)
   
    #------------------
    # Admin 1 Scale from Maximum Values across Types
    #----------------
    
    # Gridded Boundary
    df1<-df1ScaleGridLatLon
    # Convert to Spatial Point Data Frames
    df1 = SpatialPointsDataFrame(SpatialPoints(coords=(cbind(df1$lon,df1$lat))),data=df1)
    proj4string(df1)<-projX
    # Crop to the Regional file shpa boundary
    df3<-raster::crop(df1,b1);plot(df3)  # Crop to Layer shpa
    gridded(df3)<-TRUE # Create Gridded Data
    head(df3@data);max(df3@data)
    
    # Aggregate Data spatially by spatial level
    df1ScaleAdmin<<-spatAgg_gridded2shape(gridded=df3,shape=shpa1,byLev="NAME_1",boundBox=b1,
                                           moduleAggType=moduleAggType)
    df1ScaleAdmin<-df1ScaleAdmin%>%dplyr::select(-contains("NAME_1"))
    df1ScaleAdmin<-df1ScaleAdmin%>%dplyr::select(-contains("Mean"))
    head(df1ScaleAdmin);max(df1ScaleAdmin[,2:ncol(df1ScaleAdmin)])
    
    #------------------
    # Basin 1 Scale
    #-----------------
    
    df1ScaleBasin<<-spatAgg_gridded2shape(gridded=df3,shape=shpbasin1,byLev="basin_name",boundBox=b1,
                                           moduleAggType=moduleAggType)
    df1ScaleBasin<-df1ScaleBasin%>%dplyr::select(-contains("basin_name"))
    df1ScaleBasin<-df1ScaleBasin%>%dplyr::select(-contains("Mean"))
    head(df1ScaleBasin);max(df1ScaleBasin[,2:ncol(df1ScaleBasin)])
    
    if(bySubBasin==1){
    #------------------
    # subBasin 1 Scale
    #----------------
    
    df1ScalesubBasin<<-spatAgg_gridded2shape(gridded=df3,shape=shpsubBasin1,byLev="subBasin_name",boundBox=b1,
                                             moduleAggType=moduleAggType)
    df1ScalesubBasin<-df1ScalesubBasin%>%dplyr::select(-contains("subBasin_name"))
    df1ScalesubBasin<-df1ScalesubBasin%>%dplyr::select(-contains("Mean"))
    head(df1ScalesubBasin);max(df1ScalesubBasin[,2:ncol(df1ScalesubBasin)])
    }
    
    
  years<-yearsOriginal;
  
  types<-unique(df$Type)
  
  
  for (type in types){
    
    #____________________
    # GRIDDED
    #____________________
    
    # Gridded Boundary
    df1<-subset(df,select=c("lat","lon",years,"Type"))
    df1<-df1[df1$Type==type,]
    if(nrow(df1)>0){
    df1<-subset(df1,select=-c(Type))
    
    # Convert to Spatial Point Data Frames
    df1 = SpatialPointsDataFrame(
      SpatialPoints(coords=(cbind(df1$lon,df1$lat))),
      data=df1
    )
    proj4string(df1)<-projX
    
    # Crop to the Regional file shpa boundary
    df2<-raster::intersect(df1,b1);plot(df2)  # Crop to Layer shpa
    dfxtra<<-raster::intersect(df1,b1);plot(dfxtra) #create larger boundary extents for plotting in tmap
    gridded(dfxtra)<-TRUE
    df3<-df2
    gridded(df3)<-TRUE # Create Gridded Data
    dfxtra<<-dfxtra
    
    if(meanYearOnly==1){years<-years[length(years)]}
    for (year_i in years){
      r<-df3;r@data<-r@data%>%dplyr::select(-lat,-lon)%>%dplyr::select(year_i)
      r<-raster(r)
      projection(r)<-projX
      rcrop<-raster::intersect(r,b1)
      rcropP<-rasterToPolygons(rcrop)
      
      # Grid Over Admin Boundaries
      fname<-paste("map_",moduleName,"_00gridAdmin_",region_i,"_watSup",moduleUnits,"_GridPointOverlap_",type,"_",gsub("X","",year_i),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        m<-tm_shape(rcrop)+tm_raster(col=year_i,style="kmeans",n=10,title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
          tm_legend(title=type,outside = TRUE, text.size = .8)+
          tm_shape(rcropP)+tm_borders("gray40",lwd=0.2, lty=1)+tm_dots()+
          tm_shape(shpa1)+tm_borders("black",lwd=2, lty=1)+tm_fill("gray",alpha=0.1);m
        print_PDFPNG(m,dir=dir,filename=fname,
                     figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
      # Grid Over Basin Boundaries
      fname<-paste("map_",moduleName,"_00gridBasin_",region_i,"_watSup",moduleUnits,"_GridPointOverlap_",type,"_",gsub("X","",year_i),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        m<-tm_shape(rcrop)+tm_raster(col=year_i,style="kmeans",n=10,title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
          tm_legend(title=type,outside = TRUE, text.size = .8)+
          tm_shape(rcropP)+tm_borders("gray40",lwd=0.2, lty=1)+tm_dots()+
          tm_shape(shpbasin1)+tm_borders("black",lwd=2, lty=1)+tm_fill("gray",alpha=0.1);m
        print_PDFPNG(m,dir=dir,filename=fname,
                     figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
      if(bySubBasin==1){
      # Grid Over subBasin Boundaries
      fname<-paste("map_",moduleName,"_00gridsubBasin_",region_i,"_watSup",moduleUnits,"_GridPointOverlap_",type,"_",gsub("X","",year_i),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        m<-tm_shape(rcrop)+tm_raster(col=year_i,style="kmeans",n=10,title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
          tm_legend(title=type,outside = TRUE, text.size = .8)+
          tm_shape(rcropP)+tm_borders("gray40",lwd=0.2, lty=1)+tm_dots()+
          tm_shape(shpsubBasin1)+tm_borders("black",lwd=2, lty=1)+tm_fill("gray",alpha=0.1);m
        print_PDFPNG(m,dir=dir,filename=fname,
                     figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }}
    }
    
    #------------------
    # Plot Gridded
    #-----------------
    
    if(type %in% moduleRemove){
      dfCommonScaleYears<<-dfCommonScaleYearsAllTypesTotal%>%
        dplyr::filter(Type == type)%>%
        dplyr::select(-c(lat,lon,Type))}else{
      dfCommonScaleYears<<-dfCommonScaleYearsAllTypes%>%dplyr::filter(!(Type %in% moduleRemove))%>%
                        dplyr::filter(Type == type)%>%
                        dplyr::select(-c(lat,lon,Type))}
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<<-1}; max(dfCommonScaleYears)
    
    years<-yearsOriginal;
    # Gridded Boundary
    df1<-subset(df,select=c("lat","lon",years,"Type"))
    df1<-df1[df1$Type==type,]
    df1<-subset(df1,select=-c(Type))
    
    # Convert to Spatial Point Data Frames
    df1 = SpatialPointsDataFrame(SpatialPoints(coords=(cbind(df1$lon,df1$lat))),data=df1)
    proj4string(df1)<-projX
    
    # Crop to the Regional file shpa boundary
    df2<-raster::intersect(df1,b1);plot(df2)  # Crop to Layer shpa
    dfxtra<<-raster::intersect(df1,b1);plot(dfxtra) #create larger boundary extents for plotting in tmap
    gridded(dfxtra)<-TRUE
    df3<-df2
    gridded(df3)<-TRUE # Create Gridded Data
    dfxtra<<-dfxtra
    
    years<-yearsOriginal;
    dfx<-df3
    dfx@data<-subset(dfx@data,select=c(years))  # Choose the years
    dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
    
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-mapX_raster(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScaleYears)+m8+tm_legend(title=moduleUnits)+
        if(titleOn==1){tm_layout(main.title=paste(type,sep=""))} 
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE.gif",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                        filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
        animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
        print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                     dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
    }
    }

##ZTEST    
    # KMEANS
    if(sd(as.matrix(dfCommonScaleYears))!=0){
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS_OWNSCALE",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-mapX_rasterKMeans(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScaleYears)+m8+tm_legend(title=moduleUnits)+
        if(titleOn==1){tm_layout(main.title=paste(type,sep=""))} 
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS_OWNSCALE.gif",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                        filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
        animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
        print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                     dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
    }
    }
    }
    

    if(!(type %in% moduleRemove)){
    # Common scales for all selected types (excluding things like total or non-agriculture)
    
   
      dfgridX<-df1ScaleGrid
      
      fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems",sep="")  
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<- mapX_raster(data=dfx,scaleData=dfgridX) + m8 +tm_legend(title=moduleUnits)+
          if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type,sep=""))} 
        print_PDFPNG(map,dir=dir,filename=fname,
                     figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
        if(animationsOn==1){
          fname<-paste(dir,"/anim_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems.gif",sep="") 
          if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
            animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                            filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
            animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                            filename=fname,width=NA,height=NA,delay=delay)
            print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                         dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
            selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
        }
      }
      
      if(sd(as.matrix(dfCommonScaleYears))!=0){
      fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<- mapX_rasterKMeans(data=dfx,scaleData=dfgridX) + m8 +tm_legend(title=moduleUnits)+
          if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type,sep=""))} 
        print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/anim_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems_KMEANS.gif",sep="") 
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                          filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
          animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                          filename=fname,width=NA,height=NA,delay=delay)
          print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                       dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }
    }}

    }# Close if !type in moduleRemove
    #____________________
    # By ADMIN Region
    #____________________
    
    head(df3)
    
    dxpbyAdmin<<-spatAgg_gridded2shape(gridded=df3,shape=shpa1,byLev="NAME_1",boundBox=b1,moduleAggType=moduleAggType)
    head(dxpbyAdmin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbyAdmin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_ByAdminRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
    }
    
    dfCommonScaleYears<<-dxpbyAdmin%>%dplyr::select(-c(NAME_1));max(dfCommonScaleYears)
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<<-1}
    
    shpa.x<-shpa1
    shpa.x@data<-join(shpa.x@data,dxpbyAdmin,by=c("NAME_1")) %>% 
      subset(select=c(years,"NAME_1")) %>% subset(select=-c(NAME_1))
    dfx<-shpa.x
    dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
    
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    map<- m7 + mapX_fill(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
      if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Province",sep=""))} 
    map
    print_PDFPNG(map,dir=dir,fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
    selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE.gif",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                        filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
        animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
        print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                     dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
    }
    }
    
    # KMEANS 
    if(sd(as.matrix(dfCommonScaleYears))!=0){
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS_OWNSCALE",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
    if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Province",sep=""))} 
    map
    print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
    selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
        #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"KMEANS_OWNSCALE.gif",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                        filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
        animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
        print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                     dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
    }
    }
    }
    
    
    if(!(type %in% moduleRemove)){
    
   #1Scale Across Types
    
    dfgridX<-df1ScaleAdmin
    
      fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<- m7 + mapX_fill(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
        if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Province",sep=""))} 
      print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/anim_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems.gif",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                          filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
          animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                          filename=fname,width=NA,height=NA,delay=delay)
          print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                       dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }}
      
      if(sd(as.matrix(dfCommonScaleYears))!=0){
      fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
        if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Province",sep=""))} 
      print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/anim_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems_KMEANS.gif",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                          filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
          animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                          filename=fname,width=NA,height=NA,delay=delay)
          print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                       dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }}}
    
    } #Close if !type in moduleRemove
    
    #____________________
    # By BASIN Region
    #____________________
    
    head(df3)
     
    dxpbyBasin<<-spatAgg_gridded2shape(gridded=df3,shape=shpbasin1,byLev="basin_name",boundBox=b1,moduleAggType=moduleAggType)
    head(dxpbyBasin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbyBasin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_ByBasinRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
    }
    
    dfCommonScaleYears<<-dxpbyBasin%>%dplyr::select(-c(basin_name))
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<<-1}
    
    shpa.x<-shpbasin1
    shpa.x@data<-join(shpa.x@data,dxpbyBasin,by=c("basin_name")) %>% 
      subset(select=c(years,"basin_name")) %>% subset(select=-c(basin_name))
    dfx<-shpa.x
    dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
    
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    map<- m7 + mapX_fill(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
      if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Basin",sep=""))}
    map
    print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
    selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE.gif",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                        filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
        animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
        print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                     dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
    }}
    
    #KMEANS
    if(sd(as.matrix(dfCommonScaleYears))!=0){
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS_OWNSCALE",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
      if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Basin",sep=""))}
    map
    print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
    selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS_OWNSCALE.gif",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                        filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
        animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
        print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                     dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
    }}}
    
    
    if(!(type %in% moduleRemove)){
    # 1 scales for all non-agricultural demands
    
    dfgridX<-df1ScaleBasin
    
      fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<- m7 + mapX_fill(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
        if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Basin",sep=""))} 
      print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/anim_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems.gif",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                          filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
          animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                          filename=fname,width=NA,height=NA,delay=delay)
          print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                       dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }}
      
      if(sd(as.matrix(dfCommonScaleYears))!=0){
      fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
        if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Basin",sep=""))} 
      print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/anim_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems_KMEANS.gif",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                        filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
          animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                          filename=fname,width=NA,height=NA,delay=delay)
          print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                       dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }
    }}
    } # Close ! type in moduleRemove
    
    if(bySubBasin==1){
      #____________________
      # By subBasin Region
      #____________________
      
      head(df3)
     
      dxpbysubBasin<<-spatAgg_gridded2shape(gridded=df3,shape=shpsubBasin1,byLev="subBasin_name",boundBox=b1,moduleAggType=moduleAggType)
      head(dxpbysubBasin)
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbysubBasin,file=paste(dir,"/subBasin/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_BysubBasinRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
      }
      
      dfCommonScaleYears<<-dxpbysubBasin%>%dplyr::select(-c(subBasin_name))
      if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<<-1}
      
      shpa.x<-shpsubBasin1
      shpa.x@data<-join(shpa.x@data,dxpbysubBasin,by=c("subBasin_name")) %>% 
        subset(select=c(years,"subBasin_name")) %>% subset(select=-c(subBasin_name))
      dfx<-shpa.x
      dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
      
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<- m7 + mapX_fill(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
        if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by subBasin",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/subBasin/anim_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE.gif",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                          filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
          animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                          filename=fname,width=NA,height=NA,delay=delay)
          print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                       dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }
      }
      
      #KMEANS
      if(sd(as.matrix(dfCommonScaleYears))!=0){
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS_OWNSCALE",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
        if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by subBasin",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/subBasin/anim_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS_OWNSCALE.gif",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                          filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
          animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                          filename=fname,width=NA,height=NA,delay=delay)
          print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                       dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }
      }}
      
      if(!(type %in% moduleRemove)){
      # Common scales for all non-agricultural demands
      
      dfgridX<-df1ScalesubBasin
      
        fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<- m7 + mapX_fill(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
          if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by subBasin",sep=""))} 
        print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
        #---- Animation File
        if(animationsOn==1){
          fname<-paste(dir,"/subBasin/anim_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems.gif",sep="")
          if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
            animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                            filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
            animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                            filename=fname,width=NA,height=NA,delay=delay)
            print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                         dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
            selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
        }}
        
        if(sd(as.matrix(dfCommonScaleYears))!=0){
        fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems_KMEANS",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
          if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by subBasin",sep=""))} 
        print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
        #---- Animation File
        if(animationsOn==1){
          fname<-paste(dir,"/subBasin/anim_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_1ScaleDems_KMEANS.gif",sep="")
          if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
            animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                            filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
            animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                            filename=fname,width=NA,height=NA,delay=delay)
            print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                         dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
            selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
        }
        }}
        
      } # Close if ! type in moduleRemove
      }# Close bySubBasin
    } # Close if empty because Type is 0
  } # Close Type
} # Close Mapping Function

#________________________________________________________________________________
#--------------------------------------------------------------------------------
# Base Maps
#--------------------------------------------------------------------------------
#________________________________________________________________________________


#--- Choose base boundary files (Province, country, basins)

shp0<<-shp_wdspne10mAdmin0
shp<<-shp_gadm36L1    # GCAM Regions  !!!! FOR URUGUAY NO GCAM REGION SHAPEFILE
names(shp@data)[names(shp@data)=="NAME_0"]<-"GCAM_region"
shp1<<-shp_gadm36L1  # GADM Provinces
shp_basins<<-shp_PNNL235CLM5ArcMin_multi  # GCAm Basins
#shp_subBasins<<-shp_HydroBasinsLev3  # SubBasin Map

if(region_i=="Colombia"){
  shp_subBasins<<-shp_SIACHydroZones  # SubBasin Map
  shp_subBasins@data$subBasin_name<-shp_subBasins@data$NOM_ZH  # Rename the Subbasin File
}else{
  shp_subBasins<<-shp_HydroBasinsLev4  # SubBasin Map
  shp_subBasins@data$subBasin_name<-shp_subBasins@data$HYBAS_ID  # Rename the Subbasin File
}

# Regional Selected Region
shp0a<<-shp0[which(shp0$ADMIN==region_i),]
shp0a@data<-droplevels(shp0a@data)
shp0a<<-shp0a
plot(shp0a)

# Regional Selected Region
shpa<<-shp[which(shp$GCAM_region==region_i),]
if(region_i=="Argentina"){  # FOR ARGENTINA MERGE Ciudad de Buenos Aires into Buenos Aires
  shpa@data$NAME_1[which(shpa@data$NAME_1=="Ciudad de Buenos Aires")]<-"Buenos Aires"
  shpa<-aggregate(shpa, by= "NAME_1")
}
shpa@data<-droplevels(shpa@data)
shpa<<-shpa
plot(shpa)

# GCAM Regional Bounding Box
b1<<-as.data.frame(bbox(shp[which(shp$GCAM_region==region_i),]))   # Get Bounding box
expandbyPercent<<-2; b1$min;b1$max
b1$min[1]<-if(b1$min[1]<0){(1+expandbyPercent/100)*b1$min[1]}else{(1-expandbyPercent/100)*b1$min[1]};
b1$min[2]<-if(b1$min[2]<0){(1+expandbyPercent/100)*b1$min[2]}else{(1-expandbyPercent/100)*b1$min[2]};
b1$max[1]<-if(b1$max[1]<0){(1-expandbyPercent/100)*b1$max[1]}else{(1+expandbyPercent/100)*b1$max[1]};
b1$max[2]<-if(b1$max[2]<0){(1-expandbyPercent/100)*b1$max[2]}else{(1+expandbyPercent/100)*b1$max[2]};
b1$min;b1$max;
b1<<-as(extent(as.vector(t(b1))), "SpatialPolygons")
proj4string(b1)<-CRS(projX) # ASSIGN COORDINATE SYSTEM
b1<<-b1

# Simple Boundary
shp0b<<-raster::crop(shp0,b1)
shp0b@data<-droplevels(shp0b@data)
shp0b<<-shp0b
plot(shp0b)

shpb<<-raster::crop(shp,b1)
shpb@data<-droplevels(shpb@data)
shpb<<-shpb
plot(shpb)

# GADM Boundaries Selected Region
shpa1<<-shp1[which(shp1$NAME_0==region_i),]
if(region_i=="Argentina"){  # FOR ARGENTINA MERGE Ciudad de Buenos Aires into Buenos Aires
  shpa1@data$NAME_1[which(shpa1@data$NAME_1=="Ciudad de Buenos Aires")]<-"Buenos Aires"
  shpa1<-aggregate(shpa1, by= "NAME_1")
}
shpa1@data<-droplevels(shpa1@data)
shpa1<<-shpa1
plot(shpa1)

# GADM Boundaries Bounding Box
shpb1<-raster::crop(shp1,b1)
shpb1@data<-droplevels(shpb1@data)
shpb1<<-shpb1
plot(shpb1)

# GCAM Basins Boundaries Bounding Box
shpbasin<-raster::crop(shp_basins,b1)
shpbasin@data<-droplevels(shpbasin@data)
plot(shpbasin)
shpbasin<<-shpbasin
dev.off()

# GCAM Basins Boundaries For Analysis
shpbasin1<-raster::crop(shp_basins,shpa)
shpbasin1@data<-droplevels(shpbasin1@data)
plot(shpbasin1)
shpbasin1<<-shpbasin1
dev.off()

if(bySubBasin==1){
  # subBasins Boundaries Bounding Box
  shpsubBasin<<-raster::crop(shp_subBasins,b1)
  shpsubBasin@data<-droplevels(shpsubBasin@data)
  plot(shpsubBasin)
  shpsubBasin<<-shpsubBasin
  dev.off()
  
  # subBasins Boundaries For Analysis
  shpsubBasin1<-raster::crop(shp_subBasins,shpa)
  shpsubBasin1@data<-droplevels(shpsubBasin1@data)
  plot(shpsubBasin1)
  shpsubBasin1<<-shpsubBasin1
  dev.off()
  
  # Dissolve subbasins
  shpsubBasin<-aggregate(shpsubBasin, by= "subBasin_name")
  shpsubBasin<<-shpsubBasin
  plot(shpsubBasin)
  shpsubBasin1<-aggregate(shpsubBasin1, by= "subBasin_name")
  shpsubBasin1<<-shpsubBasin1
  plot(shpsubBasin1)
}

#______________________
# Merging a shapefile up
# dissolving by country or subbasin
#______________________

# Country Selected Region
shpa1x<-shp0a
plot(shpa1x)

# Bounding Box
shpb1x<-shp0b
plot(shpb1x)

# Bounding Box without chosen region
shpc1x<-gDifference(shpb1x, shpa1x)
shpc1x<<-shpc1x
plot(shpc1x,col="gray80",border="black")


#---------------------
# Base maps - Admin Boundaries Used 
#---------------------

if(!dir.exists(paste(wdfigsOut,"/",region_i,sep=""))){dir.create(paste(wdfigsOut,"/",region_i,sep=""))}
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/Basemaps",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/Basemaps",sep=""))}
dir<-paste(wdfigsOut,"/",region_i,"/Basemaps",sep="")

#-----------------------  Admin Boundaries with Labels


m1<<- tm_shape(shpb1x) + 
  tm_borders("grey",lwd=0.5, lty=1) +
  tm_shape(shpa1) + 
  tm_fill("NAME_1", style="pretty",palette="Set3",legend.show=F) +
  #tm_legend(outside = TRUE, text.size = 1) +
  tm_borders("grey") +
  tm_text("NAME_1",scale=0.7,auto.placement=F, col="black") +
  tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
  tm_scale_bar(position=c("right", "bottom"),width=0.2)+
  tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
if(titleOn==1){m1<<-m1 + tm_layout(main.title=paste(region_i," State Map",sep=""))}
m1

fname<<-paste("map_basemaps_m1_",region_i,"_provincialLabelled",sep="")
print_PDFPNG(m1,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)
selectedFigs<<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))


#-----------------------  Basin Boundaries with Labels

m2<<- tm_shape(shpb1) + 
  tm_borders("grey",lwd=0.5, lty=1) +
  tm_shape(shpbasin) + 
  tm_fill("basin_name", style="pretty",palette="Set3",legend.show=F)  +
  tm_borders("grey") +
  tm_text("basin_name",scale=0.7,auto.placement=F, col="black") +tm_shape(shpa) + 
  tm_borders("black",lwd=2, lty=1) +
  tm_add_legend(title = paste("JGCRI Region ",region_i,sep=""),type = c("line"), col = "black", lwd = 2, lty = 1) +
  tm_legend(outside = TRUE, title.size = 1) +
  tm_add_legend(title = paste("JGCRI Basins",sep=""),type = c("fill"), col = "lightcoral",border.col="grey",lwd = 1, lty = 1) +
  tm_legend(outside = TRUE, text.size = 1) +
  tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
  tm_scale_bar(position=c("right", "bottom"),width=0.2)+
  tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
if(titleOn==1){m2<<-m2 + tm_layout(main.title=paste(region_i," Basin Map",sep=""))}
m2

fname<<-paste("map_basemaps_m2_",region_i,"_basinsLabelled",sep="")
print_PDFPNG(m2,dir=dir,filename=fname,figWidth_InchMaster*0.7,figHeight_InchMaster*1,pdfpng=pdfpng)
selectedFigs<<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))


#-----------------------  Cropped Basin Boundaries with Labels

m3<<- tm_shape(shpb1x) +# tm_text("NAME_0",scale=0.6,auto.placement=T, col="grey") +
  tm_borders("grey",lwd=0.5, lty=1) +
  tm_shape(shpbasin1) + 
  tm_fill("basin_name", style="pretty",palette="Set3",legend.show=F,title = "JGCRI Basin")  +
  tm_borders("grey") +
  tm_text("basin_name",scale=0.7,auto.placement=F, col="black") +
  #tm_shape(shpa) + tm_borders("black",lwd=2, lty=1) +
  #tm_add_legend(title = paste("JGCRI Region ",region,sep=""),type = c("line"), col = "black", lwd = 2, lty = 1) +
  #tm_legend(outside = TRUE, title.size = 1) +
  tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
  tm_scale_bar(position=c("right", "bottom"),width=0.2)+
  tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
if(titleOn==1){m3<<-m3 + tm_layout(main.title=paste(region_i," Basin Map",sep=""))}
m3
fname<<-paste("map_basemaps_m3_",region_i,"_basinsLabelledCropped",sep="")
print_PDFPNG(m3,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)
selectedFigs<<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))


if(bySubBasin==1){
  
  
  m2sub<<- tm_shape(shpb1x) + 
    tm_borders("grey",lwd=0.5, lty=1) +
    tm_shape(shpsubBasin1) + 
    tm_fill("subBasin_name", style="pretty",palette="Set3",legend.show=F)  +
    tm_borders("grey") +
    #tm_add_legend(title = paste("JGCRI Region ",region,sep=""),type = c("line"), col = "black", lwd = 2, lty = 1) +
    #tm_legend(outside = TRUE, title.size = 1) +
    #tm_add_legend(title = paste("JGCRI subBasins",sep=""),type = c("fill"), col = "lightcoral",border.col="grey",lwd = 1, lty = 1) +
    #tm_legend(outside = TRUE, text.size = 1) +
    tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
    tm_scale_bar(position=c("right", "bottom"),width=0.2)+
    tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
  if(titleOn==1){m2<<-m2 + tm_layout(main.title=paste(region_i," subBasin Map",sep=""))}
  m2sub
  
  print_PDFPNG(m2sub,dir=dir,filename=paste("map_basemaps_m2sub_",region_i,"_subBasinsLabelled",sep=""),figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)
  
  
  
  m3sub<<- tm_shape(shpb1x) +# tm_text("NAME_0",scale=0.6,auto.placement=T, col="grey") +
    tm_borders("grey",lwd=0.5, lty=1) +
    tm_shape(shpsubBasin1) + 
    tm_fill("subBasin_name", style="pretty",palette="Set3",legend.show=T,title = "Sub-Basin")  +
    tm_borders("grey") +
    #tm_text("subBasin_name",scale=1,auto.placement=F, col="black") +
    #tm_shape(shpa) + tm_borders("black",lwd=2, lty=1) +
    #tm_add_legend(title = paste("JGCRI Region ",region_i,sep=""),type = c("line"), col = "black", lwd = 2, lty = 1) +
    #tm_legend(outside = TRUE, title.size = 1) +
    tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
    tm_scale_bar(position=c("right", "bottom"),width=0.2)+
    tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
  if(titleOn==1){m3<<-m3 + tm_layout(main.title=paste(region_i," subBasin Map",sep=""))}
  m3sub
  
  print_PDFPNG(m3sub,dir=dir,filename=paste("map_basemaps_m3sub_",region_i,"_subBasinsLabelledCropped",sep=""),figWidth_InchMaster*0.5,figHeight_InchMaster*0.75,pdfpng=pdfpng)
  
  m4sub<<- tm_shape(shpb1) + tm_borders("white",lwd=0.5, lty=1) +
    tm_shape(shpa1) + tm_borders("grey",lwd=0.5, lty=1) +
    tm_add_legend(title = "State borders",type = c("line"), col = "grey", lwd = 0.5, lty = 1) +
    tm_legend(outside = TRUE, title.size = 1) +
    tm_shape(shpa) + 
    tm_borders("blue",lwd=2.5, lty=1) +
    tm_add_legend(title = paste("JGCRI Region ",region_i,sep=""),type = c("line"), col = "blue", lwd = 2.5, lty = 1) +
    tm_legend(outside = TRUE, title.size = 1) +
    tm_shape(shpsubBasin1) + 
    tm_borders("red", lwd=1.5, lty=1) +
    tm_add_legend(title = paste("subBasin boundaries ",region_i,sep=""),type = c("line"), col = "red", lwd = 1.5, lty = 1) +
    tm_legend(outside = TRUE, title.size = 1) +
    tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
    tm_scale_bar(position=c("right", "bottom"),width=0.2) +
    tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
  if(titleOn==1){m4<<-m4 + tm_layout(main.title=paste(region_i," Boundary Overlap",sep=""))}
  m4sub
  
  print_PDFPNG(m4sub,dir=dir,filename=paste("map_basemaps_m4sub_",region_i,"_regionProvincesubBasinsOutlines",sep=""),figWidth_InchMaster*1,figHeight_InchMaster*0.7,pdfpng=pdfpng)
  
}

#----------------------- Overlap of region, provinces and basins

m4<<- tm_shape(shpb1) + tm_borders("white",lwd=0.5, lty=1) +
  tm_shape(shpa1) + tm_borders("grey",lwd=0.5, lty=1) +
  tm_add_legend(title = "State borders",type = c("line"), col = "grey", lwd = 0.5, lty = 1) +
  tm_legend(outside = TRUE, title.size = 1) +
  tm_shape(shpa) + 
  tm_borders("blue",lwd=2.5, lty=1) +
  tm_add_legend(title = paste("JGCRI Region ",region_i,sep=""),type = c("line"), col = "blue", lwd = 2.5, lty = 1) +
  tm_legend(outside = TRUE, title.size = 1) +
  tm_shape(shpbasin1) + 
  tm_borders("red", lwd=1.5, lty=1) +
  tm_add_legend(title = paste("Basin boundaries ",region_i,sep=""),type = c("line"), col = "red", lwd = 1.5, lty = 1) +
  tm_legend(outside = TRUE, title.size = 1) +
  tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
  tm_scale_bar(position=c("right", "bottom"),width=0.2) +
  tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
if(titleOn==1){m4<<-m4 + tm_layout(main.title=paste(region_i," Boundary Overlap",sep=""))}
m4

print_PDFPNG(m4,dir=dir,filename=paste("map_basemaps_m4_",region_i,"_regionProvinceBasinsOutlines",sep=""),figWidth_InchMaster*0.5,figHeight_InchMaster*.75,pdfpng=pdfpng)



#----------------------- Regional Map showing Countries

m5<<- tm_shape(shpb1x) + 
  tm_fill("ADMIN", alpha=0.9,style="pretty",palette="Set3",title=paste("Country",sep=""),legend.show = F) +
  tm_shape(shpb1) + 
  tm_borders(col="grey",lwd=1,lty=1) +
  tm_shape(shpb1x) + tm_text("ADMIN",scale=1,auto.placement=T, col="black") +
  tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
  tm_scale_bar(position=c("right", "bottom"),width=0.2)+
  tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
if(titleOn==1){m5<<-m5 + tm_layout(main.title=paste(region_i," Regional Map",sep=""))}
m5
fname<<-paste("map_basemaps_m5_",region_i,"_regionalMap",sep="")
print_PDFPNG(m5,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)
selectedFigs<<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))

#----------------------- Regional Map with Mapping Boundaries

m6<<- tm_shape(shpb1x) + 
  tm_fill("ADMIN", alpha=0.9,style="pretty",palette="Set3",title=paste("Country",sep=""),legend.show = F) +
  tm_shape(shpb1x) + 
  tm_borders(col="grey",lwd=0.5,lty=1) +
  tm_shape(shpa1) + tm_borders("grey",lwd=0.5, lty=1) +
  tm_add_legend(title = "State borders",type = c("line"), col = "grey", lwd = 0.5, lty = 1) +
  tm_legend(outside = TRUE, title.size = 1) +
  tm_shape(shpa) + 
  tm_borders("blue",lwd=2.5, lty=1) +
  tm_add_legend(title = paste("JGCRI Region ",region_i,sep=""),type = c("line"), col = "blue", lwd = 2.5, lty = 1) +
  tm_legend(outside = TRUE, title.size = 1) +
  tm_shape(shpbasin1) + 
  tm_borders("red", lwd=1.5, lty=1) +
  tm_add_legend(title = paste("Basin boundaries ",region_i,sep=""),type = c("line"), col = "red", lwd = 1.5, lty = 1) +
  tm_legend(outside = TRUE, title.size = 1) +
  tm_shape(shpb1x) + tm_text("ADMIN",scale=1,auto.placement=T, col="black") +
  tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
  tm_scale_bar(position=c("right", "bottom"),width=0.2) +
  tm_layout(frame = TRUE, bg.color="white")+ tm_layout_z
if(titleOn==1){m6<<-m6 + tm_layout(main.title=paste(region_i," Regional Map",sep=""))}
m6

fname<<-paste("map_basemaps_m6_",region_i,"_regionalMapBasinsProvinces",sep="")
print_PDFPNG(m6,dir=dir,filename=fname,figWidth_InchMaster*0.6,figHeight_InchMaster*0.75,pdfpng=pdfpng)
selectedFigs<<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))


#-----------------------  Surrounding Regions and Ocean for Plotting with Analysis Layers



m7<<- tm_shape(shpc1x) + 
  tm_fill("gray90",alpha=0.8,style="pretty",palette="Set3",title=paste("Country",sep=""),legend.show = F)  +
  tm_shape(shpb1x) + tm_text("ADMIN",scale=0.6,auto.placement=F,col="grey") + tm_borders("black",lwd=1,lty=1) +
  #tm_shape(shpa1x) + 
  #tm_fill("white",alpha=0.5,style="pretty",palette="Set3",title=paste("Country",sep=""),legend.show = F)  +
  tm_borders("black",lwd=1,lty=1) +
  #tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
  #tm_scale_bar(position=c("right", "bottom"),width=0.2) +
  tm_layout(frame = TRUE, bg.color="lightcyan") + tm_layout_z
if(titleOn==1){m7<<-m7 + tm_layout(main.title=paste(region_i," Regional Map",sep=""))}
m7


print_PDFPNG(m7,dir=dir,filename=paste("map_basemaps_m7_",region_i,"_regionForAnalysis",sep=""),
             figWidth_InchMaster*0.5,figHeight_InchMaster*0.75,pdfpng=pdfpng)


m8<<-tm_shape(shpb1x) + tm_text("ADMIN",scale=0.6,auto.placement=F,col="grey") + tm_borders("black",lwd=1,lty=1) +
  tm_borders("black",lwd=1,lty=1) +
  #tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
  #tm_scale_bar(position=c("right", "bottom"),width=0.2) + tm_layout_z
  if(titleOn==1){m7<<-m7 + tm_layout(main.title=paste(region_i," Regional Map",sep=""))}
m8

print_PDFPNG(m8,dir=dir,filename=paste("map_basemaps_m8_",region_i,"_regionForAnalysisBlank",sep=""),
             figWidth_InchMaster*0.5,figHeight_InchMaster*0.75,pdfpng=pdfpng)


} # Close if Mapping required Loop

#____________________________________________
# Prepare Data From GCAM DataBase
#____________________________________________

#----------
# Charts (df_all)
#-----------

if(TRUE){
df_allX<-data.frame()

# Total final energy by aggregate end-use sector
tbl <- getQuery(queryData.proj, "Total final energy by aggregate end-use sector") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
df$technology<-"Technology"
names(df)[names(df)=="sector"]<-"subsector"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"finalNrgbySec"; head(df) 
df$Query<-"Total final energy by aggregate end-use sector"
df$Title<-"Final Energy by Sec"
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"~Final~Energy~(TWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Sector",
                   FillPalette1="colorsX_finalNrg_sec",
                   Fill2=subsector,
                   FillLabel2="Sector",
                   FillPalette2="colorsX_finalNrg_sec") %>%  
  subset(.,select=-c(technology,subsector));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)
df_allX_finalNrgBySec<-df

# GDP per capita MER by region
tbl <- getQuery(queryData.proj, "GDP per capita MER by region") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
df$technology<-"Technology"
df$sector<-"Sector"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"gdpPerCapita"; head(df) 
df$Query<-"GDP per capita MER by region"
df$Title<-"GDP per Capita MER"
df$NewValue<-df$value
df$NewUnits<-"GDP~per~Capita~(Thousand~1990~USD~per~Person)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1="GDP Per Capita",
                   FillLabel1="GDP Per Capita",
                   FillPalette1="cbPalette",
                   Fill2="GDP Per Capita",
                   FillLabel2="GDP Per Capita",
                   FillPalette2="cbPalette") %>%  
  subset(.,select=-c(technology,sector));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)


# GDP MER per Region
tbl <- getQuery(queryData.proj, "GDP MER by region") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
df$technology<-"Technology"
df$sector<-"Sector"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"gdp"; head(df) 
df$Query<-"GDP MER by region"
df$Title<-"GDP MER"
df$NewValue<-df$value/1000
df$NewUnits<-"GDP~(Billion~1990~USD)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1="GDP",
                   FillLabel1="GDP",
                   FillPalette1="cbPalette",
                   Fill2="GDP",
                   FillLabel2="GDP",
                   FillPalette2="cbPalette") %>%  
  subset(.,select=-c(technology,sector));
# Add Local Data For Comparison
dfCOL<-data.frame()
dfCOL<-df%>%filter(region=="Colombia",x>=2010,x<=2030)%>%
  dplyr::select(-value,-NewValue,-scenario)%>%unique%>%
  mutate(scenario="LocalData",
         Query="R.Delgado_DANE col dept. of stats",
         NewValue= case_when(x==2010~290.417,
                             x==2015~340.9086,
                             x==2020~386.438,
                             x==2025~448.968,
                             x==2030~523.254),
         value=NewValue)
df<-rbind.data.frame(df,dfCOL)
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)
dfgdp<-df

# GDP Growth Rate
df<-dfgdp %>% group_by(scenario,region) %>% mutate(NewValue=(NewValue-lag(NewValue,order_by=year))*100/lag(NewValue,order_by=year))
df<-df %>% mutate (NewUnits="GDP~Growth~Rate~(Percent)", # Use ~ for spaces. Will be parsed later
                   Units=NewUnits,
                   value=NewValue,
                   Aggregate="mean",  # How to aggregate over spatial and temporal units
                   Query="Calculated",
                   Title="GDP Growth Rate (%)",
                   param="gdpGrowthRate",
                   Fill1="GDP growth rate",
                   FillLabel1="GDP growth rate",
                   FillPalette1="cbPalette",
                   Fill2="GDP growth rate",
                   FillLabel2="GDP growth rate",
                   FillPalette2="cbPalette")%>%as.data.frame;
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

# Population
tbl <- getQuery(queryData.proj, "Population by region") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
df$technology<-"Technology"
df$sector<-"Sector"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"pop"; head(df) 
df$Query<-"Population by region"
df$Title<-"Population"
df$NewValue<-df$value/1000
df$NewUnits<-"Population~(Millions)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1="Population",
                   FillLabel1="Population",
                   FillPalette1="cbPalette",
                   Fill2="Population",
                   FillLabel2="Population",
                   FillPalette2="cbPalette") %>%  
  subset(.,select=-c(technology,sector));
# Add Local Data For Comparison
dfCOL<-data.frame()
dfCOL<-df%>%filter(region=="Colombia",x>=2015,x<=2030)%>%
  dplyr::select(-value,-NewValue,-scenario)%>%unique%>%
  mutate(scenario="LocalData",
         Query="R.Delgado_UN",
         NewValue= case_when(x==2015~48.229,
                             x==2020~50.22,
                             x==2025~51.854,
                             x==2030~53.134),
         value=NewValue)
df<-rbind.data.frame(df,dfCOL)
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

# Primary energy consumption by region (direct equivalent)
tbl <- getQuery(queryData.proj, "primary energy consumption by region (direct equivalent)") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
df$technology<-"Technology"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"primNrgConsumByFuel"; head(df) 
df$Query<-"primary energy consumption by region (direct equivalent)"
df$Title<-"primary energy consumption by region (direct equivalent)"
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"Primary~Energy~Consumption~(TWh)" # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_Unassigned",
                   Fill2=fuel,
                   FillLabel2="Fuel",
                   FillPalette2="colorsX_PAL_pri_ene") %>%  
  subset(.,select=-c(technology,fuel));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

# Electricity generation by aggregate technology
tbl <- getQuery(queryData.proj, "Electricity generation by aggregate technology") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
#df$technology<-technology
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"elecByTech"; head(df) 
df$Query<-"Electricity generation by aggregate technology"
df$Title<-"Electricity Generation"
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"Electricity~Generation~(TWh)" # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"segment"
df<-df %>% mutate (Fill1="Fill1",
                   FillLabel1="Fill1",
                   FillPalette1="colorsX_Unassigned",
                   Fill2=technology,
                   FillLabel2="Technology",
                   FillPalette2="colorsX_elec_tech_colors") %>%  
  subset(.,select=-c(technology));
# Add Local Data For Comparison
# Add data for Existing Variables & Years
dfCOL<-data.frame()
dfCOL<-df%>%filter(region=="Colombia",x>=2020,x<=2030)%>%
  dplyr::select(-value,-NewValue,-scenario)%>%unique%>%
  mutate(scenario="LocalData",
         Query="R.Delgado_UN",
         NewValue= case_when((x==2025 & Fill2=="m Solar")~1.29,
                             (x==2025 & Fill2=="l Wind")~4.51,
                             (x==2025 & Fill2=="g Biomass")~0.64,
                             (x==2025 & Fill2=="a Coal")~2.42,
                             (x==2025 & Fill2=="c Gas")~1.62,
                             (x==2025 & Fill2=="k Hydro")~75.43,
                             (x==2030 & Fill2=="m Solar")~1.53,
                             (x==2030 & Fill2=="l Wind")~5.36,
                             (x==2030 & Fill2=="g Biomass")~0.77,
                             (x==2030 & Fill2=="a Coal")~8.64,
                             (x==2030 & Fill2=="c Gas")~2.16,
                             (x==2030 & Fill2=="k Hydro")~98.18,
                             (x==2006 & Fill2=="e Oil")~0.0159,
                             (x==2006 & Fill2=="c Gas")~6.8701,
                             (x==2006 & Fill2=="b Coal")~2.588,
                             (x==2006 & Fill2=="k Hydro")~4.02534,
                             (x==2007 & Fill2=="e Oil")~0.0198,
                             (x==2007 & Fill2=="c Gas")~6.117.9,
                             (x==2007 & Fill2=="b Coal")~2.903,
                             (x==2007 & Fill2=="k Hydro")~4.17952,
                             (x==2008 & Fill2=="e Oil")~0.0145,
                             (x==2008 & Fill2=="c Gas")~5.231,
                             (x==2008 & Fill2=="b Coal")~2.4868,
                             (x==2008 & Fill2=="k Hydro")~4.3520,
                             (x==2009 & Fill2=="e Oil")~0.3754,
                             (x==2009 & Fill2=="c Gas")~10.4172,
                             (x==2009 & Fill2=="b Coal")~3.6951,
                             (x==2009 & Fill2=="k Hydro")~3.87138,
                             (x==2010 & Fill2=="e Oil")~0.483,
                             (x==2010 & Fill2=="c Gas")~11.5451,
                             (x==2010 & Fill2=="b Coal")~3.4646,
                             (x==2010 & Fill2=="k Hydro")~3.80886,
                             (x==2011 & Fill2=="e Oil")~0.111,
                             (x==2011 & Fill2=="c Gas")~7.5962,
                             (x==2011 & Fill2=="b Coal")~1.6269,
                             (x==2011 & Fill2=="k Hydro")~4.52588,
                             (x==2012 & Fill2=="e Oil")~0.203,
                             (x==2012 & Fill2=="c Gas")~8.7113,
                             (x==2012 & Fill2=="b Coal")~2.4926,
                             (x==2012 & Fill2=="k Hydro")~4.49236,
                             (x==2013 & Fill2=="e Oil")~0.3551,
                             (x==2013 & Fill2=="c Gas")~10.9558,
                             (x==2013 & Fill2=="b Coal")~5.5262,
                             (x==2013 & Fill2=="k Hydro")~4.18359,
                             (x==2014 & Fill2=="e Oil")~0.2932,
                             (x==2014 & Fill2=="c Gas")~12.3693,
                             (x==2014 & Fill2=="b Coal")~5.6302,
                             (x==2014 & Fill2=="k Hydro")~4.21576,
                             (x==2015 & Fill2=="e Oil")~1.5767,
                             (x==2015 & Fill2=="c Gas")~12.8095,
                             (x==2015 & Fill2=="b Coal")~6.245,
                             (x==2015 & Fill2=="k Hydro")~4.24638,
                             (x==2016 & Fill2=="e Oil")~2.0563,
                             (x==2016 & Fill2=="c Gas")~10.3205,
                             (x==2016 & Fill2=="b Coal")~5.3799,
                             (x==2016 & Fill2=="k Hydro")~4.42461,
                             TRUE ~ NA_real_),
         value=NewValue);
# Add new Factors if needed
dfCOLx<-dfCOL[complete.cases(dfCOL),]
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2020, NewValue=75.338, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2006, NewValue=2.6126, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2007, NewValue=2.7894, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2008, NewValue=3.1418, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2009, NewValue=2.7641, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2010, NewValue=3.2061, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2011, NewValue=4.0292, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2012, NewValue=3.5592, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2013, NewValue=3.5220, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2014, NewValue=3.7645, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2015, NewValue=3.4535, value=NewValue))
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2011, NewValue=3.9378, value=NewValue))
df<-rbind.data.frame(df,dfCOL)
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)


tbl <- getQuery(queryData.proj, "water consumption by sector") # Tibble
df<-as.data.frame(tbl);head(df)                # Data frame
df<-df%>%filter(region==region_i)
df<-df %>% mutate (vintage=paste("Vint_",year,sep=""),
                               Query="water consumption by sector",
                               Title="Water Consumption",
                               Fill1="Fill1",
                               FillLabel1="FillLabel1",
                               FillPalette1="colorsX_Unassigned",
                               Aggregate="sum",
                               segment="segment",
                               x=year,
                               xLabel="Year",
                               param="watConsumBySec",
                               NewValue=value,
                               NewUnits="Water~Consumption~(km^3)", # Use ~ for spaces. Will be parsed later
                               Fill2=sector,
                               FillLabel2="Sector",
                               FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(sector));
# Add Local Data For Comparison
# Add data for Existing Variables & Years
dfCOL<-data.frame()
dfCOL<-df%>%filter(region=="Colombia",x==2010)%>%
  dplyr::select(-value,-NewValue,-scenario)%>%unique%>%
  mutate(scenario="LocalData",
         Query="A.Pinchao_ENA2014",
         NewValue= case_when((x==2010 & Fill2=="agriculture")~16.760,
                             (x==2010 & Fill2=="electricity")~6.465,
                             (x==2010 & Fill2=="industry")~.105,
                             (x==2010 & Fill2=="livestock")~3.049,
                             (x==2010 & Fill2=="mining")~1.233,
                             (x==2010 & Fill2=="municipal")~1.293,
                             TRUE ~ NA_real_),
         value=NewValue);
# Add new Factors if needed
#dfCOLx<-dfCOL[complete.cases(dfCOL),]
#dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2020, NewValue=75.338, value=NewValue))
df<-rbind.data.frame(df,dfCOL)
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

tbl <- getQuery(queryData.proj, "water withdrawals by sector") # Tibble
df<-as.data.frame(tbl);head(df)                # Data frame
df<-df%>%filter(region==region_i)
df<-df %>% mutate (vintage=paste("Vint_",year,sep=""),
                   Query="water withdrawals by sector",
                   Title="Water Withdrawals",
                   Fill1="Fill1",
                   FillLabel1="FillLabel1",
                   FillPalette1="colorsX_Unassigned",
                   Aggregate="sum",
                   segment="segment",
                   x=year,
                                 xLabel="Year",
                                 param="watWithdrawBySec",
                                 NewValue=value,
                                 NewUnits="Water~Withdrawals~(km^3)", # Use ~ for spaces. Will be parsed later
                                 Fill2=sector,
                                 FillLabel2="Sector",
                                 FillPalette2="colorsX_Unassigned") %>% 
  subset(.,select=-c(sector));
# Add Local Data For Comparison
# Add data for Existing Variables & Years
dfCOL<-data.frame()
dfCOL<-df%>%filter(region=="Colombia",x==2010)%>%
  dplyr::select(-value,-NewValue,-scenario)%>%unique%>%
  mutate(scenario="LocalData",
         Query="A.Pinchao_ENA2014",
         NewValue= case_when((x==2010 & Fill2=="agriculture")~16.760,
                             (x==2010 & Fill2=="electricity")~7.739,
                             (x==2010 & Fill2=="industry")~2.106,
                             (x==2010 & Fill2=="livestock")~3.049,
                             (x==2010 & Fill2=="mining")~1.233,
                             (x==2010 & Fill2=="municipal")~2.963,
                             TRUE ~ NA_real_),
         value=NewValue);
# Add new Factors if needed
#dfCOLx<-dfCOL[complete.cases(dfCOL),]
#dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2020, NewValue=75.338, value=NewValue))
df<-rbind.data.frame(df,dfCOL)
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

tbl <- getQuery(queryData.proj, "water withdrawals by crop") # Tibble
df<-as.data.frame(tbl);head(df)                # Data frame
df<-df%>%filter(region==region_i)
df<-df[df$sector!="industry" & df$sector!="mining" & df$sector!="municipal" 
       & df$sector!="electricity" & df$sector!="livestock",]
df<-df %>% mutate (vintage=paste("Vint_",year,sep=""),
                   Query="water withdrawals by crop",
                   Title="Water Withdrawals",
                   Fill1="Fill1",
                   FillLabel1="FillLabel1",
                   FillPalette1="colorsX_Unassigned",
                   Aggregate="sum",
                   segment="segment",
                   x=year,
                                  xLabel="Year",
                                  param="watWithdrawByCrop",
                                  NewValue=value,
                                  NewUnits="Water~Withdrawals~(km^3)", # Use ~ for spaces. Will be parsed later
                                  Fill2=sector,
                                  FillLabel2="Crop",
                                  FillPalette2="colorsX_Unassigned") %>% 
  subset(.,select=-c(sector));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

tbl <- getQuery(queryData.proj, "Ag Production by Crop Type") # Tibble
df<-as.data.frame(tbl);head(df)              # Data frame
df<-df%>%filter(region==region_i)
df<-df[df$sector==df$output,]  # So that biomass is not double counted
df1<-df %>% filter(Units=="EJ") %>% mutate (vintage=paste("Vint_",year,sep=""),
                     Query="Ag Production by Crop Type",
                     Title="Agricultural Production",
                     Fill1=sector,
                     FillLabel1="Sector",
                     FillPalette1="colorsX_Unassigned",
                     Aggregate="sum",
                     segment="segment",
                     x=year,
          xLabel="Year",
          param="agProdByCropBiomass",
          NewValue=value,
          NewUnits="Agricultural~Production~Biomass~(EJ)", # Use ~ for spaces. Will be parsed later
          Fill2=output,
          FillLabel2="Outputs",
          FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(output,sector));
df_allX<-rbind.data.frame(df_allX,df1); head(df_allX)

df1 <- df %>% filter(Units=="billion m3") %>% mutate (vintage=paste("Vint_",year,sep=""),
                                                                 Query="Ag Production by Crop Type",
                                                                 Title="Agricultural Production",
                                                                 Fill1=sector,
                                                                 FillLabel1="Sector",
                                                                 FillPalette1="colorsX_Unassigned",
                                                                 Aggregate="sum",
                                                                 segment="segment",
                                                     x=year,
          xLabel="Year",
          param="agProdByCropForest",
          NewValue=value,
          NewUnits="Agricultural~Production~Forest~(billion~m^3)", # Use ~ for spaces. Will be parsed later
          Fill2=output,
          FillLabel2="Outputs",
          FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(output,sector));
df_allX<-rbind.data.frame(df_allX,df1); head(df_allX)

df1 <- df %>% filter(Units=="Mt") %>% mutate (vintage=paste("Vint_",year,sep=""),
                                                      Query="Ag Production by Crop Type",
                                                      Title="Agricultural Production",
                                                      Fill1=sector,
                                                      FillLabel1="Sector",
                                                      FillPalette1="colorsX_Unassigned",
                                                      Aggregate="sum",
                                                      segment="segment",x=year,
          xLabel="Year",
          param="agProdByCrop",
          NewValue=value,
          NewUnits="Agricultural~Production~Crop~(Mt)", # Use ~ for spaces. Will be parsed later
          Fill2=output,
          FillLabel2="Outputs",
          FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(output,sector));
# Add Local Data For Comparison
# Add data for Existing Variables & Years
dfCOL<-data.frame()
dfCOL<-df1%>%filter(region=="Colombia",x>=2005,x<=2015)%>%
  dplyr::select(-value,-NewValue,-scenario)%>%unique%>%
  mutate(scenario="LocalData",
         Query="A.Pinchao_AgriculturalCensus",
         NewValue= case_when((x==2005 & Fill2=="Corn")~1.31,
                             (x==2005 & Fill2=="FiberCrop")~0.08,
                             (x==2005 & Fill2=="MiscCrop")~9.06,
                             (x==2005 & Fill2=="OilCrop")~0.10,
                             (x==2005 & Fill2=="OtherGrain")~0.44,
                             (x==2005 & Fill2=="PalmFruit")~3.24,
                             (x==2005 & Fill2=="Rice")~2.53,
                             (x==2005 & Fill2=="Root_Tuber")~4.99,
                             (x==2005 & Fill2=="SugarCrop")~21.75,
                             (x==2005 & Fill2=="Wheat")~0.05,
                             (x==2010 & Fill2=="Corn")~1.12,
                             (x==2010 & Fill2=="FiberCrop")~0.06,
                             (x==2010 & Fill2=="MiscCrop")~9.50,
                             (x==2010 & Fill2=="OilCrop")~0.12,
                             (x==2010 & Fill2=="OtherGrain")~0.30,
                             (x==2010 & Fill2=="PalmFruit")~3.76,
                             (x==2010 & Fill2=="Rice")~2.27,
                             (x==2010 & Fill2=="Root_Tuber")~5.32,
                             (x==2010 & Fill2=="SugarCrop")~20.24,
                             (x==2010 & Fill2=="Wheat")~0.02,
                             (x==2015 & Fill2=="Corn")~1.06,
                             (x==2015 & Fill2=="FiberCrop")~0.04,
                             (x==2015 & Fill2=="MiscCrop")~11.34,
                             (x==2015 & Fill2=="OilCrop")~0.17,
                             (x==2015 & Fill2=="OtherGrain")~0.27,
                             (x==2015 & Fill2=="PalmFruit")~6.08,
                             (x==2015 & Fill2=="Rice")~2.38,
                             (x==2015 & Fill2=="Root_Tuber")~6.26,
                             (x==2015 & Fill2=="SugarCrop")~24.77,
                             (x==2015 & Fill2=="Wheat")~0.04,
                             TRUE ~ NA_real_),
         value=NewValue);
# Add new Factors if needed
#dfCOLx<-dfCOL[complete.cases(dfCOL),]
#dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Total",x=2020, NewValue=75.338, value=NewValue))
df1<-rbind.data.frame(df1,dfCOL)
df_allX<-rbind.data.frame(df_allX,df1); head(df_allX)

#No Psature
df1<-df1%>%filter(Fill2!="Pasture")%>%mutate(param="agProdByCropNoPasture")
df_allX<-rbind.data.frame(df_allX,df1); head(df_allX)


tbl <- getQuery(queryData.proj, "water withdrawals by water mapping source") # Tibble
df<-as.data.frame(tbl);head(df)              # Data frame
df<-df%>%filter(region==region_i)
df<-df[grepl("_irr_",df$input),]  # Only keep irrigation water items
df$input<-gsub("water_td_irr_","",df$input);
df$input<-gsub("_W","",df$input);head(df)
df <- df %>% mutate (vintage=paste("Vint_",year,sep=""),
                                              Query="water withdrawals by water mapping source",
                                              Title="Water Withdrawals",
                                              Fill1="Fill1",
                                              FillLabel1="FillLabel1",
                                              FillPalette1="FillPalette1",
                                              Aggregate="sum",
                                              segment="segment",
                                              x=year,
                           xLabel="Year",
                           param="irrWatWithBasin",
                           NewValue=value,
                           NewUnits="Irrigation~Water~Withdrawal~(km^3)", # Use ~ for spaces. Will be parsed later
                           Fill2=input,
                           FillLabel2="Basin",
                           FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(input));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)


tbl <- getQuery(queryData.proj, "water consumption by water mapping source") # Tibble
df<-as.data.frame(tbl);head(df)              # Data frame
df<-df%>%filter(region==region_i)
df<-df[grepl("_irr_",df$input),]  # Only keep irrigation water items
df$input<-gsub("water_td_irr_","",df$input);
df$input<-gsub("_C","",df$input);head(df)
df <- df %>% mutate (vintage=paste("Vint_",year,sep=""),
                     Query="water consumption by water mapping source",
                     Title="Water Consumption",
                     Fill1="Fill1",
                     FillLabel1="FillLabel1",
                     FillPalette1="FillPalette1",
                     Aggregate="sum",
                     segment="segment",
                     x=year,
                           xLabel="Year",
                           param="irrWatConsBasin",
                           NewValue=value,
                           NewUnits="Irrigation~Water~Consumption~(km^3)", # Use ~ for spaces. Will be parsed later
                           Fill2=input,
                           FillLabel2="Basin",
                           FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(input));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

tbl <- getQuery(queryData.proj, "aggregated land allocation") # Tibble
df<-as.data.frame(tbl);head(df)              # Data frame
df<-df%>%filter(region==region_i)
df <- df %>% mutate (vintage=paste("Vint_",year,sep=""),
                     Query="aggregated land allocation",
                     Title="Land Allocation",
                     Fill1="Fill1",
                     FillLabel1="FillLabel1",
                     FillPalette1="FillPalette1",
                     Aggregate="sum",
                     segment="segment",
                     x=year,
                        xLabel="Year",
                        param="aggLandAlloc",
                        NewValue=value,
                        NewUnits="Land~Allocation~(km^2)", # Use ~ for spaces. Will be parsed later
                        Fill2=landleaf,
                        FillLabel2="Type",
                        FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(landleaf));
# Add Local Data For Comparison
# Add data for Existing Variables & Years
dfCOL<-data.frame()
dfCOL<-df%>%filter(region=="Colombia",x>=2005,x<=2015)%>%
  dplyr::select(-value,-NewValue,-scenario)%>%unique%>%
  mutate(scenario="LocalData",
         Query="A.Pinchao_AgriculturalCensus",
         NewValue= case_when((x==2010 & Fill2=="biomass")~0,
                             (x==2010 & Fill2=="crops")~45,
                             (x==2010 & Fill2=="forest (unmanaged)")~587,
                             (x==2010 & Fill2=="forest (managed)")~1.8,
                             (x==2010 & Fill2=="grass")~145,
                             (x==2010 & Fill2=="pasture (grazed)")~95,
                             (x==2010 & Fill2=="pasture (other)")~123,
                             (x==2010 & Fill2=="otherarable")~30,
                             (x==2010 & Fill2=="rock and desert")~9,
                             (x==2010 & Fill2=="shrubs")~65,
                             (x==2010 & Fill2=="urban")~4,
                             (x==2010 & Fill2=="tundra")~0,
                             TRUE ~ NA_real_),
         value=NewValue);
# Add new Factors if needed
dfCOLx<-dfCOL[complete.cases(dfCOL),]
dfCOL<-rbind.data.frame(dfCOLx,dfCOL[1,]%>%mutate(Fill2="Water",x=2010, NewValue=29, value=NewValue))
df<-rbind.data.frame(df,dfCOL)
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

tbl <- getQuery(queryData.proj, "Land Use Change Emission (future)") # Tibble
df<-as.data.frame(tbl);head(df)              # Data frame
df<-df%>%filter(region==region_i)
df <- df %>% mutate (vintage=paste("Vint_",year,sep=""),
                     Query="Land Use Change Emission (future)",
                     Title="LUC Emissions",
                     Fill1="Fill1",
                     FillLabel1="FillLabel1",
                     FillPalette1="colorsX_Unassigned",
                     Aggregate="sum",
                     segment="segment",
                     x=year,
                     xLabel="Year",
                     param="LUCemissFut",
                     NewValue=value*(GWP%>%filter(ghg=="CO2")%>%dplyr::select(GWPType))[1,1],
                     NewUnits="(MTCO2~Eq.)",  # Use ~ for spaces. Will be parsed later
                     Fill2=`land-use-change-emission`,
                     FillLabel2="Type",
                     FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(`land-use-change-emission`));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)
dfLUCEmissions<-df

if(FALSE){ # Removing for now because not interested in historical LUC Emissions
tbl <- getQuery(queryData.proj, "Land Use Change Emission") # Tibble
df<-as.data.frame(tbl);head(df)              # Data frame
df<-df%>%filter(region==region_i)
df <- df %>% mutate (vintage=paste("Vint_",year,sep=""),
                     Query="Land Use Change Emission",
                     Title="LUC Emissions",
                     Fill1="Fill1",
                     FillLabel1="FillLabel1",
                     FillPalette1="colorsX_Unassigned",
                     Aggregate="sum",
                     segment="segment",
                     x=year,
                     xLabel="Year",
                     param="LUCemiss",
                     NewValue=value*(GWP%>%filter(ghg=="CO2")%>%dplyr::select(GWPType))[1,1],
                     NewUnits="(MTCO2~Eq.)",  # Use ~ for spaces. Will be parsed later
                     Fill2=`land-use-change-emission`,
                     FillLabel2="Type",
                     FillPalette2="colorsX_Unassigned") %>%
  subset(.,select=-c(`land-use-change-emission`));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)
}

# CO2 Emissions
tbl <- getQuery(queryData.proj, "CO2 Emissions") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
df$technology<-"Technology"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"co2emission"; head(df) 
df$Query<-"CO2 Emissions"
df$Title<-"CO2 Emissions"
df$NewValue<-df$value*(GWP%>%filter(ghg=="CO2")%>%dplyr::select(GWPType))[1,1]
df$NewUnits<-"(MTCO2~Eq.)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=subsector,
                   FillLabel1="Subsector",
                   FillPalette1="colorsX_Unassigned",
                   Fill2=sector,
                   FillLabel2="Sector",
                   FillPalette2="colorsX_Unassigned") %>%  
  subset(.,select=-c(technology,sector,subsector));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

# CO2 Emissions by EndUSe
tbl <- getQuery(queryData.proj, "CO2 Emissions by enduse") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
df$technology<-"Technology"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"co2emissionByEndUse"; head(df) 
df$Query<-"CO2 Emissions"
df$Title<-"CO2 Emissions"
df$NewValue<-df$value*(GWP%>%filter(ghg=="CO2")%>%dplyr::select(GWPType))[1,1]
df$NewUnits<-"(MTCO2~Eq.)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Sector",
                   FillPalette1="colorsX_Unassigned",
                   Fill2=sector,
                   FillLabel2="Sector",
                   FillPalette2="colorsX_Unassigned") %>%  
  subset(.,select=-c(technology,sector));
# Add in Landuse Emissions
dfLUCAbs<-dfLUCEmissions%>%filter(NewValue<0)%>%mutate(Fill2="LUC_Absorption")
dfLUCAbs<-dfLUCAbs%>%group_by_at(vars(-value,-NewValue)) %>% summarize(NewValue = sum(NewValue,na.rm=T))%>%
  mutate(value=NewValue)%>%as.data.frame
dfLUCEmit<-dfLUCEmissions%>%filter(NewValue>0)%>%mutate(Fill2="LUC_Emissions")
dfLUCEmit<-dfLUCEmit%>%group_by_at(vars(-value,-NewValue)) %>% summarize(NewValue = sum(NewValue,na.rm=T))%>%
  mutate(value=NewValue)%>%as.data.frame
dfLUC<-rbind.data.frame(dfLUCAbs,dfLUCEmit);head(dfLUC) 
dfLUC$Title<-unique(df$Title);dfLUC$param<-unique(df$param);head(dfLUC) 
df<-rbind.data.frame(df,dfLUC)
# Add Local Data For Comparison
# Add data for Existing Variables & Years
dfCOL<-data.frame()
dfCOL<-df%>%filter(region=="Colombia",(x==2005 | x==2010))%>%
  dplyr::select(-value,-NewValue,-scenario)%>%unique%>%
  mutate(scenario="LocalData",
         Query="R.Delgado_UN",
         NewValue= case_when((x==2005 & Fill2=="industry")~7.217,
                             (x==2005 & Fill2=="LUC_Absorption")~-72.576,
                             (x==2005 & Fill2=="LUC_Emissions")~198.922,
                             (x==2010 & Fill2=="industry")~7.384,
                             (x==2010 & Fill2=="LUC_Absorption")~-77.697,
                             (x==2010 & Fill2=="LUC_Emissions")~187.405,
                             TRUE ~ NA_real_),
         value=NewValue);
# Add new Factors if needed
dfCOLx<-dfCOL[complete.cases(dfCOL),]
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="energy",x=2005, NewValue=56.873, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="waste",x=2005, NewValue=11.409, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="energy",x=2010, NewValue=73.634, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="waste",x=2010, NewValue=13.124, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="energy",x=2012, NewValue=78.015, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="industry",x=2012, NewValue=8.873, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="LUC_Absorption",x=2012, NewValue=-73.157, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="LUC_Emissions",x=2012, NewValue=158.597, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="waste",x=2012, NewValue=13.313, value=NewValue))
df<-rbind.data.frame(df,dfCOL)
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)

# GHG emissions by subsector
tbl <- getQuery(queryData.proj, "GHG emissions by subsector") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
df<-df%>%filter(region==region_i)
df$technology<-"Technology"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$param<-"ghgEmissionByEndUseBySectorByGHG"; head(df) 
df$Query<-"GHG Emissions"
df$Title<-"GHG Emissions"
df<-join(df,GWP%>%dplyr::select(ghg,GWPType),by="ghg");
df<-join(df,convertGgTgMTC,by="Units");head(df)
df<-df%>%mutate(NewValue=value*get(GWPType)*Convert)%>%
  dplyr::select(-c(get(GWPType),Convert))
df$NewUnits<-"(MTCO2~Eq.)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-df$subsector
df<-df %>% mutate (Fill1=sector,
                   FillLabel1="Sector",
                   FillPalette1="colorsX_Unassigned",
                   Fill2=ghg,
                   FillLabel2="GHG",
                   FillPalette2="colorsX_Unassigned") %>%  
  subset(.,select=-c(technology,sector,subsector,ghg));
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)
dfGHGEndUSeSector<-df

# GHG emissions by GHG CO2, CH4,N2O,HFCs,SF6
df<-dfGHGEndUSeSector
df<-df%>%filter(region==region_i)
df1<-df %>% mutate(Fill2= case_when ((grepl("HFC", Fill2)) ~ "HFCs",
                                    (grepl("SF6", Fill2)) ~ "SF6",
                                    (grepl("CO2", Fill2)) ~ "CO2",
                                    (grepl("N2O", Fill2)) ~ "N2O",
                                    (grepl("CH4", Fill2)) ~ "CH4",
                                    TRUE ~ "Other"),
                   param="ghgEmissByGHGGROUPS");
# Add in Landuse Emissions
df1<-df1%>%group_by_at(vars(-value,-NewValue)) %>% summarize(NewValue = sum(NewValue,na.rm=T))%>%
  mutate(value=NewValue)%>%as.data.frame
sum(df$NewValue,na.rm=T);sum(df1$NewValue,na.rm=T)
df<-df1
# Add Local Data For Comparison
# Add data for Existing Variables & Years
dfCOL<-data.frame()
dfCOL<-df%>%filter(region=="Colombia",(x==2005 | x==2010))%>%
  dplyr::select(-value,-NewValue,-scenario,-segment,-Fill1,-FillLabel1)%>%unique%>%
  mutate(scenario="LocalData",
         Query="R.Delgado_UN",
         NewValue= case_when((x==2005 & Fill2=="CO2")~201.845,
                             (x==2005 & Fill2=="CH4")~35,
                             (x==2005 & Fill2=="N2O")~20,
                             (x==2005 & Fill2=="HFCs")~1,
                             (x==2005 & Fill2=="SF6")~5,
                             (x==2010 & Fill2=="CO2")~203.85,
                             (x==2010 & Fill2=="CH4")~35,
                             (x==2010 & Fill2=="N2O")~20,
                             (x==2010 & Fill2=="HFCs")~2,
                             (x==2010 & Fill2=="SF6")~2,
                             TRUE ~ NA_real_),
         value=NewValue);
# Add new Factors if needed
dfCOLx<-dfCOL[complete.cases(dfCOL),]
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="CO2",x=2012, NewValue=185.64, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="CH4",x=2012, NewValue=35, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="N2O",x=2012, NewValue=20, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="HFCs",x=2012, NewValue=2, value=NewValue))
dfCOL<-rbind.data.frame(dfCOL,dfCOLx[1,]%>%mutate(Fill2="SF6",x=2012, NewValue=2, value=NewValue))
dfCOL<-dfCOL%>%mutate(Fill1="Fill1",FillLabel1="FillLabel1",segment="segment")
df<-rbind.data.frame(df,dfCOL)
df_allX<-rbind.data.frame(df_allX,df); head(df_allX)
rm(df1)

#--------------------------
#--------------------------

# Check a param
#head(df_allX[df_allX$param=="elecCapRetirements" & df_allX$NewValue>0,])

# Limit to selected analysis range and regions
df_allX<-df_allX[df_allX$x<=max(range(rangeX)),]
df_allX<-df_allX[df_allX$x>=min(range(rangeX)),]

df_allX<-df_allX%>%filter(region==region_i)


#
df_allOrig<-df_allX

df_allOrig<<-df_allOrig%>%mutate(
  scenario = case_when ((scenario == scenNameRefOrig) ~ scenNameRef,
                        (scenario == scenName1Orig) ~ scenName1,
                        (scenario == scenName2Orig) ~ scenName2,
                        (scenario == scenName3Orig) ~ scenName3,
                        TRUE ~ scenario),
  segment = gsub(" electricity","",segment))
scenariosComp<<-scenariosComp%>%gsub(paste("\\<",scenNameRefOrig,"\\>",sep=""),scenNameRef,.)%>%
  gsub(paste("\\<",scenName1Orig,"\\>",sep=""),scenName1,.)%>%
  gsub(paste("\\<",scenName2Orig,"\\>",sep=""),scenName2,.)%>%
  gsub(paste("\\<",scenName3Orig,"\\>",sep=""),scenName3,.);
scenariosIndv<<-scenariosIndv%>%gsub(paste("\\<",scenNameRefOrig,"\\>",sep=""),scenNameRef,.)%>%
  gsub(paste("\\<",scenName1Orig,"\\>",sep=""),scenName1,.)%>%
  gsub(paste("\\<",scenName2Orig,"\\>",sep=""),scenName2,.)%>%
  gsub(paste("\\<",scenName3Orig,"\\>",sep=""),scenName3,.);
head(df_allOrig)

df_allOrig$scenario<-as.factor(df_allOrig$scenario)
df_allOrig$scenario <- factor( as.character(df_allOrig$scenario), levels=c(scenNameRef,scenName1,scenName2,scenName3) );
df_allOrig<-df_allOrig[order(df_allOrig$scenario),];
df_allOrig<-droplevels(df_allOrig);levels(df_allOrig$scenario)


rm(); gc()


# Automatic Selection of all parameters
params<-unique(df_allOrig$param); # List of all params
params
}

#----------------

# Create Folders
if(!dir.exists(paste(wdfigsOut,"/",region_i,sep=""))){dir.create(paste(wdfigsOut,"/",region_i,sep=""))}


if(runDiffPlots==1){

#______________________
# Diff Plots and Comparisons
#______________________

#region_i<-regions[1];region_i;scenario_i<-scenariosIndv[1];scenario_i;   #For testing purposes

if(!dir.exists(paste(wdfigsOut,"/",region_i,"/DiffPlots",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/DiffPlots",sep=""))}
dir<-paste(wdfigsOut,"/",region_i,"/DiffPlots",sep="")


  for(paramx in paramsDiff){
    
    
    df_all<-df_allOrig
    
    #----------
    # Line Charts
    #-----------    
  
    l1<-df_all%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate,NewUnits)  # For National, by techs
    l1<-l1%>%filter(param==paramx & region==region_i); head(l1)
  
    l1$scenario<-as.factor(l1$scenario)
    l1$scenario <- factor( as.character(l1$scenario), levels=c(scenariosComp) );
    l1<-l1[order(l1$scenario),];
    
    
    if(nrow(l1)!=0){
      
      if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
      if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
      l1<-rbind.data.frame(xsum,xmean);head(l1)
      
      
       p <- fig_LineCompareScenario(l1) + if(titleOn==1){ggtitle (paste(scenario_ix,sep=""))}else{ggtitle(NULL)} 
      plot(p)
      
      fname<-paste(paramx,"_LinesComp_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.2,figHeight_InchMaster*1.2,pdfpng=pdfpng)
      
      
    } # Close if empty rows 
    
    
    #----------
    # Bar Charts
    #-----------
    
    l1<-df_all
    # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
    l1$Fill<-l1$Fill2;
    l1$FillLabel<-l1$FillLabel2 
    l1$FillPalette<-l1$FillPalette2
    l1<-l1%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                           Fill,FillLabel,FillPalette)  # For National, by techs
    l1<-l1%>%filter(param==paramx); head(l1)
    
    if(nrow(l1)!=0){
      
      if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
      if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
      l1<-rbind.data.frame(xsum,xmean);head(l1)
      l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
      
      
      l1$scenario<-as.factor(l1$scenario)
      l1$scenario <- factor( as.character(l1$scenario), levels=c(scenariosComp) );
      l1<-l1[order(l1$scenario),];
      
      
      p <- fig_Bar(l1) + facet_wrap(~scenario)
      plot(p)
      fname<-paste(paramx,"_BarComp_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.2,figHeight_InchMaster*1,pdfpng=pdfpng)
      
      fname<-paste(paramx,"_barDodgeTotal_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")  
      p <- fig_BarDodgeScenario(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
      plot(p)
      print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
      
      p <- fig_LineMultiple(l1) + facet_wrap(~scenario)
      plot(p)
      fname<-paste(paramx,"_LinesFacetComp_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.2,figHeight_InchMaster*1.2,pdfpng=pdfpng)
      
      
        t1<-subset(l1,select=c(scenario,region,x,NewUnits,NewValue,Fill));head(t1)
        t1$NewUnits<-t1$NewUnits%>%gsub("~"," ",.)%>%gsub("\\^","",.);head(t1)
        t1<-dcast(t1,Fill+region+x+NewUnits~scenario,value.var="NewValue",fun.aggregate=sum,na.rm=T);head(t1)
        colnames(t1)[which(names(t1) == "NewUnits")] <- "Parameter"
        colnames(t1)[which(names(t1) == "x")] <- "Year"
        write.csv(t1,file=paste(dir,"/",paramx,"_tableComp_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),".csv",sep=""),row.names=F)
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
        tref<-t1%>%dplyr::select(region,Year,Parameter,Fill,scenNameRef)
      
    }
    
    #----------
    # Bar Charts Diff
    #-----------
    
    l1<-df_all[(df_all$param==paramx),];head(l1)
    # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
    l1$Fill<-l1$Fill2;
    l1$FillLabel<-l1$FillLabel2 
    l1$FillPalette<-l1$FillPalette2
    l1<-l1%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                           Fill,FillLabel,FillPalette)  # For National, by techs
    head(l1)
    
    
    if(nrow(l1)!=0){
      
      if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
      if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
      l1<-rbind.data.frame(xsum,xmean);head(l1)
      l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
      
      
      lx<-l1%>%spread(scenario,NewValue)#%>%replace(is.na(.), 0)
      if(scenName1 %in% unique(l1$scenario)){lx<-lx%>%  
        mutate(!!paste(scenName1,"_diff",sep=""):=get(scenName1)-get(scenNameRef))%>%dplyr::select(-c(scenName1))}
      if(scenName2 %in% unique(l1$scenario)){lx<-lx%>%
        mutate(!!paste(scenName2,"_diff",sep=""):=get(scenName2)-get(scenNameRef))%>%dplyr::select(-c(scenName2))}
      if(scenName3 %in% unique(l1$scenario)){lx<-lx%>%
        mutate(!!paste(scenName3,"_diff",sep=""):=get(scenName3)-get(scenNameRef))%>%dplyr::select(-c(scenName3))}
      lx<-lx%>%gather(key=scenario,value=NewValue,-param,-region,
               -x,-xLabel,-Aggregate,-NewUnits,-Fill,-FillLabel,-FillPalette);head(lx)
      
      lx$scenario<-lx$scenario%>%gsub("_diff","",.);head(lx)
      lx<-lx%>%filter(scenario != scenNameRef);lx<-droplevels(lx)
      
      lx$scenario<-as.factor(lx$scenario)
      lx$scenario <- factor( as.character(lx$scenario), levels=c(scenariosComp) );
      lx<-lx[order(lx$scenario),];
      
      l2<-lx
      
      fname<-paste(paramx,"_barDiff_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        p <- fig_Bar(l2) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
        p <- p+ facet_grid(~scenario)
        plot(p)
        print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster,pdfpng=pdfpng)
        
      fname<-paste(paramx,"_linesDiff_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
          p <- fig_LineMultiple(l2) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          p <- p+ facet_grid(~scenario)
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster,pdfpng=pdfpng)
          
      fname<-paste(paramx,"_barDiffDodgeFacetFillFree_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")  
          p <- fig_BarDodgeScenario(l2) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          p <- p + facet_wrap(~Fill,scales="free_y")
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1.5,pdfpng=pdfpng)
          
     fname<-paste(paramx,"_barDiffDodgeFacetFill_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")  
          p <- fig_BarDodgeScenario(l2) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          p <- p + facet_wrap(~Fill)
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1.5,pdfpng=pdfpng)
          
          
      l3<-l2  
      l3<-l3%>%dplyr::select(-Fill,-FillLabel,-FillPalette)%>%group_by_at(vars(-NewValue)) %>% summarize(NewValue = sum(NewValue,na.rm=T))
      fname<-paste(paramx,"_barDiffDodgeTotal_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")  
          p <- fig_BarDodgeScenario(l3) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          p <- p + scale_fill_manual(values=colorsX_Basic[2:length(colorsX_Basic)],name="Scenario")
          p <- p
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster,pdfpng=pdfpng)
          
        
        t1<-subset(l2,select=c(scenario,region,x,NewUnits,NewValue,Fill));head(t1)
        t1$NewUnits<-t1$NewUnits%>%gsub("~"," ",.)%>%gsub("\\^","",.);head(t1)
        t1<-dcast(t1,Fill+region+x+NewUnits~scenario,value.var="NewValue",fun.aggregate=sum,na.rm=T);head(t1)
        colnames(t1)[which(names(t1) == "NewUnits")] <- "Parameter"
        colnames(t1)[which(names(t1) == "x")] <- "Year"
        t1<-join(t1,tref);head(t1)
        colnames(t1)[which(names(t1) == scenNameRef)] <- paste(scenNameRef,"_Value",sep="")
        write.csv(t1,file=paste(dir,"/",paramx,"_tableDiff_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),".csv",sep=""),row.names=F)
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
        
      
      } # close if selected params
    }# If nrow
    
    
    #----------
    # Bar Charts Prcnt
    #-----------
    
    l1<-df_all[(df_all$param==paramx),];head(l1)
    # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
    l1$Fill<-l1$Fill2;
    l1$FillLabel<-l1$FillLabel2 
    l1$FillPalette<-l1$FillPalette2
    l1<-l1%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                           Fill,FillLabel,FillPalette)  # For National, by techs
    head(l1)
    
    
    if(nrow(l1)!=0){
      
      
      if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
      if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
      l1<-rbind.data.frame(xsum,xmean);head(l1)
      l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
      
      
      lx<-l1%>%spread(scenario,NewValue)#%>%replace(is.na(.), 0)
      if(scenName1 %in% unique(l1$scenario)){lx<-lx%>%   
        mutate(!!paste(scenName1,"_prcnt",sep=""):=((get(scenName1)-get(scenNameRef))*100/get(scenNameRef)))%>%dplyr::select(-c(scenName1))}
      if(scenName2 %in% unique(l1$scenario)){lx<-lx%>% 
        mutate(!!paste(scenName2,"_prcnt",sep=""):=((get(scenName2)-get(scenNameRef))*100/get(scenNameRef)))%>%dplyr::select(-c(scenName2))}
      if(scenName3 %in% unique(l1$scenario)){lx<-lx%>% 
        mutate(!!paste(scenName3,"_prcnt",sep=""):=((get(scenName3)-get(scenNameRef))*100/get(scenNameRef)))%>%dplyr::select(-c(scenName3))}
      lx<-lx%>%gather(key=scenario,value=NewValue,-param,-region,
                      -x,-xLabel,-Aggregate,-NewUnits,-Fill,-FillLabel,-FillPalette);head(lx)
      
      lx$scenario<-lx$scenario%>%gsub("_prcnt","",.);head(lx)
      lx<-lx%>%filter(scenario != scenNameRef);lx<-droplevels(lx)
      
      lx$scenario<-as.factor(lx$scenario)
      lx$scenario <- factor( as.character(lx$scenario), levels=c(scenariosComp) );
      lx<-lx[order(lx$scenario),];
      
      l2<-lx
      
      fname<-paste(paramx,"_barDiffPrcnt_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        p <- fig_Bar(l2) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
        p <- p+ facet_grid(~scenario) +  ylab("% Difference")
        plot(p)
        print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster,pdfpng=pdfpng)
        
      fname<-paste(paramx,"_LinesDiffPrcnt_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
          p <- fig_LineMultiple(l2) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          p <- p+ facet_grid(~scenario) +  ylab("% Difference")
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster,pdfpng=pdfpng)

          
      fname<-paste(paramx,"_barDiffPrcntDodgeFacetFillFree_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")  
          p <- fig_BarDodgeScenario(l2) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          p <- p + facet_wrap(~Fill,scales="free_y") + ylab("% Difference")
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1.5,pdfpng=pdfpng)
          
      fname<-paste(paramx,"_barDiffPrcntDodgeFacetFill_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")  
          p <- fig_BarDodgeScenario(l2) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          p <- p + facet_wrap(~Fill) + ylab("% Difference")
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1.5,pdfpng=pdfpng)
        
      l3<-l2  
      l3<-l3%>%dplyr::select(-Fill,-FillLabel,-FillPalette)%>%group_by_at(vars(-NewValue)) %>% summarize(NewValue = sum(NewValue,na.rm=T))
      fname<-paste(paramx,"_barDiffPrcntDodgeTotal_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")  
          p <- fig_BarDodgeScenario(l3) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          p <- p + ylab("% Difference")
          p <- p + scale_fill_manual(values=colorsX_Basic[2:length(colorsX_Basic)],name="Scenario")
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster,pdfpng=pdfpng)
        
        t1<-subset(l2,select=c(scenario,region,x,NewUnits,NewValue,Fill));head(t1)
        t1$NewUnits<-t1$NewUnits%>%gsub("~"," ",.)%>%gsub("\\^","",.);head(t1)
        t1<-dcast(t1,Fill+region+x+NewUnits~scenario,value.var="NewValue",fun.aggregate=sum,na.rm=T);head(t1)
        colnames(t1)[which(names(t1) == "NewUnits")] <- "Parameter"
        colnames(t1)[which(names(t1) == "x")] <- "Year"
        t1<-join(t1,tref);head(t1)
        colnames(t1)[which(names(t1) == scenNameRef)] <- paste(scenNameRef,"_Value",sep="")
        t1$Paramter<-"Percentage Difference"
        write.csv(t1,file=paste(dir,"/",paramx,"_tableDiffPrcnt_GCAM_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),".csv",sep=""),row.names=F)
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
        
      }
    }
    
      
  } # Close Params
 
}# Close if Run DiffPlots


#______________________
# Start Regional and Scenario Loops
#______________________

#region_i<-regions[1];region_i;scenario_i<-scenariosIndv[1];scenario_i;   #For testing purposes

for(scenario_i in scenariosIndv){
  
  scenario_ix<-case_when(scenario_i==scenNameRef~scenNameRefOrig,
                         scenario_i==scenName1~scenName1Orig,
                         scenario_i==scenName2~scenName2Orig,
                         scenario_i==scenName3~scenName3Orig); scenario_ix
  
 
# Create Folders
if(!dir.exists(paste(wdfigsOut,"/",region_i,sep=""))){dir.create(paste(wdfigsOut,"/",region_i,sep=""))}
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,sep=""))}
dir<-paste(wdfigsOut,"/",region_i,"/",scenario_ix,sep="")


#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# GCAM Charts
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________

    
    if(runGCAMCharts==1){
      
   
      
      # Create Output Directory
      if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/GCAMCharts",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/GCAMCharts",sep=""))}
      dir<-paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/GCAMCharts",sep="")
      
  
   
    #______________________
    # Plots
    #______________________
    
    
    for(paramx in params){
     
      df_all<-df_allOrig%>%filter(scenario==scenario_i)
      
      #----------
      # Line Charts
      #-----------    
      
      l1<-df_all%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill1)  # For National, by techs
      l1<-l1%>%filter(param==paramx & region==region_i); head(l1)
      
      if(nrow(l1)!=0){
        
        if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
        if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
        l1<-rbind.data.frame(xsum,xmean);head(l1)
        
        
        fname<-paste("line_GCAM_",paramx,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          p <- fig_LineSingle(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)}  
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*0.7,figHeight_InchMaster,pdfpng=pdfpng)
          t1<-subset(l1,select=c(scenario,region,x,NewUnits,NewValue));head(t1)
          t1$NewUnits<-t1$NewUnits%>%gsub("~"," ",.)%>%gsub("\\^","",.);head(t1)
          colnames(t1)[which(names(t1) == "NewUnits")] <- "Parameter"
          colnames(t1)[which(names(t1) == "x")] <- "Year"
          write.csv(t1,file=paste(dir,"/table_GCAM_",region_i,"_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),".csv",sep=""),row.names=F)
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))  
      }
        
      } # Close if empty rows 
      
      
      #----------
      # Bar Charts
      #-----------
      
      l1<-df_all
      # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
      l1$Fill<-l1$Fill2;
      l1$FillLabel<-l1$FillLabel2 
      l1$FillPalette<-l1$FillPalette2
      l1<-l1%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                             Fill,FillLabel,FillPalette)  # For National, by techs
      l1<-l1%>%filter(param==paramx); head(l1)
      
      if(nrow(l1)!=0){
        
        if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
        if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
        l1<-rbind.data.frame(xsum,xmean);head(l1)
        l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
          
      
        fname<-paste("bar_GCAM_",paramx,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
          
        fname<-paste("linesMulti_GCAM_",paramx,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
          p <- fig_LineMultiple(l1)
          plot(p)
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
          
          t1<-subset(l1,select=c(scenario,region,x,NewUnits,NewValue,Fill));head(t1)
          t1$NewUnits<-t1$NewUnits%>%gsub("~"," ",.)%>%gsub("\\^","",.);head(t1)
          t1<-dcast(t1,scenario+region+x+NewUnits~Fill,value.var="NewValue",fun.aggregate=sum,na.rm=T);head(t1)
          colnames(t1)[which(names(t1) == "NewUnits")] <- "Parameter"
          colnames(t1)[which(names(t1) == "x")] <- "Year"
          write.csv(t1,file=paste(dir,"/table_GCAM_",region_i,"_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),".csv",sep=""),row.names=F)
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
        
      }# if nrow 0
      
      

    } #Close GCAM params
 
# Special plots for particular presentations
if(FALSE) {
  
  df_all<-df_allOrig
  
  df_all<-df_allOrig%>%filter(scenario==scenario_i)
  l1<-df_all
  
  # For plotting Final & Primary Energy on same scale
  param<-"primNrgConsumByFuel"
  l1<-df_all[(df_all$param==param),]; head(l1)
  # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
  l1$Fill<-l1$Fill2;
  l1$FillLabel<-l1$FillLabel2 
  l1$FillPalette<-l1$FillPalette2
  l1_sum<-aggregate(NewValue ~ Units+scenario+region+year+param+NewUnits, l1, sum) # To get sums over years
  p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
  p <- p + coord_cartesian(ylim=c(0,signif(max(l1_sum$NewValue)*1.1,0)))
  plot(p)
  fname<-paste("SameScale1_bar_GCAM_",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
  print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
  lx_sum<-l1_sum
  
  param<-"totFinalNrgByEndUse"
  l1<-df_bar[(df_bar$param==param & df_bar$region==region_i),]; head(l1)
  l1_sum<-aggregate(NewValue ~ Units+scenario+region+year+param+NewUnits, l1, sum) # To get sums over years
    p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)}
    p <- p + coord_cartesian(ylim=c(0,signif(max(lx_sum$NewValue)*1.1,0)))
  plot(p)
  fname<-paste("SameScale1_bar_GCAM",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
  print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
  
  # For plotting Water withdrawals and consumption on same scale
  param<-"watWithdrawBySec"
  l1<-df_bar[(df_bar$param==param & df_bar$region==region_i),]; head(l1)
  l1_sum<-aggregate(NewValue ~ Units+scenario+region+year+param+NewUnits, l1, sum) # To get sums over years
  p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
  p <- p + coord_cartesian(ylim=c(0,signif(max(l1_sum$NewValue)*1.1,0)))
  plot(p)
  fname<-paste("SameScale2_bar_GCAM",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
  print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
  lx_sum<-l1_sum
  
  param<-"watConsumBySec"
  l1<-df_bar[(df_bar$param==param & df_bar$region==region_i),]; head(l1)
  l1_sum<-aggregate(NewValue ~ Units+scenario+region+year+param+NewUnits, l1, sum) # To get sums over years
  p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)}
  p <- p + coord_cartesian(ylim=c(0,signif(max(lx_sum$NewValue)*1.1,0)))
  plot(p)
  fname<-paste("SameScale2_bar_GCAM",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
  print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
  
  # For plotting Water withdrawals and consumption on same scale
  param<-"irrWatWithBasin"
  l1<-df_bar[(df_bar$param==param & df_bar$region==region_i),]; head(l1)
  l1_sum<-aggregate(NewValue ~ Units+scenario+region+year+param+NewUnits, l1, sum) # To get sums over years
  p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
  p <- p + coord_cartesian(ylim=c(0,signif(max(l1_sum$NewValue)*1.1,0)))
  plot(p)
  fname<-paste("SameScale3_bar_GCAM",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
  print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
  lx_sum<-l1_sum
  
  param<-"irrWatConsBasin"
  l1<-df_bar[(df_bar$param==param & df_bar$region==region_i),]; head(l1)
  l1_sum<-aggregate(NewValue ~ Units+scenario+region+year+param+NewUnits, l1, sum) # To get sums over years
  p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)}
  p <- p + coord_cartesian(ylim=c(0,signif(max(lx_sum$NewValue)*1.1,0)))
  plot(p)
  fname<-paste("SameScale3_bar_GCAM",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
  print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
  
  # In Ag production by crop without Pature or Forest
  param<-"agProdByCrop"
  l1<-df_bar[(df_bar$param==param & df_bar$region==region_i),]; head(l1)
  l1<-l1[(l1$Fill!="Forest" & l1$Fill!="Pasture"),]
  p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
  plot(p)
  fname<-paste("Edit1_AgbyCropNoForestPasture_bar_GCAM_",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
  print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
}
    } # Close GCAM if GCAM runs


#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Tethys Water Demand Data
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________

if(runTethysMaps==1){

# Create Output Directory
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/TethysWatDem",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/TethysWatDem",sep=""))}
dir<-paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/TethysWatDem",sep="")
# subBasin directory
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/TethysWatDem/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/TethysWatDem/subBasin",sep=""))}


wdtethys1<-paste(wdtethys,scenario_ix,sep="") # Tethys data directory

df<-data.frame()
# Read in Data Files
df_dom <- read.csv(paste(wdtethys1,"/wddom.csv",sep=""), stringsAsFactors = F)
df_dom$Type<-"Domestic"; df<-rbind.data.frame(df,df_dom);
df_elec <- read.csv(paste(wdtethys1,"/wdelec.csv",sep=""), stringsAsFactors = F)
df_elec$Type<-"Electric"; df<-rbind.data.frame(df,df_elec);
df_irr <- read.csv(paste(wdtethys1,"/wdirr.csv",sep=""), stringsAsFactors = F)
df_irr$Type<-"Irrigation"; df<-rbind.data.frame(df,df_irr);
df_liv <- read.csv(paste(wdtethys1,"/wdliv.csv",sep=""), stringsAsFactors = F)
df_liv$Type<-"Livestock"; df<-rbind.data.frame(df,df_liv);
df_mfg <- read.csv(paste(wdtethys1,"/wdmfg.csv",sep=""), stringsAsFactors = F)
df_mfg$Type<-"MFG"; df<-rbind.data.frame(df,df_mfg);
df_min <- read.csv(paste(wdtethys1,"/wdmin.csv",sep=""), stringsAsFactors = F)
df_min$Type<-"Mining"; df<-rbind.data.frame(df,df_min);
df_noag <- read.csv(paste(wdtethys1,"/wdnonag.csv",sep=""), stringsAsFactors = F)
df_noag$Type<-"Non_Agriculture"; df<-rbind.data.frame(df,df_noag);
df_total <- read.csv(paste(wdtethys1,"/wdtotal.csv",sep=""), stringsAsFactors = F)
df_total$Type<-"Total";df<-rbind.data.frame(df,df_total);
head(df)

colnames(df)[which(names(df) == "latitude")] <- "lat"
colnames(df)[which(names(df) == "longitude")] <- "lon"
names(df)<-gsub("x","X",names(df),ignore.case=F)
head(df)

df<-df%>%dplyr::select(lat,lon,yearsX,Type)

years_tethys<-grep("X",names(df),value=T)[grep("X",names(df),value=T)!="X..ID"];years_tethys
years_tethysX<-sub("X","",years_tethys);years_tethysX

df$Mean<-rowMeans(df%>%dplyr::select(years_tethys))
colnames(df)[which(names(df) == "Mean")] <- paste("Mean_",min(years_tethysX),"to",max(years_tethysX),sep="")
head(df)

dftethys<-df

years<-c(years_tethys,paste("Mean_",min(years_tethysX),"to",max(years_tethysX),sep=""))

#------------------------------
#--- Run spatial re-aggregation function maps_ReAggregate
#--- Set paramters for function
#------------------------------

# Setup for running function maps_ReAggregaget
moduleName<-"tethys"  # Name of module being run tethys, xanthos, scarcity etc.
moduleParam<-"demWatmmPerYr"  # Type of parameter for fig names demWatmmPerYr
moduleUnits<- "mm"            # Units used for figures
moduleTitleText<- "Water Demands (mm)"  # Title for figures. Used when titleOn is 1
moduleAggType<- "depth"      # "depth" when using mm or "vol" when using km3
moduleRemove<-c("Total","Non_Agriculture")   # Remove certain categories for particular modules. Make NULL if not needed
digitsMaps<<-0 # Digits for Legend

maps_ReAggregate(region_i=region_i,scenario_i=scenario_ix,
                 moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                 moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                 moduleRemove=moduleRemove)



} # Close Tethys Run
#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Demeter Land Cover Fractional
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________

if(runDemeterMaps==1){
  
  # Create Output Directory
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/DemeterLandUse",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/DemeterLandUse",sep=""))}
  dir<-paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/DemeterLandUse",sep="")
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/DemeterLandUse/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/DemeterLandUse/subBasin",sep=""))}
  
  wdDemeter1<-paste(wdDemeter,scenario_ix,"/spatial_landcover_tabular",sep="") # Demeter data directory
  
  df<-data.frame()
  # Read in Data Files
  df_crops <- read.csv(paste(wdDemeter1,"/crops.csv",sep=""), stringsAsFactors = F)
  df_crops$Type<-"Crops"; df<-rbind.data.frame(df,df_crops);
  df_forest <- read.csv(paste(wdDemeter1,"/forest.csv",sep=""), stringsAsFactors = F)
  df_forest$Type<-"Forest"; df<-rbind.data.frame(df,df_forest);
  df_grass <- read.csv(paste(wdDemeter1,"/grass.csv",sep=""), stringsAsFactors = F)
  df_grass$Type<-"Pasture"; df<-rbind.data.frame(df,df_grass);
  df_shrub <- read.csv(paste(wdDemeter1,"/shrub.csv",sep=""), stringsAsFactors = F)
  df_shrub$Type<-"Shrub"; df<-rbind.data.frame(df,df_shrub);
  df_snow <- read.csv(paste(wdDemeter1,"/snow.csv",sep=""), stringsAsFactors = F)
  df_snow$Type<-"Snow"; df<-rbind.data.frame(df,df_snow);
  df_sparse <- read.csv(paste(wdDemeter1,"/sparse.csv",sep=""), stringsAsFactors = F)
  df_sparse$Type<-"Sparse"; df<-rbind.data.frame(df,df_sparse);
  df_urban <- read.csv(paste(wdDemeter1,"/urban.csv",sep=""), stringsAsFactors = F)
  df_urban$Type<-"Urban"; df<-rbind.data.frame(df,df_urban);
  df_water <- read.csv(paste(wdDemeter1,"/water.csv",sep=""), stringsAsFactors = F)
  df_water$Type<-"Water";df<-rbind.data.frame(df,df_water);
  head(df)
  
  colnames(df)[which(names(df) == "latitude")] <- "lat"
  colnames(df)[which(names(df) == "longitude")] <- "lon"
  names(df)<-gsub("x","X",names(df),ignore.case=F)
  head(df)
  
  df<-df%>%dplyr::select(lat,lon,yearsX,Type)
  
  years_demeter<-grep("X",names(df),value=T)[grep("X",names(df),value=T)!="X..ID"];years_demeter
  years_demeterX<-sub("X","",years_demeter);years_demeterX
  
  df$Mean<-rowMeans(df%>%dplyr::select(years_demeter))
  colnames(df)[which(names(df) == "Mean")] <- paste("Mean_",min(years_demeterX),"to",max(years_demeterX),sep="")
  head(df)
  
  years<-c(years_demeter,paste("Mean_",min(years_demeterX),"to",max(years_demeterX),sep=""))
  
#------------------------------
#--- Run spatial re-aggregation function maps_ReAggregate
#--- Set paramters for function
#------------------------------  
  
  # Setup for running function maps_ReAggregaget
  moduleName<-"demeter"  # Name of module being run tethys, xanthos, scarcity etc.
  moduleParam<-"landcovFract"  # Type of parameter for fig names demWatmmPerYr
  moduleUnits<- "fraction"            # Units used for figures
  moduleTitleText<- "Land Cover (Fractional)"  # Title for figures. Used when titleOn is 1
  moduleAggType<- "depth"      # "depth" when using mm or "vol" when using km3
  moduleRemove<-NULL   # Remove certain categories for particular modules. Make NULL if not needed
  
  maps_ReAggregate(region_i=region_i,scenario_i=scenario_ix,
                   moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                   moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                   moduleRemove=moduleRemove)
  
} # Close Demeter Run
  

#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Xanthos Water Supply
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________


if(runXanthosMaps==1){
  
  # Setup for running function maps_ReAggregaget
  moduleName<-"xanthos"  # Name of module being run tethys, xanthos, scarcity etc.
  moduleParam<-"watSup"  # Type of parameter for fig names eg demWatmmPerYr
  moduleUnits<- "mm"            # Units used for figures
  moduleTitleText<- "Water Supply (mm)"  # Title for figures. Used when titleOn is 1
  moduleAggType<- "depth"      # "depth" when using mm or "vol" when using km3
  moduleRemove<-NULL   # Remove certain categories for particular modules. Make NULL if not needed
  
  # Create Output Directory
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/XanthosWatSup",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/XanthosWatSup",sep=""))}
  dir<-paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/XanthosWatSup",sep="")
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/XanthosWatSup/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/XanthosWatSup/subBasin",sep=""))}
  
  wdXanthos1<-paste(wdXanthos,sep="") # Xanthos data directory
  
  xanthosCoords<-read.csv(paste(dirname(wdXanthos1),"/input/reference/coordinates.csv",sep=""), header=F, stringsAsFactors = F); head(xanthosCoords)
  xanthosCoords<-xanthosCoords[,2:3];head(xanthosCoords)
  names(xanthosCoords)<-c("lon","lat")
  
  
  df<-data.frame()
  # Read in Data Files
  df_q <- read.csv(paste(wdXanthos1,scenario_ix,"/q_bced_1960_1999_ipsl-cm5a-lr_1950_2005.csv",sep=""), header=F, stringsAsFactors = F)
  names(df_q)<-paste("X",c(1950:2005),sep=""); head(df_q)
  df_q$Type<-paste("Runoff",sep="");df_q<-cbind.data.frame(xanthosCoords,df_q); df<-rbind.data.frame(df,df_q);
  head(df)
  
  df<-df%>%dplyr::select(lat,lon,Type,paste("X",years_analysisXanthos,sep="")); head(df) # Create in similar year format to tethys and Demeter outputs
  df$Mean<-rowMeans(df[,4:(length(years_analysisXanthos)+3)])
  colnames(df)[which(names(df) == "Mean")] <- paste("Mean_",min(years_analysisXanthos),"to",max(years_analysisXanthos),sep="")
  
  colnames(df)[which(names(df) == "latitude")] <- "lat"
  colnames(df)[which(names(df) == "longitude")] <- "lon"
  names(df)<-gsub("x","X",names(df),ignore.case=F)
  head(df)
  
  dfxanthos<-df;
  
  years<-c(yearsXanthos,paste("Mean_",min(years_analysisXanthos),"to",max(years_analysisXanthos),sep="")); years

#------------------------------
#--- Run spatial re-aggregation function maps_ReAggregate
#--- Set paramters for function
#------------------------------  
  
  
  maps_ReAggregate(region_i=region_i,scenario_i=scenario_ix,
                   moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                   moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                   moduleRemove=moduleRemove)
  
} # Close Xanthos Run
  

#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Water Scarcity (Tethys/Xanthos)
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________

if(1 %in% c(runScarcity,runScarcityImpacts)){
  
  
  # Create Output Directory
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,sep=""))}
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Scarcity",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Scarcity",sep=""))}
  dir<-paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Scarcity",sep="")
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Scarcity/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Scarcity/subBasin",sep=""))}


  # Tethys Data
  wdtethys1<-paste(wdtethys,scenario_ix,sep="") # Tethys data directory
  df<-data.frame()
  # Read in Data Files
  df_dom <- read.csv(paste(wdtethys1,"/wddom.csv",sep=""), stringsAsFactors = F)
  df_dom$Type<-"Domestic"; df<-rbind.data.frame(df,df_dom);
  df_elec <- read.csv(paste(wdtethys1,"/wdelec.csv",sep=""), stringsAsFactors = F)
  df_elec$Type<-"Electric"; df<-rbind.data.frame(df,df_elec);
  df_irr <- read.csv(paste(wdtethys1,"/wdirr.csv",sep=""), stringsAsFactors = F)
  df_irr$Type<-"Irrigation"; df<-rbind.data.frame(df,df_irr);
  df_liv <- read.csv(paste(wdtethys1,"/wdliv.csv",sep=""), stringsAsFactors = F)
  df_liv$Type<-"Livestock"; df<-rbind.data.frame(df,df_liv);
  df_mfg <- read.csv(paste(wdtethys1,"/wdmfg.csv",sep=""), stringsAsFactors = F)
  df_mfg$Type<-"MFG"; df<-rbind.data.frame(df,df_mfg);
  df_min <- read.csv(paste(wdtethys1,"/wdmin.csv",sep=""), stringsAsFactors = F)
  df_min$Type<-"Mining"; df<-rbind.data.frame(df,df_min);
  df_noag <- read.csv(paste(wdtethys1,"/wdnonag.csv",sep=""), stringsAsFactors = F)
  df_noag$Type<-"Non_Agriculture"; df<-rbind.data.frame(df,df_noag);
  df_total <- read.csv(paste(wdtethys1,"/wdtotal.csv",sep=""), stringsAsFactors = F)
  df_total$Type<-"Total";df<-rbind.data.frame(df,df_total);
  head(df)
  colnames(df)[which(names(df) == "latitude")] <- "lat"
  colnames(df)[which(names(df) == "longitude")] <- "lon"
  names(df)<-gsub("x","X",names(df),ignore.case=F)
  head(df)
  df<-df%>%dplyr::select(lat,lon,yearsX,Type)
  years_tethys<-grep("X",names(df),value=T)[grep("X",names(df),value=T)!="X..ID"];years_tethys
  years_tethysX<-sub("X","",years_tethys);years_tethysX
  df$Mean<-rowMeans(df%>%dplyr::select(years_tethys))
  colnames(df)[which(names(df) == "Mean")] <- paste("Mean_",min(years_tethysX),"to",max(years_tethysX),sep="")
  head(df)
  dftethys<-df  
  
  # Xanthos Data
  wdXanthos1<-paste(wdXanthos,sep="") # Xanthos data directory
  xanthosCoords<-read.csv(paste(dirname(wdXanthos1),"/input/reference/coordinates.csv",sep=""), header=F, stringsAsFactors = F); head(xanthosCoords)
  xanthosCoords<-xanthosCoords[,2:3];head(xanthosCoords)
  names(xanthosCoords)<-c("lon","lat")
  df<-data.frame()
  # Read in Data Files
  df_q <- read.csv(paste(wdXanthos1,scenario_ix,"/q_bced_1960_1999_ipsl-cm5a-lr_1950_2005.csv",sep=""), header=F, stringsAsFactors = F)
  names(df_q)<-paste("X",c(1950:2005),sep=""); head(df_q)
  df_q$Type<-paste("Runoff",sep="");df_q<-cbind.data.frame(xanthosCoords,df_q); df<-rbind.data.frame(df,df_q);
  head(df)
  df<-df%>%dplyr::select(lat,lon,Type,paste("X",years_analysisXanthos,sep="")); head(df) # Create in similar year format to tethys and Demeter outputs
  df$Mean<-rowMeans(df[,4:(length(years_analysisXanthos)+3)])
  colnames(df)[which(names(df) == "Mean")] <- paste("Mean_",min(years_analysisXanthos),"to",max(years_analysisXanthos),sep="")
  colnames(df)[which(names(df) == "latitude")] <- "lat"
  colnames(df)[which(names(df) == "longitude")] <- "lon"
  names(df)<-gsub("x","X",names(df),ignore.case=F)
  head(df)
  dfxanthos<-df;

  
head(dftethys);head(dfxanthos);nrow(dftethys);nrow(dfxanthos)

# Scarcity Data frame
dfsX<-join(dftethys,dfxanthos%>%dplyr::select(lat,lon,grep("Mean",names(dfxanthos),value=T)),by=c("lat","lon"))
ns<-grep("X|Mean",names(dftethys),value=T)[!grepl("X..ID",grep("X|Mean",names(dftethys),value=T))] 
ns # columns for scarcity
xanthosDivider<-dfsX%>%dplyr::select(grep("Mean",names(dfxanthos),value=T)); head(xanthosDivider)
dfs<-cbind.data.frame(dfsX%>%dplyr::select(-ns),sweep(x=dfsX%>%dplyr::select(ns),MARGIN=1,xanthosDivider[,1],FUN="/"));
dfs<-dfs%>%dplyr::select(-c(grep("Mean",names(dfxanthos),value=T)));head(dfs)
df<-dfs
unique(df$Type)
df<-df[df$Type=="Total",]
df$Type<-gsub("Total","Scarcity",df$Type);head(df)

years_scarcity<-grep("X",names(df),value=T)[grep("X",names(df),value=T)!="X..ID"];years_scarcity
years_scarcityX<-sub("X","",years_tethys);years_scarcityX

years<-c(years_tethys,paste("Mean_",min(years_scarcityX),"to",max(years_scarcityX),sep=""));years

#------------------------------
#--- Run spatial re-aggregation function maps_ReAggregate
#--- Set paramters for function
#------------------------------  

# Setup for running function maps_ReAggregage
moduleName<-"scarcity"  # Name of module being run tethys, xanthos, scarcity etc.
moduleParam<-"scarcityFrac"  # Type of parameter for fig names demWatmmPerYr
moduleUnits<- "Fraction"            # Units used for figures
moduleTitleText<- "Water Scarcity"  # Title for figures. Used when titleOn is 1
moduleAggType<- "depth"      # "depth" when using mm or "vol" when using km3
moduleRemove<-NULL   # Remove certain categories for particular modules. Make NULL if not needed

maps_ReAggregate(region_i=region_i,scenario_i=scenario_ix,
                 moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                 moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                 moduleRemove=moduleRemove)


} # Close Scarcity Run


#------------------------------
#--- Xanthos Impacts Scenarios
#------------------------------  

if(runScarcityImpacts==1){

#---------------------
# Create Output Directory
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,sep=""))}
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Impacts",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Impacts",sep=""))}
dir<-paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Impacts",sep="")
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Impacts/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_ix,"/Impacts/subBasin",sep=""))}


# Setup for running function maps_ReAggregage
moduleName<-"RunOffScenarios"  # Name of module being run tethys, xanthos, scarcity etc.
moduleParam<-"runoff"  # Type of parameter for fig names demWatmmPerYr
moduleUnits<- "mm"            # Units used for figures
moduleTitleText<- "Runoff (mm)"  # Title for figures. Used when titleOn is 1
moduleAggType<- "depth"      # "depth" when using mm or "vol" when using km3
moduleRemove<-NULL   # Remove certain categories for particular modules. Make NULL if not needed

# Xanthos Scenarios RCP 6
# Xanthos Data
wdXanthos1<-paste(wdXanthos,sep="") # Xanthos data directory
xanthosCoords<-read.csv(paste(dirname(wdXanthos1),"/input/reference/coordinates.csv",sep=""), header=F, stringsAsFactors = F); head(xanthosCoords)
xanthosCoords<-xanthosCoords[,2:3];head(xanthosCoords)
names(xanthosCoords)<-c("lon","lat")
xanthosGridAreaHec<-read.csv(paste(dirname(wdXanthos),"/input/reference/Grid_Areas_ID.csv",sep=""), header=F, stringsAsFactors = F);
names(xanthosGridAreaHec)<-"Area_hec"; head(xanthosGridAreaHec)

# Convert to mm


df<-data.frame()
for(i in c("noresm1-m","ipsl-cm5a-lr","miroc-esm-chem","gfdl-esm2m","hadgem2-es")){
  df_q1 <- read.csv(paste(wdXanthosRuns,"/pm_abcd_mrtm_",i,"_rcp6p0_1950_2099/q_km3peryear_pm_abcd_mrtm_",i,"_rcp6p0_1950_2099.csv",sep=""), header=T, stringsAsFactors = F);head(df_q1)
  df_q1<-df_q1%>%dplyr::select(-id)
  cols2divide<-names(df_q1)
  xDivider<-xanthosGridAreaHec*0.01 # km2
  df_q1<-cbind.data.frame(df_q1%>%dplyr::select(-cols2divide),sweep(x=df_q1%>%dplyr::select(cols2divide),MARGIN=1,xDivider[,1],FUN="/"));
  df_q1<-df_q1%>%mutate_all(funs(.*1000000))  # Convert to mm/yr
  df_q1<-df_q1%>%mutate(Type=paste(moduleTitleText,sep=""),latitude=xanthosCoords$lat,longitude=xanthosCoords$lon,
                        scenario=i);
  df<-rbind.data.frame(df,df_q1);tail(df)
}
a<-gsub("X","",names(df%>%dplyr::select(-Type,-latitude,-longitude,-scenario)))%>%as.character%>%as.numeric
min(a);max(a)
df1<-df%>%mutate(Mean=rowMeans(df%>%dplyr::select(-Type,-latitude,-longitude,-scenario)));
colnames(df1)[which(names(df1) == "Mean")] <- paste("Mean_",min(a),"to",max(a),sep="")

colnames(df1)[which(names(df1) == "latitude")] <- "lat"
colnames(df1)[which(names(df1) == "longitude")] <- "lon"
names(df1)<-gsub("x","X",names(df1),ignore.case=F)
head(df1)

dfxanthosImpacts<-df1

years<<-c("X2010","X2020","X2030","X2040","X2050");

#--------------------
# Create Impacts Scenario Figures
#--------------------

# Crop to region
df<-dfxanthosImpacts%>%dplyr::select(years,scenario,lat,lon)

# Convert to Spatial Point Data Frames
df1 = SpatialPointsDataFrame(SpatialPoints(coords=(cbind(df1$lon,df1$lat))),data=df1)
proj4string(df1)<-projX

# Crop to the Regional file shpa boundary
df2<-raster::intersect(df1,b1);plot(df2)  # Crop to Layer shpa
dfxtra<<-raster::intersect(df1,b1);plot(dfxtra) #create larger boundary extents for plotting in tmap
gridded(dfxtra)<-TRUE
df3<-df2
gridded(df3)<-TRUE # Create Gridded Data
dfxtra<<-dfxtra

# Gridded scenarios Compare (Produced for each scenario)
for(XanthosScenario_i in unique(df3$scenario)){

dfx<-df3
dfx@data<-dfx@data%>%dplyr::select(years,scenario)%>%filter(scenario==XanthosScenario_i) # Choose the years
dfx@data<-dfx@data%>%dplyr::select(-scenario)  # Remove mean year

fname<-paste("map_",moduleName,"_grid_",region_i,"_",XanthosScenario_i,"_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),"_1SCALE",sep="")
map<-mapX_raster(rasterBoundary=dfxtra,data=dfx,scaleData=df3@data%>%dplyr::select(years))+
  tm_layout(title=XanthosScenario_i,title.position=c("left","bottom"))+tm_facets(nrow=1)+m8
print_PDFPNG(map,dir=dir,filename=fname,
             figWidth_Inch=mapWidthInch*1,figHeight_Inch=mapHeightInch*1,pdfpng=pdfpng);

if(animationsOn==1){
fname<-paste(dir,"/anim_",moduleName,"_grid_",region_i,"_",XanthosScenario_i,"_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),"_1SCALE.gif",sep="")
animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleTitleText,sep=""), title.size = 3, panel.label.size = 2,
                              title.position=c("left","top")),
          filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
           filename=fname,width=NA,height=NA,delay=delay)
print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
           dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
}

fname<-paste("map_",moduleName,"_grid_",region_i,"_",XanthosScenario_i,"_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),"_1SCALEKMEANS",sep="")
map<-mapX_rasterKMeans(rasterBoundary=dfxtra,data=dfx,scaleData=df3@data%>%dplyr::select(years))+
  tm_layout(title=XanthosScenario_i,title.position=c("left","bottom"))+tm_facets(nrow=1)+m8
print_PDFPNG(map,dir=dir,filename=fname,
             figWidth_Inch=mapWidthInch*1,figHeight_Inch=mapHeightInch*1,pdfpng=pdfpng);

if(animationsOn==1){
fname<-paste(dir,"/anim_",moduleName,"_grid_",region_i,"_",XanthosScenario_i,"_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),"_1SCALEKMEANS.gif",sep="")
animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleTitleText,sep=""), title.size = 3, panel.label.size = 2,
                              title.position=c("left","top")),
                filename=gsub(".gif","wLegend.gif",fname),width=NA,height=NA,delay=delay)
animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                filename=fname,width=NA,height=NA,delay=delay)
print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
             dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);

}
} # Close Loop for Different Xanthos Scenarios

#____________________
# By Admin Region
#____________________

head(df3)

dxpbyAdmin<-NULL
for(i in unique(df3$scenario)){
dxp1<-df3
dxp1@data<-dxp1@data%>%filter(scenario==i)%>%dplyr::select(-scenario)
dxp<<-spatAgg_gridded2shape(gridded=dxp1,shape=shpa1,byLev="NAME_1",boundBox=b1,moduleAggType=moduleAggType)
dxp<<-dxp%>%mutate(scenario=i)
a1<-shpa1
a1@data<-join(a1@data,dxp,by=c("NAME_1"))%>%dplyr::select(-c(NAME_1));
if(is.null(dxpbyAdmin)){dxpbyAdmin<<-a1}else{dxpbyAdmin<-rbind(dxpbyAdmin,a1,makeUniqueIDs = TRUE)}
}

head(dxpbyAdmin); unique(dxpbyAdmin$scenario)

if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
  write.csv(dxpbyAdmin,file=paste(dir,"/table_",moduleName,"_",region_i,"_Impacts_ByAdminRegion.csv",sep=""),row.names=F)
}

dfx<-dxpbyAdmin
dfx@data<-dfx@data%>%dplyr::select(years,scenario)%>%
  gather(key=year,value=NewValue,-scenario)%>%mutate(year=as.numeric(as.character(gsub("X","",year))))
head(dfx)


fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",moduleUnits,"_",gsub("X","",year_i),sep="")
if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
  map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                       val="NewValue",var1="scenario",var2="year")
  map
  print_PDFPNG(map,dir=dir,filename=fname,
               figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*0.8,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
}

#KMEANS
fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",moduleUnits,"_",gsub("X","",year_i),"_KMEANS",sep="")
if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
  map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                       val="NewValue",var1="scenario",var2="year")
  map
  print_PDFPNG(map,dir=dir,filename=fname,
               figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*0.8,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
}

#____________________
# By Basin Region
#____________________

head(df3)

dxpbyBasin<-NULL
for(i in unique(df3$scenario)){
  dxp1<-df3
  dxp1@data<-dxp1@data%>%filter(scenario==i)%>%dplyr::select(-scenario)
  dxp<<-spatAgg_gridded2shape(gridded=dxp1,shape=shpbasin1,byLev="basin_name",boundBox=b1,moduleAggType=moduleAggType)
  dxp<<-dxp%>%mutate(scenario=i)
  a1<-shpbasin1
  a1@data<-join(a1@data,dxp,by=c("basin_name"))%>%dplyr::select(-c(basin_name));
  if(is.null(dxpbyBasin)){dxpbyBasin<<-a1}else{dxpbyBasin<-rbind(dxpbyBasin,a1,makeUniqueIDs = TRUE)}
}

head(dxpbyBasin); unique(dxpbyBasin$scenario)

if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
  write.csv(dxpbyBasin,file=paste(dir,"/table_",moduleName,"_",region_i,"_Impacts_ByBasinRegion.csv",sep=""),row.names=F)
}

dfx<-dxpbyBasin
dfx@data<-dfx@data%>%dplyr::select(years,scenario)%>%
  gather(key=year,value=NewValue,-scenario)%>%mutate(year=as.numeric(as.character(gsub("X","",year))))
head(dfx)


fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",moduleUnits,"_",gsub("X","",year_i),sep="")
if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
  map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                       val="NewValue",var1="scenario",var2="year")
  map
  print_PDFPNG(map,dir=dir,filename=fname,
               figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*0.8,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
}

#KMEANS
fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",moduleUnits,"_",gsub("X","",year_i),"_KMEANS",sep="")
if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
  map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                             val="NewValue",var1="scenario",var2="year")
  map
  print_PDFPNG(map,dir=dir,filename=fname,
               figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*0.8,pdfpng=pdfpng)
  selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
}


#-------------------------------------------------------------
# Figure National Mean Runoff by scenario and Year (Line chart)
#------------------------------------------------------------

df1a<-dfxanthosImpacts%>%dplyr::select(-id,-latitude,-longitude,-Type,-contains("Mean"))%>%
  group_by(scenario)%>%
  summarize_all(funs(min))%>%
  gather(key=x,value=ymin,-scenario)%>%
  mutate(x=gsub("X","",x),x=as.numeric(as.character(x)),
         xLabel="Year",NewUnits="~km^3/yr",
         Fill=scenario,FillPalette="colorsX_Basic",FillLabel=scenario);

df1b<-dfxanthosImpacts%>%dplyr::select(-id,-latitude,-longitude,-Type,-contains("Mean"))%>%
  group_by(scenario)%>%
  summarize_all(funs(max))%>%
  gather(key=x,value=ymax,-scenario)%>%
  mutate(x=gsub("X","",x),x=as.numeric(as.character(x)),
         xLabel="Year",NewUnits="~km^3/yr",
         Fill=scenario,FillPalette="colorsX_Basic",FillLabel=scenario)

df1all<-join(df1a,df1b,by=c("scenario","x","Fill","xLabel","NewUnits","FillPalette","FillLabel"))

df1c<-dfxanthosImpacts%>%dplyr::select(-id,-latitude,-longitude,-Type,-contains("Mean"))%>%
  group_by(scenario)%>%
  summarize_all(funs(mean))%>%
  gather(key=x,value=ymean,-scenario)%>%
  mutate(x=gsub("X","",x),x=as.numeric(as.character(x)),
         xLabel="Year",NewUnits="~km^3/yr",
         Fill=scenario,FillPalette="colorsX_Basic",FillLabel=scenario);

df1all<-join(df1all,df1c,by=c("scenario","x","Fill","xLabel","NewUnits","FillPalette","FillLabel"))

df1d<-dfxanthosImpacts%>%dplyr::select(-id,-latitude,-longitude,-Type,-contains("Mean"))%>%
  group_by(scenario)%>%
  summarize_all(funs(var))%>%
  gather(key=x,value=yvar,-scenario)%>%
  mutate(x=gsub("X","",x),x=as.numeric(as.character(x)),
         xLabel="Year",NewUnits="~km^3/yr",
         Fill=scenario,FillPalette="colorsX_Basic",FillLabel=scenario);

df1all<-join(df1all,df1d,by=c("scenario","x","Fill","xLabel","NewUnits","FillPalette","FillLabel"))

df1<-df1all%>%mutate(NewValue=ymax)
p<-fig_LineCompareScenario(df1)
plot(p)
fname<-paste("lines_Max_",region_i,"_XanthosScenarios_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),sep="")
print_PDFPNG(p,dir=dir,filename=fname,
             figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*1,pdfpng=pdfpng);

df1<-df1all%>%mutate(NewValue=ymin)
p<-fig_LineCompareScenario(df1)
plot(p)
fname<-paste("lines_Min_",region_i,"_XanthosScenarios_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),sep="")
print_PDFPNG(p,dir=dir,filename=fname,
             figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*1,pdfpng=pdfpng);

df1<-df1all%>%mutate(NewValue=ymean)
p<-fig_LineCompareScenario(df1)
plot(p)
fname<-paste("lines_Mean_",region_i,"_XanthosScenarios_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),sep="")
print_PDFPNG(p,dir=dir,filename=fname,
             figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*1,pdfpng=pdfpng);

df1<-df1all%>%mutate(NewValue=yvar)
p<-fig_LineCompareScenario(df1)
plot(p)
fname<-paste("lines_Var_",region_i,"_XanthosScenarios_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),sep="")
print_PDFPNG(p,dir=dir,filename=fname,
             figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*1,pdfpng=pdfpng);

df1<-df1all%>%mutate(NewValue=rollmean(ymean,5,na.pad=T))
p<-fig_LineCompareScenario(df1)
plot(p)
fname<-paste("lines_Mean5yrMA_",region_i,"_XanthosScenarios_",moduleParam,"_",moduleUnits,"_",gsub("X","",min(years)),"to",gsub("X","",max(years)),sep="")
print_PDFPNG(p,dir=dir,filename=fname,
             figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*1,pdfpng=pdfpng);

# Figure National Mean Runoff by scenario and Year (Ribbon chart)

df1sd<-dfxanthosImpacts%>%dplyr::select(-id,-latitude,-longitude,-Type,-contains("Mean"))%>%
  group_by(scenario)%>%
  summarize_all(funs(sd))%>%
  gather(key=x,value=y1sd,-scenario)%>%
  mutate(x=gsub("X","",x),x=as.numeric(as.character(x)),
         xLabel="Year",NewUnits="~km^3/yr",
         Fill=scenario,FillPalette="colorsX_Basic",FillLabel=scenario);

df1mean<-dfxanthosImpacts%>%dplyr::select(-id,-latitude,-longitude,-Type,-contains("Mean"))%>%
  group_by(scenario)%>%
  summarize_all(funs(mean))%>%
  gather(key=x,value=ymean,-scenario)%>%
  mutate(x=gsub("X","",x),x=as.numeric(as.character(x)),
         xLabel="Year",NewUnits="~km^3/yr",
         Fill=scenario,FillPalette="colorsX_Basic",FillLabel=scenario)

df1all<-join(df1sd,df1mean,by=c("scenario","x","Fill","xLabel","NewUnits","FillPalette","FillLabel"))
df1<-df1all%>%mutate(ymin=ymean-y1sd,ymax=ymean+y1sd); head(df1)

p<-fig_RibbonCompareScenario(df1,x=x,ymin=ymin,ymax=ymax)
plot(p)

fname<-paste("ribbon_Runoffmean1sd_",region_i,"_XanthosScenarios_",gsub("X","",min(years)),"to",gsub("X","",max(years)),sep="")
print_PDFPNG(p,dir=dir,filename=fname,
             figWidth_Inch=mapWidthInch*1.5,figHeight_Inch=mapHeightInch*1,pdfpng=pdfpng);

#----------------

head(dftethys);head(dfxanthos);head(dfxanthosImpacts);nrow(dftethys);nrow(dfxanthos)

# Scarcity Data frame
dfsX<-join(dftethys,dfxanthos%>%dplyr::select(lat,lon,grep("Mean",names(dfxanthos),value=T)),by=c("lat","lon"))
ns<-grep("X|Mean",names(dftethys),value=T)[!grepl("X..ID",grep("X|Mean",names(dftethys),value=T))] 
ns # columns for scarcity
xanthosDivider<-dfsX%>%dplyr::select(grep("Mean",names(dfxanthos),value=T)); head(xanthosDivider)
dfs<-cbind.data.frame(dfsX%>%dplyr::select(-ns),sweep(x=dfsX%>%dplyr::select(ns),MARGIN=1,xanthosDivider[,1],FUN="/"));
dfs<-dfs%>%dplyr::select(-c(grep("Mean",names(dfxanthos),value=T)));head(dfs)
df<-dfs
unique(df$Type)
df<-df[df$Type=="Total",]
df$Type<-gsub("Total","Scarcity",df$Type);head(df)

years_scarcity<-grep("X",names(df),value=T)[grep("X",names(df),value=T)!="X..ID"];years_scarcity
years_scarcityX<-sub("X","",years_tethys);years_scarcityX

years<-c(years_tethys,paste("Mean_",min(years_scarcityX),"to",max(years_scarcityX),sep=""));years

#------------------------------
#--- Run spatial re-aggregation function maps_ReAggregate
#--- Set paramters for function
#------------------------------  


maps_ReAggregate(region_i=region_i,scenario_i=scenario_ix,
                 moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                 moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                 moduleRemove=moduleRemove)


}

#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Selected Figures
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________

selectFigsparams<<-gsub("Region",region_i,selectFigsparams);selectFigsparams
reportSelFigsName<<-paste("Report_SelectedFigs_",region_i,"_",scenario_ix,sep="");reportSelFigsName

if(!dir.exists(paste(wd0,"/fig_outputsSelect",sep=""))){dir.create(paste(wd0,"/fig_outputsSelect",sep=""))}
if(dir.exists(paste(wd0,"/fig_outputsSelect/",reportSelFigsName,sep=""))){unlink(paste(wd0,"/fig_outputsSelect/",reportSelFigsName,sep=""),recursive=T)}
if(!dir.exists(paste(wd0,"/fig_outputsSelect/",reportSelFigsName,sep=""))){dir.create(paste(wd0,"/fig_outputsSelect/",reportSelFigsName,sep=""))}

selectedFigs
        
selectedFigsX<<-grep(paste(selectFigsparams, collapse="|"),selectedFigs,value=T);selectedFigsX
file.copy(selectedFigsX,to=paste(wd0,"/fig_outputsSelect/",reportSelFigsName,sep=""),overwrite=T,recursive=T)
}#Close scenariosIndv

