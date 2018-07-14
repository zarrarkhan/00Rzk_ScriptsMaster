#***********
# Contact: Zarrar Khan (zarrar.khan@pnnl.gov)
# Project: IDB Argentina
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
scenarios<<-scenariosAnalysis
gcamDatabaseName<<-gcamDatabaseNameAnalysis

rangeX<<-seq(2005,2050,by=5)
yearsX<<-paste("X",rangeX,sep="")  # To put in format that is read in from tethys and Demeter
years_analysisXanthos<<-seq(1950,2005,by=1) # For Xanthos
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
# Select Report figures
#______________________

selectFigsparams<<-c()
## NOTE: Region will be substituted for each region analyzed
selectFigsparams<<-c("map_basemaps_m5_Region_regionalMap",
                    "map_basemaps_m6_Region_regionalMapBasinsProvinces",
                    "map_basemaps_m1_Region_provincialLabelled",
                    "map_basemaps_m3_Region_basinsLabelledCropped",
                    "line_GCAM_gdpPerCapita_Region_1975to2050",
                    "line_GCAM_pop_Region_1975to2050",
                    "line_GCAM_gdp_Region_1975to2050",
                    "SameScale1_bar_GCAM_primNrgConsumByFuel_Region_1975to2050",
                    "SameScale1_bar_GCAMtotFinalNrgByEndUse_Region_1975to2050",
                    "bar_GCAM_elecByTech_Region_1975to2050",
                    "SameScale2_bar_GCAMwatWithdrawBySec_Region_1975to2050",
                    "SameScale2_bar_GCAMwatConsumBySec_Region_1975to2050",
                    "SameScale3_bar_GCAMirrWatWithBasin_Region_1975to2050",
                    "SameScale3_bar_GCAMirrWatConsBasin_Region_1975to2050",
                    "bar_GCAM_watWithdrawByCrop_Region_1975to2050",
                    "bar_GCAM_aggLandAlloc_Region_1975to2050",
                    "Edit1_AgbyCropNoForestPasture_bar_GCAM_agProdByCrop_Region_1975to2050",
                    "map_xanthos_grid_Region_watSup_BySector_Mean_1950to2005_KMEANS",
                    "map_xanthos_polyAdmin_Region_watSup_BySector_Mean_1950to2005_KMEANS",
                    "map_xanthos_polysubBasin_Region_watSup_BySector_Mean_1950to2005_KMEANS",
                    "map_tethys_grid_Region_demWatmmPerYr_BySector_FREESCALE_Mean_2005to2100_KMEANS",
                    "map_tethys_grid_Region_demWatmmPerYr_BySector_FREESCALE_Mean_2005to2100_KMEANS",
                    "map_tethys_polyAdmin_Region_demWatmmPerYr_BySector_FREESCALE_Mean_2005to2100_KMEANS",
                    "map_tethys_polysubBasin_Region_demWatmmPerYr_BySector_FREESCALE_Mean_2005to2100_KMEANS",
                    "map_Demeter_grid_Region_landcovFract_BySector_2005",
                    "map_Demeter_grid_Region_landcovFract_BySector_2100",
                    "map_Demeter_grid_Region_landcovFract_BySector_FREESCALE_Mean_2005to2100_KMEANS",
                    "map_Demeter_polyAdmin_Region_landcovFract_BySector_FREESCALE_Mean_2005to2100_KMEANS",
                    "map_Demeter_polysubBasin_Region_landcovFract_BySector_FREESCALE_Mean_2005to2100_KMEANS",
                    "map_scarcity_grid_Region_scarcityFrac_Total_2005to2050_KMEANS",
                    "map_scarcity_grid_Region_scarcityFrac_BySector_2005_KMEANS",
                    "map_scarcity_polyAdmin_Region_scarcityFrac_BySector_FREESCALE_Mean_2005to2100_KMEANS",
                    "map_scarcity_polysubBasin_Region_scarcityFrac_BySector_FREESCALE_Mean_2005to2100_KMEANS")
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
for (scenario_i in scenarios){
queryData.proj<<-addScenario(conn=connx, proj="queryData.proj",scenario=scenario_i,queryFile='analysis_queries.xml')  # Check your queries file
}}else{
queryData.proj<<-loadProject("queryData.proj")  # Use already saved database
}

lscen<<-listScenarios(queryData.proj);lscen
lquer<<-listQueries(queryData.proj);lquer
#getQuery(queryData.proj,"Primary Energy Consumption (Direct Equivalent")   # Check a Query


#______________________
# Read in Spatial Data
#______________________

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

#______________________
# New Functions
#______________________

# source function file
source(paste(wdScripts,"/rJGCRI_ChartsMaps.R",sep=""))



#--------------------------
# Main Mapping function
#--------------------------

maps_ReAggregate <<- function(region_i=region_i,scenario_i=scenario_i,
                              moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                              moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                              moduleRemove=moduleRemove){
  
#  region_i=region_i;scenario_i=scenario_i;moduleName=moduleName;moduleParam=moduleParam;moduleUnits=moduleUnits;
#  moduleTitleText=moduleTitleText;moduleAggType=moduleAggType;moduleRemove=moduleRemove
  
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  # BY YEAR
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  
  yearsOriginal<<-years
  if(meanYearOnly==1){years<-years[length(years)]}
  
  for (year_i in years) {
    
    dfCommonScale<-df%>%dplyr::select(-c(lat,lon,Type))
    if(moduleName=="scarcity"){dfCommonScale[dfCommonScale > 1]<-1}
    
    
    df1<-subset(df,select=c(lat,lon,get(year_i),Type))
    df1<-dcast(df1,lat+lon~Type,value.var=year_i,fun.aggregate=sum,na.rm=F);head(df1)
    
    # Convert to Spatial Point Data Frames
    df1 = SpatialPointsDataFrame(
      SpatialPoints(coords=(cbind(df1$lon,df1$lat))),
      data=df1
    )
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
    
    #---------------------------------------------
    #----- For each year compare Demands by Sector
    #---------------------------------------------
    
    dfx<-df3
    dfx@data<-subset(dfx@data,select=c(unique(df$Type)))  # Choose the Sectors
    if(moduleRemove!=c("")){dfx@data<-dfx@data%>%dplyr::select(-moduleRemove)}
    
    # Remove outliers (anything greater than 5 sd from mean, replace with NA)
    #m<-as.matrix(dfx@data)
    #m[m>(mean(m)+5*sd(m))]<-NA
    #dfx@data<-as.data.frame(m)
    #apply(dfgrid@data,2,max)
    
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_raster(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScale)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        m8+if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i),sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_rasterKMeans(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScale) + tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
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
    
    dxp<-data.frame()
    for (type in unique(df$Type)){
      r<-df3;r@data<-r@data%>%dplyr::select(-lat,-lon)%>%dplyr::select(type);head(r)
      r<-raster(r)
      projection(r)<-projX
      rcrop<-raster::intersect(r,b1)
      rcropP<-rasterToPolygons(rcrop)
      
      if(moduleAggType=="depth"){
        x<-raster::intersect(shpa1,rcropP);plot(x);
        x@data$area<-area(x); head(x@data)
        s1<-shpa1
        s1$subRegAreaSum<-area(shpa1);head(s1)
        s1<-s1@data%>%dplyr::select(NAME_1,subRegAreaSum);head(s1)
        x@data<-join(x@data,s1,by="NAME_1");head(x)
        x@data$areaPrcnt<-x@data$area/x@data$subRegAreaSum;
        x@data$WeightedParam<-x@data%>%dplyr::select(type)*x@data%>%dplyr::select(areaPrcnt);head(x)
        ## MEAN
        dxpX<- x@data %>% subset(select=c("WeightedParam","NAME_1")) %>%
          group_by(NAME_1) %>% 
          summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
        names(dxpX)[names(dxpX)=="WeightedParam"]<-type
      }
      if(moduleAggType=="vol"){
        w <- raster::extract(r,shpa1, method="simple",weights=T, normalizeWeights=F);head(w)
        dfx<-data.frame()
        for (i in seq(w)){
          x<-as.data.frame(w[[i]])
          x$ID<-shpa1@data$NAME_1[[i]]
          x$WeightedParam<-x$value*x$weight
          #assign(paste0("df", i), x)
          dfx<-rbind.data.frame(dfx,x)
        }
        names(dfx)[names(dfx)=="ID"]<-"NAME_1";head(dfx)
        ## SUM
        dxpX<- dfx %>% subset(select=c("WeightedParam","NAME_1")) %>%
          group_by(NAME_1) %>% 
          summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
        names(dxpX)[names(dxpX)=="WeightedParam"]<-type
      }
      if(names(dxpX)[2]==unique(df$Type)[1]){dxp<-dxpX}else{dxp<-join(dxp,dxpX,by="NAME_1")};head(dxp)
    }
    dxpbyAdmin<-dxp
    head(dxpbyAdmin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    write.csv(dxpbyAdmin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorByAdminRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
    }
    
    dfCommonScale<-dxp%>%dplyr::select(-c(NAME_1))
    if(moduleName=="scarcity"){dfCommonScale[dfCommonScale > 1]<-1}
    
    #---------------------------------------------
    #----- For each year_i compare Demands by Sector (By Admin Region)
    #---------------------------------------------
    
    shpa.x<-shpa1
    shpa.x@data<-join(shpa.x@data,dxpbyAdmin,by=c("NAME_1")) %>% 
      subset(select=c(unique(df$Type),"NAME_1")) %>%dplyr::select(-c(NAME_1));
    if(moduleRemove!=c("")){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fill(data=dfx,scaleData=dfCommonScale)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Provinces",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fillKMeans(data=dfx,scaleData=dfCommonScale)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
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
    
    dxp<-data.frame()
    for (type in unique(df$Type)){
      r<-df3;r@data<-r@data%>%dplyr::select(-lat,-lon)%>%dplyr::select(type);head(r)
      r<-raster(r)
      projection(r)<-projX
      rcrop<-raster::intersect(r,b1)
      rcropP<-rasterToPolygons(rcrop)
      
      if(moduleAggType=="depth"){
        x<-raster::intersect(shpbasin1,rcropP);plot(x);
        x@data$area<-area(x); head(x@data)
        s1<-shpbasin1
        s1$subRegAreaSum<-area(shpbasin1);head(s1)
        s1<-s1@data%>%dplyr::select(basin_name,subRegAreaSum);head(s1)
        x@data<-join(x@data,s1,by="basin_name");head(x)
        x@data$areaPrcnt<-x@data$area/x@data$subRegAreaSum;
        x@data$WeightedParam<-x@data%>%dplyr::select(type)*x@data%>%dplyr::select(areaPrcnt);head(x)
        ## MEAN
        dxpX<- x@data %>% subset(select=c("WeightedParam","basin_name")) %>%
          group_by(basin_name) %>% 
          summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
        names(dxpX)[names(dxpX)=="WeightedParam"]<-type
      }
      if(moduleAggType=="vol"){
        w <- raster::extract(r,shpbasin1, method="simple",weights=T, normalizeWeights=F);head(w)
        dfx<-data.frame()
        for (i in seq(w)){
          x<-as.data.frame(w[[i]])
          x$ID<-shpbasin1@data$basin_name[[i]]
          x$WeightedParam<-x$value*x$weight
          #assign(paste0("df", i), x)
          dfx<-rbind.data.frame(dfx,x)
        }
        names(dfx)[names(dfx)=="ID"]<-"basin_name";head(dfx)
        ## SUM
        dxpX<- dfx %>% subset(select=c("WeightedParam","basin_name")) %>%
          group_by(basin_name) %>% 
          summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
        names(dxpX)[names(dxpX)=="WeightedParam"]<-type
      }
      if(names(dxpX)[2]==unique(df$Type)[1]){dxp<-dxpX}else{dxp<-join(dxp,dxpX,by="basin_name")};head(dxp)
    }
    dxpbyBasin<-dxp; head(dxpbyBasin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbyBasin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorByBasinRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
    }
    
    dfCommonScale<-dxp%>%dplyr::select(-c(basin_name))
    if(moduleName=="scarcity"){dfCommonScale[dfCommonScale > 1]<-1}
    
    #---------------------------------------------
    #----- For each year_i compare Demands by Sector (By Basin Region)
    #---------------------------------------------
    
    shpa.x<-shpbasin1
    shpa.x@data<-join(shpa.x@data,dxpbyBasin,by=c("basin_name")) %>% 
      subset(select=c(unique(df$Type),"basin_name")) %>% subset(select=-c(basin_name));
    if(moduleRemove!=c("")){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fill(data=dfx,scaleData=dfCommonScale)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Basins",sep=""))}
      map
      print_PDFPNG(map,dir=dir,fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-m7+mapX_fillKMeans(data=dfx,scaleData=dfCommonScale)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
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
      
      dxp<-data.frame()
      for (type in unique(df$Type)){
        r<-df3;r@data<-r@data%>%dplyr::select(-lat,-lon)%>%dplyr::select(type);head(r)
        r<-raster(r)
        projection(r)<-projX
        rcrop<-raster::intersect(r,b1)
        rcropP<-rasterToPolygons(rcrop)
        
        if(moduleAggType=="depth"){
          x<-raster::intersect(shpsubBasin1,rcropP);plot(x);
          x@data$area<-area(x); head(x@data)
          s1<-shpsubBasin1
          s1$subRegAreaSum<-area(shpsubBasin1);head(s1)
          s1<-s1@data%>%dplyr::select(subBasin_name,subRegAreaSum);head(s1)
          x@data<-join(x@data,s1,by="subBasin_name");head(x)
          x@data$areaPrcnt<-x@data$area/x@data$subRegAreaSum;
          x@data$WeightedParam<-x@data%>%dplyr::select(type)*x@data%>%dplyr::select(areaPrcnt);head(x)
          ## MEAN
          dxpX<- x@data %>% subset(select=c("WeightedParam","subBasin_name")) %>%
            group_by(subBasin_name) %>% 
            summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
          names(dxpX)[names(dxpX)=="WeightedParam"]<-type
        }
        if(moduleAggType=="vol"){
          w <- raster::extract(r,shpsubBasin1, method="simple",weights=T, normalizeWeights=F);head(w)
          dfx<-data.frame()
          for (i in seq(w)){
            x<-as.data.frame(w[[i]])
            x$ID<-shpsubBasin1@data$subBasin_name[[i]]
            x$WeightedParam<-x$value*x$weight
            #assign(paste0("df", i), x)
            dfx<-rbind.data.frame(dfx,x)
          }
          names(dfx)[names(dfx)=="ID"]<-"subBasin_name";head(dfx)
          ## SUM
          dxpX<- dfx %>% subset(select=c("WeightedParam","subBasin_name")) %>%
            group_by(subBasin_name) %>% 
            summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
          names(dxpX)[names(dxpX)=="WeightedParam"]<-type
        }
        if(names(dxpX)[2]==unique(df$Type)[1]){dxp<-dxpX}else{dxp<-join(dxp,dxpX,by="subBasin_name")};head(dxp)
      }
      dxpbysubBasin<-dxp; head(dxpbysubBasin)
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbysubBasin,file=paste(dir,"/subBasin/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorBysubBasinRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
      }
      
      dfCommonScale<-dxp%>%dplyr::select(-c(subBasin_name))
      if(moduleName=="scarcity"){dfCommonScale[dfCommonScale > 1]<-1}
      
      #---------------------------------------------
      #----- For each year_i compare Demands by Sector (By Admin Region)
      #---------------------------------------------
      
      shpa.x<-shpsubBasin1
      shpa.x@data<-join(shpa.x@data,dxpbysubBasin,by=c("subBasin_name")) %>% 
        subset(select=c(unique(df$Type),"subBasin_name")) %>% subset(select=-c(subBasin_name));
      if(moduleRemove!=c("")){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
      dfx<-shpa.x
      
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-m7+mapX_fill(data=dfx,scaleData=dfCommonScale)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
          if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," subBasins",sep=""))}
        map
        print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
      #KMEANS
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-m7+mapX_fillKMeans(data=dfx,scaleData=dfCommonScale)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
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
  
  years<-yearsOriginal;
  
  types<-unique(df$Type)
  
  for (type in types){
    
    
    #____________________
    # GRIDDED
    #____________________
    
    dfCommonScale<-df%>%dplyr::select(-c(lat,lon,Type))
    if(moduleName=="scarcity"){dfCommonScale[dfCommonScale > 1]<-1}
    
    
    # Gridded Boundary
    df1<-subset(df,select=c("lat","lon",years,"Type"))
    df1<-df1[df1$Type==type,]
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
    
    yearsOriginal<-years
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
    
    years<-yearsOriginal;
    dfx<-df3
    dfx@data<-subset(dfx@data,select=c(years))  # Choose the years
    dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
    
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-mapX_raster(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScale)+m8+tm_legend(title=moduleUnits)+
        if(titleOn==1){tm_layout(main.title=paste(type,sep=""))} 
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),".gif",sep="")
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
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-mapX_rasterKMeans(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScale)+m8+tm_legend(title=moduleUnits)+
        if(titleOn==1){tm_layout(main.title=paste(type,sep=""))} 
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS.gif",sep="")
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
    

    # Common scales for all non-agricultural demands
    
    if(!type %in% c("Total","Non_Agriculture")){
      dfgridX<-subset(df[(df$Type!="Total" & df$Type!="Non_Agriculture"),],select=years)
      dfgridY<-subset(df[(df$Type!="Total" & df$Type!="Non_Agriculture"),],select=c(years,"Type"))
      
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
                          filename=fname,width=NA,height=NA,delay=delay)
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }
      }
      
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
    }
    }
    
    #____________________
    # By ADMIN Region
    #____________________
    
    
    head(df3)
    
    dxp<-data.frame()
    for (year_i in years){
      r<-df3;r@data<-r@data%>%dplyr::select(-lat,-lon)%>%dplyr::select(year_i);head(r)
      r<-raster(r)
      projection(r)<-projX
      rcrop<-raster::intersect(r,b1)
      rcropP<-rasterToPolygons(rcrop)
      
      if(moduleAggType=="depth"){
        x<-raster::intersect(shpa1,rcropP);plot(x);
        x@data$area<-area(x); head(x@data)
        s1<-shpa1
        s1$subRegAreaSum<-area(shpa1);head(s1)
        s1<-s1@data%>%dplyr::select(NAME_1,subRegAreaSum);head(s1)
        x@data<-join(x@data,s1,by="NAME_1");head(x)
        x@data$areaPrcnt<-x@data$area/x@data$subRegAreaSum;
        x@data$WeightedParam<-x@data%>%dplyr::select(year_i)*x@data%>%dplyr::select(areaPrcnt);head(x)
        ## MEAN
        dxpX<- x@data %>% subset(select=c("WeightedParam","NAME_1")) %>%
          group_by(NAME_1) %>% 
          summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
        names(dxpX)[names(dxpX)=="WeightedParam"]<-year_i
      }
      if(moduleAggType=="vol"){
        w <- raster::extract(r,shpa1, method="simple",weights=T, normalizeWeights=F);head(w)
        dfx<-data.frame()
        for (i in seq(w)){
          x<-as.data.frame(w[[i]])
          x$ID<-shpa1@data$NAME_1[[i]]
          x$WeightedParam<-x$value*x$weight
          #assign(paste0("df", i), x)
          dfx<-rbind.data.frame(dfx,x)
        }
        names(dfx)[names(dfx)=="ID"]<-"NAME_1";head(dfx)
        ## SUM
        dxpX<- dfx %>% subset(select=c("WeightedParam","NAME_1")) %>%
          group_by(NAME_1) %>% 
          summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
        names(dxpX)[names(dxpX)=="WeightedParam"]<-year_i
      }
      if(names(dxpX)[2]==years[1]){dxp<-dxpX}else{dxp<-join(dxp,dxpX,by="NAME_1")};head(dxp)
    }
    dxpbyAdmin<-dxp
    head(dxpbyAdmin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbyAdmin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_ByAdminRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
    }
    
    dfCommonScale<-dxp%>%dplyr::select(-c(NAME_1))
    if(moduleName=="scarcity"){dfCommonScale[dfCommonScale > 1]<-1}
    
    shpa.x<-shpa1
    shpa.x@data<-join(shpa.x@data,dxpbyAdmin,by=c("NAME_1")) %>% 
      subset(select=c(years,"NAME_1")) %>% subset(select=-c(NAME_1))
    dfx<-shpa.x
    dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
    
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    map<- m7 + mapX_fill(data=dfx,scaleData=dfCommonScale) + tm_legend(title=moduleUnits) +
      if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Province",sep=""))} 
    map
    print_PDFPNG(map,dir=dir,fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
    selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),".gif",sep="")
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
    
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfCommonScale) + tm_legend(title=moduleUnits) +
    if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Province",sep=""))} 
    map
    print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
    selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
        #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS.gif",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
        print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                     dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
    }
    }
    # Common scales for all non-agricultural demands
    
    if(!type %in% c("Total","Non_Agriculture")){
      dfgridX<-subset(df[(df$Type!="Total" & df$Type!="Non_Agriculture"),],select=years)
      
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
                        filename=fname,width=NA,height=NA,delay=delay)
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }
      }
      
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
      }
    }
    
    
    #____________________
    # By BASIN Region
    #____________________
    
    head(df3)
    
    dxp<-data.frame()
    for (year_i in years){
      r<-df3;r@data<-r@data%>%dplyr::select(-lat,-lon)%>%dplyr::select(year_i);head(r)
      r<-raster(r)
      projection(r)<-projX
      rcrop<-raster::intersect(r,b1)
      rcropP<-rasterToPolygons(rcrop)
      
      if(moduleAggType=="depth"){
        x<-raster::intersect(shpbasin1,rcropP);plot(x);
        x@data$area<-area(x); head(x@data)
        s1<-shpbasin1
        s1$subRegAreaSum<-area(shpbasin1);head(s1)
        s1<-s1@data%>%dplyr::select(basin_name,subRegAreaSum);head(s1)
        x@data<-join(x@data,s1,by="basin_name");head(x)
        x@data$areaPrcnt<-x@data$area/x@data$subRegAreaSum;
        x@data$WeightedParam<-x@data%>%dplyr::select(year_i)*x@data%>%dplyr::select(areaPrcnt);head(x)
        ## MEAN
        dxpX<- x@data %>% subset(select=c("WeightedParam","basin_name")) %>%
          group_by(basin_name) %>% 
          summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
        names(dxpX)[names(dxpX)=="WeightedParam"]<-year_i
      }
      if(moduleAggType=="vol"){
        w <- raster::extract(r,shpbasin1, method="simple",weights=T, normalizeWeights=F);head(w)
        dfx<-data.frame()
        for (i in seq(w)){
          x<-as.data.frame(w[[i]])
          x$ID<-shpbasin1@data$basin_name[[i]]
          x$WeightedParam<-x$value*x$weight
          #assign(paste0("df", i), x)
          dfx<-rbind.data.frame(dfx,x)
        }
        names(dfx)[names(dfx)=="ID"]<-"basin_name";head(dfx)
        ## SUM
        dxpX<- dfx %>% subset(select=c("WeightedParam","basin_name")) %>%
          group_by(basin_name) %>% 
          summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
        names(dxpX)[names(dxpX)=="WeightedParam"]<-year_i
      }
      if(names(dxpX)[2]==years[1]){dxp<-dxpX}else{dxp<-join(dxp,dxpX,by="basin_name")};head(dxp)
    }
    dxpbyBasin<-dxp
    head(dxpbyBasin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbyBasin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_ByBasinRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
    }
    
    dfCommonScale<-dxp%>%dplyr::select(-c(basin_name))
    if(moduleName=="scarcity"){dfCommonScale[dfCommonScale > 1]<-1}
    
    shpa.x<-shpbasin1
    shpa.x@data<-join(shpa.x@data,dxpbyBasin,by=c("basin_name")) %>% 
      subset(select=c(years,"basin_name")) %>% subset(select=-c(basin_name))
    dfx<-shpa.x
    dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
    
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    map<- m7 + mapX_fill(data=dfx,scaleData=dfCommonScale) + tm_legend(title=moduleUnits) +
      if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Basin",sep=""))}
    map
    print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
    selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),".gif",sep="")
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
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
    map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfCommonScale) + tm_legend(title=moduleUnits) +
      if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by Basin",sep=""))}
    map
    print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
    selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    #---- Animation File
    if(animationsOn==1){
      fname<-paste(dir,"/anim_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS.gif",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
        print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                     dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
    }}
    # Common scales for all non-agricultural demands
    
    if(!type %in% c("Total","Non_Agriculture")){
      dfgridX<-subset(df[(df$Type!="Total" & df$Type!="Non_Agriculture"),],select=years)
      
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
          animation_tmapZ(map+tm_layout(legend.show=F,main.title=NA, title="", title.size = 3, panel.label.size = 2),
                          filename=fname,width=NA,height=NA,delay=delay)
          print_PDFPNG(map+ tm_layout(frame=FALSE,legend.only=T, panel.show=FALSE,legend.text.size=1),
                       dir=dir,filename=gsub(".gif","Legend",gsub(paste(dir,"/",sep=""),"",fname)),figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }}
      
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
    }
    }
    
    if(bySubBasin==1){
      #____________________
      # By subBasin Region
      #____________________
      
      
      
      head(df3)
      
      dxp<-data.frame()
      for (year_i in years){
        r<-df3;r@data<-r@data%>%dplyr::select(-lat,-lon)%>%dplyr::select(year_i);head(r)
        r<-raster(r)
        projection(r)<-projX
        rcrop<-raster::intersect(r,b1)
        rcropP<-rasterToPolygons(rcrop)
        
        if(moduleAggType=="depth"){
          x<-raster::intersect(shpsubBasin1,rcropP);plot(x);
          x@data$area<-area(x); head(x@data)
          s1<-shpsubBasin1
          s1$subRegAreaSum<-area(shpsubBasin1);head(s1)
          s1<-s1@data%>%dplyr::select(subBasin_name,subRegAreaSum);head(s1)
          x@data<-join(x@data,s1,by="subBasin_name");head(x)
          x@data$areaPrcnt<-x@data$area/x@data$subRegAreaSum;
          x@data$WeightedParam<-x@data%>%dplyr::select(year_i)*x@data%>%dplyr::select(areaPrcnt);head(x)
          ## MEAN
          dxpX<- x@data %>% subset(select=c("WeightedParam","subBasin_name")) %>%
            group_by(subBasin_name) %>% 
            summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
          names(dxpX)[names(dxpX)=="WeightedParam"]<-year_i
        }
        if(moduleAggType=="vol"){
          w <- raster::extract(r,shpsubBasin1, method="simple",weights=T, normalizeWeights=F);head(w)
          dfx<-data.frame()
          for (i in seq(w)){
            x<-as.data.frame(w[[i]])
            x$ID<-shpsubBasin1@data$subBasin_name[[i]]
            x$WeightedParam<-x$value*x$weight
            #assign(paste0("df", i), x)
            dfx<-rbind.data.frame(dfx,x)
          }
          names(dfx)[names(dfx)=="ID"]<-"subBasin_name";head(dfx)
          ## SUM
          dxpX<- dfx %>% subset(select=c("WeightedParam","subBasin_name")) %>%
            group_by(subBasin_name) %>% 
            summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
          names(dxpX)[names(dxpX)=="WeightedParam"]<-year_i
        }
        if(names(dxpX)[2]==years[1]){dxp<-dxpX}else{dxp<-join(dxp,dxpX,by="subBasin_name")};head(dxp)
      }
      dxpbysubBasin<-dxp
      head(dxpbysubBasin)
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
write.csv(dxpbysubBasin,file=paste(dir,"/subBasin/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_BysubBasinRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
      }
      
      dfCommonScale<-dxp%>%dplyr::select(-c(subBasin_name))
      if(moduleName=="scarcity"){dfCommonScale[dfCommonScale > 1]<-1}
      
      shpa.x<-shpsubBasin1
      shpa.x@data<-join(shpa.x@data,dxpbysubBasin,by=c("subBasin_name")) %>% 
        subset(select=c(years,"subBasin_name")) %>% subset(select=-c(subBasin_name))
      dfx<-shpa.x
      dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
      
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<- m7 + mapX_fill(data=dfx,scaleData=dfCommonScale) + tm_legend(title=moduleUnits) +
        if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by subBasin",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/subBasin/anim_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),".gif",sep="")
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
      
      
      #KMEANS
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<- m7 + mapX_fillKMeans(data=dfx,scaleData=dfCommonScale) + tm_legend(title=moduleUnits) +
        if(titleOn==1){tm_layout(main.title=paste(region_i,moduleTitleText,type," by subBasin",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      #---- Animation File
      if(animationsOn==1){
        fname<-paste(dir,"/subBasin/anim_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_KMEANS.gif",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        animation_tmapZ(map+tm_layout(main.title=NA, title=paste(moduleUnits,sep=""), title.size = 3, panel.label.size = 2),
                        filename=fname,width=NA,height=NA,delay=delay)
          selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
      }}
      # Common scales for all non-agricultural demands
      
      if(!type %in% c("Total","Non_Agriculture")){
        dfgridX<-subset(df[(df$Type!="Total" & df$Type!="Non_Agriculture"),],select=years)
        
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
        }
        
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
        }
      }
      }
    } # Close bySubBasin
  } # Close Type
} # Close Mapping Function

#______________________
# Start Regional and Scenario Loops
#______________________

#region_i<-regions[1];region_i;scenario_i<-scenarios[1];scenario_i;   #For testing purposes

for(scenario_i in scenarios){
    
    
# Create Folders
if(!dir.exists(paste(wdfigsOut,"/",region_i,sep=""))){dir.create(paste(wdfigsOut,"/",region_i,sep=""))}
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,sep=""))}
dir<-paste(wdfigsOut,"/",region_i,"/",scenario_i,sep="")


#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# GCAM Charts
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________

    
    if(runGCAMCharts==1){
      
      #____________________________________________
      # Prepare Data From GCAM DataBase
      #____________________________________________
      
      #----------
      # Line Charts (df_line)
      #-----------
      
      df_line<-data.frame()
      
      tbl <- getQuery(queryData.proj, "GDP per capita MER by region") # Tibble  # MER is Market Exchange rate
      df<-as.data.frame(tbl);              # Data frame
      df$param<-"gdpPerCapita"; head(df)
      df$Query<-"GDP per capita MER by region"
      df$NewValue<-df$value
      df$NewUnits<-"GDP~per~Capita~(Thousand~1990~USD~per~Person)"  # Use ~ for spaces. Will be parsed later
      df$x<-df$year
      df$xLabel<-"Year"
      df_line<-rbind.data.frame(df_line,df); head(df_line)
      
      tbl <- getQuery(queryData.proj, "GDP MER by region") # Tibble
      df<-as.data.frame(tbl);              # Data frame
      df$param<-"gdp"; head(df) 
      df$Query<-"GDP MER by region"
      df$NewValue<-df$value/1000
      df$NewUnits<-"GDP~(Billion~1990~USD)" # Use ~ for spaces. Will be parsed later
      df$x<-df$year
      df$xLabel<-"Year"
      df_line<-rbind.data.frame(df_line,df); head(df_line)
      
      tbl <- getQuery(queryData.proj, "Population by region") # Tibble
      df<-as.data.frame(tbl);              # Data frame
      df$param<-"pop"; head(df) 
      df$Query<-"Population by region" 
      df$NewValue<- df$value/1000
      df$NewUnits<-"Population~(Millions)" # Use ~ for spaces. Will be parsed later
      df$x<-df$year
      df$xLabel<-"Year"
      df_line<-rbind.data.frame(df_line,df); head(df_line)
      
      #----------
      # Bar Charts (df_bar)
      #-----------
      
      df_bar<-data.frame()
      
      
      tbl <- getQuery(queryData.proj, "Total final energy by aggregate end-use sector") # Tibble
      df<-as.data.frame(tbl);head(df)                 # Data frame
      totFinalNrgByEndUse<-df %>% mutate (x=year,
                                          xLabel="Year",
                                          param="totFinalNrgByEndUse",
                                          NewValue=value,
                                          NewUnits="Final~Energy~by~End-Use~Sector~(EJ)", # Use ~ for spaces. Will be parsed later
                                          Fill=sector,
                                          FillLabel="Sector",
                                          FillPalette="enduse_colors") %>%
                                          subset(.,select=-c(sector));
      df_bar<-rbind.data.frame(df_bar,totFinalNrgByEndUse); head(df_bar)
      
      tbl <- getQuery(queryData.proj, "primary energy consumption by region (direct equivalent)") # Tibble
      df<-as.data.frame(tbl);head(df)                # Data frame
      primNrgConsumByFuel<-df %>% mutate (x=year,
                                          xLabel="Year",
                                          param="primNrgConsumByFuel",
                                          NewValue=value,
                                          NewUnits="Primary~Energy~Consumption~(EJ)", # Use ~ for spaces. Will be parsed later
                                          Fill=fuel,
                                          FillLabel="Fuel",
                                          FillPalette="colorsX_PAL_pri_ene")%>%
                                          subset(.,select=-c(fuel));
      df_bar<-rbind.data.frame(df_bar,primNrgConsumByFuel); head(df_bar)
      
      tbl <- getQuery(queryData.proj, "Electricity generation by aggregate technology") # Tibble
      df<-as.data.frame(tbl);head(df)                # Data frame
      elecByTech<-df %>% mutate (x=year,
                                 xLabel="Year",
                                 param="elecByTech",
                                 NewValue=value,
                                 NewUnits="Electricity~Generation~(EJ)", # Use ~ for spaces. Will be parsed later
                                 Fill=technology,
                                 FillLabel="Technology",
                                 FillPalette="colorsX_elec_tech_colors") %>%
                                 subset(.,select=-c(technology));
      df_bar<-rbind.data.frame(df_bar,elecByTech); head(df_bar)
      
      tbl <- getQuery(queryData.proj, "water consumption by sector") # Tibble
      df<-as.data.frame(tbl);head(df)                # Data frame
      watConsumBySec<-df %>% mutate (x=year,
                                     xLabel="Year",
                                     param="watConsumBySec",
                                     NewValue=value,
                                     NewUnits="Water~Consumption~(km^3)", # Use ~ for spaces. Will be parsed later
                                     Fill=sector,
                                     FillLabel="Sector",
                                     FillPalette="colorsX_Unassigned") %>%
                                     subset(.,select=-c(sector));
      df_bar<-rbind.data.frame(df_bar,watConsumBySec); head(df_bar)
      unique(watConsumBySec$Fill)
      
      tbl <- getQuery(queryData.proj, "water withdrawals by sector") # Tibble
      df<-as.data.frame(tbl);head(df)                # Data frame
      watWithdrawBySec<-df %>% mutate (x=year,
                                       xLabel="Year",
                                       param="watWithdrawBySec",
                                       NewValue=value,
                                       NewUnits="Water~Withdrawals~(km^3)", # Use ~ for spaces. Will be parsed later
                                       Fill=sector,
                                       FillLabel="Sector",
                                       FillPalette="colorsX_Unassigned") %>% 
                                       subset(.,select=-c(sector));
      df_bar<-rbind.data.frame(df_bar,watWithdrawBySec); head(df_bar)
      unique(watWithdrawBySec$Fill)
      
      tbl <- getQuery(queryData.proj, "water withdrawals by crop") # Tibble
      df<-as.data.frame(tbl);head(df)                # Data frame
      df<-df[df$sector!="industry" & df$sector!="mining" & df$sector!="municipal" 
             & df$sector!="electricity" & df$sector!="livestock",]
      watWithdrawByCrop<-df %>% mutate (x=year,
                                        xLabel="Year",
                                        param="watWithdrawByCrop",
                                        NewValue=value,
                                        NewUnits="Water~Withdrawals~(km^3)", # Use ~ for spaces. Will be parsed later
                                        Fill=sector,
                                        FillLabel="Crop",
                                        FillPalette="colorsX_Unassigned") %>% 
                                        subset(.,select=-c(sector));
      df_bar<-rbind.data.frame(df_bar,watWithdrawByCrop); head(df_bar)
      unique(watWithdrawByCrop$Fill)
      
      tbl <- getQuery(queryData.proj, "Ag Production by Crop Type") # Tibble
      df<-as.data.frame(tbl);head(df)              # Data frame
      df<-df[df$sector==df$output,]  # So that biomass is not double counted
      agProdByCropBiomass <- df %>% filter(Units=="EJ") %>% 
                                   mutate (x=year,
                                   xLabel="Year",
                                   param="agProdByCropBiomass",
                                   NewValue=value,
                                   NewUnits="Agricultural~Production~Biomass~(EJ)", # Use ~ for spaces. Will be parsed later
                                   Fill=output,
                                   FillLabel="Outputs",
                                   FillPalette="colorsX_Unassigned") %>%
                                   subset(.,select=-c(output,sector));
      df_bar<-rbind.data.frame(df_bar,agProdByCropBiomass); head(df_bar)
      unique(agProdByCropBiomass$Fill)
      
      agProdByForest <- df %>% filter(Units=="billion m3") %>% 
        mutate (x=year,
                xLabel="Year",
                param="agProdByCropForest",
                NewValue=value,
                NewUnits="Agricultural~Production~Forest~(billion~m^3)", # Use ~ for spaces. Will be parsed later
                Fill=output,
                FillLabel="Outputs",
                FillPalette="colorsX_Unassigned") %>%
        subset(.,select=-c(output,sector));
      df_bar<-rbind.data.frame(df_bar,agProdByForest); head(df_bar)
      unique(agProdByForest$Fill)
      
      agProdByCrop <- df %>% filter(Units=="Mt") %>% 
        mutate (x=year,
                xLabel="Year",
                param="agProdByCrop",
                NewValue=value,
                NewUnits="Agricultural~Production~Crop~(Mt)", # Use ~ for spaces. Will be parsed later
                Fill=output,
                FillLabel="Outputs",
                FillPalette="colorsX_Unassigned") %>%
        subset(.,select=-c(output,sector));
      df_bar<-rbind.data.frame(df_bar,agProdByCrop); head(df_bar)
      unique(agProdByCrop$Fill)
      
      
      tbl <- getQuery(queryData.proj, "water withdrawals by water mapping source") # Tibble
      df<-as.data.frame(tbl);head(df)              # Data frame
      df<-df[grepl("_irr_",df$input),]  # Only keep irrigation water items
      df$input<-gsub("water_td_irr_","",df$input);
      df$input<-gsub("_W","",df$input);head(df)
      wwIrrBasin<-df %>% mutate (x=year,
                                   xLabel="Year",
                                   param="irrWatWithBasin",
                                   NewValue=value,
                                   NewUnits="Irrigation~Water~Withdrawal~(km^3)", # Use ~ for spaces. Will be parsed later
                                   Fill=input,
                                   FillLabel="Basin",
                                   FillPalette="colorsX_Unassigned") %>%
        subset(.,select=-c(input));
      df_bar<-rbind.data.frame(df_bar,wwIrrBasin); head(df_bar)
      unique(wwIrrBasin$Fill)
      
      
      tbl <- getQuery(queryData.proj, "water consumption by water mapping source") # Tibble
      df<-as.data.frame(tbl);head(df)              # Data frame
      df<-df[grepl("_irr_",df$input),]  # Only keep irrigation water items
      df$input<-gsub("water_td_irr_","",df$input);
      df$input<-gsub("_C","",df$input);head(df)
      wcIrrBasin<-df %>% mutate (x=year,
                                 xLabel="Year",
                                 param="irrWatConsBasin",
                                 NewValue=value,
                                 NewUnits="Irrigation~Water~Consumption~(km^3)", # Use ~ for spaces. Will be parsed later
                                 Fill=input,
                                 FillLabel="Basin",
                                 FillPalette="colorsX_Unassigned") %>%
        subset(.,select=-c(input));
      df_bar<-rbind.data.frame(df_bar,wcIrrBasin); head(df_bar)
      unique(wcIrrBasin$Fill)
      
      tbl <- getQuery(queryData.proj, "aggregated land allocation") # Tibble
      df<-as.data.frame(tbl);head(df)              # Data frame
      landAll<-df %>% mutate (x=year,
                              xLabel="Year",
                              param="aggLandAlloc",
                              NewValue=value,
                              NewUnits="Land~Allocation~(km^2)", # Use ~ for spaces. Will be parsed later
                              Fill=landleaf,
                              FillLabel="Type",
                              FillPalette="colorsX_Unassigned") %>%
        subset(.,select=-c(landleaf));
      df_bar<-rbind.data.frame(df_bar,landAll); head(df_bar)
      unique(landAll$Fill)
    
      
      # Limit to selected analysis range
      df_line<-df_line[df_line$x<=max(range(rangeX)),]
      df_bar<-df_bar[df_bar$x<=max(range(rangeX)),]
      
      # Automatic Selection of all parameters
      unique(df_line$param);unique(df_bar$param) # List of all params
      params<-c(unique(df_line$param),unique(df_bar$param))  # For all Parameters
      
      
      
      # Create Output Directory
      if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/GCAMCharts",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/GCAMCharts",sep=""))}
      dir<-paste(wdfigsOut,"/",region_i,"/",scenario_i,"/GCAMCharts",sep="")
      
  
   
    #______________________
    # Plots
    #______________________
    
    
    for(param in params){
     
        #----------
        # Line Charts
        #-----------    
        
        l1<-df_line[(df_line$param==param & df_line$region==region_i),]; head(l1)
        
        if(nrow(l1)!=0){
          
        fname<-paste("line_GCAM_",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        p <- fig_LineSingle(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)}  
        plot(p)
        print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*0.7,figHeight_InchMaster,pdfpng=pdfpng)
        t1<-subset(l1,select=c(scenario,region,year,NewUnits,NewValue));head(t1)
        t1$NewUnits<-t1$NewUnits%>%gsub("~"," ",.)%>%gsub("\\^","",.);head(t1)
        colnames(t1)[which(names(t1) == "NewUnits")] <- "Parameter"
        write.csv(t1,file=paste(dir,"/table_GCAM_",region_i,"_",param,"_",min(range(l1$x)),"to",max(range(l1$x)),".csv",sep=""),row.names=F)
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}  
        }
        
        
        #----------
        # Bar Charts
        #-----------
        
        l1<-df_bar[(df_bar$param==param & df_bar$region==region_i),]; head(l1)
        
        if(nrow(l1)!=0){
          
        l1_sum<-aggregate(NewValue ~ Units+scenario+region+year+param+NewUnits, l1, sum) # To get sums over years
          
        fname<-paste("bar_GCAM_",param,"_",region_i,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        p <- fig_Bar(l1) + if(titleOn==1){ggtitle (paste(region_i,sep=""))}else{ggtitle(NULL)} 
        plot(p)
        print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
        
        t1<-subset(l1,select=c(scenario,region,year,NewUnits,NewValue,Fill));head(t1)
        t1$NewUnits<-t1$NewUnits%>%gsub("~"," ",.)%>%gsub("\\^","",.);head(t1)
        t1<-dcast(t1,scenario+region+year+NewUnits~Fill,value.var="NewValue",fun.aggregate=sum,na.rm=T);head(t1)
        colnames(t1)[which(names(t1) == "NewUnits")] <- "Parameter"
        write.csv(t1,file=paste(dir,"/table_GCAM_",region_i,"_",param,"_",min(range(l1$x)),"to",max(range(l1$x)),".csv",sep=""),row.names=F)
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))}
        
        }
        
    } #Close GCAM params
 
# Special plots for particular presentations
if(TRUE) {
  
  # For plotting Final & Primary Energy on same scale
  param<-"primNrgConsumByFuel"
  l1<-df_bar[(df_bar$param==param & df_bar$region==region_i),]; head(l1)
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

# Dissolve subbasins in Colombia to 41 Hydrological Zones
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

if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/Basemaps",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/Basemaps",sep=""))}
dir<-paste(wdfigsOut,"/",region_i,"/",scenario_i,"/Basemaps",sep="")

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


#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Tethys Water Demand Data
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________

if(runTethysMaps==1){

# Create Output Directory
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/TethysWatDem",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/TethysWatDem",sep=""))}
dir<-paste(wdfigsOut,"/",region_i,"/",scenario_i,"/TethysWatDem",sep="")
# subBasin directory
if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/TethysWatDem/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/TethysWatDem/subBasin",sep=""))}


wdtethys1<-paste(wdtethys,scenario_i,sep="") # Tethys data directory

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
moduleRemove<-c("Total","Non_Agriculture")   # Remove certain categories for particular modules. Make c("") if not needed

maps_ReAggregate(region_i=region_i,scenario_i=scenario_i,
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
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/DemeterLandUse",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/DemeterLandUse",sep=""))}
  dir<-paste(wdfigsOut,"/",region_i,"/",scenario_i,"/DemeterLandUse",sep="")
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/DemeterLandUse/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/DemeterLandUse/subBasin",sep=""))}
  
  wdDemeter1<-paste(wdDemeter,scenario_i,"/spatial_landcover_tabular",sep="") # Demeter data directory
  
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
  moduleRemove<-c("")   # Remove certain categories for particular modules. Make c("") if not needed
  
  maps_ReAggregate(region_i=region_i,scenario_i=scenario_i,
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
  moduleRemove<-c("")   # Remove certain categories for particular modules. Make c("") if not needed
  
  # Create Output Directory
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/XanthosWatSup",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/XanthosWatSup",sep=""))}
  dir<-paste(wdfigsOut,"/",region_i,"/",scenario_i,"/XanthosWatSup",sep="")
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/XanthosWatSup/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/XanthosWatSup/subBasin",sep=""))}
  
  wdXanthos1<-paste(wdXanthos,sep="") # Xanthos data directory
  
  xanthosCoords<-read.csv(paste(dirname(wdXanthos1),"/input/reference/coordinates.csv",sep=""), header=F, stringsAsFactors = F); head(xanthosCoords)
  xanthosCoords<-xanthosCoords[,2:3];head(xanthosCoords)
  names(xanthosCoords)<-c("lon","lat")
  
  
  df<-data.frame()
  # Read in Data Files
  df_q <- read.csv(paste(wdXanthos1,scenario_i,"/q_bced_1960_1999_ipsl-cm5a-lr_1950_2005.csv",sep=""), header=F, stringsAsFactors = F)
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
  
  
  maps_ReAggregate(region_i=region_i,scenario_i=scenario_i,
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

if(runScarcity==1){

  # Create Output Directory
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/Scarcity",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/Scarcity",sep=""))}
  dir<-paste(wdfigsOut,"/",region_i,"/",scenario_i,"/Scarcity",sep="")
  if(!dir.exists(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/Scarcity/subBasin",sep=""))){dir.create(paste(wdfigsOut,"/",region_i,"/",scenario_i,"/Scarcity/subBasin",sep=""))}


  # Tethys Data
  wdtethys1<-paste(wdtethys,scenario_i,sep="") # Tethys data directory
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
  df_q <- read.csv(paste(wdXanthos1,scenario_i,"/q_bced_1960_1999_ipsl-cm5a-lr_1950_2005.csv",sep=""), header=F, stringsAsFactors = F)
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

# Setup for running function maps_ReAggregaget
moduleName<-"scarcity"  # Name of module being run tethys, xanthos, scarcity etc.
moduleParam<-"scarcityFrac"  # Type of parameter for fig names demWatmmPerYr
moduleUnits<- "Fraction"            # Units used for figures
moduleTitleText<- "Water Scarcity"  # Title for figures. Used when titleOn is 1
moduleAggType<- "depth"      # "depth" when using mm or "vol" when using km3
moduleRemove<-c("")   # Remove certain categories for particular modules. Make c("") if not needed

maps_ReAggregate(region_i=region_i,scenario_i=scenario_i,
                 moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                 moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                 moduleRemove=moduleRemove)

} # Close Scarcity Run

#________________________________________________________________________________
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Selected Figures
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#________________________________________________________________________________

selectFigsparams<-gsub("Region",region_i,selectFigsparams);selectFigsparams
reportSelFigsName<-paste("Report_SelectedFigs_",region_i,"_",scenario_i,sep="");reportSelFigsName

if(dir.exists(paste(wdfigsOut,"/SelectedFigs/",reportSelFigsName,sep=""))){unlink(paste(wdfigsOut,"/SelectedFigs/",reportSelFigsName,sep=""),recursive=T)}
if(!dir.exists(paste(wdfigsOut,"/SelectedFigs/",reportSelFigsName,sep=""))){
  dir.create(paste(wdfigsOut,"/SelectedFigs",sep=""));dir.create(paste(wdfigsOut,"/SelectedFigs/",reportSelFigsName,sep=""))}

if(pdfpng!=1){selectedFigsPnG<-gsub(".pdf",".png",selectedFigs);selectedFigs<-c(selectedFigs,selectedFigsPnG)}
selectedFigs
        
selectedFigsX<-grep(paste(selectFigsparams, collapse="|"),selectedFigs,value=T);selectedFigsX
file.copy(selectedFigsX,to=paste(wdfigsOut,"/SelectedFigs/",reportSelFigsName,sep=""),overwrite=T,recursive=T)
}#Close Scenarios