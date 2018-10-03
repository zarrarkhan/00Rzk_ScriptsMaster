#***********
# Contact: Zarrar Khan (zarrar.khan@pnnl.gov)
# Project: IDB
# File: 
# *********

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

# Map PDF details
mapWidthInch<<-10
mapHeightInch<<-8

#______________________
# New Functions
#______________________

if(FALSE){ # If running as independent
  
  regionAnalysis<<-c("India")
  
  wd0<<-getwd();wd0
  wdfigsOut<<-paste(wd0,"/fig_outputs",sep="");wdfigsOut;
  if(!dir.exists(wdfigsOut)){dir.create(wdfigsOut)}  # Output Directory
  wdsp<<-"D:/00SpatialData"  # Spatial Data folder Check in rgcam_FiguresMASTER.R to see if all spatial files are in this folder
  wdScripts<<-"D:/rJGCRI_ChartsMaps"
  
  source(paste(wdScripts,"/rJGCRI_libs.R",sep=""))
  source(paste(wdScripts,"/color_schemes.R",sep=""))  # From the GCAm R diagnostics package
  source(paste(wdScripts,"/rJGCRI_ChartsMaps.R",sep=""))
  source(paste(wdScripts,"/rJGCRI_gis.R",sep=""))
  
}

if(reReadMaps==1){if(dir.exists(paste(wdfigsOut,"/Boundaries",sep=""))){unlink(paste(wdfigsOut,"/Boundaries",sep=""),recursive=T)}}
if(!dir.exists(paste(wdfigsOut,"/Boundaries",sep=""))){dir.create(paste(wdfigsOut,"/Boundaries",sep=""))}
dir<-paste(wdfigsOut,"/Boundaries",sep="")


if(file.exists(paste(dir,"/MapData.RData",sep=""))){load(paste(dir,"/MapData.RData",sep=""))}

if(!exists("shp_wdspne10mAdmin0") |
   !exists("shp_PNNL32Reg") |
   !exists("shp_PNNL235CLM5ArcMin_multi") |
   !exists("shp_SIACHydroZones") |
   !exists("shp_HydroBasinsLev3") |
   !exists("shp_HydroBasinsLev4") |
   !exists("shp_gadm36L1") | !exists("projX")){
  
if(file.exists(paste(dir,"/MapData.RData",sep=""))){unlink(paste(dir,"/MapData.RData",sep=""),recursive=T)}

#______________________
# Working Direcotries and Data
#______________________

# Supporting shapefiles directories (Admin/Basin)
wdspne10mAdmin0<<-paste(wdsp,"/naturalEarth/ne_10m_admin_0_countries_lakes",sep="");wdspne10mAdmin0
wdspPNNL_CV<<-paste(wdsp,"/boundaries_PNNLChrisVernon/shp",sep="");wdspPNNL_CV 
wdspGADM<<-paste(wdsp,"/gadm",sep="");wdspGADM
wdspSIAChydrozones<<-paste(wdsp,"/Zonificacion_hidrografica_2013",sep="");wdspSIAChydrozones
wdspHydroBasins<<-paste(wdsp,"/HydroBASINS/hydrobasins_processed",sep="");wdspHydroBasins

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

# Simplify polygons
# https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
shp_HydroBasinsLev4 <- gBuffer(shp_HydroBasinsLev4, byid=TRUE, width=0)
shp_HydroBasinsLev4<<-spTransform(shp_HydroBasinsLev4, CRS(projX))
# Check any bad polys?
#sum(gIsValid(shp_HydroBasinsLev4, byid=TRUE)==FALSE)  

save(shp_wdspne10mAdmin0,
     shp_PNNL32Reg,
     shp_PNNL235CLM5ArcMin_multi,
     shp_SIACHydroZones
     ,shp_HydroBasinsLev3
     ,shp_HydroBasinsLev4
     ,shp_gadm36L1,projX, file = paste(dir,"/MapData.RData",sep=""))

}


for (region_i in regionAnalysis){
#________________________________________________________________________________
#--------------------------------------------------------------------------------
# Base Maps
#--------------------------------------------------------------------------------
#________________________________________________________________________________


#--- Choose base boundary files (Province, country, basins)

shp0<<-shp_wdspne10mAdmin0
shp<<-shp_gadm36L1    
names(shp@data)[names(shp@data)=="NAME_0"]<-"GCAM_region"
shp1<<-shp_gadm36L1  
shp_basins<<-shp_PNNL235CLM5ArcMin_multi  

if(region_i %in% unique(shp0$ADMIN)) {

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
# Maps For Figures 
#---------------------

map_fig_filled<<- tm_shape(shpc1x) + 
  tm_fill("gray90",alpha=0.8,style="pretty",palette="Set3",title=paste("Country",sep=""),legend.show = F)  +
  tm_shape(shpb1x) + tm_text("ADMIN",scale=0.6,auto.placement=F,col="grey") + 
  tm_borders("black",lwd=1,lty=1) +
  tm_layout(frame = T, bg.color="lightcyan") + tm_layout_z
if(titleOn==1){m7<<-m7 + tm_layout(main.title=paste(region_i," Regional Map",sep=""))};map_fig_filled
fname<<-paste("map_basemaps_",region_i,"_0ForFigures_filled",sep="")
print_PDFPNG(map_fig_filled,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)


map_fig_empty<<-tm_shape(shpb1x) + tm_text("ADMIN",scale=0.6,auto.placement=F,col="grey") + 
  tm_borders("black",lwd=1,lty=1) + tm_layout(frame = T)+tm_layout_z
  if(titleOn==1){m7<<-m7 + tm_layout(main.title=paste(region_i," Regional Map",sep=""))};map_fig_empty
fname<<-paste("map_basemaps_",region_i,"_0ForFigures_empty",sep="")
print_PDFPNG(map_fig_empty,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)


#---------------------
# Base maps - Admin Boundaries Used 
#---------------------

#-----------------------  GCAM vs Other Boundaries

#-----------------------  Admin Boundaries

m<-tm_shape(shpa1) + 
  tm_fill("white",legend.show=F) +
  tm_borders("gray20",lwd=1, lty=1)+
  tm_layout(frame = F, bg.color="white")+ tm_layout_z;
if(titleOn==1){m<-m + tm_layout(main.title=paste(region_i," State Map",sep=""))};m
fname<<-paste("map_basemaps_",region_i,"_Admin_Empty",sep="")
print_PDFPNG(m,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)

m<-tm_shape(shpa1) + 
  tm_fill("white",legend.show=F) +
  tm_borders("gray20",lwd=1, lty=1) +
  tm_text("NAME_1",scale=0.7,auto.placement=F, col="black") +
  tm_layout(frame = F, bg.color="white")+ tm_layout_z;
if(titleOn==1){m<-m + tm_layout(main.title=paste(region_i," State Map",sep=""))};m
fname<<-paste("map_basemaps_",region_i,"_Admin_Empty_Labels",sep="")
print_PDFPNG(m,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)

#-----------------------  Basin Boundaries

m<-tm_shape(shpbasin1) + 
  tm_fill("white",legend.show=F) +
  tm_borders("gray20",lwd=1, lty=1)+
  tm_layout(frame = F, bg.color="white")+ tm_layout_z;
if(titleOn==1){m<-m + tm_layout(main.title=paste(region_i," State Map",sep=""))};m
fname<<-paste("map_basemaps_",region_i,"_Basin_Empty",sep="")
print_PDFPNG(m,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)

m<-tm_shape(shpbasin1) + 
  tm_fill("white",legend.show=F) +
  tm_borders("gray20",lwd=1, lty=1) +
  tm_text("basin_name",scale=0.7,auto.placement=F, col="black") +
  tm_layout(frame = F, bg.color="white")+ tm_layout_z;
if(titleOn==1){m<-m + tm_layout(main.title=paste(region_i," State Map",sep=""))};m
fname<<-paste("map_basemaps_",region_i,"_Basin_Empty_Labels",sep="")
print_PDFPNG(m,dir=dir,filename=fname,figWidth_InchMaster*0.5,figHeight_InchMaster*1,pdfpng=pdfpng)

#-----------------------  Sub-Basin Boundaries

if(bySubBasin==1){
  
}

#------------------
} # Close if Region exists Loop
} # Close Regions

