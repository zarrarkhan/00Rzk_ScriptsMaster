#***********
# Contact: Zarrar Khan (zarrar.khan@pnnl.gov)
# Project: General 
# *********


#______________________
# Initial setup
#______________________

#rm(list=ls()) # Not used because this file is now sourced
#graphics.off()
memory.limit(size=1E+10)

#____________________________________________
#____________________________________________
# C. GIS FUNCTIONS
#____________________________________________
#____________________________________________


spatAgg_gridded2shape<- function(gridded=df3,shape=shpa1,byLev="NAME_1",boundBox=b1,
                                  moduleAggType=moduleAggType
                                  ) {

#gridded=df3;shape=shpa1;byLev="NAME_1";boundBox=b1;moduleAggType=moduleAggType
#gridded=df3;shape=shpsubBasin1;byLev="subBasin_name";boundBox=b1;moduleAggType=moduleAggType
  
colsx<-names(gridded@data%>%dplyr::select(-c(lat,lon)));colsx 
dxp<-data.frame()

r<-stack(gridded)
projection(r)<-proj4string(shape)
  
  if(moduleAggType=="depth"){
   
    rcrop<-raster::intersect(r,shape)
    rcropP<-rasterToPolygons(rcrop)
    rcropPx<-raster::intersect(shape,rcropP);
    rcropPx@data$area<-area(rcropPx); 
    head(rcropPx@data)
    s1<-shape
    s1$subRegAreaSum<-area(shape);head(s1)
    s1<-s1@data%>%dplyr::select(byLev,subRegAreaSum);head(s1)
    rcropPx@data<-join(rcropPx@data,s1,by=byLev);head(rcropPx)
    rcropPx@data$areaPrcnt<-rcropPx@data$area/rcropPx@data$subRegAreaSum;head(rcropPx)
    x<-data.frame(mapply(`*`,rcropPx@data%>%dplyr::select(colsx),rcropPx@data%>%dplyr::select(areaPrcnt),SIMPLIFY=FALSE))%>%
      cbind.data.frame(rcropPx@data%>%dplyr::select(byLev));head(x)
    dxpX<-x%>%group_by(.dots = list(byLev))%>% summarise_all(funs(round(sum(.,na.rm=T),2)))%>%as.data.frame 
    head(dxpX)
  }
  if(moduleAggType=="vol"){
    w <- raster::extract(r,shape, method="simple",weights=T, normalizeWeights=F);head(w)
    dfx<-data.frame()
    for (i in seq(w)){
      if(!is.null(w[[i]]))
      x<-as.data.frame(w[[i]])
      x$ID<-shape@data[[byLev]][[i]]
      x1<-data.frame(mapply(`*`,x%>%dplyr::select(colsx),x%>%dplyr::select(weight),SIMPLIFY=FALSE))%>%
        cbind.data.frame(x%>%dplyr::select(ID));head(x1)
      #assign(paste0("df", i), x)
      dfx<-rbind.data.frame(dfx,x1)
    }
    names(dfx)[names(dfx)=="ID"]<-byLev;head(dfx)
    dxpX<-dfx%>%group_by(.dots = list(byLev))%>% summarise_all(funs(round(sum(.,na.rm=T),2)))%>%as.data.frame 
    head(dxpX)
  }
  return(dxpX)
}

#--------------------------
# Main Mapping function - Grid, Admin Boundary, Basin
#--------------------------

maps_ReAggregate <- function(df=df,region_i=region_i,scenario_i=scenario_ix,
                              moduleName=moduleName,moduleParam=moduleParam,moduleUnits=moduleUnits,
                              moduleTitleText=moduleTitleText,moduleAggType=moduleAggType,
                              moduleRemove=moduleRemove){
  
  if(FALSE){ # Setting variables for manual run
    df=df;region_i=region_i;scenario_i=scenario_ix;moduleName=moduleName;moduleParam=moduleParam;
    moduleUnits=moduleUnits;moduleTitleText=moduleTitleText;moduleAggType=moduleAggType;
    moduleRemove=moduleRemove;}
  
  yearsOriginal<-years
  dfOriginal<-df
  
  if(TRUE){
  
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  # BY YEAR
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  
  if(meanYearOnly==1){years<-years[length(years)]}
  
  for (year_i in years) {
    
    
    df1<-df%>%dplyr::select(lat,lon,year_i,Type)
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
    dfxtra<-dfxtra
    
    dfCommonScaleYears<-df3@data%>%dplyr::select(-c(lat,lon,moduleRemove))
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<-1};
    head(dfCommonScaleYears) ;max(dfCommonScaleYears)
    
    #---------------------------------------------
    #----- For each year compare Demands by Sector
    #---------------------------------------------
    
    dfx<-df3
    dfx@data<-dfx@data%>%dplyr::select(unique(df$Type))  # Choose the Sectors
    if(!is.null(moduleRemove)){dfx@data<-dfx@data%>%dplyr::select(-moduleRemove)}
    
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_raster(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        map_fig_empty+if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i),sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_rasterKMeans(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        map_fig_empty+ if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i),sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #---------------------------------------------
    #----- For each year compare Demands by Sector Free Scale
    #---------------------------------------------
    
    dfx<-df3
    dfx@data<-dfx@data%>%dplyr::select(unique(df$Type))  # Choose the Sectors
    #dfx@data<-subset(dfx@data,select=-c(Total))  # Remove
    
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_rasterFreeScale(rasterBoundary=dfxtra,data=dfx) + map_fig_empty+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i),"\nFree Scale",sep=""))}
      map 
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map <- mapX_rasterFreeScaleKMeans(rasterBoundary=dfxtra,data=dfx) + map_fig_empty+
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
    dxpbyAdmin<-spatAgg_gridded2shape(gridded=df3,shape=shpa1,byLev="NAME_1",boundBox=b1,moduleAggType=moduleAggType)
    head(dxpbyAdmin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      write.csv(dxpbyAdmin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorByAdminRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
    }
    
    dfCommonScaleYears<-dxpbyAdmin%>%dplyr::select(-c(NAME_1,moduleRemove))
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<-1}
    head(dfCommonScaleYears) ;max(dfCommonScaleYears)
    
    #---------------------------------------------
    #----- For each year_i compare Demands by Sector (By Admin Region)
    #---------------------------------------------
    
    shpa.x<-shpa1
    shpa.x@data<-join(shpa.x@data,dxpbyAdmin,by=c("NAME_1")) %>% 
      dplyr::select(unique(df$Type),"NAME_1") %>%dplyr::select(-c(NAME_1));
    if(!is.null(moduleRemove)){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-map_fig_filled+mapX_fill(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Provinces",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng)
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-map_fig_filled+mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
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
      dplyr::select(unique(df$Type),"NAME_1") %>% subset(select=-c(NAME_1))
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-map_fig_filled+mapX_fillFreeScale(data=dfx)+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Provinces Free Scale",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-map_fig_filled+mapX_fillFreeScaleKMeans(data=dfx)+
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
    dxpbyBasin<-spatAgg_gridded2shape(gridded=df3,shape=shpbasin1,byLev="basin_name",boundBox=b1,moduleAggType=moduleAggType)
    head(dxpbyBasin)
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      write.csv(dxpbyBasin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorByBasinRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
    }
    
    dfCommonScaleYears<-dxpbyBasin%>%dplyr::select(-c(basin_name,moduleRemove))
    if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<-1}
    
    #---------------------------------------------
    #----- For each year_i compare Demands by Sector (By Basin Region)
    #---------------------------------------------
    
    shpa.x<-shpbasin1
    shpa.x@data<-join(shpa.x@data,dxpbyBasin,by=c("basin_name")) %>% 
      dplyr::select(unique(df$Type),"basin_name") %>% dplyr::select(-basin_name);
    if(!is.null(moduleRemove)){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-map_fig_filled+mapX_fill(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Basins",sep=""))}
      map
      print_PDFPNG(map,dir=dir,fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-map_fig_filled+mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
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
      dplyr::select(unique(df$Type),"basin_name") %>% dplyr::select(-basin_name)
    dfx<-shpa.x
    
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{get("fname")})){
      map<-map_fig_filled+mapX_fillFreeScale(data=dfx)+
        if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," Basin Free Scale",sep=""))}
      map
      print_PDFPNG(map,dir=dir,filename=fname,
                   figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
      selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
    }
    
    #KMEANS
    fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),"_KMEANS",sep="")
    if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
      map<-map_fig_filled+mapX_fillFreeScaleKMeans(data=dfx)+
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
      
      dxpbysubBasin<-spatAgg_gridded2shape(gridded=df3,shape=shpsubBasin1,byLev="subBasin_name",boundBox=b1,moduleAggType=moduleAggType)
      head(dxpbysubBasin)
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        write.csv(dxpbysubBasin,file=paste(dir,"/subBasin/table_",moduleName,"_",region_i,"_",moduleParam,"_BySectorBysubBasinRegion_",gsub("X","",year_i),".csv",sep=""),row.names=F)
      }
      
      dfCommonScaleYears<-dxpbysubBasin%>%dplyr::select(-c(subBasin_name,moduleRemove))
      if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<-1}
      
      #---------------------------------------------
      #----- For each year_i compare Demands by Sector (By Admin Region)
      #---------------------------------------------
      
      shpa.x<-shpsubBasin1
      shpa.x@data<-join(shpa.x@data,dxpbysubBasin,by=c("subBasin_name")) %>% 
        dplyr::select(unique(df$Type),"subBasin_name") %>% dplyr::select(-subBasin_name);
      if(!is.null(moduleRemove)){shpa.x@data<-shpa.x@data%>%dplyr::select(-moduleRemove)}
      dfx<-shpa.x
      
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-map_fig_filled+mapX_fill(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
          if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," subBasins",sep=""))}
        map
        print_PDFPNG(map,dir=dir,filename=fname,figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
      #KMEANS
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_",gsub("X","",year_i),"_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-map_fig_filled+mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears)+tm_legend(title=paste(gsub("X","",year_i)," (",moduleUnits,")",sep=""))+
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
        dplyr::select(unique(df$Type),"subBasin_name") %>% dplyr::select(-subBasin_name)
      dfx<-shpa.x
      
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-map_fig_filled+mapX_fillFreeScale(data=dfx)+
          if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," subBasin Free Scale",sep=""))}
        map
        print_PDFPNG(map,dir=dir,filename=fname,
                     figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
      #KMEANS
      fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_BySector_FREESCALE_",gsub("X","",year_i),"_KMEANS",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-map_fig_filled+mapX_fillFreeScaleKMeans(data=dfx)+
          if(titleOn==1){tm_layout(main.title=paste(region_i," ",moduleTitleText," by Sector ",gsub("X","",year_i)," subBasin Free Scale",sep=""))}
        map
        print_PDFPNG(map,dir=dir,filename=fname,
                     figWidth_Inch=mapWidthInch,figHeight_Inch=mapHeightInch,pdfpng=pdfpng);
        selectedFigs<-c(selectedFigs,paste(dir,"/",fname,".pdf",sep=""))
      }
      
    } # Close bySubBasin loop
    
  } # End Year Loop
  
  
  } # FALSE for testing
  
  
  if(TRUE){ # Testing
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
  df<-dfOriginal
  
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
  df1ScaleGridLatLon<- df3@data%>%dplyr::select(-c(Type))%>%
    group_by(lat,lon) %>% 
    summarise_all(funs(max(.,na.rm=T))) %>% as.data.frame
  head(df1ScaleGridLatLon)
  
  df1ScaleGrid<-df1ScaleGridLatLon %>% dplyr::select(-c(lat,lon))
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
  df1ScaleAdmin<-spatAgg_gridded2shape(gridded=df3,shape=shpa1,byLev="NAME_1",boundBox=b1,
                                        moduleAggType=moduleAggType)
  df1ScaleAdmin<-df1ScaleAdmin%>%dplyr::select(-contains("NAME_1"))
  df1ScaleAdmin<-df1ScaleAdmin%>%dplyr::select(-contains("Mean"))
  head(df1ScaleAdmin);max(df1ScaleAdmin[,2:ncol(df1ScaleAdmin)])
  
  #------------------
  # Basin 1 Scale
  #-----------------
  
  df1ScaleBasin<-spatAgg_gridded2shape(gridded=df3,shape=shpbasin1,byLev="basin_name",boundBox=b1,
                                        moduleAggType=moduleAggType)
  df1ScaleBasin<-df1ScaleBasin%>%dplyr::select(-contains("basin_name"))
  df1ScaleBasin<-df1ScaleBasin%>%dplyr::select(-contains("Mean"))
  head(df1ScaleBasin);max(df1ScaleBasin[,2:ncol(df1ScaleBasin)])
  
  if(bySubBasin==1){
    #------------------
    # subBasin 1 Scale
    #----------------
    
    df1ScalesubBasin<-spatAgg_gridded2shape(gridded=df3,shape=shpsubBasin1,byLev="subBasin_name",boundBox=b1,
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
      dfxtra<-raster::intersect(df1,b1);plot(dfxtra) #create larger boundary extents for plotting in tmap
      gridded(dfxtra)<-TRUE
      df3<-df2
      gridded(df3)<-TRUE # Create Gridded Data
      dfxtra<-dfxtra
      
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
        dfCommonScaleYears<-dfCommonScaleYearsAllTypesTotal%>%
          dplyr::filter(Type == type)%>%
          dplyr::select(-c(lat,lon,Type))}else{
            dfCommonScaleYears<-dfCommonScaleYearsAllTypes%>%dplyr::filter(!(Type %in% moduleRemove))%>%
              dplyr::filter(Type == type)%>%
              dplyr::select(-c(lat,lon,Type))}
      if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<-1}; max(dfCommonScaleYears)
      
      years<-yearsOriginal;
      # Gridded Boundary
      df1<-df%>%dplyr::select("lat","lon",years,"Type")
      df1<-df1[df1$Type==type,]
      df1<-df1%>%dplyr::select(-Type)
      
      # Convert to Spatial Point Data Frames
      df1 = SpatialPointsDataFrame(SpatialPoints(coords=(cbind(df1$lon,df1$lat))),data=df1)
      proj4string(df1)<-projX
      
      # Crop to the Regional file shpa boundary
      df2<-raster::intersect(df1,b1);plot(df2)  # Crop to Layer shpa
      dfxtra<-raster::intersect(df1,b1);plot(dfxtra) #create larger boundary extents for plotting in tmap
      gridded(dfxtra)<-TRUE
      df3<-df2
      gridded(df3)<-TRUE # Create Gridded Data
      dfxtra<-dfxtra
      
      years<-yearsOriginal;
      dfx<-df3
      dfx@data<-dfx@data%>%dplyr::select(years)  # Choose the years
      dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
      
      fname<-paste("map_",moduleName,"_grid_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<-mapX_raster(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScaleYears)+map_fig_empty+tm_legend(title=moduleUnits)+
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
          map<-mapX_rasterKMeans(rasterBoundary=dfxtra,data=dfx,scaleData=dfCommonScaleYears)+map_fig_empty+tm_legend(title=moduleUnits)+
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
          map<- mapX_raster(rasterBoundary=dfxtra,data=dfx,scaleData=dfgridX) + map_fig_empty +tm_legend(title=moduleUnits)+
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
            map<- mapX_rasterKMeans(rasterBoundary=dfxtra,data=dfx,scaleData=dfgridX) + map_fig_empty +tm_legend(title=moduleUnits)+
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
      
      dxpbyAdmin<-spatAgg_gridded2shape(gridded=df3,shape=shpa1,byLev="NAME_1",boundBox=b1,moduleAggType=moduleAggType)
      head(dxpbyAdmin)
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        write.csv(dxpbyAdmin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_ByAdminRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
      }
      
      dfCommonScaleYears<-dxpbyAdmin%>%dplyr::select(-c(NAME_1));max(dfCommonScaleYears)
      if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<-1}
      
      shpa.x<-shpa1
      shpa.x@data<-join(shpa.x@data,dxpbyAdmin,by=c("NAME_1")) %>% 
        subset(select=c(years,"NAME_1")) %>% subset(select=-c(NAME_1))
      dfx<-shpa.x
      dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
      
      fname<-paste("map_",moduleName,"_polyAdmin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<- map_fig_filled + mapX_fill(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
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
          map<- map_fig_filled + mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
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
          map<- map_fig_filled + mapX_fill(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
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
            map<- map_fig_filled + mapX_fillKMeans(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
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
      
      dxpbyBasin<-spatAgg_gridded2shape(gridded=df3,shape=shpbasin1,byLev="basin_name",boundBox=b1,moduleAggType=moduleAggType)
      head(dxpbyBasin)
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        write.csv(dxpbyBasin,file=paste(dir,"/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_ByBasinRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
      }
      
      dfCommonScaleYears<-dxpbyBasin%>%dplyr::select(-c(basin_name))
      if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<-1}
      
      shpa.x<-shpbasin1
      shpa.x@data<-join(shpa.x@data,dxpbyBasin,by=c("basin_name")) %>% 
        subset(select=c(years,"basin_name")) %>% subset(select=-c(basin_name))
      dfx<-shpa.x
      dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
      
      fname<-paste("map_",moduleName,"_polyBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE",sep="")
      if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
        map<- map_fig_filled + mapX_fill(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
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
          map<- map_fig_filled + mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
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
          map<- map_fig_filled + mapX_fill(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
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
            map<- map_fig_filled + mapX_fillKMeans(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
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
        
        dxpbysubBasin<-spatAgg_gridded2shape(gridded=df3,shape=shpsubBasin1,byLev="subBasin_name",boundBox=b1,moduleAggType=moduleAggType)
        head(dxpbysubBasin)
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          write.csv(dxpbysubBasin,file=paste(dir,"/subBasin/table_",moduleName,"_",region_i,"_",moduleParam,"_",type,"_BysubBasinRegion_",min(rangeX),"to",max(rangeX),".csv",sep=""),row.names=F)
        }
        
        dfCommonScaleYears<-dxpbysubBasin%>%dplyr::select(-c(subBasin_name))
        if(moduleName=="scarcity"){dfCommonScaleYears[dfCommonScaleYears > 1]<-1}
        
        shpa.x<-shpsubBasin1
        shpa.x@data<-join(shpa.x@data,dxpbysubBasin,by=c("subBasin_name")) %>% 
          subset(select=c(years,"subBasin_name")) %>% subset(select=-c(subBasin_name))
        dfx<-shpa.x
        dfx@data<-dfx@data%>%dplyr::select(-contains("Mean"))  # Remove mean year
        
        fname<-paste("subBasin/map_",moduleName,"_polysubBasin_",region_i,"_",moduleParam,"_",type,"_",min(rangeX),"to",max(rangeX),"_OWNSCALE",sep="")
        if(gsub(".*/","",fname) %in% (if(selectFigsOnly==1){selectFigsparams}else{gsub(".*/","",fname)})){
          map<- map_fig_filled + mapX_fill(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
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
            map<- map_fig_filled + mapX_fillKMeans(data=dfx,scaleData=dfCommonScaleYears) + tm_legend(title=moduleUnits) +
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
            map<- map_fig_filled + mapX_fill(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
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
              map<- map_fig_filled + mapX_fillKMeans(data=dfx,scaleData=dfgridX) + tm_legend(title=moduleUnits) +
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
  } # Close Testing Type
} # Close Mapping Function
