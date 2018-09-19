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


spatAgg_gridded2shape<<- function(gridded=df3,shape=shpa1,byLev="NAME_1",boundBox=b1,
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
