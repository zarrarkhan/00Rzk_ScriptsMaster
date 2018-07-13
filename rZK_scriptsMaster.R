#***********
# Contact: Zarrar Khan (zarrar.khan@pnnl.gov)
# Project: General 
# File: rZK_scriptsMaster.R
# *********


#______________________
# Initial setup
#______________________

#rm(list=ls()) # Not used because this file is now sourced
#graphics.off()
memory.limit(size=1E+10)

#______________________
# A.2 Libraries
#______________________

# If you don't have dev tools
#install.packages("devtools") 
# Rtools https://cran.r-project.org/bin/windows/Rtools/ 
# devtools::install_github("hadley/devtools"))

# Install libraries
packagesX<-c("ggplot2","RColorBrewer","reshape2","magrittr","plyr","dplyr","tools","scales","rgdal",
             "rgeos","raster","tmap","animation","tis","tibble","here")
for(i in packagesX){if(i %in% rownames(installed.packages()) == FALSE) {install.packages(i)};library(i,character.only = TRUE)}

#Additional Libraries
if("rgcam" %in% rownames(installed.packages()) == FALSE) {devtools::install_github('JGCRI/rgcam')};library(rgcam)

# Download Pakcage ncdf4 zip file from http://cirrus.ucsd.edu/~pierce/ncdf/
if(!file.exists("D:/software/ncdf4_1.12.zip")){print("ncdf zip package not available. Donwload and keep in folder.")}
if("ncdf4" %in% rownames(installed.packages()) == FALSE){install.packages("D:/software/ncdf4_1.12.zip", repos = NULL)};library(ncdf4)


#-------- Print pdf and png figures
print_PDFPNG <<- function(figure,dir,filename, figWidth_Inch=mapWidthInch, figHeight_Inch=mapHeightInch,pdfpng=pdfpng){
  
  if(pdfpng=='pdf'){pdf(paste(dir,"/",filename,".pdf",sep=""),width=figWidth_Inch,height=figHeight_Inch)
    print(figure)
    dev.off()}
  if(pdfpng=='png'){png(paste(dir,"/",filename,".png",sep=""),width=figWidth_Inch, height=figHeight_Inch, units="in",res=300)
    print(figure)
    dev.off()}
  if(pdfpng=='both'){
    pdf(paste(dir,"/",filename,".pdf",sep=""),width=figWidth_Inch,height=figHeight_Inch)
    print(figure)
    dev.off()
    png(paste(dir,"/",filename,".png",sep=""),width=figWidth_Inch, height=figHeight_Inch, units="in",res=300)
    print(figure)
    dev.off()
  }
}

#-------- ANIMATED TMAPS animation_tmap https://rdrr.io/cran/tmap/man/animation_tmap.html
animation_tmapZ <- function(tm, filename="animation.gif", width=NA, height=NA, delay=60){
  checkIM <- system("cmd.exe",input="magick -version")
  if (checkIM!=0) stop("Could not find ImageMagick. Make sure it is installed and included in the systems PATH")
  d <- paste(getwd(), "/tmap_plots", sep="")   #------------ Create Folder for plots
  dir.create(d, showWarnings = FALSE)
  save_tmap(tm+tm_facets(nrow=1,ncol=1,free.scales=F), filename = paste(d, "plot%03d.png", sep="/"), width=width, height=height) #------------ In tmap tm_facets MAKE SURE nrow/ncol=1, tm_facets(free.scales=FALSE,nrow=1,ncol=1) 
  processed <- system("cmd.exe",input=paste("magick -delay ", delay, " ", d, "/*.png \"", filename, "\"", sep=""))
  unlink(d, recursive = TRUE) #-------------- cleaning up plots and temporary variables
  invisible()
}



# ggplot2 Theme
z_theme <<- theme_bw() + 
  theme(
    text =                element_text(family = NULL, face = "plain",colour = "black", size = 24 ,hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9)
    , axis.text.x =       element_text(size=24)
    , axis.text.y =       element_text(size=24)
    ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
    ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
    ,legend.key =         element_blank()
    ,legend.key.size =    unit(1.5, 'lines')
    ,legend.text =        element_text(size = rel(1.0), colour = "black")
    ,legend.title =       element_text(size = rel(1.2), face = NULL, hjust = 0, colour = "black")
    #,strip.background =   element_rect(fill = NA, colour = "black")
    ,plot.margin =        unit(c(1, 1, 1, 1), "lines")
    ,plot.title=          element_text(face="bold", hjust=0,size=20,margin = margin(b=20))
  )

# tmap plot formats
tm_layout_z <- tm_layout(main.title.position="left",main.title.size=1.5,
                         inner.margins = rep(0,4),outer.margin=rep(0.05,4))


#______________________
# A.2 Libraries
#______________________


source(paste(here(),"/color_schemes.R",sep=""))  # From the GCAm R diagnostics package

# Custom Colors for large unassigned palettes
# Custom Colors https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colorsX_Unassigned= unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


# Color scheme for electricity generation by aggregate fuel
colorsX_elec_subsec <- c( "coal" = "#a0237c","gas" = "#25a9e0","oil" = "#d01c2a","biomass" = "#00931d",
                          "nuclear" = "#ef8e27",
                          "geothermal" = "#ad440c",
                          "hydro" = "#fdfa28",
                          "wind" = "#3d86f9",
                          "solar" = "#fdd67b",
                          "refined liquids" = "#507fab",
                          "electricity" = "grey40")

pdf(paste(wdfigsOut,"/ColorsCheck_colorsX_elec_subsec.pdf",sep=""))  # Print Colors for reference
pie(rep(1,length(colorsX_elec_subsec)),label=names(colorsX_elec_subsec),col=colorsX_elec_subsec)
dev.off()

colorsX_elec_techs <- c( "Gen_III" = "#af8e27",
                         "Gen_II_LWR" = "#ef8e27",
                         "CSP" = "#cdd67b",
                         "PV" = "#fdd67b",
                         "biomass (IGCC CCS)" = "#00a31d",
                         "biomass (IGCC)" = "#00c31d",
                         "biomass (conv CCS)" = "#00e31d",
                         "biomass (conv)" = "#00f31d",
                         "coal (IGCC CCS)" = "#c0237c",
                         "coal (IGCC)" = "#f0237c",
                         "coal (conv pul CCS)" = "#e0237c",
                         "coal (conv pul)" = "#a0237c",
                         "gas (CC CCS)" = "#25c9e0",
                         "gas (CC)" = "#25a9e0",
                         "gas (CT)" = "#25e9e0",
                         "gas (steam)" = "#25f9e0",
                         "hydro" = "#fdfa28",
                         "refined liquids (CC CCS)" = "#000000",
                         "refined liquids (CC)" = "#000000",
                         "refined liquids (CT)" = "#000000",
                         "refined liquids (steam)" = "#000000",
                         "wind" = "#0000ff",
                         "red","green",unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))


pie(rep(1,length(colorsX_elec_techs)),label=names(colorsX_elec_techs),col=colorsX_elec_techs)
pdf(paste(wdfigsOut,"/ColorsCheck_colorsX_elec_techs.pdf",sep=""))  # Print Colors for reference
pie(rep(1,length(colorsX_elec_techs)),label=names(colorsX_elec_techs),col=colorsX_elec_techs)
dev.off()


colorsX_elec_sec<-c( "elect_td_bld" = "#ff230e",
                     "elect_td_ind" = "darkolivegreen4",
                     "elect_td_trn" = "dodgerblue",
                     "electricity" = "#ea9219")

pdf(paste(wdfigsOut,"/ColorsCheck_colorsX_elec_sec.pdf",sep=""))  # Print Colors for reference
pie(rep(1,length(colorsX_elec_sec)),label=names(colorsX_elec_sec),col=colorsX_elec_sec)
dev.off()


colorsX_finalNrg_sec<-c( "building" = "#facda4",
                         "comm non-building" = "#ff230e",
                         "industry" = "#cef4d1",
                         "transportation" = "#d0f6f7")

#____________________________________________
#____________________________________________
# C. Chart and map Functions
#____________________________________________
#____________________________________________

fig_dispatchCurve<- function(l1){
  
  paletteX<-get(l1$FillPalette);
  if(useNewLabels==1){
    if(!is.null(names(paletteX))){
      names(paletteX)<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",names(paletteX)))}
    l1$Fill<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",l1$Fill))
  }
  
  p <- ggplot(l1, aes(ymin = 0))
  p <- p + geom_rect(aes(xmin = wm, xmax = w,ymax = Cost, fill = Fill)) + z_theme
  p <- p + guides(fill = guide_legend(color=NULL,reverse=T,title=unique(l1$FillLabel)))
  p <- p + xlab(eval(parse(text=unique(l1$NewUnits)))) + ylab(eval(parse(text=unique(l1$CostUnits))))
  p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  p <- p + scale_fill_manual(values=paletteX)
}

fig_LineCompareParam<- function(l1){
  #l1<-l1[order(compare), ]
  p<-ggplot(l1,aes(x=x,y=NewValue,group=Title))
  p<-p + z_theme
  p<-p + geom_line(aes(color=Title),stat="identity",position="identity",size=2) 
  p<-p + guides(fill = guide_legend(override.aes = list(colour = NULL)))
  p<-p + xlab(unique(l1$xLabel)) + ylab(eval(parse(text=unique(l1$NewUnits))))
  p<-p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  p<-p + scale_x_continuous (breaks=(seq(min(range(l1$x))-breakx_majMaster,max(range(l1$x))+breakx_majMaster,by=breakx_majMaster)), 
                             minor_breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_minMaster)),expand=c(0,breakx_majMaster/2))
  p<-p + scale_y_continuous(breaks = pretty_breaks(n = prettyBreaksyMaster)) 
  p <- p + scale_color_brewer(palette="Set1",name="Parameter")
}

fig_LineCompareScenario<- function(l1){
  #l1<-l1[order(compare), ]
  p<-ggplot(l1,aes(x=x,y=NewValue,group=scenario))
  p<-p + z_theme
  p<-p + geom_line(aes(color=scenario),stat="identity",position="identity",size=2) 
  p<-p + guides(fill = guide_legend(override.aes = list(colour = NULL)))
  p<-p + xlab(unique(l1$xLabel)) + ylab(eval(parse(text=unique(l1$NewUnits))))
  p<-p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  p<-p + scale_x_continuous (breaks=(seq(min(range(l1$x))-breakx_majMaster,max(range(l1$x))+breakx_majMaster,by=breakx_majMaster)), 
                             minor_breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_minMaster)),expand=c(0,breakx_majMaster/2))
  p<-p + scale_y_continuous(breaks = pretty_breaks(n = prettyBreaksyMaster)) 
  p <- p + scale_color_brewer(palette="Set1",name="Scenario")
}


fig_BarSingle<-function(l1){
  
  p <- ggplot(l1,aes(x=x,y=NewValue,group=scenario),fill="gray80",color="gray20")
  p <- p + z_theme
  p <- p + geom_bar(aes(), stat="identity",position=position_dodge(width=2))
  p <- p + guides(fill = guide_legend(color=NULL,reverse=T,title=unique(l1$FillLabel)))
  p <- p + xlab(unique(l1$xLabel)) + ylab(eval(parse(text=unique(l1$NewUnits))))
  p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  p <- p + scale_x_continuous (breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_majMaster)), 
                               minor_breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_minMaster)),expand=c(0,breakx_majMaster/2))
  p <- p +  scale_y_continuous(breaks = pretty_breaks(n = prettyBreaksyMaster))
}


fig_LineSingle<- function(l1){
  p<-ggplot(l1,aes(x=x,y=NewValue,group=scenario))
  p<-p + z_theme
  p<-p + geom_line(aes(),color="red",stat="identity",position="identity",size=2) 
  p<-p + guides(fill = guide_legend(override.aes = list(colour = NULL)))
  p<-p + xlab(unique(l1$xLabel)) + ylab(eval(parse(text=unique(l1$NewUnits))))
  p<-p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  p<-p + scale_x_continuous (breaks=(seq(min(range(l1$x))-breakx_majMaster,max(range(l1$x))+breakx_majMaster,by=breakx_majMaster)), 
                             minor_breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_minMaster)),expand=c(0,breakx_majMaster/2))
  p<-p + scale_y_continuous(breaks = pretty_breaks(n = prettyBreaksyMaster))}


fig_Bar<-function(l1){
  
  paletteX<-get(l1$FillPalette);
  if(useNewLabels==1){
    if(!is.null(names(paletteX))){
      names(paletteX)<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",names(paletteX)))}
    l1$Fill<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",l1$Fill))
  }
  
  p <- ggplot(l1[order((l1$Fill)),],aes(x=x,y=NewValue,group=scenario, fill=Fill))
  p <- p + z_theme
  p <- p + geom_bar(aes(), stat="identity",position="stack")
  p <- p + guides(fill = guide_legend(color=NULL,reverse=T,title=unique(l1$FillLabel)))
  p <- p + xlab(unique(l1$xLabel)) + ylab(eval(parse(text=unique(l1$NewUnits))))
  p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  p <- p + scale_x_continuous (breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_majMaster)), 
                               minor_breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_minMaster)),expand=c(0,breakx_majMaster/2))
  p <- p +  scale_y_continuous(breaks = pretty_breaks(n = prettyBreaksyMaster))
  p <- p + scale_fill_manual(values=paletteX)
}


fig_LineMultiple<-function(l1){
  
  paletteX<-get(l1$FillPalette);
  if(useNewLabels==1){
    if(!is.null(names(paletteX))){
      names(paletteX)<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",names(paletteX)))}
    l1$Fill<-toTitleCase(sub("\\b[a-zA-Z0-9]{1} \\b", "",l1$Fill))
  }
  
  p <- ggplot(l1[order((l1$Fill)),],aes(x=x,y=NewValue,group=Fill))
  p <- p + z_theme
  p <- p + geom_line(aes(color=Fill),stat="identity",position="identity",size=2) 
  p <- p + guides(fill = guide_legend(color=NULL,reverse=T,title=unique(l1$FillLabel)))
  p <- p + xlab(unique(l1$xLabel)) + ylab(eval(parse(text=unique(l1$NewUnits))))
  p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  p <- p + scale_x_continuous (breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_majMaster)), 
                               minor_breaks=(seq(min(range(l1$x)),max(range(l1$x)),by=breakx_minMaster)),expand=c(0,breakx_majMaster/2))
  p <- p +  scale_y_continuous(breaks = pretty_breaks(n = prettyBreaksyMaster))
  p <- p + scale_color_manual(values=paletteX)
}

mapX_fill<- function(data=dfx,scaleData=dfx@data){
  tm_shape(data) + tm_fill(col=names(data),palette = colx,style="fixed",breaks=pretty_breaks(5)(range(scaleData[complete.cases(scaleData),])),
                           auto.palette.mapping = FALSE, legend.show=T,title="",showNA=F) + 
    tm_facets(free.scales.fill=FALSE) +
    tm_legend(outside = TRUE, text.size = .8)+
    tm_layout(panel.labels=gsub("X","",names(data)),
              panel.label.bg.color = "white",
              panel.label.size = 2,
              legend.position = c("LEFT","TOP"),
              legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) +  
    tm_layout_z +
    #tm_add_legend(title = paste("",sep=""),type = c("fill"), 
    #              col = c(colx[1],colx[round(length(colx)/2,0)],colx[length(colx)]),
    #              labels=c(paste("Min = ",prettyNum(signif(min(data@data,na.rm=T)/1,2),big.mark=","),sep=""),
    #                       paste("Mean = ",prettyNum(signif(mean(as.matrix(data@data)/1,na.rm=T),2),big.mark=","),sep=""),
    #                       paste("Max = ",prettyNum(signif(max(data@data,na.rm=T)/1,2),big.mark=","),sep=""))) +
    #tm_add_legend(title = paste("",sep=""),type = c("fill"),col = "grey",labels="Missing") +
    tm_borders("black",lwd=0.5,lty=1) 
}

mapX_fillFreeScale<<- function(data=dfx){
  tm_shape(data) + tm_fill(col=names(data),palette = colx,style="pretty",n=5,
                           auto.palette.mapping = FALSE, legend.show=T,showNA=F) + tm_facets(free.scales.fill=T) +
    tm_legend(outside = F, text.size = .8)+
    tm_layout(panel.labels=gsub("X","",names(data)),
              panel.label.bg.color = "white",
              panel.label.size = 2,
              legend.position = c("LEFT","TOP"),
              legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) + 
    tm_layout_z + tm_borders("black",lwd=0.5,lty=1) 
}

#Ztest
mapX_raster<<- function(rasterBoundary=dfxtra,data=dfx,scaleData=dfx@data){
  tm_shape(rasterBoundary) + tm_raster(col=names(rasterBoundary)[1],alpha=0,legend.show=F) +
    tm_shape(data) +  tm_raster(col=names(data),palette = colx,style="fixed",breaks=pretty_breaks(5)(range(scaleData[complete.cases(scaleData),])),
                                auto.palette.mapping = FALSE, legend.show=T,title="",showNA=F) + 
    tm_facets(free.scales=FALSE) +
    tm_layout(panel.labels=gsub("X","",names(data)),
              panel.label.bg.color = "white",
              panel.label.size = 2,
              legend.position = c("LEFT","TOP"),
              legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) + 
    tm_layout_z +
    #tm_add_legend(title = paste("",sep=""),type = c("fill"), 
    #              col = c(colx[1],colx[round(length(colx)/2,0)],colx[length(colx)]),
    #              labels=c(paste("Min = ",prettyNum(signif(min(data@data,na.rm=T)/1,2),big.mark=","),sep=""),
    #                       paste("Mean = ",prettyNum(signif(mean(as.matrix(data@data)/1,na.rm=T),2),big.mark=","),sep=""),
    #                       paste("Max = ",prettyNum(signif(max(data@data,na.rm=T)/1,2),big.mark=","),sep=""))) +
    tm_legend(outside = TRUE, text.size = .8)+
    tm_shape(shpa1) + tm_borders("black",lwd=0.5,lty=2) 
}

mapX_rasterFreeScale<<-function(rasterBoundary=dfxtra,data=dfx){
  tm_shape(rasterBoundary) + tm_raster(col=names(rasterBoundary)[1],alpha=0,legend.show=F) +
    tm_shape(data) +  tm_raster(col=names(data),palette = colx,style="pretty",n=5,
                                auto.palette.mapping = FALSE, legend.show=T,showNA=F) + 
    tm_facets(free.scales=T) +
    tm_layout(panel.labels=gsub("X","",names(data)),
              panel.label.bg.color = "white",
              panel.label.size = 2,
              legend.position = c("LEFT","TOP"),
              legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) + 
    tm_layout_z + tm_legend(outside = F, text.size = .8)+
    tm_shape(shpa1) + tm_borders("black",lwd=0.5,lty=2) 
}


# KMEANS Versions for uneven distributions

mapX_fillKMeans<<- function(data=dfx,scaleData=dfx@data){
  tm_shape(data) + tm_fill(col=names(data),palette = colx,
                           style="fixed",breaks=classIntervals(as.numeric(as.vector(as.matrix(scaleData)),rm.na=T), n=5, style = "kmeans")[2]$brks,
                           auto.palette.mapping = FALSE, legend.show=T,title="",showNA=F) + 
    tm_facets(free.scales.fill=FALSE) +
    tm_legend(outside = TRUE, text.size = .8)+
    tm_layout(panel.labels=gsub("X","",names(data)),
              panel.label.bg.color = "white",
              panel.label.size = 2,
              legend.position = c("LEFT","TOP"),
              legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) +  
    tm_layout_z +
    #tm_add_legend(title = paste("",sep=""),type = c("fill"), 
    #              col = c(colx[1],colx[round(length(colx)/2,0)],colx[length(colx)]),
    #              labels=c(paste("Min = ",prettyNum(signif(min(data@data,na.rm=T)/1,2),big.mark=","),sep=""),
    #                       paste("Mean = ",prettyNum(signif(mean(as.matrix(data@data)/1,na.rm=T),2),big.mark=","),sep=""),
    #                       paste("Max = ",prettyNum(signif(max(data@data,na.rm=T)/1,2),big.mark=","),sep=""))) +
    #tm_add_legend(title = paste("",sep=""),type = c("fill"),col = "grey",labels="Missing") +
    tm_borders("black",lwd=0.5,lty=1) 
}

mapX_fillFreeScaleKMeans<<- function(data=dfx){
  tm_shape(data) + tm_fill(col=names(data),palette = colx,style="kmeans",n=5,
                           auto.palette.mapping = FALSE, legend.show=T,showNA=F) + tm_facets(free.scales.fill=T) +
    tm_legend(outside = F, text.size = .8)+
    tm_layout(panel.labels=gsub("X","",names(data)),
              panel.label.bg.color = "white",
              panel.label.size = 2,
              legend.position = c("LEFT","TOP"),
              legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) + 
    tm_layout_z + tm_borders("black",lwd=0.5,lty=1) 
}


#a<-classIntervals(as.numeric(as.vector(as.matrix(scaleData)),rm.na=T), n=5, style = "kmeans")[2]$brks

##Ztest
mapX_rasterKMeans<<- function(rasterBoundary=dfxtra,data=dfx,scaleData=dfx@data){
  tm_shape(rasterBoundary) + tm_raster(col=names(rasterBoundary)[1],alpha=0,legend.show=F,showNA=F) +
    tm_shape(data) +  tm_raster(col=names(data),palette = colx,
                                style="fixed",breaks=classIntervals(as.numeric(as.vector(as.matrix(scaleData)),rm.na=T), n=5, style = "kmeans")[2]$brks,
                                auto.palette.mapping = FALSE, legend.show=T,title="") + 
    tm_facets(free.scales=FALSE) +
    tm_layout(panel.labels=gsub("X","",names(data)),
              panel.label.bg.color = "white",
              panel.label.size = 2,
              legend.position = c("LEFT","TOP"),
              legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) + 
    tm_layout_z +
    #tm_add_legend(title = paste("",sep=""),type = c("fill"), 
    #              col = c(colx[1],colx[round(length(colx)/2,0)],colx[length(colx)]),
    #              labels=c(paste("Min = ",prettyNum(signif(min(data@data,na.rm=T)/1,2),big.mark=","),sep=""),
    #                       paste("Mean = ",prettyNum(signif(mean(as.matrix(data@data)/1,na.rm=T),2),big.mark=","),sep=""),
    #                       paste("Max = ",prettyNum(signif(max(data@data,na.rm=T)/1,2),big.mark=","),sep=""))) +
    tm_legend(outside = TRUE, text.size = .8)+
    tm_shape(shpa1) + tm_borders("black",lwd=0.5,lty=2) 
}

mapX_rasterFreeScaleKMeans<<-function(rasterBoundary=dfxtra,data=dfx){
  tm_shape(rasterBoundary) + tm_raster(col=names(rasterBoundary)[1],alpha=0,legend.show=F) +
    tm_shape(data) +  tm_raster(col=names(data),palette = colx,style="kmeans",n=5,
                                auto.palette.mapping = FALSE, legend.show=T,showNA=F) + 
    tm_facets(free.scales=T) +
    tm_layout(panel.labels=gsub("X","",names(data)),
              panel.label.bg.color = "white",
              panel.label.size = 2,
              legend.position = c("LEFT","TOP"),
              legend.format = list(digits = 2),legend.title.size = 1,legend.text.size = 0.8) + 
    tm_layout_z + tm_legend(outside = F, text.size = .8)+
    tm_shape(shpa1) + tm_borders("black",lwd=0.5,lty=2) 
}
