#***********
# Contact: Zarrar Khan (zarrar.khan@pnnl.gov)
# Project: GCAM-USA 
# File: rgcam_Figures.R
# Date Modified: 6 Apr 2018
# *********


#____________________________________________
#____________________________________________
# Initial setup
#____________________________________________
#____________________________________________


#----------------------
# Clear Variables
#----------------------

rm(list=ls()) # Clear all old variables
gc()
graphics.off()
memory.limit(size=1E+10)

wdScripts<<-"D:/rJGCRI_ChartsMaps"


#----------------------
# Libs
#----------------------

source(paste(wdScripts,"/rJGCRI_libs.R",sep=""))

#____________________________________________
#____________________________________________
# Select run variables
#____________________________________________
#____________________________________________

reReadData     <<- 1  #Read gcamdata queryData.proj again?
reReadDataRef     <<- 0  #Read gcamdata queryData.proj again?
deleteOldFigs<<-1  # To completely delete old figures select 1, else it will only overwrite
runVintages<<-0 # Run vintages separately (takes a long time)
compareRef<<-1 # 1 for comparison, 0 for no
pdfpng<<-'png'  #'pdf', 'png', or 'both'


reportSelFigsName<<-"00Report_GCAMUSAElecDispatch_SelectedFigs"
selectedFigs<-c()

US48<- c('AL','AZ','AR','CA','CO','CT','DE','FL','GA','ID','IL','IN','IA','KS','KY','LA',
         'ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND',
         'OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
US50<- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA',
         'ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND',
         'OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

#Reference Regions for the EPSA ref scenario and USA for GCAM 4.4 tag
refRegions<<-c('Alaska grid','California grid','Central East grid','Central Northeast grid',
              'Central Northwest grid','Central Southwest grid','Florida grid','Hawaii grid','Mid-Atlantic grid',
              'New England grid','New York grid','Northwest grid','Southeast grid','Southwest grid','Texas grid',"USA")


scenNameRefOrig<<-"GCAM-USA_Disp13NoCoalNuc1_2WindSolarto2030AEOGasRef";scenNameRef<-"Reference"
scenName1Orig<<-"GCAM-USA_Disp13NoCoalNuc1_2WindSolarto2030AEOGasHi";scenName1<-"Low Gas price"
scenName2Orig<<-"GCAM-USA_Disp13NoCoalNuc1_2WindSolarto2030AEOGasLo";scenName2<-"High Gas price"
scenName3Orig<<-"GCAM-USA_Disp13NoCoalNuc1_2WindSolarto2030AEOGasSpike";scenName3<-"Historical Gas Price variation"
scenariosComp<<-c(scenNameRefOrig,scenName1Orig,scenName2Orig,scenName3Orig)   # Scenarios to include in intercomparison. Leave as c() if no comparison
scenariosIndv<<-c(scenNameRefOrig) # Scenarios to run detailed analyis for
scenariosREF<<-c("GCAM_USA_Ref","Reference_GCAM44Tag")      # Reference Scenario


NumStates<<-50 # For US 48 enter 48 for US 50 enter 50
rangeX<<-seq(2010,2100,by=5)
rangeFigs<<-seq(2015,2050,by=5)

delay<<-60 # Animation delay
colx<<-colorsAbsolute #------Choose color palette
#colx<<-(brewer.pal(9,"RdYlGn")) #------Choose color palette
pie(rep(1,length(colx)),label=names(colx),col=colx)

# Chart Details
titleOn<<-0 # 1 for yes, 0 for no
useNewLabels<<- 0 # 1 for new modified labels without letter prefix and capitalized
prettyBreaksyMaster<<-5
breakx_majMaster<<-10
breakx_minMaster<<-5

# PDF details
figWidth_InchMaster<<-11
figWidth_StateCharts_Multplier<<-2
figWidth_FreeScale_Multplier<<-1.2
figHeight_InchMaster<<-8
figHeight_StateCharts_Multplier<<-1.8

# PDF details
mapWidthInch<<-10
mapHeightInch<<-8


#____________________________________________
#____________________________________________
# C. Working Direcotries and Data
#____________________________________________
#____________________________________________

finalFigsOutFolder <<- "FinalFigures"

wdsp<<-"D:/00SpatialData"  # Spatial Data folder Check in rgcam_FiguresMASTER.R to see if all spatial files are in this folder


wd0<<-getwd();wd0
wdfigsOut<-paste(wd0,"/fig_outputs",sep="");wdfigsOut;
if(deleteOldFigs==1){if(dir.exists(wdfigsOut)){unlink(wdfigsOut,recursive=T)}}
if(!dir.exists(wdfigsOut)){dir.create(wdfigsOut)}  # Output Directory

# GCAM Data outputs
wddb<-paste(dirname(wd0),"/gcam-core/output",sep="");wddb    # GCAM Database directories

# Supporting shapefiles directories (Admin/Basin Boundaries)
wdspUSStates500k_USBur<-paste(wdsp,"/USCensusBureau/cb_2017_us_state_500k",sep="");wdspUSStates500k_USBur 
wdspUSStates20m_USBur<-paste(wdsp,"/USCensusBureau/cb_2017_us_state_20m",sep="");wdspUSStates20m_USBur 
wdspNaturalEarth<-paste(wdsp,"/naturalEarth",sep="");wdspNaturalEarth
wdspPNNL_CV<-paste(wdsp,"/boundaries_PNNLChrisVernon/shp",sep="");wdspPNNL_CV 


#----------------------
# New Fucntions
#----------------------

source(paste(wdScripts,"/rJGCRI_ChartsMaps.R",sep=""))


#____________________________________________
#____________________________________________
# D. Connect to GCAM https://github.com/JGCRI/rgcam
#____________________________________________
#____________________________________________

myDB<-"database_basexdb"          # Name of database
connx <- localDBConn(wddb, myDB)    # Connect to database

# Add scenarios to the project
if(reReadData==1){
  if(file.exists("queryData.proj")){file.remove("queryData.proj")} # Delete old project file
  for (scenario_i in scenariosComp){
    queryData.proj<<-addScenario(conn=connx, proj="queryData.proj",scenario=scenario_i,queryFile='analysis_queriesGCAMUSA.xml')  # Check your queries file
  }}else{
    queryData.proj<<-loadProject("queryData.proj")  # Use already saved database
  }

lscen<-listScenarios(queryData.proj);lscen
lquer<-listQueries(queryData.proj);lquer

#Test Query
queryCheck<-lquer
for(query_i in queryCheck){
print(query_i)
print(head(as.data.frame(getQuery(queryData.proj,query_i))))   # Check a Query
}

#-----------------
# Ref Scenario
#-----------------

myDBREF1<-"database_basexdb_EPSA"          # Name of database
myDBREF2<-"database_basexdb_GCAM44Ref"          # Name of database
connx1 <- localDBConn(wddb, myDBREF1)    # Connect to database
connx2 <- localDBConn(wddb, myDBREF2)    # Connect to database

if(reReadDataRef==1){
    if(file.exists("queryDataREF.proj")){file.remove("queryDataREF.proj")} # Delete old project file
    for (scenario_i in scenarios){
      queryDataREF.proj<-addScenario(conn=connx1, proj="queryDataREF.proj",scenario=scenariosREF[1],queryFile='analysis_queries.xml')  # Check your queries file
      queryDataREF.proj<-addScenario(conn=connx2, proj="queryDataREF.proj",scenario=scenariosREF[2],queryFile='analysis_queries.xml')  # Check your queries file
    }}else{
      queryDataREF.proj<<-loadProject("queryDataREF.proj")  # Use already saved database
    }

lscenREF<-listScenarios(queryDataREF.proj);lscenREF
lquerREF<-listQueries(queryDataREF.proj);lquerREF

#Test Query
head(as.data.frame(getQuery(queryDataREF.proj,"Electricity production by technology by vintage")))  # Check a Query



#____________________________________________
#____________________________________________
# E. Read in Segment Data
#____________________________________________
#____________________________________________

# Segment hours and settings
# From : gcam-core\input\gcam-data-system\gcam-usa-data\assumptions\A23.seasonal_segments.csv
# From: gcam-core\input\gcam-data-system\gcam-usa-data\level1\L102.load_segments.csv
# From: input\gcam-data-system\gcam-usa-data\mappings\states_subregions.csv

lsegs <<- read.csv(paste(dirname(wd0),"/gcam-core/input/gcam-data-system/gcam-usa-data/level1/L102.load_segments.csv",
                         sep=""), stringsAsFactors = F, skip=5,header=T); head(lsegs)
lgridState<<-read.csv(paste(dirname(wd0),"/gcam-core/input/gcam-data-system/gcam-usa-data/mappings/states_subregions.csv",
                            sep=""), stringsAsFactors = F, skip=0,header=T); head(lgridState)

lgridStateSegs<<-join(lsegs%>%dplyr::select(grid_region,segment,hours),
                      lgridState%>%dplyr::select(state,grid_region),by="grid_region"); head(lgridStateSegs)

#seg_Order<<-c("fall_day","fall_night","winter_day","winter_night","spring_day","spring_night",
#              "summer_day","summer_night","superpeak");
#seg_OrderHours=c(1016,1168,898.5,1261.5,1196,1012,1290,905,13))

seg_Order<<-c("superpeak",
              "Jan_day","Jan_night","Feb_day","Feb_night","Mar_day","Mar_night",
              "Apr_day","Apr_night","May_day","May_night","Jun_day","Jun_night",
              "Jul_day","Jul_night","Aug_day","Aug_night","Sep_day","Sep_night",
              "Oct_day","Oct_night","Nov_day","Nov_night","Dec_day","Dec_night");

df_segHours<<-join(data.frame(segment=seg_Order),lgridStateSegs%>%dplyr::select(state,segment,hours),by="segment")%>%unique;
head(df_segHours)



#____________________________________________
#____________________________________________
# E. Read in Spatial Data
#____________________________________________
#____________________________________________

#Natural Earth Boundaries
shp_natEarthAdmin<<-readOGR(paste(wdspNaturalEarth,"/ne_10m_admin_1_states_provinces",sep=""),"ne_10m_admin_1_states_provinces")

# US States from US Census Bureau
shp_USStates500k<<-readOGR(paste(wdspUSStates500k_USBur,sep=""),"cb_2017_us_state_500k")
shp_USStates20m<<-readOGR(paste(wdspUSStates20m_USBur,sep=""),"cb_2017_us_state_20m")


# PNNL Regions and Basins
shp_PNNL32Reg<<-readOGR(paste(wdspPNNL_CV,sep=""),"region32_0p5deg")
shp_PNNL235CLM5ArcMin_multi<<-readOGR(paste(wdspPNNL_CV,sep=""),"Global235_CLM_final_5arcmin_multipart")

LookupTable_PNNL32Region<-data.frame(reg32_id=c(0:32),
                                     GCAM_region=c("0","USA","Africa_Eastern","Africa_Northern","Africa_Southern",
                                                   "Africa_Western","Australia_NZ","Brazil","Canada",
                                                   "Central America and Caribbean","Central Asia","China","EU-12",
                                                   "EU-15","Europe_Eastern","Europe_Non_EU","European Free Trade Association",
                                                   "India","Indonesia","Japan","Mexico",
                                                   "Middle East","Pakistan","Russia","South Africa",
                                                   "South America Northern","South America_Southern","South Asia","South Korea",
                                                   "Southeast Asia","Taiwan","Argentina","Colombia"))

shp_PNNL32Reg@data<-join(shp_PNNL32Reg@data,LookupTable_PNNL32Region,by=c("reg32_id")); head(shp_PNNL32Reg@data)



#--------------- Set projection layer
projX<<-proj4string(shp_PNNL32Reg) # Setting to 32 Region GCAM shape layer
shp_USStates20m<<-spTransform(shp_USStates20m, CRS(projX))


#____________________________________________
#____________________________________________
# F1. READ IN DATA
#____________________________________________
#____________________________________________

#----------
# Charts (df_all)
#-----------

# Get subsector tech list
tbl <- getQuery(queryData.proj, "Electricity capacity by technology") # Tibble  
df<-as.data.frame(tbl);  head(df)            # Data frame
ss1<-subset(df,select=c(subsector,technology));head(ss1)
ss2<-ss1[!duplicated(ss1),];head(ss2)

df_all<-data.frame()

# Prices for All markets (regional natural gas)
tbl <- getQuery(queryData.proj, "Prices for all markets") # Tibble 
df<-as.data.frame(tbl);  head(df)            # Data frame
df<-df%>%filter(market== "USAregional natural gas")
df$vintage<-paste("Vint_",df$year,sep="")
df$param<-"regNatGasPrice"; head(df) 
df$Query<-"Prices for all markets"
df$Title<-"US Regional Natural Gas Price"
df$NewValue<-df$value*conv1975USDperGJ22017USDperMBTU
df$NewUnits<-"~US~Regional~Natural~Gas~Price~(2017~USD/MBTU)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df$region<-"USA"
df<-df %>% mutate (Fill1=market,
                   FillLabel1="market",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=market,
                   FillLabel2="market",
                   FillPalette2="colorsX_elec_subsec")%>%  
  subset(.,select=-c(market));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_USregNatGasPrice<-df

# electricity generation dispatch cost by tech
tbl <- getQuery(queryData.proj, "elec operating costs by technology and vintage") # Tibble 
df<-as.data.frame(tbl);  head(df)            # Data frame
test1<-df%>%filter((year==2015 | year==2020 | year==2025 | year ==2030),grepl("conv pul\\)|gas \\(CC\\),",technology),
scenario=="GCAM-USA_Disp13NoCoalNuc1_2WindSolarto2030AEOGasRef",region=="TX")%>%dplyr::select(-scenario)%>%
  spread(year,value);test1
y<-colsplit(df$technology, pattern=",",names=c("technology","vintage")) # Split techs by vintage
y$vintage<-sub("year=","Vint_",y$vintage); head(y)
#y$vintage[y$vintage=="Vint_1990"]<-"Vint_2010"; head(y)  # Switch historical vintages to 2010
df<-cbind.data.frame(subset(df,select=-c(technology,sector)),y); head(df)       # Rejoin split vectors
df$param<-"elecGenDispCost"; head(df) 
df$Query<-"Zedit Electricity dispatch generation costs by technology"
df$Title<-"Elec Gen Dispatch cost by tech"
df$NewValue<-df$value*conv1975USDperGJ22017USDperMWh
df$NewUnits<-"~Generation~Dispatch~Cost~(2017~USD/MWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecGenDispCost<-df



# electricity generation by segment and tech
tbl <- getQuery(queryData.proj, "electricity generation by segment and tech") # Tibble 
df<-as.data.frame(tbl);  head(df)            # Data frame
df$vintage<-paste("Vint_",df$`tech-year`,sep=""); head(df)
names(df)[names(df)=="tech-name"]<-"technology"
df<-join(df,ss2,by="technology");head(df)
df$param<-"elecGenbyVerSeg"; head(df) 
df$Query<-"electricity generation by segment and tech"
df$Title<-"Elec Gen by Vert Segment"
df$region<-df$state
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"~Electricity~Generation~(TWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df<-df%>%dplyr::select(-state,-`tech-year`); head(df)
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecGenbyVerSeg<-df

# ZEdit Electricity generation capacity cost by technology by segment
tbl <- getQuery(queryData.proj, "ZEdit Electricity generation capacity cost by technology by segment") # Tibble 
df<-as.data.frame(tbl);  head(df)            # Data frame
df$vintage<-paste("Vint_",df$year,sep=""); head(df)
names(df)[names(df)=="sector"]<-"segment"
df$param<-"elecGenCapCostbyHorSeg"; head(df) 
df$Query<-"ZEdit Electricity generation capacity cost by technology by segment"
df$Title<-"Elec Gen Cap Cost by Hor Segment"
df$NewValue<-df$value*conv1975USDperGJ22017USDperMWh
df$NewUnits<-"~Generation~Capacity~Cost~(2017~USD/MWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df<-df%>%filter(!grepl("CCS",technology));head(df)
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecGenCapCostbyHorSeg<-df

# ZEdit Electricity production by technology by vintage by segment
tbl <- getQuery(queryData.proj, "ZEdit Electricity production by technology by vintage by segment") # Tibble 
df<-as.data.frame(tbl);  head(df)            # Data frame
y<-colsplit(df$technology, pattern=",",names=c("technology","vintage")) # Split techs by vintage
y$vintage<-sub("year=","Vint_",y$vintage)
y$vintage[y$vintage=="Vint_1990"]<-"Vint_2010"; head(y)  # Switch historical vintages to 2010
df<-cbind.data.frame(subset(df,select=-c(technology)),y); head(df)       # Rejoin split vectors
df<-subset(df,select=-c(sector))  # Remove subsectors
df$year[df$year==1990]<-2010; head(df)
names(df)[names(df)=="output"]<-"segment"
df$param<-"elecProdbyHorSeg"; head(df) 
df$Query<-"ZEdit Electricity production by technology by vintage by segment"
df$Title<-"Elec Prod by Hor Segment"
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"Electricity~Generation~(TWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecProdbyHorSeg<-df

# ZEdit Electricity capacity-factor by technology by segment
tbl <- getQuery(queryData.proj, "ZEdit Electricity capacity-factor by technology by segment") # Tibble 
df<-as.data.frame(tbl);head(df)              # Data frame
#df$technology<-"Technology"
names(df)[names(df)=="sector"]<-"segment"
df$vintage<-paste("Vint_",df$year,sep=""); 
df$vintage[df$vintage=="Vint_1990"]<-"Vint_2010"; head(df)  # Switch historical vintages to 2010
df$year[df$year==1990]<-2010; head(df)
df$param<-"capFacbyHorSeg"; head(df) 
df$Query<-"ZEdit Electricity capacity-factor by technology by segment"
df$Title<-"Cap Factor by Hor Segment"
df$NewValue<-df$value
df$NewUnits<-"~Capacity~factor"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_capFacbyHorSeg<-df

# ZEdit Electricity capacity by technology by segment
df1<-df_all_elecProdbyHorSeg;names(df1)[names(df1)=="NewValue"]<-"elecProdbyHorSeg";df1<-df1%>%dplyr::select(-Units,-value,-param,-Query,-Title,-NewUnits,-Aggregate)
df2<-df_all_capFacbyHorSeg;names(df2)[names(df2)=="NewValue"]<-"capFacbyHorSeg";df2<-df2%>%dplyr::select(-Units,-value,-param,-Query,-Title,-NewUnits,-Aggregate)
df<-join(df1,df2,by=c("scenario","region","Fill2","FillLabel2","FillPalette2","segment","year","Fill1","FillLabel1","FillPalette1","vintage","x","xLabel"));head(df)
df$param<-"elecCapbyHorSeg"; head(df) 
df$Query<-"None"
df$Title<-"Elec Cap by Hor Segment"
df$NewUnits<-"~Generation~Cap~(GW)"  # Use ~ for spaces. Will be parsed later
df$Units<-df$NewUnits
df$Aggregate<-"sum";  head(df) # How to aggregate over spatial and temporal units;
df$NewValue<-df$elecProdbyHorSeg/df$capFacbyHorSeg
df<-df%>%dplyr::select(-c(elecProdbyHorSeg,capFacbyHorSeg))
df$value<-df$NewValue; head(df)
df_all_elecCapbyHorSeg<-df; head(df)


# Total final energy by aggregate end-use sector
tbl <- getQuery(queryData.proj, "Total final energy by aggregate end-use sector") # Tibble 
df<-as.data.frame(tbl); head(df)             # Data frame
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
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_finalNrgBySec<-df

# Total final energy by aggregate end-use sector REFERENCE
tbl <- getQuery(queryDataREF.proj, "Total final energy by aggregate end-use sector") # Tibble 
df<-as.data.frame(tbl);              # Data frame
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
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_finalNrgBySecREF<-df


# Electric production by technology & Vintage
tbl <- getQuery(queryData.proj, "Electricity production by technology by vintage") # Tibble 
df<-as.data.frame(tbl);  head(df)            # Data frame
y<-colsplit(df$technology, pattern=",",names=c("technology","vintage")) # Split techs by vintage
y$vintage<-sub("year=","Vint_",y$vintage)  
df<-cbind.data.frame(subset(df,select=-c(technology)),y); head(df)       # Rejoin split vectors
df$param<-"elecProdByTechVint"; head(df) 
df$Query<-"Electricity production by technology by vintage"
df$Title<-"Elec Prod"
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"~Electricity~Generation~(TWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecProd<-df

# Electric production by technology & Vintage
tbl <- getQuery(queryData.proj, "Total electricity delivered before losses") # Tibble 
df<-as.data.frame(tbl);  head(df)            # Data frame
df<-df%>%rename(year=`demand-physical`)
df$year<-gsub("vintage=","",df$year);head(df)
df$vintage<-paste("Vint_",df$year)  
df$param<-"elecDelivB4Losses"; head(df) 
df$Query<-"Total electricity delivered before losses"
df$Title<-"Elec Delivered"
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"~Electricity~Consumption~(TWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1="Fill1",
                   FillLabel1="Fill1",
                   FillPalette1="colorsX_Unassigned",
                   Fill2="Fill2",
                   FillLabel2="Sector",
                   FillPalette2="colorsX_elec_subsec");
df_all<-rbind.data.frame(df_all,df); head(df_all)



# Electric production by technology & Vintage REFERENCE
tbl <- getQuery(queryDataREF.proj, "Electricity production by technology by vintage") # Tibble 
df<-as.data.frame(tbl);  head(df)            # Data frame
y<-colsplit(df$technology, pattern=",",names=c("technology","vintage")) # Split techs by vintage
y$vintage<-sub("year=","Vint_",y$vintage)  
df<-cbind.data.frame(subset(df,select=-c(technology)),y); head(df)       # Rejoin split vectors
df$param<-"elecProdByTechVint"; head(df) 
df$Query<-"Electricity production by technology by vintage"
df$Title<-"Elec Prod"
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"~Electricity~Generation~(TWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecProdREF<-df

# Electric costs by sector
tbl <- getQuery(queryData.proj, "Electricity costs by sector") # Tibble 
df<-as.data.frame(tbl);              # Data frame
names(df)[names(df)=="sector"]<-"subsector"
df$technology<-"Technology"
df$vintage<-paste("Vint_",df$year,sep=""); head(df)       # Rejoin split vectors
df$param<-"elecCostSec"; head(df) 
df$Query<-"Electricity costs by sector"
df$Title<-"Elec Cost"
df$NewValue<-df$value*conv1975USDperGJ22017USDperMWh
df$NewUnits<-"~Electricity~Cost~(2017~USD/MWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df<-df%>%filter(subsector!="electricity")
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_sec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecCostSec<-df

# Electric costs by Subsector
tbl <- getQuery(queryData.proj, "Electricity costs by subsector") # Tibble 
df<-as.data.frame(tbl);              # Data frame
df<-df%>%dplyr::select(-sector)
df$technology<-"Technology"
df$vintage<-paste("Vint_",df$year,sep=""); head(df)       # Rejoin split vectors
df$param<-"elecCostSubsec"; head(df) 
df$Query<-"Electricity costs by subsector"
df$Title<-"Elec Cost"
df$NewValue<-df$value*conv1975USDperGJ22017USDperMWh
df$NewUnits<-"~Electricity~Cost~(2017~USD/MWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecCostSubsec<-df

# Electric costs by sector REference
tbl <- getQuery(queryDataREF.proj, "Electricity costs by sector") # Tibble 
df<-as.data.frame(tbl);              # Data frame
names(df)[names(df)=="sector"]<-"subsector"
df$technology<-"Technology"
df$vintage<-paste("Vint_",df$year,sep=""); head(df)       # Rejoin split vectors
df$param<-"elecCostSec"; head(df) 
df$Query<-"Electricity costs by sector"
df$Title<-"Elec Cost"
df$NewValue<-df$value*conv1975USDperGJ22017USDperMWh
df$NewUnits<-"~Electricity~Cost~(2017~USD/MWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_sec") %>%  
  subset(.,select=-c(technology,subsector));
#df<-df%>%filter(subsector!="electricity")
df_all<-rbind.data.frame(df_all,df); head(df_all)
dfREF_line_elecCostSec<-df

# Electric costs by Subsector Reference
tbl <- getQuery(queryDataREF.proj, "Electricity costs by subsector") # Tibble 
df<-as.data.frame(tbl);              # Data frame
df<-df%>%dplyr::select(-sector)
df$technology<-"Technology"
df$vintage<-paste("Vint_",df$year,sep=""); head(df)       # Rejoin split vectors
df$param<-"elecCostSubsec"; head(df) 
df$Query<-"Electricity costs by subsector"
df$Title<-"Elec Cost"
df$NewValue<-df$value*conv1975USDperGJ22017USDperMWh
df$NewUnits<-"~Electricity~Cost~(2017~USD/MWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));
df_all<-rbind.data.frame(df_all,df); head(df_all)
dfREF_line_elecCostSubsec<-df

# Electric Capacity by technology Investments
tbl <- getQuery(queryData.proj, "Electricity capacity by technology") # Tibble  
df<-as.data.frame(tbl);              # Data frame
df$vintage<-paste("Vint_",df$year,sep="")                   # Add colomn for vintage
df$param<-"elecCapByTech";
df$Query<-"Electricity Cap by technology"
df$Title<-"New Elec Cap"
df$NewValue<-df$value*convEJ2GW
df$NewUnits<-"~Electricity~Capacity~(GW)"  # Use ~ for spaces. Will be parsed later
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$x<-df$year
df$xLabel<-"Year"; head(df)
# Add all the years
yearsx<-unique(df$year)
dfx<-data.frame()
for(years in yearsx){
  df1<-df
  df1$year<-years
  dfx<-rbind.data.frame(dfx,df1)
}
dfx$value[dfx$year!=dfx$x]<-0;   # Set the values to 0 for the years where no cap was invested
dfx$NewValue[dfx$year!=dfx$x]<-0; # Set the values to 0 for the years where no cap was invested
df<-dfx
df$x<-df$year
df$xLabel<-"Year"; head(df)
df$segment<-"Segment"
df<-df %>% mutate (Fill1=technology,
                   FillLabel1="Technology",
                   FillPalette1="colorsX_elec_techs",
                   Fill2=subsector,
                   FillLabel2="Subsector",
                   FillPalette2="colorsX_elec_subsec") %>%  
  subset(.,select=-c(technology,subsector));

# Adjust 2010
# Source: https://www.eia.gov/dnav/ng/ng_pri_fut_s1_d.htm
EIA2010TotalCapGW<-1138.638
df2010<-df%>%filter(x==2010,vintage=="Vint_2010")%>%group_by(scenario)%>%summarize(sum=sum(NewValue))%>%as.data.frame%>%
  mutate(Adjust=1+(EIA2010TotalCapGW-sum)/sum,year=2010,vintage="Vint_2010",param="elecCapByTech")%>%dplyr::select(-sum);df2010
df<-join(df,df2010,by=c("year","scenario","vintage","param"))%>%
  mutate(NewValue=case_when(!is.na(Adjust)~NewValue*Adjust,
                            TRUE~NewValue))%>%dplyr::select(-Adjust);head(df)
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecCap<-df

if(FALSE){
# Check Data
l1<-df%>%dplyr::filter(scenario==scenNameRefOrig)
l1$Fill<-l1$Fill2;
l1$FillLabel<-l1$FillLabel2 
l1$FillPalette<-l1$FillPalette2
l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill,FillLabel,FillPalette)  # For National, by techs
head(l1)
if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
l1<-rbind.data.frame(xsum,xmean);head(l1)
l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
p <- fig_Bar(l1)
plot(p)
}

# "Electric Capacity by technology cumulative Investments
df<-df_all_elecCap
df$CumInvest<-0;
df[df$year>=2010,]<-as.data.frame(df[df$year>=2010,] %>% 
                group_by(scenario, region, Fill1,vintage) %>%
                mutate(CumInvest=cumsum(NewValue)))
df<-subset(df,select=-c(NewValue))
colnames(df)[which(names(df) == "CumInvest")] <- "NewValue"
df$param<-"elecCumCapInvest";
df$Query<-"None"
df$Title<-"Cum Elec Cap Invest"
df$NewUnits<-"Electricity~Capacity~(GW)"  # Use ~ for spaces. Will be parsed later
df$Units<-df$NewUnits
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecCapInvestCum<-df

if(FALSE){
# Check Data
l1<-df%>%dplyr::filter(scenario==scenNameRefOrig)
l1$Fill<-l1$Fill2;
l1$FillLabel<-l1$FillLabel2 
l1$FillPalette<-l1$FillPalette2
l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill,FillLabel,FillPalette)  # For National, by techs
head(l1)
if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
l1<-rbind.data.frame(xsum,xmean);head(l1)
l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
p <- fig_Bar(l1)
plot(p)
}

#y<-df[(df$vintage=="Vint_2010" & df$NewValue>0 & df$x!="2010"),];y

#----------
# Retirements based on Life Time
#----------

# Read life times: Note the years in this csv are actually vintages
lt <- read.csv(paste(dirname(wd0),"/gcam-core/input/gcam-data-system/gcam-usa-data/level2/L223.TechLifetime_Dispatch.csv",
                     sep=""), stringsAsFactors = F, skip=4,header=T); head(lt)
lt$retireYear<-lt$year+lt$lifetime;head(lt)
lt$vintage<-paste("Vint_",lt$year,sep="");head(lt)
ltx<-lt
ltx<-ltx %>% mutate (Fill1=technology,
                   Fill2=subsector) %>%  
  subset(.,select=-c(technology,subsector));
head(ltx)

ncx<-subset(df_all_elecCap,select=-c(x,xLabel)); head(ncx)
# Join the retirement to the installed capacity
lty<-join(ncx,subset(ltx,select=-c(supplysector,year)),by=c("Fill2","region","Fill1","vintage"));head(lty)
lty<-lty[complete.cases(lty),]; head(lty)
lty<-subset(lty,select=-c(year,NewValue))
colnames(lty)[which(names(lty) == "retireYear")] <- "year";
colnames(lty)[which(names(lty) == "value")] <- "retirements";head(lty[lty$retirements>0,]); nrow(lty)
lty<-lty[lty$retirements>0,]
lty<-unique(lty); head(lty); nrow(lty)
ncx$invest<-ncx$NewValue;
lty1<-join(ncx,lty,by=c("Units","scenario","region","Fill2","FillLabel2","FillPalette2",
                        "vintage","param","Query","Title",
                        "NewUnits","Aggregate","Fill1","FillLabel1","FillPalette1","year","segment"));
lty1$retirements[is.na(lty1$retirements)]<-0; head(lty1[lty1$retirements>0 ,])

ltz<-lty1
ltz$x<-ltz$year
ltz$xLabel<-"Year"
head(ltz)
head(ltz[ltz$year=="2070" & ltz$retirements>0,]); 
head(ltz[duplicated(ltz),]);

#-------
# Check the S Curve retirements
#--------

# S curves for gradual retirement
ltSC <- read.csv(paste(dirname(wd0),"/gcam-core/input/gcam-data-system/gcam-usa-data/level2/L223.TechSCurve_Dispatch.csv",
                       sep=""), stringsAsFactors = F, skip=4,header=T); head(ltSC)
ltSC$vintage<-paste("Vint_",ltSC$year,sep="");
colnames(ltSC)[which(names(ltSC) == "lifetime")] <- "lifetimeSC";
ltSC<-subset(ltSC,select=-c(year,supplysector));
ltSC<-ltSC %>% mutate (Fill1=technology,
                     Fill2=subsector) %>%  
  subset(.,select=-c(technology,subsector));
head(ltSC)

ncx<-subset(df_all_elecCapInvestCum,select=-c(x,xLabel))
ncx$CumInvest<-ncx$NewValue
lty2<-join(ncx,ltSC,by=c("Fill2","region","Fill1","vintage"));head(lty2)
lty2$CumRetireSC<-0;
# S Curve formula 1/(1+e^(steepness*(years active-halflife))) from
# The equation for the 2010 lifetime plants is in the C++ file in \gcam-core\cvs\objects\technologies\source\s_curve_shutdown_decider.cpp
lty2$CumRetireSC[!is.na(lty2$half.life)]<-lty2$CumInvest[!is.na(lty2$half.life)]*(1-(1/(1+exp(lty2$steepness[!is.na(lty2$half.life)]*(lty2$year[!is.na(lty2$half.life)]-
                                                                                                                                      as.numeric(sub("Vint_","",lty2$vintage[!is.na(lty2$half.life)]))-
                                                                                                                                      lty2$half.life[!is.na(lty2$half.life)]))
                                                                                )))
head(lty2[lty2$CumRetireSC>0 & !is.na(lty2$CumRetireSC),])

# Calculate the yearly retirements (diff(c(0,CumRetireSC)))
lty3<-lty2
lty3$retireSC<-0;
lty3<-lty3 %>% 
                                          group_by(scenario, region, Fill1,vintage) %>%
                                          mutate(retireSC=diff(c(0,CumRetireSC)))
lty3<-as.data.frame(lty3)
lty3$x<-lty3$year
lty3$xLabel<-"Year"

head(lty3[lty3$year=="2070" & lty3$retireSC>0,],40); 

# Replace the regular retirements and Cumulative retires with with S curve retirements
ltz1<-join(subset(ltz,select=-c(NewValue,NewUnits,param,Query,Title)),
           subset(lty3,select=-c(NewValue,NewUnits,param,Query,Title)),type="full",
           by=c("scenario","Fill2","FillLabel2","FillPalette2","region","Fill1","FillLabel1","FillPalette1","vintage","year","x","xLabel"))
ltz1$retirements[!is.na(ltz1$half.life)]<-ltz1$retireSC[!is.na(ltz1$half.life)]
ltz1<-unique(ltz1)

# Calculate Cumulative retirements
ltz1$CumRetirements<-0;
ltz2<-subset(ltz1,select=-c(lifetimeSC ,steepness, half.life, CumRetireSC ,retireSC,lifetime))
ltz2<-ltz2[order(ltz2$year),]; head(ltz2);

ltz2[ltz2$year>=2010,]<-as.data.frame(ltz2[ltz2$year>=2010,] %>% 
                                        group_by(scenario, region,Fill1,vintage) %>%
                                        mutate(CumRetirements=cumsum(retirements)))
head(ltz2[ltz2$year=="2070" & ltz2$CumRetirements>0,])
#ltz2[ltz2$CumInvest>0 & ltz2$technology=="gas (CC)" & ltz2$region=="CA" & ltz2$vintage=="Vint_2020",]
ltz2<-ltz2%>%mutate(CumCap=case_when(x>2010~(CumInvest-CumRetirements),
                                     TRUE~CumInvest));
ltz2<-ltz2[order(ltz2$x),]; head(ltz2)
#a<-ltz2[ltz2$CumInvest>0 & ltz2$technology=="gas (CC)" & ltz2$region=="CA" & ltz2$vintage=="Vint_2020",];a
#plot(a$x,a$CumCap,type="l",col="red");par(new=TRUE);plot(a$x,a$CumRetirements,type="p",col="blue");

rm(lty3,ltz1,ltz,lty2,lty1)

# "Electric Capacity by technology Retirements
df<-subset(ltz2,select=-c(CumInvest,CumCap,CumRetirements,invest))
colnames(df)[which(names(df) == "retirements")] <- "NewValue"
df$param<-"elecCapRetirements";
df$Query<-"None"
df$Title<-"Elec Cap Retire"
df$NewUnits<-"Electricity~Capacity~(GW)"  # Use ~ for spaces. Will be parsed later
df$Units<-df$NewUnits
df$value<-df$NewValue
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecCapRetire<-df;head(df)

if(FALSE){
  # Check Data
  l1<-df%>%dplyr::filter(scenario==scenNameRefOrig)
  l1$Fill<-l1$Fill2;
  l1$FillLabel<-l1$FillLabel2 
  l1$FillPalette<-l1$FillPalette2
  l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill,FillLabel,FillPalette)  # For National, by techs
  head(l1)
  if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
  if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
  l1<-rbind.data.frame(xsum,xmean);head(l1)
  l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
  p <- fig_Bar(l1)
  plot(p)
}


# "Electric Capacity by technology cumulative Retirements
df<-subset(ltz2,select=-c(CumInvest,CumCap,retirements,invest))
colnames(df)[which(names(df) == "CumRetirements")] <- "NewValue"
df$param<-"elecCumCapRetirements";
df$Query<-"None"
df$Title<-"Cum Elec Cap Retire"
df$NewUnits<-"Electricity~Capacity~(GW)"  # Use ~ for spaces. Will be parsed later
df$Units<-df$NewUnits
df$value<-df$NewValue
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecCapRetireCum<-df

if(FALSE){
  # Check Data
  l1<-df%>%dplyr::filter(scenario==scenNameRefOrig)
  l1$Fill<-l1$Fill2;
  l1$FillLabel<-l1$FillLabel2 
  l1$FillPalette<-l1$FillPalette2
  l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill,FillLabel,FillPalette)  # For National, by techs
  head(l1)
  if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
  if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
  l1<-rbind.data.frame(xsum,xmean);head(l1)
  l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
  p <- fig_Bar(l1)
  plot(p)
}

# "Electric Capacity by technology cumulative Capacity
df<-subset(ltz2,select=-c(CumInvest,CumRetirements,retirements,invest))
colnames(df)[which(names(df) == "CumCap")] <- "NewValue"
df$param<-"elecCumCap";
df$Query<-"None"
df$Title<-"Cum Elec Cap"
df$NewUnits<-"Electricity~Capacity~(GW)"  # Use ~ for spaces. Will be parsed later
df$Units<-df$NewUnits
df$value<-df$NewValue
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecCapCum<-df; head(df)

if(FALSE){
  # Check Data
  l1<-df%>%dplyr::filter(scenario==scenNameRefOrig)
  l1$Fill<-l1$Fill2;
  l1$FillLabel<-l1$FillLabel2 
  l1$FillPalette<-l1$FillPalette2
  l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill,FillLabel,FillPalette)  # For National, by techs
  head(l1)
  if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
  if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
  l1<-rbind.data.frame(xsum,xmean);head(l1)
  l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
  p <- fig_Bar(l1)
  plot(p)
}

# "Electric Capacity Factor by technology cumulative
jx<-df_all_elecCapCum;colnames(jx)[which(names(jx) == "NewValue")] <- "NewValue_elecCapCum";colnames(jx)[which(names(jx) == "NewUnits")] <- "NewUnits_elecCapCum"
jy<-df_all_elecProd;colnames(jy)[which(names(jy) == "NewValue")] <- "NewValue_elecProd";colnames(jy)[which(names(jy) == "NewUnits")] <- "NewUnits_elecProd"
jx<-subset(jx,select=-c(Title,Query,value));jy<-subset(jy,select=-c(Title,Query,value,x,xLabel));
df<-join(jx,jy,by=c("scenario","region","year","Fill1","vintage")); head(df)
df<-df[complete.cases(df),];head(df);
df$NewValue<-df$NewValue_elecProd*1000/(df$NewValue_elecCapCum*8760); head(df)
df$param<-"elecCapFacByTech";
df$Query<-"None"
df$Title<-"Elec Cap Factor"
df$NewUnits<-"Electricity~Capacity~Factor"  # Use ~ for spaces. Will be parsed later
df$Units<-df$NewUnits
df$NewValue[df$NewValue>2]<-NA
df$value<-df$NewValue
df$Aggregate<-"mean"  # How to aggregate over spatial and temporal units
df<-subset(df,select=c(Units,scenario,region,Fill2,FillLabel2,FillPalette2,
                       year,value,vintage,param,Query,Title,NewValue,NewUnits,x,xLabel,Aggregate,Fill1,FillLabel1,FillPalette1))
head(df)
df$segment<-"Segment"
df_all<-rbind.data.frame(df_all,df); head(df_all)
df_all_elecCapFac<-df

if(FALSE){
  # Check Data
  l1<-df%>%dplyr::filter(scenario==scenNameRefOrig)
  l1$Fill<-l1$Fill2;
  l1$FillLabel<-l1$FillLabel2 
  l1$FillPalette<-l1$FillPalette2
  l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill,FillLabel,FillPalette)  # For National, by techs
  head(l1)
  if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
  if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
  l1<-rbind.data.frame(xsum,xmean);head(l1)
  l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
  p <- fig_LineMultiple(l1)
  plot(p)
}

# "Final energy by aggregate end-use sector and fuel
tbl <- getQuery(queryData.proj, "Final energy by aggregate end-use sector and fuel") # Tibble 
df<-as.data.frame(tbl);   head(df)           # Data frame
df$technology<-"Technology"
names(df)[names(df)=="sector"]<-"subsector"
df$vintage<-paste("Vint_",df$year,sep="");   
df$param<-"finalNrgbySecbyFuel"; head(df) 
df$Query<-"Final energy by aggregate end-use sector and fuel"
df$Title<-"Final Energy by Sec by Fuel"
df$NewValue<-df$value*convEJ2TWh
df$NewUnits<-"~Final~Energy~(TWh)"  # Use ~ for spaces. Will be parsed later
df$x<-df$year
df$xLabel<-"Year"
df$Aggregate<-"sum"  # How to aggregate over spatial and temporal units
df$segment<-"Segment"
levX<-sort(unique(df$input))
df<-df %>% mutate (Fill1=subsector,
                   FillLabel1="Fuel",
                   FillPalette1="colorsX_Unassigned",
                   Fill2=input,
                   FillLabel2="Fuel",
                   FillPalette2="colorsX_Unassigned") %>%  
  subset(.,select=-c(technology,subsector,input));
df$Fill2 <- factor( as.character(df$Fill2), levels=rev(levX));
df_all<-rbind.data.frame(df_all,df); head(df_all)

if(FALSE){
  # Check Data
  l1<-df%>%dplyr::filter(scenario=="GCAM-USA_Disp13NoCoalNuc1_2WindSolarto2030AEOGasRef")
  l1$Fill<-l1$Fill2;
  l1$FillLabel<-l1$FillLabel2 
  l1$FillPalette<-l1$FillPalette2
  l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill,FillLabel,FillPalette)  # For National, by techs
  head(l1)
  if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
  if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
  l1<-rbind.data.frame(xsum,xmean);head(l1)
  l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
  p <- fig_Bar(l1)
  plot(p)
}


#--------------------------
#--------------------------

# Check a param
#head(df_all[df_all$param=="elecCapRetirements" & df_all$NewValue>0,])

# Limit to selected analysis range and regions
df_all<-df_all[df_all$region %in% c(US50,"USA"),]
df_all<-df_all[df_all$x<=max(range(rangeX)),]
df_all<-df_all[df_all$x>=min(range(rangeX)),]

#
df_all$scenario<-df_all$scenario%>%gsub(scenNameRefOrig,scenNameRef,.)%>%
    gsub(scenName1Orig,scenName1,.)%>%gsub(scenName2Orig,scenName2,.)%>%gsub(scenName3Orig,scenName3,.);
df_all$segment<-df_all$segment%>%gsub(" electricity","",.)
scenariosComp<<-scenariosComp%>%gsub(scenNameRefOrig,scenNameRef,.)%>%
  gsub(scenName1Orig,scenName1,.)%>%gsub(scenName2Orig,scenName2,.)%>%gsub(scenName3Orig,scenName3,.);
scenariosIndv<<-scenariosIndv%>%gsub(scenNameRefOrig,scenNameRef,.)%>%
  gsub(scenName1Orig,scenName1,.)%>%gsub(scenName2Orig,scenName2,.)%>%gsub(scenName3Orig,scenName3,.);
head(df_all)

df_all$scenario<-as.factor(df_all$scenario)
df_all$scenario <- factor( as.character(df_all$scenario), levels=c(scenariosComp) );
df_all<-df_all[order(df_all$scenario),];


df_allOrig<-df_all

rm(lty3,ltz1,ltz,lty2,lty1,
   df_all_elecCap,df_all_elecCapInvestCum,df_all_elecCapRetireCum,
   df_all_elecCapRetire,ltz2,ncx,df_all_elecGenbyVerSeg,
   jx,dfx,jy,df,
   #df_all_elecCapCum,df_all_elecProd,df_all_elecCapFac,
   df_all_capFacbyHorSeg); gc()

df_all$x<-as.numeric(as.character(df_all$x))
df_allOrig$x<-as.numeric(as.character(df_allOrig$x))
  
  #____________________________________________
  #____________________________________________
  
  # G. Base Maps
  #____________________________________________
  #____________________________________________
  
  
  #--- Choose base boundary files (Province, country, basins)
  shp<-shp_PNNL32Reg       # GCAM Regions
  shp1<-shp_USStates20m
  
  #USe 48 State Shape
  shpUS48<-shp1
  #shpUS48@data$STUSPS<-as.character(shpUS48@data$STUSPS)
  shpUS48<-shpUS48[shpUS48$STUSPS %in% US48,]
  shpUS48@data<-droplevels(shpUS48@data)
  plot(shpUS48)
  
  #USe 50 State Shape
  shpUS50<-shp1
  #shpUS48@data$STUSPS<-as.character(shpUS48@data$STUSPS)
  shpUS50<-shpUS50[shpUS50$STUSPS %in% US50,]
  shpUS50@data<-droplevels(shpUS50@data)
  plot(shpUS50)
  
  # GCAM Regional Boundaries Selected Region
  shpa<-shp
  plot(shpa)
  
  # GCAM US Boundaries Bounding Box
  b1<-as.data.frame(bbox(shpUS48))   # Get Bounding box
  expandbyPercent<-10; b1$min;b1$max
  b1$min[1]<-if(b1$min[1]<0){(1+expandbyPercent/100)*b1$min[1]}else{(1-expandbyPercent/100)*b1$min[1]};
  b1$min[2]<-if(b1$min[2]<0){(1+expandbyPercent/100)*b1$min[2]}else{(1-expandbyPercent/100)*b1$min[2]};
  b1$max[1]<-if(b1$max[1]<0){(1-expandbyPercent/100)*b1$max[1]}else{(1+expandbyPercent/100)*b1$max[1]};
  b1$max[2]<-if(b1$max[2]<0){(1-expandbyPercent/100)*b1$max[2]}else{(1+expandbyPercent/100)*b1$max[2]};
  b1$min;b1$max;
  b1<-as(extent(as.vector(t(b1))), "SpatialPolygons")
  proj4string(b1)<-CRS(projX) # ASSIGN COORDINATE SYSTEM
  
  
  #---------------------
  # Base maps - Admin Boundaries Used 
  #---------------------
  
  # Create Output Directory for each Scenario
  if(dir.exists(paste(wd0,"/",finalFigsOutFolder,sep=""))){unlink(paste(wd0,"/",finalFigsOutFolder,sep=""),recursive=T)}
  if(!dir.exists(paste(wd0,"/",finalFigsOutFolder,sep=""))){dir.create(paste(wd0,"/",finalFigsOutFolder,sep=""))}
  if(!dir.exists(paste(wd0,"/",finalFigsOutFolder,sep=""))){dir.create(paste(wd0,"/",finalFigsOutFolder,sep=""))}
  dir<-paste(wd0,"/",finalFigsOutFolder,sep="")
  
  #-----------------------  Admin Boundaries with Labels
  
  
  m1<- tm_shape(shpUS48) + 
    tm_fill("MAP_COLORS", style="pretty",palette="Spectral",legend.show=F)  +
    tm_borders("grey") +
    tm_text("STUSPS",scale=0.8,auto.placement=F, col="black") +
    tm_compass(north=0,type="arrow", position=c("right", "bottom"),size=1.5) +
    tm_scale_bar(position=c("left", "bottom"),width=0.2)+
    tm_layout(frame = TRUE, bg.color="white", 
              main.title=paste("US lower 48 State Map",sep="")) + tm_layout_z +
    tm_layout(inner.margins=rep(0.05,4))
  m1
  
  print_PDFPNG(m1,dir=dir,filename=paste("basemaps_m1_US48Labelled",sep=""),figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  
  #-----------------------  Admin Boundaries for plotting
  
  
  m2<- tm_shape(shpUS48) + 
    tm_borders("black") +
    tm_layout(inner.margins=rep(0.05,4))
  m2
  m2<<-m2
  
  print_PDFPNG(m2,dir=dir,filename=paste("basemaps_m2_US48BlankForPlotting",sep=""),figWidth_InchMaster,figHeight_InchMaster,pdfpng=pdfpng)
  
  
  
  #____________________________________________
  #____________________________________________
  # Final Figures
  #____________________________________________
  #____________________________________________
  
  # Final Figures only using Scenarios to Compare 
  df_all<-df_allOrig%>%dplyr::filter(scenario %in% scenariosComp);unique(df_allOrig$scenario);unique(df_all$scenario)
  df_all<-df_all%>%dplyr::filter(x %in% rangeFigs);head(df_all)
  params<-unique(df_all$param);params  # For all Parameters
  
  if(TRUE){
  #----------------------------------
  # Figure 1 - Scenarios 1 x 2
  # a) 1x1 Single Chart National Gas Price vs year for (Lo, Hi, Shock)
  # b) 1x1 Single Chart National Electricity Demand vs year for (Hi, Lo Demands)
  #----------------------------------
  
  # Figure 1a 
  paramx<-"regNatGasPrice"
  
  l1<-df_all%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits)  # For National, by techs
  l1<-l1%>%filter(param==paramx); head(l1)
  
  if(nrow(l1)!=0){
    
    if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
    if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
    l1<-rbind.data.frame(xsum,xmean);head(l1)
    
    l1a<-l1%>%filter(scenario==scenName3)%>%droplevels
    l1a$scenario<-"Data used in model"
    l1<-l1%>%filter(scenario!=scenName3)%>%droplevels
    #l1<-l1%>%filter(scenario!="Historical Price Analog")%>%droplevels
    
    
    # Source: https://www.eia.gov/dnav/ng/ng_pri_fut_s1_d.htm
    EIAGasPrice<-data.frame(x=c(1998:2017),
                NewValue=c(2.20,2.39,4.53,4.17,3.56,5.76,6.20,9.14,7.08,	
                           7.33,9.32,4.15,4.60,4.21,2.89,3.92,4.60,2.76,	
                           2.65,3.15))
    
    EIAGasPrice<-EIAGasPrice%>%mutate(scenario="EIA",
                                      param=unique(l1$param),
                                      xLabel=unique(l1$xLabel),
                                      Aggregate=unique(l1$Aggregate),
                                      NewUnits=unique(l1$NewUnits))
    l1<-rbind.data.frame(l1,EIAGasPrice);head(l1)
    
    # Source: https://www.eia.gov/dnav/ng/ng_pri_fut_s1_d.htm
    HistAnalog<-data.frame(x=c(2015:2050),
                            NewValue=c(4.674,	4.917,	5.16,	5.402,	5.645,	5.888,	
                                       5.619,	5.351,	5.083,	4.815,	4.546,	4.068,	
                                       3.589,	3.111,	2.632,	2.153,	2.339,	4.441,	
                                       4.08,	3.483,	5.636,	6.069,	8.954,	6.934,	
                                       7.182,	9.129,	4.06,	4.503,	4.122,	2.834,	3.843,	
                                       4.503,	2.7,	2.597,	3.081,	4.632))
    
    HistAnalog<-HistAnalog%>%mutate(scenario="Historical Price Variation",
                                      param=unique(l1$param),
                                      xLabel=unique(l1$xLabel),
                                      Aggregate=unique(l1$Aggregate),
                                      NewUnits=unique(l1$NewUnits))
    l1<-rbind.data.frame(l1,HistAnalog);head(l1)
    
    scen_Order<-c("EIA","Historical Price Variation",scenariosComp);scen_Order
    l1$scenario <- factor( as.character(l1$scenario), levels=scen_Order );
    colorsX_Temp<<-c("gray70","black","red","green3","blue","gray30")
    
    l1$x<-as.numeric(as.character(l1$x))
    l1a$x<-as.numeric(as.character(l1a$x))
    
    p <- fig_LineCompareScenario(l1) + if(titleOn==1){ggtitle (paste(unique(l1$Title)," ","National US ",NumStates," States"," ",scenario_i,sep=""))}else{ggtitle(NULL)} 
    p <- p + geom_point(data =l1%>%filter(scenario!="Historical Price Variation" & scenario!="EIA"),
                        aes(x = x, y = NewValue,color=scenario),size=5)
    p <- p + geom_point(data = l1a,aes(x = x, y = NewValue),size=5,color="black")
    p <- p + expand_limits(y = 0) + scale_color_manual(values=colorsX_Temp,name="Scenario")
    p <- p + scale_x_continuous(breaks=seq(1995,2050,by=5)) + expand_limits(x = 1995)
    plot(p)
    
    fname<-paste("FinalFigLines_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
    print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1.2,pdfpng=pdfpng)
    
    tname<-paste("table_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
    write.csv(l1,paste(dir,"/",tname,".csv",sep=""),row.names=F)
    
    } # Close if empty rows 
  }# Close if TRUE
 
  if(TRUE){
  #---------------------------------------------
  # Figure 2 - Reference Case 1 x 3
  # a) Single Bar Chart National Elec Prod by year
  # b) Single Bar Chart National Cum Cap by year
  # c) Single Bar Chart National Cap Factor by year
  #----------------------------------------------
  
  paramsTemp<-c("elecProdByTechVint","elecCumCap","elecCapFacByTech")
  
  df_all<-df_allOrig%>%dplyr::filter(scenario %in% scenariosComp);unique(df_allOrig$scenario);unique(df_all$scenario)
  df_all<-df_all%>%dplyr::filter(x %in% seq(2010,2050,by=5));head(df_all)
  
  for(paramx in paramsTemp){
    
    l1<-df_all%>%dplyr::filter(scenario==scenNameRef)
    # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
    l1$Fill<-l1$Fill2;
    l1$FillLabel<-l1$FillLabel2 
    l1$FillPalette<-l1$FillPalette2
    l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                           Fill,FillLabel,FillPalette)  # For National, by techs
    l1<-l1%>%filter(param==paramx); head(l1)
    
    if(nrow(l1)!=0){
      
      if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
      if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
      l1<-rbind.data.frame(xsum,xmean);head(l1)
      l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
      
      l1<-l1%>%filter(x>2010)
      p <- fig_Bar(l1)
      plot(p)
      fname<-paste("FinalFigBarRef_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*0.8,figHeight_InchMaster*0.8,pdfpng=pdfpng)
      
      p <- fig_LineMultiple(l1)
      if(paramx=="elecCapFacByTech"){p<-p+expand_limits(y=c(0,1))}
      plot(p)
      fname<-paste("FinalFigLinesRef_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*0.8,figHeight_InchMaster*0.8,pdfpng=pdfpng)
      
      tname<-paste("table_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      write.csv(l1,paste(dir,"/",tname,".csv",sep=""),row.names=F)
      
      
    } # Close if empty rows 
    
  }
  }# Close if TRUE
  
  if(TRUE){
  #---------------------------------------------
  # Looking at Gas details
  # Figure 2a - Reference Case 1 x 3
  # a) Single Bar Chart National Elec Prod by year
  # b) Single Bar Chart National Cum Cap by year
  # c) Single Bar Chart National Cap Factor by year
  #----------------------------------------------
  
  paramsTemp<-c("elecProdByTechVint","elecCumCap","elecCapFacByTech")
  
  df_all<-df_allOrig%>%dplyr::filter(scenario %in% scenariosComp);unique(df_allOrig$scenario);unique(df_all$scenario)
  df_all<-df_all%>%dplyr::filter(x %in% seq(2010,2050,by=5));head(df_all)
  
  for(paramx in paramsTemp){
    
    l1<-df_all%>%dplyr::filter(scenario==scenNameRef,Fill2!="gas")
    # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
    l1$Fill<-l1$Fill2;
    l1$FillLabel<-l1$FillLabel2 
    l1$FillPalette<-l1$FillPalette2
    l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                           Fill,FillLabel,FillPalette)  # For National, by techs
    l1<-l1%>%filter(param==paramx); head(l1)
    
    # Gas Broken Out by Technologies
    l1a<-df_all%>%dplyr::filter(scenario==scenNameRef & Fill2=="gas")
    # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
    l1a$Fill<-l1a$Fill1;
    l1a$FillLabel<-l1a$FillLabel1 
    l1a$FillPalette<-l1a$FillPalette1
    l1a<-l1a%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                           Fill,FillLabel,FillPalette)  # For National, by techs
    l1a<-l1a%>%filter(param==paramx); head(l1a)
    
    l1<-rbind.data.frame(l1,l1a);head(l1)
    
   if(nrow(l1)!=0){
      
      if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
      if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
      l1<-rbind.data.frame(xsum,xmean);head(l1)
      l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
      l1<-l1%>%mutate(FillLabel=="Subsector")
      
      if(FALSE){
      # 2010 EIA Data - Gas broken down by 2015 model result pecentages
      # Source: https://www.eia.gov/electricity/data.php#generation 
      if(paramx=="elecCumCap")
      l1<-l1%>%mutate(NewValue=case_when( (scenario==scenNameRef & x==2010 & Fill=="gas (CC CCS)")~467.21*0,
                                          (scenario==scenNameRef & x==2010 & Fill=="gas (CC)")~467.21*0.68,
                                          (scenario==scenNameRef & x==2010 & Fill=="gas (CT)")~467.21*0.21,
                                          (scenario==scenNameRef & x==2010 & Fill=="gas (steam)")~467.21*0.11,
                                          (scenario==scenNameRef & x==2010 & Fill=="biomass")~5.04,
                                          (scenario==scenNameRef & x==2010 & Fill=="coal")~342.50,
                                          (scenario==scenNameRef & x==2010 & Fill=="geothermal")~3.5,
                                          (scenario==scenNameRef & x==2010 & Fill=="hydro")~78.2,
                                          (scenario==scenNameRef & x==2010 & Fill=="nuclear")~106.73,
                                          (scenario==scenNameRef & x==2010 & Fill=="refined liquids")~62.50,
                                          (scenario==scenNameRef & x==2010 & Fill=="solar")~0.99,
                                          (scenario==scenNameRef & x==2010 & Fill=="wind")~39.52, 
                                          TRUE~NewValue))
    
      # Source: https://www.eia.gov/electricity/data.php#generation 
      #EIACap2010<-data.frame(Fill=c("biomass","coal","gas","geothermal","hydro","nuclear",
      #                              "refined liquids","solar","wind","other"),
       #                      NewValue=c(5.04,342.30,467.21,3.50,78.20,106.73,62.50,0.99,39.52,32.64))
      }
      
      l1<-l1%>%filter(NewValue > 1E-15 | NewValue < -1E-15);l1<-droplevels(l1);head(l1)
      
      l1<-l1%>%filter(x>2010)
      p <- fig_Bar(l1)
      plot(p)
      fname<-paste("FinalFigBarRefGas_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*0.8,figHeight_InchMaster*0.8,pdfpng=pdfpng)
      
      l1$Fill = with(l1, factor(Fill, levels = rev(levels(Fill))))
      p <- fig_LineMultiple(l1)
      if(paramx=="elecCapFacByTech"){p<-p+expand_limits(y=c(0,1))}
      plot(p)
      fname<-paste("FinalFigLinesRefGas_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*0.8,figHeight_InchMaster*0.8,pdfpng=pdfpng)
      
      tname<-paste("tableGas_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
      write.csv(l1,paste(dir,"/",tname,".csv",sep=""),row.names=F)
      
      
    } # Close if empty rows 
    
  }
  } #Close TRUE
  
  df_all<-df_allOrig%>%dplyr::filter(scenario %in% scenariosComp);unique(df_allOrig$scenario);unique(df_all$scenario)
  df_all<-df_all%>%dplyr::filter(x %in% rangeFigs);head(df_all)
  
  
  if(TRUE){
   #------------------------------------
  # Figure 4 - Dispatch Curve Eg for TX - 2030
  # a) 5 x 2 Dispatch TX 2030 by hor seg
   #------------------------------------
   
   l1<-df_all%>%dplyr::filter(scenario==scenNameRef);head(l1)
   # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
   l1$Fill<-l1$Fill1;
   l1$FillLabel<-l1$FillLabel1 
   l1$FillPalette<-l1$FillPalette1
   l1<-l1%>%dplyr::select(param,region,NewValue,scenario,year,x,xLabel,Aggregate,NewUnits,
                          Fill,FillLabel,FillPalette,vintage)  # For National, by techs
   
   l1<-l1%>%filter(region=="TX")
   
   if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
   if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
   l1<-rbind.data.frame(xsum,xmean);head(l1)
   l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
   
   if(nrow(l1)!=0){
     
     df1<-l1%>%dplyr::filter(param=="elecCumCap")
     dfx<-l1%>%dplyr::filter(param=="elecGenDispCost")%>%dplyr::select(region,vintage,scenario,year,Fill,NewValue,NewUnits)%>%
       rename(Cost=NewValue,CostUnits=NewUnits)
     
     test<-dfx%>%filter((Fill=="coal (conv pul)" | Fill =="gas (CC)"),region=="TX")%>%spread(Fill,Cost)%>%
       mutate(Diff=`coal (conv pul)`-`gas (CC)`); test
     
     
     lx<-join(df1,dfx,by=c("scenario","Fill","region","year","vintage"))
     lx$CostUnits<-unique(dfx$CostUnits)[1]
     
     lx<-lx[complete.cases(lx),]
     
     lx<-lx[order(lx$Cost),]%>%
       group_by(scenario,region,year)%>%
       mutate(w = cumsum(NewValue))%>%
       mutate(wm=w-NewValue)%>%
       mutate(wt=(wm + (w - wm)/2))%>%as.data.frame
     head(lx)
     
     lx<-lx%>%dplyr::filter(Cost<mean(as.numeric(lx$NewValue))+sd(as.numeric(lx$NewValue))*1E+3)
     lx<-droplevels(lx)
     head(lx)
     
     lx<-lx%>%mutate(Fill = recode_factor(Fill,"Gen_III" = "nuclear","Gen_II_LWR"="nuclear",
                                          "refined liquids (steam)"="refined liquids",
                                          "refined liquids (CT)"="refined liquids",
                                          "refined liquids (CC)"="refined liquids"))
     if(nrow(lx[lx$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(lx[lx$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
     if(nrow(lx[lx$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(lx[lx$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
     lx<-rbind.data.frame(xsum,xmean);head(lx)
     lx$Fill<-as.factor(lx$Fill);lx<-droplevels(lx);head(lx)
     
     
     for (i in c(2030,2050)){
       l1<-lx[lx$year==i,]
       if(nrow(l1)!=0){
    
           
           l1<-l1[l1$Cost<(mean(l1$Cost)+2*sd(l1$Cost)),]
           p <-  fig_dispatchCurve(l1) + ggtitle(paste(unique(l1$region)," - ",i,sep=""))
           p<- p + xlab(paste('Capacity (GW)',sep="")) + ylab('Variable Generation Cost (2015 USD/MWh)')
           plot(p)
           
           fname<-paste("FinalFigDispatch_",unique(l1$region),"_",min(range(l1$x)),sep="")
           print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1,pdfpng=pdfpng)
          
         }
     }
   } 
   
  } # Close IF TRUE
  
  if(TRUE){
    #------------------------------------
    # Central East Grid
    # Figure 4 - Dispatch Curve Eg for TX - 2030
    # a) 5 x 2 Dispatch TX 2030 by hor seg
    #------------------------------------
    
    l1<-df_all%>%dplyr::filter(scenario==scenNameRef);head(l1)
    # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
    l1$Fill<-l1$Fill1;
    l1$FillLabel<-l1$FillLabel1 
    l1$FillPalette<-l1$FillPalette1
    l1<-l1%>%dplyr::select(param,region,NewValue,scenario,year,x,xLabel,Aggregate,NewUnits,
                           Fill,FillLabel,FillPalette,vintage)  # For National, by techs
    
    lgridStateX<-lgridState%>%mutate(region=state)%>%dplyr::select(region,grid_region)
    l1<-join(l1,lgridStateX,by="region")%>%mutate(region=grid_region)%>%dplyr::select(-grid_region);
    head(l1)
    
    l1<-l1%>%filter(region=="Central East grid")
    
    if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
    if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
    l1<-rbind.data.frame(xsum,xmean);head(l1)
    l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
    
    if(nrow(l1)!=0){
      
      df1<-l1%>%dplyr::filter(param=="elecCumCap")
      dfx<-l1%>%dplyr::filter(param=="elecGenDispCost")%>%dplyr::select(region,vintage,scenario,year,Fill,NewValue,NewUnits)%>%
        rename(Cost=NewValue,CostUnits=NewUnits)
      
      test<-dfx%>%filter((Fill=="coal (conv pul)" | Fill =="gas (CC)"))%>%spread(Fill,Cost)%>%
        mutate(Diff=`coal (conv pul)`-`gas (CC)`); test
      
      
      lx<-join(df1,dfx,by=c("scenario","Fill","region","year","vintage"))
      lx$CostUnits<-unique(dfx$CostUnits)[1]
      
      lx<-lx[complete.cases(lx),]
      
      lx<-lx[order(lx$Cost),]%>%
        group_by(scenario,region,year)%>%
        mutate(w = cumsum(NewValue))%>%
        mutate(wm=w-NewValue)%>%
        mutate(wt=(wm + (w - wm)/2))%>%as.data.frame
      head(lx)
      
      lx<-lx%>%dplyr::filter(Cost<mean(as.numeric(lx$NewValue))+sd(as.numeric(lx$NewValue))*1E+3)
      lx<-droplevels(lx)
      head(lx)
      
      lx<-lx%>%mutate(Fill = recode_factor(Fill,"Gen_III" = "nuclear","Gen_II_LWR"="nuclear",
                                           "refined liquids (steam)"="refined liquids",
                                           "refined liquids (CT)"="refined liquids",
                                           "refined liquids (CC)"="refined liquids"))
      if(nrow(lx[lx$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(lx[lx$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
      if(nrow(lx[lx$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(lx[lx$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
      lx<-rbind.data.frame(xsum,xmean);head(lx)
      lx$Fill<-as.factor(lx$Fill);lx<-droplevels(lx);head(lx)
      
      
      for (i in c(2030,2050)){
        l1<-lx[lx$year==i,]
        if(nrow(l1)!=0){
          
          
          l1<-l1[l1$Cost<(mean(l1$Cost)+2*sd(l1$Cost)),]
          p <-  fig_dispatchCurve(l1) + ggtitle(paste(unique(l1$region)," - ",i,sep=""))
          p<- p + xlab(paste('Capacity (GW)',sep="")) + ylab('Variable Generation Cost (2015 USD/MWh)')
          plot(p)
          
          fname<-paste("FinalFigDispatch_",unique(l1$region),"_",min(range(l1$x)),sep="")
          print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1,pdfpng=pdfpng)
          
        }
      }
    } 
    
  } # IF TRUE
   
   if(TRUE){
   #------------------------
   # Figure 4 Dispatch Generation by Hours
   #------------------
   
   # From : gcam-core\input\gcam-data-system\gcam-usa-data\assumptions\A23.seasonal_segments.csv
   
   l1<-df_all%>%dplyr::filter(scenario==scenNameRef & param=="elecGenbyVerSeg");head(l1)
   # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
   l1$Fill<-l1$Fill1;
   l1$FillLabel<-l1$FillLabel1 
   l1$FillPalette<-l1$FillPalette1
   l1<-l1%>%dplyr::select(param,NewValue,scenario,year,x,xLabel,Aggregate,NewUnits,
                          Fill,FillLabel,FillPalette,segment,region,vintage)  # For National, by techs
   head(l1)
   
   #l1<-l1%>%filter(region=="TX" | region=="CA" | region=="IL" | region=="WA")
   
   if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
   if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
   l1<-rbind.data.frame(xsum,xmean);head(l1)
   l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
     
     for (i in c(2030,2050)){
       l1a<-l1[l1$year==i,]
       if(nrow(l1a)!=0){
         if(length(unique(l1a$segment))>1){
           
           
           l1a<-join(l1a,df_segHours%>%mutate(region=state)%>%dplyr::select(-state),
                     by=c("region","segment"));head(l1a)
           l1a<-droplevels(l1a);head(l1a)
           
           
           l1a<-l1a%>%mutate(Fill = recode_factor(Fill,"Gen_III" = "nuclear","Gen_II_LWR"="nuclear",
                                                "refined liquids (steam)"="refined liquids",
                                                "refined liquids (CT)"="refined liquids",
                                                "refined liquids (CC)"="refined liquids"))
           if(nrow(l1a[l1a$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1a[l1a$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
           if(nrow(l1a[l1a$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1a[l1a$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
           l1a<-rbind.data.frame(xsum,xmean);head(l1a)
           l1a$Fill<-as.factor(l1a$Fill);l1a<-droplevels(l1a);head(l1a)
           
           
           l1asum<-l1a%>% 
             group_by(segment) %>% 
             summarise(Sum = sum(NewValue))%>% as.data.frame; head(l1asum)
           l1a<-join(l1a,l1asum,by="segment");head(l1a)
           l1a<-l1a%>%arrange(-Sum); head(l1a)
           
           seg_OrderTemp<-as.character(unique(l1a$segment));seg_OrderTemp
           seg_OrderTemp<-c("superpeak",seg_OrderTemp[!grepl("superpeak",seg_OrderTemp)]);seg_OrderTemp
           l1a$segment<-as.factor(l1a$segment)
           l1a$segment <- factor( as.character(l1a$segment), levels=seg_OrderTemp );
           l1a<-droplevels(l1a)
           
           l1b<-l1a%>%dplyr::select(-vintage)
           if(nrow(l1b[lx$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1b[l1b$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
           if(nrow(l1b[lx$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1b[l1b$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
           l1b<-rbind.data.frame(xsum,xmean);head(l1b)
           l1b$Fill<-as.factor(l1b$Fill);l1b<-droplevels(l1b);head(l1b)
           
           l2<-l1b%>%mutate(x=segment)
           l2<-l2[l2$NewValue>1E-10,]
           l2<-droplevels(l2)
           head(l2)
           
           # Grid Regions
           l2a<-join(l2,lgridState%>%mutate(region=state)%>%dplyr::select(region,grid_region),by="region");head(l2a)
           l2a<-l2a%>%dplyr::select(-region)%>%rename(region=grid_region);head(l2a)
           if(nrow(l2a[lx$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l2a[l2a$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
           if(nrow(l2a[lx$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l2a[l2a$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
           l2a<-rbind.data.frame(xsum,xmean);head(l2a)
           l2a$Fill<-as.factor(l2a$Fill);l2a<-droplevels(l2a);head(l2a)
           p <- fig_BarXfactor(l2a)+ ggtitle(paste(i,sep=""))+facet_wrap(~region)
           plot(p)
           fname<-paste("FinalFigDispatchTWhHours_GridRegions",i,sep="")
           print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*3,figHeight_InchMaster*2,pdfpng=pdfpng)
           
           # National
           l2b<-l2%>%dplyr::select(-region)
           if(nrow(l2b[lx$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l2b[l2b$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
           if(nrow(l2b[lx$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l2b[l2b$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
           l2b<-rbind.data.frame(xsum,xmean);head(l2b)
           l2b$Fill<-as.factor(l2b$Fill);l2b<-droplevels(l2b);head(l2b)
           p <- fig_BarXfactor(l2b)+ ggtitle(paste(i,sep=""))
           plot(p)
           fname<-paste("FinalFigDispatchTWhHours_National",i,sep="")
           print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1.2,pdfpng=pdfpng)
           
         
           l3<-l1a%>%mutate(NewValue=NewValue*1000/(hours))%>%
                    mutate(NewUnits="~Load~(GW)")%>%
                    mutate(x=segment)
           l3<-l3[l3$NewValue>1E-15,]
           l3<-droplevels(l3)
           head(l3)
           
           l3$segment<-as.factor(l3$segment)
           l3$segment <- factor( as.character(l3$segment), levels=seg_OrderTemp );
           l3<-droplevels(l3)
           
           # Grid Regions
           l3a<-join(l3,lgridState%>%mutate(region=state)%>%dplyr::select(region,grid_region),by="region");head(l3a)
           l3a<-l3a%>%dplyr::select(-region)%>%rename(region=grid_region);head(l3a)
           if(nrow(l3a[lx$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l3a[l3a$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
           if(nrow(l3a[lx$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l3a[l3a$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
           l3a<-rbind.data.frame(xsum,xmean);head(l3a)
           l3a$Fill<-as.factor(l3a$Fill);l3a<-droplevels(l3a);head(l3a)
           p <- fig_BarXfactor(l3a)+ ggtitle(paste(i,sep=""))+facet_wrap(~region)
           plot(p)
           fname<-paste("FinalFigDispatchGW_GridRegions",i,sep="")
           print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*3,figHeight_InchMaster*2,pdfpng=pdfpng)
           
           # National
           l3b<-l3%>%dplyr::select(-region)
           if(nrow(l3b[lx$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l3b[l3b$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
           if(nrow(l3b[lx$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l3b[l3b$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
           l3b<-rbind.data.frame(xsum,xmean);head(l3b)
           l3b$Fill<-as.factor(l3b$Fill);l3b<-droplevels(l3b);head(l3b)
           p <- fig_BarXfactor(l3b)+ ggtitle(paste(i,sep=""))
           plot(p)
           fname<-paste("FinalFigDispatchGW_National",i,sep="")
           print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1.2,pdfpng=pdfpng)
          
           
        
           
         }}
     }

   } # IF TRUE
  
  if(TRUE){

  #-------------------------------------------------------------------------
  # Figure 5 - Scenarios 4 x 1
  # a) 1 x 3 Elec Prod by year Facets Gas Scenarios (Ref, Diff1, Diff2, Diff3)
  # b) 1 x 3 Elec Prod by year Facets Demand Scenarios (Ref, Diff1, Diff2, Diff3)
  # c) 1 x 3 Cum Cap by year Facets Gas Scenarios (Ref, Diff1, Diff2, Diff3)
  # d) 1 x 3 Cum Cap by year Facets Demand Scenarios (Ref, Diff1, Diff2, Diff3)
   #-------------------------------------------------------------------------
   
   
   paramsTemp<-c("elecProdByTechVint","elecCumCap")
   
   for(paramx in paramsTemp){
     
     l1<-df_all
     # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
     l1$Fill<-l1$Fill2;
     l1$FillLabel<-l1$FillLabel2 
     l1$FillPalette<-l1$FillPalette2
     l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                            Fill,FillLabel,FillPalette)  # For National, by techs
     l1<-l1%>%filter(param==paramx); head(l1)
     
     if(nrow(l1)!=0){
       
       if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
       if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
       l1<-rbind.data.frame(xsum,xmean);head(l1)
       l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
       
       lx<-l1%>%spread(scenario,NewValue)%>%
         mutate(!!paste(scenName1,"_diff",sep=""):=get(scenName1)-get(scenNameRef))%>%
         mutate(!!paste(scenName2,"_diff",sep=""):=get(scenName2)-get(scenNameRef))%>%
         mutate(!!paste(scenName3,"_diff",sep=""):=get(scenName3)-get(scenNameRef))%>%
         dplyr::select(-c(scenNameRef,scenName1,scenName2,scenName3))%>%
         gather(key=scenario,value=NewValue,-param,-x,-xLabel,-Aggregate,-NewUnits,-Fill,-FillLabel,-FillPalette);head(lx)
       
       lx$scenario<-lx$scenario%>%gsub("_diff","",.);head(lx)
       
       lx$scenario<-as.factor(lx$scenario)
       lx$scenario <- factor( as.character(lx$scenario), levels=c(scenariosComp) );
       lx<-lx[order(lx$scenario),];
       
       l2<-lx
       
       p <- fig_Bar(l2)
       p <- p+ facet_grid(~scenario)
       plot(p)
       fname<-paste("FinalFigBarDiff_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       p <- fig_LineMultiple(l2) 
       p<-p+ facet_grid(~scenario)
       plot(p)
       fname<-paste("FinalFigLinesDiff_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       
       # Reference Plots for similar size
       lx<-l1%>%spread(scenario,NewValue)%>%
         mutate(!!paste(scenNameRef,"1",sep=""):=get(scenNameRef))%>%
         mutate(!!paste(scenNameRef,"2",sep=""):=get(scenNameRef))%>%
         dplyr::select(-c(scenName1,scenName2,scenName3))%>%
         gather(key=scenario,value=NewValue,-param,-x,-xLabel,-Aggregate,-NewUnits,-Fill,-FillLabel,-FillPalette);head(lx)
       
       l1<-lx
       
       p <- fig_Bar(l1)
       p <- p+ facet_grid(~scenario)
       plot(p)
       fname<-paste("FinalFigBarDiffRef_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       p <- fig_LineMultiple(l1) 
       p<-p+ facet_grid(~scenario)
       plot(p)
       fname<-paste("FinalFigLinesDiffRef_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.5,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       
     } # Close if empty rows 
     
   } # Close Params
  } # Close if TRUE
  
  if(TRUE){
  #-------------------------------------------------------------------------
  # Figure 6 - Scenarios Elec Prod by State 2030 - 5 x 4
  # Maps (6x4) Rows - Scenarios (Gas Hi,lo,Spike, Dem Hi, Dem Lo)  Col - Coal, Gas, Renewable, Total
  #-------------------------------------------------------------------------
   
   
   #----------
   # Maps State Total Year by SubSector
   #-----------
   
   paramsTemp<-c("elecProdByTechVint","elecCumCap","elecDelivB4Losses")
   
   l1<-df_all%>%dplyr::filter(region %in% US48);head(l1)
   # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
   l1$Fill<-l1$Fill2;
   l1$FillLabel<-l1$FillLabel2 
   l1$FillPalette<-l1$FillPalette2
   l1<-l1%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                          Fill,FillLabel,FillPalette)  # For National, by techs
   
   if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
   if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
   l1<-rbind.data.frame(xsum,xmean);head(l1)
   l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
   
   
   # Select parameters
   
   for(paramx in paramsTemp){
     
     lx<-l1%>%filter(param==paramx,x==2030); head(lx)
     # Get Data for Coal, Gas, Aggregated for Renewables & Total
     lx1<-lx%>%filter(Fill %in% c("coal","gas"));head(lx1)
     lx2<-lx%>%filter(Fill %in% c("solar","wind"))%>%dplyr::select(-Fill)%>%mutate(Fill="Renewables")%>%
       group_by_at(vars(one_of((names(l1)[names(l1)!="NewValue"]))))%>%summarize(NewValue = sum(NewValue))%>%as.data.frame();head(lx2)
     lx3<-lx%>%dplyr::select(-Fill)%>%mutate(Fill="Total")%>%
       group_by_at(vars(one_of((names(l1)[names(l1)!="NewValue"]))))%>%summarize(NewValue = sum(NewValue))%>%as.data.frame();head(lx3)
     lx<-rbind.data.frame(lx1,lx2,lx3)
     lx<-droplevels(lx)
     
     
     
     lx<-lx%>%spread(scenario,NewValue)%>%
       mutate(!!paste(scenName1,"_diff",sep=""):=get(scenName1)-get(scenNameRef))%>%
       mutate(!!paste(scenName2,"_diff",sep=""):=get(scenName2)-get(scenNameRef))%>%
       mutate(!!paste(scenName3,"_diff",sep=""):=get(scenName3)-get(scenNameRef))%>%
       mutate(!!paste(scenName1,"_prcnt",sep=""):=(get(scenName1)-get(scenNameRef))*100/get(scenNameRef))%>%
       mutate(!!paste(scenName2,"_prcnt",sep=""):=(get(scenName2)-get(scenNameRef))*100/get(scenNameRef))%>%
       mutate(!!paste(scenName3,"_prcnt",sep=""):=(get(scenName3)-get(scenNameRef))*100/get(scenNameRef))%>%
       mutate(!!paste(scenNameRef,"1",sep=""):=get(scenNameRef))%>%
       mutate(!!paste(scenNameRef,"2",sep=""):=get(scenNameRef))
     
     
     
     tname<-paste("tableMap_",paramx,"_",unique(lx$x),sep="")
     write.csv(lx%>%mutate(year=x, fuel=Fill)%>%dplyr::select(-x,-xLabel,-Aggregate,-Fill,-FillLabel,-FillPalette,
                                                              -!!paste(scenNameRef,"1",sep=""),-!!paste(scenNameRef,"2",sep="")),paste(dir,"/",tname,".csv",sep=""),row.names=F)
     
     
     lx[mapply(is.infinite, lx)] <- 100
     lx[mapply(is.nan, lx)] <- NA
     
     
     lx<-lx%>%dplyr::select(-c(scenName1,scenName2,scenName3))
     
       
    
       #----------
       # Map by SUM of subsectors
       #-----------
       
       if(nrow(lx)!=0){
         
         #ly<-dcast(lx,region+scenario~Fill,value.var="NewValue",fun.aggregate=sum,na.rm=T)
         ly<-lx
         ly<-ly%>%dplyr::select(-contains("prcnt")); head(ly)
         ly<-ly%>%gather(key=scenario,value=NewValue,-region,-param,-x,-xLabel,-Aggregate,-NewUnits,-Fill,-FillLabel,-FillPalette);
         head(ly);
         ly$scenario<-ly$scenario%>%gsub("_diff","",.);head(ly)
         ly$scenario<-as.factor(ly$scenario)
         ly$scenario <- factor( as.character(ly$scenario), levels=c(scenariosComp,paste(scenNameRef,"1",sep=""),
                                                                    paste(scenNameRef,"2",sep="")) );
         ly<-ly[order(ly$scenario),];
         
         colnames(ly)[which(names(ly) == "region")] <- "STUSPS"; head(ly)
         
         if(nrow(ly)!=0){
           shpa.x<-shpUS48
           dfx<-NULL
           dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
             mutate(NewValue=0)%>%as.data.frame; head(dfa)
           dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
             mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
             dplyr::select(-c(NewValue.x,NewValue.y));
           
           for(scenario_i in unique(ly$scenario)[(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                             paste(scenNameRef,"2",sep="")))]){
             for(fill_i in unique(ly$Fill)){
               a1<-shpa.x
               a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
                 subset(select=c("STUSPS","scenario","Fill","NewValue"))
               if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
             }
           }
           head(dfx)
           
            
           colx<<-colorsAbsolute #------Choose color palette
          
           
          map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                      val="NewValue",var1="scenario",var2="Fill")+
             tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                       panel.label.size = 1.5,panel.label.height = 2)+m2; map
           print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarRef_",paramx,"_",unique(lx$x),sep=""),
                        figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
           
           colx<<-colorsAbsolute5
           map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                val="NewValue",var1="scenario",var2="Fill")+
             tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                       panel.label.size = 1.5,panel.label.height = 2)+m2; map
           print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarRefKMEANS_",paramx,"_",unique(lx$x),sep=""),
                        figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
           
       
           shpa.x<-shpUS48
           dfx<-NULL
           dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
             mutate(NewValue=0)%>%as.data.frame; head(dfa)
           dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
             mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
             dplyr::select(-c(NewValue.x,NewValue.y));
           
           for(scenario_i in unique(ly$scenario)[!(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                              paste(scenNameRef,"2",sep="")))]){
             for(fill_i in unique(ly$Fill)){
               a1<-shpa.x
               a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
                 subset(select=c("STUSPS","scenario","Fill","NewValue"))
               if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
             }
           }
           head(dfx)
           
           colx<<-colorsX_Diff
           
           dfx@data<-dfx@data%>%mutate(scenario = recode_factor(scenario,"Historical Gas Price variation" = "Historical Gas\nPrice variation"))
           dfx@data$scenario <- factor( as.character(dfx@data$scenario), levels=c(scenName1,scenName2,"Historical Gas\nPrice variation") );
           
           
           map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                val="NewValue",var1="scenario",var2="Fill")+
             tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                       title="Difference",panel.label.size = 1.5,panel.label.height = 2)+m2; map
           print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarDiff_",paramx,"_",unique(lx$x),sep=""),
                        figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
           
           colx<<-colorsX_Diff5
           map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                val="NewValue",var1="scenario",var2="Fill")+
             tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                       title="Difference",panel.label.size = 1.5,panel.label.height = 2)+m2; map
           print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarDiffKMEANS_",paramx,"_",unique(lx$x),sep=""),
                        figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
           
           
         }
     
     #ly<-dcast(lx,region+scenario~Fill,value.var="NewValue",fun.aggregate=sum,na.rm=T)
     ly<-lx
     ly<-ly%>%dplyr::select(-contains("diff")); head(ly)
     ly<-ly%>%gather(key=scenario,value=NewValue,-region,-param,-x,-xLabel,-Aggregate,-NewUnits,-Fill,-FillLabel,-FillPalette);
     head(ly);
     ly$scenario<-ly$scenario%>%gsub("_prcnt","",.);head(ly)
     ly$scenario<-as.factor(ly$scenario)
     ly$scenario <- factor( as.character(ly$scenario), levels=c(scenariosComp,paste(scenNameRef,"1",sep=""),
                                                                paste(scenNameRef,"2",sep="")) );
     ly<-ly[order(ly$scenario),];
     
     colnames(ly)[which(names(ly) == "region")] <- "STUSPS"; head(ly)
     if(nrow(ly)!=0){
       shpa.x<-shpUS48
       dfx<-NULL
       dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
         mutate(NewValue=0)%>%as.data.frame; head(dfa)
       dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
         mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
         dplyr::select(-c(NewValue.x,NewValue.y));
       
       for(scenario_i in unique(ly$scenario)[!(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                          paste(scenNameRef,"2",sep="")))]){
         for(fill_i in unique(ly$Fill)){
           a1<-shpa.x
           a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
             subset(select=c("STUSPS","scenario","Fill","NewValue"))
           if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
         }
       }
       head(dfx)
       
       #colx<<-colorsAbsolute #------Choose color palette
       #colx<<-(brewer.pal(9,"RdYlGn")) #------Choose color palette
       #pie(rep(1,length(colx)),label=names(colx),col=colx)
       colx<<-colorsX_Diff
       
       dfx@data<-dfx@data%>%mutate(scenario = recode_factor(scenario,"Historical Gas Price variation" = "Historical Gas\nPrice variation"))
       dfx@data$scenario <- factor( as.character(dfx@data$scenario), levels=c(scenName1,scenName2,"Historical Gas\nPrice variation") );
       
       map <- mapX_fill2VarKmeans(data=dfx,scaleData=data.frame(NewValue=seq(-100,100,by=5)),
                                  val="NewValue",var1="scenario",var2="Fill")+
         tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                   title="% Difference", panel.label.size = 1.5,panel.label.height = 2)+m2; map
     print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarPrcntKMEANS_",paramx,"_",unique(lx$x),sep=""),
                  figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
     
     colx<<-colorsX_Diff5
      map <- mapX_fill2Var(data=dfx,scaleData=data.frame(NewValue=seq(-100,100,by=5)),
                                    val="NewValue",var1="scenario",var2="Fill")+
           tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                     title="% Difference", panel.label.size = 1.5,panel.label.height = 2)+m2; map
       print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarPrcnt_",paramx,"_",unique(lx$x),sep=""),
                    figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
       

     }
     
     }
   } # Close Params
   
   colx<<-colorsAbsolute #------Choose color palette
  } # Close if TRUE
  
  
  if(TRUE){
    #-------------------------------------------------------------------------
    # Figure 6 WITHOUT TOTALS
    # Figure 6X - Scenarios Elec Prod by State 2030 - 5 x 4
    # Maps (6x4) Rows - Scenarios (Gas Hi,lo,Spike, Dem Hi, Dem Lo)  Col - Coal, Gas, Renewable, Total
    #-------------------------------------------------------------------------
    
    
    #----------
    # Maps State Total Year by SubSector
    #-----------
    
    paramsTemp<-c("elecProdByTechVint","elecCumCap","elecDelivB4Losses")
    
    l1<-df_all%>%dplyr::filter(region %in% US48);head(l1)
    # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
    l1$Fill<-l1$Fill2;
    l1$FillLabel<-l1$FillLabel2 
    l1$FillPalette<-l1$FillPalette2
    l1<-l1%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                           Fill,FillLabel,FillPalette)  # For National, by techs
    
    if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
    if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
    l1<-rbind.data.frame(xsum,xmean);head(l1)
    l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
    
    
    # Select parameters
    
    for(paramx in paramsTemp){
      
      lx<-l1%>%filter(param==paramx,x==2030); head(lx)
      # Get Data for Coal, Gas, Aggregated for Renewables & Total
      lx1<-lx%>%filter(Fill %in% c("coal","gas"));head(lx1)
      lx2<-lx%>%filter(Fill %in% c("solar","wind"))%>%dplyr::select(-Fill)%>%mutate(Fill="Renewables")%>%
        group_by_at(vars(one_of((names(l1)[names(l1)!="NewValue"]))))%>%summarize(NewValue = sum(NewValue))%>%as.data.frame();head(lx2)
      #lx3<-lx%>%dplyr::select(-Fill)%>%mutate(Fill="Total")%>%
      #  group_by_at(vars(one_of((names(l1)[names(l1)!="NewValue"]))))%>%summarize(NewValue = sum(NewValue))%>%as.data.frame();head(lx3)
      lx<-rbind.data.frame(lx1,lx2#,lx3
                           )
      lx<-droplevels(lx)
      
      
      
      lx<-lx%>%spread(scenario,NewValue)%>%
        mutate(!!paste(scenName1,"_diff",sep=""):=get(scenName1)-get(scenNameRef))%>%
        mutate(!!paste(scenName2,"_diff",sep=""):=get(scenName2)-get(scenNameRef))%>%
        mutate(!!paste(scenName3,"_diff",sep=""):=get(scenName3)-get(scenNameRef))%>%
        mutate(!!paste(scenName1,"_prcnt",sep=""):=(get(scenName1)-get(scenNameRef))*100/get(scenNameRef))%>%
        mutate(!!paste(scenName2,"_prcnt",sep=""):=(get(scenName2)-get(scenNameRef))*100/get(scenNameRef))%>%
        mutate(!!paste(scenName3,"_prcnt",sep=""):=(get(scenName3)-get(scenNameRef))*100/get(scenNameRef))%>%
        mutate(!!paste(scenNameRef,"1",sep=""):=get(scenNameRef))%>%
        mutate(!!paste(scenNameRef,"2",sep=""):=get(scenNameRef))
      
      
      
      tname<-paste("tableMap_NOTOTAL_",paramx,"_",unique(lx$x),sep="")
      write.csv(lx%>%mutate(year=x, fuel=Fill)%>%dplyr::select(-x,-xLabel,-Aggregate,-Fill,-FillLabel,-FillPalette,
                                                               -!!paste(scenNameRef,"1",sep=""),-!!paste(scenNameRef,"2",sep="")),paste(dir,"/",tname,".csv",sep=""),row.names=F)
      
      
      lx[mapply(is.infinite, lx)] <- 100
      lx[mapply(is.nan, lx)] <- NA
      
      
      lx<-lx%>%dplyr::select(-c(scenName1,scenName2,scenName3))
      
      
      
      #----------
      # Map by SUM of subsectors
      #-----------
      
      if(nrow(lx)!=0){
        
        #ly<-dcast(lx,region+scenario~Fill,value.var="NewValue",fun.aggregate=sum,na.rm=T)
        ly<-lx
        ly<-ly%>%dplyr::select(-contains("prcnt")); head(ly)
        ly<-ly%>%gather(key=scenario,value=NewValue,-region,-param,-x,-xLabel,-Aggregate,-NewUnits,-Fill,-FillLabel,-FillPalette);
        head(ly);
        ly$scenario<-ly$scenario%>%gsub("_diff","",.);head(ly)
        ly$scenario<-as.factor(ly$scenario)
        ly$scenario <- factor( as.character(ly$scenario), levels=c(scenariosComp,paste(scenNameRef,"1",sep=""),
                                                                   paste(scenNameRef,"2",sep="")) );
        ly<-ly[order(ly$scenario),];
        
        colnames(ly)[which(names(ly) == "region")] <- "STUSPS"; head(ly)
        
        if(nrow(ly)!=0){
          shpa.x<-shpUS48
          dfx<-NULL
          dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
            mutate(NewValue=0)%>%as.data.frame; head(dfa)
          dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
            mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
            dplyr::select(-c(NewValue.x,NewValue.y));
          
          for(scenario_i in unique(ly$scenario)[(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                            paste(scenNameRef,"2",sep="")))]){
            for(fill_i in unique(ly$Fill)){
              a1<-shpa.x
              a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
                subset(select=c("STUSPS","scenario","Fill","NewValue"))
              if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
            }
          }
          head(dfx)
          
          
          colx<<-colorsAbsolute #------Choose color palette
          
          
          map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                               val="NewValue",var1="scenario",var2="Fill")+
            tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                      panel.label.size = 1.5,panel.label.height = 2)+m2; map
          print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarRef_NOTOTAL_",paramx,"_",unique(lx$x),sep=""),
                       figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
          
          colx<<-colorsAbsolute5
          map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                     val="NewValue",var1="scenario",var2="Fill")+
            tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                      panel.label.size = 1.5,panel.label.height = 2)+m2; map
          print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarRefKMEANS_NOTOTAL_",paramx,"_",unique(lx$x),sep=""),
                       figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
          
          
          shpa.x<-shpUS48
          dfx<-NULL
          dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
            mutate(NewValue=0)%>%as.data.frame; head(dfa)
          dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
            mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
            dplyr::select(-c(NewValue.x,NewValue.y));
          
          for(scenario_i in unique(ly$scenario)[!(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                             paste(scenNameRef,"2",sep="")))]){
            for(fill_i in unique(ly$Fill)){
              a1<-shpa.x
              a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
                subset(select=c("STUSPS","scenario","Fill","NewValue"))
              if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
            }
          }
          head(dfx)
          
          colx<<-colorsX_Diff
          
          dfx@data<-dfx@data%>%mutate(scenario = recode_factor(scenario,"Historical Gas Price variation" = "Historical Gas\nPrice variation"))
          dfx@data$scenario <- factor( as.character(dfx@data$scenario), levels=c(scenName1,scenName2,"Historical Gas\nPrice variation") );
          
          
          map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                               val="NewValue",var1="scenario",var2="Fill")+
            tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                      title="Difference",panel.label.size = 1.5,panel.label.height = 2)+m2; map
          print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarDiff_NOTOTAL_",paramx,"_",unique(lx$x),sep=""),
                       figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
          
          colx<<-colorsX_Diff5
          map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                     val="NewValue",var1="scenario",var2="Fill")+
            tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                      title="Difference",panel.label.size = 1.5,panel.label.height = 2)+m2; map
          print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarDiffKMEANS_NOTOTAL_",paramx,"_",unique(lx$x),sep=""),
                       figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
          
          
        }
        
        #ly<-dcast(lx,region+scenario~Fill,value.var="NewValue",fun.aggregate=sum,na.rm=T)
        ly<-lx
        ly<-ly%>%dplyr::select(-contains("diff")); head(ly)
        ly<-ly%>%gather(key=scenario,value=NewValue,-region,-param,-x,-xLabel,-Aggregate,-NewUnits,-Fill,-FillLabel,-FillPalette);
        head(ly);
        ly$scenario<-ly$scenario%>%gsub("_prcnt","",.);head(ly)
        ly$scenario<-as.factor(ly$scenario)
        ly$scenario <- factor( as.character(ly$scenario), levels=c(scenariosComp,paste(scenNameRef,"1",sep=""),
                                                                   paste(scenNameRef,"2",sep="")) );
        ly<-ly[order(ly$scenario),];
        
        colnames(ly)[which(names(ly) == "region")] <- "STUSPS"; head(ly)
        if(nrow(ly)!=0){
          shpa.x<-shpUS48
          dfx<-NULL
          dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
            mutate(NewValue=0)%>%as.data.frame; head(dfa)
          dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
            mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
            dplyr::select(-c(NewValue.x,NewValue.y));
          
          for(scenario_i in unique(ly$scenario)[!(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                             paste(scenNameRef,"2",sep="")))]){
            for(fill_i in unique(ly$Fill)){
              a1<-shpa.x
              a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
                subset(select=c("STUSPS","scenario","Fill","NewValue"))
              if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
            }
          }
          head(dfx)
          
          #colx<<-colorsAbsolute #------Choose color palette
          #colx<<-(brewer.pal(9,"RdYlGn")) #------Choose color palette
          #pie(rep(1,length(colx)),label=names(colx),col=colx)
          colx<<-colorsX_Diff
          
          dfx@data<-dfx@data%>%mutate(scenario = recode_factor(scenario,"Historical Gas Price variation" = "Historical Gas\nPrice variation"))
          dfx@data$scenario <- factor( as.character(dfx@data$scenario), levels=c(scenName1,scenName2,"Historical Gas\nPrice variation") );
          
          map <- mapX_fill2VarKmeans(data=dfx,scaleData=data.frame(NewValue=seq(-100,100,by=5)),
                                     val="NewValue",var1="scenario",var2="Fill")+
            tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                      title="% Difference", panel.label.size = 1.5,panel.label.height = 2)+m2; map
          print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarPrcntKMEANS_NOTOTAL_",paramx,"_",unique(lx$x),sep=""),
                       figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
          
          colx<<-colorsX_Diff5
          map <- mapX_fill2Var(data=dfx,scaleData=data.frame(NewValue=seq(-100,100,by=5)),
                               val="NewValue",var1="scenario",var2="Fill")+
            tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                      title="% Difference", panel.label.size = 1.5,panel.label.height = 2)+m2; map
          print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarPrcnt_NOTOTAL_",paramx,"_",unique(lx$x),sep=""),
                       figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
          
          
        }
        
      }
    } # Close Params
    
    colx<<-colorsAbsolute #------Choose color palette
  } # Close if TRUE
  
  
   
   if(TRUE){
   #-------------------------------------------------------------------------
   # Figure 6a - Trade Maps : Electricity Production - Consumption
   # Maps (6x4) Rows - Scenarios (Gas Hi,lo,Spike, Dem Hi, Dem Lo)  Col - Coal, Gas, Renewable, Total
   #-------------------------------------------------------------------------
   
     #------------
     # IMPORTS, Exports, Consumption
     #---------------
   
   paramx<-"trade"
     
   ldf<-df_all%>%dplyr::filter(region %in% US48);head(l1)
   l1<-ldf%>%dplyr::select(param,region,NewValue,scenario,x,xLabel,Aggregate)
   
   # Reduce generation by ownuse coefficient
   ownuse<-read.csv(paste(dirname(wd0),"/gcam-core/input/gcam-data-system/gcam-usa-data/level2/L2232.TechCoef_elecownuse_FERC.csv",
                                    sep=""), stringsAsFactors = F, skip=4,header=T); head(ownuse)
   ownuseX<-ownuse%>%mutate(grid_region=region)%>%dplyr::select(grid_region,coefficient,year);head(ownuseX)
   lgridStateX<-lgridState%>%dplyr::select(state,grid_region);head(lgridStateX)
   ownuseX<-join(ownuseX,lgridStateX,by=c("grid_region"))%>%rename(region=state,x=year)%>%dplyr::select(-grid_region); head(ownuseX)
   
   l1a<-l1%>%filter(param=="elecProdByTechVint");head(l1a)
   l1a<-join(l1a,ownuseX,by=c("region","x"));head(l1a)
   l1a<-l1a%>%mutate(NewValue=NewValue/coefficient,
                      param="elecProdByTechVintLessOwnUse")%>%
                      dplyr::select(-coefficient);head(l1a)
   l1b<-l1%>%filter(param=="elecDelivB4Losses");head(l1b)
   
   l1c<-rbind.data.frame(l1a,l1b)
   
   if(nrow(l1c[l1c$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1c[l1c$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
   if(nrow(l1c[l1c$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1c[l1c$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
   l1c<-rbind.data.frame(xsum,xmean);head(l1c)
   
   ltrade1<-l1c%>%filter(param=="elecDelivB4Losses" | param=="elecProdByTechVintLessOwnUse")%>%
     spread(param,NewValue)%>%mutate(trade=elecProdByTechVintLessOwnUse-elecDelivB4Losses)%>%
     gather(key="Fill",value="NewValue",trade)
   
   tname<-paste("table_",paramx,"_",min(range(ltrade1$x)),"to",max(range(ltrade1$x)),sep="")
   write.csv(ltrade1%>%
               mutate(year=x,diff=NewValue)%>%
               dplyr::select(region,scenario,year,elecDelivB4Losses,elecProdByTechVintLessOwnUse,diff),paste(dir,"/",tname,".csv",sep=""),row.names=F)
   
   ltrade<-ltrade1%>%dplyr::select(-elecProdByTechVintLessOwnUse,-elecDelivB4Losses); head(ltrade)
   
   
   l1Imports<-ltrade%>%mutate(NewValue = case_when(NewValue>0~0,TRUE~NewValue*-1),Fill="Imports",
                              NewUnits="~Electricity~Trade~(TWh)", param="trade");head(l1Imports)
   
   l1Exports<-ltrade%>%mutate(NewValue = case_when(NewValue<0~0,TRUE~NewValue*1),Fill="Exports",
                              NewUnits="~Electricity~Trade~(TWh)", param="trade");head(l1Exports);
   
   la<-rbind.data.frame(l1Imports,l1Exports)  
   
   tname<-paste("table_ImportsExports_",min(range(la$x)),"to",max(range(la$x)),sep="")
   write.csv(la%>%
               mutate(year=x)%>%spread(Fill,NewValue)%>%
               dplyr::select(region,scenario,year,Exports,Imports),paste(dir,"/",tname,".csv",sep=""),row.names=F)
   
   la%>%filter(region=="CA" | region=="TX",scenario=="Reference")
   ltrade1%>%filter(region=="CA" | region=="TX",scenario=="Reference")
  
   
     
     lx<-la%>%filter(param==paramx,x==2030); head(lx)
     
     lx<-lx%>%spread(scenario,NewValue)%>%
       mutate(!!paste(scenName1,"_diff",sep=""):=get(scenName1)-get(scenNameRef))%>%
       mutate(!!paste(scenName2,"_diff",sep=""):=get(scenName2)-get(scenNameRef))%>%
       mutate(!!paste(scenName3,"_diff",sep=""):=get(scenName3)-get(scenNameRef))%>%
       mutate(!!paste(scenName1,"_prcnt",sep=""):=(get(scenName1)-get(scenNameRef))*100/get(scenNameRef))%>%
       mutate(!!paste(scenName2,"_prcnt",sep=""):=(get(scenName2)-get(scenNameRef))*100/get(scenNameRef))%>%
       mutate(!!paste(scenName3,"_prcnt",sep=""):=(get(scenName3)-get(scenNameRef))*100/get(scenNameRef))%>%
       mutate(!!paste(scenNameRef,"1",sep=""):=get(scenNameRef))%>%
       mutate(!!paste(scenNameRef,"2",sep=""):=get(scenNameRef))%>%
       dplyr::select(-c(scenName1,scenName2,scenName3))
     
     lx[mapply(is.infinite, lx)] <- 100
     lx[mapply(is.nan, lx)] <- NA
     
     head(lx)
     #----------
     # Map by SUM of subsectors
     #-----------
     
     if(nrow(lx)!=0){
       
       # All scenarios
       ly<-la%>%filter(param==paramx,x==2030); head(ly)
       colnames(ly)[which(names(ly) == "region")] <- "STUSPS"; head(ly)
       
       shpa.x<-shpUS48
       dfx<-NULL
       dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
         mutate(NewValue=0)%>%as.data.frame; head(dfa)
       dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
         mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
         dplyr::select(-c(NewValue.x,NewValue.y));
     
       
       for(scenario_i in unique(ly$scenario)[(unique(ly$scenario) %in% c(scenNameRef,scenName1,scenName2,scenName3))]){
         for(fill_i in unique(ly$Fill)){
           a1<-shpa.x
           a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
             subset(select=c("STUSPS","scenario","Fill","NewValue"))
           if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
         }
       }
       head(dfx)
       
       
       colx<<-colorsAbsolute #------Choose color palette
       
       dfx@data<-dfx@data%>%mutate(scenario = recode_factor(scenario,"Historical Gas Price variation" = "Historical Gas\nPrice variation"))
       dfx@data$scenario <- factor( as.character(dfx@data$scenario), levels=c(scenNameRef,scenName1,scenName2,"Historical Gas\nPrice variation") );
       
       map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                  val="NewValue",var1="scenario",var2="Fill")+
         tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                   panel.label.size = 1.5,panel.label.height = 2)+m2; map
       print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarAllScen_",paramx,"_",unique(lx$x),sep=""),
                    figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
       
       colx<<-colorsAbsolute5
       map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                            val="NewValue",var1="scenario",var2="Fill")+
         tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                   panel.label.size = 1.5,panel.label.height = 2)+m2; map
       print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarAllScenKMEANS_",paramx,"_",unique(lx$x),sep=""),
                    figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
       
       
       # Reference Scenarios
       #ly<-dcast(lx,region+scenario~Fill,value.var="NewValue",fun.aggregate=sum,na.rm=T)
       ly<-lx
       ly<-ly%>%dplyr::select(-contains("prcnt")); head(ly)
       ly<-ly%>%gather(key=scenario,value=NewValue,-region,-x,-xLabel,-Aggregate,-NewUnits,-Fill,-param);
       head(ly);
       ly$scenario<-ly$scenario%>%gsub("_diff","",.);head(ly)
       ly$scenario<-as.factor(ly$scenario)
       ly$scenario <- factor( as.character(ly$scenario), levels=c(scenariosComp,paste(scenNameRef,"1",sep=""),
                                                                  paste(scenNameRef,"2",sep="")) );
       ly<-ly[order(ly$scenario),];
       
       colnames(ly)[which(names(ly) == "region")] <- "STUSPS"; head(ly)
       
       
         shpa.x<-shpUS48
         dfx<-NULL
         dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
           mutate(NewValue=0)%>%as.data.frame; head(dfa)
         dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
           mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
           dplyr::select(-c(NewValue.x,NewValue.y));
         
         for(scenario_i in unique(ly$scenario)[(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                           paste(scenNameRef,"2",sep="")))]){
           for(fill_i in unique(ly$Fill)){
             a1<-shpa.x
             a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
               subset(select=c("STUSPS","scenario","Fill","NewValue"))
             if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
           }
         }
         head(dfx)
         
         
         colx<<-colorsAbsolute #------Choose color palette
         
         
         map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                    val="NewValue",var1="scenario",var2="Fill")+
           tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                     panel.label.size = 1.5,panel.label.height = 2)+m2; map
         print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarRef_",paramx,"_",unique(lx$x),sep=""),
                      figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
         colx<<-colorsAbsolute5
         map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                              val="NewValue",var1="scenario",var2="Fill")+
           tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                     panel.label.size = 1.5,panel.label.height = 2)+m2; map
         print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarRefKMEANS_",paramx,"_",unique(lx$x),sep=""),
                      figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
         
        
       
         
         # Differences

         dfx<-NULL
          
         for(scenario_i in unique(ly$scenario)[!(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                            paste(scenNameRef,"2",sep="")))]){
           for(fill_i in unique(ly$Fill)){
             a1<-shpa.x
             a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
               subset(select=c("STUSPS","scenario","Fill","NewValue"))
             if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
           }
         }
         head(dfx)
         
         #colx<<-colorsAbsolute #------Choose color palette
         #colx<<-(brewer.pal(9,"RdYlGn")) #------Choose color palette
         #pie(rep(1,length(colx)),label=names(colx),col=colx)
         colx<<-colorsX_Diff
         
         dfx@data<-dfx@data%>%mutate(scenario = recode_factor(scenario,"Historical Gas Price variation" = "Historical Gas\nPrice variation"))
         dfx@data$scenario <- factor( as.character(dfx@data$scenario), levels=c(scenName1,scenName2,"Historical Gas\nPrice variation") );
         
         
         map <- mapX_fill2Var(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                                    val="NewValue",var1="scenario",var2="Fill")+
           tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                     title="Difference",panel.label.size = 1.5,panel.label.height = 2)+m2; map
         print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarDiff_",paramx,"_",unique(lx$x),sep=""),
                      figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
         colx<<-colorsX_Diff5
         map <- mapX_fill2VarKmeans(data=dfx,scaleData=dfx@data%>%subset(select=c("NewValue")),
                              val="NewValue",var1="scenario",var2="Fill")+
           tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                     title="Difference",panel.label.size = 1.5,panel.label.height = 2)+m2; map
         print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarDiffKMEANS_",paramx,"_",unique(lx$x),sep=""),
                      figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
         
       
       #ly<-dcast(lx,region+scenario~Fill,value.var="NewValue",fun.aggregate=sum,na.rm=T)
       ly<-lx
       ly<-ly%>%dplyr::select(-contains("diff")); head(ly)
       ly<-ly%>%gather(key=scenario,value=NewValue,-region,-x,-xLabel,-Aggregate,-NewUnits,-Fill);
       head(ly);
       ly$scenario<-ly$scenario%>%gsub("_prcnt","",.);head(ly)
       ly$scenario<-as.factor(ly$scenario)
       ly$scenario <- factor( as.character(ly$scenario), levels=c(scenariosComp,paste(scenNameRef,"1",sep=""),
                                                                  paste(scenNameRef,"2",sep="")) );
       ly<-ly[order(ly$scenario),];
       ly<-ly%>%filter(!is.na(scenario))
       ly<-droplevels(ly)
       unique(ly$scenario)
       
       colnames(ly)[which(names(ly) == "region")] <- "STUSPS"; head(ly)
       
       if(nrow(ly)!=0){
         shpa.x<-shpUS48
         dfx<-NULL
         dfa<-expand.grid(STUSPS=unique(ly$STUSPS),Fill=unique(ly$Fill),scenario=unique(ly$scenario))%>%
           mutate(NewValue=0)%>%as.data.frame; head(dfa)
         dfb<-full_join(ly%>%subset(select=c("STUSPS","scenario","Fill","NewValue")),dfa,by=c("STUSPS","Fill","scenario"))%>%
           mutate(NewValue.x = as.numeric(as.character(NewValue.x)))%>%mutate(NewValue = case_when(is.na(NewValue.x)~NewValue.y,!is.na(NewValue.x)~NewValue.x))%>%
           dplyr::select(-c(NewValue.x,NewValue.y));
         dfb<-dfb[complete.cases(dfb),]
         
         for(scenario_i in unique(ly$scenario)[!(unique(ly$scenario) %in% c(scenNameRef,paste(scenNameRef,"1",sep=""),
                                                                            paste(scenNameRef,"2",sep="")))]){
           for(fill_i in unique(ly$Fill)){
             a1<-shpa.x
             a1@data<-join(shpa.x@data,dfb%>%filter(Fill==fill_i,scenario==scenario_i),by=c("STUSPS"))%>% 
               subset(select=c("STUSPS","scenario","Fill","NewValue"))
             if(is.null(dfx)){dfx<-a1}else{dfx<-rbind(dfx,a1,makeUniqueIDs = TRUE)}
           }
         }
         head(dfx)
         
         #colx<<-colorsAbsolute #------Choose color palette
         #colx<<-(brewer.pal(9,"RdYlGn")) #------Choose color palette
         #pie(rep(1,length(colx)),label=names(colx),col=colx)
         colx<<-colorsX_Diff
         
         dfx@data<-dfx@data%>%mutate(scenario = recode_factor(scenario,"Historical Gas Price variation" = "Historical Gas\nPrice variation"))
         dfx@data$scenario <- factor( as.character(dfx@data$scenario), levels=c(scenName1,scenName2,"Historical Gas\nPrice variation") );
         
         map <- mapX_fill2Var(data=dfx,scaleData=data.frame(NewValue=seq(-100,100,by=5)),
                              val="NewValue",var1="scenario",var2="Fill")+
           tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                     title="% Difference", panel.label.size = 1.5,panel.label.height = 2)+m2; map
         print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarPrcnt_",paramx,"_",unique(lx$x),sep=""),
                      figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
         colx<<-colorsX_Diff5
         map <- mapX_fill2VarKmeans(data=dfx,scaleData=data.frame(NewValue=seq(-100,100,by=5)),
                              val="NewValue",var1="scenario",var2="Fill")+
           tm_layout(main.title=capitalize(paste(gsub("~"," ",unique(lx$NewUnits))," ",unique(lx$x),sep="")),
                     title="% Difference", panel.label.size = 1.5,panel.label.height = 2)+m2; map
         print_PDFPNG(map,dir=dir,filename=paste("FinalFigMap2VarPrcntKMEANS_",paramx,"_",unique(lx$x),sep=""),
                      figWidth_Inch=mapWidthInch*1.2,figHeight_Inch=mapHeightInch*0.7,pdfpng=pdfpng)
         
       }
       
     } 
     
   } # If TRUE
     
   
   colx<<-colorsAbsolute #------Choose color palette
   
    #---------------------------------------------
   # Figure S1 - Supplementary Material
   # 1 x 5 - Cap Factor by technology for each scenario by year
   #----------------------------------------------
   
   paramsTemp<-c("elecProdByTechVint","elecCumCap","elecCapFacByTech","elecCapByTech","finalNrgbySecbyFuel")
   
  
 for(paramx in paramsTemp){
     
     l1<-df_all
     # Choose Fill1, FillLabel1, FillPalette1 for technologies or 2 for subsector
     l1$Fill<-l1$Fill2;
     l1$FillLabel<-l1$FillLabel2 
     l1$FillPalette<-l1$FillPalette2
     l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                            Fill,FillLabel,FillPalette)  # For National, by techs
     l1<-l1%>%filter(param==paramx); head(l1)
     
    
     if(nrow(l1)!=0){
       
       if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
       if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
       l1<-rbind.data.frame(xsum,xmean);head(l1)
       l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
       
       
       if(paramx=="finalNrgbySecbyFuel"){ l1$Fill <- factor(as.character(l1$Fill), levels=(rev(levels(l1$Fill))))}
       p <- fig_Bar(l1)
       p <- p+ facet_grid(~scenario)
       plot(p)
       fname<-paste("FinalFigSuplBar_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*2,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       p <- fig_LineMultiple(l1) 
       p<-p+ facet_grid(~scenario)
       plot(p)
       fname<-paste("FinalFigSuplLines_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*2,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       
     } # Close if empty rows 
     
   } # Close Params
   
   # New Coal and Nuclear
   
   paramsTemp<-c("elecCapByTech")
   
   
   for(paramx in paramsTemp){
     
     l1<-df_all
     # Choose Fill1, FillLabel1, FillPalette2 for technologies or 2 for subsector
     l1$Fill<-l1$Fill1;
     l1$FillLabel<-l1$FillLabel1 
     l1$FillPalette<-l1$FillPalette1
     l1<-l1%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,
                            Fill,FillLabel,FillPalette)  # For National, by techs
     l1<-l1%>%filter(param==paramx & (Fill=="coal (conv pul)" | Fill=="Gen_II_LWR" |
                                        Fill=="Gen_III" | Fill=="coal (conv pul CCS)" | Fill=="coal (IGCC CCS)")); head(l1)
     
     if(nrow(l1)!=0){
       
       if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
       if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
       l1<-rbind.data.frame(xsum,xmean);head(l1)
       l1$Fill<-as.factor(l1$Fill);l1<-droplevels(l1);head(l1)
       l1<-l1[l1$NewValue>0.1,]
       l1<-droplevels(l1)
       
       p <- fig_Bar(l1)
       p <- p+ facet_grid(~scenario)
       plot(p)
       fname<-paste("FinalFigSuplBarCoalNuc_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*2,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       p <- fig_LineMultiple(l1) 
       p<-p+ facet_grid(~scenario)
       plot(p)
       fname<-paste("FinalFigSuplLinesCoalNuc_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*2,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       
     } # Close if empty rows 
     
   } # Close Params
   
   
   
   # Gas CC capacity factors, elec gen, cap across certain regions
  
   paramsTemp<-c("elecProdByTechVint","elecCumCap","elecCapFacByTech")
   
   for(paramx in paramsTemp){
     
     l1<-df_all%>%dplyr::select(region,param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill1);head(l1)  # For National, by techs
     l1<-l1%>%filter(Fill1=="gas (CC)" & (region=="TX" | region=="CA")); head(l1)
     l1<-l1%>%filter(param==paramx)
   
   if(nrow(l1)!=0){
     
     if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
     if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
     l1<-rbind.data.frame(xsum,xmean);head(l1)
     l1<-droplevels(l1)
     
     p <- fig_LineCompareScenario(l1) + if(titleOn==1){ggtitle (paste(unique(l1$Title)," ","National US ",NumStates," States"," ",scenario_i,sep=""))}else{ggtitle(NULL)} 
     p <- p + facet_wrap(~region,ncol=2) + ggtitle("Gas (CC)")
     plot(p)
     
     fname<-paste("FinalFigSupLinesGasCCReg_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
     print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1.3,figHeight_InchMaster*1,pdfpng=pdfpng)
     
     tname<-paste("tableGasCCReg_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
     write.csv(l1,paste(dir,"/",tname,".csv",sep=""),row.names=F)
     
   } # Close if empty rows 
   } # Close for params
   
   # Gas CC capacity factors, elec gen, cap across certain regions
   
   paramsTemp<-c("elecProdByTechVint","elecCumCap","elecCapFacByTech")
   
   for(paramx in paramsTemp){
     
     l1<-df_all%>%dplyr::select(param,NewValue,scenario,x,xLabel,Aggregate,NewUnits,Fill1);head(l1)  # For National, by techs
     l1<-l1%>%filter(Fill1=="gas (CC)"); head(l1)
     l1<-l1%>%filter(param==paramx)
     
     if(nrow(l1)!=0){
       
       if(nrow(l1[l1$Aggregate=="sum",])>0){xsum<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="sum",]), sum, na.rm=TRUE)}else{xsum<-data.frame()}
       if(nrow(l1[l1$Aggregate=="mean",])>0){xmean<-aggregate(NewValue~., data=subset(l1[l1$Aggregate=="mean",]), mean, na.rm=TRUE)}else{xmean<-data.frame()}
       l1<-rbind.data.frame(xsum,xmean);head(l1)
       l1<-droplevels(l1)
       
       p <- fig_LineCompareScenario(l1) + if(titleOn==1){ggtitle (paste(unique(l1$Title)," ","National US ",NumStates," States"," ",scenario_i,sep=""))}else{ggtitle(NULL)} 
       p <- p  + ggtitle("Gas (CC) National")
       plot(p)
       
       fname<-paste("FinalFigSupLinesGasCCNat_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       print_PDFPNG(p,dir=dir,filename=fname,figWidth_InchMaster*1,figHeight_InchMaster*1,pdfpng=pdfpng)
       
       tname<-paste("tableGasCCNat_",paramx,"_",min(range(l1$x)),"to",max(range(l1$x)),sep="")
       write.csv(l1,paste(dir,"/",tname,".csv",sep=""),row.names=F)
       
     } # Close if empty rows 
   } # Close for params
   