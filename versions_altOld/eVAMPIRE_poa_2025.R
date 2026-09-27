##   [x].r  - VAMPIRE index test bed and generator -- EV + solar only on PostCodes
##
##   Created:        24 May 2026
##   Updated:        02 August 2026
##   Updated:        16 September 2026, altered for 2025
##
##   Notes:
##   1. Moved to Post codes

##   Sources: 
##   [1] Postcodes and Postal Areas, ABS - https://www.abs.gov.au/websitedbs/censushome.nsf/home/factsheetspoa?opendocument&navpos=450
##   [2] 2021 General Community Profile for Postal Areas (POA), South Australia, Census Data Packs, ABS - https://www.abs.gov.au/census/find-census-data/datapacks/download/2021_GCP_POA_for_SA_short-header.zip
##


####-----0.ADMIN - Include libraries, global variables, print/export settings-----------

#setwd( [insert full path]"/VAMPIREcode" )

###--A. Clean
rm(list = setdiff ( ls() , "") )
plot(1.1)
hide<-dev.off()
rm(hide)

###--B.Load Library for country manipulations
suppressMessages ( library ( "reshape2"     , lib.loc=.libPaths() ) )      # melt, dcast
suppressMessages ( library ( "ggplot2"      , lib.loc=.libPaths() ) )      # geom_col
suppressMessages ( library ( "openxlsx"     , lib.loc=.libPaths() ) )      # worksheet functions
suppressMessages ( library ( "ggmap"        , lib.loc=.libPaths() ) )      # mapping functions, ggmap()

###--C.Dirs and Vars

##global
year   <- c ( 2024 )  # ( 2016 , 2021 , 2026 )
Din    <- "reupdatedevsolarvamprieonpoa"

##vampire settings
vRange <- c ( 0.1 , 0.25 , 0.50 , 0.75 , 0.9  )

##output settings
label  <- c ( "updated2026" , "e_postcodeJan2025" )
Dout    <- c ( "final" )
wdth   <- 1920
hgth   <- 1080

#suppressMessages ( library ( "ozgs"         , lib.loc=.libPaths() ) )      # map_data()
#adelaide_gccs      <- gccsa("Greater Adelaide", edition = 3)
#post               <- poa ( edition = 3, filter_geom = adelaide_gccs$geometry  )   
#post$POA           <- as.double( post$poa_code_2021 )
#adeList            <- c ( post$POA , 5950 )
#post               <- poa ( edition = 3 )
#post$POA           <- as.double( post$poa_code_2021 )
#post               <- subset ( post , post$POA %in% adeList )
#library(sf)
#post$geo <- st_as_sf(post$geometry)
#save ( post , file = "Data/reupdatedevsolarvamprieonpoa/Adelaide_post_mapserver.rData")
#rm ( adelaide_gccs , adeList )
load ( paste0 ( "data/", Din , "/Adelaide_post_mapserver.rData" ) )
library(sf)
post$geo <- st_as_sf(post$geometry)

####-----END 0.ADMIN---------------

####-----VAMPRIE LOOP--------------------

##load and process abs data
i <- 1  ##debug/manual
for ( i in 1:length ( year ) ) {
  
  ###EVs -- following "Notes for EV calculations.docx"
  VA              <- read.xlsx ( paste0 ( "Data/", Din , "/EV_Index_Registration_Data_2021-2025.xlsx" ) , sheet = 2 ,  startRow = 1 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- subset ( VA , VA$State %in% c ( "SA" , "OTHER" ) )
  names(VA)[1]     <- "POA"
  VA              <- subset ( VA , !( grepl ( "Cells|Total" , VA$POA ) | is.na ( VA$POA ) ) )
  VAM             <- melt ( VA , id.vars = c ( "State" , "Fuel.Type" , "POA" )  )
  VAM$variable    <- as.character ( VAM$variable )
  VAM$variable    <- substr ( VAM$variable , nchar ( VAM$variable )  - 3 , nchar ( VAM$variable ) )
  VAM$variable    <- as.numeric ( VAM$variable ) - 1
  rm ( VA )
  
  VAMP            <- merge ( VAM , post , by = "POA" , all.y = TRUE )
  
  VAMPI           <- subset ( VAMP , VAMP$variable == year[i] )
  VAMPIh          <- dcast ( VAMPI , POA ~ Fuel.Type , value.var = "value" , fun.aggregate = sum   )
  VAMPIh$BEVshare <- round ( VAMPIh$BEV / ( VAMPIh$ICE + VAMPIh$`Hybrid/PHEV` + VAMPIh$BEV ) , 3 )
  maxEV           <- max ( VAMPIh$BEVshare )
  VAMPIh$BEVs     <- ifelse ( VAMPIh$BEVshare < maxEV * vRange[1] , 5 , 
                               ifelse ( ( VAMPIh$BEVshare < maxEV * vRange[2] & VAMPIh$BEVshare >= maxEV * vRange[1] ) , 4 ,
                                        ifelse ( ( VAMPIh$BEVshare < maxEV * vRange[3] & VAMPIh$BEVshare >= maxEV * vRange[2] ) , 3 ,
                                                 ifelse ( ( VAMPIh$BEVshare < maxEV * vRange[4] & VAMPIh$BEVshare >= maxEV * vRange[3] ) , 2 ,
                                                          ifelse ( ( VAMPIh$BEVshare >= maxEV * vRange[5] ) , 0 , 1 ) ) ) ) )
  
  ###
  write.csv ( VAMPIh , file = paste0 ( "results/" , Dout  , "/d_eVAMPIRE_EVs_" , year[i] , "_" , label[2] , ".csv" ) , row.names = FALSE )
  
  adeMAP  <- merge ( post , VAMPIh , by = "POA" , all.x = FALSE  )
  plot <- ggplot( adeMAP ) +
    geom_sf ( aes ( fill = BEVs ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 5 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( "0" , format ( round (x , 0 ) , nsmall = 0 ) ) ) +
    ggtitle( bquote ( "eVAMPIRE " ~ .( year[i] ) ~ " - BEVs        " ) ) +
    theme_void ( )
  ggsave ( filename = paste0 ( "results/" , Dout  , "/p_eVAMPIRE_EVs_" , year[i]  , "_" , label[2] ,  ".png" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##add income to VAMPIRE Matrix, Add up Vampire components, make a map
  VAMPe1 <- VAMPIh
  ALL    <- VAMPe1
  
  rm ( adeMAP , VAMP, VAMPI , VAMPIh , VAM  )
  
  
  
  ###Solar Panels -- following "Notes for Solar Installs calculations.docx"
  perCol            <- 171   ##134 is to Dec 2021 ; 171 is for Jan 2025
  SOLa              <- read.csv ( paste0 ( "Data/", Din , "/sgu-solar-installations-2011-to-present-and-totals_clean.csv" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
  SOLa              <- SOLa[,c ( 1:perCol)]  
  SOLa$Historic_total_InstQty_2001to2011 <- as.numeric ( gsub ("," , "" , SOLa$Historic_total_InstQty_2001to2011 ) )
  SOLa$TotIper      <- rowSums ( SOLa[, 2:perCol], na.rm = TRUE)
  SOLb              <- SOLa[,c ( 1,( perCol+1 ) )]
  names ( SOLb )[1] <- "POA"
  SOLa              <- read.csv ( paste0 ( "Data/", Din , "/sgu-solar-capacity-2011-to-present-and-totals.csv" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
  SOLa              <- SOLa[,c ( 1:perCol)]
  SOLa[,2:perCol]   <- lapply ( SOLa[,2:perCol] , function (x) as.numeric( gsub ("," , "" ,  x ) ) )
  SOLa$TotCper      <- rowSums ( SOLa[, 2:perCol], na.rm = TRUE)
  SOLc              <- SOLa[,c ( 1,( perCol+1 ) )]
  names ( SOLc )[1] <- "POA"
  SOL               <- merge ( SOLb , SOLc , by = "POA" )
  SOL$CperI         <- SOL$TotCper / SOL$TotIper
  
  SOLh              <- read.csv ( paste0 ( "Data/", Din , "/2021Census_G37_SA_POA.csv" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
  SOLh$hh           <- SOLh$Total_Total - SOLh$Total_DS_Flat_apart
  SOLh$POA          <- as.numeric( gsub ( "POA" , "" , SOLh$POA_CODE_2021 ) )
  SOLh              <- subset ( SOLh , select = c ( "POA" , "hh" ) )
  
  ###STILL issue with allocation of SA1 share to postcode... check sig digits
  VAMP             <- merge ( SOL , SOLh , by = "POA" , all.y = TRUE )
  rm ( SOLa , SOLb , SOLc , SOLh , SOL )
  
  ###Consider installations vs # HH in each SA1
  VAMP$SOLpct      <- ifelse ( VAMP$hh == 0 , 0 , VAMP$TotIper / VAMP$hh )
  VAMP$SOLpct      <- ifelse ( VAMP$SOLpct > 1 , 1 , VAMP$SOLpct )
  ###generate VAmpire component using data range
  VAMP$SOL        <- ifelse ( VAMP$SOLpct < vRange[1] , 5 , 
                              ifelse ( ( VAMP$SOLpct < vRange[2] & VAMP$SOLpct >= vRange[1] ) , 4 ,
                                       ifelse ( ( VAMP$SOLpct < vRange[3] & VAMP$SOLpct >= vRange[2] ) , 3 ,
                                                ifelse ( ( VAMP$SOLpct < vRange[4] & VAMP$SOLpct >= vRange[3] ) , 2 ,
                                                         ifelse ( ( VAMP$SOLpct >= vRange[5] ) , 0 , 1 ) ) ) ) )
  
  VAMP$SOL <- ifelse ( VAMP$hh == 0 , NA , VAMP$SOL )
  
  ###
  write.csv ( VAMP , file = paste0 ( "results/" , Dout  , "/d_eVAMPIRE_PVs_" , year[i] , "_" , label[2] , ".csv" ) , row.names = FALSE )
  
  adeMAP  <- merge ( post , VAMP , by = "POA" , all.x = FALSE  )
  plot <- ggplot( adeMAP ) +
    geom_sf( aes ( fill = SOL ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 5 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( "0" , format ( round (x , 0 ) , nsmall = 0 ) ) ) +
    ggtitle( bquote ( "eVAMPIRE " ~ .( year[i] ) ~ " - PV Install %" ) ) +
    theme_void ( )
  ggsave ( filename = paste0 ( "results/" , Dout  , "/p_eVAMPIRE_PVs_" , year[i]  , "_" , label[2] ,  ".png" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##add income to VAMPIRE Matrix, Add up Vampire components, make a map
  VAMPe2 <- VAMP
  ALL   <- merge ( ALL , VAMPe2 , by = "POA" )
  
  ALL$eVAM <- ALL$BEVs + ALL$SOL 
  write.csv ( ALL , file = paste0 ( "results/" , Dout  , "/d_eVAMPIRE_ALLcomponents_plusEVPV" , year[i] , "_" , label[2] , ".csv" ) , row.names = FALSE )
  
  adeMAP  <- merge ( post , ALL , by = "POA" , all.x = FALSE  )
  plot <- ggplot( adeMAP ) +
    geom_sf( aes ( fill = eVAM ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 10 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( format ( round (x , 2 ) , nsmall = 1 ) ) ) +
    ggtitle( bquote ( "eVAMPIRE " ~ .( year[i] ) ) ) + #~ " - ( BEV + PV )" ) ) +
    theme_void ( )
  ggsave ( filename = paste0 ( "results/" , Dout  , "/p_eVAMPIRE_Composite_updateEVPV" , year[i]  , "_" , label[2] ,  ".png" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  rm ( adeMAP , VAMP )
}

####-----END VAMPIRE LOOP--------------------


