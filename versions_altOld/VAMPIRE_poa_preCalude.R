##   [x].r  - VAMPIRE index test bed and generator
##
##   Created:        24 May 2026
##   Updated:        7 June 2026
##   Updated:        16 August 2026 -- moved to POAs
##
##   Notes:
##   1. Following "Vampire Methodology.docx" (Sipe, 2026) with a few changes, see relevant pptx
##   2. Changed names of provided POA data to "ABS_" + name of file with spaces = "_" + year of data

##   Issues:
##   1. Warnings from variable duplication when matching abs data - NO IMPACT - can modify all headers for abs data before merging, not completed 

##   Sources: 
##   [1] Postcodes and Postal Areas, ABS - https://www.abs.gov.au/websitedbs/censushome.nsf/home/factsheetspoa?opendocument&navpos=450
##   [2] ABS. 2021. “Number of Motor Vehicles (Ranges) (VEHRD).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/transport/number-motor-vehicles-ranges-vehrd.
##   [3] ABS. 2021. “Method of Travel to Work (6 Travel Modes) (MTW06P).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/transport/method-travel-work-6-travel-modes-mtw06p.
##   [4] ABS. 2021. “Tenure Type (TEND).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/housing/tenure-type-tend.
##   [5] ABS. 2021. “Total Household Income as Stated (Weekly) (HINASD).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/income-and-work/total-household-income-stated-weekly-hinasd.

####-----0.ADMIN - Include libraries, global variables, print/export settings-----------

#setwd( [insert full path]"/VAMPIREcode" )

###--A. Clean
rm(list = setdiff ( ls() , "") )
plot(1.1)
hide<-dev.off()
rm(hide)

###--B.Load Library for country manipulations
suppressPackageStartupMessages({
  library(openxlsx)  # read.xlsx()
  library(ggplot2)   # geom_sf(), ggsave()
  library(sf)        # st_as_sf()
})
#suppressMessages ( library ( "reshape2"     , lib.loc=.libPaths() ) )      # melt, dcast
#suppressMessages ( library ( "ggplot2"      , lib.loc=.libPaths() ) )      # geom_col
#suppressMessages ( library ( "openxlsx"     , lib.loc=.libPaths() ) )      # worksheet functions
#suppressMessages ( library ( "ggmap"        , lib.loc=.libPaths() ) )      # mapping functions, ggmap()

###--C.Dirs and Vars

##global
year   <- c ( 2021 )  # ( 2016 , 2021 , 2026 )
Din    <- "poa2021"

##vampire settings
vRange <- c ( 0.1 , 0.25 , 0.50 , 0.75 , 0.9  )

##output settings
label  <- c ( "up2026" )
Dout    <- c ( "poa2021out" )
wdth   <- 1920
hgth   <- 1080

###POA plot settings
#adelaide_gccs      <- gccsa("Greater Adelaide", edition = 3)
#post               <- poa ( edition = 3, filter_geom = adelaide_gccs$geometry  )   
#post$POA           <- as.double( post$poa_code_2021 )
#adeList            <- c ( post$POA , 5950 )
#post               <- poa ( edition = 3 )
#post$POA           <- as.double( post$poa_code_2021 )
#post               <- subset ( post , post$POA %in% adeList )
#library(sf)
#post$geo <- st_as_sf(post$geometry)
#save ( post , file = paste0 ( "data/", Din , "/Adelaide_post_mapserver.rData" ) )
#rm ( adelaide_gccs , adeList )
load ( paste0 ( "data/", Din , "/Adelaide_post_mapserver.rData" ) )
post$geo <- st_as_sf(post$geometry)

## ---- 1. Helper functions ---------------------------------------------------

#' Load and clean a raw ABS census table.
#'
#' Applies the cleaning steps common to every ABS extract used here: strip
#' the header/footer rows, rename the first column to POA, drop summary
#' rows, and coerce POA to numeric.
clean_abs_table <- function(file_path, sheet = 1, start_row = 9) {
  df <- openxlsx::read.xlsx(
    file_path,
    sheet = sheet,
    startRow = start_row,
    colNames = TRUE,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE
  )
  
  df <- df[-1, -1]
  names(df)[1] <- "POA"
  df <- subset(df, !(grepl("Cells|Total", POA) | is.na(POA)))
  df$POA <- as.double(gsub(", SA", "", df$POA))
  df
}

####-----END 0.ADMIN---------------

####-----VAMPRIE LOOP--------------------

##load and process abs data
i <- 1  ##debug/manual
for ( i in 1:length ( year ) ) {
  
  ###MV data, load and clean
  VA              <- read.xlsx ( paste0 ( "data/", Din , "/ABS_Motor_vehicles_" , year[i] ,  ".xlsx" ) , sheet = 1 ,  startRow = 9 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- VA[-1,-1]
  names ( VA )[1] <- 'POA'
  VA              <- subset ( VA , !( grepl ( "Cells|Total" , VA$POA ) | is.na ( VA$POA ) ) )
  VA$POA          <- gsub ( ", SA" , "" , VA$POA )
  VA$POA          <- as.double( VA$POA )
  
  ###if year before latest geographic definitions, take appropriate steps
  ##REMOVED for now
  ##if latest geographic definitions 
  if ( year [i] == 2021 ) { VAMP = VA }
  
  ###Calculate Vampire component
  VAMP$adjTot     <- rowSums ( VAMP[, c ( 2:8 )] , na.rm = TRUE )  ## note that total of categories does NOT equal the Total given!?
  VAMP$vamTot     <- VAMP$adjTot - VAMP$Not.stated - VAMP$Not.applicable # v2 removal - VAMP$No.motor.vehicles - VAMP$One.motor.vehicle  ## using sum of the categories rather than provided "Total"
  VAMP$vamPct     <- ifelse( VAMP$vamTot == 0 , 0 , ( VAMP$Two.motor.vehicles + VAMP$Three.motor.vehicles + VAMP$Four.or.more.motor.vehicles ) / VAMP$vamTot )

  ###generate VAmpire component using data range -- move to function
  VAMP$MV         <- ifelse ( VAMP$vamPct < vRange[1] , 0 , 
                             ifelse ( ( VAMP$vamPct < vRange[2] & VAMP$vamPct >= vRange[1] ) , 1 ,
                                      ifelse ( ( VAMP$vamPct < vRange[3] & VAMP$vamPct >= vRange[2] ) , 2 ,
                                               ifelse ( ( VAMP$vamPct < vRange[4] & VAMP$vamPct >= vRange[3] ) , 3 ,
                                                        ifelse ( ( VAMP$vamPct >= vRange[5] ) , 5 , 4 ) ) ) ) )
  VAMP$MV <- ifelse ( VAMP$vamTot == 0 , NA , VAMP$MV )
  
  ### Output intermediate data (and maps) for Q&A and analysis
  write.csv ( VAMP , file = paste0 ( "results/" , Dout  , "/d_VAMPIRE_motorVehicles_" , year[i] , "_" , label[1] , ".csv" ) , row.names = FALSE )
  
  adeMAP  <- merge ( post , VAMP , by = "POA" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = MV ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 5 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( "0" , format ( round (x , 2 ) , nsmall = 0 ) ) ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ) ) + #~ " - MotorVehicles" ) ) +
    theme_void ( )
  ggsave ( filename = paste0 ( "results/" , Dout  , "/p_VAMPIRE_motorVehicles_" , year[i]  , "_" , label[1] ,  ".png" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##save as VAMP component for later
  names ( VAMP )[9:12] <- lapply ( names ( VAMP )[9:12] , function ( x ) paste0 ( "mvs_" , x ) )
  VAMP1 <- VAMP
  rm ( VA , VAMP , adeMAP, plot )
  ##-------
  
  
  ###JOURNEY TO WORK
  VA              <- read.xlsx ( paste0 ( "data/", Din , "/ABS_Journey_to_Work_" , year[i] , ".xlsx" , sep = "" ) , sheet = 1 ,  startRow = 9 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- VA[-1,-1]
  names ( VA )[1] <- 'POA'
  VA              <- subset ( VA , !( grepl ( "Cells|Total" , VA$POA ) | is.na ( VA$POA ) ) )
  VA$POA          <- gsub ( ", SA" , "" , VA$POA )
  VA$POA          <- as.double( VA$POA ) 
  
  ###if year before latest geographic definitions, take appropriate steps
  ##REMOVED for now
  ##if latest geographic definitions 
  if ( year [i] == 2021 ) { VAMP = VA }

  ###Calculate Vampire component
  VAMP$adjTot     <- rowSums ( VAMP[, c ( 2:8 )] , na.rm = TRUE )  ## note that total of categories does NOT equal the Total given!?
  VAMP$vamTot     <- VAMP$adjTot - VAMP$Worked.at.home.or.did.not.go.to.work - VAMP$Not.stated - VAMP$Not.applicable
  VAMP$vamPct     <- ifelse( VAMP$vamTot == 0 , 0 , ( VAMP$Vehicle) / VAMP$vamTot )
  
  ###generate VAmpire component using data range
  VAMP$JTW        <- ifelse ( VAMP$vamPct < vRange[1] , 0 , 
                                ifelse ( ( VAMP$vamPct < vRange[2] & VAMP$vamPct >= vRange[1] ) , 1 ,
                                         ifelse ( ( VAMP$vamPct < vRange[3] & VAMP$vamPct >= vRange[2] ) , 2 ,
                                                  ifelse ( ( VAMP$vamPct < vRange[4] & VAMP$vamPct >= vRange[3] ) , 3 ,
                                                           ifelse ( ( VAMP$vamPct >= vRange[5] ) , 5 , 4 ) ) ) ) )
  VAMP$JTW <- ifelse ( VAMP$vamTot == 0 , NA , VAMP$JTW )
  
  ### Output intermediate data (and maps) for Q&A and analysis
  write.csv ( VAMP , file = paste0 ( "results/" , Dout  , "/d_VAMPIRE_journyToWork_" , year[i] , "_" , label[1] , ".csv" ) , row.names = FALSE )
  
  adeMAP  <- merge ( post , VAMP , by = "POA" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = JTW ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 5 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( "0" , format ( round (x , 2 ) , nsmall = 0 ) ) ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ) ) + #~ " - Journey to Work" ) ) +
    theme_void ( )
  ggsave ( filename = paste0 ( "results/" , Dout  , "/p_VAMPIRE_journeyToWork_" , year[i]  , "_" , label[1] ,  ".png" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##save as VAMP component for later
  names ( VAMP )[9:12] <- lapply ( names ( VAMP )[9:12] , function ( x ) paste0 ( "jtw_" , x ) )
  VAMP2 <- VAMP
  ALL   <- merge ( VAMP1 , VAMP2 , by = "POA" )
  rm ( VA , VAMP , adeMAP , plot )
  ##-------
  
  
  ###TENURE TYPE
  VA              <- read.xlsx ( paste0 ( "data/", Din , "/ABS_Tenure_" , year[i] , ".xlsx" , sep = "" ) , sheet = 1 ,  startRow = 9 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- VA[-1,-1]
  names ( VA )[1] <- 'POA'
  VA              <- subset ( VA , !( grepl ( "Cells|Total" , VA$POA ) | is.na ( VA$POA ) ) )
  VA$POA          <- gsub ( ", SA" , "" , VA$POA )
  VA$POA          <- as.double( VA$POA ) 
  
  ###if year before latest geographic definitions, take appropriate steps
  ##REMOVED for now
  ##if latest geographic definitions 
  if ( year [i] == 2021 ) { VAMP = VA }
    
  ###Calculate Vampire component
  VAMP$adjTot     <- rowSums ( VAMP[, c ( 2:10 )] , na.rm = TRUE )  ## note that total of categories does NOT equal the Total given!?
  VAMP$vamTot     <- VAMP$adjTot - VAMP$Not.stated - VAMP$Not.applicable
  VAMP$vamPct     <- ifelse( VAMP$vamTot == 0 , 0 , ( VAMP$Owned.with.a.mortgage ) / VAMP$vamTot )
  
  ###generate VAmpire component using data range
  VAMP$TEN        <- ifelse ( VAMP$vamPct < vRange[1] , 0 , 
                              ifelse ( ( VAMP$vamPct < vRange[2] & VAMP$vamPct >= vRange[1] ) , 1 ,
                                       ifelse ( ( VAMP$vamPct < vRange[3] & VAMP$vamPct >= vRange[2] ) , 2 ,
                                                ifelse ( ( VAMP$vamPct < vRange[4] & VAMP$vamPct >= vRange[3] ) , 3 ,
                                                         ifelse ( ( VAMP$vamPct >= vRange[5] ) , 5 , 4 ) ) ) ) )
  VAMP$TEN <- ifelse ( VAMP$vamTot == 0 , NA , VAMP$TEN )
  
  ### Output intermediate data (and maps) for Q&A and analysis
  write.csv ( VAMP , file = paste ( "results/" , Dout  , "/d_VAMPIRE_Tenure_" , year[i] , "_" , label[1] , ".csv" , sep = ""  ) , row.names = FALSE )
  
  adeMAP  <- merge ( post , VAMP , by = "POA" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = TEN ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 5 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( "0" , format ( round (x , 2 ) , nsmall = 0 ) ) ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ) ) + #~ " - Tenure" ) ) +
    theme_void ( )
  ggsave ( filename = paste ( "results/" , Dout  , "/p_VAMPIRE_Tenure_" , year[i]  , "_" , label[1] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##save as VAMP component for later
  names ( VAMP )[11:14] <- lapply ( names ( VAMP )[11:14] , function ( x ) paste0 ( "ten_" , x ) )
  VAMP3 <- VAMP
  ALL   <- merge ( ALL , VAMP3 , by = "POA" )
  rm ( VA , VAMP , adeMAP, plot )
  ##-------
  
  
  ###INCOME
  VA              <- read.xlsx ( paste0 ( "data/", Din , "/ABS_Income_" , year[i] , ".xlsx" , sep = "" ) , sheet = 1 ,  startRow = 9 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- VA[-1,-1]
  names ( VA )[1] <- 'POA'
  VA              <- subset ( VA , !( grepl ( "Cells|Total" , VA$POA ) | is.na ( VA$POA ) ) )
  VA$POA          <- gsub ( ", SA" , "" , VA$POA )
  VA$POA          <- as.double( VA$POA ) 
  
  ###if year before latest geographic definitions, take appropriate steps
  ##REMOVED for now
  ##if latest geographic definitions 
  if ( year [i] == 2021 ) { VAMP = VA }
  
  ###Calculate Vampire component
  VAMP$adjTot     <- rowSums ( VAMP[, c ( 2:25 )] , na.rm = TRUE )  ## note that total of categories does NOT equal the Total given!?
  VAMP$vamTot     <- VAMP$adjTot - VAMP$All.incomes.not.stated - VAMP$Not.applicable - VAMP$Negative.income - VAMP$Nil.income

  ##estimate weighted income allocation based on each census category
  inc             <- gsub ( "[A-Za-z]|\\(.*|\\.|\\$|\\," , "" , names ( VAMP )[4:23] )
  incStart        <- as.numeric ( gsub ( "\\-.*" , "" , inc  ) )
  incEnd          <- as.numeric ( gsub ( ".*\\-" , "" , inc  ) )
  incEnd[20]      <- 12000
  VAMP$aInc       <- ifelse ( VAMP$vamTot == 0 , 0 ,  
                            ( VAMP$`$1-$149.($1-$7,799)` * mean ( c (incStart[1],incEnd[1]) ) +
                             VAMP$`$150-$299.($7,800-$15,599)` * mean ( c (incStart[2],incEnd[2]) ) +
                             VAMP$`$300-$399.($15,600-$20,799)` * mean ( c (incStart[3],incEnd[3]) ) +
                             VAMP$`$400-$499.($20,800-$25,999)` * mean ( c (incStart[4],incEnd[4]) ) +
                             VAMP$`$500-$649.($26,000-$33,799)` * mean ( c (incStart[5],incEnd[5]) ) +
                             VAMP$`$650-$799.($33,800-$41,599)` * mean ( c (incStart[6],incEnd[6]) ) +
                             VAMP$`$800-$999.($41,600-$51,999)` * mean ( c (incStart[7],incEnd[7]) ) +
                             VAMP$`$1,000-$1,249.($52,000-$64,999)` * mean ( c (incStart[8],incEnd[8]) ) +
                             VAMP$`$1,250-$1,499.($65,000-$77,999)` * mean ( c (incStart[9],incEnd[9]) ) +
                             VAMP$`$1,500-$1,749.($78,000-$90,999)` * mean ( c (incStart[10],incEnd[10]) ) +
                             VAMP$`$1,750-$1,999.($91,000-$103,999)` * mean ( c (incStart[11],incEnd[11]) ) +
                             VAMP$`$2,000-$2,499.($104,000-$129,999)` * mean ( c (incStart[12],incEnd[12]) ) +
                             VAMP$`$2,500-$2,999.($130,000-$155,999)` * mean ( c (incStart[13],incEnd[13]) ) +
                             VAMP$`$3,000-$3,499.($156,000-$181,999)` * mean ( c (incStart[14],incEnd[14]) ) +
                             VAMP$`$3,500-$3,999.($182,000-$207,999)` * mean ( c (incStart[15],incEnd[15]) ) +
                             VAMP$`$4,000-$4,499.($208,000-$233,999)` * mean ( c (incStart[16],incEnd[16]) ) +
                             VAMP$`$4,500-$4,999.($234,000-$259,999)` * mean ( c (incStart[17],incEnd[17]) ) +
                             VAMP$`$5,000-$5,999.($260,000-$311,999)` * mean ( c (incStart[18],incEnd[18]) ) +
                             VAMP$`$6,000-$7,999.($312,000-$415,999)` * mean ( c (incStart[19],incEnd[19]) ) +
                             VAMP$`$8,000.or.more.($416,000.or.more)` * mean ( c (incStart[20],incEnd[20]) ) )  / VAMP$vamTot )
  
  ###generate income quantiles using data range and record pertinant stats
  qMV              <- quantile ( VAMP$aInc , c ( 0 , 1 , vRange ) )  #get range limits for vampire based on range of data and ranges provided in instructions
  VAMP$INC         <- ifelse ( VAMP$aInc < qMV[[3]] , 5 , 
                                ifelse ( ( VAMP$aInc < qMV[[4]] & VAMP$aInc >= qMV[[3]] ) , 4 ,
                                         ifelse ( ( VAMP$aInc < qMV[[5]] & VAMP$aInc >= qMV[[4]] ) , 3 ,
                                                  ifelse ( ( VAMP$aInc < qMV[[6]] & VAMP$aInc >= qMV[[5]] ) , 2 ,
                                                           ifelse ( ( VAMP$aInc >= qMV[[7]] ) , 0 , 1 ) ) ) ) )
  VAMP$INCqNam <- ifelse ( VAMP$aInc < qMV[[3]] , paste ( names ( qMV[1]) , " >= x >" , names ( qMV[3]) ) , 
                             ifelse ( ( VAMP$aInc < qMV[[4]] & VAMP$aInc >= qMV[[3]] ) , paste ( names ( qMV[3]) , " >= x > " , names ( qMV[4]) ) ,
                                      ifelse ( ( VAMP$aInc < qMV[[5]] & VAMP$aInc >= qMV[[4]] ) , paste ( names ( qMV[4]) , " >= x > " , names ( qMV[5]) )  ,
                                               ifelse ( ( VAMP$aInc < qMV[[6]] & VAMP$aInc >= qMV[[5]] ) , paste ( names ( qMV[5]) , " >= x > " , names ( qMV[6]) ) ,
                                                        ifelse ( ( VAMP$aInc >= qMV[[7]] ) , paste ( names ( qMV[7]) , " >= x > " , names ( qMV[2]) ) , paste ( names ( qMV[6]) , " >= x > " , names ( qMV[7]) ) ) ) ) ) )
  VAMP$INCqBot  <- ifelse ( VAMP$aInc < qMV[[3]] , qMV[[1]] , 
                              ifelse ( ( VAMP$aInc < qMV[[4]] & VAMP$aInc >= qMV[[3]] ) , qMV[[3]] ,
                                       ifelse ( ( VAMP$aInc < qMV[[5]] & VAMP$aInc >= qMV[[4]] ) , qMV[[4]] ,
                                                ifelse ( ( VAMP$aInc < qMV[[6]] & VAMP$aInc >= qMV[[5]] ) , qMV[[5]] ,
                                                         ifelse ( ( VAMP$aInc >= qMV[[7]] ) , qMV[[7]] , qMV[[6]] ) ) ) ) )
  VAMP$INCqTop  <- ifelse ( VAMP$aInc < qMV[[3]] , qMV[[3]] , 
                              ifelse ( ( VAMP$aInc < qMV[[4]] & VAMP$aInc >= qMV[[3]] ) , qMV[[4]] ,
                                       ifelse ( ( VAMP$aInc < qMV[[5]] & VAMP$aInc >= qMV[[4]] ) , qMV[[5]] ,
                                                ifelse ( ( VAMP$aInc < qMV[[6]] & VAMP$aInc >= qMV[[5]] ) , qMV[[6]] ,
                                                         ifelse ( ( VAMP$aInc >= qMV[[7]] ) , qMV[[2]] , qMV[[7]] ) ) ) ) )
  
  VAMP$INC <- ifelse ( VAMP$vamTot == 0 , NA , VAMP$INC )
  
  ### Output intermediate data (and maps) for Q&A and analysis
  write.csv ( VAMP , file = paste0 ( "results/" , Dout  , "/d_VAMPIRE_Income_" , year[i] , "_" , label[1] , ".csv" ) , row.names = FALSE )
  
  adeMAP  <- merge ( post , VAMP , by = "POA" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = INC ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 5 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( "0" , format ( round (x , 2 ) , nsmall = 0 ) ) ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ) ) + #~ " - Income" ) ) +
    theme_void ( )
  ggsave ( filename = paste ( "results/" , Dout  , "/p_VAMPIRE_Income_" , year[i]  , "_" , label[1] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##add income to VAMPIRE Matrix, Add up Vampire components, make a map
  names ( VAMP )[26:28] <- lapply ( names ( VAMP )[26:28] , function ( x ) paste0 ( "inc_" , x ) )
  VAMP4 <- VAMP
  ALL   <- merge ( ALL , VAMP4 , by = "POA" )
  rm ( VA , VAMP , adeMAP, plot )
  ##-------
  
  ALL$VAM <- ALL$MV + ALL$JTW + ALL$TEN + ALL$INC
  write.csv ( ALL , file = paste ( "results/" , Dout  , "/d_VAMPIRE_ALLcomponents_" , year[i] , "_" , label[1] , ".csv" , sep = ""  ) , row.names = FALSE )
  
  adeMAP  <- merge ( post , ALL , by = "POA" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = VAM ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 20 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( format ( round (x , 2 ) , nsmall = 0 ) ) ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ) ) + #~ " - Composite" ) ) +
    theme_void ( )
  ggsave ( filename = paste ( "results/" , Dout  , "/p_VAMPIRE_Composite_" , year[i]  , "_" , label[1] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  rm ( adeMAP , plot , qMV , incStart , incEnd , inc  )

}

####-----END VAMPIRE LOOP--------------------


