##   [x].r  - VAMPIRE index test bed and generator
##
##   Created:        24 May 2026
##   Updated:        7 June 2026
##
##   Notes:
##   1. Changed name of 2021 file from "2021 Motor Vehicle Adelaide SA1.xlsx" to "2021 Motor Vehicles Adelaide SA1.xlsx" to allow recursive code to work
##   2. Data for 2016 gives 7 digit SA1 codes... can it be downloaded in 11 digit format, or just from 2021 and on??
##   3. Changed name of 2016 file from "2016 JTW Adelaide SA1.xlsx" to "2016 JTWork Adelaide SA1.xlsx" to allow recursive code to work
##   4. JTW data from 2016 is missing transport categories rendering the JTW vampire component either 1 or 0
##   5. Manual merge of duplicate SA1s in moving from 2016 to 2021 SA1a --assumption here to be checked is that the duplicated rows are not an error and are the results of two 2016 SA1s being merged in 2021
##   6. Remove negative and nill income categories from divisor
##   7. Income to use for highest income bracket in weighted average ($8000 and above)??
##   8. How to assign income vampires? Using 4000 per week at max for SA1 atm.
##   9. Changed name of 2021 income file from "2021 HH Weekly Income Adelaide SA1.xlsx" to "2021 HH Income Adelaide SA1.xlsx" to allow recursive code to work

##   Sources: 
##   [1] Can't seem to find 2016 data on ABS webpage!!??
##   [2] ABS. 2021. “Number of Motor Vehicles (Ranges) (VEHRD).” Australian Bureau of Statistics. https://www.abs.gov.au/census/guide-census-Data/census-dictionary/2021/variables-topic/transport/number-motor-vehicles-ranges-vehrd.
##   [3] ABS. 2021. “2016 Statistical Areas Level 1 to 2021 Statistical Areas Level 1.” July 20. https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/correspondences/CG_SA1_2016_SA1_2021.csv.
##   [4] ABS. 2016. “Statistical Area Level 1 – ASGS Ed. 2.” Australian Bureau of Statistics, July 12. https://digital.atlas.gov.au/maps/8592aa3554e64cafbb5b68e0a81d89ac/about.
##
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
suppressMessages ( library ( "ozgs"         , lib.loc=.libPaths() ) )      # map_data()

###--C.Dirs and Variables

##global variables
year   <- c ( 2016 , 2021 )  
type   <- 0   ## 0 = base/traditional   ,   1 = new/includes PV/EV
label  <- c ( "original" , "originalAbs" , "updated" , "extended" )

vRange <- c ( 0.1 , 0.25 , 0.50 , 0.75 , 0.9  ) 
#iRange <- c ( 400 , 1000 , 2000 , 3000 , 3600 )

#sa1merge <- c ( 40201102660 , 40202102854 , 40202103516 , 40304117702 , 40304117716 , 40401109132 )

##plot settings
## https://geo.abs.gov.au/arcgis/rest/services/ASGS2021/SA1/MapServer
#sa1               <- sa1 ( gccsa_name = "Greater Adelaide" , edition = 3 )     ##gccsa_name doesn't work as a limited... fix!
#ade               <- subset ( sa1 , sa1$gccsa_name_2021 == "Greater Adelaide" )
#ade$SA1           <- as.double( ade$sa1_code_2021 )
#save ( ade , file = "Data/Adelaide_sa1_mapserver.rData")
load ( "Data/Adelaide_sa1_mapserver.rData" )

wdth          <- 1920
hgth          <- 1080

####-----END 0.ADMIN---------------

####-----VAMPRIE LOOP--------------------

##load and process abs data
i <- 2  ##debug/manual
for ( i in 1:length ( year ) ) {
  
  ###MV data, load and clean -- following "Vampire Methodology.docx"
  VA              <- read.xlsx ( paste ( paste ( "Data/absDataTable" , year[i] , "/" , sep = "" ) , year[i] , " Motor Vehicles Adelaide SA1.xlsx" , sep = "" ) , sheet = 1 ,  startRow = 9 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- VA[-1,-1]
  names ( VA )[1] <- 'SA1'
  VA$SA1          <- as.double( VA$SA1 )
  VA              <- subset ( VA , !is.na(VA$SA1) )  
  
  ###if year before latest SA1 definitions, take appropriate steps
  ##for 2016
  if ( year[i] == 2016 ) {
    # Check if 7-digit and convert to 11-digit
    if ( nchar ( VA$SA1[1] ) < 11 ) { 
      GEO      <- read.csv ( paste ( "Data/" , "SA1.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
      GEO      <- GEO[,2:3]
      names ( GEO ) <- c ( "SA1_2016" , "SA1short" )
      names ( VA )[1] <- "SA1short"
      VAM  <- merge ( VA , GEO , by = "SA1short" , all.x = TRUE )
      VAM  <- VAM[,-1]
    }
    #Convert to 2021 SA1 geography using Ratio's provided by ABS for this purpose
    GEO                     <- read.csv ( paste ( "Data/" , "CG_SA1_2016_SA1_2021.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
    GEO                     <- GEO[,1:3]
    names ( GEO )           <- c ( "SA1_2016" , "SA1" , "trans" )
    VAMP                    <- merge ( GEO , VAM , by = "SA1_2016" , all.y = TRUE )
    VAMP[4:( ncol (VAMP))]  <- lapply ( VAMP[4:( ncol (VAMP))] , function (x) VAMP$trans * x )  
    VAMP                    <- VAMP[, c ( 2 , 4:( ncol (VAMP) ) ) ]
    ##merge duplicate rows (see Notes)
    VAMP                    <- aggregate( . ~ SA1 , data = VAMP , FUN = sum  )
    
    rm ( VAM , GEO )
  }
  if ( year [i] == 2021 ) { VAMP = VA }
  
  ###Calculate Vampire component
  VAMP$adjTot     <- rowSums ( VAMP[, c ( 2:8 )] , na.rm = TRUE )  ## note that total of categories does NOT equal the Total given!?
  VAMP$vamTot     <- VAMP$adjTot - VAMP$Not.stated - VAMP$Not.applicable # v2 removal - VAMP$No.motor.vehicles - VAMP$One.motor.vehicle  ## using sum of the categories rather than provided "Total"
  VAMP$vamPct     <- ifelse( VAMP$vamTot == 0 , 0 , ( VAMP$Two.motor.vehicles + VAMP$Three.motor.vehicles + VAMP$Four.or.more.motor.vehicles ) / VAMP$vamTot )

  ###generate VAmpire component using data range
  VAMP$MV         <- ifelse ( VAMP$vamPct < vRange[1] , 0 , 
                             ifelse ( ( VAMP$vamPct < vRange[2] & VAMP$vamPct >= vRange[1] ) , 1 ,
                                      ifelse ( ( VAMP$vamPct < vRange[3] & VAMP$vamPct >= vRange[2] ) , 2 ,
                                               ifelse ( ( VAMP$vamPct < vRange[4] & VAMP$vamPct >= vRange[3] ) , 3 ,
                                                        ifelse ( ( VAMP$vamPct >= vRange[5] ) , 5 , 4 ) ) ) ) )
  
  VAMP$MV <- ifelse ( VAMP$vamTot == 0 , NA , VAMP$MV )
  
  ### Output intermediate data (and maps) for Q&A and analysis
  write.csv ( VAMP , file = paste ( "results/" , "VAMPIRE_motorVehicles_" , year[i] , "_" , label[3] , ".csv" , sep = ""  ) , row.names = FALSE )
  
  adeMAP  <- merge ( ade , VAMP , by = "SA1" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = MV ) , size = 0.02  ) +
    scale_fill_gradient ( low = "green" , high = "red" ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ~ " - MotorVehicles" ) ) +
    theme_void ( )
  ggsave ( filename = paste ( "results/" , "VAMPIRE_motorVehicles_" , year[i]  , "_" , label[3] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##save as VAMP component for later
  VAMP1 <- VAMP
  
  
  ###JOURNEY TO WORK -- following "Vampire Methodology.docx"
  VA              <- read.xlsx ( paste ( paste ( "Data/absDataTable" , year[i] , "/" , sep = "" ) , year[i] , " JTW Adelaide SA1.xlsx" , sep = "" ) , sheet = 1 ,  startRow = 9 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- VA[-1,-1]
  names ( VA )[1] <- 'SA1'
  VA$SA1          <- as.double( VA$SA1 )
  VA              <- subset ( VA , !is.na(VA$SA1) )  
  
  ###if year before latest SA1 definitions, take appropriate steps
  ##for 2016
  if ( year[i] == 2016 ) {
    # Check if 7-digit and convert to 11-digit
    if ( nchar ( VA$SA1[1] ) < 11 ) { 
      GEO      <- read.csv ( paste ( "Data/" , "SA1.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
      GEO      <- GEO[,2:3]
      names ( GEO ) <- c ( "SA1_2016" , "SA1short" )
      names ( VA )[1] <- "SA1short"
      VAM  <- merge ( VA , GEO , by = "SA1short" , all.x = TRUE )
      VAM  <- VAM[,-1]
    }
    #Convert to 2021 SA1 geography using Ratio's provided by ABS for this purpose
    GEO                     <- read.csv ( paste ( "Data/" , "CG_SA1_2016_SA1_2021.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
    GEO                     <- GEO[,1:3]
    names ( GEO )           <- c ( "SA1_2016" , "SA1" , "trans" )
    VAMP                    <- merge ( GEO , VAM , by = "SA1_2016" , all.y = TRUE )
    VAMP[4:( ncol (VAMP))]  <- lapply ( VAMP[4:( ncol (VAMP))] , function (x) VAMP$trans * x )  
    VAMP                    <- VAMP[, c ( 2 , 4:( ncol (VAMP) ) ) ]
    ##merge duplicate rows (see Notes)
    VAMP                    <- aggregate( . ~ SA1 , data = VAMP , FUN = sum  )
    
    rm ( VAM , GEO )
    
    ###Calculate Vampire component
    VAMP$adjTot     <- rowSums ( VAMP[, c ( 2:8 )] , na.rm = TRUE )  ## note that total of categories does NOT equal the Total given!?
    VAMP$vamTot     <- VAMP$adjTot - VAMP$Worked.at.home.or.Did.not.go.to.work - VAMP$Mode.not.stated - VAMP$Not.applicable
    VAMP$vamPct     <- ifelse( VAMP$vamTot == 0 , 0 , ( VAMP$Vehicle) / VAMP$vamTot )
  }
  if ( year [i] == 2021 ) { 
    VAMP = VA 
    ###Calculate Vampire component
    VAMP$adjTot     <- rowSums ( VAMP[, c ( 2:9 )] , na.rm = TRUE )  ## note that total of categories does NOT equal the Total given!?
    VAMP$vamTot     <- VAMP$adjTot - VAMP$Worked.at.home.or.Did.not.go.to.work - VAMP$Mode.not.stated - VAMP$Not.applicable - VAMP$Overseas.visitor
    VAMP$vamPct     <- ifelse( VAMP$vamTot == 0 , 0 , ( VAMP$Vehicle) / VAMP$vamTot )
  }
  
  ###generate VAmpire component using data range
  VAMP$JTW        <- ifelse ( VAMP$vamPct < vRange[1] , 0 , 
                                ifelse ( ( VAMP$vamPct < vRange[2] & VAMP$vamPct >= vRange[1] ) , 1 ,
                                         ifelse ( ( VAMP$vamPct < vRange[3] & VAMP$vamPct >= vRange[2] ) , 2 ,
                                                  ifelse ( ( VAMP$vamPct < vRange[4] & VAMP$vamPct >= vRange[3] ) , 3 ,
                                                           ifelse ( ( VAMP$vamPct >= vRange[5] ) , 5 , 4 ) ) ) ) )
  
  VAMP$JTW <- ifelse ( VAMP$vamTot == 0 , NA , VAMP$JTW )
  
  ### Output intermediate data (and maps) for Q&A and analysis
  write.csv ( VAMP , file = paste ( "results/" , "VAMPIRE_JTWork_" , year[i] , "_" , label[3] , ".csv" , sep = ""  ) , row.names = FALSE )
  
  adeMAP  <- merge ( ade , VAMP , by = "SA1" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = JTW ) , size = 0.02  ) +
    scale_fill_gradient ( low = "green" , high = "red" ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ~ " - Journey to Work" ) ) +
    theme_void ( )
  ggsave ( filename = paste ( "results/" , "VAMPIRE_JTWork_" , year[i]  , "_" , label[3] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##save as VAMP component for later
  VAMP2 <- VAMP
  ALL   <- merge ( VAMP1 , VAMP2 , by = "SA1" )
  
  ###TENURE TYPE -- following "Vampire Methodology.docx"
  VA              <- read.xlsx ( paste ( paste ( "Data/absDataTable" , year[i] , "/" , sep = "" ) , year[i] , " Tenure Type Adelaide SA1.xlsx" , sep = "" ) , sheet = 1 ,  startRow = 9 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- VA[-1,-1]
  names ( VA )[1] <- 'SA1'
  VA$SA1          <- as.double( VA$SA1 )
  VA              <- subset ( VA , !is.na(VA$SA1) )  
  
  ###if year before latest SA1 definitions, take appropriate steps
  ##for 2016
  if ( year[i] == 2016 ) {
    # Check if 7-digit and convert to 11-digit
    if ( nchar ( VA$SA1[1] ) < 11 ) { 
      GEO      <- read.csv ( paste ( "Data/" , "SA1.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
      GEO      <- GEO[,2:3]
      names ( GEO ) <- c ( "SA1_2016" , "SA1short" )
      names ( VA )[1] <- "SA1short"
      VAM  <- merge ( VA , GEO , by = "SA1short" , all.x = TRUE )
      VAM  <- VAM[,-1]
    }
    #Convert to 2021 SA1 geography using Ratio's provided by ABS for this purpose
    GEO                     <- read.csv ( paste ( "Data/" , "CG_SA1_2016_SA1_2021.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
    GEO                     <- GEO[,1:3]
    names ( GEO )           <- c ( "SA1_2016" , "SA1" , "trans" )
    VAMP                    <- merge ( GEO , VAM , by = "SA1_2016" , all.y = TRUE )
    VAMP[4:( ncol (VAMP))]  <- lapply ( VAMP[4:( ncol (VAMP))] , function (x) VAMP$trans * x )  
    VAMP                    <- VAMP[, c ( 2 , 4:( ncol (VAMP) ) ) ]
    ##merge duplicate rows (see Notes)
    VAMP                    <- aggregate( . ~ SA1 , data = VAMP , FUN = sum  )
    
    rm ( VAM , GEO )
  }
  if ( year[i] == 2021 ) { VAMP = VA }
    
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
  write.csv ( VAMP , file = paste ( "results/" , "VAMPIRE_Tenure_" , year[i] , "_" , label[3] , ".csv" , sep = ""  ) , row.names = FALSE )
  
  adeMAP  <- merge ( ade , VAMP , by = "SA1" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = TEN ) , size = 0.02  ) +
    scale_fill_gradient ( low = "green" , high = "red" ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ~ " - Tenure" ) ) +
    theme_void ( )
  ggsave ( filename = paste ( "results/" , "VAMPIRE_Tenure_" , year[i]  , "_" , label[3] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##save as VAMP component for later
  VAMP3 <- VAMP
  ALL   <- merge ( ALL , VAMP3 , by = "SA1" )
  
  rm ( adeMAP , VA , VAMP )
  
  
  #i<-2
  ###INCOME -- following "Vampire Methodology.docx"
  VA              <- read.xlsx ( paste ( paste ( "Data/absDataTable" , year[i] , "/" , sep = "" ) , year[i] , " HH Income Adelaide SA1.xlsx" , sep = "" ) , sheet = 1 ,  startRow = 9 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  VA              <- VA[-1,-1]
  names ( VA )[1] <- 'SA1'
  VA$SA1          <- as.double( VA$SA1 )
  VA              <- subset ( VA , !is.na(VA$SA1) )  
  
  ###if year before latest SA1 definitions, take appropriate steps
  ##for 2016
  if ( year[i] == 2016 ) {
    # Check if 7-digit and convert to 11-digit
    if ( nchar ( VA$SA1[1] ) < 11 ) { 
      GEO      <- read.csv ( paste ( "Data/" , "SA1.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
      GEO      <- GEO[,2:3]
      names ( GEO ) <- c ( "SA1_2016" , "SA1short" )
      names ( VA )[1] <- "SA1short"
      VAM  <- merge ( VA , GEO , by = "SA1short" , all.x = TRUE )
      VAM  <- VAM[,-1]
    }
    #Convert to 2021 SA1 geography using Ratio's provided by ABS for this purpose
    GEO                     <- read.csv ( paste ( "Data/" , "CG_SA1_2016_SA1_2021.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
    GEO                     <- GEO[,1:3]
    names ( GEO )           <- c ( "SA1_2016" , "SA1" , "trans" )
    VAMP                    <- merge ( GEO , VAM , by = "SA1_2016" , all.y = TRUE )
    VAMP[4:( ncol (VAMP))]  <- lapply ( VAMP[4:( ncol (VAMP))] , function (x) VAMP$trans * x )  
    VAMP                    <- VAMP[, c ( 2 , 4:( ncol (VAMP) ) ) ]
    ##merge duplicate rows (see Notes)
    VAMP                    <- aggregate( . ~ SA1 , data = VAMP , FUN = sum  )
    
    rm ( VAM , GEO )
  }
  if ( year [i] == 2021 ) { VAMP = VA }
  
  ###Calculate Vampire component
  VAMP$adjTot     <- rowSums ( VAMP[, c ( 2:26 )] , na.rm = TRUE )  ## note that total of categories does NOT equal the Total given!?
  VAMP$vamTot     <- VAMP$adjTot - VAMP$Partial.income.stated - VAMP$All.incomes.not.stated - VAMP$Not.applicable 
  VAMP$wAvg       <- VAMP$vamTot - VAMP$Negative.income - VAMP$Nil.income
  ##weighted income allocation based on each census category
  inc             <- gsub ( "[A-Za-z]|\\(.*|\\.|\\$|\\," , "" , names ( VAMP )[4:23] )
  incStart        <- as.numeric ( gsub ( "\\-.*" , "" , inc  ) )
  incEnd          <- as.numeric ( gsub ( ".*\\-" , "" , inc  ) )
  incEnd[20]      <- 12000
  VAMP$aInc       <- ifelse ( VAMP$wAvg == 0 , 0 ,  
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
                             VAMP$`$8,000.or.more.($416,000.or.more)` * mean ( c (incStart[20],incEnd[20]) ) )  / VAMP$wAvg )
  
  #VAMP$vamPct     <- ifelse( VAMP$vamTot == 0 , 0 , ( VAMP$Owned.with.a.mortgage ) / VAMP$vamTot )
  ###generate income quantiles using data range
  qMV              <- quantile ( VAMP$aInc , c ( 0 , 1 , 0.10  , 0.25 , 0.50 , 0.75 , 0.90 ) )  #get range limits for vampire based on range of data and ranges provided in instructions
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
  
  ###generate VAmpire component using data range
  #VAMP$INC        <- ifelse ( VAMP$aInc < iRange[1] , 0 , 
  #                            ifelse ( ( VAMP$aInc < iRange[2] & VAMP$aInc >= iRange[1] ) , 1 ,
  #                                     ifelse ( ( VAMP$aInc < iRange[3] & VAMP$aInc >= iRange[2] ) , 2  ,
  #                                              ifelse ( ( VAMP$aInc < iRange[4] & VAMP$aInc >= iRange[3] ) , 3 ,
  #                                                       ifelse ( ( VAMP$aInc >= iRange[5] ) , 5 , 4 ) ) ) ) )
  
  ### Output intermediate data (and maps) for Q&A and analysis
  write.csv ( VAMP , file = paste ( "results/" , "VAMPIRE_Income_" , year[i] , "_" , label[1] , ".csv" , sep = ""  ) , row.names = FALSE )
  
  adeMAP  <- merge ( ade , VAMP , by = "SA1" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = INC ) , size = 0.02  ) +
    scale_fill_gradient ( low = "green" , high = "red" ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ~ " - Income" ) ) +
    theme_void ( )
  ggsave ( filename = paste ( "results/" , "VAMPIRE_Income_" , year[i]  , "_" , label[1] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  ##add income to VAMPIRE Matrix, Add up Vampire components, make a map
  VAMP4 <- VAMP
  ALL   <- merge ( ALL , VAMP4 , by = "SA1" )
  
  ALL$VAM <- ALL$MV + ALL$JTW + ALL$TEN + ALL$INC
  write.csv ( ALL , file = paste ( "results/" , "VAMPIRE_ALLcomponents_" , year[i] , "_" , label[3] , ".csv" , sep = ""  ) , row.names = FALSE )
  
  adeMAP  <- merge ( ade , ALL , by = "SA1" , all.x = FALSE  )
  plot <- ggplot( adeMAP) +
    geom_sf( aes ( fill = VAM ) , size = 0.02  ) +
    scale_fill_continuous( limits = c ( 0 , 20 ) , palette = c ( "green" , "red") ) +
#    scale_fill_gradient ( low = "green" , high = "red" ) +
    ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ~ " - Composite" ) ) +
    theme_void ( )
  ggsave ( filename = paste ( "results/" , "VAMPIRE_Composite_" , year[i]  , "_" , label[3] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
  
  rm ( adeMAP , VA , VAMP )

  
  if (i == 2 ) {  
    ###EVs -- following "Notes for EV calculations.docx"
    VA              <- read.xlsx ( paste ( "Data/AUsEV/EV_Index_Registration_Data_2021-2025.xlsx" , sep = "" ) , sheet = 2 ,  startRow = 1 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
    VA              <- subset ( VA , VA$State == "SA" )
    VAM             <- melt ( VA , id.vars = c ( "State" , "Fuel.Type" , "Postcode" )  )
    VAM$variable    <- as.character ( VAM$variable )
    VAM$variable    <- substr ( VAM$variable , nchar ( VAM$variable )  - 3 , nchar ( VAM$variable ) )
    VAM$variable    <- as.numeric ( VAM$variable ) - 1
    rm ( VA )
    
    ###STILL issue with allocation of SA1 share to postcode... check sig digits
    XOVa            <- read.csv ( paste ( "Data/" , "post_sa1_overlap.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
    XOVa            <- XOVa[, c ( 6 , 8 , 10 )]
    XOVb            <- read.csv ( paste ( "Data/" , "postCodeArea.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
    XOVb            <- XOVb[,c( 5 , 7 )]
    names ( XOVb )[2] <- "PostArea"
    XOV             <- merge ( XOVa , XOVb , by = "postcode" , all = TRUE )
    XOV$share       <- XOV$Shape_Area / XOV$PostArea
    XOV             <- XOV[, c ( 1 , 2 , 5 )]
    names ( XOV )   <- c ( "Postcode" , "SA1" , "share" )
    rm ( XOVa , XOVb )
    XOV             <- subset ( XOV , !is.na ( XOV$SA1 ) )
    chk             <- aggregate ( share ~ Postcode , FUN = sum , data = XOV )
    
    VAMP            <- merge ( XOV , VAM , by = "Postcode" , all = TRUE )
    VAMP$adjVal     <- VAMP$share * VAMP$value 
    VAMP            <- subset ( VAMP ,  !is.na ( VAMP$SA1 ) )
    VAMP            <- subset ( VAMP ,  !is.na ( VAMP$State ) )
    rm ( VAM ) 
    a <- subset ( VAMP , VAMP$Postcode == 5000)
    sum ( a$adjVal)
    rm ( chk , a )
    
    VAMPI           <- aggregate( adjVal ~ SA1 + Fuel.Type + variable , FUN = sum , data = VAMP )
    VAMPIx          <- subset ( VAMPI , VAMPI$variable == year[i] )
    VAMPIh          <- dcast ( VAMPIx , SA1 ~ Fuel.Type , value.var = "adjVal" , fun.aggregate = sum   )
    VAMPIh$BEVshare <- round ( VAMPIh$BEV / ( VAMPIh$ICE + VAMPIh$`Hybrid/PHEV` + VAMPIh$BEV ) , 3 )
    maxEV           <- max ( VAMPIh$BEVshare )
    VAMPIh$BEVs     <- ifelse ( VAMPIh$BEVshare < maxEV * 0.1 , 5 , 
                                 ifelse ( ( VAMPIh$BEVshare < maxEV * 0.25 & VAMPIh$BEVshare >= maxEV * 0.1 ) , 4 ,
                                          ifelse ( ( VAMPIh$BEVshare < maxEV * 0.5 & VAMPIh$BEVshare >= maxEV * 0.25 ) , 3 ,
                                                   ifelse ( ( VAMPIh$BEVshare < maxEV * 0.75 & VAMPIh$BEVshare >= maxEV * 0.5 ) , 2 ,
                                                            ifelse ( ( VAMPIh$BEVshare >= maxEV * 0.90 ) , 0 , 1 ) ) ) ) )
    
    ###
    write.csv ( VAMPIh , file = paste ( "results/" , "VAMPIRE_EVs_" , year[i] , "_" , label[4] , ".csv" , sep = ""  ) , row.names = FALSE )
   
    adeMAP  <- merge ( ade , VAMPIh , by = "SA1" , all.x = FALSE  )
    plot <- ggplot( adeMAP) +
      geom_sf( aes ( fill = BEVs ) , size = 0.02  ) +
      scale_fill_gradient ( low = "green" , high = "red" ) +
      ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ~ " - BEVs" ) ) +
      theme_void ( )
    ggsave ( filename = paste ( "results/" , "VAMPIRE_EVs_" , year[i]  , "_" , label[4] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
    
    ##add income to VAMPIRE Matrix, Add up Vampire components, make a map
    VAMP5 <- VAMPIh
    ALL   <- merge ( ALL , VAMP5 , by = "SA1" )
    
    rm ( adeMAP , VAMP, VAMPI , VAMPIh , VAMPIx )
    
    
    
    ###Solar Panels -- following "Notes for Solar Installs calculations.docx"
    SOLa              <- read.csv ( paste ( "Data/AusCER/" , "sgu-solar-installations-2011-to-present-and-totals_clean.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
    SOLa              <- SOLa[,c ( 1:134)]
    SOLa$Historic_total_InstQty_2001to2011 <- as.numeric ( SOLa$Historic_total_InstQty_2001to2011 )
    SOLa$TotIDec21    <- rowSums ( SOLa[, 2:134], na.rm = TRUE)
    SOLb              <- SOLa[,c ( 1,135)]
    names ( SOLb )[1] <- "Postcode"
    SOLa              <- read.csv ( paste ( "Data/AusCER/" , "sgu-solar-capacity-2011-to-present-and-totals.csv" , sep = "" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
    SOLa              <- SOLa[,c ( 1:134)]
    SOLa[,2:134]      <- lapply ( SOLa[,2:134] , function (x) as.numeric(x) )
    SOLa$TotCDec21    <- rowSums ( SOLa[, 2:134], na.rm = TRUE)
    SOLc              <- SOLa[,c ( 1,135)]
    names ( SOLc )[1] <- "Postcode"
    SOL               <- merge ( SOLb , SOLc , by = "Postcode" )
    rm ( SOLa , SOLb , SOLc )
  
    ###STILL issue with allocation of SA1 share to postcode... check sig digits
    SOLa              <- merge ( SOL , XOV , by = "Postcode" , all = TRUE ) 
    SOLa$adjTotI      <- SOLa$share * SOLa$TotIDec21
    SOLa$adjTotC      <- SOLa$share * SOLa$TotCDec21
    SOLa              <- subset ( SOLa ,  !is.na ( SOLa$SA1 ) )
    SOLb              <- aggregate( adjTotI ~ SA1 , FUN = sum , data = SOLa )
    SOLc              <- aggregate( adjTotC ~ SA1 , FUN = sum , data = SOLa )
    SOLd              <- merge ( SOLb , SOLc , by = "SA1" )
    SOLd$CperI        <- SOLd$adjTotC / SOLd$adjTotI 
    VAMP             <- merge ( SOLd , VAMP3[,c(1,11)] , by = "SA1")
    rm ( SOLa , SOLb , SOLc , SOLd )
  
    ###Consider installations vs # HH in each SA1
    VAMP$SOLpct      <- ifelse ( VAMP$Total == 0 , 0 , VAMP$adjTotI / VAMP$Total )
    ###generate VAmpire component using data range
    VAMP$SOL        <- ifelse ( VAMP$SOLpct < vRange[1] , 0 , 
                                ifelse ( ( VAMP$SOLpct < vRange[2] & VAMP$SOLpct >= vRange[1] ) , 1 ,
                                         ifelse ( ( VAMP$SOLpct < vRange[3] & VAMP$SOLpct >= vRange[2] ) , 2 ,
                                                  ifelse ( ( VAMP$SOLpct < vRange[4] & VAMP$SOLpct >= vRange[3] ) , 3 ,
                                                           ifelse ( ( VAMP$SOLpct >= vRange[5] ) , 5 , 4 ) ) ) ) )
    
    VAMP$SOL <- ifelse ( VAMP$Total == 0 , NA , VAMP$SOL )
    
    ###
    write.csv ( VAMP , file = paste ( "results/" , "VAMPIRE_PVs_" , year[i] , "_" , label[4] , ".csv" , sep = ""  ) , row.names = FALSE )
    
    adeMAP  <- merge ( ade , VAMP , by = "SA1" , all.x = FALSE  )
    plot <- ggplot( adeMAP) +
      geom_sf( aes ( fill = SOL ) , size = 0.02  ) +
      scale_fill_gradient ( low = "green" , high = "red" ) +
      ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ~ " - PV Install %" ) ) +
      theme_void ( )
    ggsave ( filename = paste ( "results/" , "VAMPIRE_PVs_" , year[i]  , "_" , label[4] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
    
    ##add income to VAMPIRE Matrix, Add up Vampire components, make a map
    VAMP6 <- VAMP
    ALL   <- merge ( ALL , VAMP6 , by = "SA1" )
    
    ALL$VAMu <- ALL$BEVs + ALL$SOL 
    write.csv ( ALL , file = paste ( "results/" , "VAMPIRE_ALLcomponents_plusEVPV" , year[i] , "_" , label[4] , ".csv" , sep = ""  ) , row.names = FALSE )
    
    adeMAP  <- merge ( ade , ALL , by = "SA1" , all.x = FALSE  )
    plot <- ggplot( adeMAP ) +
      geom_sf( aes ( fill = VAMu ) , size = 0.02  ) +
      scale_fill_continuous( limits = c ( 0 , 10 ) , palette = c ( "green" , "red") ) +
      #    scale_fill_gradient ( low = "green" , high = "red" ) +
      ggtitle( bquote ( "VAMPIRE " ~ .( year[i] ) ~ " - ( BEV + PV )" ) ) +
      theme_void ( )
    ggsave ( filename = paste ( "results/" , "VAMPIRE_Composite_updateEVPV" , year[i]  , "_" , label[4] ,  ".png" , sep = "" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )
    
    rm ( adeMAP , VAMP, SOL )
  }
}

####-----END VAMPIRE LOOP--------------------


