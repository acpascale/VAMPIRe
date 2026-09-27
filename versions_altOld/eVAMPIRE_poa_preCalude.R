##   [x].r  - eVAMPIRE index generator
##
##   Created:        May 2026
##   Updated:        August 2026
##   Updated:        September 2026 -- cleaned and modularized code using Claude Sonnet 5 Medium, "Convert code to best practice R with function calls"

##   Datasets not included as owned by ABS, AAA, AECR: 
##   [1] Postcodes and Postal Areas, ABS - https://www.abs.gov.au/websitedbs/censushome.nsf/home/factsheetspoa?opendocument&navpos=450
##   [2] ABS. 2021. “Tenure and Landlord Type (TENLLD).” Australian Bureau of Statistics. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/housing/tenure-and-landlord-type-tenlld.
##   [3] AAA. n.d. “Electric Vehicle Index.” Australian Automobile Association. Accessed May 7, 2026. https://www.aaa.asn.au/research-data/electric-vehicle/.
##   [4] Australian Clean Energy Regulator. 2026. “Small-Scale Installation Postcode Data.” April 16. https://cer.gov.au/markets/reports-and-data/small-scale-installation-postcode-data.

####--0.CONFIG - Include libraries, global variables, print/export settings, functions-----------

###--A.Load Library for country manipulations
suppressPackageStartupMessages({
  library(openxlsx)  # read.xlsx()
  library(ggplot2)   # geom_sf(), ggsave()
  library(sf)        # st_as_sf()
  library(ozgs)      # gccs() , poa()
})

###--C.Dirs and Vars

##global
form        <- "poa"
year        <- 2021  # ( 2026 )
data_dir    <- paste0 ( "data/" , form , year )

##vampire settings
vRange <- c ( 0.1 , 0.25 , 0.50 , 0.75 , 0.9  )

##output settings
results_dir <- paste0 ( "results/" , form , year , "out" )
width        <- 1920
height       <- 1080

####-----END 0.ADMIN---------------


####-----eVAMPRIE--------------------

###0. Get geographic dataset -- make into a helper function
#' Load the selected geography for estimation and display
#'
#' The code below uses the ozgs (https://github.com/gardiners/ozgs) wrapper to download spatial data, but could be downloaded directly from provider
adelaide_gccs      <- gccsa("Greater Adelaide", edition = 3)
post               <- poa ( edition = 3, filter_geom = adelaide_gccs$geometry  )   
post$GEO           <- as.double( post$poa_code_2021 )
adeList            <- c ( post$GEO , 5950 )
post               <- poa ( edition = 3 )
post$GEO           <- as.double( post$poa_code_2021 )
post               <- subset ( post , post$GEO %in% adeList )
save ( post , file = paste0 ( data_dir , "/Adelaide_post_mapserver.rData" ) )
rm ( adelaide_gccs , adeList )
load ( paste0 ( data_dir , "/Adelaide_post_mapserver.rData" ) )
post$geo <- st_as_sf(post$geometry)


###EVs -- following "Notes for EV calculations.docx"
VA              <- read.xlsx ( paste0 ( data_dir , "/EV_Index_Registration_Data_2021-2025.xlsx" ) , sheet = 2 ,  startRow = 1 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
VA              <- subset ( VA , VA$State %in% c ( "SA" , "OTHER" ) )
names(VA)[1]     <- "POA"
VA              <- subset ( VA , !( grepl ( "Cells|Total" , VA$GEO ) | is.na ( VA$GEO ) ) )
VAM             <- melt ( VA , id.vars = c ( "State" , "Fuel.Type" , "GEO" )  )
VAM$variable    <- as.character ( VAM$variable )
VAM$variable    <- substr ( VAM$variable , nchar ( VAM$variable )  - 3 , nchar ( VAM$variable ) )
VAM$variable    <- as.numeric ( VAM$variable ) - 1
rm ( VA )

VAMP            <- merge ( VAM , post , by = "GEO" , all.y = TRUE )

VAMPI           <- subset ( VAMP , VAMP$variable == year[i] )
VAMPIh          <- dcast ( VAMPI , GEO ~ Fuel.Type , value.var = "value" , fun.aggregate = sum   )
VAMPIh$BEVshare <- round ( VAMPIh$BEV / ( VAMPIh$ICE + VAMPIh$`Hybrid/PHEV` + VAMPIh$BEV ) , 3 )
maxEV           <- max ( VAMPIh$BEVshare )
VAMPIh$BEVs     <- ifelse ( VAMPIh$BEVshare < maxEV * vRange[1] , 5 , 
                             ifelse ( ( VAMPIh$BEVshare < maxEV * vRange[2] & VAMPIh$BEVshare >= maxEV * vRange[1] ) , 4 ,
                                      ifelse ( ( VAMPIh$BEVshare < maxEV * vRange[3] & VAMPIh$BEVshare >= maxEV * vRange[2] ) , 3 ,
                                               ifelse ( ( VAMPIh$BEVshare < maxEV * vRange[4] & VAMPIh$BEVshare >= maxEV * vRange[3] ) , 2 ,
                                                        ifelse ( ( VAMPIh$BEVshare >= maxEV * vRange[5] ) , 0 , 1 ) ) ) ) )

###
write.csv ( VAMPIh , file = paste0 ( results_dir  , "/d_eVAMPIRE_EVs_" , year[i] , "_" , label[2] , ".csv" ) , row.names = FALSE )

adeMAP  <- merge ( post , VAMPIh , by = "GEO" , all.x = FALSE  )
plot <- ggplot( adeMAP ) +
  geom_sf ( aes ( fill = BEVs ) , size = 0.02  ) +
  scale_fill_continuous( limits = c ( 0 , 5 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( "0" , format ( round (x , 2 ) , nsmall = 1 ) ) ) +
  ggtitle( bquote ( "eVAMPIRE " ~ .( year[i] ) ) ) + #~ " - BEVs        " ) ) +
  theme_void ( )
ggsave ( filename = paste0 ( results_dir  , "/p_eVAMPIRE_EVs_" , year[i]  , "_" , label[2] ,  ".png" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )

##add income to VAMPIRE Matrix, Add up Vampire components, make a map
VAMPe1 <- VAMPIh
ALL    <- VAMPe1

rm ( adeMAP , VAMP, VAMPI , VAMPIh , VAM  )



###Solar Panels -- following "Notes for Solar Installs calculations.docx"
SOLa              <- read.csv ( paste0 ( data_dir , "/sgu-solar-installations-2011-to-present-and-totals_clean.csv" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
SOLa              <- SOLa[,c ( 1:134)]
SOLa$Historic_total_InstQty_2001to2011 <- as.numeric ( gsub ("," , "" , SOLa$Historic_total_InstQty_2001to2011 ) )
SOLa$TotIDec21    <- rowSums ( SOLa[, 2:134], na.rm = TRUE)
SOLb              <- SOLa[,c ( 1,135)]
names ( SOLb )[1] <- "GEO"
SOLa              <- read.csv ( paste0 ( data_dir , "/sgu-solar-capacity-2011-to-present-and-totals.csv" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
SOLa              <- SOLa[,c ( 1:134)]
SOLa[,2:134]      <- lapply ( SOLa[,2:134] , function (x) as.numeric( gsub ("," , "" ,  x ) ) )
SOLa$TotCDec21    <- rowSums ( SOLa[, 2:134], na.rm = TRUE)
SOLc              <- SOLa[,c ( 1,135)]
names ( SOLc )[1] <- "GEO"
SOL               <- merge ( SOLb , SOLc , by = "GEO" )
SOL$CperI         <- SOL$TotCDec21 / SOL$TotIDec21

SOLh              <- read.csv ( paste0 ( data_dir , "/2021Census_G37_SA_POA.csv" ) , header=TRUE , stringsAsFactors = FALSE , fileEncoding = "UTF-8-BOM" )
SOLh$hh           <- SOLh$Total_Total - SOLh$Total_DS_Flat_apart
SOLh$GEO          <- as.numeric( gsub ( "GEO" , "" , SOLh$POA_CODE_2021 ) )
SOLh              <- subset ( SOLh , select = c ( "GEO" , "hh" ) )

###STILL issue with allocation of SA1 share to postcode... check sig digits
VAMP             <- merge ( SOL , SOLh , by = "GEO" , all.y = TRUE )
rm ( SOLa , SOLb , SOLc , SOLh , SOL )

###Consider installations vs # HH in each SA1
VAMP$SOLpct      <- ifelse ( VAMP$hh == 0 , 0 , VAMP$TotIDec21 / VAMP$hh )
VAMP$SOLpct      <- ifelse ( VAMP$SOLpct > 1 , 1 , VAMP$SOLpct )
###generate VAmpire component using data range
VAMP$SOL        <- ifelse ( VAMP$SOLpct < vRange[1] , 5 , 
                            ifelse ( ( VAMP$SOLpct < vRange[2] & VAMP$SOLpct >= vRange[1] ) , 4 ,
                                     ifelse ( ( VAMP$SOLpct < vRange[3] & VAMP$SOLpct >= vRange[2] ) , 3 ,
                                              ifelse ( ( VAMP$SOLpct < vRange[4] & VAMP$SOLpct >= vRange[3] ) , 2 ,
                                                       ifelse ( ( VAMP$SOLpct >= vRange[5] ) , 0 , 1 ) ) ) ) )

VAMP$SOL <- ifelse ( VAMP$hh == 0 , NA , VAMP$SOL )

###
write.csv ( VAMP , file = paste0 ( results_dir  , "/d_eVAMPIRE_PVs_" , year[i] , "_" , label[2] , ".csv" ) , row.names = FALSE )

adeMAP  <- merge ( post , VAMP , by = "GEO" , all.x = FALSE  )
plot <- ggplot( adeMAP ) +
  geom_sf( aes ( fill = SOL ) , size = 0.02  ) +
  scale_fill_continuous( limits = c ( 0 , 5 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( "0" , format ( round (x , 2 ) , nsmall = 1 ) ) ) +
  ggtitle( bquote ( "eVAMPIRE " ~ .( year[i] ) ) ) + #~ " - PV Install %" ) ) +
  theme_void ( )
ggsave ( filename = paste0 ( results_dir  , "/p_eVAMPIRE_PVs_" , year[i]  , "_" , label[2] ,  ".png" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )

##add income to VAMPIRE Matrix, Add up Vampire components, make a map
VAMPe2 <- VAMP
ALL   <- merge ( ALL , VAMPe2 , by = "GEO" )

ALL$eVAM <- ALL$BEVs + ALL$SOL 
write.csv ( ALL , file = paste0 ( results_dir  , "/d_eVAMPIRE_ALLcomponents_plusEVPV" , year[i] , "_" , label[2] , ".csv" ) , row.names = FALSE )

adeMAP  <- merge ( post , ALL , by = "GEO" , all.x = FALSE  )
plot <- ggplot( adeMAP ) +
  geom_sf( aes ( fill = eVAM ) , size = 0.02  ) +
  scale_fill_continuous( limits = c ( 0 , 10 ) , palette = c ( "green" , "red" ) , labels = function ( x ) paste0 ( format ( round (x , 2 ) , nsmall = 1 ) ) ) +
  ggtitle( bquote ( "eVAMPIRE " ~ .( year[i] ) ) ) + #~ " - ( BEV + PV )" ) ) +
  theme_void ( )
ggsave ( filename = paste0 ( results_dir  , "/p_eVAMPIRE_Composite_updateEVPV" , year[i]  , "_" , label[2] ,  ".png" ) , width = wdth , height = hgth ,  units = "px" , plot = plot , device = "png" )

rm ( adeMAP , VAMP )

####-----END eVAMPIRE--------------------


