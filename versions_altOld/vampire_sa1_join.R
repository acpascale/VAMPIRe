##   vampire_sa1_join.r  - script for combining vampires
##
##   Created:        05 September 2024 (geography join for nzi)
##   Last updated:   27 April 2026 
##
##   ToDo: 
##

#-----0.ADMIN - Include libraries and other important-----------

##--A. Clean
setwd("X:/WORK/d1_PROJECTS/p2026_Vampire_UniSA/VampireIndex2026")
#source("d0_2code/clean.R")

##--B.Load Library for country manipulations
suppressMessages ( library ( "openxlsx"     , lib.loc=.libPaths() ) )      # worksheet functions
suppressMessages ( library ( "reshape2"     , lib.loc=.libPaths() ) )      # melt, dcast

##--C.Variables
years <- c ( 2021 , 2016 , 2011 , 2006 , 2001 )

#-----END 0.ADMIN---------------


#-----1.HACK--------------------

####-- SA1
SA1           <- read.xlsx ( "VAMPIRE_timeS.xlsx" , sheet = 7 , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
SA1           <- SA1[,-c(5,7)]

#merge 2021 and 2016 , 2011 , 2006 , 2001
for ( i in 1:5 ) {
  TEM              <- read.xlsx ( "VAMPIRE_timeS.xlsx" , sheet = (7 - i ) , colNames = TRUE ,  skipEmptyRows = TRUE , skipEmptyCols = TRUE  )
  names (TEM)[2:5] <- lapply ( names (TEM)[2:5] , function ( x ) paste ( x , years[i] , sep = "" ) ) 
  if ( i == 1 ) { VAM <- merge ( SA1 , TEM , by = "ID" , all.x = TRUE  ) }
  if ( i > 1  ) { VAM <- merge ( VAM , TEM , by = "ID" , all.x = TRUE  ) }
}
rm ( TEM , i )


write.csv ( VAM , paste ( "VAMPIRE_timeS.csv" , sep = "" ) , row.names = FALSE )
rm (  )

#-----END 1.HACK--------------------