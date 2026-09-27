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
  library(ozgs)      # gccsa(), poa()
  library(reshape2)  # melt(), dcast() -- used by the EV component
})

###--B.Dirs and Vars

##global
form        <- "poa"
year        <- 2021  # ( 2026 )
data_dir    <- paste0 ( "data/" , form , year )

##vampire settings
v_range <- c ( 0.1 , 0.25 , 0.50 , 0.75 , 0.9  )

##output settings
results_dir <- paste0 ( "results/" , form , year , "out" )
width        <- 1920
height       <- 1080

###--C. Helper functions

#' Bucket a value into an ascending 0-5 score using `breaks_vec` as the four
#' interior breakpoints plus the top threshold for score 5.
vampire_classify <- function(x, breaks_vec) {
  breaks <- c(-Inf, breaks_vec, Inf)
  score  <- cut(x, breaks = breaks, labels = 0:5, right = FALSE)
  as.numeric(as.character(score))
}
#-----

#' Prefix a set of newly-added columns with a component tag (e.g. "mvs_"),
#' matched by name rather than hard-coded position. This avoids the silent
#' mismatches that caused the variable-duplication warnings noted in the
#' original script when columns shifted position.
rename_component_cols <- function(df, cols, prefix) {
  idx <- match(cols, names(df))
  names(df)[idx] <- paste0(prefix, cols)
  df
}
#-----

#' Same breakpoints, but scored in descending order (low x -> 5, high x -> 0).
#' Used for both eVAMPIRE components: less BEV uptake / fewer solar
#' installs per household is treated as more vulnerable.
vampire_classify_desc <- function(x, breaks_vec) {
  5 - vampire_classify(x, breaks_vec)
}
#-----

#' Merge a component's data onto the postcode geometry and save a choropleth.
save_evampire_map <- function(post, df, fill_var, year, out_path, width, height,
                              fill_limits = c(0, 5)) {
  merged <- merge(post, df, by = "GEO", all.x = FALSE)
  
  p <- ggplot(merged) +
    geom_sf(aes(fill = .data[[fill_var]]), size = 0.02) +
    scale_fill_continuous(
      limits = fill_limits, low = "green", high = "red",
      labels = function(x) formatC(x, format = "f", digits = 1)
    ) +
    ggtitle(bquote("eVAMPIRE" ~ .(year))) +
    theme_void()
  
  ggsave(filename = out_path, plot = p, width = width, height = height,
         units = "px", device = "png")
  invisible(p)
}

#' Load the postcode geometry for the chosen GCCSA, building and caching it
#' on first use instead of rebuilding it (and immediately re-reading the
#' file it just wrote) on every run.
load_or_build_geometry <- function(data_dir, gccsa_name = "Greater Adelaide",
                                   extra_poa = 5950, edition = 3) {
  cache_path <- file.path(data_dir, "Adelaide_post_mapserver.rData")
  
  if (!file.exists(cache_path)) {
    gccs   <- ozgs::gccsa(gccsa_name, edition = edition)
    region <- ozgs::poa(edition = edition, filter_geom = gccs$geometry)
    region$GEO <- as.double(region$poa_code_2021)
    poa_list <- c(region$GEO, extra_poa)
    
    post <- ozgs::poa(edition = edition)
    post$GEO <- as.double(post$poa_code_2021)
    post <- subset(post, GEO %in% poa_list)
    
    save(post, file = cache_path)
  }
  
  env <- new.env()
  load(cache_path, envir = env)
  post <- env$post
  post$geo <- sf::st_as_sf(post$geometry)
  post
}

####-----END 0.CONFIG---------------


####-----eVAMPRIE--------------------

###--0. Get geographic dataset
post <- load_or_build_geometry(data_dir)
#--
  
###--1. EV Data from AAA
#' EV registrations component (BEV share of registrations)
file_path <- file.path(data_dir, "EV_Index_Registration_Data_2021-2025.xlsx")

va <- openxlsx::read.xlsx(file_path, sheet = 2, startRow = 1, colNames = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE)
va <- subset(va, State %in% c("SA", "OTHER"))
names(va)[1] <- "GEO"  # first column holds the postcode
va <- subset(va, !(grepl("Cells|Total", GEO) | is.na(GEO)))

va_long <- reshape2::melt(va, id.vars = c("State", "Fuel.Type", "GEO"))
va_long$variable <- as.character(va_long$variable)
va_long$variable <- as.numeric( substr(va_long$variable, nchar(va_long$variable) - 3, nchar(va_long$variable)) ) - 1

vamp      <- merge(va_long, post, by = "GEO", all.y = TRUE)
vamp_year <- subset(vamp, variable == year)

ev_wide <- reshape2::dcast(vamp_year, GEO ~ Fuel.Type, value.var = "value", fun.aggregate = sum)
ev_wide$BEVshare <- round( ev_wide$BEV / (ev_wide$ICE + ev_wide$`Hybrid/PHEV` + ev_wide$BEV), 3 )

max_ev <- max(ev_wide$BEVshare, na.rm = TRUE)
ev_wide$BEVs <- vampire_classify_desc(ev_wide$BEVshare, max_ev * v_range)

write.csv ( ev_wide, paste0 ( results_dir, "/d_eVAMPIRE_EVs_", year , ".csv" ), row.names = FALSE)
save_evampire_map(
  post, ev_wide, "BEVs", year,
  paste0(results_dir, "/p_eVAMPIRE_EVs_", year , ".png"),
  width, height
)

ev <- rename_component_cols( ev_wide , names(ev_wide)[2:(ncol(ev_wide)-1)] , "evs_" )
#--

###--2. PV Data from AECR
#' Solar installations component (installs per household)

## Both CER files are wide, one column per month; column 1 is the
## postcode and columns 2:134 are the monthly (or historic-cumulative)
## counts being summed into a single total.
## Download PV installation data from -> https://cer.gov.au/document/sgu-solar-installations-2011-to-present-and-totals
## Download PV capacity data from -> https://cer.gov.au/document/sgu-solar-capacity-2011-to-present-and-totals
## Download household type data from -> https://www.abs.gov.au/census/find-census-data/datapacks
##  Select 2021 , General Community Profile , Postal Areas (POA) 
##  Download and unpack DataPack -> South Australia
##  Use "2021Census_G37_SA_POA.csv"

installs <- read.csv(
  file.path(data_dir, "sgu-solar-installations-2011-to-present-and-totals.csv"),
  header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
)
installs <- installs[, 1:134]  #134 is Dec2021 - can seacrh for more elegantly
installs$Historic.Total.Installation.Quantity..2001...2010. <- as.numeric(gsub(",", "", installs$Historic.Total.Installation.Quantity..2001...2010.))
installs$TotIDec21 <- rowSums(installs[, 2:134], na.rm = TRUE)
installs_totals <- installs[, c(1, 135)]
names(installs_totals)[1] <- "GEO"

##capacity data is unused in current eVAMPIRE, left in as future researchers may want to include in sub-metric and/or index
capacity <- read.csv(
  file.path(data_dir, "sgu-solar-capacity-2011-to-present-and-totals.csv"),
  header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
)
capacity <- capacity[, 1:134]
capacity[, 2:134] <- lapply(capacity[, 2:134], function(x) as.numeric(gsub(",", "", x)))
capacity$TotCDec21 <- rowSums(capacity[, 2:134], na.rm = TRUE)
capacity_totals <- capacity[, c(1, 135)]
names(capacity_totals)[1] <- "GEO"

solar <- merge(installs_totals, capacity_totals, by = "GEO")
solar$CperI <- solar$TotCDec21 / solar$TotIDec21

households <- read.csv(
  file.path(data_dir, "2021Census_G37_SA_POA.csv"),
  header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
)
households$hh  <- households$Total_Total - households$Total_DS_Flat_apart
households$GEO <- as.numeric(gsub("POA", "", households$POA_CODE_2021))  # see fix note (3) above
households <- subset(households, select = c("GEO", "hh"))

vamp <- merge(solar, households, by = "GEO", all.y = TRUE)

vamp$SOLpct <- ifelse(vamp$hh == 0, 0, vamp$TotIDec21 / vamp$hh)
vamp$SOLpct <- pmin(vamp$SOLpct, 1)
vamp$SOL    <- vampire_classify_desc(vamp$SOLpct, v_range)
vamp$SOL[vamp$hh == 0] <- NA

write.csv(vamp, paste0 ( results_dir, "/d_eVAMPIRE_PVs_", year , ".csv" ), row.names = FALSE)
save_evampire_map(
  post, vamp, "SOL", year,
  paste0 ( results_dir , "/p_eVAMPIRE_PVs_", year, ".png"),
  width, height
)

sol <- rename_component_cols(vamp, names(vamp)[2:(ncol(vamp)-1)] , "pvs_")
#--

###--3. Composite
vamp      <- merge(ev, sol, by = "GEO")
vamp$eVAM <- vamp$BEVs + vamp$SOL
write.csv ( vamp , paste0( results_dir, "/d_eVAMPIRE_ALLcomponents_", year, ".csv" ) , row.names = FALSE)

save_evampire_map(
  post, vamp, "eVAM", year,
  paste0( results_dir , "/" , "p_eVAMPIRE_Composite_", year , ".png"),
  width, height, fill_limits = c(0, 10)
)
#--
####-----END eVAMPIRE--------------------