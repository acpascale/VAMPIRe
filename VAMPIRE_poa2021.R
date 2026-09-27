##   [x].r  - VAMPIRE index generator
##
##   Created:        May 2026
##   Updated:        August 2026 -- moved from SA1 to POAs
##   Updated:        September 2026 -- cleaned and modularized code using Claude Sonnet 5 Medium, "Convert code to best practice R with function calls"
##

##   Datasets not included as owned by ABS: 
##   [1] Postcodes and Postal Areas, ABS - https://www.abs.gov.au/websitedbs/censushome.nsf/home/factsheetspoa?opendocument&navpos=450
##   [2] ABS. 2021. “Number of Motor Vehicles (Ranges) (VEHRD).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/transport/number-motor-vehicles-ranges-vehrd.
##   [3] ABS. 2021. “Method of Travel to Work (6 Travel Modes) (MTW06P).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/transport/method-travel-work-6-travel-modes-mtw06p.
##   [4] ABS. 2021. “Tenure Type (TEND).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/housing/tenure-type-tend.
##   [5] ABS. 2021. “Total Household Income as Stated (Weekly) (HINASD).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/income-and-work/total-household-income-stated-weekly-hinasd.


####--0.CONFIG - Include libraries, global variables, print/export settings, functions-----------

###--A.Load Library for country manipulations
suppressPackageStartupMessages({
  library(openxlsx)  # read.xlsx()
  library(ggplot2)   # geom_sf(), ggsave()
  library(sf)        # st_as_sf()
  #library(ozgs)      # gccs() , poa()
})

###--B.Dirs and Vars

##global
form        <- "poa"
year        <- 2021  # ( 2016 , 2021 , 2026 )
data_dir    <- paste0 ( "data/" , form , year )

##vampire settings
v_range <- c ( 0.1 , 0.25 , 0.50 , 0.75 , 0.9  )

##output settings
results_dir <- paste0 ( "results/" , form , year , "out" )
width        <- 1920
height       <- 1080

###--C. Helper functions (many generated from original code by Claude)

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
  names(df)[1] <- "GEO"
  df <- subset(df, !(grepl("Cells|Total", GEO) | is.na(GEO)))
  df$GEO <- as.double(gsub(", SA", "", df$GEO))
  df
}
#-----
#' Compute the VAMPIRE component percentage for one census table.
#'
#' `total_cols` are summed to get an adjusted total; `exclude_cols` are
#' subtracted from that total to get the usable denominator (`vamTot`);
#' `numerator_cols` are summed to get the count of interest, and the
#' percentage (`vamPct`) is their ratio.
compute_vampire_component <- function(df, total_cols, exclude_cols, numerator_cols) {
  df$adjTot <- rowSums(df[, total_cols], na.rm = TRUE)
  df$vamTot <- df$adjTot - rowSums(df[, exclude_cols, drop = FALSE], na.rm = TRUE)
  
  numerator <- rowSums(df[, numerator_cols, drop = FALSE], na.rm = TRUE)
  df$vamPct <- ifelse(df$vamTot == 0, 0, numerator / df$vamTot)
  df
}
#-----
#' Bucket a percentage into the 0-5 VAMPIRE score using the configured
#' breakpoints (`v_range` supplies the four interior breaks plus the top
#' threshold for score 5).
vampire_classify <- function(pct, v_range) {
  breaks <- c(-Inf, v_range, Inf)
  score  <- cut(pct, breaks = breaks, labels = 0:5, right = FALSE)
  as.numeric(as.character(score))
}
#-----
#' Merge a component's data onto the postcode geometry and save a choropleth.
save_vampire_map <- function(post, df, fill_var, year, out_path, width, height,
                             fill_limits = c(0, 5)) {
  merged <- merge(post, df, by = "GEO", all.x = FALSE)
  
  p <- ggplot(merged) +
    geom_sf(aes(fill = .data[[fill_var]]), size = 0.02) +
    scale_fill_continuous(
      limits = fill_limits, low = "green", high = "red",
      labels = function(x) formatC(x, format = "f", digits = 0)
    ) +
    ggtitle(bquote("VAMPIRE" ~ .(year))) +
    theme_void()
  
  ggsave(filename = out_path, plot = p, width = width, height = height,
         units = "px", device = "png")
  invisible(p)
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
#' Estimate mean weekly household income per POA from banded income counts.
#'
#' Parses the dollar-range column headers into numeric midpoints and takes a
#' weighted average, replacing the original 20-term hard-coded sum with a
#' single matrix multiplication (easier to audit and doesn't break if column
#' order shifts, so long as `income_cols` still matches the bands present).
compute_weighted_income <- function(df, income_cols = names(df)[4:23]) {
  ranges    <- gsub("[A-Za-z]|\\(.*|\\.|\\$|,", "", income_cols)
  inc_start <- as.numeric(gsub("-.*", "", ranges))
  inc_end   <- as.numeric(gsub(".*-", "", ranges))
  inc_end[length(inc_end)] <- 12000  # top bracket is open-ended in the source labels
  
  midpoints    <- (inc_start + inc_end) / 2
  weighted_sum <- as.matrix(df[, income_cols]) %*% midpoints
  
  ifelse(df$vamTot == 0, 0, as.numeric(weighted_sum) / df$vamTot)
}
#-----
#' Assign income-based VAMPIRE scores using the same `v_range` quantile
#' breakpoints, and record which quantile band (name + bounds) each POA
#' falls into. Lower income -> higher (more vulnerable) score.
compute_income_quantile_bands <- function(a_inc, v_range) {
  q      <- quantile(a_inc, probs = c(0, 1, v_range))
  bucket <- as.integer(cut(a_inc, breaks = c(-Inf, q[3:7], Inf), right = FALSE))
  
  score  <- c(5, 4, 3, 2, 1, 0)[bucket]
  bottom <- c(q[1], q[3], q[4], q[5], q[6], q[7])[bucket]
  top    <- c(q[3], q[4], q[5], q[6], q[7], q[2])[bucket]
  name   <- paste(
    names(q)[c(1, 3, 4, 5, 6, 7)][bucket], ">= x >",
    names(q)[c(3, 4, 5, 6, 7, 2)][bucket]
  )
  
  list(score = score, bottom = bottom, top = top, name = name)
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

####-----VAMPRIE--------------------

###--0. Get geographic dataset
post <- load_or_build_geometry(data_dir)
#--

###--1. Number of Motor Vehicles
file_path <- file.path(data_dir, paste0("ABS_Motor_vehicles_", year, ".xlsx"))
vamp      <- clean_abs_table(file_path)

vamp <- compute_vampire_component(
  vamp,
  total_cols     = 2:8,
  exclude_cols   = c("Not.stated", "Not.applicable"),
  numerator_cols = c("Two.motor.vehicles", "Three.motor.vehicles", "Four.or.more.motor.vehicles")
)
vamp$MV <- vampire_classify(vamp$vamPct, v_range)
vamp$MV[vamp$vamTot == 0] <- NA

write.csv ( vamp , file = paste0 ( results_dir  , "/d_VAMPIRE_motorVehicles_" , year , ".csv" ) , row.names = FALSE )
save_vampire_map(
  post, vamp, "MV", year,
  paste0 ( results_dir , "/" , "p_VAMPIRE_motorVehicles_", year, ".png"),
  width, height
)

mv <- rename_component_cols(vamp, names(vamp)[2:(ncol(vamp)-1)] , "mvs_")
#--

###--2. Method of Travel to Work
file_path <- file.path(data_dir, paste0("ABS_Journey_to_Work_", year, ".xlsx"))
vamp      <- clean_abs_table(file_path)

vamp <- compute_vampire_component(
  vamp,
  total_cols     = 2:8,
  exclude_cols   = c("Worked.at.home.or.did.not.go.to.work", "Not.stated", "Not.applicable"),
  numerator_cols = "Vehicle"
)
vamp$JTW <- vampire_classify(vamp$vamPct, v_range)
vamp$JTW[vamp$vamTot == 0] <- NA

write.csv ( vamp , file = paste0 ( results_dir  , "/d_VAMPIRE_journeyToWork_" , year , ".csv" ) , row.names = FALSE )
save_vampire_map(
  post, vamp, "JTW", year,
  paste0( results_dir , "/" , "p_VAMPIRE_journeyToWork_", year , ".png"),
  width, height
)

jtw <- rename_component_cols(vamp, names(vamp)[2:(ncol(vamp)-1)] , "jtw_")
#--

###--3. Tenure Type
file_path <- file.path(data_dir, paste0("ABS_Tenure_", year, ".xlsx"))
vamp      <- clean_abs_table(file_path)

vamp <- compute_vampire_component(
  vamp,
  total_cols     = 2:10,
  exclude_cols   = c("Not.stated", "Not.applicable"),
  numerator_cols = "Owned.with.a.mortgage"
)
vamp$TEN <- vampire_classify(vamp$vamPct, v_range)
vamp$TEN[vamp$vamTot == 0] <- NA

write.csv ( vamp , file = paste0 ( results_dir  , "/d_VAMPIRE_Tenure_" , year , ".csv" ) , row.names = FALSE )
save_vampire_map(
  post, vamp, "TEN", year,
  paste0( results_dir , "/" , "p_VAMPIRE_Tenure_", year, ".png"),
  width, height
)

ten <- rename_component_cols(vamp, names(vamp)[2:(ncol(vamp)-1)] , "ten_")
#--

###--4. Total Household Income as Stated (Weekly) 
file_path <- file.path(data_dir, paste0("ABS_Income_", year, ".xlsx"))
vamp <- clean_abs_table(file_path)

vamp$adjTot <- rowSums (vamp[, 2:25], na.rm = TRUE)
vamp$vamTot <- vamp$adjTot - vamp$All.incomes.not.stated - vamp$Not.applicable - vamp$Negative.income - vamp$Nil.income
vamp$aInc   <- compute_weighted_income(vamp)

bands        <- compute_income_quantile_bands(vamp$aInc, v_range)
vamp$INCqNam <- bands$name
vamp$INCqBot <- bands$bottom
vamp$INCqTop <- bands$top
vamp$INC     <- bands$score
vamp$INC[vamp$vamTot == 0] <- NA

write.csv ( vamp , file = paste0 ( results_dir  ,  "/d_VAMPIRE_Income_" , year , ".csv" ) , row.names = FALSE )
save_vampire_map(
  post, vamp, "INC", year,
  paste0( results_dir , "/" , "p_VAMPIRE_Income_", year , ".png"),
  width, height
)

inc <- rename_component_cols( vamp , names(vamp)[2:(ncol(vamp)-1)] , "inc_" )
#--

###--5. Composite
vamp     <- Reduce(function(x, y) merge(x, y, by = "GEO"), list(mv, jtw, ten, inc))
vamp$VAM <- vamp$MV + vamp$JTW + vamp$TEN + vamp$INC

write.csv ( vamp , paste0( results_dir, "/d_VAMPIRE_ALLcomponents_", year, ".csv" ) , row.names = FALSE)

save_vampire_map(
  post, vamp, "VAM", year,
  paste0( results_dir , "/" , "p_VAMPIRE_Composite_", year , ".png"),
  width, height, fill_limits = c(0, 20)
)
#--
####-----END VAMPIRE--------------------