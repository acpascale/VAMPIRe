##   [x].r  - VAMPIRE index test bed and generator
##
##   Created:        24 May 2026
##   Updated:        7 June 2026
##   Updated:        16 August 2026 -- moved to POAs
##
##   Notes:
##   1. Following "Vampire Methodology.docx" (Sipe, 2026) with a few changes, see relevant pptx
##   2. Changed names of provided POA data to "ABS_" + name of file with spaces = "_" + year of data
##   3. Claude Sonnet 5 Medium - "Convert code (VAMPIRE_poa.R) to best practice R with function calls"

##   Issues:
##   1. Warnings from variable duplication when matching abs data - NO IMPACT - can modify all headers for abs data before merging, not completed 

##   Sources: 
##   [1] Postcodes and Postal Areas, ABS - https://www.abs.gov.au/websitedbs/censushome.nsf/home/factsheetspoa?opendocument&navpos=450
##   [2] ABS. 2021. “Number of Motor Vehicles (Ranges) (VEHRD).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/transport/number-motor-vehicles-ranges-vehrd.
##   [3] ABS. 2021. “Method of Travel to Work (6 Travel Modes) (MTW06P).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/transport/method-travel-work-6-travel-modes-mtw06p.
##   [4] ABS. 2021. “Tenure Type (TEND).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/housing/tenure-type-tend.
##   [5] ABS. 2021. “Total Household Income as Stated (Weekly) (HINASD).” Australian Bureau of Statistics, October 15. https://www.abs.gov.au/census/guide-census-data/census-dictionary/2021/variables-topic/income-and-work/total-household-income-stated-weekly-hinasd.


## ---- 0. Libraries --------------------------------------------------------
suppressPackageStartupMessages({
  library(openxlsx)  # read.xlsx()
  library(ggplot2)   # geom_sf(), ggsave()
  library(sf)        # st_as_sf()
})

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

#' Bucket a percentage into the 0-5 VAMPIRE score using the configured
#' breakpoints (`v_range` supplies the four interior breaks plus the top
#' threshold for score 5).
vampire_classify <- function(pct, v_range) {
  breaks <- c(-Inf, v_range, Inf)
  score  <- cut(pct, breaks = breaks, labels = 0:5, right = FALSE)
  as.numeric(as.character(score))
}

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

#' Prefix a set of newly-added columns with a component tag (e.g. "mvs_"),
#' matched by name rather than hard-coded position. This avoids the silent
#' mismatches that caused the variable-duplication warnings noted in the
#' original script when columns shifted position.
rename_component_cols <- function(df, cols, prefix) {
  idx <- match(cols, names(df))
  names(df)[idx] <- paste0(prefix, cols)
  df
}

#' Build a standard output path under results/<out_dir>/.
file_out <- function(out_dir, prefix, year, label, ext = "csv") {
  file.path("results", out_dir, paste0(prefix, "_", year, "_", label[1], ".", ext))
}

#' Merge a component's data onto the postcode geometry and save a choropleth.
save_vampire_map <- function(post, df, fill_var, year, out_path, width, height,
                             fill_limits = c(0, 5)) {
  merged <- merge(post, df, by = "POA", all.x = FALSE)
  
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

#' Load the postcode/POA geometry used as the base map for every component.
load_postcode_geometry <- function(data_dir) {
  env <- new.env()
  load(file.path(data_dir, "Adelaide_post_mapserver.rData"), envir = env)
  post <- env$post
  post$geo <- sf::st_as_sf(post$geometry)
  post
}

## ---- 2. Component processors ----------------------------------------------
## Each function loads its ABS table, computes the component score, writes
## the intermediate CSV + map, and returns the data with its working columns
## tagged so it can be merged into the composite index without name clashes.

process_motor_vehicles <- function(data_dir, year, post, v_range, out_dir, label, width, height) {
  file_path <- file.path(data_dir, paste0("ABS_Motor_vehicles_", year, ".xlsx"))
  vamp <- clean_abs_table(file_path)
  
  vamp <- compute_vampire_component(
    vamp,
    total_cols     = 2:8,
    exclude_cols   = c("Not.stated", "Not.applicable"),
    numerator_cols = c("Two.motor.vehicles", "Three.motor.vehicles", "Four.or.more.motor.vehicles")
  )
  vamp$MV <- vampire_classify(vamp$vamPct, v_range)
  vamp$MV[vamp$vamTot == 0] <- NA
  
  write.csv(vamp, file_out(out_dir, "d_VAMPIRE_motorVehicles", year, label), row.names = FALSE)
  save_vampire_map(
    post, vamp, "MV", year,
    file_out(out_dir, "p_VAMPIRE_motorVehicles", year, label, "png"),
    width, height
  )
  
  rename_component_cols(vamp, c("adjTot", "vamTot", "vamPct", "MV"), "mvs_")
}

process_journey_to_work <- function(data_dir, year, post, v_range, out_dir, label, width, height) {
  file_path <- file.path(data_dir, paste0("ABS_Journey_to_Work_", year, ".xlsx"))
  vamp <- clean_abs_table(file_path)
  
  vamp <- compute_vampire_component(
    vamp,
    total_cols     = 2:8,
    exclude_cols   = c("Worked.at.home.or.did.not.go.to.work", "Not.stated", "Not.applicable"),
    numerator_cols = "Vehicle"
  )
  vamp$JTW <- vampire_classify(vamp$vamPct, v_range)
  vamp$JTW[vamp$vamTot == 0] <- NA
  
  write.csv(vamp, file_out(out_dir, "d_VAMPIRE_journeyToWork", year, label), row.names = FALSE)
  save_vampire_map(
    post, vamp, "JTW", year,
    file_out(out_dir, "p_VAMPIRE_journeyToWork", year, label, "png"),
    width, height
  )
  
  rename_component_cols(vamp, c("adjTot", "vamTot", "vamPct", "JTW"), "jtw_")
}

process_tenure <- function(data_dir, year, post, v_range, out_dir, label, width, height) {
  file_path <- file.path(data_dir, paste0("ABS_Tenure_", year, ".xlsx"))
  vamp <- clean_abs_table(file_path)
  
  vamp <- compute_vampire_component(
    vamp,
    total_cols     = 2:10,
    exclude_cols   = c("Not.stated", "Not.applicable"),
    numerator_cols = "Owned.with.a.mortgage"
  )
  vamp$TEN <- vampire_classify(vamp$vamPct, v_range)
  vamp$TEN[vamp$vamTot == 0] <- NA
  
  write.csv(vamp, file_out(out_dir, "d_VAMPIRE_Tenure", year, label), row.names = FALSE)
  save_vampire_map(
    post, vamp, "TEN", year,
    file_out(out_dir, "p_VAMPIRE_Tenure", year, label, "png"),
    width, height
  )
  
  rename_component_cols(vamp, c("adjTot", "vamTot", "vamPct", "TEN"), "ten_")
}

process_income <- function(data_dir, year, post, v_range, out_dir, label, width, height) {
  file_path <- file.path(data_dir, paste0("ABS_Income_", year, ".xlsx"))
  vamp <- clean_abs_table(file_path)
  
  vamp$adjTot <- rowSums(vamp[, 2:25], na.rm = TRUE)
  vamp$vamTot <- vamp$adjTot - vamp$All.incomes.not.stated - vamp$Not.applicable -
    vamp$Negative.income - vamp$Nil.income
  vamp$aInc   <- compute_weighted_income(vamp)
  
  bands <- compute_income_quantile_bands(vamp$aInc, v_range)
  vamp$INC     <- bands$score
  vamp$INCqNam <- bands$name
  vamp$INCqBot <- bands$bottom
  vamp$INCqTop <- bands$top
  vamp$INC[vamp$vamTot == 0] <- NA
  
  write.csv(vamp, file_out(out_dir, "d_VAMPIRE_Income", year, label), row.names = FALSE)
  save_vampire_map(
    post, vamp, "INC", year,
    file_out(out_dir, "p_VAMPIRE_Income", year, label, "png"),
    width, height
  )
  
  rename_component_cols(
    vamp,
    c("adjTot", "vamTot", "aInc", "INC", "INCqNam", "INCqBot", "INCqTop"),
    "inc_"
  )
}

## ---- 3. Orchestration -------------------------------------------------

#' Run all four VAMPIRE components for one year, merge them into the
#' composite index, and save the combined CSV + map.
run_vampire_year <- function(year, data_dir, post, v_range, out_dir, label, width, height) {
  mv  <- process_motor_vehicles(data_dir, year, post, v_range, out_dir, label, width, height)
  jtw <- process_journey_to_work(data_dir, year, post, v_range, out_dir, label, width, height)
  ten <- process_tenure(data_dir, year, post, v_range, out_dir, label, width, height)
  inc <- process_income(data_dir, year, post, v_range, out_dir, label, width, height)
  
  all_components <- Reduce(function(x, y) merge(x, y, by = "POA"), list(mv, jtw, ten, inc))
  all_components$VAM <- all_components$MV + all_components$JTW +
    all_components$TEN + all_components$INC
  
  write.csv(all_components, file_out(out_dir, "d_VAMPIRE_ALLcomponents", year, label),
            row.names = FALSE)
  save_vampire_map(
    post, all_components, "VAM", year,
    file_out(out_dir, "p_VAMPIRE_Composite", year, label, "png"),
    width, height, fill_limits = c(0, 20)
  )
  
  all_components
}

## ---- 4. Main ------------------------------------------------------------

main <- function() {
  config <- list(
    data_dir = file.path("data", "poa2021"),
    years    = c(2021),  # c(2016, 2021, 2026)
    v_range  = c(0.10, 0.25, 0.50, 0.75, 0.90),
    label    = c("updated2026", "e_postcode"),
    out_dir  = "poa2021out",
    width    = 1920,
    height   = 1080
  )
  
  post <- load_postcode_geometry ( config$data_dir )
  
  results <- lapply (
    config$years, run_vampire_year,
    data_dir = config$data_dir, post = post, v_range = config$v_range,
    out_dir = config$out_dir, label = config$label,
    width = config$width, height = config$height
  )
  
  names(results) <- config$years
  invisible(results)
}

## Only auto-run when this file is executed as a script (e.g. via
## `Rscript vampire_index_generator.R` or `source()`), not when its
## functions are sourced into another session for testing/reuse.
if (identical(environment(), globalenv())) {
  main()
}