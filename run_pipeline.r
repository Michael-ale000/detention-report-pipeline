# run_pipeline.r — Single entry point for the full detention report pipeline.
#
# Usage: Open ice_detention_reports.Rproj in RStudio, drop the new xlsx into
#        ./DetentionFacilities/, then run:
#
#   source("run_pipeline.r")
#
# Steps:
#   1. Load data + derive all date globals from the latest xlsx
#   2. Individual subpopulation ADP  -> IntervalAdpForIndividualColumn_DATE_v1.xlsx
#   3. Interval ADP charts           -> plots{YYYY_MM_DD}/ and plots7/
#   4. Render HTML reports           -> outputs/

# ---- Step 1: Load core data and derive date globals ----
source("interval_adp.r")   # produces: final, latest_date, all_clean, output_clean

touchpoint_date <- latest_date
as_of_date      <- format(latest_date, "%B %d, %Y")
plots_folder    <- paste0("plots", format(latest_date, "%Y_%m_%d"))
dir.create(plots_folder, showWarnings = FALSE)
dir.create("plots7",     showWarnings = FALSE)

message("=== Pipeline running for ", as_of_date, " ===")

# ---- Step 2: Individual subpopulation ADP ----
source("Individual_Internal_ADP.r")

# ---- Step 3: Charts ----
source("interval_adp_chartmaker.r")  # reads plots_folder, as_of_date, touchpoint_date from globals
source("MandatoryDetentionNumber.r")
source("mandatory_chartmaker.r")

# ---- Step 4: Render HTML reports ----
source("facilities_reporting.r")     # reads plots_folder, as_of_date from globals

message("=== Pipeline complete for ", as_of_date, " ===")
