# mandatory_chartmaker.r
#
# Generates mandatory detention interval ADP charts for every active facility.
# Mirrors the structure of interval_adp_chartmaker.r.
#
# Usage (standalone):
#   1. Run MandatoryDetentionNumber.r first to produce:
#        "mandatory detention with interval adp calculation for all detention data.xlsx"
#   2. Then run:
#        source("mandatory_chartmaker.r")
#
# Usage (via run_pipeline.r):
#   Source this file after MandatoryDetentionNumber.r has run.  The globals
#   touchpoint_date and as_of_date are already set by run_pipeline.r.

library(tidyverse)
library(ggplot2)
library(readxl)
library(extrafont)
library(cowplot)
library(magick)
library(scales)
library(lubridate)

source("one_facility_mandatory_stacked_plot.r")
# source("one_facility_mandatory_stacked_plot_v2.r")
# source("one_facility_mandatory_stacked_plot_v3.r")

# ---- Locate the mandatory xlsx -----------------------------------------------
mand_file <- list.files(
  ".",
  pattern    = "^mandatory detention.*\\.xlsx$",
  full.names = TRUE
)
if (length(mand_file) == 0) {
  stop(
    "Mandatory detention file not found.\n",
    "Run MandatoryDetentionNumber.r first to produce:\n",
    "  'mandatory detention with interval adp calculation for all detention data.xlsx'"
  )
}
# Use the most recently modified file if there are multiple
mand_file <- mand_file[order(file.mtime(mand_file), decreasing = TRUE)][1]
message("Reading: ", mand_file)

mandatory_data <- read_excel(mand_file)
mandatory_data$`Pull Date` <- as.Date(mandatory_data$`Pull Date`)

# ---- Load total interval ADP data (denominator for % chart) ------------------
# Uses all_merged_file.xlsx (written by interval_adp_chartmaker.r) or `final` if in memory.
if (exists("final")) {
  total_adp_data <- final |>
    select(DETLOC, `Pull Date`, back_interval_adp, adp) |>
    mutate(`Pull Date` = as.Date(`Pull Date`))
} else if (file.exists("all_merged_file.xlsx")) {
  total_adp_data <- read_excel("all_merged_file.xlsx") |>
    select(DETLOC, `Pull Date`, back_interval_adp, adp) |>
    mutate(`Pull Date` = as.Date(`Pull Date`))
} else {
  warning("all_merged_file.xlsx not found — % line charts will be skipped.\n",
          "Run interval_adp_chartmaker.r first to generate it.")
  total_adp_data <- NULL
}

# ---- Set globals if running standalone (not via run_pipeline.r) --------------
if (!exists("touchpoint_date")) {
  touchpoint_date <- max(mandatory_data$`Pull Date`, na.rm = TRUE)
  as_of_date      <- format(touchpoint_date, "%B %d, %Y")
}

mandatory_plots_folder <- paste0("mandatory_plots", format(touchpoint_date, "%Y_%m_%d"))
dir.create(mandatory_plots_folder, showWarnings = FALSE)

message("=== Mandatory Chartmaker running for ", as_of_date, " ===")
message("    Output folder: ", mandatory_plots_folder)
# ---- Facilities active at touchpoint with non-zero mandatory detention --------
active_facilities <- mandatory_data |>
  filter(
    `Pull Date` == touchpoint_date,
    !is.na(DETLOC),
    !is.na(Mandatory),
    Mandatory > 0
  ) |>
  distinct(Name, DETLOC)

message("=== ", nrow(active_facilities), " facilities to plot ===")

# ---- Loop over each facility -------------------------------------------------
for (i in seq_len(nrow(active_facilities))) {
  code <- active_facilities$DETLOC[i]
  name <- active_facilities$Name[i]
  
  if (is.null(total_adp_data)) next
  
  message("  Plotting ", code, " (", name, ")")
  
  stacked_tbl <- tryCatch(
    one_facility_mandatory_stacked_table(code, mandatory_data, total_adp_data),
    error = function(e) {
      message("  ERROR building stacked table for ", code, ": ", e$message)
      NULL
    }
  )
  if (is.null(stacked_tbl) || nrow(stacked_tbl) == 0) next
  
  s_latest   <- stacked_tbl |> filter(Pull.Date == max(Pull.Date))
  s_interval <- as.character(round(s_latest$back_interval_adp[1], 0))
  s_max_y <- max(100, round(max(stacked_tbl$back_interval_adp, stacked_tbl$adp, na.rm = TRUE) + 50, -2))
  
  tryCatch(
    one_facility_mandatory_stacked_plot(
      df           = stacked_tbl,
      name         = name,
      code         = code,
      interval_adp = s_interval,
      max_y        = s_max_y
    ),
    error = function(e) message("  ERROR plotting stacked for ", code, ": ", e$message)
  )
  
  # stacked_tbl_v2 <- tryCatch(
  #   one_facility_mandatory_stacked_table_v2(code, mandatory_data, total_adp_data),
  #   error = function(e) {
  #     message("  ERROR building stacked v2 table for ", code, ": ", e$message)
  #     NULL
  #   }
  # )
  # if (!is.null(stacked_tbl_v2) && nrow(stacked_tbl_v2) > 0) {
  #   tryCatch(
  #     one_facility_mandatory_stacked_plot_v2(
  #       df           = stacked_tbl_v2,
  #       name         = name,
  #       code         = code,
  #       interval_adp = s_interval,
  #       max_y        = s_max_y
  #     ),
  #     error = function(e) message("  ERROR plotting stacked v2 for ", code, ": ", e$message)
  #   )
  # }
  
  # stacked_tbl_v3 <- tryCatch(
  #   one_facility_mandatory_stacked_table_v3(code, mandatory_data, total_adp_data),
  #   error = function(e) {
  #     message("  ERROR building stacked v3 table for ", code, ": ", e$message)
  #     NULL
  #   }
  # )
  # if (!is.null(stacked_tbl_v3) && nrow(stacked_tbl_v3) > 0) {
  #   tryCatch(
  #     one_facility_mandatory_stacked_plot_v3(
  #       df           = stacked_tbl_v3,
  #       name         = name,
  #       code         = code,
  #       interval_adp = s_interval,
  #       max_y        = s_max_y
  #     ),
  #     error = function(e) message("  ERROR plotting stacked v3 for ", code, ": ", e$message)
  #   )
  # }
}

message("=== Mandatory Chartmaker complete. Plots saved to: ", mandatory_plots_folder, " ===")
