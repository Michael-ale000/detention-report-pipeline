# stale_facilities_charts.r
#
# Generates interval ADP bar charts, ALOS scatter plots, and mandatory detention
# stacked charts for facilities that are in index.json but NOT in the latest
# detention stats xlsx ("stale" facilities — ones that dropped out of recent
# releases but still have pages on detentionreports.com).
#
# The regular chartmakers only cover facilities present at touchpoint_date; this
# script covers the remainder. Each stale facility's charts use its OWN most
# recent pull date (from whichever older xlsx it last appeared in), since `final`
# holds the full history of every facility across all files in ./DetentionFacilities.
#
# Usage (after interval_adp_chartmaker.r / run_pipeline.r, so globals exist):
#   source("stale_facilities_charts.r")
# Standalone also works — it sources what it needs.

library(tidyverse)   # dplyr/ggplot2/stringr etc. for the one_facility_* helpers
library(lubridate)   # month()/year() in one_facility_table.r
library(jsonlite)
library(readxl)
library(openxlsx)
library(extrafont)
library(cowplot)
library(magick)

# ---- Ensure pipeline data + globals exist ------------------------------------
if (!exists("final")) source("interval_adp.r")
source("one_facility_plot.r")
source("one_facility_table.r")
source("one_facility_scatter_plot.r")
source("one_facility_mandatory_stacked_plot.r")

if (!exists("touchpoint_date")) {
  touchpoint_date <- max(final$`Pull Date`, na.rm = TRUE)
  as_of_date      <- format(touchpoint_date, "%B %d, %Y")
  plots_folder    <- paste0("plots", format(touchpoint_date, "%Y_%m_%d"))
}
dir.create(plots_folder, showWarnings = FALSE)
dir.create("plots7", showWarnings = FALSE)
mandatory_plots_folder <- paste0("mandatory_plots", format(touchpoint_date, "%Y_%m_%d"))
dir.create(mandatory_plots_folder, showWarnings = FALSE)

# ---- Identify stale facilities: in index.json but not in the latest release ----
index_detlocs <- jsonlite::read_json("index.json", simplifyVector = TRUE) |>
  filter(DETLOC != "NOTES") |>
  pull(DETLOC) |>
  unique()

current_detlocs <- final |>
  filter(`Pull Date` == touchpoint_date, !is.na(DETLOC)) |>
  pull(DETLOC) |>
  unique()

stale_detlocs <- setdiff(index_detlocs, current_detlocs)

# Facilities in index.json with no rows in `final` at all can't be charted from here
no_data_detlocs <- setdiff(stale_detlocs, unique(final$DETLOC))
stale_detlocs   <- setdiff(stale_detlocs, no_data_detlocs)

message("=== Stale facility chartmaker ===")
message("index.json facilities:      ", length(index_detlocs))
message("in latest release:          ", length(current_detlocs))
message("stale (chartable):          ", length(stale_detlocs))
if (length(no_data_detlocs) > 0) {
  message("NO DATA in any xlsx (skipped): ", paste(no_data_detlocs, collapse = ", "))
}

# ---- Mandatory data (optional — skipped if the xlsx isn't there) --------------
mand_file <- list.files(".", pattern = "^mandatory detention.*\\.xlsx$", full.names = TRUE)
mandatory_data <- NULL
if (length(mand_file) > 0) {
  mand_file <- mand_file[order(file.mtime(mand_file), decreasing = TRUE)][1]
  mandatory_data <- read_excel(mand_file)
  mandatory_data$`Pull Date` <- as.Date(mandatory_data$`Pull Date`)
} else {
  message("Mandatory xlsx not found — mandatory charts will be skipped. Run MandatoryDetentionNumber.r first to include them.")
}

total_adp_data <- final |>
  select(DETLOC, `Pull Date`, back_interval_adp, adp) |>
  mutate(`Pull Date` = as.Date(`Pull Date`))

# ---- Helpers shared with interval_adp_chartmaker.r ----------------------------
get_legend_pos <- function(code) {
  cfg <- read.csv("legend_config.csv", stringsAsFactors = FALSE)
  row <- cfg[cfg$DETLOC == code, ]
  if (nrow(row) == 0) return(list(x = 0.1, y = 0.9))
  list(x = row$x_pos[1], y = row$y_pos[1])
}

# ---- Generate all three chart types per stale facility ------------------------
for (code in stale_detlocs) {

  facility_tbl <- tryCatch(
    one_facility_table(code) |> filter(!is.na(adp)),
    error = function(e) NULL
  )
  if (is.null(facility_tbl) || nrow(facility_tbl) == 0) {
    message("SKIP ", code, " — no usable rows in final")
    next
  }

  facility_name <- as.character(facility_tbl$Name[1])
  own_date      <- max(facility_tbl$Pull.Date, na.rm = TRUE)
  own_as_of     <- format(own_date, "%B %d, %Y")

  message("--- ", code, " (", facility_name, ") — last seen ", own_as_of, " ---")

  # 1. Interval ADP bar chart (same prep as plot_all in interval_adp_chartmaker.r)
  facility_max <- as.numeric(round(max(facility_tbl$adp, facility_tbl$back_interval_adp, na.rm = TRUE) + 50, -2))
  if (!is.finite(facility_max)) facility_max <- 200

  latest_row <- facility_tbl |>
    filter(Pull.Date == own_date) |>
    mutate(adp = ifelse(is.finite(adp), adp, 0),
           back_interval_adp = ifelse(is.finite(back_interval_adp), back_interval_adp, 0))
  date_adp          <- as.character(round(latest_row$adp[1], 0))
  date_interval_adp <- as.character(round(latest_row$back_interval_adp[1], 0))

  lp <- get_legend_pos(code)
  tryCatch(
    one_facility_plot(facility_tbl |> replace(is.na(facility_tbl), 0),
                      own_as_of, facility_name, code, date_adp,
                      date_interval_adp, facility_max, lp$x, lp$y),
    error = function(e) message("  ERROR bar chart for ", code, ": ", e$message)
  )

  # 2. ALOS scatter — compared against all facilities on the stale facility's own date
  tryCatch(
    one_facility_scatter_plot(final, own_as_of, facility_name, code,
                              snapshot_date = own_date),
    error = function(e) message("  ERROR scatter for ", code, ": ", e$message)
  )

  # 3. Mandatory stacked chart — only if the facility ever reported mandatory > 0
  if (!is.null(mandatory_data)) {
    has_mandatory <- mandatory_data |>
      filter(DETLOC == code, !is.na(Mandatory), Mandatory > 0) |>
      nrow() > 0

    if (has_mandatory) {
      stacked_tbl <- tryCatch(
        one_facility_mandatory_stacked_table(code, mandatory_data, total_adp_data),
        error = function(e) { message("  ERROR mandatory table for ", code, ": ", e$message); NULL }
      )
      if (!is.null(stacked_tbl) && nrow(stacked_tbl) > 0) {
        s_latest   <- stacked_tbl |> filter(Pull.Date == max(Pull.Date))
        s_interval <- as.character(round(s_latest$back_interval_adp[1], 0))
        s_max_y    <- max(100, round(max(stacked_tbl$back_interval_adp, stacked_tbl$adp, na.rm = TRUE) + 50, -2))
        # subtitle date = the facility's last date in the mandatory data (may differ from own_date)
        s_as_of <- format(max(stacked_tbl$Pull.Date, na.rm = TRUE), "%B %d, %Y")
        tryCatch(
          one_facility_mandatory_stacked_plot(df = stacked_tbl, name = facility_name,
                                              code = code, interval_adp = s_interval,
                                              max_y = s_max_y, as_of = s_as_of),
          error = function(e) message("  ERROR mandatory chart for ", code, ": ", e$message)
        )
      }
    } else {
      message("  no mandatory detention reported — mandatory chart skipped")
    }
  }
}

message("=== Stale facility chartmaker complete: ", length(stale_detlocs), " facilities processed ===")
