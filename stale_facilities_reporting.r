# stale_facilities_reporting.r
#
# Renders HTML reports for facilities that are in index.json but NOT in the
# latest detention stats xlsx ("stale" facilities). Companion to
# stale_facilities_charts.r — run that first so the charts exist.
#
# How it differs from facilities_reporting.r:
#   - facility list comes from index.json minus the latest release
#   - each facility's interval row comes from a purpose-built
#     IntervalAdpForStaleFacilities_*.xlsx (its own last pull date, not the latest)
#   - each facility's metadata comes from the OLDER release it last appeared in,
#     passed to the qmd via the meta_xlsx param
#   - as_of_date shown in the report is the facility's own last-seen date
#
# Usage:  source("stale_facilities_reporting.r")

library(tidyverse)
library(lubridate)
library(jsonlite)
library(openxlsx)
library(readxl)
library(janitor)
library(quarto)

# ---- Interval data for ALL dates (grouped_selected) ---------------------------
# Individual_Internal_ADP.r builds grouped_selected (every facility x every pull
# date, with the subpopulation back_interval_adp columns the qmd needs).
if (!exists("grouped_selected")) source("Individual_Internal_ADP.r")

# ---- Identify stale facilities ------------------------------------------------
touchpoint_date <- max(grouped_selected$`Pull Date`, na.rm = TRUE)

index_detlocs <- read_json("index.json", simplifyVector = TRUE) |>
  filter(DETLOC != "NOTES") |>
  pull(DETLOC) |>
  unique()

current_detlocs <- grouped_selected |>
  filter(`Pull Date` == touchpoint_date, !is.na(DETLOC)) |>
  pull(DETLOC) |>
  unique()

stale_detlocs <- setdiff(index_detlocs, current_detlocs)

# Optional: set only_detlocs <- c("BOPGUA", "KANHOLD", ...) before sourcing this
# script to render just those facilities instead of the full stale set.
if (exists("only_detlocs")) {
  not_stale <- setdiff(only_detlocs, stale_detlocs)
  if (length(not_stale) > 0) {
    message("NOTE: not in the stale set (already current or not in index.json): ", paste(not_stale, collapse = ", "))
  }
  stale_detlocs <- intersect(stale_detlocs, only_detlocs)
}

message(">>> ", length(stale_detlocs), " stale facilities to render this run")

# ---- Build the stale interval xlsx (each facility at its OWN last pull date) ----
operator_info <- openxlsx::read.xlsx("./ICE_Facility_Owner&Operator.xlsx") |>
  select(DETLOC, Owner, Operator, Institutional.Type)

stale_interval <- grouped_selected |>
  filter(DETLOC %in% stale_detlocs) |>
  group_by(DETLOC) |>
  filter(`Pull Date` == max(`Pull Date`, na.rm = TRUE)) |>
  slice(1) |>
  ungroup() |>
  left_join(operator_info, by = "DETLOC")

stale_interval_xlsx <- paste0("IntervalAdpForStaleFacilities_",
                              format(touchpoint_date, "%Y-%m-%d"), ".xlsx")
write.xlsx(stale_interval, file = stale_interval_xlsx)
message(">>> Wrote ", stale_interval_xlsx, " (", nrow(stale_interval), " rows)")

# ---- Map each pull date to the xlsx file it came from --------------------------
# Same date-extraction logic as interval_adp.r's read_all_sheets().
extract_pull_date <- function(file) {
  sheets <- excel_sheets(file)
  sheet  <- sheets[grepl("Facilities", sheets, ignore.case = TRUE)][1]
  if (is.na(sheet)) return(NA)
  df <- read_excel(file, sheet = sheet, n_max = 10, col_names = FALSE)
  raw <- str_extract(as.character(df[[1]]), "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}")
  raw <- raw[!is.na(raw)]
  if (length(raw) == 0) return(NA)
  mdy(raw[1])
}

det_files <- list.files("./DetentionFacilities", pattern = "\\.xlsx$", full.names = TRUE)
file_dates <- tibble(file = det_files) |>
  mutate(pull_date = as.Date(vapply(file, function(f) as.character(extract_pull_date(f)), character(1)))) |>
  filter(!is.na(pull_date))

# ---- Look up each stale facility's exact Name in its source release ------------
# params$Name must match the raw (ALL CAPS) Name in the meta xlsx exactly.
read_release_names <- function(file) {
  sheets <- excel_sheets(file)
  sheet  <- sheets[grepl("Facilities", sheets, ignore.case = TRUE)][1]
  df <- read_excel(file, sheet = sheet, col_names = FALSE)
  header_row <- which(df[[1]] == "Name")[1]
  df |>
    row_to_names(row_number = header_row) |>
    pull(Name)
}

coords <- read.csv("./src/facility_locations.csv") |>
  select(Name, Geocodio.Longitude, Geocodio.Latitude) |>
  mutate(Name = str_to_upper(trimws(Name))) |>
  rename(Longitude = Geocodio.Longitude, Latitude = Geocodio.Latitude)

# plots_folder for the qmd image paths — stale charts were saved into the CURRENT folder
if (!exists("plots_folder")) {
  plots_folder <- paste0("plots", format(touchpoint_date, "%Y_%m_%d"))
}

release_names_cache <- list()

stale_params <- stale_interval |>
  select(DETLOC, Name, `Pull Date`) |>
  pmap(function(DETLOC, Name, `Pull Date`) {
    own_date  <- `Pull Date`
    meta_file <- file_dates |> filter(pull_date == own_date) |> pull(file) |> first()
    if (length(meta_file) == 0 || is.na(meta_file)) {
      message("SKIP ", DETLOC, " — no xlsx found for pull date ", own_date)
      return(NULL)
    }

    if (is.null(release_names_cache[[meta_file]])) {
      release_names_cache[[meta_file]] <<- read_release_names(meta_file)
    }
    raw_names  <- release_names_cache[[meta_file]]
    exact_name <- raw_names[toupper(trimws(raw_names)) == toupper(trimws(Name))][1]
    if (is.na(exact_name)) {
      message("SKIP ", DETLOC, " — Name '", Name, "' not found in ", basename(meta_file))
      return(NULL)
    }

    coord_row <- coords |> filter(Name == toupper(trimws(exact_name)))
    if (nrow(coord_row) == 0 || is.na(coord_row$Latitude[1])) {
      message("SKIP ", DETLOC, " — no coordinates in facility_locations.csv for '", exact_name, "'")
      return(NULL)
    }

    list(
      Name          = exact_name,
      DETLOC        = DETLOC,
      Latitude      = as.numeric(coord_row$Latitude[1]),
      Longitude     = as.numeric(coord_row$Longitude[1]),
      plots_folder  = plots_folder,
      as_of_date    = format(own_date, "%B %d, %Y"),
      interval_xlsx = stale_interval_xlsx,
      meta_xlsx     = meta_file
    )
  }) |>
  compact()

message(">>> HTML reports will be generated for ", length(stale_params),
        " of ", length(stale_detlocs), " stale facilities.")
dir.create("html", showWarnings = FALSE)

# ---- Render -------------------------------------------------------------------
walk(stale_params, function(p) {
  output_file <- paste0(p$DETLOC, ".html")
  message("=== Rendering ", output_file, " (", p$Name, ", as of ", p$as_of_date, ") ===")
  tryCatch({
    quarto_render("ice_detention_reporting.qmd",
                  execute_params = p,
                  output_file    = output_file)
    file.rename(output_file, file.path("html", output_file))
  }, error = function(e) message("ERROR rendering ", output_file, ": ", e$message))
})

message("=== Stale facility reporting complete ===")
