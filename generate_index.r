# generate_index.r
# Produces index.json for detentionreports.com
#
# Fields: Name, City, State, Zip, DETLOC, filename, LatestUpdate, CurrentIntervalADP
#
# Usage (standalone):   source("generate_index.r")
# Usage (after pipeline): already has `final` in env from run_pipeline.r, so just source this.
#
# Output: index.json in the project root (copy to param-reporting repo as needed)

library(dplyr)
library(stringr)
library(purrr)
library(jsonlite)

# ---- Load data if not already in environment ----
if (!exists("final")) {
  message("'final' not found — sourcing interval_adp.r ...")
  source("interval_adp.r")
}

# ---- Build facility index entirely from final ----
facility_index <- final |>
  filter(!is.na(DETLOC)) |>
  group_by(DETLOC) |>
  filter(`Pull Date` == max(`Pull Date`, na.rm = TRUE)) |>
  slice_max(order_by = back_interval_adp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    Name               = toupper(Name),
    City               = City,
    State              = State,
    Zip                = Zip,
    DETLOC             = DETLOC,
    filename           = paste0(str_replace_all(str_replace_all(toupper(Name), "/", "-"), " ", "_"), ".html"),
    LatestUpdate       = format(`Pull Date`, "%Y-%m-%d"),
    CurrentIntervalADP = round(back_interval_adp)
  ) |>
  filter(!is.na(CurrentIntervalADP)) |>
  arrange(Name)

# ---- Keep only facilities in old_index OR active at touchpoint_date ----
old_index_base <- jsonlite::read_json("old_index.json", simplifyVector = TRUE) |> filter(DETLOC != "NOTES")
facility_index <- facility_index |>
  filter(DETLOC %in% old_index_base$DETLOC | LatestUpdate == format(touchpoint_date, "%Y-%m-%d"))

# ---- Prepend the static notes entry ----
notes_entry <- list(
  Name               = "1 - NOTES ON DATA PROCESSING",
  City               = "Notes",
  State              = "US",
  Zip                = "00000",
  DETLOC             = "NOTES",
  filename           = "notes.html",
  LatestUpdate       = NA_character_,
  CurrentIntervalADP = NA_integer_
)

index_list    <- c(list(notes_entry), pmap(facility_index, list))
index_df      <- bind_rows(notes_entry, facility_index)
duplicate_df  <- index_df |> group_by(Name) |> filter(n() > 1) |> mutate(n_occurrences = n()) |> ungroup() |> arrange(Name)

new_only_df   <- facility_index |> filter(!DETLOC %in% old_index_base$DETLOC, LatestUpdate == format(touchpoint_date, "%Y-%m-%d"))
#count <- index_df %>% filter(LatestUpdate=="2026-04-02")
# ---- Write JSON ----
output_path <- "index.json"

write_json(
  index_list,
  output_path,
  pretty      = 4,
  auto_unbox  = TRUE,
  na          = "null"
)

message(
  "index.json written: ", nrow(facility_index), " facilities + notes entry\n",
  "Output: ", normalizePath(output_path)
)
