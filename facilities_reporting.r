##Validation
library(openxlsx)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
library(ggplot2)
library(quarto)
library(stringr)
library(purrr)
library(readxl)

# Auto-detect latest xlsx
file_list <- list.files("./DetentionFacilities", pattern = "\\.xlsx$", full.names = TRUE)
path <- file_list[which.max(file.mtime(file_list))]
message("Using data file: ", basename(path))

# Auto-detect Facilities sheet
all_sheets <- openxlsx::getSheetNames(path)
sheet_name <- all_sheets[grepl("Facilities", all_sheets, ignore.case = TRUE)][1]
df <- openxlsx::read.xlsx(path, sheet = sheet_name)

# Load location CSV (stable name)
locations_path <- "./src/facility_locations.csv"
fy26_fac_locations <- read.csv(locations_path)
coords <- fy26_fac_locations |>
  select(c(Name, Geocodio.Longitude, Geocodio.Latitude)) |>
  mutate(Name = str_to_upper(Name)) |>
  rename(Longitude = Geocodio.Longitude,
         Latitude  = Geocodio.Latitude)

detloc_ref <- read.csv("detloc_2025-04-13.csv") |>
  select(DETLOC, Name) |>
  distinct(Name, .keep_all = TRUE)

# Fallback DETLOC source for facilities not found in detloc_2025-04-13.csv
operator_detloc_ref <- openxlsx::read.xlsx("./ICE_Facility_Owner&Operator.xlsx") |>
  select(DETLOC, Name) |>
  distinct(Name, .keep_all = TRUE)


# Identify facilities
facilities <- df |>
  row_to_names(row_number = 6) |>
  #filter(!is.na(`Level A`)) |>
  arrange(Name) |>
  select(Name) |>
  filter(Name != "CLINTON COUNTY JAIL") #%>%
  #filter(Name=="CORR. CTR OF NORTHWEST OHIO")
  # Clinton County Jail is excluded here and rendered separately, near the end of this
  # script ("CLINTON COUNTY JAIL — rendered separately"), since its Name collides across states.

facilities_final <- facilities |>
  left_join(coords, by = c("Name")) |>
  filter(!is.na(Latitude)) |>
  left_join(detloc_ref, by = "Name") |>
  left_join(operator_detloc_ref, by = "Name", suffix = c("", ".operator")) |>
  mutate(DETLOC = coalesce(DETLOC, DETLOC.operator)) |>
  select(-DETLOC.operator)

look_for_these <- facilities |>
  anti_join(facilities_final, by = "Name")

if (nrow(look_for_these) > 0) {
  message("\n========================================")
  message("MISSING COORDINATES — ", nrow(look_for_these), " facility(ies) will be SKIPPED (not in facility_locations.csv):")
  for (nm in look_for_these$Name) message("  - ", nm)
  message("Add lat/long for these facilities to: ", locations_path)
  message("========================================\n")
}

missing_detloc <- facilities_final |>
  filter(is.na(DETLOC))

if (nrow(missing_detloc) > 0) {
  message("\n========================================")
  message("MISSING DETLOC — ", nrow(missing_detloc), " facility(ies) have no match in detloc_2025-04-13.csv OR ICE_Facility_Owner&Operator.xlsx:")
  for (nm in missing_detloc$Name) message("  - ", nm)
  message("These will error out in quarto_render(). Add a case_when override or fix the Name in one of those reference files.")
  message("========================================\n")
}

facilities_final <- facilities_final |>
  mutate(Latitude  = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Auto-detect interval ADP xlsx
# Excludes "_v2_for_mandatory" files: those are written by MandatoryDetentionNumber.r
# (which runs after Individual_Internal_ADP.r in run_pipeline.r) and lack the
# Owner/Operator/Institutional.Type columns the report needs, so if picked here every
# facility's report would show "Not Available" for those fields. mtime ordering alone
# isn't reliable to distinguish them since both files can land on the same mtime.
interval_files <- list.files(".", pattern = "IntervalAdpForIndividualColumn.*\\.xlsx$", full.names = TRUE)
interval_files <- interval_files[!grepl("_for_mandatory", interval_files)]
interval_xlsx  <- interval_files[which.max(file.mtime(interval_files))]

# Use globals from run_pipeline.r if available; otherwise derive from interval xlsx filename
if (!exists("plots_folder")) {
  interval_date <- str_extract(basename(interval_xlsx), "\\d{4}-\\d{2}-\\d{2}")
  plots_folder  <- paste0("plots", format(as.Date(interval_date), "%Y_%m_%d"))
  as_of_date    <- format(as.Date(interval_date), "%B %d, %Y")
}

# Pass date/path params to QMD via facilities_final columns
facilities_final <- facilities_final |>
  mutate(
    plots_folder  = plots_folder,
    as_of_date    = as_of_date,
    interval_xlsx = interval_xlsx
  )

# facilities_final <- facilities_final %>% 
#   slice(180:n())
params_list <- pmap(facilities_final, list)

# Queue reports
message(">>> HTML reports will be generated for ", nrow(facilities_final), " facilities.")
dir.create("html", showWarnings = FALSE)

########## ALL FACILITIES — comment out when running new contracts only ##########
reports <-
  tibble(
    input          = rep("ice_detention_reporting.qmd", nrow(facilities_final)),
    execute_params = params_list,
    output_file    = str_glue("{facilities_final$DETLOC}.html")
  )

pwalk(reports, function(input, execute_params, output_file) {
  quarto_render(input = input, execute_params = execute_params, output_file = output_file)
  file.rename(output_file, file.path("html", output_file))
})
########## END ALL FACILITIES ##########


########## CLINTON COUNTY JAIL — rendered separately ##########
# "CLINTON COUNTY JAIL" is shared by facilities in different states (NY/Plattsburgh, IN/Frankfort,
# and historically IA), so a Name-only join against coords/detloc fans out or grabs the wrong
# DETLOC. It's excluded from `facilities` above (line ~48) for that reason; here it's resolved by
# Name + City instead, so it no longer requires manual editing to include.
clinton_facilities <- df |>
  row_to_names(row_number = 6) |>
  filter(Name == "CLINTON COUNTY JAIL") |>
  distinct(Name, City)

if (nrow(clinton_facilities) > 0) {

  clinton_coords <- fy26_fac_locations |>
    select(c(Name, City, Geocodio.Longitude, Geocodio.Latitude)) |>
    mutate(Name = str_to_upper(Name), City = str_to_upper(City)) |>
    rename(Longitude = Geocodio.Longitude, Latitude = Geocodio.Latitude)

  clinton_detloc_ref <- read.csv("detloc_2025-04-13.csv") |>
    select(DETLOC, Name, City) |>
    mutate(Name = str_to_upper(Name), City = str_to_upper(City)) |>
    distinct(Name, City, .keep_all = TRUE)

  clinton_final <- clinton_facilities |>
    mutate(City = str_to_upper(City)) |>
    left_join(clinton_coords, by = c("Name", "City")) |>
    filter(!is.na(Latitude)) |>
    left_join(clinton_detloc_ref, by = c("Name", "City")) |>
    mutate(Latitude  = as.numeric(Latitude),
           Longitude = as.numeric(Longitude),
           plots_folder  = plots_folder,
           as_of_date    = as_of_date,
           interval_xlsx = interval_xlsx) |>
    select(-City)

  if (any(is.na(clinton_final$DETLOC))) {
    message("CLINTON COUNTY JAIL: could not resolve DETLOC for ", sum(is.na(clinton_final$DETLOC)),
            " location(s) — check the City spelling against detloc_2025-04-13.csv")
  }

  clinton_params <- pmap(clinton_final, list)

  clinton_reports <- tibble(
    input          = rep("ice_detention_reporting.qmd", nrow(clinton_final)),
    execute_params = clinton_params,
    output_file    = str_glue("{clinton_final$DETLOC}.html")
  )

  pwalk(clinton_reports, function(input, execute_params, output_file) {
    quarto_render(input = input, execute_params = execute_params, output_file = output_file)
    file.rename(output_file, file.path("html", output_file))
  })

  message(">>> CLINTON COUNTY JAIL: rendered ", nrow(clinton_final), " HTML report(s).")
}
########## END CLINTON COUNTY JAIL ##########


########## NEW CONTRACTS ONLY — comment out block above and uncomment this when running new contracts ##########
# new_contracts_detloc <- read.csv("facility_with_new_contracts final.csv")
# 
# facilities_new_only <- facilities_final |>
#   filter(DETLOC %in% new_contracts_detloc$name)
# 
# params_new_only <- pmap(facilities_new_only, list)
# 
# reports_new_only <- tibble(
#   input          = rep("ice_detention_reporting.qmd", nrow(facilities_new_only)),
#   execute_params = params_new_only,
#   output_file    = str_glue("{facilities_new_only$DETLOC}.html")
# )
# 
# pwalk(reports_new_only, function(input, execute_params, output_file) {
#   quarto_render(input = input, execute_params = execute_params, output_file = output_file)
#   file.rename(output_file, file.path("html", output_file))
#})
######### END NEW CONTRACTS ONLY ##########


####### for coordinates.json file ###############
# path <- "C:/Users/acer/Desktop/detention report automation/DetentionFacilities/FY26_detentionStats04092026.xlsx"
# df <- openxlsx::read.xlsx(path, sheet = sheet_name)
# facilities <- df |>
#   row_to_names(row_number = 6) |>
#   arrange(Name) |>
#   select(Name,City,State)
# dim(facilities)
# coords <- fy26_fac_locations |>
#   select(c(Name, Geocodio.Longitude, Geocodio.Latitude,City,State))|>
#   mutate(Name = str_to_upper(Name))|>
#   rename(Longitude = Geocodio.Longitude,
#          Latitude = Geocodio.Latitude)
# coordinates <- facilities |>
#   left_join(coords, by=c("Name","City","State"))
# dim(coordinates)
# 
# jsonlite::write_json(
#   coordinates,
#   "CountyCoordinatestest.json",
#   pretty = TRUE,
#   auto_unbox = TRUE
# )

