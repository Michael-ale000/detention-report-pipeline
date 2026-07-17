#calculate total cumulative beds used each data release and find change
library(tidyverse)
library(openxlsx)
library(readxl)
library(janitor)
library(stringr)
library(rlang)
library(rjson)
library(here)
here()


# Define the directory containing the Excel files CHANGE THIS TO WHEREVER YOU HOLD ALL OF THE DETENTION EXCEL FILES
dir_path <- "./DetentionFacilities"


# Get a list of all Excel files in the directory
file_list <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)
print(file_list)


# Function to read all sheets from an Excel file
read_all_sheets <- function(file) {
  target_string <- "Facilities"
  sheet_names <- excel_sheets(file)
  matching_sheets <- sheet_names[grepl(target_string, sheet_names, ignore.case = TRUE)]
  df <- lapply(matching_sheets, function(sheet) read_excel(file, sheet=sheet))[[1]]
  
  date_pattern <- "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}"  # Matches DD-MM-YYYY or DD/MM/YYYY
  
  date <- df |>
    mutate(new = str_extract(df[[1]], date_pattern))|>
    select(new) |>
    filter(!is.na(new)) |>
    pull()
  
  row_num <- df %>%
    mutate(row_number = row_number()) %>%
    filter(.[[1]] == "Name") %>%
    pull(row_number)
  
  clean <- df |>
    row_to_names(row_number = row_num) |>
    select(1:21) |>
    rename_with(~gsub("FY[0-9]+ ALOS", "ALOS", .), everything()) |>
    mutate(`Pull Date` = date) |>
    filter(!is.na(City))
  
  return(clean)
}

# Read all Excel files and their sheets
all_data <- map(file_list, read_all_sheets)

#Combine list of dataframes
output <- bind_rows(all_data)
#write.xlsx(output, "detentionstats_2022-12_2025-08.xlsx")

# Extract the day in the fiscal year
output_clean <- output |>
  mutate(Name = str_to_title(Name),
         Address = str_to_title(Address),
         City = str_to_title(City)) |>
  mutate(across(c(ALOS:`No ICE Threat Level`), as.numeric)) |>
  rowwise()|>
  mutate(adp_sec = sum(across(starts_with("Level")), na.rm = TRUE))|>
  mutate(adp_gen = sum(across(c(`Male Crim`: `Female Non-Crim`)), na.rm = TRUE))|>
  mutate(adp_threat = sum(across(c(`ICE Threat Level 1`: `No ICE Threat Level`)), na.rm = TRUE)) |>
  mutate(adp = rowMeans(across(c(adp_sec, adp_gen, adp_threat)), na.rm = TRUE))|> #2025-08-04 release had no rounding, so we averaged
  mutate(adp = ifelse(adp==0 && is.na(`Male Crim`), NA, adp))|> ##Added this to preserve NAs when blank
  mutate(`Pull Date` = mdy(`Pull Date`)) |>
  mutate(
    pull_year = year(`Pull Date`),
    fiscal_start = if_else(`Pull Date` >= as.Date(paste0(pull_year, "-10-01")),
                           as.Date(paste0(pull_year, "-10-01")),
                           as.Date(paste0(pull_year - 1, "-10-01"))),
    fiscal_year_day = as.numeric(difftime(`Pull Date`, fiscal_start, units = "days")) + 1
  ) |>
  mutate(adp = ifelse(Name == "Elmore County Jail" & `Pull Date` == "2025-08-04", 1, adp))|> #Had an ADP of 0 in the Level Column on this date, but male/female had 1
  mutate(adp = ifelse(Name == "Douglas County" & `Pull Date` == "2025-08-04", 1, adp))|> #Had an ADP of 0 in the Level Column on this date, but male/female had 1
  mutate(cumulative_beds = adp*fiscal_year_day) # Calculate cumulative beds from start of fiscal year

# Check for duplicates
duplicates <- output_clean |>
  summarise(n = dplyr::n(), .groups = "drop") |>
  filter(n > 1L) 


# finding the latest detention facility
latest_date <- max(output_clean$`Pull Date`,na.rm=TRUE)
facility_first_seen <- output_clean %>% 
  group_by(Name) %>% 
  summarise(
    first_seen = min(`Pull Date`,na.rm=TRUE),
    .groups = "drop"
  )
new_facilities_latest_pull <-facility_first_seen %>% 
  filter(first_seen == latest_date)
new_facility_records <- output_clean |>
  filter(
    `Pull Date` == latest_date,
    Name %in% new_facilities_latest_pull$Name
  )

#Read in detloc ids
source("detloc_processing_2025-12-08.r")

# Join with DETLOC
#join with detloc codes
output_clean_join <- output_clean |>
  filter(`Pull Date` > "2023-09-30")|>
  left_join(detloc_code_join, by=c("Name"="Name", "City"="City"), relationship="many-to-many") 

count_by_date <- output_clean_join |>
  group_by(Name, DETLOC, Address.x, City, State.x, Address.y, State.y) |>
  count()


all_clean <- output_clean_join |>
  select(-c(Address.y, State.y))|>
  rename(Address = Address.x,
         State = State.x) |>
  mutate(DETLOC = case_when(City == "Binghamton" ~ "BROMMNY",
                            City == "Baldwin" ~ "NRLKCMI",
                            City == "London" ~ "LAURELKY",
                            City == "La Grange" ~ "OLDHAKY",
                            City == "Baraboo" ~ "SAUKCWI",
                            City == "Alexandria" & State == "LA" ~ "JENATLA",
                            City == "Lock Haven" ~ "CLINTPA",
                            City == "Rigby" ~ "JEFFEID",
                            City == "Sault Sainte Marie" ~ "CHIPPMI",
                            City == "Guthrie" ~ "LOGANOK",
                            City == "Falfurrias" ~ "BROKSTX",
                            City == "Green Bay" ~ "BROWNWI",
                            City == "Killona" ~ "NLSCOLA",
                            City == "Angola" ~ "LICEPLA", #For Louisiana ICE processing
                            .default = DETLOC)) |>
  mutate(DETLOC = case_when(Name == "Folkston D Ray Ice Proces" ~ "FOLKIPC",
                            Name == "Folkston D Ray Ice Processing Ctr" ~ "FOLKIPC",
                            Name == "Turner Guilford Knight (Tgk) Jail" ~ "TGKJLFL",
                            Name == "Ero El Paso Camp East Montana" ~ "EROELP",
                            Name == "Dod Detention Facility At Fort Bliss" ~ "EROELP",
                            Name == "Western Regional Jail And Correctional Facility" ~ "WVWESTR",
                            .default = DETLOC))




##THIS IS TOTAL INTERVAL ADP: A Separate 
#start by comparing just two of the releases
final <- all_clean |>
  filter(`Pull Date`!="2025-07-07")|> #Bad data! Don't use from Pull Date 2025-07-07!
  select(c(Name, City, State, Zip, DETLOC, `Pull Date`, cumulative_beds, adp, fiscal_year_day, ALOS)) |>
  mutate(cumulative_pull_date = paste0("cumulative_", `Pull Date`)) |>
  arrange(cumulative_pull_date) |>
  group_by(DETLOC)|>
  distinct()|>
  mutate(DETLOC = ifelse(Name == "Washoe County Jail", "WASHONV", DETLOC))|>
  mutate(DETLOC = ifelse(Name == "Richwood Correctional Center", "RWCCMLA", DETLOC))|>
  #mutate(City = ifelse(City=="Richwood", "Monroe", City)) |>
  filter(!(Name == "Nevada Southern Detention Center" & `Pull Date` == "2025-02-08"))|> #Nevada Southern has a blank for this week
  filter(!(Name == "Natrona County Jail" & `Pull Date` == "2025-06-23"))|> #Natrona County has a blank for this week
  filter(!(Name == "Bourbon Co Det Center" & `Pull Date` == "2025-06-23"))|> # Bourbon County has a blank for this week
  filter(!(Name == "Miami Federal Detention" & `Pull Date` == "2025-06-23"))|> #Miami Fed has a blank for this week
  filter(!(Name == "Fayette County Detention Center" & `Pull Date` == "2025-06-23"))|> #Weird for Fayette has a blank for this week
  filter(!(Name == "Finney County Jail" & `Pull Date` == "2025-06-23"))|> #Weird for Finney has a blank for this week
  filter(!(Name == "Sauk County Sheriff" & `Pull Date` == "2025-06-23"))|> #Weird for Sauk has a blank for this week
  filter(!(Name == "La Paz County Adult Detention Facility" & `Pull Date` == "2025-06-23"))|> #Weird for La Paz has a blank for this week
  filter(!(Name == "New Hanover County Jail" & `Pull Date` == "2025-06-23"))|> #Weird for New Hanover has a blank for this week
  filter(!(Name == "Lexington County Jail" & `Pull Date` == "2025-06-23"))|> #Weird for New Hanover has a blank for this week
  filter(!(Name == "Montgomery County Jail" & `Pull Date` == "2025-06-23")) |>
  filter(!(Name == "Northwest Regional Corrections Center" & `Pull Date` == "2025-06-23")) |>
  filter(!(Name == "Nassau County Correctional Center" & `Pull Date` == "2025-06-23" & adp < 10))|> #Duplicate Nassau on 2025-06-23
  mutate(diff_lag = cumulative_beds - lag(cumulative_beds, default = first(cumulative_beds))) |> #Calculate cumulative difference using lag
  mutate(diff_lead = lead(cumulative_beds)-cumulative_beds) |> #Calculate cumulative difference using lead
  mutate(back_interval = as.numeric(difftime(`Pull Date`, lag(`Pull Date`, #Calculate interval between dates
                                                              default = first(`Pull Date`)),
                                             units = "days")),
         central_interval = as.numeric(difftime(lead(`Pull Date`), lag(`Pull Date`)), units="days")) |> 
  mutate(back_interval_adp = diff_lag/back_interval) |> #Calculate adp per day during interval 
  mutate(back_interval_adp = ifelse(is.na(back_interval_adp), adp, back_interval_adp))|>
  mutate(central_interval_adp = (diff_lag+diff_lead)/central_interval)|> #calculate entire difference of interval
  mutate(diff_lag = ifelse(diff_lag < 0, NA, diff_lag),
         diff_lead = ifelse(diff_lead < 0, NA, diff_lead),
         back_interval_adp = ifelse(back_interval_adp <= 0, (cumulative_beds/fiscal_year_day), back_interval_adp),
         central_interval_adp = ifelse(back_interval_adp <= 0, (cumulative_beds/fiscal_year_day), central_interval_adp)) |> #End of Fiscal Year Adjustment since cumulative beds drops after 9/30
  mutate(reporting_diff_back = back_interval_adp - adp,  #Calculate difference between reported Average Daily Population and Interval ADP
         reporting_diff_central = central_interval_adp - adp) |>
  filter(DETLOC != "NWSCFVT") #NORTHWEST STATE CORRECTIONAL CENTER changed codes. NWSCFVT is the old one.


distinct_county <- all_clean |>
  select(c(DETLOC, Address, City, Zip)) |>
  distinct()

#write.xlsx(distinct_county, "detention_for_fips_lookup_2025-09-23.xlsx") #FOR CHLOE
