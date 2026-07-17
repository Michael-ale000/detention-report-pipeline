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

options(scipen = 999)
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
    select(1:22) |>
    rename_with(~gsub("FY[0-9]+ ALOS", "ALOS", .), everything()) |>
    mutate(`Pull Date` = date) |>
    filter(!is.na(City))
  
  return(clean)
}

# Read all Excel files and their sheets
all_data <- map(file_list, read_all_sheets)

#Combine list of dataframes
output <- bind_rows(all_data)
unique(output$`Pull Date`) # viewing what unique pull dates we have within the dataframe.
#view(output) # checking if we getting the exact data table
colnames(output)
# Extract the day in the fiscal year
# cols <- names(output %>% select(ALOS:Mandatory))

# problem_cols <- cols[
#   sapply(output %>% select(ALOS:Mandatory), function(x) {
#     suppressWarnings(any(is.na(as.numeric(x)) & !is.na(x)))
#   })
# ]
# 
# problem_cols

sum(is.na(output$ALOS))
output_clean <- output |>
  mutate(`Pull Date` = lubridate::mdy(`Pull Date`)) |>
  filter(`Pull Date` > "2023-09-30")|>
  filter(!(Name == "Natrona County Jail" & `Pull Date` == "2025-06-23"))|> #Natrona County has a blank for this week
  filter(!(Name == "Nevada Southern Detention Center" & `Pull Date` == "2025-02-08"))|> #Nevada Southern has a blank for this week
  filter(!(Name == "Miami Federal Detention" & `Pull Date` == "2025-06-23"))|> #Miami Fed has a blank for this week
  filter(!(Name == "Fayette County Detention Center" & `Pull Date` == "2025-06-23"))|> #Weird for Fayette has a blank for this week
  filter(!(Name == "Finney County Jail" & `Pull Date` == "2025-06-23"))|> #Weird for Finney has a blank for this week
  filter(!(Name == "La Paz County Adult Detention Facility" & `Pull Date` == "2025-06-23"))|> #Weird for La Paz has a blank for this week
  filter(!(Name == "New Hanover County Jail" & `Pull Date` == "2025-06-23"))|> #Weird for New Hanover has a blank for this week
  filter(!(Name == "Lexington County Jail" & `Pull Date` == "2025-06-23"))|> #Weird for New Hanover has a blank for this week
  filter(!(Name == "Montgomery County Jail" & `Pull Date` == "2025-06-23")) |>
  filter(!(Name == "Northwest Regional Corrections Center" & `Pull Date` == "2025-06-23")) |>
  mutate(Name = str_to_title(Name),
         Address = str_to_title(Address),
         City = str_to_title(City)) |>
  mutate(across(c(ALOS:`Mandatory`), as.numeric))|>
  rowwise()|>
  mutate(adp_sec = sum(across(starts_with("Level")), na.rm = TRUE))|>
  mutate(adp_gen = sum(across(c(`Male Crim`: `Female Non-Crim`)), na.rm = TRUE))|>
  mutate(adp_threat = sum(across(c(`ICE Threat Level 1`: `No ICE Threat Level`)), na.rm = TRUE)) |>
  mutate(adp = rowMeans(across(c(adp_sec, adp_gen, adp_threat)), na.rm = TRUE))|> #ADP is ALWAYS the average of the three categories
  mutate(
    pull_year = year(`Pull Date`),
    fiscal_start = if_else(`Pull Date` >= as.Date(paste0(pull_year,"-10-01")),#handling two consequences for example 2023-09-10 is FY 2023 but 2023-11-01 is FY24
                           as.Date(paste0(pull_year,"-10-01")),
                           as.Date(paste0(pull_year - 1,"-10-01"))
    ),
    fiscal_year_day = as.numeric(difftime(`Pull Date`,fiscal_start,units="days")) + 1 #October 1 is Day1 not Day 0 so added 1.
  )|>
  #mutate(across(c(`ICE Threat Level 1`, `ICE Threat Level 2`, `ICE Threat Level 3`, `No ICE Threat Level`),as.numeric))|>
  mutate(Cumulative_beds = adp*fiscal_year_day)|># Calculate cumulative beds from start of fiscal year
  mutate(Cumulative_level_A = `Level A`*fiscal_year_day)|> #calculate cumulative_level_A
  mutate(Cumulative_level_B = `Level B`*fiscal_year_day)|>
  mutate(Cumulative_level_C = `Level C`*fiscal_year_day)|>
  mutate(Cumulative_level_D = `Level D`*fiscal_year_day)|>
  mutate(Cumulative_MaleCrim = `Male Crim` * fiscal_year_day)|>
  mutate(Cumulative_MaleNonCrim = `Male Non-Crim`*fiscal_year_day)|>
  mutate(Cumulative_FemaleCrim = `Female Crim` * fiscal_year_day)|>
  mutate(Cumulative_FemaleNonCrim = `Female Non-Crim` * fiscal_year_day) %>% 
  mutate(Cumulative_Mandatory_detention = `Mandatory`*fiscal_year_day) %>% 
  mutate(Name = trimws(Name))
# mutate(Cumulative_ICE_Threat_Level1 = `ICE Threat Level 1` * fiscal_year_day)|>
# mutate(Cumulative_ICE_Threat_Level2 = `ICE Threat Level 2` * fiscal_year_day)|>
# mutate(Cumulative_ICE_Threat_Level3 = `ICE Threat Level 3` * fiscal_year_day)|>
# mutate(Cumulative_ICE_Threat_Level_No = `No ICE Threat Level` * fiscal_year_day)
colnames(output_clean)
#View(output_clean)
############## Adding Detention Codes for Uniformity #####################
#Read file in detloc ids
source("detloc_processing_2025-12-08.r")
#joining detloc codes for making uniformity
output_clean_join <- output_clean %>% 
  left_join(detloc_code_join,by=c("Name"="Name","City"="City"),relationship="many-to-many")
count_by_date <- output_clean_join %>% 
  group_by(Name,DETLOC,Address.x,City,State.x,Address.y,State.y) %>% 
  count()



final_clean <- output_clean_join %>% 
  select(-c(Address.y,State.y)) %>% 
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
                            City == "Green Bay" ~ "BROWNWI",
                            City == "Guthrie" ~ "LOGANOK",
                            City == "Richwood" ~ "RWCCMLA",
                            City == "Chambersburg" ~ "FRANKPA",
                            City == "Cottonwood Falls" ~ "CHASEKS",
                            Name == "Folkston D Ray Ice Proces" ~ "FOLKIPC", #TEMPORARY
                            Name == "Ero El Paso Camp East Montana" ~ "EROELP", #TEMPORARY
                            Name == "Dod Detention Facility At Fort Bliss" ~ "EROELP", #TEMPORARY
                            Name == "Saipan Department Of Corrections (Suspe)" ~ "MPSIPAN",
                            Name == "Washoe County Jail" ~ "WASHONV",
                            Name == "Clinton County Correctional Facility" ~ "CLINTPA",
                            Name == "Robert A. Deyton Detention Facility" ~ "RADDFGA",
                            Name == "Greentree Inn Houston Iah" ~ "GRIHIAH", #FAKE
                            Name == "Folkston D Ray Ice Processing Ctr" ~ "FOLKIPC",
                            Name == "Louisiana Ice Processing" ~ "LICEPLA",
                            .default = DETLOC))


#view(final_clean) 
final_clean %>% 
  count(n,name="count") # we came to know that the values of DETLOC is empty for Washoe county jail
missing<-final_clean %>% 
  filter(is.na(DETLOC))
#view(missing)
cumulative_cols <- names(final_clean)[startsWith(names(final_clean),"Cumulative")]
print(cumulative_cols)
final_group <- final_clean %>% 
  select(c(Name, City, State, DETLOC, `Pull Date`, Cumulative_beds, adp, `Level A`,
           Cumulative_level_A, `Level B`, Cumulative_level_B, `Level C`, Cumulative_level_C,
           `Level D`, Cumulative_level_D, `Male Crim`, Cumulative_MaleCrim,
           `Male Non-Crim`, Cumulative_MaleNonCrim, `Female Crim`,`Mandatory`, Cumulative_FemaleCrim,
           `Female Non-Crim`, Cumulative_FemaleNonCrim,Cumulative_Mandatory_detention, fiscal_year_day)) |>
  mutate(cumulative_pull_date = paste0("cumulative_",`Pull Date`)) %>% 
  arrange(DETLOC,`Pull Date`) %>% 
  #distinct()|>
  group_by(DETLOC) %>% 
  filter(!(Name =="Nevada Southern Detention Center" & `Pull Date`==as.Date("2025-02-08"))) %>% #Code copied from adams maybe a cleaning code or handling special event
  filter(!(Name =="Miami Federal Detention" & `Pull Date`==as.Date("2025-06-23"))) %>% #Code copied from adams maybe a cleaning code or handling special event
  #filter(!(Name == "Fayette County Detention Center" & `Pull Date` == as.Date("2025-06-23")))|> #Weird for Fayette has a blank for this week
  #filter(!(Name == "Finney County Jail" & `Pull Date` == as.Date("2025-06-23")))|> #Weird for Finney has a blank for this week
  #filter(!(Name == "La Paz County Adult Detention Facility" & as.Date(`Pull Date` == "2025-06-23")))|> #Weird for La Paz has a blank for this week
  #filter(!(Name == "New Hanover County Jail" & as.Date(`Pull Date` == "2025-06-23")))|> #Weird for New Hanover has a blank for this week
  #filter(!(Name == "Lexington County Jail" & as.Date(`Pull Date` == "2025-06-23")))|> #Weird for New Hanover has a blank for this week
  #filter(!(Name == "Montgomery County Jail" & as.Date(`Pull Date` == "2025-06-23"))) |>
  arrange(`Pull Date`) %>% 
  mutate(across(all_of(cumulative_cols),list(
    diff_lag = ~ . - lag(.),#corresponds function(x) x - lag(x) for each groupby group
    diff_lead = ~ lead(.) - .,
    back_interval_adp = ~ (. - lag(.)) / as.numeric(difftime(`Pull Date`, lag(`Pull Date`), units = "days")),
    central_interval_adp = ~ ((. - lag(.)) + (lead(.) - .)) / as.numeric(difftime(lead(`Pull Date`), lag(`Pull Date`), units = "days"))
  ), .names = "{.col}_{.fn}")) %>% # column name and fn is the name of the function being called for eg diff_lag,diff_lead etc
  mutate(across(ends_with("diff_lag"), ~ ifelse(. < 0, NA, .))) %>% 
  mutate(across(ends_with("back_interval_adp"), ~ ifelse(. < 0, NA, .))) %>% # if values are in -ve then assigning them NA
  mutate(across(ends_with("central_interval_adp"), ~ ifelse(. < 0, NA, .))) %>%
  mutate(across(ends_with("back_interval_adp"),~ifelse(is.na(.),0,.))) %>% # Replace NA with 0 which will be more meaningful
  # mutate(across(ends_with("central_interval_adp"),~ifelse(is.na(.),0,.))) %>% 
  mutate(across(ends_with("central_interval_adp"), ~ ifelse(. < 0, NA_real_, .))) %>%
  ungroup()|>
  mutate(Cumulative_beds_back_interval_adp = ifelse(Cumulative_beds_back_interval_adp == 0, adp, Cumulative_beds_back_interval_adp),
         Cumulative_level_A_back_interval_adp = ifelse(Cumulative_level_A_back_interval_adp == 0, `Level A`, Cumulative_level_A_back_interval_adp),
         Cumulative_level_B_back_interval_adp = ifelse(Cumulative_level_B_back_interval_adp == 0, `Level B`, Cumulative_level_B_back_interval_adp),
         Cumulative_level_C_back_interval_adp = ifelse(Cumulative_level_C_back_interval_adp == 0, `Level C`, Cumulative_level_C_back_interval_adp),
         Cumulative_level_D_back_interval_adp = ifelse(Cumulative_level_D_back_interval_adp == 0, `Level D`, Cumulative_level_D_back_interval_adp),
         Cumulative_MaleCrim_back_interval_adp = ifelse(Cumulative_MaleCrim_back_interval_adp == 0, `Male Crim`, Cumulative_MaleCrim_back_interval_adp),
         Cumulative_MaleNonCrim_back_interval_adp = ifelse(Cumulative_MaleNonCrim_back_interval_adp == 0, `Male Non-Crim`, Cumulative_MaleNonCrim_back_interval_adp),
         Cumulative_FemaleCrim_back_interval_adp = ifelse(Cumulative_FemaleCrim_back_interval_adp == 0, `Female Crim`, Cumulative_FemaleCrim_back_interval_adp),
         Cumulative_FemaleNonCrim_back_interval_adp = ifelse(Cumulative_FemaleNonCrim_back_interval_adp == 0, `Female Non-Crim`, Cumulative_FemaleNonCrim_back_interval_adp),
         Cumulative_Mandatory_detention_back_interval_adp = ifelse(Cumulative_Mandatory_detention_back_interval_adp ==0,`Mandatory`, Cumulative_Mandatory_detention_back_interval_adp),
         #Cumulative_Mandatory_detention_central_interval_adp = ifelse(Cumulative_Mandatory_detention_central_interval_adp ==0,`Mandatory`, Cumulative_Mandatory_detention_central_interval_adp),
         
         # ICE_Threat_Level1_back_interval_adp = ifelse(. == 0, `ICE Threat Level 1`, .),
         # ICE_Threat_Level2_back_interval_adp = ifelse(. == 0, `ICE Threat Level 2`, .),
         # ICE_Threat_Level3_back_interval_adp = ifelse(. == 0, `ICE Threat Level 3`, .),
         # ICE_Threat_Level_No_back_interval_adp = ifelse(. == 0, `No ICE Threat Level`, .)
  ) |>
  mutate(
    Cumulative_Mandatory_detention_central_interval_adp =
      ifelse(
        is.na(Cumulative_Mandatory_detention_central_interval_adp),
        `Mandatory`,
        Cumulative_Mandatory_detention_central_interval_adp
      )
  ) %>%
  rename_with(
    ~ gsub("^Cumulative_", "", .x),
    .cols = matches("^(Cumulative_).*(diff_lag|diff_lead|back_interval_adp|central_interval_adp)$") #renaming by removing prefix cumulative after adding formula name
  )
colnames(final_group)
test1 <- final_group %>% 
  select(Name,State,DETLOC,Mandatory,Mandatory_detention_diff_lag,Mandatory_detention_diff_lead,Mandatory_detention_back_interval_adp,Mandatory_detention_central_interval_adp)
#write.csv(final_group,"finalgroup.csv")

colnames(final_group)
grouped_selected <- final_group %>%
  select(Name, City, State, DETLOC, `Pull Date`,fiscal_year_day, adp, 
         `Level A`, `Level B`, `Level C`, `Level D`,
         `Male Crim`, `Male Non-Crim`, `Female Crim`, `Female Non-Crim`,`Mandatory`,
         starts_with("Cumulative"), ends_with(c("back_interval_adp", "central_interval_adp"))) %>% #Selecting only those columns that we required.
  arrange(desc(`Pull Date`))


##for test ######
test <- grouped_selected %>% 
  select(Name,City,State,DETLOC,`Pull Date`,fiscal_year_day,adp,beds_back_interval_adp,Mandatory,Mandatory_detention_back_interval_adp,Mandatory_detention_central_interval_adp)
write.xlsx(test,"test.xlsx")
############### for mandatory detention ##########

mandatory_filter <- grouped_selected %>% 
  select(Name,City,State,DETLOC,`Pull Date`,fiscal_year_day,Mandatory,Mandatory_detention_back_interval_adp)
write.xlsx(mandatory_filter,"mandatory detention with interval adp calculation for all detention data.xlsx")

#View(grouped_selected)
latest_date <- max(grouped_selected$`Pull Date`) # extracting the top recent date 
date <- format(latest_date,"%Y-%m-%d")
final_table <- grouped_selected %>% #filtering the only recent data
  filter(`Pull Date` == date)
filename <- paste0("IntervalAdpForIndividualColumn_",date,"_v2_for_mandatory.xlsx") #saving the recent data's all backinterval into and excel file
write.xlsx(final_table,file = filename)

#######checking for null detloc #####

# no_detloc <- final_clean %>% 
#   filter(is.na(DETLOC)) %>% 
#   select(Name,DETLOC,`Pull Date`,Address,State,City)
# 
# fulton <- final_clean %>% 
#   filter(Name == 'Folkston D Ray Ice Processing Ctr')


###########checking for new detention facility in ###########

# data_before_the_latest_release <- grouped_selected %>% 
#   filter(`Pull Date`!="2025-12-11")
# all_facilitis_before_latest_release <- unique(data_before_the_latest_release$Name)
# all_facilites_in_new_release <- unique(final_table$Name)
# unique <- setdiff(all_facilites_in_new_release,all_facilitis_before_latest_release)
