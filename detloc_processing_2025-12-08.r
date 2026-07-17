#detloc processing
library(tidyverse)
detloc <- read.csv('detloc_2025-04-13.csv')

detloc <- detloc |>
  select(c(DETLOC, Name, Address, City, State)) |>
  mutate(across(c(Name, Address, City), str_to_title))

unique_facilities <- output_clean |>
  group_by(Name, Address, City, State)|>
  distinct()|>
  count()


detloc_code_join <- unique_facilities |>
  left_join(detloc, by= join_by("Address", "City", "State"), 
            relationship = "many-to-many") |>
  mutate(DETLOC = case_when(City == "Rigby" ~ "JEFFEID",
                            City == "Aurora" ~ "DENICDF",
                            City == "Sault Sainte Marie" ~ "CHIPPMI",
                            City == "Lock Haven" ~ "CLINTPA",
                            City == "Robstown" ~ "CBENDTX",
                            City == "Naples" ~ "COLLIFL",
                            City == "Hagatna" ~ "GUDOCHG",
                            City == "Gastonia" ~ "GASTNNC",
                            City == "Rapid City" ~ "PENNISD",
                            City == "Richwood" ~ "RWCCMLA",
                            City == "Lovejoy" ~ "RADDFGA",
                            City == "Saipan" ~ "MPSIPAN",
                            City == "Charleston" ~ "WVSCENT",
                            City == "Reno" ~ "WASHONV",
                            City == "Alvarado" ~ "PRLDCTX",
                            City == "Conroe" ~ "MTGPCTX",
                            City == "Orlando" ~ "ORANGFL",
                            City == "Goshen" ~ "ORANGNY",
                            City == "Rolla" ~ "PHELPMO",
                            City == "Holdrege" ~ "PHELPNE",
                            City == "Saipan" ~ "MPSIPAN",
                            City == "Sault Sainte Marie" ~ "CHIPPMI",
                            .default = DETLOC))|>
  mutate(DETLOC = case_when(Name.x == "Greentree Inn Houston Iah" ~ "GREENIAH", ### I MADE THIS ONE UP---NOT IN SYSTEM
                            Name.x == "Krome North Service Processing Center" ~ "KRO",
                            Name.x == "Nevada Southern Detention Center" ~ "NVSDCNV",
                            Name.x == "Florence Staging Facility" ~ "FSF",
                            Name.x == "Florence Service Processing Center" ~ "FLO",
                            Name.x == "Florence Spc" ~ "FLO",
                            Name.x == "Migrant Ops Center Main A" ~ "GTMOACU",
                            Name.x == "Jtf Camp Six" ~ "GTMODCU",
                            Name.x == "Richwood Correctional Center" ~ "RWCCMLA",
                            Name.x == "Mccook Ice Igsa" ~ "MCCKNE",
                            Name.x == "Baker Correctional Institution" ~ "FLBAKCI",
                            Name.x == "Florida Soft-Sided Facility" ~ "FLDSSFS",
                            Name.x == "Sauk County Sheriff" ~ "SAUKCWI",
                            Name.x == "Western Regional Jail And Correctional Facility" ~ "WVWESTR",
                            Name.x == "Dilley Processing Single Adult Female" ~ "DILLSAF",
                            .default = DETLOC)) |>
  mutate(DETLOC = ifelse(Address == "500 Hilbig Rd", "JCRLYTX", DETLOC)) 

detloc_code_join <- detloc_code_join |>
  filter(DETLOC != "GRYDCKY") |> #Old codes GRAYSON COUNTY, Lexington 
  filter(DETLOC != "LEXFCKY") |>
  filter(Address != "419 Shoemaker Road") |>
  filter(City != "Richwood") |>
  #filter(Address != "3347 Tamiami Trail E") |>
  filter(!is.na(Address)) |>
  filter(DETLOC != "BOPMVC") |>
  filter(DETLOC != "WILLCTX") |>
  filter(Address != "911 Parr Blvd 775 328 3308") |>
  #filter(Address != "200 Courthouse Way" && Name.x == "Jefferson County Jail") |>
  filter(Address != "4909 Fm (Farm To Market) 2826") |>
  #filter(Address != "Tekken St., Susupe Village" && Name.x == "Saipan Department Of Corrections (Susupe)") |>
  #filter(City != "Sault Sainte Marie") |>
  filter(DETLOC != "BOPNEO") |>
  filter(Name.y != "Krome Hold Room") |>
  filter(Name.y != "Florence Staging Facility")|>
  filter(Name.y != "Montgomery Hold Rm")|>
  filter(Name.y != "Prairieland Suboffice Hold Room")|>
  select(-c(Name.y))|>
  rename(Name = Name.x)

# Ensure no NAs in Detloc
detloc_nas <- detloc_code_join |>
  filter(is.na(DETLOC))
