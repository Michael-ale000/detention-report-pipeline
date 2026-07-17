#one_facility_table


one_facility_table <- function(code){   #Might have to use facility instead, depending
  center <- final |> #depending
    filter(DETLOC == code) |>
    mutate(back_interval_adp = ifelse(is.na(back_interval_adp), adp, back_interval_adp)) |>
    group_by(DETLOC)|>
    mutate(last_date = lag(`Pull Date`))|>
    mutate(next_date = lead(`Pull Date`))|>
    mutate(next_date = as.Date(ifelse(`Pull Date` == as.Date("2025-07-21"), as.Date("2025-08-05"), next_date)))|> #touchpoint
    mutate(next_date = as.Date(ifelse(is.na(next_date), touchpoint_date + 14, next_date))) |>
    mutate(last_date = as.Date(ifelse(`Pull Date` == as.Date("2023-10-10"), as.Date("2023-10-01"), last_date)))|>
    mutate(last_date = as.Date(ifelse(is.na(last_date), `Pull Date`-14, last_date)))|>
    mutate(FY = paste0("FY", if_else(month(`Pull Date`) >= 10,
                                     year(`Pull Date`) + 1,
                                     year(`Pull Date`)) - 2000)) |>
    rename(Pull.Date = `Pull Date`)|>
    mutate(pct_diff_back = reporting_diff_back/adp)|>
    arrange(desc(pct_diff_back))
}

platte <- one_facility_table("PLATTWY")
pahrump <- one_facility_table("NVSDCNV")
alex_va <- one_facility_table("ALEXAVA")
alex_la <- one_facility_table("JENATLA")
washoe <- one_facility_table("WASHONV")
dilley <- one_facility_table("STFRCTX")
freeborn <- one_facility_table("FREEBMN")
richwood <- one_facility_table("RWCCMLA")
natchez <- one_facility_table("ADAMSMS")
torrance <- one_facility_table("TOORANM")
otay_mesa <- one_facility_table("CCASDCA")
moshannon <- one_facility_table("MSVPCPA")
stewart <- one_facility_table("STWRTGA")
winnfield <- one_facility_table("LAWINCI")
eloy <- one_facility_table("EAZ")
karnes <- one_facility_table("KRNRCTX")
cedar_rapids <- one_facility_table("LINNJIA")
la_paz <- one_facility_table("LAPAZAZ")
clinton_ny <- one_facility_table("CLICONY")
taylor <- one_facility_table("CCAHUTX")
miami_fed <- one_facility_table("BOPMIM")
conroe <- one_facility_table("JCRLYTX")
imperial <- one_facility_table("IRADFCA")
houston <- one_facility_table("HOUICDF")
jackson <- one_facility_table("JKPCCLA")
nassau <- one_facility_table("NASSANY") 
sauk <- one_facility_table("SAUKCWI")
fonda <- one_facility_table("MONTGNY")
bourbon <- one_facility_table("BOURBKY")
natrona <- one_facility_table("NATROWY")
victoria <- one_facility_table("VICTOTX")
broward <- one_facility_table("WCCPBFL")
valle <- one_facility_table("ELVDFTX")
mont_tx <- one_facility_table("MTGPCTX")
otero <- one_facility_table("OTRPCNM")
roswell <- one_facility_table("CHAVENM")
frankpa <- one_facility_table("FRANKPA")
davenport <- one_facility_table("SCOTTIA")
karnes <- one_facility_table("KARNETX")
folkd <- one_facility_table("FIPCDGA")
angola <- one_facility_table("LICEPLA")
#finney <- one_facility_plot("FINNEKS")
eastern <- one_facility_table("WVEASTR")
dilley_female <- one_facility_table("DILLSAF")
karnes <- karnes |>
  filter(!is.na(adp))

davenport <- davenport |>
  filter(!is.na(adp))

victoria <- victoria |>
  filter(!is.na(adp))

frankpa <- frankpa |>
  filter(!is.na(adp))

roswell <- roswell |>
  filter(!is.na(adp))

sauk <- sauk |>
  filter(!is.na(adp))

dilley <- dilley |>
  filter(Name != "Trusted Adult South Tex Dilley Fsc") |>
  mutate(last_date = if_else(Pull.Date == as.Date("2025-04-14"), as.Date("2024-03-31"), as.Date(last_date)))

cedar_rapids <- cedar_rapids |>
  mutate(last_date = if_else(Pull.Date == as.Date("2025-06-09"), as.Date("2025-05-12"), as.Date(last_date)))|>
  mutate(last_date = if_else(Pull.Date == as.Date("2025-06-09"), as.Date("2025-05-12"), as.Date(last_date)))
dilley_female <- one_facility_table("DILLSAF")
torrance <- one_facility_table("TOORANM")


