#interval adp chartmaker

if (!exists("final")) source('interval_adp.r')
source('one_facility_plot.r')
source('one_facility_table.r')
source('one_facility_scatter_plot.r')
library(extrafont)
library(cowplot)
library(magick)
#loadfonts(device = "win")

###ONLY DOES BACKWARD AVERAGE
#View(final)
#dim(final)
write.xlsx(final,"all_merged_file.xlsx")

# Set date globals if not already defined (e.g. when run standalone vs. via run_pipeline.r)
if (!exists("touchpoint_date")) {
  touchpoint_date <- max(final$`Pull Date`, na.rm = TRUE)
  as_of_date      <- format(touchpoint_date, "%B %d, %Y")
  plots_folder    <- paste0("plots", format(touchpoint_date, "%Y_%m_%d"))
  dir.create(plots_folder, showWarnings = FALSE)
  dir.create("plots7", showWarnings = FALSE)
}

message("=== Chartmaker running for ", as_of_date, " ===")

l <- image_read("Relevant_Research_HZ_Color.png")
t <- grid::roundrectGrob()

top_now <- final |>
  filter(`Pull Date` == touchpoint_date) |>
  arrange(desc(cumulative_beds))|>
  head(12)


top_reportdelta_now <- final |>
  mutate(pct_diff_back = reporting_diff_back/adp)|>
  arrange(desc(pct_diff_back))|>
  head(12)

facilities_now <- final |>
  filter(`Pull Date` >= touchpoint_date) |>
  filter(!is.na(adp))|>
  filter(!is.na(DETLOC))|>
  distinct(Name, DETLOC)

message(">>> Interval ADP charts will be generated for ", nrow(facilities_now), " facilities.")

new <- count_by_date |>
  filter(n==1)

new_this_time <- new |>
  semi_join(facilities_now)


# ---- Helper: look up legend position from legend_config.csv ----
get_legend_pos <- function(code) {
  cfg <- read.csv("legend_config.csv", stringsAsFactors = FALSE)
  row <- cfg[cfg$DETLOC == code, ]
  if (nrow(row) == 0) return(list(x = 0.1, y = 0.9))
  list(x = row$x_pos[1], y = row$y_pos[1])
}

# ---- Helper: compute reported/interval ADP and max_y from a facility table ----
get_facility_adp <- function(tbl) {
  latest <- tbl |> filter(Pull.Date == max(Pull.Date))
  reported <- as.character(round(latest$adp[1], 0))
  interval  <- as.character(round(latest$back_interval_adp[1], 0))
  max_y     <- max(300, round(max(tbl$adp, tbl$back_interval_adp, na.rm = TRUE) * 1.2, -2))
  list(reported = reported, interval = interval, max_y = max_y)
}


plot_all <- function(df){
  many_dfs <- lapply(facilities_now$DETLOC, function(i){
    data.frame(one_facility_table(i))
  })
  # Name the list using the DETLOC value (column 5)
  names(many_dfs) <- sapply(many_dfs, function(df) as.character(df[1, 5]))

  #Determine the max level for each facility
  maxes <- function(df){
    max_adp <- max(df$adp)
    max_interval_adp <- max(df$back_interval_adp)
    final_max <- max(max_adp, max_interval_adp)

    df <- df |>
      mutate(max = as.numeric(round((final_max+50), -2))) #Rounding to the nearest 100
  }

  maxes_dfs <- lapply(many_dfs, function(df){
    data.frame(maxes(df))
  })

  facilities <- as_tibble(names(maxes_dfs))

  #Select df of file
  sapply(facilities$value, function(i){
    facility_code <- as.character(i)
    print(facility_code)
    facility <- as_tibble(maxes_dfs[[i]])
    facility <- facility |>
      replace(is.na(facility), 0)
    facility_name <- as.character(facility[1,1])
    print(facility_name)
    facility_max <- ifelse(is.finite(facility$max[1]), facility$max[1], 200)
    adps <- facility |>
      filter(Pull.Date == max(Pull.Date)) |>
      mutate(adp = ifelse(is.finite(adp), adp, 0),
             back_interval_adp = ifelse(is.finite(back_interval_adp),
                                        back_interval_adp, 0))|>
      select(c(adp, back_interval_adp))
    date_adp <- as.character(round(adps$adp,0))
    print(date_adp)
    date_interval_adp <- as.character(round(adps$back_interval_adp,0))
    print(date_interval_adp)
    lp <- get_legend_pos(facility_code)
    one_facility_plot(facility, as_of_date, facility_name, facility_code, date_adp,
                      date_interval_adp, facility_max, lp$x, lp$y)
  })

}

plot_all(final)

## All of these have particular issues related to the data or legend that require special handling.
source('one_facility_plot.r')

# Dilley uses a separate function due to the data gap in 2024-2025
fa <- get_facility_adp(dilley)
lp <- get_legend_pos("STFRCTX")
one_facility_plot_for_dilley(dilley, as_of_date, "Dilley Immigration Processing Center", "STFRCTX",
                             fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

fa <- get_facility_adp(dilley_female)
one_facility_plot(dilley_female, as_of_date, "Dilley Processing Single Adult Female", "DILLSAF",
                  fa$reported, fa$interval, fa$max_y, 0.1, 0.9)

lp <- get_legend_pos("LAWINCI")
fa <- get_facility_adp(winnfield)
one_facility_plot(winnfield, as_of_date, "Winn Correctional Center", "LAWINCI",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("EAZ")
fa <- get_facility_adp(eloy)
one_facility_plot(eloy, as_of_date, "Eloy Federal Contract Facility", "EAZ",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("CCAHUTX")
fa <- get_facility_adp(taylor)
one_facility_plot(taylor, as_of_date, "T Don Hutto Detention Center", "CCAHUTX",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("JCRLYTX")
fa <- get_facility_adp(conroe)
one_facility_plot(conroe, as_of_date, "Joe Corley Processing Ctr", "JCRLYTX",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("IRADFCA")
fa <- get_facility_adp(imperial)
one_facility_plot(imperial, as_of_date, "Imperial Regional Detention Facility", "IRADFCA",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("HOUICDF")
fa <- get_facility_adp(houston)
one_facility_plot(houston, as_of_date, "Houston Contract Detention Facility", "HOUICDF",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("JKPCCLA")
fa <- get_facility_adp(jackson)
one_facility_plot(jackson, as_of_date, "Jackson Parish Correctional Center", "JKPCCLA",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

# fa <- get_facility_adp(victoria)
# one_facility_plot(victoria, as_of_date, "Victoria County Jail", "VICTOTX",
#                   fa$reported, fa$interval, fa$max_y, 0.1, 0.9)


lp <- get_legend_pos("WCCPBFL")
fa <- get_facility_adp(broward)
one_facility_plot(broward, as_of_date, "Broward Transitional Center", "WCCPBFL",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("ELVDFTX")
fa <- get_facility_adp(valle)
one_facility_plot(valle, as_of_date, "El Valle Detention Facility", "ELVDFTX",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("MTGPCTX")
fa <- get_facility_adp(mont_tx)
one_facility_plot(mont_tx, as_of_date, "Montgomery ICE Processing Center", "MTGPCTX",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

lp <- get_legend_pos("OTRPCNM")
fa <- get_facility_adp(otero)
one_facility_plot(otero, as_of_date, "Otero County Processing Center", "OTRPCNM",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)

fa <- get_facility_adp(davenport)
one_facility_plot(davenport, as_of_date, "Scott County Det. Facility", "SCOTTIA",
                  fa$reported, fa$interval, fa$max_y, 0.1, 0.9)

fa <- get_facility_adp(karnes)
one_facility_plot(karnes, as_of_date, "Karnes County Correctional Center", "KARNETX",
                  fa$reported, fa$interval, fa$max_y, 0.1, 0.9)
lp <- get_legend_pos("TOORANM")
fa <- get_facility_adp(torrance)
one_facility_plot(torrance, as_of_date, "Torrance County Detention Facility", "TOORANM",
                  fa$reported, fa$interval, fa$max_y, lp$x, lp$y)






source('one_facility_scatter_plot.r')


mean_back_interval <- mean(final$back_interval_adp, na.rm = TRUE)


########### Scatter plot integration #####################

plot_all_scatter <- function(df, detloc_filter = NULL) {

  # Use provided filter, or fall back to all codes in the dataset
  facility_codes <- if (!is.null(detloc_filter)) detloc_filter else unique(df$DETLOC)

  message("Found ", length(facility_codes), " facilities.")
  for (code in facility_codes) {

    message("Processing facility code: ", code)

    # Extract facility name (first non-NA name)
    facility_name <- df %>%
      filter(DETLOC == code) %>%
      pull(Name) %>%
      unique() %>%
      first()

    if (is.na(facility_name)) {
      message("  Skipping ", code, " (missing facility name)")
      next
    }

    # Generate plot
    one_facility_scatter_plot(
      df            = df,
      monthdayyear  = as_of_date,
      facility_name = facility_name,
      code          = code
    )

    message("  Saved plot for ", facility_name)
  }

  invisible(NULL)
}

message(">>> Scatter charts will be generated for ", nrow(facilities_now), " facilities (latest pull date only).")
plot_all_scatter(final, facilities_now$DETLOC)
one_facility_scatter_plot(final, as_of_date, "Dilley Immigration Processing Center", "STFRCTX")


########## Finding % change ####################

sorted_dates <- sort(unique(final$`Pull Date`), decreasing = TRUE)
last_two_dates <- sorted_dates[1:2]

current_and_previous_facilities <- final %>%
  filter(`Pull Date` %in% last_two_dates)

df_clean <- current_and_previous_facilities %>%
  group_by(DETLOC) %>%
  filter(n_distinct(`Pull Date`) == 2) %>%
  ungroup()

latest_col <- as.character(last_two_dates[1])
prev_col   <- as.character(last_two_dates[2])

pct_change_by_facility <- df_clean %>%
  group_by(DETLOC, Name, `Pull Date`) %>%
  summarise(
    population = sum(back_interval_adp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = `Pull Date`,
    values_from = population
  ) %>%
  mutate(
    absolute_change = .data[[latest_col]] - .data[[prev_col]],
    pct_change = if_else(
      is.na(.data[[prev_col]]) | .data[[prev_col]] == 0,
      NA_real_,
      absolute_change / .data[[prev_col]] * 100
    )
  ) %>%
  filter(
    absolute_change > 50,
    pct_change > 5
  )
