# one_facility_mandatory_stacked_plot.r
#
# Per-facility stacked bar chart: Mandatory vs Discretionary Detention over time.
#
# Each bar = total interval ADP, split into:
#   Bottom segment → Mandatory_detention_back_interval_adp  (red,  legally required)
#   Top segment    → beds_back_interval_adp - Mandatory     (teal, discretionary)
# Dashed line      → ICE reported ADP (same as existing ADP chart)
#
# Two functions:
#   one_facility_mandatory_stacked_table() — joins mandatory + total ADP, computes segments
#   one_facility_mandatory_stacked_plot()  — renders and saves the chart
#
# Globals expected:
#   touchpoint_date        — Date of the most recent pull
#   as_of_date             — e.g. "March 08, 2026"
#   mandatory_plots_folder — output directory


# ---- Data prep ---------------------------------------------------------------

one_facility_mandatory_stacked_table <- function(code, mandatory_data, total_adp_data) {
  # mandatory_data : DETLOC, Pull Date, Mandatory, Mandatory_detention_back_interval_adp
  # total_adp_data : DETLOC, Pull Date, back_interval_adp, adp

  mand <- mandatory_data |>
    filter(DETLOC == code) |>
    mutate(`Pull Date` = as.Date(`Pull Date`))

  total <- total_adp_data |>
    filter(DETLOC == code) |>
    mutate(`Pull Date` = as.Date(`Pull Date`)) |>
    select(DETLOC, `Pull Date`, back_interval_adp, adp)

  joined <- mand |>
    left_join(total, by = c("DETLOC", "Pull Date")) |>
    filter(!is.na(back_interval_adp), back_interval_adp > 0) |>
    mutate(
      # Ensure mandatory never exceeds total (data quirk guard)
      mandatory_adp    = pmin(Mandatory_detention_back_interval_adp, back_interval_adp, na.rm = TRUE),
      discretionary_adp = back_interval_adp - mandatory_adp,
      FY = paste0("FY", if_else(
        month(`Pull Date`) >= 10,
        year(`Pull Date`) + 1,
        year(`Pull Date`)
      ) - 2000)
    ) |>
    group_by(DETLOC) |>
    arrange(`Pull Date`) |>
    mutate(
      last_date = lag(`Pull Date`),
      last_date = as.Date(ifelse(`Pull Date` == as.Date("2023-10-10"), as.Date("2023-10-01"), last_date)),
      last_date = as.Date(ifelse(is.na(last_date), `Pull Date` - 14, last_date))
    ) |>
    rename(Pull.Date = `Pull Date`) |>
    ungroup()

  return(joined)
}


# ---- Chart function ----------------------------------------------------------

one_facility_mandatory_stacked_plot <- function(df, name, code,
                                                 interval_adp, max_y,
                                                 as_of = as_of_date) {
  logo_path <- if (file.exists("logo.png")) "logo.png" else "Relevant_Research_HZ_Color.png"

  # Latest mandatory % for subtitle
  latest        <- df |> filter(Pull.Date == max(Pull.Date))
  mandatory_pct <- round((latest$mandatory_adp[1] / latest$back_interval_adp[1]) * 100, 1)

  step <- 100  # y-axis break interval (matches existing ADP charts)

  plt <- df |>
    ggplot() +

    # ── Top segment: Discretionary (coloured by FY) ──
    geom_rect(
      aes(
        xmin = Pull.Date, xmax = last_date,
        ymin = mandatory_adp, ymax = back_interval_adp,
        fill = FY
      ),
      color = "white"
    ) +

    # ── Bottom segment: Mandatory (fixed red across all FYs) ──
    geom_rect(
      aes(
        xmin = Pull.Date, xmax = last_date,
        ymin = 0, ymax = mandatory_adp,
        fill = "Mandatory"
      ),
      color = "white"
    ) +

    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%Y-%m",
      limits  = c(as.Date("2023-10-01"), lubridate::ceiling_date(max(df$Pull.Date, na.rm = TRUE), "month")), #axis ends at the facility's own last pull date (== touchpoint for current facilities)
      expand  = c(0, 0)
    ) +

    scale_y_continuous(
      limits = c(0, max_y),
      breaks = seq(0, max_y, by = step),
      expand = c(0, 0),
      label  = scales::label_comma()
    ) +

    # Mandatory red + FY teal palette in one scale
    scale_fill_manual(
      name   = "",
      values = c(
        "Mandatory" = "#3BC1A8",
        "FY24"      = "#006475",
        "FY25"      = "#00A1B7",
        "FY26"      = "#616571"
      ),
      breaks = c("Mandatory", "FY24", "FY25", "FY26"),
      labels = c("Mandatory", "FY24", "FY25", "FY26")
    ) +

    guides(fill = guide_legend(nrow = 1)) +

    labs(
      y        = "Average Daily Population",
      x        = "",
      title    = paste0("Mandatory Detention ADP for ", name),
      subtitle = paste0(
        "As of ", as_of, ", ", mandatory_pct, "% of the interval-adjusted population (",
        as.character(round(latest$mandatory_adp[1], 0)), " of ", interval_adp,
        ") were subject to mandatory detention.\n"
      ),
      caption = "Source: ICE"
    ) +

    coord_cartesian(clip = "off") +

    theme_classic(base_family = "Segoe UI") +
    theme(
      axis.text.x       = element_text(angle = 67, vjust = 1, hjust = 1),
      strip.background  = element_blank(),
      text              = element_text(family = "Segoe UI Symbol", size = 16),
      plot.title        = element_text(face = "bold"),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.box        = "horizontal",
      legend.title      = element_blank(),
      legend.key.size   = unit(0.5, "cm"),
      legend.spacing.x  = unit(0.3, "cm"),
      plot.margin       = unit(c(0.5, 0.5, 0.6, 0.5), "in")  # extra bottom for legend
    )

  # Save raw plot
  filename <- file.path(mandatory_plots_folder, paste0(code, "_mandatory_stacked_plt.png"))
  print(filename)
  ggsave(filename, plot = plt, width = 16, height = 9, units = "in", dpi = 300)

  # Overlay logo
  final_plt <- ggdraw() +
    draw_plot(plt) +
    draw_image(
      logo_path,
      x     = 0.14,
      y     = -0.27,
      width = 0.06,
      hjust = 1,
      vjust = 0.2
    )

  final_filename <- file.path(mandatory_plots_folder, paste0(code, "_mandatory_stacked_final_plt.png"))
  ggsave(final_filename, plot = final_plt, width = 16, height = 9, units = "in", dpi = 300)
}
