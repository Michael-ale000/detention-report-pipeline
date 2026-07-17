one_facility_scatter_plot <- function(df, monthdayyear, facility_name, code,
                                      snapshot_date = touchpoint_date) {
  logo_path <- if (file.exists("logo.png")) "logo.png" else "Relevant_Research_HZ_Color.png"
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(cowplot)
  })
  
  # -----------------------------
  # Quadrant color mapping
  # -----------------------------
  quad_colors <- c(
    "Q1" = "#006475",
    "Q2" = "#00A1B7",
    "Q3" = "#616571",
    "Q4" = "#9DA7BF"
  )
  
  quad_desc <- c(
    "Q1" = "ICE detains more people but for fewer days compared to other facilities",
    "Q2" = "ICE detains more people and for more days compared to other facilities",
    "Q3" = "ICE detains fewer people and for fewer days compared to other facilities",
    "Q4" = "ICE detains fewer people but for more days compared to other facilities"
  )
  
  # -----------------------------
  # Filter pull date
  # -----------------------------
  df <- df %>% filter(`Pull Date` == snapshot_date) #touch point (or the facility's own last pull date for stale facilities)
  
  # -----------------------------
  # Count and remove NAs
  # -----------------------------
  na_count <- df %>%
    filter(is.na(ALOS) | is.na(back_interval_adp)) %>%
    nrow()
  
  df <- df %>%
    filter(!is.na(ALOS), !is.na(back_interval_adp))
  
  # -----------------------------
  # GLOBAL AVERAGES
  # -----------------------------
  if (nrow(df) == 0) {
    stop("All facilities have missing data on this date.")
  }
  
  average_alos <- round(mean(df$ALOS), 2)
  average_interval_adp <- round(mean(df$back_interval_adp), 2)

  # -----------------------------
  # Dynamic y-axis max (population), rounded up to the nearest 100
  # e.g. a max of 2050 -> axis max of 2100, instead of a fixed 3500
  # -----------------------------
  y_axis_max <- ceiling(max(df$back_interval_adp, na.rm = TRUE) / 100) * 100
  y_axis_breaks <- seq(0, y_axis_max, by = 250)

  # -----------------------------
  # ASSIGN QUADRANTS FIRST ✅
  # -----------------------------
  df <- df %>%
    mutate(
      quadrant = case_when(
        ALOS < average_alos & back_interval_adp > average_interval_adp ~ "Q1",
        ALOS > average_alos & back_interval_adp > average_interval_adp ~ "Q2",
        ALOS < average_alos & back_interval_adp < average_interval_adp ~ "Q3",
        ALOS > average_alos & back_interval_adp < average_interval_adp ~ "Q4",
        TRUE ~ NA_character_
      )
    )
  
  # -----------------------------
  # Safely isolate selected facility ✅
  # -----------------------------
  selected_row <- df %>% filter(DETLOC == code)
  
  # ✅ Graceful fallback instead of crashing
  if (nrow(selected_row) == 0) {
    
    message(paste("Skipping facility", code, "- missing ALOS or population data"))
    
    placeholder_plot <- ggplot() +
      annotate(
        "text",
        x = 0.5, y = 0.6,
        label = paste0("Facility: ", facility_name),
        size = 8, fontface = "bold"
      ) +
      annotate(
        "text",
        x = 0.5, y = 0.45,
        label = "No ALOS or population data available for this facility on this date.",
        size = 5
      ) +
      annotate(
        "text",
        x = 0.5, y = 0.30,
        label = paste0("Source: ICE. Current as of ", monthdayyear),
        size = 4
      ) +
      theme_void()
    
    final_filename <- file.path("plots7", paste0(code, "_NO_DATA.png"))
    
    ggsave(
      filename = final_filename,
      plot = placeholder_plot,
      width = 16, height = 9, units = "in", dpi = 300
    )
    
    return(final_filename)
  }
  
  selected_quadrant <- selected_row %>% pull(quadrant) %>% .[1]
  
  subtitle_text <- ifelse(
    is.na(selected_quadrant),
    "Selected facility cannot be assigned to a quadrant due to missing comparative data.",
    quad_desc[selected_quadrant]
  )
  
  # -----------------------------
  # Label coordinates
  # -----------------------------
  x_left  <- average_alos * 0.5
  x_right <- average_alos + (100 - average_alos)

  # label cap kept at the same relative position (2750/3500) the constants
  # below were originally tuned for, now scaled to the dynamic y_axis_max
  y_label_cap <- y_axis_max * (2750 / 3500)

  y_top    <- average_interval_adp + (y_label_cap - average_interval_adp) * 0.9
  y_bottom <- average_interval_adp * 0.5

  x_alos_label <- min(max(average_alos + 8, 0), 100)
  y_alos_label <- min(max(average_interval_adp + 1800 * (y_axis_max / 3500), 0), y_label_cap)

  x_pop_label  <- min(max(average_alos + 120, 0), 100)
  y_pop_label  <- min(max(average_interval_adp + 150 * (y_axis_max / 3500), 0), y_label_cap)
  
  # -----------------------------
  # BUILD PLOT ✅
  # -----------------------------
  plt <- ggplot(df, aes(x = ALOS, y = back_interval_adp)) +
    
    geom_point(aes(fill = quadrant),
               size = 3, shape = 21, color = "grey60") +
    
    scale_fill_manual(values = quad_colors, na.value = "#FFFFFF") +
    
    geom_point(
      data = selected_row,
      aes(x = ALOS, y = back_interval_adp),
      size = 9,
      shape = 24,
      fill = quad_colors[selected_quadrant],
      color = quad_colors[selected_quadrant],
      stroke = 1.2
    ) +
    geom_text(
      data = selected_row,
      aes(x = ALOS, y = back_interval_adp, label=Name),
      nudge_x = 3,
      nudge_y = 2,
      vjust = "inward",
      hjust = "inward",
      size = 8
    )+
    
    geom_vline(xintercept = average_alos, color = "#a3a3a3") +
    geom_hline(yintercept = average_interval_adp, color = "#a3a3a3") +
    
    annotate("text",
             x = x_alos_label, y = y_alos_label,
             label = paste0("All facilities\nALOS = ", average_alos),
             fontface = "italic", color = "#717171") +
    
    annotate("text",
             x = 85, y = y_pop_label,
             label = paste0("All facilities\nAvg. Pop = ", average_interval_adp),
             fontface = "italic", color = "#717171") +
    
    annotate("text", x = x_left,  y = y_top,
             label = "Many Detained,\nShorter Detention", alpha = 0.5) +
    
    annotate("text", x = x_right-40, y = y_top,
             label = "Many Detained,\nLonger Detention", alpha = 0.5) +
    
    annotate("text", x = x_left,  y = y_bottom,
             label = "Fewer Detained,\nShorter Detention", alpha = 0.5) +
    
    annotate("text", x = x_right-40, y = y_bottom,
             label = "Fewer Detained,\nLonger Detention", alpha = 0.5) +
    
    labs(
      title = paste0("ICE Detention Facilities: ", facility_name),
      subtitle = paste0("As of ", monthdayyear, ", ", subtitle_text),
      x = "Average Length of Stay (Days)",
      y = "Interval Average Daily Population",
      caption = paste0(
        "Source: ICE"
      )
    ) +
    
    scale_x_continuous(limits = c(0, 125), breaks = seq(0,125,25), expand = c(0, 0)) +
    #scale_x_continuous(limits = c(0, 220), breaks = seq(0,220,25), expand = c(0, 0)) + #for two bridge
    scale_y_continuous(limits = c(0, y_axis_max), breaks = y_axis_breaks, expand = c(0, 0)) + # dynamic max, rounded up to nearest 100
    
    coord_flip(clip = "off") +
    
    theme_classic(base_family = "Segoe UI") +
    theme(
      text = element_text(size = 16),
      plot.title = element_text(face = "bold"),
      legend.position = "none",
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in")
    )
  
  # -----------------------------
  # Highlight legend marker
  # -----------------------------
  # plt <- plt +
  #   annotate("point", x = 140, y = 1670, shape = 24, size = 6,
  #            fill = quad_colors[selected_quadrant],
  #            color = quad_colors[selected_quadrant], stroke = 1.2) +
  #   
  #   annotate("text", x = 140, y = 1700,
  #            label = facility_name,
  #            hjust = 0, size = 4)
  
  # -----------------------------
  # Save intermediate
  # -----------------------------
  filename <- file.path("plots7", paste0(code, "_plt.png"))
  ggsave(filename, plot = plt, width = 16, height = 9, units = "in", dpi = 300)
  
  # -----------------------------
  # Add logo
  # -----------------------------
  final_plt <- ggdraw() +
    draw_plot(plt) +              # Draw the plot as the base layer
    draw_image(
      logo_path,
      x = 0.14,                   # 98% to the right
      y = -0.245,                   # 3% from the absolute bottom (Caption line)
      width = 0.06,               # Size of the logo
      hjust = 1,                  # Anchor to the right edge of the logo
      vjust = 0.2                 # Anchor to the vertical middle of the logo
    )
  
  # -----------------------------
  # Save final image
  # -----------------------------
  final_filename <- file.path("plots7", paste0(code, "_final.png"))
  ggsave(final_filename, plot = final_plt, width = 16, height = 9, units = "in", dpi = 300)
  
  return(final_filename)
}

#one_facility_scatter_plot(final, "November 28, 2025", "Washoe County Jail", "WASHONV")
