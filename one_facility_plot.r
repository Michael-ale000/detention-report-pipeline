one_facility_plot <- function(df, monthdayyear, name, code, reported_adp, interval_adp, max, x_pos, y_pos){
  logo_path <- if (file.exists("logo.png")) "logo.png" else "Relevant_Research_HZ_Color.png"
  plt <- df|>
    ggplot()+
    geom_rect(aes(xmin=Pull.Date, xmax=last_date, ymin=0, ymax=back_interval_adp,
                  fill=FY), color='white') +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m",
                 limits=c(as.Date("2023-10-01"), lubridate::ceiling_date(max(df$Pull.Date, na.rm = TRUE), "month")), #axis ends at the facility's own last pull date (== touchpoint for current facilities)
                 expand = c(0,0)) +
    scale_y_continuous(limits=c(0,max), breaks=seq(0,max,100), expand = c(0,0), #Establishes the y-axis
                       label=scales::label_comma())+
    scale_fill_manual(values = c("FY24" = "#006475", "FY25" = "#00A1B7", "FY26"="#616571"))+
    labs(y="Average Daily Population", x="",
         title=paste0("Interval ADP and Reported ADP for ", name),
         subtitle=paste0("As of ", monthdayyear, ", ICE reported an annualized average of ", reported_adp,
                         " detainees at the facility. \nUsing an interval average, the number detained on that date was likely closer to ",
                         interval_adp,"."),
         legend="",
         caption="Source: ICE")+
    coord_cartesian(clip = "off") +
    theme_classic(base_family = "Segue UI")+
    theme(axis.text.x = element_text(angle=67, vjust = 1, hjust=1),
          strip.background = element_blank(),
          text = element_text(family = 'Segoe UI Symbol', size=16),
          plot.title = element_text(face = "bold"),
          legend.position = c(x_pos, y_pos),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          plot.margin = unit(c(0.5, 0.5,0.25, 0.5), "in"))
  
  filename <- file.path(plots_folder, paste0(code, "_plt.png"))
  print(filename)
  ggsave(filename, width=16, heigh=9, units = 'in')
  
  
  # final_plt <- ggdraw()+
  #   draw_image(filename, scale=1)+
  #   draw_image("rotated_logo.png", x=0.975, y=-0.263, width=0.02)
  # final_plt
  final_plt <- ggdraw() +
    draw_plot(plt) +              # Draw the plot as the base layer
    draw_image(
      logo_path,
      x = 0.14,                   # 98% to the right
      y = -0.27,                   # 3% from the absolute bottom (Caption line)
      width = 0.06,               # Size of the logo
      hjust = 1,                  # Anchor to the right edge of the logo
      vjust = 0.2                 # Anchor to the vertical middle of the logo
    )
  final_plt

  final_filename <- file.path(plots_folder, paste0(code, "_final_plt.png"))
  ggsave(final_filename, width=16, height=9, units = 'in')
}





########### For Dilley #########################
one_facility_plot_for_dilley <- function(df, monthdayyear, name, code, reported_adp, interval_adp, max, x_pos, y_pos){
  logo_path <- if (file.exists("logo.png")) "logo.png" else "Relevant_Research_HZ_Color.png"

  df <- df |>
    mutate(
      Pull.Date = as.Date(Pull.Date),
      adp = dplyr::na_if(adp, 0)
    )
  plt <- df |>
    ggplot() +
    geom_rect(
      aes(xmin = Pull.Date, xmax = last_date, ymin = 0, ymax = back_interval_adp, fill = FY),
      color = "white"
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%Y-%m",
      limits = c(as.Date("2023-10-01"), lubridate::ceiling_date(max(df$Pull.Date, na.rm = TRUE), "month")), #axis ends at the facility's own last pull date (== touchpoint for current facilities)
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, max),
      breaks = seq(0, max, 100),
      expand = c(0, 0),
      label = scales::label_comma()
    ) +
    scale_fill_manual(values = c("FY24" = "#006475", "FY25" = "#00A1B7", "FY26" = "#616571")) +
    labs(
      y = "Average Daily Population",
      x = "",
      title = paste0("Interval ADP and Reported ADP for ", name),
      subtitle = paste0(
        "As of ", monthdayyear, ", ICE reported an annualized average of ", reported_adp,
        " detainees at the facility. \nUsing an interval average, the number detained on that date was likely closer to ",
        interval_adp, "."
      ),
      legend = "",
      caption = "Source: ICE"
    ) +
    coord_cartesian(clip = "off") +
    theme_classic(base_family = "Segue UI") +
    theme(
      axis.text.x = element_text(angle = 67, vjust = 1, hjust = 1),
      strip.background = element_blank(),
      text = element_text(family = "Segoe UI Symbol", size = 16),
      plot.title = element_text(face = "bold"),
      legend.position = c(x_pos, y_pos),
      legend.direction = "horizontal",
      legend.title = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.25, 0.5), "in")
    )
  
  filename <- file.path(plots_folder, paste0(code, "_plt.png"))
  print(filename)
  ggsave(filename, plot = plt, width = 16, height = 9, units = "in")
  
  final_plt <- ggdraw() +
    draw_plot(plt) +
    draw_image(
      logo_path,
      x = 0.14,
      y = -0.27,
      width = 0.06,
      hjust = 1,
      vjust = 0.2
    )
  final_filename <- file.path(plots_folder, paste0(code, "_final_plt.png"))
  ggsave(final_filename, plot = final_plt, width = 16, height = 9, units = "in")
}

