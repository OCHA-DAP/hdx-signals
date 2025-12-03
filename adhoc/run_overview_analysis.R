box::use(
  src/utils/cloud_storage,
  src/indicators/idmc_displacement/utils/raw_displacement,
  src/indicators/idmc_displacement/utils/wrangle_displacement,
  src/indicators/acled_conflict/utils/raw_conflict,
  src/indicators/acled_conflict/utils/wrangle_conflict,
  src/indicators/jrc_agricultural_hotspots/utils/raw_agricultural_hotspots,
  src/indicators/jrc_agricultural_hotspots/utils/wrangle_agricultural_hotspots,
  src/indicators/acaps_inform_severity/utils/raw_inform,
  src/indicators/acaps_inform_severity/utils/wrangle_inform,
  src/indicators/ipc_food_insecurity/utils/raw_food_insecurity,
  src/indicators/ipc_food_insecurity/utils/wrangle_food_insecurity,
  src/indicators/wfp_market_monitor/utils/raw_market_monitor,
  src/indicators/wfp_market_monitor/utils/wrangle_market_monitor
)

box::use(
  dplyr,
  gg = ggplot2,
  ggpattern,
  cowplot,
  lubridate,
  logger,
  scales,
  az = AzureStor,
  utils,
  ggnewscale
)

# Global variables
years_back <- 3
n_top_locations <- 3
signal_gap_days <- 30

# Date Auto-detection YYYY-MM-DD
end_date <- as.Date(Sys.Date())
start_date <- as.Date(format(Sys.Date() - lubridate$years(years_back), "%Y-%m-%d"))

end_year <- lubridate$year(end_date)
start_year <- lubridate$year(start_date)

####### DATA #######

# GET RAW AND WRANGLE DISPLACEMENT DATA
disp_data <- raw_displacement$raw()
disp_wrangle <- wrangle_displacement$wrangle(disp_data)

# GET RAW AND WRANGLE CONFLICT DATA
conflict_data <- raw_conflict$raw()
conflict_wrangle <- wrangle_conflict$wrangle(conflict_data)

# GET RAW AND WRANGLE AGRICULTURAL DATA
agri_data <- raw_agricultural_hotspots$raw()
agri_wrangle <- wrangle_agricultural_hotspots$wrangle(agri_data)

# GET RAW AND WRANGLE FOOD INSECURITY
food_data <- raw_food_insecurity$raw()
food_wrangle <- wrangle_food_insecurity$wrangle(food_data)

# GET RAW AND WRANGLE INFORM SEVERITY INDEX
infsev_data <- raw_inform$raw()
infsev_wrangle <- wrangle_inform$wrangle(infsev_data)

# GET RAW AND WRANGLE MARKET
market_data <- raw_market_monitor$raw()
market_wrangle <- wrangle_market_monitor$wrangle(market_data)

# GET AZURE SIGNALS DATA
df <- cloud_storage$read_az_file(
  name = "output/signals.parquet",
  container = "prod"
)


# Fitler singals in chosen ndays interval
filter_signals_by_days <- function(dates, min_days = 30) {
  if (length(dates) == 0) return(logical(0))
  if (length(dates) == 1) return(TRUE)

  keep <- rep(FALSE, length(dates))
  keep[1] <- TRUE
  last_kept <- dates[1]

  for (i in 2:length(dates)) {
    if (as.numeric(dates[i] - last_kept) > min_days) {
      keep[i] <- TRUE
      last_kept <- dates[i]
    }
  }
  return(keep)
}

date_label_func <- function(dates) {
  labels <- character(length(dates))
  prev_year <- NA


  for (i in seq_along(dates)) {
    if (is.na(dates[i])) {
      labels[i] <- ""
      next
    }
    month_label <- toupper(format(dates[i], "%b"))
    year_label <- format(dates[i], "%Y")

    if (is.na(prev_year) || year_label != prev_year) {
      labels[i] <- paste0(month_label, "\n", year_label)
      prev_year <- year_label
    } else {
      labels[i] <- month_label
    }
  }
  return(labels)
}


# Extract year
df <- dplyr$mutate(df, year = as.numeric(format(df$date, "%Y")))

# Apply filter
df <- df |>
  dplyr$arrange(iso3, indicator_name, date) |>
  dplyr$group_by(iso3, indicator_name) |>
  dplyr$filter(filter_signals_by_days(date, min_days = signal_gap_days)) |>
  dplyr$ungroup()

# Find minimum common year for all indicators
first_alert <- df |>
  dplyr$group_by(indicator_name) |>
  dplyr$summarise(first_year = min(year), .groups = "drop")

# For filter
effective_start_year <- max(max(first_alert$first_year), start_year)

# Frequency distribution
df_summary_filtered <- df |>
  dplyr$group_by(indicator_name, year) |>
  dplyr$summarise(freq = dplyr$n(), .groups = "drop") |>
  dplyr$filter(year >= effective_start_year & year <= end_year)


df_dist <- df_summary_filtered |>
  dplyr$group_by(indicator_name) |>
  dplyr$summarise(total_freq = sum(freq), .groups = "drop")

indicator_labels <- c(
  "conflict" = "Conflict",
  "agricultural_hotspots" = "Agricultural\nHotspots",
  "displacement_conflict" = "Conflict\nDisplacement",
  "inform_severity" = "Inform Severity\nIndex",
  "displacement_disaster" = "Disaster\nDisplacement",
  "food_insecurity" = "Food\nInsecurity",
  "market_monitor" = "Market\nMonitor",
  "cholera" = "Cholera"
)


# Plot - Absolute frequency distributions
plot_indicator_frequency <- function(df_dist, indicator_labels) {

  p <- gg$ggplot(df_dist, gg$aes(x = reorder(indicator_name, -total_freq), y = total_freq, fill = indicator_name)) +
    gg$geom_col() +
    gg$labs(title = "Sum of indicators frequency", x = "Indicator", y = "Number of Campaigns") +
    gg$scale_x_discrete(labels = function(x) indicator_labels[x]) +
    gg$theme_minimal() +
    gg$theme(axis.text.x = gg$element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9),
             axis.text.y = gg$element_text(size = 9),
             axis.title.x = gg$element_text(size = 12, margin = gg$margin(t = 15, b = 0)),
             axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),
             plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold"),
             legend.position = "none")
}


# Plot - Frequency distribution for each indicator per year
plot_indicator_freq_year <- function(df_summary_filtered, indicator_labels) {
  p <- gg$ggplot(df_summary_filtered, gg$aes(x = factor(year), y = freq, fill = indicator_name)) +
    gg$geom_col() +
    gg$facet_wrap(~ indicator_name,
                  labeller = gg$labeller(indicator_name = indicator_labels),
                  scales = "free_y", ncol = 4, nrow = 2) +
    gg$labs(x = "Year", y = "Number of campaigns", fill = "Indicator") +
    gg$theme_minimal() +
    gg$theme(axis.text.x = gg$element_text(angle = 45, hjust = 1, size = 9),
             axis.text.y = gg$element_text(size = 9),
             axis.title.x = gg$element_text(size = 12, margin = gg$margin(t = 15, b = 0)),
             axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),
             strip.text = gg$element_text(size = 11, face = "bold"),
             legend.position = "none")
  p
}

plot_signals_overvew_pdf <- function(
  df_dist,
  df_summary_filtered,
  indicator_labels,
  effective_start_year,
  end_year,
  save_azure = TRUE,
  file_type = "pdf"
) {

  p1 <- plot_indicator_frequency(df_dist, indicator_labels)
  p2 <- plot_indicator_freq_year(df_summary_filtered, indicator_labels)

  # Combine Plots
  combined_plot <- cowplot$ggdraw() +
    cowplot$draw_plot(p1, x = 0.05, y = 0.55, width = 0.9, height = 0.35) +
    cowplot$draw_plot(p2, x = 0.05, y = 0.1, width = 0.9, height = 0.35)

  # Title
  title_plot <- cowplot$ggdraw() +
    cowplot$draw_label(paste0("Indicator Frequency Distribution (", effective_start_year, "-", end_year, ")"),
                       x = 0.5, y = 0.5,
                       size = 14,
                       fontface = "bold",
                       hjust = 0.5, vjust = 0.5) +
    gg$theme(panel.background = gg$element_rect(fill = "white", color = NA),
             plot.background = gg$element_rect(fill = "white", color = NA))

  # resize for margins
  final_plot <- cowplot$plot_grid(title_plot, combined_plot,
                                  ncol = 1,
                                  rel_heights = c(0.08, 0.92))


  gen_date  <- format(Sys.Date(), "%Y-%m-%d")

  # Local Path
  local_dir <- file.path("adhoc", paste0("signal_overview_analysis/signals_overview_", gen_date))
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

  # File Name
  file_name  <- paste0("indicator_distributions_", gen_date, ".", file_type)
  local_file <- file.path(local_dir, file_name)

  # Save plot locally
  gg$ggsave(local_file,
            plot = final_plot,
            width = ifelse(file_type == "pdf", 8.27, 800 / 96),   # A4 width in in vs px
            height = ifelse(file_type == "pdf", 11.69, 600 / 96), # A4 height in in vs px
            units = ifelse(file_type == "pdf", "in", "px"),
            device = file_type,
            useDingbats = FALSE)

  # Optionally save on Azure
  if (save_azure) {
    # Path
    azure_file <- paste0("signals_overview_analysis/signals_overview_", gen_date, "/", file_name)

    container_obj <- cloud_storage$get_container("prod")

    # Save in Prod
    invisible(
      utils$capture.output(
        az$upload_blob(
          container = container_obj,
          src = local_file,
          dest = azure_file
        )
      )
    )

    # Remove local file after upload
    file.remove(local_file)
  }
}



### FOCUS ON THE PAST N YEARS ###

# Filter data for last year
df_last_year <- df |>
  dplyr$filter(date >= start_date & date < end_date) |>
  dplyr$distinct(iso3, indicator_name, date, .keep_all = TRUE)

logger$log_info(
  "Analysis period: %s to %s",
  format(start_date, "%Y-%m-%d"),
  format(end_date, "%Y-%m-%d")
)

logger$log_info(
  "Records in period: %d",
  nrow(df_last_year)
)

# Get top locations by frequency
top3 <- df_last_year |>
  dplyr$group_by(iso3, location) |>
  dplyr$summarise(freq = dplyr$n(), .groups = "drop") |>
  dplyr$arrange(dplyr$desc(freq)) |>
  dplyr$slice_head(n = n_top_locations)

# Create dictionary ISO location for top N
top3_dict <- setNames(top3$location, top3$iso3)

# Log Info

logger$log_info("Top {n_top_locations} locations:")

for (i in seq_len(nrow(top3))) {
  logger$log_info(
    "{i}. {top3$location[i]} ({top3$iso3[i]}): {top3$freq[i]} occurrences"
  )
}

# Filter data for top locations
top3_iso3 <- top3$iso3
df_top3 <- df_last_year |>
  dplyr$filter(iso3 %in% top3_iso3)

# Summarize by location and indicator
df_top3_summary <- df_top3 |>
  dplyr$group_by(iso3, location, indicator_name) |>
  dplyr$summarise(freq = dplyr$n(), .groups = "drop") |>
  dplyr$arrange(location, dplyr$desc(freq))

# Print all indicators by location (top 3 states)
logger$log_info("All indicators by location:")

for (loc in unique(df_top3_summary$location)) {
  logger$log_info("{loc}:")

  loc_data <- df_top3_summary |>
    dplyr$filter(location == loc) |>
    dplyr$arrange(dplyr$desc(freq))

  for (i in seq_len(nrow(loc_data))) {
    logger$log_info("  {loc_data$indicator_name[i]}: {loc_data$freq[i]}")
  }
}

### FUNCTIONS PLOT INDICATORS IN THE PAST YEAR FOR THE TOP3 NATIONS ###

plot_conflict_analysis <- function(
    conflict_wrangle,
    iso3 = "MOZ",
    date_start = as.Date("2024-09-10"),
    date_end = as.Date("2025-09-10"),
    indicator_name = "conflict",
    title = "Monthly Rolling Sum of Conflict Fatalities",
    x_label = NULL,
    y_label = "Conflict Fatalities (k)",
    df_top3 = NULL,
    date_breaks = "2 months"
) {
  conflict_wrangle_filtered <- conflict_wrangle |>
    dplyr$filter(iso3 == !!iso3 & date >= !!date_start & date < !!date_end)
  p <- gg$ggplot(conflict_wrangle_filtered, gg$aes(x = date, y = fatalities_30d)) +
    gg$geom_line(color = "#0063B3", linewidth = 1) +
    gg$labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    gg$scale_y_continuous(
      labels = scales$label_number(scale_cut = scales$cut_short_scale()),
      limits = c(0, NA),
      expand = gg$expansion(mult = c(0, 0.05))
    ) +
    gg$scale_x_date(
      labels = date_label_func,
      date_breaks = date_breaks,
      expand = c(0.02, 0)
    ) +
    gg$theme_minimal() +
    gg$theme(
      plot.margin = gg$margin(t = 50, r = 10, b = 10, l = 10),
      # Simpler grid style
      panel.grid.major.x = gg$element_blank(),
      panel.grid.minor.x = gg$element_blank(),
      panel.grid.major.y = gg$element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor.y = gg$element_blank(),
      # Axis styling
      axis.text.x = gg$element_text(size = 10, color = "grey60"),
      axis.text.y = gg$element_text(size = 10, color = "grey60"),
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold", margin = gg$margin(b = 15))
    )
  if (!is.null(df_top3)) {
    conflict_filtered <- df_top3[df_top3$iso3 == iso3 & df_top3$indicator_name == indicator_name, ]
    if (nrow(conflict_filtered) > 0) {
      conflict_dates <- unique(conflict_filtered$date)
      conflict_dates <- conflict_dates[conflict_dates >= date_start & conflict_dates < date_end]
      if (length(conflict_dates) > 0) {
        # calculate offset x short date diff (less than 60 days)
        x_offsets <- rep(0, length(conflict_dates))
        if (length(conflict_dates) > 1) {
          for (i in 1:(length(conflict_dates) - 1)) {
            if (as.numeric(conflict_dates[i + 1] - conflict_dates[i]) < 60) {
              x_offsets[i] <- -15
            }
          }
        }

        p <- p + gg$geom_vline(
          xintercept = conflict_dates,
          color = "red",
          linetype = "dashed",
          alpha = 0.7
        ) +
          # Date labels - horizontal, to the left of the line
          gg$annotate(
            "text",
            x = conflict_dates + x_offsets,
            y = Inf,
            label = toupper(format(conflict_dates, "%d %b")),
            vjust = 0.5,
            hjust = 1.1,
            size = 3.5,
            color = "red",
            fontface = "bold"
          ) +
          # "Signal" label only on the first line
          gg$annotate(
            "text",
            x = conflict_dates[1] + x_offsets[1],
            y = Inf,
            label = "Signal",
            vjust = 2,
            hjust = 1.1,
            size = 3,
            color = "red",
            fontface = "bold"
          ) +
          gg$coord_cartesian(clip = "off")
      }
    }
  }
  return(p)
}



plot_displacement_analysis <- function(
  disp_wrangle,
  iso3 = "MOZ",
  date_start = as.Date("2024-09-10"),
  date_end = as.Date("2025-09-10"),
  displacement_type = "Disaster", # "Disaster" or "Conflict"
  indicator_name = NULL, # will be auto-set based on displacement_type if NULL
  title = NULL, # will be auto-generated if NULL
  x_label = NULL,
  y_label = "30-Day Rolling Sum (k)",
  df_top3 = NULL,
  use_k_scale = TRUE, # whether to scale y-axis to thousands
  date_breaks = "2 months"
) {


  # Validate displacement_type
  if (!displacement_type %in% c("Disaster", "Conflict")) {
    stop("displacement_type must be either 'Disaster' or 'Conflict'")
  }

  # Auto-set indicator_name if not provided
  if (is.null(indicator_name)) {
    indicator_name <- paste0("displacement_", tolower(displacement_type))
  }

  # Auto-generate title if not provided
  if (is.null(title)) {
    title <- paste0("7-Day Rolling Sum of ", displacement_type, "-Driven Displacements")
  }

  # Filter data for specified country and time period
  country_data <- disp_wrangle |>
    dplyr$filter(iso3 == !!iso3 & date >= !!date_start & date < !!date_end)

  # Filter by displacement type
  displacement_data <- country_data |>
    dplyr$filter(displacement_type == !!displacement_type)

  # Check if there's data for the specified displacement type
  if (nrow(displacement_data) == 0) {
    available_types <- unique(country_data$displacement_type)
    warning(paste("No", displacement_type, "displacement data found for", iso3,
                  "in the specified period. Available types:",
                  paste(available_types, collapse = ", ")))
    return(NULL)
  }

  # Create base plot
  p <- gg$ggplot(displacement_data, gg$aes(x = date, y = displacement_30d)) +
    gg$geom_line(color = "#0063B3", linewidth = 1) +
    gg$labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    gg$scale_x_date(
      labels = date_label_func,
      date_breaks = date_breaks,
      expand = c(0.02, 0)
    ) +
    gg$theme_minimal() +
    gg$theme(
      plot.margin = gg$margin(t = 50, r = 10, b = 10, l = 10),
      # Simplified grid - only major horizontal lines, lighter color
      panel.grid.major.x = gg$element_blank(),
      panel.grid.minor.x = gg$element_blank(),
      panel.grid.major.y = gg$element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor.y = gg$element_blank(),
      # Axis styling
      axis.text.x = gg$element_text(size = 10, color = "grey60"),
      axis.text.y = gg$element_text(size = 10, color = "grey60"),
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold", margin = gg$margin(b = 15)),
      # Legend styling
      legend.position = "bottom",
      legend.title = gg$element_blank()
    )

  # Apply k scaling if requested
  if (use_k_scale) {
    p <- p + gg$scale_y_continuous(
      labels = scales$label_number(scale_cut = scales$cut_short_scale())
    )
  } else {
    p <- p + gg$scale_y_continuous()
  }

  # Add vertical lines if df_top3 is provided
  if (!is.null(df_top3)) {
    displacement_filtered <- df_top3[df_top3$iso3 == iso3 & df_top3$indicator_name == indicator_name, ]
    if (nrow(displacement_filtered) > 0) {
      displacement_dates <- unique(displacement_filtered$date)
      displacement_dates <- displacement_dates[displacement_dates >= date_start & displacement_dates < date_end]
      if (length(displacement_dates) > 0) {
        # calculate offset x short date diff (less than 60 days)
        x_offsets <- rep(0, length(displacement_dates))
        if (length(displacement_dates) > 1) {
          for (i in 1:(length(displacement_dates) - 1)) {
            if (as.numeric(displacement_dates[i + 1] - displacement_dates[i]) < 60) {
              x_offsets[i] <- -15
            }
          }
        }

        p <- p + gg$geom_vline(
          xintercept = displacement_dates,
          color = "red",
          linetype = "dashed",
          alpha = 0.7
        ) +
          # Date labels - horizontal, to the left of the line
          gg$annotate(
            "text",
            x = displacement_dates + x_offsets,
            y = Inf,
            label = toupper(format(displacement_dates, "%d %b")),
            vjust = 0.5,
            hjust = 1.1,
            size = 3.5,
            color = "red",
            fontface = "bold"
          ) +
          # "Signal" label only on the first line
          gg$annotate(
            "text",
            x = displacement_dates[1] + x_offsets[1],
            y = Inf,
            label = "Signal",
            vjust = 2,
            hjust = 1.1,
            size = 3,
            color = "red",
            fontface = "bold"
          ) +
          gg$coord_cartesian(clip = "off")
      }
    }
  }
  return(p)
}


plot_agricultural_hotspot <- function(
    agri_wrangle,
    df_top3,
    iso3_code = "MOZ",
    start_date = "2024-09-10",
    end_date = "2025-09-10",
    title = "Agricultural Hotspot Timeline",
    date_breaks = "2 months"
) {
  # Input validation
  if (!is.data.frame(agri_wrangle) || !is.data.frame(df_top3)) {
    stop("agri_wrangle and df_top3 must be data.frame")
  }
  if (!iso3_code %in% agri_wrangle$iso3) {
    warning(paste("ISO3 code", iso3_code, "not found in data"))
  }
  # Filter data for country and date range
  agri_wrangle_filtered <- agri_wrangle |>
    dplyr$filter(iso3 == iso3_code &
                   date >= as.Date(start_date) &
                   date < as.Date(end_date))
  # Monthly aggregation
  agri_wrangle_monthly <- agri_wrangle_filtered |>
    dplyr$mutate(
      month = as.Date(format(date, "%Y-%m-11"))
    ) |>
    dplyr$group_by(month) |>
    dplyr$summarise(hs_code = dplyr$first(hs_code), .groups = "drop")
  # Create complete sequence of months
  start_month <- as.Date(format(as.Date(start_date), "%Y-%m-11"))
  end_month <- as.Date(format(as.Date(end_date), "%Y-%m-11"))
  all_months <- data.frame(
    month = seq(start_month, end_month, by = "month")
  )
  # Join and identify missing values
  agri_complete <- all_months |>
    dplyr$left_join(agri_wrangle_monthly, by = "month") |>
    dplyr$mutate(missing = is.na(hs_code))
  # Convert to factor with missing value handling
  agri_complete$hs_code <- factor(agri_complete$hs_code, levels = c(0, 1, 2))
  # Filter for agricultural hotspot dates
  agri_filtered <- df_top3[df_top3$iso3 == iso3_code &
                             df_top3$indicator_name == "agricultural_hotspots", ]
  agri_dates <- as.Date(unique(agri_filtered$date))
  agri_dates <- agri_dates[agri_dates >= as.Date(start_date) & agri_dates < as.Date(end_date)]
  # Calculate uniform tile width based on time range
  tile_width <- 25
  # Create the plot
  plot <- gg$ggplot(agri_complete, gg$aes(x = month, y = 1)) +
    ggpattern$geom_tile_pattern(
      gg$aes(
        fill = hs_code,
        pattern = ifelse(missing, "stripe", "none")
      ),
      color = "white",
      width = tile_width,
      height = 0.8,
      pattern_fill = "grey50",
      pattern_colour = "grey50",
      pattern_density = 0.4,
      pattern_spacing = 0.05
    ) +
    gg$scale_fill_manual(
      values = c("0" = "#A5D6A7", "1" = "#FFE082", "2" = "#8D6E63"),
      name = NULL,
      na.value = "white",
      na.translate = FALSE,
      labels = c("0" = "No hotspot", "1" = "Moderate", "2" = "Severe"),
      drop = FALSE
    ) +
    ggpattern$scale_pattern_manual(
      values = c("none" = "none", "stripe" = "stripe"),
      name = NULL,
      breaks = "stripe",
      labels = c("stripe" = "No data"),
      guide = gg$guide_legend(
        override.aes = list(
          fill = "white",
          colour = "grey70",
          pattern_fill = "grey50",
          pattern_colour = "grey50",
          pattern_density = 0.4,
          pattern_spacing = 0.05
        )
      )
    ) +
    gg$guides(
      fill = gg$guide_legend(order = 1, override.aes = list(pattern = "none")),
      pattern = gg$guide_legend(order = 2)
    ) +
    gg$scale_x_date(
      labels = date_label_func,
      date_breaks = date_breaks,
      expand = c(0.02, 0)
    ) +
    gg$coord_cartesian(
      xlim = c(start_month - 15, end_month + 15),
      ylim = c(0.4, 2),
      clip = "off"
    ) +
    gg$labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    gg$theme_minimal() +
    gg$theme(
      plot.margin = gg$margin(t = 50, r = 10, b = 10, l = 10),
      panel.grid.major.x = gg$element_blank(),
      panel.grid.minor.x = gg$element_blank(),
      panel.grid.major.y = gg$element_blank(),
      panel.grid.minor.y = gg$element_blank(),
      axis.text.x = gg$element_text(size = 10, color = "grey60"),
      axis.text.y = gg$element_blank(),
      axis.ticks.y = gg$element_blank(),
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold", margin = gg$margin(b = 15)),
      legend.position = "bottom",
      legend.justification = "center",
      legend.margin = gg$margin(t = 0, r = 0, b = 0, l = 0),
      legend.text = gg$element_text(size = 10),
      legend.title = gg$element_blank()
    )

  # Add signal lines with annotations
  if (length(agri_dates) > 0) {
    # Calcola offset x per date vicine (meno di 60 giorni)
    x_offsets <- rep(0, length(agri_dates))
    if (length(agri_dates) > 1) {
      for (i in 1:(length(agri_dates) - 1)) {
        if (as.numeric(agri_dates[i + 1] - agri_dates[i]) < 60) {
          x_offsets[i] <- -15
        }
      }
    }

    plot <- plot +
      gg$geom_vline(
        xintercept = agri_dates,
        color = "red",
        linetype = "dashed",
        alpha = 0.7
      ) +
      gg$annotate(
        "text",
        x = agri_dates + x_offsets,
        y = 1.7,
        label = toupper(format(agri_dates, "%d %b")),
        hjust = 1.1,
        size = 3.5,
        color = "red",
        fontface = "bold"
      ) +
      gg$annotate(
        "text",
        x = agri_dates[1] + x_offsets[1],
        y = 1.55,
        label = "Signal",
        hjust = 1.1,
        size = 3,
        color = "red",
        fontface = "bold"
      )
  }

  return(plot)
}


plot_inform_severity <- function(
    infsev_wrangle,
    iso3 = "HTI",
    date_start = as.Date("2024-09-10"),
    date_end = as.Date("2025-09-10"),
    indicator_name = "inform_severity",
    title = NULL,
    x_label = NULL,
    y_label = "INFORM Severity Index",
    df_top3 = NULL,
    line_color = "#0063B3",
    date_breaks = "2 months"
) {
  # Auto-generate title if not provided
  if (is.null(title)) {
    title <- paste("INFORM Severity Index in", iso3)
  }
  # Filter data for specified country and time period
  infsev_data <- infsev_wrangle |>
    dplyr$filter(iso3 == !!iso3 & date >= !!date_start & date <= !!date_end) |>
    dplyr$arrange(date) |>
    dplyr$group_by(date, iso3) |>
    dplyr$summarise(inform_severity_index = mean(inform_severity_index, na.rm = TRUE),
                    .groups = "drop") |>
    dplyr$filter(!is.na(inform_severity_index))
  # Check if there's data after filtering
  if (nrow(infsev_data) == 0) {
    warning(paste("No INFORM Severity data found for", iso3, "in the specified period"))
    return(NULL)
  }

  y_min <- min(infsev_data$inform_severity_index, na.rm = TRUE)
  y_max <- max(infsev_data$inform_severity_index, na.rm = TRUE)
  y_range <- y_max - y_min
  y_padding <- y_range * 0.2

  # Create base plot
  p <- gg$ggplot(infsev_data, gg$aes(x = date, y = inform_severity_index)) +
    gg$geom_line(color = line_color, linewidth = 1) +
    gg$labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    gg$scale_y_continuous(
      labels = scales$label_number(scale_cut = scales$cut_short_scale()),
      limits = c(max(0, y_min - y_padding), y_max + y_padding)
    ) +
    gg$scale_x_date(
      labels = date_label_func,
      date_breaks = date_breaks,
      expand = c(0.02, 0)
    ) +
    gg$theme_minimal() +
    gg$theme(
      plot.margin = gg$margin(t = 50, r = 10, b = 10, l = 10),
      panel.grid.major.x = gg$element_blank(),
      panel.grid.minor.x = gg$element_blank(),
      panel.grid.major.y = gg$element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor.y = gg$element_blank(),
      axis.text.x = gg$element_text(size = 10, color = "grey60"),
      axis.text.y = gg$element_text(size = 10, color = "grey60"),
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold", margin = gg$margin(b = 15))
    )
  # Add vertical lines if df_top3 is provided
  if (!is.null(df_top3)) {
    if (all(c("iso3", "indicator_name", "date") %in% names(df_top3))) {
      dates <- df_top3 |>
        dplyr$filter(iso3 == !!iso3 & indicator_name == !!indicator_name) |>
        dplyr$pull(date) |>
        unique()
      infsev_dates <- dates[dates >= date_start & dates <= date_end]
      if (length(infsev_dates) > 0) {
        # calculate offset x short date diff (less than 60 days)
        x_offsets <- rep(0, length(infsev_dates))
        if (length(infsev_dates) > 1) {
          for (i in 1:(length(infsev_dates) - 1)) {
            if (as.numeric(infsev_dates[i + 1] - infsev_dates[i]) < 60) {
              x_offsets[i] <- -15
            }
          }
        }

        p <- p + gg$geom_vline(
          xintercept = infsev_dates,
          color = "red",
          linetype = "dashed",
          alpha = 0.7
        ) +
          gg$annotate(
            "text",
            x = infsev_dates + x_offsets,
            y = Inf,
            label = toupper(format(infsev_dates, "%d %b")),
            vjust = 0.5,
            hjust = 1.1,
            size = 3.5,
            color = "red",
            fontface = "bold"
          ) +
          gg$annotate(
            "text",
            x = infsev_dates[1] + x_offsets[1],
            y = Inf,
            label = "Signal",
            vjust = 2,
            hjust = 1.1,
            size = 3,
            color = "red",
            fontface = "bold"
          ) +
          gg$coord_cartesian(clip = "off")
      }
    }
  }
  return(p)
}


plot_food_insecurity <- function(
    iso3_code,
    df_top3,
    food_wrangle,
    start_date,
    end_date,
    title = "IPC Food Insecurity: Population in Crisis Phases",
    date_breaks = "2 months"
) {

  # 1. INITIAL DATA FILTERING
  food_filtered <- df_top3[df_top3$iso3 == iso3_code & df_top3$indicator_name == "food_insecurity", ]
  food_dates <- as.Date(unique(food_filtered$date))
  food_dates <- food_dates[food_dates >= as.Date(start_date) & food_dates < as.Date(end_date)]

  # Filter data for the country and critical phases
  food_wrangle_country_p <- food_wrangle |>
    dplyr$filter(iso3 == iso3_code &
                   date >= as.Date(start_date) &
                   date < as.Date(end_date) &
                   phase %in% c("phase3", "phase4", "phase5"))

  # Check if data exists - return empty plot
  if (nrow(food_wrangle_country_p) == 0) {
    p <- gg$ggplot() +
      gg$labs(
        title = title,
        subtitle = "No Food Insecurity data available for this country/period",
        x = NULL,
        y = "Population (%)"
      ) +
      gg$theme_minimal() +
      gg$theme(
        plot.margin = gg$margin(t = 50, r = 10, b = 10, l = 10),
        panel.grid.major.x = gg$element_blank(),
        panel.grid.minor.x = gg$element_blank(),
        panel.grid.major.y = gg$element_line(color = "grey85", linewidth = 0.3),
        panel.grid.minor.y = gg$element_blank(),
        plot.title = gg$element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = gg$element_text(size = 10, color = "grey60"),
        axis.text.y = gg$element_text(size = 10, color = "grey60"),
        axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0))
      ) +
      gg$xlim(as.Date(start_date), as.Date(end_date)) +
      gg$ylim(0, 100)
    return(p)
  }

  # 2. FUNCTION TO BUILD DATASET FOR A SINGLE PHASE
  construct_final_dataset <- function(data) {
    current_data <- data |>
      dplyr$filter(!is.na(`percentage-current`) & !is.na(`plot_date-current`)) |>
      dplyr$arrange(`plot_date-current`) |>
      dplyr$select(plot_date = `plot_date-current`, percentage = `percentage-current`, date) |>
      dplyr$mutate(type = "current")

    last_current_date <- if (nrow(current_data) > 0) max(current_data$plot_date, na.rm = TRUE) else as.Date("1900-01-01")

    projected_data <- data |>
      dplyr$filter(!is.na(`percentage-projected`) & !is.na(`plot_date-projected`) & `plot_date-projected` > last_current_date) |>
      dplyr$group_by(`plot_date-projected`) |>
      dplyr$arrange(dplyr$desc(date)) |>
      dplyr$slice(1) |>
      dplyr$ungroup() |>
      dplyr$select(plot_date = `plot_date-projected`, percentage = `percentage-projected`, date) |>
      dplyr$mutate(type = "projected") |>
      dplyr$arrange(plot_date)

    last_projected_date <- if (nrow(projected_data) > 0) max(projected_data$plot_date, na.rm = TRUE) else last_current_date

    second_projected_data <- data |>
      dplyr$filter(!is.na(`percentage-second_projected`) & !is.na(`plot_date-second_projected`) & `plot_date-second_projected` > last_projected_date) |>
      dplyr$group_by(`plot_date-second_projected`) |>
      dplyr$arrange(dplyr$desc(date)) |>
      dplyr$slice(1) |>
      dplyr$ungroup() |>
      dplyr$select(plot_date = `plot_date-second_projected`, percentage = `percentage-second_projected`, date) |>
      dplyr$mutate(type = "second_projected") |>
      dplyr$arrange(plot_date)

    dplyr$bind_rows(current_data, projected_data, second_projected_data) |>
      dplyr$arrange(plot_date) |>
      dplyr$select(plot_date, percentage, type)
  }

  # 3. PROCESS ALL PHASES
  construct_all_phases_dataset <- function(data) {
    phases <- unique(data$phase)
    all_datasets <- lapply(phases, function(phase_name) {
      phase_final <- construct_final_dataset(data |> dplyr$filter(phase == phase_name))
      phase_final$phase <- phase_name
      phase_final
    })
    dplyr$bind_rows(all_datasets) |>
      dplyr$select(phase, plot_date, percentage, type) |>
      dplyr$arrange(phase, plot_date)
  }

  # 4. CREATE THE COMBINED DATASET
  final_dataset_all_phases <- construct_all_phases_dataset(food_wrangle_country_p)

  # 5. PREPARE DATA FOR PLOTTING
  plot_data <- final_dataset_all_phases |>
    dplyr$mutate(
      phase_label = dplyr$case_when(
        phase == "phase3" ~ "Phase 3 (Crisis)",
        phase == "phase4" ~ "Phase 4 (Emergency)",
        phase == "phase5" ~ "Phase 5 (Famine)",
        TRUE ~ phase
      )
    )

  current_data <- plot_data |> dplyr$filter(type == "current")
  projected_data <- plot_data |> dplyr$filter(type %in% c("projected", "second_projected"))

  projection_start <- if (nrow(projected_data) > 0) min(projected_data$plot_date, na.rm = TRUE) else NULL

  # Create transition points
  transition_points <- NULL
  if (nrow(current_data) > 0 && nrow(projected_data) > 0) {
    last_current_by_phase <- current_data |>
      dplyr$group_by(phase_label) |>
      dplyr$arrange(plot_date) |>
      dplyr$slice_tail(n = 1) |>
      dplyr$select(phase_label, last_current_date = plot_date, last_current_perc = percentage)

    first_projected_by_phase <- projected_data |>
      dplyr$group_by(phase_label) |>
      dplyr$arrange(plot_date) |>
      dplyr$slice_head(n = 1) |>
      dplyr$select(phase_label, first_proj_date = plot_date, first_proj_perc = percentage)

    transitions_joined <- last_current_by_phase |>
      dplyr$inner_join(first_projected_by_phase, by = "phase_label") |>
      dplyr$filter(!is.na(first_proj_date) & !is.na(last_current_date))

    if (nrow(transitions_joined) > 0) {
      transition_points <- transitions_joined |>
        dplyr$rowwise() |>
        dplyr$reframe(
          phase_label = phase_label,
          plot_date = c(last_current_date, first_proj_date),
          percentage = c(last_current_perc, first_proj_perc),
          transition_id = rep(paste(phase_label, "transition", sep = "_"), 2)
        ) |>
        dplyr$ungroup()
    }
  }

  # 6. CREATE THE PLOT
  p <- gg$ggplot(plot_data, gg$aes(x = plot_date, y = percentage, color = phase_label))

  # Add shaded area for projected period with fill aesthetic for legend
  if (!is.null(projection_start)) {
    p <- p + gg$annotate(
      "rect",
      xmin = projection_start,
      xmax = +Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey90",
      alpha = 0.5
    )
  }

  # Add lines
  if (nrow(current_data) > 0) {
    p <- p + gg$geom_line(data = current_data, gg$aes(group = phase_label), linewidth = 1.2, linetype = "solid")
  }
  if (nrow(projected_data) > 0) {
    p <- p + gg$geom_line(data = projected_data, gg$aes(group = phase_label), linewidth = 1, linetype = "dashed")
  }
  if (!is.null(transition_points) && nrow(transition_points) > 0) {
    p <- p + gg$geom_line(data = transition_points, gg$aes(group = transition_id), linewidth = 1.2, linetype = "dashed")
  }

  # Add points (with colors, but hide shape from legend)
  if (nrow(current_data) > 0) {
    p <- p + gg$geom_point(data = current_data, size = 2.5, alpha = 0.9, show.legend = FALSE)
  }
  if (nrow(projected_data) > 0) {
    p <- p + gg$geom_point(data = projected_data, size = 2.5, alpha = 0.9, show.legend = FALSE)
  }

  # Complete the plot with scales and theme
  p <- p +
    gg$scale_color_manual(
      values = c("Phase 3 (Crisis)" = "#E67800", "Phase 4 (Emergency)" = "#C80000", "Phase 5 (Famine)" = "#640000"),
      name = "IPC Phase"
    ) +
    gg$scale_x_date(labels = date_label_func, date_breaks = date_breaks) +
    gg$scale_y_continuous(limits = c(0, NA), expand = gg$expansion(mult = c(0, 0.05))) +
    gg$labs(title = title, x = NULL, y = "Population (%)") +
    gg$theme_minimal() +
    gg$theme(
      plot.margin = gg$margin(t = 50, r = 10, b = 10, l = 10),
      panel.grid.major.x = gg$element_blank(),
      panel.grid.minor.x = gg$element_blank(),
      panel.grid.major.y = gg$element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor.y = gg$element_blank(),
      axis.text.x = gg$element_text(size = 10, color = "grey60"),
      axis.text.y = gg$element_text(size = 10, color = "grey60"),
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold", margin = gg$margin(b = 15)),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = gg$element_text(size = 10),
      legend.title = gg$element_text(size = 11, face = "bold")
    ) +
    gg$guides(
      color = gg$guide_legend(order = 1, override.aes = list(linewidth = 2, linetype = "solid")),
      fill = gg$guide_legend(order = 2)
    )

  # 7. ADD SIGNAL ANNOTATIONS
  if (length(food_dates) > 0) {
    x_offsets <- rep(0, length(food_dates))
    if (length(food_dates) > 1) {
      for (i in 1:(length(food_dates) - 1)) {
        if (as.numeric(food_dates[i + 1] - food_dates[i]) < 60) {
          x_offsets[i] <- -15
        }
      }
    }

    p <- p +
      gg$geom_vline(xintercept = food_dates, color = "red", linetype = "dashed", alpha = 0.7) +
      gg$annotate("text", x = food_dates + x_offsets, y = Inf, label = toupper(format(food_dates, "%d %b")),
                  vjust = 0.5, hjust = 1.1, size = 3.5, color = "red", fontface = "bold") +
      gg$annotate("text", x = food_dates[1] + x_offsets[1], y = Inf, label = "Signal",
                  vjust = 2, hjust = 1.1, size = 3, color = "red", fontface = "bold") +
      gg$coord_cartesian(clip = "off")
  }

  last_data_point <- max(plot_data$plot_date, na.rm = TRUE)
  p <- p + gg$coord_cartesian(xlim = c(as.Date(start_date), last_data_point), clip = "off")

  return(p)
}


plot_market_change <- function(
    iso3_code,
    market_wrangle,
    df_top3,
    start_date,
    end_date,
    title,
    date_breaks = "2 months"
) {
  # Filter market data for the specified country and date range
  market_wrangle_filtered <- market_wrangle |>
    dplyr$filter(iso3 == iso3_code & date >= as.Date(start_date) & date < as.Date(end_date))
  # Remove NA values from basket_change
  market_wrangle_filtered <- market_wrangle_filtered[!is.na(market_wrangle_filtered$basket_change), ]
  # Filter df_top3 for market monitor data
  market_filtered <- df_top3[df_top3$iso3 == iso3_code & df_top3$indicator_name == "market_monitor", ]
  market_dates <- as.Date(unique(market_filtered$date))
  market_dates <- market_dates[market_dates >= as.Date(start_date) & market_dates < as.Date(end_date)]

  # Calculate min y for text positioning
  min_y <- min(market_wrangle_filtered$basket_change, na.rm = TRUE)
  text_y <- min(min_y * 0.8, -5)  # Position text in the negative area

  # Create the plot
  plot <- gg$ggplot(market_wrangle_filtered, gg$aes(x = date, y = basket_change)) +
    # Shaded area below 0
    gg$annotate(
      "rect",
      xmin = -Inf,
      xmax = +Inf,
      ymin = -Inf,
      ymax = 0,
      fill = "lightcoral",
      alpha = 0.15
    ) +
    # Label for shaded area
    gg$annotate(
      "text",
      x = as.Date(start_date) + 30,
      y = text_y,
      label = "Negative change",
      hjust = 0,
      size = 3.5,
      color = "grey40",
      fontface = "italic"
    ) +
    # Reference line at 0 (solid)
    gg$geom_hline(yintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.5) +
    # Data line and points (same color)
    gg$geom_line(color = "#0063B3", linewidth = 1) +
    gg$geom_point(color = "#0063B3", size = 2) +
    gg$labs(
      title = title,
      x = NULL,
      y = "% Change Monthly"
    ) +
    gg$scale_x_date(labels = date_label_func, date_breaks = date_breaks) +
    gg$theme_minimal() +
    gg$theme(
      plot.margin = gg$margin(t = 50, r = 10, b = 10, l = 10),
      panel.grid.major.x = gg$element_blank(),
      panel.grid.minor.x = gg$element_blank(),
      panel.grid.major.y = gg$element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor.y = gg$element_blank(),
      axis.text.x = gg$element_text(size = 10, color = "grey60"),
      axis.text.y = gg$element_text(size = 10, color = "grey60"),
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold", margin = gg$margin(b = 15))
    )

  # Add signal lines with annotations
  if (length(market_dates) > 0) {
    x_offsets <- rep(0, length(market_dates))
    if (length(market_dates) > 1) {
      for (i in 1:(length(market_dates) - 1)) {
        if (as.numeric(market_dates[i + 1] - market_dates[i]) < 60) {
          x_offsets[i] <- -15
        }
      }
    }

    plot <- plot +
      gg$geom_vline(
        xintercept = market_dates,
        color = "red",
        linetype = "dashed",
        alpha = 0.7
      ) +
      gg$annotate(
        "text",
        x = market_dates + x_offsets,
        y = Inf,
        label = toupper(format(market_dates, "%d %b")),
        vjust = 0.5,
        hjust = 1.1,
        size = 3.5,
        color = "red",
        fontface = "bold"
      ) +
      gg$annotate(
        "text",
        x = market_dates[1] + x_offsets[1],
        y = Inf,
        label = "Signal",
        vjust = 2,
        hjust = 1.1,
        size = 3,
        color = "red",
        fontface = "bold"
      ) +
      gg$coord_cartesian(clip = "off")
  }

  return(plot)
}

plot_signals_timeline <- function(
    df_top3,
    iso3_code,
    start_date,
    end_date,
    title = "Signals Overview",
    date_breaks = "2 months"
) {
  # Filter for the country and date range
  signals_data <- df_top3 |>
    dplyr$filter(iso3 == iso3_code &
                   date >= as.Date(start_date) &
                   date < as.Date(end_date))

  # Check if data exists
  if (nrow(signals_data) == 0) {
    p <- gg$ggplot() +
      gg$labs(
        title = title,
        subtitle = "No signals detected for this country/period",
        x = NULL,
        y = NULL
      ) +
      gg$theme_minimal() +
      gg$theme(
        plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = gg$element_text(hjust = 0.5, size = 11, color = "grey50")
      ) +
      gg$xlim(as.Date(start_date), as.Date(end_date))
    return(p)
  }

  # Create readable indicator labels
  signals_data <- signals_data |>
    dplyr$mutate(
      indicator_label = dplyr$case_when(
        indicator_name == "displacement_disaster" ~ "Displacement (Disaster)",
        indicator_name == "displacement_conflict" ~ "Displacement (Conflict)",
        indicator_name == "conflict" ~ "Conflict Fatalities",
        indicator_name == "agricultural_hotspots" ~ "Agricultural Hotspots",
        indicator_name == "food_insecurity" ~ "Food Insecurity",
        indicator_name == "inform_severity" ~ "INFORM Severity",
        indicator_name == "market_monitor" ~ "Market Monitor",
        TRUE ~ indicator_name
      )
    )

  # Define order for indicators (top to bottom)
  indicator_order <- c(
    "Displacement (Disaster)",
    "Displacement (Conflict)",
    "Conflict Fatalities",
    "Agricultural Hotspots",
    "Food Insecurity",
    "INFORM Severity",
    "Market Monitor"
  )

  # Filter to only include indicators present in data and set factor order
  present_indicators <- indicator_order[indicator_order %in% unique(signals_data$indicator_label)]
  signals_data$indicator_label <- factor(signals_data$indicator_label, levels = rev(present_indicators))

  # Get unique dates for vertical lines
  unique_dates <- unique(signals_data$date)

  # Create plot
  # Create plot
  p <- gg$ggplot(signals_data, gg$aes(x = date, y = indicator_label)) +
    # Points at intersection of date and indicator
    gg$geom_point(
      size = 4,
      color = "red",
      alpha = 0.9
    ) +
    # Day number near the point
    gg$geom_text(
      gg$aes(label = toupper(format(date, "%d %b"))),
      nudge_y = 0.35,
      size = 2,
      color = "red",
      fontface = "bold"
    ) +
    gg$scale_x_date(
      labels = date_label_func,
      date_breaks = date_breaks,
      limits = c(as.Date(start_date), as.Date(end_date)),
      expand = c(0.02, 0)
    ) +
    gg$labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    gg$theme_minimal() +
    gg$theme(
      plot.margin = gg$margin(t = 20, r = 10, b = 10, l = 10),
      panel.grid.major.x = gg$element_blank(),
      panel.grid.minor.x = gg$element_blank(),
      panel.grid.major.y = gg$element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor.y = gg$element_blank(),
      axis.text.x = gg$element_text(size = 10, color = "grey60"),
      axis.text.y = gg$element_text(size = 10, color = "grey30"),
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold", margin = gg$margin(b = 15))
    )

  return(p)
}


report_by_country <- function(
  top3,
  df_top3,
  start_date,
  end_date,
  years_back,
  disp_wrangle,
  conflict_wrangle,
  agri_wrangle,
  food_wrangle,
  infsev_wrangle,
  market_wrangle,
  save_azure = TRUE,
  file_type = "pdf"
) {
  # Loop through top 3 locations
  for (i in seq_len(nrow(top3))) {

    # Get current ISO3 and location
    current_iso3 <- top3$iso3[i]
    location <- top3$location[i]
    occ <- top3$freq[i]

    logger$log_info("Processing plots for: {location} ({current_iso3})")

    # Add p0
    p0 <- plot_signals_timeline(
      df_top3 = df_top3,
      iso3_code = current_iso3,
      start_date = start_date,
      end_date = end_date,
      title = "Signals Overview",
      date_breaks = "2 months"
    )

    # Create all plots
    p1 <- plot_displacement_analysis(
      disp_wrangle = disp_wrangle,
      iso3 = current_iso3,
      date_start = start_date,
      date_end = end_date,
      displacement_type = "Disaster",
      title = "30-Day Rolling Sum of Disaster-Driven Displacements",
      x_label = NULL,
      y_label = "30-Day Rolling Sum (k)",
      df_top3 = df_top3
    )

    p2 <- plot_displacement_analysis(
      disp_wrangle = disp_wrangle,
      iso3 = current_iso3,
      date_start = start_date,
      date_end = end_date,
      displacement_type = "Conflict",
      title = "30-Day Rolling Sum of Conflict-Driven Displacements",
      x_label = NULL,
      y_label = "30-Day Rolling Sum (k)",
      df_top3 = df_top3
    )

    p3 <- plot_conflict_analysis(
      conflict_wrangle = conflict_wrangle,
      iso3 = current_iso3,
      date_start = start_date,
      date_end = end_date,
      title = "Monthly Rolling Sum of Conflict Fatalities",
      x_label = NULL,
      y_label = "Conflict Fatalities (k)",
      df_top3 = df_top3
    )

    p4 <- plot_agricultural_hotspot(
      agri_wrangle = agri_wrangle,
      df_top3 = df_top3,
      iso3_code = current_iso3,
      start_date = start_date,
      end_date = end_date,
      title = "Agricultural Hotspot Timeline"
    )

    p5 <- plot_food_insecurity(
      iso3_code = current_iso3,
      df_top3 = df_top3,
      food_wrangle = food_wrangle,
      start_date = start_date,
      end_date = end_date,
      title = "Population in Food Insecurity Phases (3-5)"
    )

    p6 <- plot_inform_severity(
      infsev_wrangle = infsev_wrangle,
      iso3 = current_iso3,
      date_start = start_date,
      date_end = end_date,
      title = paste("Inform Severity Index"),
      df_top3 = df_top3
    )

    p7 <- plot_market_change(
      iso3_code = current_iso3,
      market_wrangle = market_wrangle,
      df_top3 = df_top3,
      start_date = start_date,
      end_date = end_date,
      title = paste("Monthly Percentage Basket Cost Change")
    )

    # Combined plot - simple vertical stacking
    combined_plot <- cowplot$plot_grid(
      p0, p1, p2, p3, p4, p5, p6, p7,
      nrow = 8,
      ncol = 1,
      align = "hv",
      axis = "tblr"
    )

    # Report Title
    final_title <- paste0("Signals Indicators in ", location, " (", start_year, "-", end_year, ")")

    subtitle <- paste0(occ, " Total Signals in the last ", years_back, " years")

    title_plot <- cowplot$ggdraw() +
      # Main title (centered)
      cowplot$draw_label(final_title,
                         x = 0.5, y = 0.65,
                         size = 16,
                         fontface = "bold",
                         hjust = 0.5, vjust = 0.5) +
      # Subtitle below title (centered)
      cowplot$draw_label(subtitle,
                         x = 0.5, y = 0.35,
                         size = 12,
                         fontface = "italic",
                         hjust = 0.5, vjust = 0.5,
                         color = "gray30") +
      gg$theme(panel.background = gg$element_rect(fill = "white", color = NA),
               plot.background = gg$element_rect(fill = "white", color = NA))

    # Final plot with title
    final_plot <- cowplot$plot_grid(title_plot, combined_plot,
                                    ncol = 1,
                                    rel_heights = c(0.05, 0.95))  # Less space for title

    gen_date  <- format(Sys.Date(), "%Y-%m-%d")

    # File Name
    file_name <- paste0(current_iso3, "_indicators_overview_", gen_date, ".", file_type)

    # Fix dimensions
    if (file_type == "pdf") {
      width  <- 12     # for in
      height <- 36
      units  <- "in"
    } else if (file_type == "png") {
      width  <- 800    # in px
      height <- 600
      units  <- "px"
    }

    # Save plot
    if (save_azure) {
      # Use temp file for Azure upload
      temp_file <- tempfile(fileext = paste0(".", file_type))
      gg$ggsave(temp_file,
                plot = final_plot,
                width = width,
                height = height,
                units = units,
                dpi = 300,
                device = file_type,
                useDingbats = FALSE)

      # Upload to Azure
      azure_file <- paste0("signals_overview_analysis/signals_overview_", gen_date, "/", file_name)

      container_obj <- cloud_storage$get_container("prod")

      invisible(
        utils$capture.output(
          az$upload_blob(
            container = container_obj,
            src = temp_file,
            dest = azure_file
          )
        )
      )
    } else {
      # Save locally only if not uploading to Azure
      local_dir <- file.path("adhoc", paste0("signal_overview_analysis/signals_overview_", gen_date))
      if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
      local_file <- file.path(local_dir, file_name)

      gg$ggsave(local_file,
                plot = final_plot,
                width = width,
                height = height,
                units = units,
                dpi = 300,
                device = file_type,
                useDingbats = FALSE)
    }

  }
}


Sys.setlocale("LC_TIME", "C")

plot_signals_overvew_pdf(df_dist,
                         df_summary_filtered,
                         indicator_labels,
                         effective_start_year,
                         end_year,
                         save_azure = FALSE)

report_by_country(top3,
                  df_top3,
                  start_date,
                  end_date,
                  years_back,
                  disp_wrangle,
                  conflict_wrangle,
                  agri_wrangle,
                  food_wrangle,
                  infsev_wrangle,
                  market_wrangle,
                  save_azure = FALSE)

