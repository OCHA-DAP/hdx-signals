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
  utils
)

# Global variables
years_back <- 3
n_top_locations <- 3

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


# Extract year
df <- dplyr$mutate(df, year = as.numeric(format(df$date, "%Y")))


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
  x_label = "Date",
  y_label = "Conflict Fatalities (monthly)",
  df_top3 = NULL
) {

  conflict_wrangle_filtered <- conflict_wrangle |>
    dplyr$filter(iso3 == !!iso3 & date >= !!date_start & date < !!date_end)


  p <- gg$ggplot(conflict_wrangle_filtered, gg$aes(x = date, y = fatalities_30d)) +
    gg$geom_line(color = "steelblue", linewidth = 1) +
    gg$labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    gg$scale_y_continuous() +
    gg$scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    gg$theme_minimal() +
    gg$theme(
      axis.text.x = gg$element_text(angle = 45, hjust = 1, size = 10),           # X tick labels
      axis.text.y = gg$element_text(size = 10),                                  # Y tick labels
      axis.title.x = gg$element_text(size = 12, margin = gg$margin(t = 15, b = 0)),  # X axis title spacing
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),  # Y axis title spacing
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold")       # Centered bold title
    )


  if (!is.null(df_top3)) {
    conflict_filtered <- df_top3[df_top3$iso3 == iso3 & df_top3$indicator_name == indicator_name, ]

    if (nrow(conflict_filtered) > 0) {
      conflict_dates <- unique(conflict_filtered$date)

      conflict_dates <- conflict_dates[conflict_dates >= date_start & conflict_dates < date_end]

      if (length(conflict_dates) > 0) {
        p <- p + gg$geom_vline(
          xintercept = conflict_dates,
          color = "red",
          linetype = "dashed",
          alpha = 0.7
        )
      }
    }
  }

  p
}



plot_displacement_analysis <- function(
  disp_wrangle,
  iso3 = "MOZ",
  date_start = as.Date("2024-09-10"),
  date_end = as.Date("2025-09-10"),
  displacement_type = "Disaster", # "Disaster" or "Conflict"
  indicator_name = NULL, # will be auto-set based on displacement_type if NULL
  title = NULL, # will be auto-generated if NULL
  x_label = "Date",
  y_label = "30-Day Rolling Sum (k)",
  df_top3 = NULL,
  use_k_scale = TRUE # whether to scale y-axis to thousands
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
    gg$geom_line(color = "steelblue", linewidth = 1) +
    gg$labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    gg$scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    gg$theme_minimal() +
    gg$theme(
      axis.text.x = gg$element_text(angle = 45, hjust = 1, size = 10),           # X tick labels
      axis.text.y = gg$element_text(size = 10),                                  # Y tick labels
      axis.title.x = gg$element_text(size = 12, margin = gg$margin(t = 15, b = 0)),  # X axis title spacing
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),  # Y axis title spacing
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold")       # Centered bold title
    )

  # Apply k scaling if requested
  if (use_k_scale) {
    p <- p + gg$scale_y_continuous(labels = scales$label_number(scale = 1e-3, suffix = "k"))
  } else {
    p <- p + gg$scale_y_continuous()
  }

  # Add vertical lines if df_top3 is provided
  if (!is.null(df_top3)) {
    displacement_filtered <- df_top3[df_top3$iso3 == iso3 & df_top3$indicator_name == indicator_name, ]

    if (nrow(displacement_filtered) > 0) {
      displacement_dates <- unique(displacement_filtered$date)
      # Filter dates that fall within the plot range
      displacement_dates <- displacement_dates[displacement_dates >= date_start & displacement_dates < date_end]

      if (length(displacement_dates) > 0) {
        p <- p + gg$geom_vline(
          xintercept = displacement_dates,
          color = "red",
          linetype = "dashed",
          alpha = 0.7
        )
      }
    }
  }

  p
}


plot_agricultural_hotspot <- function(
  agri_wrangle,
  df_top3,
  iso3_code = "MOZ",
  start_date = "2024-09-10",
  end_date = "2025-09-10",
  title = "Agricultural Hotspot Timeline"
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
  agri_dates <- unique(agri_filtered$date)

  # Calculate uniform tile width based on time range
  tile_width <- 25  # Width in days - adjust as needed for better spacing

  # Create the plot
  plot <- gg$ggplot(agri_complete, gg$aes(x = month, y = 1)) +
    ggpattern$geom_tile_pattern(
      gg$aes(
        fill = hs_code,
        pattern = ifelse(missing, "stripe", "none")
      ),
      color = "white",
      width = tile_width,
      height = 0.5,
      pattern_fill = "black",
      pattern_density = 0.4,
      pattern_spacing = 0.05
    ) +
    gg$geom_vline(
      xintercept = agri_dates,
      color = "red",
      linetype = "dashed",
      alpha = 0.7
    ) +
    gg$scale_fill_manual(
      values = c("0" = "lightgray", "1" = "orange", "2" = "red"),
      name = "Hotspot level",
      na.value = "white",
      labels = c("0" = "No hotspot", "1" = "Moderate", "2" = "Severe")
    ) +
    gg$guides(
      fill = gg$guide_legend(override.aes = list(pattern = "none"))
    ) +
    gg$scale_x_date(
      breaks = agri_complete$month,
      date_labels = "%b %Y",
      expand = c(0.02, 0)
    ) +
    gg$coord_cartesian(
      xlim = c(start_month - 15, end_month + 15)
    ) +
    gg$labs(
      title = title,
      x = "Month-Year",
      y = NULL
    ) +
    gg$theme_minimal() +
    gg$theme(
      axis.text.x = gg$element_text(angle = 45, hjust = 1, size = 10),           # X tick labels
      axis.text.y = gg$element_blank(),                                          # No Y labels
      axis.ticks.y = gg$element_blank(),                                         # No Y ticks
      axis.title.x = gg$element_text(size = 12, margin = gg$margin(t = 15, b = 0)),  # X axis title spacing
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold"),      # Centered bold title
      legend.position = "bottom",
      legend.text = gg$element_text(size = 10),                                  # Legend text size
      legend.title = gg$element_text(size = 11, face = "bold")                   # Legend title size
    )

  plot
}


plot_inform_severity <- function(
  infsev_wrangle,
  iso3 = "HTI",
  date_start = as.Date("2024-09-10"),
  date_end = as.Date("2025-09-10"),
  indicator_name = "inform_severity",
  title = NULL,
  x_label = "Date",
  y_label = "INFORM Severity Index",
  df_top3 = NULL,
  line_color = "steelblue"
) {

  # Auto-generate title if not provided
  if (is.null(title)) {
    title <- paste("INFORM Severity Index in", iso3)
  }

  # Filter data for specified country and time period
  infsev_data <- infsev_wrangle |>
    dplyr$filter(iso3 == !!iso3 & date >= !!date_start & date <= !!date_end) |>
    # Ensure data is properly sorted and handle potential duplicates
    dplyr$arrange(date) |>
    dplyr$group_by(date, iso3) |>
    dplyr$summarise(inform_severity_index = mean(inform_severity_index, na.rm = TRUE),
                    .groups = "drop") |>
    # Remove any remaining NA values that could cause plotting issues
    dplyr$filter(!is.na(inform_severity_index))

  # Check if there's data after filtering
  if (nrow(infsev_data) == 0) {
    warning(paste("No INFORM Severity data found for", iso3, "in the specified period"))
    return(NULL)
  }

  # Create base plot
  p <- gg$ggplot(infsev_data, gg$aes(x = date, y = inform_severity_index)) +
    gg$geom_line(color = line_color, linewidth = 1) +
    gg$labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    gg$scale_y_continuous() +
    gg$scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month",
      expand = c(0.02, 0)
    ) +
    gg$theme_minimal() +
    gg$theme(
      axis.text.x = gg$element_text(angle = 45, hjust = 1, size = 10),           # X tick labels
      axis.text.y = gg$element_text(size = 10),                                  # Y tick labels
      axis.title.x = gg$element_text(size = 12, margin = gg$margin(t = 15, b = 0)),  # X axis title spacing
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),  # Y axis title spacing
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold")       # Centered bold title
    )

  # Add vertical lines if df_top3 is provided
  if (!is.null(df_top3)) {
    if (all(c("iso3", "indicator_name", "date") %in% names(df_top3))) {
      dates <- df_top3 |>
        dplyr$filter(iso3 == !!iso3 & indicator_name == !!indicator_name) |>
        dplyr$pull(date) |>
        unique()

      infsev_filtered <- dates[dates >= date_start & dates <= date_end]


      if (length(infsev_filtered) > 0) {
        p <- p + gg$geom_vline(
          xintercept = infsev_filtered,
          color = "red",
          linetype = "dashed",
          alpha = 0.7
        )
      }
    }
  }

  p
}



plot_food_insecurity <- function(
  iso3_code,
  df_top3,
  food_wrangle,
  start_date,
  end_date,
  title = "Food Insecurity Phases: Current Data and Projections"
) {

  # 1. INITIAL DATA FILTERING
  food_filtered <- df_top3[df_top3$iso3 == iso3_code & df_top3$indicator_name == "food_insecurity", ]
  food_dates <- unique(food_filtered$date)

  # Filter data for the country and critical phases
  food_wrangle_country_p <- food_wrangle |>
    dplyr$filter(iso3 == iso3_code &
                   date >= as.Date(start_date) &
                   date < as.Date(end_date) &
                   phase %in% c("phase3", "phase4", "phase5"))

  # Check if data exists
  if (nrow(food_wrangle_country_p) == 0) {
    # Create empty dataset with correct structure instead of stopping
    empty_plot_data <- data.frame(
      phase_label = character(0),
      plot_date = as.Date(character(0)),
      percentage = numeric(0),
      type = character(0),
      line_type = character(0),
      stringsAsFactors = FALSE
    )

    # Create empty plot
    p <- gg$ggplot(empty_plot_data, gg$aes(x = plot_date, y = percentage)) +
      gg$labs(
        title = title,
        subtitle = "No Food Insecurity data available for this country/period",
        x = "Date",
        y = "Percentage (%)"
      ) +
      gg$theme_minimal() +
      gg$theme(
        plot.title = gg$element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = gg$element_text(size = 11, hjust = 0.5, color = "gray60"),
        panel.grid.minor = gg$element_blank()
      ) +
      # Set some reasonable axis limits for empty plot
      gg$xlim(as.Date(start_date), as.Date(end_date)) +
      gg$ylim(0, 100)

    return(p)
  }

  # 2. FUNCTION TO BUILD DATASET FOR A SINGLE PHASE
  construct_final_dataset <- function(data) {

    # REAL DATA (current)
    current_data <- data |>
      dplyr$filter(!is.na(`percentage-current`) & !is.na(`plot_date-current`)) |>
      dplyr$arrange(`plot_date-current`) |>
      dplyr$select(
        plot_date = `plot_date-current`,
        percentage = `percentage-current`,
        date
      ) |>
      dplyr$mutate(type = "current")

    # Find the last real date
    if (nrow(current_data) > 0) {
      last_current_date <- max(current_data$plot_date, na.rm = TRUE)
    } else {
      last_current_date <- as.Date("1900-01-01")
    }

    # PROJECTIONS (projected)
    projected_data <- data |>
      dplyr$filter(
        !is.na(`percentage-projected`) &
          !is.na(`plot_date-projected`) &
          `plot_date-projected` > last_current_date
      ) |>
      dplyr$group_by(`plot_date-projected`) |>
      dplyr$arrange(dplyr$desc(date)) |>
      dplyr$slice(1) |>
      dplyr$ungroup() |>
      dplyr$select(
        plot_date = `plot_date-projected`,
        percentage = `percentage-projected`,
        date
      ) |>
      dplyr$mutate(type = "projected") |>
      dplyr$arrange(plot_date)

    # Find the last projection date
    if (nrow(projected_data) > 0) {
      last_projected_date <- max(projected_data$plot_date, na.rm = TRUE)
    } else {
      last_projected_date <- last_current_date
    }

    # SECOND PROJECTION (second_projected)
    second_projected_data <- data |>
      dplyr$filter(
        !is.na(`percentage-projected2`) &
          !is.na(`plot_date-second_projected`) &
          `plot_date-second_projected` > last_projected_date
      ) |>
      dplyr$group_by(`plot_date-second_projected`) |>
      dplyr$arrange(dplyr$desc(date)) |>
      dplyr$slice(1) |>
      dplyr$ungroup() |>
      dplyr$select(
        plot_date = `plot_date-second_projected`,
        percentage = `percentage-projected2`,
        date
      ) |>
      dplyr$mutate(type = "second_projected") |>
      dplyr$arrange(plot_date)

    # COMBINE ALL DATA
    final_dataset <- dplyr$bind_rows(
      current_data,
      projected_data,
      second_projected_data
    ) |>
      dplyr$arrange(plot_date) |>
      dplyr$select(plot_date, percentage, type)

  }

  # 3. FUNCTION TO PROCESS ALL PHASES TOGETHER
  construct_all_phases_dataset <- function(data) {

    phases <- unique(data$phase)
    all_datasets <- list()

    for (phase_name in phases) {
      # Filter for the current phase
      phase_data <- data |> dplyr$filter(phase == phase_name)

      # Build the dataset for this phase
      phase_final <- construct_final_dataset(phase_data)

      # Add the phase column
      phase_final$phase <- phase_name

      # Add to the list
      all_datasets[[phase_name]] <- phase_final
    }

    # Combine all datasets
    combined_dataset <- dplyr$bind_rows(all_datasets) |>
      dplyr$select(phase, plot_date, percentage, type) |>
      dplyr$arrange(phase, plot_date)

  }

  # 4. CREATE THE COMBINED DATASET
  final_dataset_all_phases <- construct_all_phases_dataset(food_wrangle_country_p)

  # 5. PREPARE DATA FOR PLOTTING
  plot_data <- final_dataset_all_phases |>
    dplyr$mutate(
      # Create a variable for line type
      line_type = dplyr$case_when(
        type == "current" ~ "solid",
        type %in% c("projected", "second_projected") ~ "dashed",
        TRUE ~ "solid"
      ),
      # Create more readable labels for phases
      phase_label = dplyr$case_when(
        phase == "phase3" ~ "Phase 3 (Crisis)",
        phase == "phase4" ~ "Phase 4 (Emergency)",
        phase == "phase5" ~ "Phase 5 (Famine)",
        TRUE ~ phase
      )
    )

  # Prepare separate data for line type
  current_data <- plot_data |>
    dplyr$filter(type == "current")

  projected_data <- plot_data |>
    dplyr$filter(type %in% c("projected", "second_projected"))

  # Create transition points ONLY if we have both current and projected data
  transition_points <- NULL

  # Check if we need transition points (both current and projected data exist)
  if (nrow(current_data) > 0 && nrow(projected_data) > 0) {

    # Get last current data point for each phase
    last_current_by_phase <- current_data |>
      dplyr$group_by(phase_label) |>
      dplyr$arrange(plot_date) |>
      dplyr$slice_tail(n = 1) |>
      dplyr$select(phase_label, plot_date, percentage) |>
      dplyr$rename(last_current_date = plot_date, last_current_perc = percentage)

    # Get first projected data point for each phase
    first_projected_by_phase <- projected_data |>
      dplyr$group_by(phase_label) |>
      dplyr$arrange(plot_date) |>
      dplyr$slice_head(n = 1) |>
      dplyr$select(phase_label, plot_date, percentage) |>
      dplyr$rename(first_proj_date = plot_date, first_proj_perc = percentage)

    # Join and create transition points only where both exist
    transitions_joined <- last_current_by_phase |>
      dplyr$inner_join(first_projected_by_phase, by = "phase_label") |>
      dplyr$filter(!is.na(first_proj_date) & !is.na(last_current_date))

    # Create transition points if we have valid transitions
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

  # Add solid lines for current data if exists
  if (nrow(current_data) > 0) {
    p <- p + gg$geom_line(
      data = current_data,
      gg$aes(group = phase_label),
      linewidth = 1.2,
      linetype = "solid"
    )
  }

  # Add dashed lines for projected data if exists
  if (nrow(projected_data) > 0) {
    p <- p + gg$geom_line(
      data = projected_data,
      gg$aes(group = phase_label),
      linewidth = 1.2,
      linetype = "dashed"
    )
  }

  # Add dashed transition lines if they exist
  if (!is.null(transition_points) && nrow(transition_points) > 0) {
    p <- p + gg$geom_line(
      data = transition_points,
      gg$aes(group = transition_id),
      linewidth = 1.2,
      linetype = "dashed"
    )
  }

  vline_layer <- NULL
  if (length(food_dates) > 0) {
    vline_layer <- gg$geom_vline(
      xintercept = food_dates,
      color = "red",
      linetype = "dashed",
      alpha = 0.7
    )
  }

  # Complete the plot
  p <- p +
    # Add points to highlight data
    gg$geom_point(
      gg$aes(shape = type),
      size = 2,
      alpha = 0.8
    ) +

    # Standard IPC colors
    gg$scale_color_manual(
      values = c("Phase 3 (Crisis)" = "#E67800",
                 "Phase 4 (Emergency)" = "#C80000",
                 "Phase 5 (Famine)" = "#640000"),
      name = "IPC Phase"
    ) +

    # Point shapes
    gg$scale_shape_manual(
      values = c("current" = 16, "projected" = 17, "second_projected" = 15),
      labels = c("current" = "Current", "projected" = "Projected", "second_projected" = "Second Projection"),
      name = "Data Type"
    ) +

    # Custom labels
    gg$labs(
      title = title,
      subtitle = "Solid lines: current data | Dashed lines: projections",
      x = "Date",
      y = "Percentage (%)"
    ) +

    # Vertical lines for food_dates (only if they exist)
    vline_layer +

    # Clean theme
    gg$theme_minimal() +
    gg$theme(
      plot.title = gg$element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = gg$element_text(size = 11, hjust = 0.5, color = "gray60"),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = gg$element_blank(),
      axis.text.x = gg$element_text(angle = 45, hjust = 1)
    ) +

    # Guidelines to facilitate reading
    gg$guides(
      color = gg$guide_legend(override.aes = list(linewidth = 2, linetype = "solid")),
      shape = gg$guide_legend(override.aes = list(size = 3))
    )

  # 7. RETURN THE PLOT
  p
}


plot_market_change <- function(iso3_code, market_wrangle, df_top3, start_date, end_date, title) {

  # Filter market data for the specified country and date range
  market_wrangle_filtered <- market_wrangle |>
    dplyr$filter(iso3 == iso3_code & date >= as.Date(start_date) & date < as.Date(end_date))

  # Remove NA values from basket_change
  market_wrangle_filtered <- market_wrangle_filtered[!is.na(market_wrangle_filtered$basket_change), ]

  # Filter df_top3 for market monitor data
  market_filtered <- df_top3[df_top3$iso3 == iso3_code & df_top3$indicator_name == "market_monitor", ]
  market_dates <- unique(market_filtered$date)

  # Create the plot
  plot <- gg$ggplot(market_wrangle_filtered, gg$aes(x = date, y = basket_change)) +
    gg$geom_line() +
    gg$geom_point() +
    gg$geom_hline(yintercept = 0, linetype = "dashed", color = "violet") +
    gg$geom_vline(xintercept = market_dates, color = "red", linetype = "dashed", alpha = 0.7) +
    gg$labs(
      title = title,
      x = "Date",
      y = "% Change Monthly"
    ) +
    gg$scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    gg$theme_minimal() +
    gg$theme(
      axis.text.x = gg$element_text(angle = 45, hjust = 1, size = 10),           # X tick labels
      axis.text.y = gg$element_text(size = 10),                                  # Y tick labels
      axis.title.x = gg$element_text(size = 12, margin = gg$margin(t = 15, b = 0)),  # X axis title spacing
      axis.title.y = gg$element_text(size = 12, margin = gg$margin(r = 15, l = 0)),  # Y axis title spacing
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold")       # Centered bold title
    )
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

    # Create all plots
    p1 <- plot_displacement_analysis(
      disp_wrangle = disp_wrangle,
      iso3 = current_iso3,
      date_start = start_date,
      date_end = end_date,
      displacement_type = "Disaster",
      title = "30-Day Rolling Sum of Disaster-Driven Displacements",
      x_label = "Date",
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
      x_label = "Date",
      y_label = "30-Day Rolling Sum (k)",
      df_top3 = df_top3
    )

    p3 <- plot_conflict_analysis(
      conflict_wrangle = conflict_wrangle,
      iso3 = current_iso3,
      date_start = start_date,
      date_end = end_date,
      title = "Monthly Rolling Sum of Conflict Fatalities",
      x_label = "Date",
      y_label = "Conflict Fatalities (monthly)",
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
      title = "Food Insecurity Phases"
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
      p1, p2, p3, p4, p5, p6, p7,
      nrow = 7,
      ncol = 1,
      align = "hv",
      axis = "tblr"
    )

    # Report Title
    final_title <- paste0("Signals Indicators in ", location, " (", start_year, "-", end_year, ")")

    subtitle <- paste0("Number of Signals in the last ", years_back, " years: ", occ)

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
      height <- 24
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
                         save_azure = TRUE)

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
                  save_azure = TRUE)
