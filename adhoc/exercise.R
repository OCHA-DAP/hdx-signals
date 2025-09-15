install.packages("ggplot2")

box::use(
  src/utils/cloud_storage,
  src/utils/get_env,
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
  magrittr[`%>%`],
  gg = ggplot2,
  ggpattern,
  lubridate,
  scales,
  tidyr,
  cowplot
)

# Global variables
START_DATE <- as.Date("2024-09-10")
END_DATE <- as.Date("2025-09-10")
N_TOP_LOCATIONS <- 3

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
  name='output/signals.parquet',
  container='prod'
)

# Extract year
df <- dplyr$mutate(df, year = as.numeric(format(df$date, "%Y")))


# Find minimum common year for all indicators
first_alert <- df %>%
  dplyr$group_by(indicator_name) %>%
  dplyr$summarise(first_year = min(year), .groups = "drop")

# Frequency distribution
df_summary_filtered <- df %>%
  dplyr$group_by(indicator_name, year) %>%
  dplyr$summarise(freq = dplyr$n(), .groups = "drop") %>%
  dplyr$filter(year >= max(first_alert$first_year))

# Relative frequencies
df_summary_pct <- df_summary_filtered %>%
  dplyr$group_by(indicator_name) %>%
  dplyr$summarise(freq_total = sum(freq), .groups = "drop") %>%
  dplyr$mutate(percent = 100 * freq_total / sum(freq_total))

# Plot 1 - Relative frequency of indicators from 2020 (aggregated)
p1 <- gg$ggplot(df_summary_pct, gg$aes(x = reorder(indicator_name, -percent), y = percent, fill = indicator_name)) +
  gg$geom_col() +
  gg$labs(title = "Relative frequency of indicators (from 2020)", x = "Indicator", y = "Percentage (%)") +
  gg$theme_minimal() +
  gg$theme(axis.text.x = gg$element_text(angle = 45, hjust = 1),
           legend.position = "none")

# print(p1)

# Plot 2 - Frequency distribution for each indicator per year
p2 <- gg$ggplot(df_summary_filtered, gg$aes(x = factor(year), y = freq, fill = indicator_name)) +
  gg$geom_col() +
  gg$facet_wrap(~ indicator_name, scales = "free_y", ncol = 4, nrow = 2) +
  gg$labs(x = "Year", y = "Frequency", fill = "Indicator") +
  gg$theme_minimal() +
  gg$theme(
    axis.text.x = gg$element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# print(p2)


combined_plot <- cowplot::plot_grid(p1, p2,
                                    nrow = 2,
                                    ncol = 1,
                                    align = "v")

# Save to PNG
gg$ggsave("indicators_fr_dist.png",
          plot = combined_plot,
          width = 8,
          height = 10,
          dpi = 300,
          units = "in")


### FOCUS ON THE PAST YEAR ###

# Filter data for last year
df_last_year <- df %>%
  dplyr::filter(date >= START_DATE & date < END_DATE) %>%
  dplyr::distinct(iso3, indicator_name, date, .keep_all = TRUE)

cat("Analysis period:", format(START_DATE, "%Y-%m-%d"), "to", format(END_DATE, "%Y-%m-%d"), "\n")
cat("Records in period:", nrow(df_last_year), "\n\n")

# Get top locations by frequency
top3 <- df_last_year %>%
  dplyr::group_by(iso3, location) %>%
  dplyr::summarise(freq = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(freq)) %>%
  dplyr::slice_head(n = N_TOP_LOCATIONS)

# Create dictionary ISO location for top N
top3_dict <- setNames(top3$location, top3$iso3)

# Print top locations
cat("\nTop", N_TOP_LOCATIONS, "locations:\n")
for(i in 1:nrow(top3)) {
  cat(sprintf("%d. %s (%s): %d occurrences\n", i, top3$location[i], top3$iso3[i], top3$freq[i]))
}
cat("\n")

# Filter data for top locations
top3_iso3 <- top3$iso3
df_top3 <- df_last_year %>%
  dplyr::filter(iso3 %in% top3_iso3)

# Summarize by location and indicator
df_top3_summary <- df_top3 %>%
  dplyr::group_by(iso3, location, indicator_name) %>%
  dplyr::summarise(freq = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(location, dplyr::desc(freq))

# Print all indicators by location (top 3 states)
cat("All indicators by location:\n")
for(loc in unique(df_top3_summary$location)) {
  cat(loc, ":\n")
  loc_data <- df_top3_summary %>%
    dplyr::filter(location == loc) %>%
    dplyr::arrange(dplyr::desc(freq))

  for(i in 1:nrow(loc_data)) {
    cat("  ", loc_data$indicator_name[i], ":", loc_data$freq[i], "\n")
  }
  cat("\n")
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

  conflict_wrangle_filtered <- conflict_wrangle %>%
    dplyr::filter(iso3 == !!iso3 & date >= !!date_start & date < !!date_end)


  p <- gg$ggplot(conflict_wrangle_filtered, gg$aes(x = date, y = fatalities_30d)) +
    gg$geom_line(color = "steelblue", linewidth = 1) +
    gg$labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    gg$scale_y_continuous() +
    gg$scale_x_date(
      date_labels = "%b %Y",   # mostra mese + anno
      date_breaks = "1 month"  # tick ogni mese
    ) +
    gg$theme_minimal() +
    gg$theme(
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = gg$element_text(angle = 45, hjust = 1)
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
  country_data <- disp_wrangle %>%
    dplyr::filter(iso3 == !!iso3 & date >= !!date_start & date < !!date_end)

  # # Check if there's data after filtering
  # if (nrow(country_data) == 0) {
  #   warning(paste("No data found for", iso3, "in the specified period"))
  #   return(NULL)
  # }

  # Filter by displacement type
  displacement_data <- country_data %>%
    dplyr::filter(displacement_type == !!displacement_type)

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
    gg$theme_minimal() +
    gg$theme(
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = gg$element_text(angle = 45, hjust = 1)
    )

  # Apply k scaling if requested
  if (use_k_scale) {
    p <- p + gg$scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k"))
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

  return(p)
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
  agri_wrangle_filtered <- agri_wrangle %>%
    dplyr::filter(iso3 == iso3_code &
                    date >= as.Date(start_date) &
                    date < as.Date(end_date))

  # Monthly aggregation
  agri_wrangle_monthly <- agri_wrangle_filtered %>%
    dplyr::mutate(
      month = as.Date(format(date, "%Y-%m-11"))
    ) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(hs_code = dplyr::first(hs_code), .groups = "drop")

  # Create complete sequence of months
  start_month <- as.Date(format(as.Date(start_date), "%Y-%m-11"))
  end_month <- as.Date(format(as.Date(end_date), "%Y-%m-11"))
  all_months <- data.frame(
    month = seq(start_month, end_month, by = "month")
  )

  # Join and identify missing values
  agri_complete <- all_months %>%
    dplyr::left_join(agri_wrangle_monthly, by = "month") %>%
    dplyr::mutate(missing = is.na(hs_code))

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
    ggpattern::geom_tile_pattern(
      gg$aes(
        fill = hs_code,
        pattern = ifelse(missing, "stripe", "none")
      ),
      color = "white",
      width = tile_width,  # Fixed width for uniform spacing
      height = 0.5,
      pattern_fill = "black",
      pattern_density = 0.4,
      pattern_spacing = 0.05
    ) +
    gg$geom_vline(
      xintercept = agri_dates,  # Convert dates to numeric
      color = "red",
      linetype = "dashed",
      alpha = 0.7
    ) +
    gg$scale_fill_manual(
      values = c("0" = "lightgray", "1" = "orange", "2" = "red"),
      name = "Hotspot level",
      na.value = "white",  # Missing data appaiono bianchi con strisce
      labels = c("0" = "No hotspot", "1" = "Moderate", "2" = "Severe")
    ) +
    gg$guides(
      fill = gg$guide_legend(override.aes = list(pattern = "none"))  # Rimuovi pattern dalla legenda
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
      axis.text.y = gg$element_blank(),
      axis.ticks.y = gg$element_blank(),
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = gg$element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  return(plot)
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
  infsev_data <- infsev_wrangle %>%
    dplyr::filter(iso3 == !!iso3 & date >= !!date_start & date <= !!date_end) %>%
    # Ensure data is properly sorted and handle potential duplicates
    dplyr::arrange(date) %>%
    dplyr::group_by(date, iso3) %>%
    dplyr::summarise(inform_severity_index = mean(inform_severity_index, na.rm = TRUE),
                     .groups = 'drop') %>%
    # Remove any remaining NA values that could cause plotting issues
    dplyr::filter(!is.na(inform_severity_index))

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
      minor_breaks = "1 week",
      expand = c(0.02, 0)
    ) +
    gg$theme_minimal() +
    gg$theme(
      plot.title = gg$element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = gg$element_text(angle = 45, hjust = 1)
    )

  # Add vertical lines if df_top3 is provided
  if (!is.null(df_top3)) {
    if (all(c("iso3", "indicator_name", "date") %in% names(df_top3))) {
      infsev_filtered <- df_top3 %>%
        dplyr::filter(iso3 == !!iso3 & indicator_name == !!indicator_name) %>%
        dplyr::pull(date) %>%
        unique() %>%
        .[. >= date_start & . <= date_end]

      if (length(infsev_filtered) > 0) {
        p <- p + gg$geom_vline(
          xintercept = infsev_filtered,
          color = "red",
          linetype = "dashed",
          alpha = 0.7
        )
      }
    } else {
      warning("df_top3 does not contain required columns: iso3, indicator_name, date")
    }
  }

  return(p)
}



plot_food_insecurity <- function(iso3_code, df_top3, food_wrangle, start_date, end_date, title = "Food Insecurity Phases: Current Data and Projections") {

  # require(dplyr)

  # 1. INITIAL DATA FILTERING
  food_filtered <- df_top3[df_top3$iso3 == iso3_code & df_top3$indicator_name == "food_insecurity", ]
  food_dates <- unique(food_filtered$date)

  # Filter data for the country and critical phases
  food_wrangle_country_p <- food_wrangle %>%
    dplyr::filter(iso3 == iso3_code &
                    date >= as.Date(start_date) &
                    date < as.Date(end_date) &
                    phase %in% c("phase3", "phase4", "phase5"))

  # # Check if data exists
  # if (nrow(food_wrangle_country_p) == 0) {
  #   stop("No data found for the specified parameters")
  # }

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
    current_data <- data %>%
      dplyr::filter(!is.na(`percentage-current`) & !is.na(`plot_date-current`)) %>%
      dplyr::arrange(`plot_date-current`) %>%
      dplyr::select(
        plot_date = `plot_date-current`,
        percentage = `percentage-current`,
        date
      ) %>%
      dplyr::mutate(type = "current")

    # Find the last real date
    if (nrow(current_data) > 0) {
      last_current_date <- max(current_data$plot_date, na.rm = TRUE)
    } else {
      last_current_date <- as.Date("1900-01-01")
    }

    # PROJECTIONS (projected)
    projected_data <- data %>%
      dplyr::filter(
        !is.na(`percentage-projected`) &
          !is.na(`plot_date-projected`) &
          `plot_date-projected` > last_current_date
      ) %>%
      dplyr::group_by(`plot_date-projected`) %>%
      dplyr::arrange(desc(date)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        plot_date = `plot_date-projected`,
        percentage = `percentage-projected`,
        date
      ) %>%
      dplyr::mutate(type = "projected") %>%
      dplyr::arrange(plot_date)

    # Find the last projection date
    if (nrow(projected_data) > 0) {
      last_projected_date <- max(projected_data$plot_date, na.rm = TRUE)
    } else {
      last_projected_date <- last_current_date
    }

    # SECOND PROJECTION (second_projected)
    second_projected_data <- data %>%
      dplyr::filter(
        !is.na(`percentage-second_projected`) &
          !is.na(`plot_date-second_projected`) &
          `plot_date-second_projected` > last_projected_date
      ) %>%
      dplyr::group_by(`plot_date-second_projected`) %>%
      dplyr::arrange(desc(date)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        plot_date = `plot_date-second_projected`,
        percentage = `percentage-second_projected`,
        date
      ) %>%
      dplyr::mutate(type = "second_projected") %>%
      dplyr::arrange(plot_date)

    # COMBINE ALL DATA
    final_dataset <- dplyr::bind_rows(
      current_data,
      projected_data,
      second_projected_data
    ) %>%
      dplyr::arrange(plot_date) %>%
      dplyr::select(plot_date, percentage, type)

    return(final_dataset)
  }

  # 3. FUNCTION TO PROCESS ALL PHASES TOGETHER
  construct_all_phases_dataset <- function(data) {

    phases <- unique(data$phase)
    all_datasets <- list()

    for (phase_name in phases) {
      # Filter for the current phase
      phase_data <- data %>% dplyr::filter(phase == phase_name)

      # Build the dataset for this phase
      phase_final <- construct_final_dataset(phase_data)

      # Add the phase column
      phase_final$phase <- phase_name

      # Add to the list
      all_datasets[[phase_name]] <- phase_final
    }

    # Combine all datasets
    combined_dataset <- dplyr::bind_rows(all_datasets) %>%
      dplyr::select(phase, plot_date, percentage, type) %>%
      dplyr::arrange(phase, plot_date)

    return(combined_dataset)
  }

  # 4. CREATE THE COMBINED DATASET
  final_dataset_all_phases <- construct_all_phases_dataset(food_wrangle_country_p)

  # 5. PREPARE DATA FOR PLOTTING
  plot_data <- final_dataset_all_phases %>%
    dplyr::mutate(
      # Create a variable for line type
      line_type = dplyr::case_when(
        type == "current" ~ "solid",
        type %in% c("projected", "second_projected") ~ "dashed",
        TRUE ~ "solid"
      ),
      # Create more readable labels for phases
      phase_label = dplyr::case_when(
        phase == "phase3" ~ "Phase 3 (Crisis)",
        phase == "phase4" ~ "Phase 4 (Emergency)",
        phase == "phase5" ~ "Phase 5 (Famine)",
        TRUE ~ phase
      )
    )

  # Prepare separate data for line type
  current_data <- plot_data %>%
    dplyr::filter(type == "current")

  projected_data <- plot_data %>%
    dplyr::filter(type %in% c("projected", "second_projected"))

  # Create transition points ONLY if we have both current and projected data
  transition_points <- NULL

  # Check if we need transition points (both current and projected data exist)
  if (nrow(current_data) > 0 && nrow(projected_data) > 0) {

    # Get last current data point for each phase
    last_current_by_phase <- current_data %>%
      dplyr::group_by(phase_label) %>%
      dplyr::arrange(plot_date) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::select(phase_label, plot_date, percentage) %>%
      dplyr::rename(last_current_date = plot_date, last_current_perc = percentage)

    # Get first projected data point for each phase
    first_projected_by_phase <- projected_data %>%
      dplyr::group_by(phase_label) %>%
      dplyr::arrange(plot_date) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::select(phase_label, plot_date, percentage) %>%
      dplyr::rename(first_proj_date = plot_date, first_proj_perc = percentage)

    # Join and create transition points only where both exist
    transitions_joined <- last_current_by_phase %>%
      dplyr::inner_join(first_projected_by_phase, by = "phase_label") %>%
      dplyr::filter(!is.na(first_proj_date) & !is.na(last_current_date))

    # Create transition points if we have valid transitions
    if (nrow(transitions_joined) > 0) {
      transition_points <- transitions_joined %>%
        dplyr::rowwise() %>%
        dplyr::reframe(
          phase_label = phase_label,
          plot_date = c(last_current_date, first_proj_date),
          percentage = c(last_current_perc, first_proj_perc),
          transition_id = rep(paste(phase_label, "transition", sep = "_"), 2)
        ) %>%
        dplyr::ungroup()
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
    {if(length(food_dates) > 0) gg$geom_vline(xintercept = food_dates, color = "red", linetype = "dashed", alpha = 0.7)} +

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
  return(p)
}


plot_market_change <- function(iso3_code, market_wrangle, df_top3, start_date, end_date, title) {

  # Filter market data for the specified country and date range
  market_wrangle_filtered <- market_wrangle %>%
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
    gg$labs(title = title,
            x = "Date",
            y = "% Change Monthly") +
    gg$scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    gg$theme_minimal() +
    gg$theme(axis.text.x = gg$element_text(angle = 45, hjust = 1)) +
    gg$geom_vline(xintercept = market_dates, color = "red", linetype = "dashed", alpha = 0.7)

  return(plot)
}


############### PRINT ###############


# cat("Analysis period:", format(START_DATE, "%Y-%m-%d"), "to", format(END_DATE, "%Y-%m-%d"), "\n")
# cat("Records in period:", nrow(df_last_year), "\n\n")
#
# # Print top locations
# cat("\nTop", N_TOP_LOCATIONS, "locations:\n")
# for(i in 1:nrow(top3)) {
#   cat(sprintf("%d. %s (%s): %d occurrences\n", i, top3$location[i], top3$iso3[i], top3$freq[i]))
# }
# cat("\n")
#
# # Print all indicators by location (top 3 states)
# cat("All indicators by location:\n")
# for(loc in unique(df_top3_summary$location)) {
#   cat(loc, ":\n")
#   loc_data <- df_top3_summary %>%
#     dplyr::filter(location == loc) %>%
#     dplyr::arrange(dplyr::desc(freq))
#
#   for(i in 1:nrow(loc_data)) {
#     cat("  ", loc_data$indicator_name[i], ":", loc_data$freq[i], "\n")
#   }
#   cat("\n")
# }
#
# cat("Saved: 'indicators_fr_dist.png' \n\n")


# Loop through top 3 locations
for(i in 1:nrow(top3)) {

  # Get current ISO3 and location
  ISO3 <- top3$iso3[i]
  location <- top3$location[i]

  cat("Processing plots for:", location, "(", ISO3, ")\n")

  # Create all plots
  p1 <- plot_displacement_analysis(
    disp_wrangle = disp_wrangle,
    iso3 = ISO3,
    date_start = START_DATE,
    date_end = END_DATE,
    displacement_type = "Disaster",
    title = paste("30-Day Rolling Sum of Disaster-Driven Displacements in", location),
    x_label = "Date",
    y_label = "30-Day Rolling Sum (k)",
    df_top3 = df_top3
  )

  p2 <- plot_displacement_analysis(
    disp_wrangle = disp_wrangle,
    iso3 = ISO3,
    date_start = START_DATE,
    date_end = END_DATE,
    displacement_type = "Conflict",
    title = paste("30-Day Rolling Sum of Conflict-Driven Displacements in", location),
    x_label = "Date",
    y_label = "30-Day Rolling Sum (k)",
    df_top3 = df_top3
  )

  p3 <- plot_conflict_analysis(
    conflict_wrangle = conflict_wrangle,
    iso3 = ISO3,
    date_start = START_DATE,
    date_end = END_DATE,
    title = paste("Monthly Rolling Sum of Conflict Fatalities in", location),
    x_label = "Date",
    y_label = "Conflict Fatalities (monthly)",
    df_top3 = df_top3
  )

  p4 <- plot_agricultural_hotspot(
    agri_wrangle = agri_wrangle,
    df_top3 = df_top3,
    iso3_code = ISO3,
    start_date = START_DATE,
    end_date = END_DATE,
    title = paste("Agricultural Hotspot Timeline in", location)
  )

  p5 <- plot_food_insecurity(
    iso3_code = ISO3,
    df_top3 = df_top3,
    food_wrangle = food_wrangle,
    start_date = "2020-09-10",
    end_date = END_DATE,
    title = paste("Food Insecurity Phases in", location, ": Current Data and Projections")
  )

  p6 <- plot_inform_severity(
    infsev_wrangle = infsev_wrangle,
    iso3 = ISO3,
    date_start = START_DATE,
    date_end = END_DATE,
    title = paste("INFORM Severity Index in", location),
    df_top3 = df_top3
  )

  p7 <- plot_market_change(
    iso3_code = ISO3,
    market_wrangle = market_wrangle,
    df_top3 = df_top3,
    start_date = START_DATE,
    end_date = END_DATE,
    title = paste('Monthly Percentage Basket Cost Change in', location)
  )

  combined_plot <- cowplot$plot_grid(
    p1, p2, p3, p4, p5, p6, p7,
    nrow = 7,
    ncol = 1,
    align = "v"
  )

  # Create filename
  filename <- paste0(ISO3, "_indicators_past_year.png")

  # Save combined plot
  gg$ggsave(filename,
            plot = combined_plot,
            width = 12,
            height = 20,
            dpi = 300,
            units = "in")

  cat("Saved:", filename, "\n\n")
}

