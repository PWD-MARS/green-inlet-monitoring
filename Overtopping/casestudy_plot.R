library(ggplot2)
library(gridExtra)

marsRainfallPlot <- function(dtime, rainfall_in, event, reverse_y = FALSE) {
  #### Should this be based on detect events? Doesn't fetchRainfall give this info?
  #### There needs to be a pre-process step to select a single event.
  
  # Data validation
  if(length(dtime) != length(rainfall_in)) {
    stop("Datetime and rainfall lengths must be equal")
  }
  
  if(length(event) > 1) {
    stop("Argument 'event' must be of length 1")
  }
  
  # Combine into dataframe
  rain_data <- data.frame(dtime,
                          rainIN = rainfall_in) %>% 
    dplyr::arrange(dtime)
  ### Shouldn't this happen before we combine into a df?
  if(nrow(rain_data) == 0) {
    stop("No data loaded")
  }
  
  # Minimum interval is 15 min
  min_interval <- lubridate::minutes(15)
  
  # Add cumulative rainfall to df
  #### Should we have culmuative amount of rain in inches or percent of rain from a storm?
  rain_data <- rain_data %>% dplyr::mutate(cumulative = cumsum(rainIN))
  # Generate title block
  startdate <- min(rain_data$dtime) - min_interval
  title_text <- paste0("Hyetograph\n| Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::label_time("%Y-%m-%d %H:%M", tz = "America/New_York")(startdate),
                       sep = "")
  
  # Shift dtime so column shows rainfall between interval
  rain_data$dtime <- rain_data$dtime - min_interval
  
  # Calculate plotting parameters
  min_date <- min(rain_data$dtime, na.rm = TRUE)
  max_date <- max(rain_data$dtime, na.rm = TRUE)
  min_rain <- 0
  max_rain <- max(rain_data$rainIN, na.rm = TRUE)
  # calculate scaling factor for secondary y-axis for cumulative rainfall
  #### Both axis are for inches of rainfall but they have 2 diff scales. Discrete vs Cumulative
  #### This makes it really hard to understand and I would recommend changing the cumulative
  #### to % of storm total and then specifically reference the size of the storm.
  #### Why are we calculating the scaling factor like this? Why 110%? This done outside of scale
  max_cumulative_scaling <- max(1.1*rain_data$cumulative, na.rm = TRUE)/max_rain
  
  #Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date
  range_rainfall <- max_rain - min_rain
  
  # If rainfall range is < 0.1, set as max and recalculate cumulative scale
  if(range_rainfall < 0.1) {
    max_rain <- 0.1 
    #### This should always be 0.1 right?
    range_rainfall <- max_rain - min_rain
    max_cumulative_scaling<- max(1.1*rain_data$cumulative, na.rm = TRUE)/max_rain
  }
  
  # Scale fix for events with only one measurement interval
  #### This function is for a single event. Why are we plotting a single observation?
  if(nrow(rain_data) == 1) {
    max_cumulative_scaling <- max(rain_data$cumulative, na.rm = TRUE)/max_rain
  }
  
  # Calculate break intervals for y-axis
  #### Should these be hard-coded? Clean up with switch?
  if (range_rainfall > 0.5) {
    rain_major_interval <- 0.2
    rain_minor_interval <- 0.1
  } else {
    if(range_rainfall > 0.2) {
      rain_major_interval <- 0.1
      rain_minor_interval <- 0.05
    } else {
      rain_major_interval <- 0.05
      rain_minor_interval <- 0.01
    }}
  # Calculate major break intervals for x-axis
  #### This needs to be simplified. 
  # Event duration is < 4 days
  if(units(event_duration) == "days" & event_duration < 4) {
    # Set to 12-hour intervals
    x <- "12 hours"
  } else {
    # If duration is >= 4 days
    if(units(event_duration) == "days" & event_duration >= 4) {
      # Set x-axis major breaks to 1/4 days of the event duration
      x <- paste0(floor(event_duration/4)," days")
    } else {
      # If event duration is between 12 and 24hrs
      if(event_duration > 12) {
        # Set to 6hr interval
        x <- "6 hours"
      } else {
        # If the event duration is between 8 and 12hrs
        if(event_duration > 8) {
          # Set to 2hr interval
          x <- "2 hours"
          # Any duration < 8hrs
        } else {
          # Set to 1hr interval
          x <- "hour"
        }
      }
    }
  }
  # Calculations for dashed vertical line at day boundaries
  #### This seems a bit strange. I'm guessing we want to go from min day to min day with 00:00:00
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "America/New_York"),
                                               by = "day", length.out = 14),
                                    tz = "America/New_York")
  
  # Calculate axis breaks based on plotting limits
  # Select major x-axis breaks based on event duration
  major_date_breaks <- seq.POSIXt(day_marker[1], max_date, by = x)
  
  # Set minor x-axis breaks as 1hr
  minor_date_breaks <- seq.POSIXt(day_marker[1], max_date + lubridate::hours(6), by = "hour")
  
  # Add row for cumulative rainfall
  #### Why are we doing this? Replacing the last two values with new data?
  end <- data.frame(dtime = c(max_date - min_interval, max_date),
                    rainIN = c(0,0),
                    cumulative = c(max(rain_data$cumulative), max(rain_data$cumulative)))
  rain_data <- rbind(rain_data, end)
  
  # Determine scale function
  if (reverse_y == TRUE ) {
    y_scale_function <- ggplot2::scale_y_reverse
  } else {
    y_scale_function <- ggplot2::scale_y_continuous
  }
  # Filter by unique datetime.
  #### Why are we doing this? We essentially replaced the last two values, and then
  #### this gets rid of them? This will cause potentially unexpected results.
  rain_data <- rain_data %>% dplyr::distinct(dtime, .keep_all = TRUE)
  
  # Plot Hyteograph
  hyetograph <-
    ggplot2::ggplot(data = rain_data,
                    ggplot2::aes(x = dtime,
                                 y = cumulative/max_cumulative_scaling)) +
    # Culmulative Rainfall
    ggplot2::geom_area(ggplot2::aes(fill = "  Cumulative Rainfall    "),
                       color = "grey32",
                       alpha = 0.2) +
    # Rainfall
    ggplot2::geom_bar(data = rain_data,
                      ggplot2::aes(x = dtime,
                                   y = rainIN,
                                   fill = "  Rainfall"),
                      stat = "identity") +
    # Scale
    ggplot2::scale_fill_manual(values = c("slateblue1",
                                          "cornflowerblue"),
                               guide = ggplot2::guide_legend(title = NULL,
                                                             override.aes = list(
                                                               alpha = c(0.2,1)))) +
    
    # Day boundaries
    ggplot2::geom_vline(xintercept = day_marker,
                        color = "black",
                        linetype = "dashed",
                        linewidth = 1.2) +
    # Use B/W theme
    ggplot2::theme_bw() +
    # Scales
    ggplot2::scale_x_datetime(
      name = " ",
      labels = scales::date_format("%H:%M", "America/New_York"),
      limits = c(min_date - min_interval, max_date),
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks
    ) +
    y_scale_function(
      breaks = seq(min_rain, max_rain, by = rain_major_interval),
      minor_breaks = seq(min_rain, max_rain, by = rain_minor_interval),
      sec.axis = ggplot2::sec_axis(~.*max_cumulative_scaling, name = "Cumulative Rainfall (in)")) +
    ggplot2::labs(
      y = "Rainfall (in)",
      title = title_text) +
    # Plot Theme
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14, color = "black"),
      axis.text.y = ggplot2::element_text(size = 14, color = "black"),
      panel.background =  ggplot2::element_rect(fill = "white", colour = NA),
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"),
      panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2),
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 10),
      legend.title=ggplot2::element_blank()
    )
}

marsWaterLevelPlot <- function(event, 
                               structure_name, 
                               storage_depth_ft, 
                               obs_datetime, 
                               obs_level_ft,
                               level_names = NULL,
                               orifice_show = FALSE,
                               orifice_height_ft = NULL,
                               datetime_2 = NA,
                               level_ft_2 = NA,
                               datetime_3 = NA,
                               level_ft_3 = NA,
                               datetime_4 = NA,
                               level_ft_4 = NA,
                               metrics_show = FALSE,
                               obs_RSPU,
                               obs_infil_inhr,
                               obs_draindown_hr,
                               obs_overtopping,
                               overflow_label,
                               installation_height_ft) {
  
  #### Why are there more arguments than noted? Also, why so many?
  # Confirm that storage depth is explicitly defined
  if(!is.numeric(storage_depth_ft) | is.na(storage_depth_ft)) {
    stop("storage_depth is not numeric.")
  }
  # Set negative water levels to zero
  obs_level_ft[which(obs_level_ft < 0)] <- 0
  
  # Check that there is at least 1 level observation
  #### Do we really want to do this analysis with 1 level observation?
  if(length(obs_level_ft) == 0){
    stop(paste0("No data loaded in observed Event", event, "."))
  }
  
  #### There are no checks for any other data values. You can have level without any datetime
  # Check to make sure no gaps beyond 6 hours in time series
  prepseries <- obs_datetime %>%
    data.frame() %>%
    dplyr::mutate(lag_time = dplyr::lag(obs_datetime, 1)) %>%
    dplyr::mutate(gap_hr = difftime(obs_datetime, lag_time, units = "hours")) %>%
    dplyr::filter(gap_hr > 6)
  
  #### This doesn't seem very helpful.
  if(nrow(prepseries) > 0) {
    message(paste0("Warning: Missing values in observed time series."))
    warning_label <- "Warning: Missing values in observed time series."
  } else {
    warning_label <- ""
  }
  
  # Add orifice to plot
  if(orifice_show == TRUE) {
    #### Why are we changing this?
    orifice_plot <- orifice_height_ft
    orifice_lab <- paste0("orifice elevation: ",round(orifice_height_ft, 2))
  }else{
    #### This is why the x-axis has a thick black line which looks out of place.
    #### This should be removed.
    orifice_plot <- 0 #line will be covered by bottom of structure if option is not selected
  }
  
  # Set default names for levels if none are provided
  if(is.null(level_names)) {
    level_names <- c("Prod Water Level (Offset)",
                     "Sandbox Water Level (Corrected)",
                     "Obs. Level 3",
                     "Obs. Level 4")
  }
  
  # Calculate plotting parameters
  
  # Minimum and maximum data values
  min_date <- min(obs_datetime, na.rm = TRUE)
  max_date <- max(obs_datetime, na.rm = TRUE)
  
  # Axis breaks by category
  event_duration <- max_date - min_date
  
  # Set date marker offset by duration
  if(units(event_duration) == "days") {
    marker_scale <- 0.02
    ## The duration of the event + 2 days?
    day_lengths <- event_duration + 2
  } else {
    marker_scale <- 0.015
    day_lengths <- 14
  }
  
  # Dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "America/New_York"),
                                               by = "day", length.out = day_lengths), tz = "America/New_York")
  
  # Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"),
                                           tz = "America/New_York")
  
  # #All plots use one-hour interval for minor x-axis breaks
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12),
                                                      max_date + lubridate::hours(6), by = "hour"),
                                           tz = "America/New_York")
  
  #Generate title block
  title_text <- paste0("Water Level\nSMP ID: ", structure_name,
                       " | Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::label_time("%Y-%m-%d %H:%M", tz = "America/New_York")(min_date),
                       sep = "")
  
  
  # Build dataframes
  # Full dataset
  obs_df <- data.frame(obs_datetime, obs_level_ft)
  # Each water level section
  if(!is.na(level_ft_2[1])) {
    obs2_df <- data.frame(datetime_2, level_ft_2)
  }
  if(!is.na(level_ft_3[1])) {
    obs3_df <- data.frame(datetime_3, level_ft_3)
  }
  if(!is.na(level_ft_4[1])) {
    obs4_df <- data.frame(datetime_4, level_ft_4)
  }
  
  # Generate plot
  level_plot <-
    ggplot2::ggplot(data = obs_df) +
    # Day boundaries
    ggplot2::geom_vline(xintercept = day_marker,
                        color = "black",
                        linetype = "dashed",
                        linewidth = 1.2) +
    ggplot2::annotate("text", x = day_marker-marker_scale*event_duration,
                      y = 0.8*storage_depth_ft,
                      label = day_marker,
                      angle = 90,
                      size = ggplot2::rel(5)) +
    #   #Warning message for data gaps in observed record
    ggplot2::annotate("text", x = day_marker[1]+1,
                      # Put warning halfway between up the storage depth
                      y = 0.5*storage_depth_ft,
                      label = warning_label, #empty if no warning
                      hjust = 0,
                      color = "red",
                      size = ggplot2::rel(5)) +
    #### Places a darker line across the x-axis? Why?
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
    # Top of storage
    ggplot2::geom_hline(yintercept = storage_depth_ft, color = "orange", linewidth = 1.2) +
    
    ggplot2::geom_label(x = min_date + (0.75 * event_duration),
                        y = storage_depth_ft*1.07,
                        #### Should this be called maximum storage depth or storage height?
                        label = paste0("Overflow Depth (", overflow_label, ")"),
                        size = ggplot2::rel(5),
                        fill = alpha("white", 1),
                        linewidth = 0) +
    
    ggplot2::geom_hline(yintercept = installation_height_ft, color = "orange", linewidth = 1.2)+
    ggplot2::geom_label(x = min_date + (0.75 * event_duration),
                        y = installation_height_ft * 1.5,
                        #### Should this be called maximum storage depth or storage height?
                        label = paste0("Sensor Installation Height"),
                        size = ggplot2::rel(5),
                        fill = alpha("white", 1),
                        linewidth = 0)+
    
    #Observed water level
    ggplot2::geom_line(data = obs_df,
                       ggplot2::aes(x = obs_datetime,
                                    y = obs_level_ft,
                                    color = paste(level_names[1])),
                       linewidth = 2
    ) +
    
    # Formatting
    ggplot2::theme_bw() + # a basic black and white theme
    # Scales
    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "America/New_York"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)), # set x axis limits
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, storage_depth_ft+2, by = if(storage_depth_ft > 2) round(storage_depth_ft/4, 0) else ceiling(storage_depth_ft/4)),
      minor_breaks = seq(-0.5,2*storage_depth_ft, by = 0.1),
      limits = c(0, storage_depth_ft + 0.5)) +
    ggplot2::scale_color_manual(values = c("#7822E0","#E0DE43","#E03838","#E12CE0","#16E050")) +
    ggplot2::labs(
      y = "Water Level (ft)",
      title = title_text
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)),
      legend.title=ggplot2::element_blank())
  # Add level 2
  #### We already created the dfs for these, we don't need to check the arg anymore
  if(!is.na(level_ft_2[1])) {
    level_plot <- level_plot +
      ggplot2::geom_line(data = obs2_df,
                         ggplot2::aes(x = datetime_2,
                                      y = level_ft_2,
                                      color = paste(level_names[2])),
                         linewidth = 2
      )
  }
  if(!is.na(level_ft_3[1])) {
    level_plot <- level_plot +
      ggplot2::geom_line(data = obs3_df,
                         ggplot2::aes(x = datetime_3,
                                      y = level_ft_3,
                                      color = paste(level_names[3])),
                         linewidth = 2
      )
  }
  if(!is.na(level_ft_4[1])) {
    level_plot <- level_plot +
      ggplot2::geom_line(data = obs4_df,
                         ggplot2::aes(x = datetime_4,
                                      y = level_ft_4,
                                      color = paste(level_names[4])),
                         linewidth = 2
      )
  }
  
  if(orifice_show == TRUE ) {
    level_plot <- level_plot +
      ggplot2::geom_hline(yintercept = orifice_plot, color = "grey", linetype = 2, linewidth = 1.2) +
      ggplot2::geom_label(label = orifice_lab,
                          y = orifice_height_ft*1.1,
                          x = obs_datetime[round(0.75*length(obs_datetime))])
    
  }
  
  # Add metrics
  #### This is broken, needs to be determined if we want to fix it.
  # if(metrics_show == TRUE){
  #
  #   #set missing values to ""
  #   if( missing(obs_draindown_hr) ){obs_draindown_hr <- ""}
  #   if( missing(sim_draindown_hr) ){sim_draindown_hr <- ""}
  #   if( missing(obs_infil_inhr) ){obs_infil_inhr <- ""}
  #   if( missing(sim_infil_inhr) ){sim_infil_inhr <- ""}
  #   if( missing(obs_RSPU) ){obs_RSPU <- ""}
  #   if( missing(sim_RSPU) ){sim_RSPU <- ""}
  #   if( missing(obs_overtopping) ){obs_overtopping <- ""}
  #   if( missing(sim_overtopping) ){sim_overtopping <- ""}
  #
  #   level_plot %<>% marsMetricsTable( obs_RSPU = obs_RSPU,
  #                                     obs_infil_inhr = obs_infil_inhr,
  #                                     obs_draindown_hr = obs_draindown_hr,
  #                                     obs_overtopping = obs_overtopping,
  #                                     sim_RSPU = sim_RSPU,
  #                                     sim_infil_inhr = sim_infil_inhr,
  #                                     sim_draindown_hr = sim_draindown_hr,
  #                                     sim_overtopping = sim_overtopping)
  # }
  
  level_plot
}

get_legend<-function(myggplot) {
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

marsCombinedPlot_Offset <- function(event, 
                             structure_name, # This is a really confusing name/description
                             obs_datetime, 
                             obs_level_ft,
                             obs_datetime_2,
                             obs_level_ft_2,
                             storage_depth_ft,
                             orifice_show = FALSE,
                             orifice_height_ft = NULL,
                             rainfall_datetime,
                             rainfall_in,
                             metrics_show = FALSE,
                             obs_RSPU,
                             obs_infil_inhr,
                             obs_draindown_hr,
                             obs_overtopping,
                             overflow_label,
                             installation_height_ft) {

  #Add a last date so the hyetograph looks better
  rainfall_in <- append(rainfall_in, 0)
  #### Why are we adding the last level datetime to rainfall?
  rainfall_datetime <- append(rainfall_datetime, max(obs_datetime))
  
  # Create individual plots
  level_plot <- marsWaterLevelPlot(event = event,
                                   structure_name = structure_name,
                                   obs_datetime = obs_datetime,
                                   obs_level_ft = obs_level_ft,
                                   datetime_2 = obs_datetime_2,
                                   level_ft_2 = obs_level_ft_2,
                                   storage_depth_ft = storage_depth_ft,
                                   orifice_show = orifice_show,
                                   orifice_height_ft = orifice_height_ft,
                                   overflow_label = overflow_label,
                                   installation_height_ft = installation_height_ft)
  
  # Default to reversing y-axis for rainfall
  rainfall_plot <- marsRainfallPlot(event = event,
                                    dtime = rainfall_datetime,
                                    rainfall_in = rainfall_in,
                                    reverse_y = TRUE)
  
  # Combine Plots
  # Save legends
  level_legend <- get_legend(level_plot)
  rainfall_legend <- get_legend(rainfall_plot)
  
  # Calculate minimum and maximum data values
  min_date <- min(obs_datetime, na.rm = TRUE)
  max_date <- max(obs_datetime, na.rm = TRUE)
  
  # Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date
  #set date marker offset by duration
  if(units(event_duration) == "days") {
    marker_scale <- 0.02
  } else {
    marker_scale <- 0.015
  }
  
  # Dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "America/New_York"), by = "day", length.out = 14), tz = "America/New_York")
  
  # X-axis breaks
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "America/New_York")
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12),
                                                      max_date + lubridate::hours(6),
                                                      by = "hour"), tz = "America/New_York")
  # Title
  title_text <- paste0("Water Level\nSMP ID: ", structure_name,
                       " | Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::date_format("%Y-%m-%d %H:%M", tz = "America/New_York")(min_date),
                       sep = "")
  
  # Remove legends and titles and update axes
  level_plot <- level_plot +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = ggplot2::rel(1)),
                   axis.text = ggplot2::element_text(size = ggplot2::rel(.95)))
  rainfall_plot <- rainfall_plot +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1.35)),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.25)),
                   axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.25))) +
    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "America/New_York"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)),
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks)  +
    ggplot2::labs(title = title_text)
  #### Broken: See Issue #41
  # if(metrics_show == TRUE){
  #   
  #   #set missing values to ""
  #   if( missing(obs_draindown_hr) ){obs_draindown_hr <- ""}
  #   if( missing(sim_draindown_hr) ){sim_draindown_hr <- ""}
  #   if( missing(obs_infil_inhr) ){obs_infil_inhr <- ""}
  #   if( missing(sim_infil_inhr) ){sim_infil_inhr <- ""}
  #   if( missing(obs_RSPU) ){obs_RSPU <- ""}
  #   if( missing(sim_RSPU) ){sim_RSPU <- ""}
  #   if( missing(obs_overtopping) ){obs_overtopping <- ""}
  #   if( missing(sim_overtopping) ){sim_overtopping <- ""}
  #   
  #   level_plot %<>% marsMetricsTable(obs_RSPU = obs_RSPU,
  #                                    obs_infil_inhr = obs_infil_inhr,
  #                                    obs_draindown_hr = obs_draindown_hr,
  #                                    obs_overtopping = obs_overtopping,
  #                                    sim_RSPU = sim_RSPU,
  #                                    sim_infil_inhr = sim_infil_inhr,
  #                                    sim_draindown_hr = sim_draindown_hr,
  #                                    sim_overtopping = sim_overtopping) 
  # }
  # 
  # 
  # 
  # Calculate max width
  level_grob <- ggplot2::ggplotGrob(level_plot)
  rainfall_grob <- ggplot2::ggplotGrob(rainfall_plot)
  # 
  # Set max width
  maxWidth = grid::unit.pmax(level_grob$widths[2:9], rainfall_grob$widths[2:9])
  level_grob$widths[2:9] <- maxWidth
  rainfall_grob$widths[2:9] <- maxWidth
  
  # Arrange the plots and export
  combined_plot <- gridExtra::grid.arrange(rainfall_grob, level_grob,
                                           rainfall_legend, level_legend, 
                                           ncol = 1,
                                           heights = c(1.1, 2, 0.15, 0.15),
                                           newpage = TRUE)
  
  combined_plot
}
