#### Green Inlet Monitoring
#### OT plotting
#### Written by : Brian Cruice
#### Written on: 01/10/2022
#### Updated: 04/03/2024

### 0.1 packages
library(tidyverse)
library(odbc)
library(DBI)
library(pwdgsi)
library(lubridate)
library(ggplot2)
library(cowplot)
library(xlsx)
library(wesanderson)

#### 1.0 Set up ####
# Read data, set up folders
folderpath <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/"

systems <- c('171-1', '171-2', '1-3', '1-1', '1006-1', '179-5','488-5', '439-1')


# Get list of folders
folders <- list.files(folderpath)
# Remove files
folders <- folders[grep(x = folders, pattern = "\\.", invert = TRUE) %>% as.vector()]

# Find latest date
latest_date <- NA
for(i in 1:length(folders)){
  if(try(as.Date(folders[i]), silent = TRUE) %>% is.Date()){
    # set new date
    new_date <- as.Date(folders[i])
    
    # usurp if latest date
    latest_date <- max(c(new_date, latest_date), na.rm = TRUE)
  }
}

latest_date <- "2024-04-25"

file_path <- paste0(latest_date,"/overtopping_data.csv")
raw_data <- read.csv(paste0(folderpath,"/",file_path))

# read key dates
date_file <- "graph_dates.csv"
raw_dates <- read.csv(paste0(folderpath,date_file))
  
# read inlet types
inlet_type <- xlsx::read.xlsx(file = paste0(folderpath,"Assets.xlsx"),
                              sheetName = "Inlet Depths")

# read System characteristics
sys_char_file <- paste0(folderpath,"SystemCharacteristics.xlsx")
sys_char <- xlsx::read.xlsx(file = sys_char_file,
                            sheetName = "Characteristics")



#### 1.2 filter dates not to be used ####
# 1-1 Taken offline
last_date_1_1 <- raw_dates  %>% dplyr::filter(system_id == '1-1') %>%
  dplyr::filter(grepl("Short-Circuiting Observed",graph_text) |
                  grepl("System Offline", graph_text)) %>%
  dplyr::select(date_complete) %>% unlist() %>% mdy() %>% min()

date_formats <- c(guess_formats(raw_data$eventdatastart_edt, "mdy HMS"),
                  guess_formats(raw_data$eventdatastart_edt, "mdy"))
dates_x <- raw_data$eventdatastart_edt %>% lubridate::as_datetime(format = date_formats)
raw_data$eventdatastart_edt <- dates_x

filtered_data <- raw_data %>% dplyr::filter(smp_id != '1-1-1' |
                                              (smp_id == '1-1-1'  &
                                                 eventdatastart_edt < last_date_1_1)
)

# 179-5-1 distribution pipe capping
cap_date <- as.Date("2021-11-08")
uncapped_date <- as.Date("2021-12-30") 

filtered_data <- filtered_data %>% dplyr::filter(smp_id != '179-5-1' |
                                                   (smp_id == '179-5-1' &
                                                      eventdatastart_edt < cap_date |
                                                      eventdatastart_edt > uncapped_date
                                                   ))

# 439-1-1 short-circuiting
last_date_439 <- as.Date("2022-01-14")

filtered_data <- filtered_data %>% dplyr::filter(smp_id != '439-1-1' |
                                                   (smp_id == '439-1-1' &
                                                      eventdatastart_edt < last_date_439
                                                   ))



# Grab measurements for all ows
mars_con <- odbc::dbConnect(odbc::odbc(), "mars14_datav2")

ow_query <- paste0("WITH sys as (SELECT *, admin.fun_smp_to_system(smp_id) as system_id  FROM fieldwork.tbl_ow)
             SELECT * FROM sys WHERE system_id IN ('",paste(systems, collapse = "', '"),"')")
ow <- dbGetQuery(mars_con, ow_query)

well_meas <- dbGetQuery(mars_con, paste0("SELECT * FROM fieldwork.viw_ow_plus_measurements WHERE ow_uid in (",
                                         paste(ow$ow_uid, collapse= ", "),")")) %>%
  dplyr::filter(ow_suffix == "OW1" | ow_suffix == "GI1")

dbDisconnect(mars_con)

# Create plot path if needed
# current_date <- today()
plotpath <- paste0(folderpath,"/Overtopping Graphs/",Sys.Date())
if(!dir.exists(plotpath)){dir.create(plotpath)}

# Connect to database
mars_con <- odbc::dbConnect(odbc::odbc(), "mars14_datav2")

# list of unique monitoring locations
mon_locs <- filtered_data %>% dplyr::select(smp_id, ow_suffix) %>% distinct()


#### 1.5 SMP ID to SystemID  function ####

smp_2_sys <- function(smp_id){
  x <- str_split(smp_id, pattern = "-") %>% unlist()
  sys_id <- paste0(x[1],"-",x[2])
  return(sys_id)
}

#### 2.0 Plotting functions ####
overtopping_intensity_plot <- function(data, design_storm, event_dates = NULL, event_descriptions = NULL){

  #clean dates for x-axis
  # date_formats <- c(guess_formats(data$eventdatastart_edt, "mdy HMS"),
  #                   guess_formats(data$eventdatastart_edt, "mdy"))
  # dates_x <- data$eventdatastart_edt %>% lubridate::as_datetime(format = date_formats)
  # 
  # data$eventdatastart_edt <- dates_x
  # event_dates <- event_dates %>% lubridate::mdy() %>% lubridate::as_datetime()
  min_date <- min(data$eventdatastart_edt)
  max_date <- max(data$eventdatastart_edt)
  
  
  event_dates <- event_dates %>% lubridate::as_datetime(format = date_formats)
  
  #Set overtop to sizes
  data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
  data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
  data$overtop_col[data$overtop == FALSE] <- wes_palettes$Royal1[1]
  data$overtop_col[data$overtop == TRUE] <- wes_palettes$Royal1[2]
  
  #subset of data exceeding design storm
  data_ovr_design <- data %>% dplyr::filter(eventdepth_in > design_storm)
  data <- data %>% mutate("ExceedDesignStorm" = ifelse(eventdepth_in > design_storm,"True",NA))
  # y-max value
  ymax_obs <- max(data$eventpeakintensity_inhr, na.rm = TRUE)
  
  
  plot_x <- ggplot(data,
                   aes(x = eventdatastart_edt,
                       y = eventpeakintensity_inhr)) +
    geom_point(aes(color = factor(overtop_col),
                   size = factor(overtop_sz))) +
    geom_hline(yintercept = 2.5, color = wes_palettes$Royal1[4], size = 1.5) +
    scale_y_continuous(limits = c(0,max(3.6,ymax_obs)), minor_breaks =seq(0,max(3,ymax_obs),0.2)) +
    scale_x_datetime(date_minor_breaks = "2 months") +
    ylab("Event Peak Intensity (in/hr)") + xlab("Event Date/Time") +
    geom_text(label = "Philadelphia 1-year, 15-minute Peak Intensity: 2.5 in/hr",
              y = 2.6, color = "black", size = 12 / .pt, hjust = "left",
              x = data$eventdatastart_edt[round(0.05*length(data$eventdatastart_edt))]) +
    ggtitle(paste0("Event Peak Intensity and Overtopping vs Time for ",data$smp_id[1])) +
    
    #add design storm values
    geom_point(aes(x = eventdatastart_edt,
                   y = eventpeakintensity_inhr, size = factor(overtop_sz),  color = factor(overtop_col), shape = factor(ExceedDesignStorm))) +
    scale_shape_manual(name = paste0("Exceeds Design Storm Depth: ",round(design_storm,2)," in"), values = c(2), labels = c("True"), na.translate = FALSE) +
    scale_color_manual(name = "Overtopping", values = wes_palettes$Royal1, labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    scale_size_manual(name = "Overtopping", values = c(2,4), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    #from pwdgsi plots
    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = wes_palettes$Royal1[3], colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))
  
  if(length(event_dates) > 0 & length(event_descriptions) > 0){
    for(i in 1:length(event_dates)){
      plot_x <- plot_x + geom_vline(xintercept = event_dates[i], color = wes_palettes$Royal1[4], size = 1.1, linetype = "dashed") +
                         geom_text(label = event_descriptions[i], angle = 90,
                                   y = 2.7, color = "black", size = 12 / .pt, hjust = "left",
                                   x = as.numeric(event_dates[i] + days(8)))
    }
    
  }
  
  
  
  return(plot_x)
  
}


overtopping_hdif_plot <- function(data, design_storm, event_dates = NULL, event_descriptions = NULL){
  
  #clean dates for x-axis
  # date_formats <- c(guess_formats(data$eventdatastart_edt, "mdy HMS"),
  #                   guess_formats(data$eventdatastart_edt, "mdy"))
  # dates_x <- data$eventdatastart_edt %>% lubridate::as_datetime(format = date_formats)
  # 
  # data$eventdatastart_edt <- dates_x
  # event_dates <- event_dates %>% lubridate::mdy() %>% lubridate::as_datetime()
  min_date <- min(data$eventdatastart_edt)
  max_date <- max(data$eventdatastart_edt)
  
  event_dates <- event_dates %>% lubridate::as_datetime(format = date_formats)
  
  #Set overtop to sizes
  data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
  data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
  data$overtop_col[data$overtop == FALSE] <- wes_palettes$Royal1[1]
  data$overtop_col[data$overtop == TRUE] <- wes_palettes$Royal1[2]
  
  #subset of data exceeding design storm
  data_ovr_design <- data %>% dplyr::filter(eventdepth_in > design_storm)
  data <- data %>% mutate("ExceedDesignStorm" = ifelse(eventdepth_in > design_storm,"True",NA))
  # y-max value
  ymax_obs <- max(data$rel_head_dif, na.rm = TRUE)
  
  
  plot_x <- ggplot(data,
                   aes(x = eventdatastart_edt,
                       y = rel_head_dif)) +
    geom_point(aes(color = factor(overtop),
                   size = factor(overtop))) +
     scale_y_continuous(limits = c(0,max(7,ymax_obs)), minor_breaks =seq(0,max(3,ymax_obs),0.2)) +
    scale_x_datetime(date_minor_breaks = "2 months") +
    ylab("Head Differential at Peak Water Level in Green Inlet (ft)") + xlab("Event Date/Time") +
    ggtitle(paste0("Head Differential and Overtopping vs Time for ",data$smp_id[1])) +
    
    #add design storm values
    geom_point(aes(x = eventdatastart_edt,
                   y = rel_head_dif, size = factor(overtop),  color = factor(overtop), shape = factor(ExceedDesignStorm))) +
    scale_shape_manual(name = paste0("Exceeds Design Storm Depth: ",round(design_storm,2)," in"), values = c(2), labels = c("True"), na.translate = FALSE) +
    scale_color_manual(name = "Overtopping", values = wes_palettes$Royal1, labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    scale_size_manual(name = "Overtopping", values = c(2,4), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    #from pwdgsi plots
    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = wes_palettes$Royal1[3], colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))
  
  if(length(event_dates) > 0 & length(event_descriptions) > 0){
    for(i in 1:length(event_dates)){
      plot_x <- plot_x + geom_vline(xintercept = event_dates[i], color = wes_palettes$Royal1[4], size = 1.1, linetype = "dashed") +
        geom_text(label = event_descriptions[i], angle = 90,
                  y = 2.7, color = "black", size = 12 / .pt, hjust = "left",
                  x = as.numeric(event_dates[i] + days(8)))
    }
    
  }
  
  
  
  return(plot_x)
  
}
  
pipejet_overtop_plot <- function(data, design_storm){
  
  #clean dates for x-axis
  data <- data %>% dplyr::filter(!is.na(last_jet))
  
  # data$last_jet <- data$eventdatastart_edt %>% lubridate::mdy_hm()
  # event_dates <- event_dates %>% lubridate::mdy() %>% lubridate::as_datetime()
  # min_date <- min(lubridate::date(data$last_jet))
  # max_date <- max(lubridate::date(data$last_jet))
  
  
  #Set overtop to sizes
  data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
  data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
  data$overtop_col[data$overtop == FALSE] <- wes_palettes$Royal1[1]
  data$overtop_col[data$overtop == TRUE] <- wes_palettes$Royal1[2]
  
  #subset of data exceeding design stor
  data_ovr_design <- data %>% dplyr::filter(eventdepth_in > design_storm)
  data <- data %>% mutate("ExceedDesignStorm" = ifelse(eventdepth_in > design_storm,"True",NA))
  # y-max value
  ymax_obs <- max(data$eventpeakintensity_inhr, na.rm = TRUE)
  
  
  plot_x <- ggplot(data,
                   aes(x = last_jet,
                       y = eventpeakintensity_inhr)) +
    geom_point(aes(color = factor(overtop),
                   size = factor(overtop))) +
    geom_hline(yintercept = 2.5, color = wes_palettes$Royal1[4], size = 1.5) +
    scale_y_continuous(limits = c(0,max(3.6,ymax_obs)), minor_breaks =seq(0,max(3,ymax_obs),0.2)) +
    ylab("Event Peak Intensity (in/hr)") + xlab("Days Since Last Pipe Jetting") +
    geom_text(label = "Philadelphia 1-year, 15-minute Peak Intensity: 2.5 in/hr",
              y = 2.6, color = "black", size = 12 / .pt, hjust = "left",
              # x = data$last_jet[round(0.05*length(data$last_jet))]) +
              x = max(data$last_jet)*0.05) +
    ggtitle(paste0("Event Peak Intensity and Overtopping vs Days Since Pipe Jetting for ",data$smp_id[1])) +
    
    #add design storm values
    geom_point(aes(x = last_jet,
                   y = eventpeakintensity_inhr, size = factor(overtop),  color = factor(overtop), shape = factor(ExceedDesignStorm))) +
    scale_shape_manual(name = paste0("Exceeds Design Storm Depth: ",round(design_storm,2)," in"), values = c(2), labels = c("True"), na.translate = FALSE) +
    scale_color_manual(name = "Overtopping", values = wes_palettes$Royal1, labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    scale_size_manual(name = "Overtopping", values = c(2,4), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    #from pwdgsi plots
    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = wes_palettes$Royal1[3], colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))
  

  
  return(plot_x)
  
}


pipejet_hdif_plot <- function(data, design_storm){
  # browser()
  #clean dates for x-axis
  data <- data %>% dplyr::filter(!is.na(last_jet))
  
  # data$last_jet <- data$eventdatastart_edt %>% lubridate::mdy_hm()
  # event_dates <- event_dates %>% lubridate::mdy() %>% lubridate::as_datetime()
  # min_date <- min(lubridate::date(data$last_jet))
  # max_date <- max(lubridate::date(data$last_jet))
  
  
  #Set overtop to sizes
  data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
  data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
  data$overtop_col[data$overtop == FALSE] <- wes_palettes$Royal1[1]
  data$overtop_col[data$overtop == TRUE] <- wes_palettes$Royal1[2]
  
  #subset of data exceeding design storm
  data_ovr_design <- data %>% dplyr::filter(eventdepth_in > design_storm)
  data <- data %>% mutate("ExceedDesignStorm" = ifelse(eventdepth_in > design_storm,"True",NA))
  # y-max value
  ymax_obs <- max(data$rel_head_dif, na.rm = TRUE)
  
  
  plot_x <- ggplot(data,
                   aes(x = last_jet,
                       y = rel_head_dif)) +
    geom_point(aes(color = factor(overtop),
                   size = factor(overtop))) +
    scale_y_continuous(limits = c(0,max(7,ymax_obs)), minor_breaks =seq(0,max(3,ymax_obs),0.2)) +
    ylab("Head Differential at Peak Water Level in Green Inlet (ft)") + xlab("Days Since Last Pipe Jetting") +
    ggtitle(paste0("Head Differential and Overtopping vs Days Since Pipe Jetting for ",data$smp_id[1])) +
    
    #add design storm values
    geom_point(aes(x = last_jet,
                   y = rel_head_dif, size = factor(overtop),  color = factor(overtop), shape = factor(ExceedDesignStorm))) +
    scale_shape_manual(name = paste0("Exceeds Design Storm Depth: ",round(design_storm,2)," in"), values = c(2), labels = c("True"), na.translate = FALSE) +
    scale_color_manual(name = "Overtopping", values = wes_palettes$Royal1, labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    scale_size_manual(name = "Overtopping", values = c(2,4), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    #from pwdgsi plots
    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = wes_palettes$Royal1[3], colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))
  
  
  
  return(plot_x)
  
}


overtopping_qmax_plot <- function(data, design_storm, system_chars, event_dates = NULL, event_descriptions = NULL){
  
  #clean dates for x-axis
  # date_formats <- c(guess_formats(data$eventdatastart_edt, "mdy HMS"),
  #                   guess_formats(data$eventdatastart_edt, "mdy"))
  # dates_x <- data$eventdatastart_edt %>% lubridate::as_datetime(format = date_formats)
  # 
  # data$eventdatastart_edt <- dates_x
  # event_dates <- event_dates %>% lubridate::mdy() %>% lubridate::as_datetime()
  min_date <- min(data$eventdatastart_edt)
  max_date <- max(data$eventdatastart_edt)
  
  
  event_dates <- event_dates %>% lubridate::as_datetime(format = date_formats)
  
  #Set overtop to sizes
  data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
  data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
  data$overtop_col[data$overtop == FALSE] <- wes_palettes$Royal1[1]
  data$overtop_col[data$overtop == TRUE] <- wes_palettes$Royal1[2]
  
  #subset of data exceeding design stor
  data_ovr_design <- data %>% dplyr::filter(eventdepth_in > design_storm)
  data <- data %>% mutate("ExceedDesignStorm" = ifelse(eventdepth_in > design_storm,"True",NA))
  
  
  # qmax value
  qmax <- system_chars$Max.Flow.w..Perforations..CFS. %>% as.numeric() %>% round(2)
  
  # y-max value
  ymax_obs <- max(c(data$qpeak,qmax), na.rm = TRUE)*1.1 # buffer for higher qpeak values
  
  plot_x <- ggplot(data,
                   aes(x = eventdatastart_edt,
                       y = qpeak)) +
    geom_point(aes(color = overtop,
                   size = overtop)) + 
    
    scale_color_manual(values = c(wes_palettes$Royal1[2], wes_palettes$Royal1[1])) +
    scale_size_manual(values = c(as.numeric(4), as.numeric(2)))
    
    
    if(length(event_dates) > 0 & length(event_descriptions) > 0){
      for(i in 1:length(event_dates)){
        plot_x <- plot_x + geom_vline(xintercept = event_dates[i], color = wes_palettes$Royal1[4], size = 1.1, linetype = "dashed") +
          geom_text(label = event_descriptions[i], angle = 90,
                    y = 2.7, color = "black", size = 12 / .pt, hjust = "left",
                    x = as.numeric(event_dates[i] + days(14)))
      }
      
    }
    
  plot_x <- plot_x +  geom_hline(yintercept = qmax, color = wes_palettes$Royal1[4], size = 1.5) +
                      scale_y_continuous(limits = c(0,ymax_obs), minor_breaks =seq(0,max(3,ymax_obs),0.2)) +
                      scale_x_datetime(date_minor_breaks = "2 months") +
                      ylab("Event Qpeak (cfs)") + xlab("Event Date/Time") +
                      geom_text(label = paste0("System Distribution Pipe Qmax: ",qmax," cfs"),
                                y = (qmax*1.05), color = "black", size = 12 / .pt, hjust = "left",
                                x = data$eventdatastart_edt[round(0.05*length(data$eventdatastart_edt))]) +
                      ggtitle(paste0("Event Peak Flow Rate and Overtopping vs Time for ",data$smp_id[1])) +
                      
                      #add design storm values
                      geom_point(aes(x = eventdatastart_edt,
                                     y = qpeak, size = factor(overtop),  color = factor(overtop), shape = factor(ExceedDesignStorm))) +
                      scale_shape_manual(name = paste0("Exceeds Design Storm Depth: ",round(design_storm,2)," in"), values = c(2), labels = c("True"), na.translate = FALSE) +
                      scale_color_manual(name = "Overtopping", values = wes_palettes$Royal1, labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
                      scale_size_manual(name = "Overtopping", values = c(2,4), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
                      #from pwdgsi plots
                      ggplot2::theme(
                        #text = element_text(size = rel(2)), #size previously set to 16
                        axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                        axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                        panel.background =  ggplot2::element_rect(fill = wes_palettes$Royal1[3], colour = NA), # set white background
                        panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                        panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                        panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                        legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                        legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))
  
  
  return(plot_x)
  
}

overtopping_qmax_dsgnonly_plot <- function(data, design_storm, system_chars, event_dates = NULL, event_descriptions = NULL){
  
  #clean dates for x-axis
  # date_formats <- c(guess_formats(data$eventdatastart_edt, "mdy HMS"),
  #                   guess_formats(data$eventdatastart_edt, "mdy"))
  # dates_x <- data$eventdatastart_edt %>% lubridate::as_datetime(format = date_formats)
  # 
  # data$eventdatastart_edt <- dates_x
  # event_dates <- event_dates %>% lubridate::mdy() %>% lubridate::as_datetime()
  min_date <- min(data$eventdatastart_edt)
  max_date <- max(data$eventdatastart_edt)
  
  # 
  # event_dates <- event_dates %>% lubridate::as_datetime(format = date_formats)
  
  #Set overtop to sizes
  data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
  data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
  data$overtop_col[data$overtop == FALSE] <- wes_palettes$Royal1[1]
  data$overtop_col[data$overtop == TRUE] <- wes_palettes$Royal1[2]
  
  #subset of data under design storm
  data <- data %>% dplyr::filter(eventdepth_in <= design_storm)
  
  
  # qmax value
  qmax <- system_chars$Max.Flow.w..Perforations..CFS. %>% as.numeric() %>% round(2)
  
  # y-max value
  ymax_obs <- max(c(data$qpeak,qmax), na.rm = TRUE)*1.1 # buffer for higher qpeak values
  
  #start plot
  plot_x <- ggplot(data,
                   aes(x = eventdatastart_edt,
                       y = qpeak)) +
    geom_point(aes(color = overtop,
                   size = overtop)) +
    scale_color_manual(values = c(wes_palettes$Royal1[1], wes_palettes$Royal1[2])) +
    scale_size_manual(values = c(as.numeric(2), as.numeric(4)))
  
  
  if(length(event_dates) > 0 & length(event_descriptions) > 0){
    for(i in 1:length(event_dates)){
      plot_x <- plot_x + geom_vline(xintercept = as_datetime(mdy(event_dates[i])), color = wes_palettes$Royal1[4], size = 1.1, linetype = "dashed") +
        geom_text(label = event_descriptions[i], angle = 90,
                  y = 2.7, color = "black", size = 12 / .pt, hjust = "left",
                  x = as.numeric(as_datetime(mdy(event_dates[i]))  + days(14)))
    }
    
  }
  
  plot_x <- plot_x +  geom_hline(yintercept = qmax, color = wes_palettes$Royal1[4], size = 1.5) +
    scale_y_continuous(limits = c(0,ymax_obs), minor_breaks =seq(0,max(3,ymax_obs),0.2)) +
    scale_x_datetime(date_minor_breaks = "2 months") +
    ylab("Event Qpeak (cfs)") + xlab("Event Date/Time") +
    geom_text(label = paste0("System Distribution Pipe Qmax: ",qmax," cfs"),
              y = (qmax*1.05), color = "black", size = 12 / .pt, hjust = "left",
              x = data$eventdatastart_edt[round(0.05*length(data$eventdatastart_edt))]) +
    ggtitle(paste0("Event Peak Flow Rate and Overtopping vs Time for ",data$smp_id[1])) +
    #from pwdgsi plots
    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = wes_palettes$Royal1[3], colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))
  
  
  return(plot_x)
  
}


#### 3.0 iterate through locations, savings plots ####

#### 3.1 Plots with storms over Design depth ####
  
for(i in 1:nrow(mon_locs)){

# Subset plot data
plot_data <- filtered_data %>%
             dplyr::filter(smp_id == mon_locs$smp_id[i]) %>%
             dplyr::filter(ow_suffix == mon_locs$ow_suffix[i]) %>%
             dplyr::select(ow_uid, radar_event_uid, ow_suffix, eventdatastart_edt,
                           smp_id, eventavgintensity_inhr, eventpeakintensity_inhr, eventdepth_in, overtop, rel_head_dif)

# Get System ID
sys_id <- smp_2_sys(mon_locs$smp_id[i])

# Grab design storm Depth  
design_storm <- dbGetQuery(mars_con,
                           paste0("SELECT sys_creditedstormsizemanaged_in FROM external.tbl_systembdv
                                  WHERE system_id = '",sys_id,"'")) %>% pull

# Select maintenance events
event_data <- raw_dates %>% dplyr::filter(system_id == sys_id &
                                          graph == TRUE) %>%
                            dplyr::select(system_id, date_complete, graph_text)


# create plot
location_plot <- overtopping_intensity_plot(data =plot_data, 
                                            design_storm = design_storm,
                                            event_dates = event_data$date_complete,
                                            event_descriptions = event_data$graph_text)    


# hdif plot
hdif_plot <- overtopping_hdif_plot(data =plot_data, 
                                   design_storm = design_storm,
                                   event_dates = event_data$date_complete,
                                   event_descriptions = event_data$graph_text)

# Save Plot
ggsave(plot = location_plot,
       filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],".png"),
       width = 10, height = 8)

ggsave(plot = hdif_plot,
       filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_hdif.png"),
       width = 10, height = 8)

}

#### 3.2 Plots excluding storms over Design depth ####

# This is clunky, chunky code to later be corrected. One could easily make both plots in a single for-loop

for(i in 1:nrow(mon_locs)){
  
  # Subset plot data
  plot_data <- filtered_data %>%
    dplyr::filter(smp_id == mon_locs$smp_id[i]) %>%
    dplyr::filter(ow_suffix == mon_locs$ow_suffix[i]) %>%
    dplyr::select(ow_uid, radar_event_uid, ow_suffix, eventdatastart_edt,
                  smp_id, eventavgintensity_inhr, eventpeakintensity_inhr, eventdepth_in, overtop, rel_head_dif)
  
  # Get System ID
  sys_id <- smp_2_sys(mon_locs$smp_id[i])
  
  # Grab design storm Depth  
  design_storm <- dbGetQuery(mars_con,
                             paste0("SELECT sys_creditedstormsizemanaged_in FROM external.tbl_systembdv WHERE system_id = '",sys_id,"'")) %>% pull
  
  #filter to only include design storms
  plot_data <- plot_data %>% dplyr::filter(eventdepth_in <= design_storm)
  
  # Select maintenance events
  event_data <- raw_dates %>% dplyr::filter(system_id == sys_id &
                                              graph == TRUE) %>%
    dplyr::select(system_id, date_complete, graph_text)
  
  # Select system characteristics
  loc_sys_char <- sys_char %>% dplyr::filter(smp_id == mon_locs$smp_id[i],
                                             GI == mon_locs$ow_suffix[i])
  
  # calculate qpeak; rational method using peak intensity as i. Q= CiA. Use full system drainage area for distr. pipe
  plot_data$qpeak <- plot_data$eventpeakintensity_inhr*0.95*loc_sys_char$System.Drainage.Area..SF. * (1/12) * (1/3600) # last terms to cfs

  
  
  # create plot
  location_plot <- overtopping_intensity_plot(data =plot_data, 
                                              design_storm = design_storm,
                                              event_dates = event_data$date_complete,
                                              event_descriptions = event_data$graph_text)    
  
  
  # hdif plot
  hdif_plot <- overtopping_hdif_plot(data =plot_data, 
                                     design_storm = design_storm,
                                     event_dates = event_data$date_complete,
                                     event_descriptions = event_data$graph_text)
  
  #qmax plot
  qmax_plot <- overtopping_qmax_dsgnonly_plot(data = plot_data,
                                              system_chars = loc_sys_char,
                                              design_storm = design_storm,
                                              event_dates = event_data$date_complete,
                                              event_descriptions = event_data$graph_text)
  
  # Save Plot
  ggsave(plot = location_plot,
         filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_design_storm_only.png"),
         width = 2100, height = 1620, units = "px")
  ggsave(plot = hdif_plot,
         filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_design_storm_only_hdif.png"),
         width = 2100, height = 1620, units = "px")  
  ggsave(plot = qmax_plot,
         filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_design_storm_only_qmax.png"),
         width = 2100, height = 1620, units = "px") 
  

  }


#### 3.3 Days since pipe jetting plots ####

last_jet <- data.frame(matrix(ncol = 3))
colnames(last_jet) <- c("ow_uid","radar_event_uid","last_jet")

for(i in 1:nrow(mon_locs)){
  
  # Subset plot data
  plot_data <- filtered_data %>%
    dplyr::filter(smp_id == mon_locs$smp_id[i]) %>%
    dplyr::filter(ow_suffix == mon_locs$ow_suffix[i]) %>%
    dplyr::select(ow_uid, radar_event_uid, ow_suffix, eventdatastart_edt, rel_head_dif,
                  smp_id, eventavgintensity_inhr, eventpeakintensity_inhr, eventdepth_in, overtop)
  
  # Get System ID
  sys_id <- smp_2_sys(mon_locs$smp_id[i])
  
  # Grab design storm Depth  
  design_storm <- dbGetQuery(mars_con,
                             paste0("SELECT sys_creditedstormsizemanaged_in FROM external.tbl_systembdv
                                  WHERE system_id = '",sys_id,"'")) %>% pull
  
  # Select maintenance events
  event_data <- raw_dates %>% dplyr::filter(system_id == sys_id & graph == TRUE) %>%
    dplyr::select(system_id, date_complete, graph_text)
  
  # Only pipe jetting events
  pipe_jetting_days <- raw_dates %>% dplyr::filter(system_id == sys_id & graph == TRUE) %>%
    dplyr::select(system_id, date_complete, graph_text) %>%
    dplyr::filter(graph_text == "Pipe Jetting")
  
  
  for(j in 1:nrow(plot_data)){
    
    #get the current date in question
    date_x <- plot_data$eventdatastart_edt[j] %>% ymd_hms()
    
    # list of dates where pipe jetting occurred, coerced into datetime format
    jet_days <- pipe_jetting_days$date_complete %>% mdy() %>% as_datetime()
    
    # Find the number of days between the current date and each pipe jetting event
    time_difs <- difftime(date_x, jet_days, units = "days")
    
    # Remove pipe jetting events happening in the future; limit it to a list of past pipe jetting events
    time_difs <- time_difs[time_difs > 0]
    
    # Find the shortest distance in days since a pipe jetting event. Set to NA if none are found.
    plot_data$last_jet[j] <- ifelse(length(time_difs) > 0,
                                    min(time_difs),
                                    NA)
    
  }
  
  # plot_data without design storms
  plot_data_dsgn <- plot_data %>% dplyr::filter(eventdepth_in <= design_storm)

  
  
  plot_z <- pipejet_overtop_plot(data =plot_data, 
                                 design_storm = design_storm)
  
  plot_h <- pipejet_hdif_plot(data = plot_data,
                              design_storm = design_storm)
  
  plot_zds <- pipejet_overtop_plot(data =plot_data_dsgn, 
                                 design_storm = design_storm)
  
  plot_hds <- pipejet_hdif_plot(data = plot_data_dsgn,
                              design_storm = design_storm)
  
  # Save Plot
  ggsave(plot = plot_z,
         filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_days_since_jetting.png"),
         width = 2100, height = 1620, units = "px")
  
  ggsave(plot = plot_h,
         filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_hdif_days_since_jetting.png"),
         width = 2100, height = 1620, units = "px")
  
  ggsave(plot = plot_zds,
         filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_dsgnonly_days_since_jetting.png"),
         width = 2100, height = 1620, units = "px")
  
  ggsave(plot = plot_hds,
         filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_dsgnonly_hdif_days_since_jetting.png"),
         width = 2100, height = 1620, units = "px")
  
  
  
  last_jet_x <- plot_data %>% dplyr::select(ow_uid,radar_event_uid,last_jet)
  
  last_jet <- rbind(last_jet,last_jet_x)
  
}


# save last_jet to overtopping
ot_last_jet <- filtered_data %>% dplyr::left_join(last_jet, by = c("ow_uid", "radar_event_uid")) %>%
  dplyr::distinct()




#### 3.4 Plot Qpeak vs. Qmax, see above re: clunkiness; chunkiness ####


qpeak <- data.frame(matrix(ncol = 3))
colnames(qpeak) <- c("ow_uid","radar_event_uid","qpeak")

for(i in 1:nrow(mon_locs)){
  
  # Subset plot data
  plot_data <- filtered_data %>%
    dplyr::filter(smp_id == mon_locs$smp_id[i]) %>%
    dplyr::filter(ow_suffix == mon_locs$ow_suffix[i]) %>%
    dplyr::select(ow_uid, radar_event_uid, ow_suffix, eventdatastart_edt,
                  smp_id, eventavgintensity_inhr, eventpeakintensity_inhr, eventdepth_in, overtop)
  
  # Get System ID
  sys_id <- smp_2_sys(mon_locs$smp_id[i])
  
  # Grab design storm Depth  
  design_storm <- dbGetQuery(mars_con,
                             paste0("SELECT sys_creditedstormsizemanaged_in FROM external.tbl_systembdv
                                  WHERE system_id = '",sys_id,"'")) %>% pull
  
  # Select maintenance events
  event_data <- raw_dates %>% dplyr::filter(system_id == sys_id &
                                              graph == TRUE) %>%
    dplyr::select(system_id, date_complete, graph_text)
  
  
  # Select system characteristics
  loc_sys_char <- sys_char %>% dplyr::filter(smp_id == mon_locs$smp_id[i],
                                             GI == mon_locs$ow_suffix[i])
  
  # calculate qpeak; rational method using peak intensity as i. Q= CiA. Use full system drainage area for distr. pipe
  plot_data$qpeak <- plot_data$eventpeakintensity_inhr*0.95*loc_sys_char$System.Drainage.Area..SF. * (1/12) * (1/3600) # last terms to cfs
  
  # Let's try TR-55 for comparisons sake
  
  #impervious retention, S assuming Cn of 98 for impervious DA
  # tr55_S <- (1000/98) -10
  # 
  # plot_data$q_tr55 <- (plot_data$eventdepth_in - 0.2*tr55_S)^2/(plot_data$eventdepth_in + 0.8*tr55_S)
  # 
  # create plot
  qpeak_plot <- overtopping_qmax_plot(data =plot_data, 
                                      design_storm = design_storm,
                                      system_chars = loc_sys_char,
                                      event_dates = event_data$date_complete,
                                      event_descriptions = event_data$graph_text)    
  
  
  # Save Plot
  ggsave(plot = qpeak_plot,
         filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],"_qpeak.png"),
         width = 2100, height = 1620, units = "px")
  
  qpeak_x <- plot_data %>% dplyr::select(ow_uid,radar_event_uid,qpeak)
  
  qpeak <- rbind(qpeak, qpeak_x)
  
  
}


# add qpeak to ot_last_jet and save to server
ot_last_jet <- ot_last_jet %>% dplyr::left_join(qpeak, by = c("ow_uid", "radar_event_uid")) %>%
  dplyr::distinct()

write.csv(ot_last_jet,
          file = paste0(folderpath,latest_date,"/ot_with_last_jet_data.csv"))
