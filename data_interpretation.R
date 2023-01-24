#### Green Inlet Monitoring
#### Summary and Data Computation
#### Written by : Brian Cruice
#### Written on: 01/18/2022

### 0.1 packages
library(tidyverse)
library(odbc)
library(DBI)
library(pwdgsi)
library(lubridate)
library(ggplot2)
library(cowplot)

#### 1.0 Set up ####
# Read data, set up folders
folderpath <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/"

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

file_path <- paste0(latest_date,"/overtopping_data.csv")
raw_data <- read.csv(paste0(folderpath,"/",file_path))

# Cityworks events
event_dates <- read.csv(paste0(folderpath,"/","graph_dates.csv"))


#monitoring locations
mon_locs <- raw_data %>% dplyr::select(smp_id, ow_suffix) %>% distinct()


#smp to system fx
smp_2_sys <- function(smp_id){
  x <- str_split(smp_id, pattern = "-") %>% unlist()
  sys_id <- paste0(x[1],"-",x[2])
  return(sys_id)
}


#### 1.5 Essential Date Removals ####

#add system to raw_data
raw_data <- raw_data %>% dplyr::mutate(system_id = smp_2_sys(smp_id))

# Removing sections of time based on observations from ciytworks and SRTS

# 1-1 Taken offline
last_date_1_1 <- event_dates %>% dplyr::filter(system_id == '1-1') %>%
                dplyr::filter(grepl("Short-Circuiting Observed",graph_text) |
                              grepl("System Offline", graph_text)) %>%
                dplyr::select(date_complete) %>% unlist() %>% min()

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

# Make sure we're distinct
filtered_data <- distinct(filtered_data)

#### 2.0 Data investigation ####

# boxplots

ot_peak_bplot <- ggplot(data = filtered_data, aes(y = eventpeakintensity_inhr, x = smp_id, fill = overtop)) +
                 geom_boxplot(outlier.shape = NA) +
                 geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
                 xlab("SMP ID") + ylab("Event 15-minute Peak Intensity (in/hr)") +
                 ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +

#from pwdgsi plots; house style
ggplot2::theme(
  #text = element_text(size = rel(2)), #size previously set to 16
  axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
  axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
  axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
  panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
  panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
  panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
  panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
  legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
  legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_peak_bplot

ot_avg_bplot <- ggplot(data = filtered_data, aes(y = eventavgintensity_inhr, x = smp_id, fill = overtop)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event Mean Intensity (in/hr)") +
  ggtitle("Event Mean Intensities by SMP ID and Overtopping Status") +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_avg_bplot

ot_depth_bplot <- ggplot(data = filtered_data, aes(y = eventdepth_in, x = smp_id, fill = overtop)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event Depth (in)") +
  ggtitle("Event Depth by SMP ID and Overtopping Status") +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_depth_bplot

# look at seasonality (change these based on PWD season dates laters)

# Season Function
season_fx <- function(date_time){
  x <- mdy_hm(date_time)
  x_day <- x %>% day()
  x_mon <- x %>% month()
  
  #spring, summer, fall, winter
  x <- ifelse(x_mon == 4 | x_mon == 5 | (x_mon == 3 & x_day > 20) | (x_mon == 6 & x_day < 21),
              "Spring",
              
       ifelse((x_mon == 6 & x_day > 20) | x_mon == 7 | x_mon == 8 | (x_mon == 9 & x_day < 21),
              "Summer",
       
       ifelse((x_mon == 9 & x_day > 20) | x_mon == 10 | x_mon == 11 | (x_mon ==12 & x_day < 21),
              "Autumn",
              "Winter")))
  
  return(x)
  
}

filtered_data <- filtered_data %>% mutate(season = season_fx(eventdatastart_edt))

# Season, peak plot
ot_peak_season_bplot <- ggplot(data = filtered_data, aes(y = eventpeakintensity_inhr, x = season, fill = overtop)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event 15-minute Peak Intensity (in/hr)") +
  ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +
  scale_color_manual(values = c("darkgray","black")) + 
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_peak_season_bplot


# Save Plots



