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
library(plotly)
library(lme4)
library(xlsx)
library(kableExtra)
library(PerformanceAnalytics)

# Save plots?
plot_save <- FALSE

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

file_path <- paste0(latest_date,"/ot_with_last_jet_data.csv")
raw_data <- read.csv(paste0(folderpath,"/",file_path)) %>% dplyr::select(-X,-X.1)

# Cityworks events
event_dates <- read.csv(paste0(folderpath,"/","graph_dates.csv"))


# read inlet types
inlet_type <- xlsx::read.xlsx(file = paste0(folderpath,"Assets.xlsx"),
                              sheetName = "Inlet Depths")

# Read system characteristics
sys_char_file <- paste0(folderpath,"SystemCharacteristics.xlsx")
sys_char <- xlsx::read.xlsx(file = sys_char_file,
                            sheetName = "Characteristics")

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
raw_data$system_id <- sapply(raw_data$smp_id,smp_2_sys)

# Removing sections of time based on observations from ciytworks and SRTS

#first, make all dates the correct format.
raw_data$eventdatastart_edt %<>% ymd_hms() 
raw_data$eventdataend_edt   %<>% ymd_hms()

# 1-1 Taken offline
last_date_1_1 <- event_dates %>% dplyr::filter(system_id == '1-1') %>%
                dplyr::filter(grepl("Short-Circuiting Observed",graph_text) |
                              grepl("System Offline", graph_text)) %>%
                dplyr::select(date_complete) %>% unlist() %>% min() %>% mdy()

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

# check new total storms
filtered_data %>% dplyr::select(ow_uid,radar_event_uid) %>% distinct() %>% dplyr::group_by(ow_uid) %>% 
              summarize(storm_count = n()) %>% left_join(inlet_type) %>% dplyr::select(ow_uid,smp_id,storm_count)

#### 2.0 Data Visualization ####

# boxplots

ot_peak_bplot <- ggplot(data = filtered_data, aes(y = eventpeakintensity_inhr, x = smp_id, fill = overtop)) +
                 geom_boxplot(outlier.shape = NA) +
                 geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
                 xlab("SMP ID") + ylab("Event 15-minute Peak Intensity (in/hr)") +
                 ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +
                 geom_hline(yintercept = 2.5, color = "red", size = 1.5) +
                 # geom_text(label = "Philadelphia 1-year, 15-minute Peak Intensity: 2.5 in/hr",
                 #           y = 2.6, color = "black", size = 12 / .pt, hjust = "left") +
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


# Review trap vs. no trap
trap_status <- inlet_type %>% dplyr::select(ow_uid, Trap, drainage_area_sf)

filtered_data <- filtered_data %>% left_join(trap_status, by = "ow_uid")
filtered_data <- filtered_data %>% left_join(sys_char, by = 'system_id')

# peak intensity inlet Plots
sys_char$Max.Flow.w..Perforations..CFS. %<>% as.numeric()

ot_peak_inlet_plot <- ggplot(data = filtered_data, aes(y = eventpeakintensity_inhr, x = Trap, fill = overtop)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("Trap Present?") + ylab("Event 15-minute Peak Intensity (in/hr)") +
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

ot_peak_inlet_plot

# peak Q inlet plots

ot_peak_q_bplot <- ggplot(data = filtered_data, aes(y = qpeak, x = smp_id.x, fill = overtop)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event 15-minute Peak Flow, Qpeak (cfs)") +
  ggtitle("Event Peak Flow (Qpeak) by SMP ID and Overtopping Status") +
  geom_point(data = sys_char, aes(x = smp_id, y = Max.Flow.w..Perforations..CFS.), size = 3, shape = 25, fill = "yellow") + 
  
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

ot_peak_q_bplot



# Overtopping percent vs stat present

ot_trap_bplot <- ggplot(data = filtered_data, aes(y = eventpeakintensity_inhr, x = overtop, fill = Trap)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = Trap), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Overtopping") + ylab("Event 15-minute Peak Intensity (in/hr)") +
  ggtitle("Event 15-minute Peak Intensity by SMP ID and Overtopping Status") +

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

ot_trap_bplot

# Overtopping percent vs stat present

ot_trap_avg_bplot <- ggplot(data = filtered_data, aes(y = eventavgintensity_inhr, x = overtop, fill = Trap)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = Trap), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Overtopping") + ylab("Event 15-minute Peak Intensity (in/hr)") +
  ggtitle("Event 15-minute Peak Intensity by SMP ID and Overtopping Status") +
  
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

ot_trap_avg_bplot

# Save boxplots
bplot_folder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/boxplots"

if(plot_save == TRUE){
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intensity_by_SMP.png"), plot = ot_peak_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_avg_intensity_by_SMP.png"), plot = ot_avg_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intensity_by_inlet_type.png"), plot = ot_peak_inlet_plot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intensity_by_season.png"), plot = ot_peak_season_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_Q_by_SMP.png"), plot = ot_peak_q_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_avg_intensity_by_Trap.png"), plot = ot_trap_avg_bplot, width = 10, height = 8)
}


#### 2.5 Drainage Area ####

DA_plot <- ggplot(data = filtered_data, aes(x = drainage_area_sf, y = eventpeakintensity_inhr, col = overtop, size = overtop)) +
           geom_point() + 
           ylab("Event Peak Intensity (in/hr)") + xlab("Inlet Drainage Area (sf)") +
           scale_x_continuous(limits = c(0, 24000)) +
           ggtitle("Event Peak Intesity vs Inlet Drainage Area") +
  
  #add design storm values
  scale_color_manual(name = "Overtopping", values = c("steelblue3","firebrick3"), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
  scale_size_manual(name = "Overtopping", values = c(2,4), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
  #from pwdgsi plots
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





#### 2.7 Qpeak > Qmax, peak intensity > design intensity ####

# percent exceeding 2.5 in/hr
peak_int_perc <- 100*(sum(filtered_data$eventpeakintensity_inhr > 2.5)/
                        length(filtered_data$eventavgintensity_inhr))

# same percent, overtopping only
ot_filtered_data <- filtered_data %>% dplyr::filter(overtop == TRUE)

peak_int_perc <- 100*(sum(ot_filtered_data$eventpeakintensity_inhr > 2.5)/
                        length(ot_filtered_data$eventavgintensity_inhr))

#percent exceeding qmax
qmax <- sys_char %>% dplyr::select(smp_id, Max.Flow.w..Perforations..CFS.)
colnames(qmax) <- c("smp_id.x","qmax")
qmax$qmax %<>% as.numeric()

qmax <- left_join(filtered_data, qmax, by = "smp_id.x") %>%
        dplyr::select(radar_event_uid,smp_id.x,qpeak,qmax,overtop) %>% distinct()

qpeak_perc <- 100*(sum(qmax$qpeak > qmax$qmax)/
                          length(qmax$qpeak))

#assume 50% occlusioin


# same percent, overtopping only

qmax_oto <- qmax %>% dplyr::filter(overtop == TRUE)
qpeak_perc_oto <- 100*(sum(qmax_oto$qpeak > qmax_oto$qmax)/
                         length(qmax_oto$qpeak))


#### 3.0 STAT Model Creation ####



# 3.1 Indpendent variable transformations and scaling

# Look at some distributions
hist(filtered_data$eventpeakintensity_inhr)
# take ln to normalize 
hist(log(filtered_data$eventpeakintensity_inhr))

#average intensity
hist(filtered_data$eventavgintensity_inhr)
# take ln to normalize 
hist(log(filtered_data$eventavgintensity_inhr))


hist(filtered_data$qpeak)
hist(log(filtered_data$qpeak))

hist(filtered_data$last_jet)
hist(sqrt(filtered_data$last_jet))



# use transformation to get better residuals
filtered_data$log_peak_int <- log(filtered_data$eventpeakintensity_inhr)
filtered_data$log_avg_int <- log(filtered_data$eventavgintensity_inhr)
filtered_data$log_qpeak <- log(filtered_data$qpeak)
filtered_data$sqrt_last_jet <- sqrt(filtered_data$last_jet)
filtered_data$log_event_depth <- log(filtered_data$eventdepth_in)


#scale factors
filtered_data$scl_log_peak_int <- scale(filtered_data$log_peak_int)
filtered_data$scl_log_avg_int <- scale(filtered_data$log_avg_int)
filtered_data$scl_log_qpeak <- scale(filtered_data$log_qpeak)
filtered_data$scl_sqrt_last_jet <- scale(filtered_data$sqrt_last_jet)

#cool table time

variable_table <- data.frame(Variable = c("Event Depth", "Peak Intensity","Average Intensity", "Peak Flow", "Days Since Last Jet"),
                            Histogram = "",
                            Transformation = c("ln", "ln", "ln", "ln", "sqrt"),
                            Trans_histogram = "")

colnames(variable_table)[4] <- "Transformed Histogram"

var_list <- list(filtered_data$eventdepth_in,
                 filtered_data$eventpeakintensity_inhr,
                  filtered_data$eventavgintensity_inhr,
                  filtered_data$qpeak,
                  filtered_data$last_jet)
var_trans_list <- list(filtered_data$log_event_depth,
                        filtered_data$log_peak_int,
                        filtered_data$log_avg_int,
                        filtered_data$log_qpeak,
                        filtered_data$sqrt_last_jet)


var_trans_scale_list <- list(filtered_data$scl_log_peak_int,
                             filtered_data$scl_log_avg_int,
                             filtered_data$scl_log_peak_int,
                             filtered_data$scl_sqrt_last_jet)

variable_table %>% kbl(booktabs = TRUE) %>%
  kable_classic_2(full_width = TRUE) %>%
  column_spec(1, width = 1) %>%
  column_spec(2, width = 10, image = spec_hist(var_list, same_lim = FALSE)) %>%
  column_spec(3, width = 1) %>%
  column_spec(4, width = 10, image = spec_hist(var_trans_list, same_lim = FALSE)) %>%
  kable_styling(htmltable_class = 'lightable-classic-2', font_size = 16) %>%
  save_kable(file = paste0(bplot_folder,"/variable_table.png"), zoom = 1.5)


#scale variables


# 3.2 Correlation Matrix
filtered_data_by_site <- filtered_data %>% dplyr::group_by(ow_uid) %>%
  summarize(Size = n(),
            Overtop_pct = 100*sum(overtop)/n()) %>%
  distinct() %>%
  dplyr::left_join(select(filtered_data, ow_uid, system_id, Trap), by = "ow_uid") %>%  
  left_join(sys_char, by = "system_id") %>% distinct()


cor_plot <- filtered_data_by_site %>% dplyr::select(Overtop_pct,
                                                    System.Loading.Ratio,
                                                    System.Infiltration.Footprint..sf.,
                                                    Storm.Size.Managed..in.,
                                                    System.Drainage.Area..SF.,
                                                    Inlet.Drainage.Area..SF.,
                                                    Max.Flow.w..Perforations..CFS.,
                                                    Distrib..Length..FT.,
                                                    X..Distrib..Bends,
                                                    Distrib..Size.)

colnames(cor_plot) <- c("Overtop_pct",
                        "LR",
                        "Infil_fprint_sf",
                        "Storm_size_in",
                        "Sys_DA_sf",
                        "Inlet_DA_sf",
                        "Qmax_cfs",
                        "Dist_pipe_length",
                        "Number_of_dist_pipe_bends",
                        "Dist_pipe_size")
# make numeric
cols <- c(1:ncol(cor_plot))
cor_plot[,cols] = apply(cor_plot[,cols], 2, function(x) as.numeric(as.character(x)))

chart.Correlation(cor_plot, histogram = TRUE)
# png(filename = paste0(bplot_folder,"/correlation_plot.png"))


# Storm event correaltion matrix
cor_plot_storm <- filtered_data %>% dplyr::select(log_event_depth,
                                                  log_peak_int,
                                                  log_avg_int,
                                                  log_qpeak,
                                                  sqrt_last_jet)


colnames(cor_plot_storm) <- c("Event Depth (in)",
                              "Peak Infiltration (in/hr)",
                              "Average Infiltration (in/hr)",
                              "Peak Flow Rate (cfs)",
                              "Days Since Last Pipe Jetting")
# Remove infinite values
cor_plot_storm <- cor_plot_storm[!is.infinite(cor_plot_storm$`Peak Flow Rate (cfs)`),]

chart.Correlation(cor_plot_storm, histogram = TRUE)
# png(filename = paste0(bplot_folder,"/correlation_plot.png"))



# 3.3 ANOVA analysis for Trap Presence


# one site specific boxplot

#rename for graphic

filtered_data_by_site$`Inlet Type` <- filtered_data_by_site$Trap
filtered_data_by_site$`Inlet Type`[filtered_data_by_site$`Inlet Type` == TRUE] <- "Old (with Trap)"
filtered_data_by_site$`Inlet Type`[filtered_data_by_site$`Inlet Type` == FALSE] <- "New (without Trap)"

  
site_inlet_type_plot <- ggplot(data = filtered_data_by_site ,aes(y = Overtop_pct, x = `Inlet Type`, fill = `Inlet Type`)) + 
                            geom_boxplot() +
                            geom_point(position=position_jitterdodge(jitter.width = 0.1),
                                       aes(fill = `Inlet Type`), shape = 21, alpha = 0.7, size = 3) +
                            ylab("Percent of Storm Events with Overtopping (%)") +
                            ggtitle("Percent Overtopping by Inlet Type") +
  #house style plotting
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    title = ggplot2::element_text(size = ggplot2::rel(1.3), color = "black"), 
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "none", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(2.4)))

site_inlet_type_plot


if(plot_save == TRUE){
  ggsave(filename = paste0(bplot_folder,"/Overtopping_Percent_vs_Inlet_Type.png"), plot = site_inlet_type_plot, width = 7.5, height = 6)
}

#binomial ANOVA

trap_model <- glmer(data = filtered_data, overtop~Trap + (1|ow_uid), family = binomial)

trap_model <- glmer(data = filtered_data, overtop~Trap + (1|ow_uid), family = binomial)

summary(trap_model)

# 3.4 Predictive models

# Turn each monitoring location into a factor
filtered_data$ow_uid  <- filtered_data$ow_uid %>% as.factor()


# model 1
model1 <- glm(data = filtered_data, overtop~ Trap + eventpeakintensity_inhr, family = binomial)

hist(model1$residuals)
qqnorm(model1$residuals)

# model 2
model2 <- lm(overtop ~ Trap + eventpeakintensity_inhr, data = filtered_data)

anova(model2)
hist(model2$residuals)
shapiro.test(model2$residuals)
qqnorm(model2$residuals)

# model 3
model3 <- lm(overtop ~ Trap, data = filtered_data)

anova(model3)
hist(model3$residuals)
anova(model3)
qqnorm(model3$residuals)


# model 4
model4 <- glm(data = filtered_data, overtop~ Trap + log_peak_int, family = binomial)

hist(model4$residuals)
qqnorm(model4$residuals)
shapiro.test(model4$residuals)


# model 5

model5 <- lm(overtop ~ Trap + log_peak_int, data = filtered_data)

anova(model5)
hist(model5$residuals)
shapiro.test(model5$residuals)
qqnorm(model5$residuals)

# model 6

model6 <- lm(overtop ~ Trap + log_avg_int, data = filtered_data)

anova(model6)
hist(model6$residuals)
shapiro.test(model6$residuals)
qqnorm(model6$residuals)

# model 7
model7 <- lm(overtop ~ log_avg_int, data = filtered_data)

anova(model7)
hist(model7$residuals)
shapiro.test(model7$residuals)
qqnorm(model7$residuals); qqline(model7$residuals)

# model 8
model8 <- lm(overtop ~ log_peak_int, data = filtered_data)

anova(model8)
hist(model8$residuals)
shapiro.test(model8$residuals)
qqnorm(model8$residuals); qqline(model8$residuals)

# model 9
model9 <- lm(overtop ~ log_peak_int + Trap + last_jet,data = filtered_data)

anova(model9) # ANOVA results show days since jetting being insignificant
hist(model9$residuals)
shapiro.test(model9$residuals)
qqnorm(model9$residuals); qqline(model9$residuals)
summary(model9)

# model 10
# ow_uid as character to apply clustering as a random effects variable
filtered_data$ow_uid <- filtered_data$ow_uid %>% as.character()

# rescale variables
hist(filtered_data$log_peak_int)
hist(scale(filtered_data$log_peak_int))


model10 <- glmer(overtop ~ log_qpeak + Trap + sqrt_last_jet + (1|ow_uid), data = filtered_data, family = binomial())

model10_residuals <- resid(model10)
anova(model10)
hist(model10_residuals)
shapiro.test(model10_residuals)
qqnorm(model10_residuals); qqline(model10_residuals)

#model 11

#scale inlet drainage area
filtered_data$scl_inl_DA <- scale(filtered_data$Inlet.Drainage.Area..SF.)

model11 <- glmer(overtop ~ log_qpeak + sqrt_last_jet + scl_inl_DA + Distrib..Size. + (1|ow_uid), data = filtered_data, family = binomial())

model11_residuals <- resid(model11)
anova(model11)
hist(model11_residuals)
shapiro.test(model11_residuals)
qqnorm(model11_residuals); qqline(model11_residuals)
summary(model11)


#model12

model12 <- glmer(overtop ~  log_qpeak + sqrt_last_jet + scl_inl_DA + Trap + (1|ow_uid), data = filtered_data, family = binomial())

model12_residuals <- resid(model12)
anova(model12)
hist(model12_residuals)
shapiro.test(model12_residuals)
qqnorm(model12_residuals); abline(a = 0, b = 1)
summary(model12)


# overtopping percentage vs. max distribution pipe flow way

plot(filtered_data_by_site$Dist.Pipe.Head..ft.,filtered_data_by_site$Overtop_pct)
