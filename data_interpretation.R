#### Green Inlet Monitoring
#### Summary and Data Computation
#### Written by : Brian Cruice
#### Written on: 01/18/2022
#### Run on: 04/03/2024

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
library(wesanderson)

# Save plots?
plot_save <- TRUE

#### 1.0 Set up ####
#Database Connection
mars_con <- odbc::dbConnect(odbc::odbc(), "mars14_datav2")

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
raw_data <- read.csv(paste0(folderpath,file_path)) %>% dplyr::select(-X,-X.1)

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

# Removing sections of time based on observations from cityworks and SRTS

# first, make all dates the correct format.
date_formats <- c(guess_formats(raw_data$eventdatastart_edt, "mdy HMS"),
                  guess_formats(raw_data$eventdatastart_edt, "mdy"))

start_dates_x <- raw_data$eventdatastart_edt %>% lubridate::as_datetime(format = date_formats)
end_dates_x <-  raw_data$eventdataend_edt %>% lubridate::as_datetime(format = date_formats)

raw_data$eventdatastart_edt <- start_dates_x
raw_data$eventdataend_edt <- end_dates_x

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


# Normalize head differential
filtered_data <- filtered_data %>%
                 group_by(smp_id) %>%
                 mutate(max_head = max(rel_head_dif)) %>%
                 ungroup() %>%
                 mutate(norm_head = rel_head_dif/max_head) %>%
                 select(-max_head)
  

filtered_data <- filtered_data %>% dplyr::filter(norm_head >= 0)


#### 1.6 New Data Summary After Date Filtration ####
# design depths
sysbdv <- dbGetQuery(mars_con,
                     paste0("SELECT * FROM external.tbl_systembdv where system_id IN ('",
                            paste(unique(filtered_data$system_id), collapse = "', '"),
                            "')")
)

sys_dsgn_strm <- sysbdv %>% dplyr::select(system_id, sys_creditedstormsizemanaged_in)

# join to ot_data
dsgn_storm_data <- filtered_data %>%
  dplyr::left_join(sys_dsgn_strm, by = 'system_id') %>%
  dplyr::filter(eventdepth_in <= sys_creditedstormsizemanaged_in)

# summarize design data
gi_dsgn_summary <- dsgn_storm_data %>% group_by(ow_uid) %>% summarize(n = n(),
                                                                      overtopping_count = sum(overtop),
                                                                      mean_event = mean(eventdepth_in),
                                                                      avg_RPSU = mean(percentstorageused_relative, na.rm = TRUE)) %>%
  dplyr::left_join(dsgn_storm_data, by = "ow_uid") %>%
  dplyr::select(ow_uid,smp_id,ow_suffix,mean_event,n,overtopping_count,avg_RPSU) %>%
  unique() %>% dplyr::filter(!is.na(smp_id))

gi_dsgn_summary <- gi_dsgn_summary %>% dplyr::mutate(overtop_perc = overtopping_count/n)

write.csv(gi_dsgn_summary, paste0(folderpath, "/", latest_date,"/gi_design_storm_summary.csv"))

#### 2.0 Data Visualization ####


#### 2.1 Overtopping Plots for all storms ####
# boxplots

ot_peak_bplot <- ggplot(data = filtered_data, aes(y = eventpeakintensity_inhr, x = smp_id, fill = overtop)) +
                 geom_boxplot(size = 1.1, outlier.shape = NA) +
                 geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
                 xlab("SMP ID") + ylab("Event 15-minute Peak Intensity (in/hr)") +
                 ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +
                 geom_hline(yintercept = 2.5, color = "red", size = 1.5) +
                 # geom_text(label = "Philadelphia 1-year, 15-minute Peak Intensity: 2.5 in/hr",
                 #           y = 2.6, color = "black", size = 12 / .pt, hjust = "left") +
                 #from pwdgsi plots; house style
                 scale_fill_manual(values = wes_palettes$Moonrise2) +
                  
                 #from pwdgsi plots; house style
                 ggplot2::theme(
                    #text = element_text(size = rel(2)), #size previously set to 16
                    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
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
                geom_boxplot(size = 1.1, outlier.shape = NA) +
                geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
                xlab("SMP ID") + ylab("Event Mean Intensity (in/hr)") +
                ggtitle("Event Mean Intensities by SMP ID and Overtopping Status") +
                scale_fill_manual(values = wes_palettes$Moonrise2) +
                
                #from pwdgsi plots; house style
                ggplot2::theme(
                  #text = element_text(size = rel(2)), #size previously set to 16
                  axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                  axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                  title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
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
                  geom_boxplot(size = 1.1, outlier.shape = NA) +
                  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
                  xlab("SMP ID") + ylab("Event Depth (in)") +
                  ggtitle("Event Depth by SMP ID and Overtopping Status") +
                  scale_fill_manual(values = wes_palettes$Moonrise2) +
                  
                  #from pwdgsi plots; house style
                  ggplot2::theme(
                    #text = element_text(size = rel(2)), #size previously set to 16
                    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
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
   x <- date_time
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
                        geom_boxplot(size = 1.1, outlier.shape = NA) +
                        geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
                        xlab("SMP ID") + ylab("Event 15-minute Peak Intensity (in/hr)") +
                        ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +
                        scale_color_manual(values = c("darkgray","black")) + 
                        scale_fill_manual(values = wes_palettes$Moonrise2) +
                        
                        #from pwdgsi plots; house style
                        ggplot2::theme(
                          #text = element_text(size = rel(2)), #size previously set to 16
                          axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                          axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                          axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                          panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                          panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                          panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                          panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                          legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                          legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))


ot_peak_season_bplot


filtered_data %>% dplyr::group_by(season) %>% summarise(Perc_szn_overtop = 100*sum(overtop)/n()) %>% dplyr::select(season, Perc_szn_overtop) %>% write.table(file = "clipboard")

# Review trap vs. no trap
trap_status <- inlet_type %>% dplyr::select(ow_uid, Trap, drainage_area_sf)

filtered_data <- filtered_data %>% left_join(trap_status, by = "ow_uid")
filtered_data <- filtered_data %>% left_join(dplyr::select(sys_char, -smp_id), by = 'system_id')

filtered_data$Sys_Age <- lubridate::as.difftime(filtered_data$eventdatastart_edt - lubridate::as_datetime(filtered_data$Construction.Complete.Date))

# peak intensity inlet Plots
sys_char$Max.Flow.w..Perforations..CFS. %<>% as.numeric()

ot_peak_inlet_plot <- ggplot(data = filtered_data, aes(y = eventpeakintensity_inhr, x = Trap, fill = overtop)) +
                        geom_boxplot(size = 1.1, outlier.shape = NA) +
                        geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
                        xlab("Trap Present?") + ylab("Event 15-minute Peak Intensity (in/hr)") +
                        ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +
                        scale_fill_manual(values = wes_palettes$Moonrise2) +
                        
                        #from pwdgsi plots; house style
                        ggplot2::theme(
                          #text = element_text(size = rel(2)), #size previously set to 16
                          axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
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

ot_peak_q_bplot <- ggplot(data = filtered_data, aes(y = qpeak, x = smp_id, fill = overtop)) +
                    geom_boxplot(size = 1.1, outlier.shape = NA) +
                    geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
                    xlab("SMP ID") + ylab("Event 15-minute Peak Flow, Qpeak (cfs)") +
                    ggtitle("Event Peak Flow (Qpeak) by SMP ID and Overtopping Status") +
                    geom_point(data = sys_char, aes(x = smp_id, y = Max.Flow.w..Perforations..CFS.), size = 3, shape = 25, fill = "yellow") + 
                    scale_fill_manual(values = wes_palettes$Moonrise2) +
                    
                    #from pwdgsi plots; house style
                    ggplot2::theme(
                      #text = element_text(size = rel(2)), #size previously set to 16
                      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                      axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                      title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
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
                  geom_boxplot(size = 1.1, outlier.shape = NA) +
                  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = Trap), shape = 21, alpha = 0.7, size = 1.5) +
                  xlab("Overtopping") + ylab("Event 15-minute Peak Intensity (in/hr)") +
                  ggtitle("Event 15-minute Peak Intensity by SMP ID and Overtopping Status") +
                  scale_fill_manual(values = wes_palettes$Moonrise2) +
                  
                  #from pwdgsi plots; house style
                  ggplot2::theme(
                    #text = element_text(size = rel(2)), #size previously set to 16
                    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
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
                      geom_boxplot(size = 1.1, outlier.shape = NA) +
                      geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = Trap), shape = 21, alpha = 0.7, size = 1.5) +
                      xlab("Overtopping") + ylab("Event 15-minute Peak Intensity (in/hr)") +
                      ggtitle("Event 15-minute Peak Intensity by SMP ID and Overtopping Status") +
                      scale_fill_manual(values = wes_palettes$Moonrise2) +
                      
                      #from pwdgsi plots; house style
                      ggplot2::theme(
                        #text = element_text(size = rel(2)), #size previously set to 16
                        axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                        axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                        title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                        axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                        panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                        panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                        panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                        panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                        legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                        legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_trap_avg_bplot

# Slope plot

#summarize by site

filtered_data_by_site <- filtered_data %>% dplyr::group_by(ow_uid) %>%
  summarize(Size = n(),
            mean_head_dif = mean(rel_head_dif),
            Overtop_pct = 100*sum(overtop)/n()) %>%
  distinct() %>%
  dplyr::left_join(select(filtered_data, ow_uid, system_id, Trap), by = "ow_uid") %>%  
  left_join(sys_char, by = "system_id") %>% distinct()

#boxplot

ot_peak_slope_bplot <- ggplot(data = filtered_data_by_site, aes(y = Overtop_pct, x = as.factor(Distrib..Slope....), fill = as.factor(Distrib..Slope....))) +
                        geom_boxplot(size = 1.1, outlier.shape = NA) +
                        geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 4) +
                        xlab("Distribution Pipe Slope") + ylab("Percent Overtopping") +
                        ggtitle("Percent Overtopping by Distribution Pipe Slope") +
                        scale_color_manual(values = c("darkgray","black")) + 
                        scale_fill_manual(values = wes_palettes$Moonrise2) + 
                        
                        #from pwdgsi plots; house style
                        ggplot2::theme(
                          #text = element_text(size = rel(2)), #size previously set to 16
                          axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                          axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                          axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                          panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                          panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                          panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                          panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                          legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                          legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_peak_slope_bplot

#### 2.2 Overtopping Plots for design storms ####

design_data <-filtered_data %>% 
  dplyr::filter(eventdepth_in <= Storm.Size.Managed..in.)


ot_peak_bplot_des <- ggplot(data = design_data, aes(y = eventpeakintensity_inhr, x = smp_id, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event 15-minute Peak Intensity (in/hr)") +
  ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +
  geom_hline(yintercept = 2.5, color = "red", size = 1.5) +
  # geom_text(label = "Philadelphia 1-year, 15-minute Peak Intensity: 2.5 in/hr",
  #           y = 2.6, color = "black", size = 12 / .pt, hjust = "left") +
  #from pwdgsi plots; house style
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_peak_bplot_des

ot_avg_bplot_des <- ggplot(data = design_data, aes(y = eventavgintensity_inhr, x = smp_id, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event Mean Intensity (in/hr)") +
  ggtitle("Event Mean Intensities by SMP ID and Overtopping Status") +
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_avg_bplot_des

ot_depth_bplot_des <- ggplot(data = design_data, aes(y = eventdepth_in, x = smp_id, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event Depth (in)") +
  ggtitle("Event Depth by SMP ID and Overtopping Status") +
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))


ot_depth_bplot_des

# look at seasonality (change these based on PWD season dates laters)

design_data <- design_data %>% mutate(season = season_fx(eventdatastart_edt))



# Season, peak plot
ot_peak_season_bplot_des <- ggplot(data = design_data, aes(y = eventpeakintensity_inhr, x = season, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event 15-minute Peak Intensity (in/hr)") +
  ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))


ot_peak_season_bplot_des


design_data %>% dplyr::group_by(season) %>% summarise(Perc_szn_overtop = 100*sum(overtop)/n()) %>% dplyr::select(season, Perc_szn_overtop) %>% write.table(file = "clipboard")

# Review trap vs. no trap
trap_status <- inlet_type %>% dplyr::select(ow_uid, Trap, drainage_area_sf)

design_data <- design_data %>% left_join(trap_status, by = "ow_uid")
# design_data <- design_data %>% left_join(sys_char, by = 'system_id')

design_data$Sys_Age <- lubridate::as.difftime(design_data$eventdatastart_edt - lubridate::as_datetime(design_data$Construction.Complete.Date))

# peak intensity inlet Plots
sys_char$Max.Flow.w..Perforations..CFS. %<>% as.numeric()

ot_peak_inlet_plot_des <- ggplot(data = design_data, aes(y = eventpeakintensity_inhr, x = Trap.x, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("Trap Present?") + ylab("Event 15-minute Peak Intensity (in/hr)") +
  ggtitle("Event Peak Intensities by SMP ID and Overtopping Status") +
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_peak_inlet_plot_des

# peak Q inlet plots

ot_peak_q_bplot_des <- ggplot(data = design_data, aes(y = qpeak, x = smp_id, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = overtop), shape = 21, alpha = 0.7) +
  xlab("SMP ID") + ylab("Event 15-minute Peak Flow, Qpeak (cfs)") +
  ggtitle("Event Peak Flow (Qpeak) by SMP ID and Overtopping Status") +
  geom_point(data = sys_char, aes(x = smp_id, y = Max.Flow.w..Perforations..CFS.), size = 3, shape = 25, fill = "yellow") + 
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_peak_q_bplot_des



# Overtopping percent vs stat present

ot_trap_bplot_des <- ggplot(data = design_data, aes(y = eventpeakintensity_inhr, x = overtop, fill = Trap.x)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = Trap.x), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Overtopping") + ylab("Event 15-minute Peak Intensity (in/hr)") +
  ggtitle("Event 15-minute Peak Intensity by SMP ID and Overtopping Status") +
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_trap_bplot_des

# Overtopping percent vs stat present

ot_trap_avg_bplot_des <- ggplot(data = design_data, aes(y = eventavgintensity_inhr, x = overtop, fill = Trap.x)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), aes(fill = Trap.x), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Overtopping") + ylab("Event 15-minute Peak Intensity (in/hr)") +
  ggtitle("Event 15-minute Peak Intensity by SMP ID and Overtopping Status") +
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_trap_avg_bplot_des

# Slope plot

#summarize by site

design_data_by_site <- design_data %>% dplyr::group_by(ow_uid) %>%
  summarize(Size = n(),
            Overtop_pct = 100*sum(overtop)/n()) %>%
  distinct() %>%
  dplyr::left_join(select(design_data, ow_uid, system_id, Trap.x), by = "ow_uid") %>%  
  left_join(sys_char, by = "system_id") %>% distinct()

#boxplot

ot_peak_slope_bplot_des <- ggplot(data = design_data_by_site, aes(y = Overtop_pct, x = as.factor(Distrib..Slope....), fill = as.factor(Distrib..Slope....))) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 4) +
  xlab("Distribution Pipe Slope") + ylab("Percent Overtopping") +
  ggtitle("Percent Overtopping by Distribution Pipe Slope") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  

  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_peak_slope_bplot_des

#### 2.3 Head Differential Visualizations ####

# By pipe slope

# by pipe slope labels
label_text <- filtered_data %>%
  dplyr::group_by(Distrib..Slope....) %>%
  summarize(slope_count = n()) %>%
  ungroup() %>%
  right_join(filtered_data, by = c("Distrib..Slope....")) %>%
  dplyr::group_by(overtop, Distrib..Slope....) %>%
  summarize(label = paste0(round(100*n()/slope_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(overtop,Distrib..Slope....,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

ot_hdif_slope_bplot <- ggplot(data = filtered_data, aes(y = rel_head_dif, x = as.factor(Distrib..Slope....), fill = overtop)) +
                        geom_boxplot(size = 1.1, outlier.shape = NA) +
                        geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
                        xlab("Distribution Pipe Slope (%)") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
                        ggtitle("Overtopping by Head Differential and Distribution Pipe Slope") +
                        scale_fill_manual(values = c("grey41","grey82")) + 
                        scale_fill_manual(values = wes_palettes$Moonrise2) + 
                        
                        geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
                                   hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
                        
                        #from pwdgsi plots; house style
                        ggplot2::theme(
                          #text = element_text(size = rel(2)), #size previously set to 16
                          axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                          axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                          axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                          panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                          panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                          panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                          panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                          legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                          legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_hdif_slope_bplot

## By system

#by system labels
label_text <- filtered_data %>%
  dplyr::group_by(system_id) %>%
  summarize(sys_count = n()) %>%
  ungroup() %>%
  right_join(filtered_data, by = c("system_id")) %>%
  dplyr::group_by(overtop, system_id) %>%
  summarize(label = paste0(round(100*n()/sys_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(overtop,system_id,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

ot_hdif_system_bplot <- ggplot(data = filtered_data, aes(y = rel_head_dif, x = system_id, fill = overtop)) +
                        geom_boxplot(size = 1.1, outlier.shape = NA) +
                        geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
                        xlab("System ID") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
                        ggtitle("Overtopping by Head Differential per System") +
                        scale_color_manual(values = c("darkgray","black")) + 
                        scale_fill_manual(values = wes_palettes$Moonrise2) + 
                        
                        geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
                                   hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
                        
                        #from pwdgsi plots; house style
                        ggplot2::theme(
                          #text = element_text(size = rel(2)), #size previously set to 16
                          axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                          axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                          axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                          panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                          panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                          panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                          panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                          legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                          legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_hdif_system_bplot


## By Season

#by season,system labels
label_text <- filtered_data %>%
  dplyr::group_by(system_id) %>%
  summarize(sys_count = n()) %>%
  ungroup() %>%
  right_join(filtered_data, by = c("system_id")) %>%
  dplyr::group_by(season, system_id) %>%
  summarize(label = paste0(round(100*n()/sys_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(season,system_id,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_szn_sys_bplot <- ggplot(data = filtered_data, aes(y = rel_head_dif, x = system_id, fill = season)) +
                      geom_boxplot(size = 1.1, outlier.shape = NA) +
                      geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
                      xlab("System ID") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
                      ggtitle("Head Differential by System and Season") +
                      scale_color_manual(values = c("darkgray","black")) + 
                      scale_fill_manual(values = wes_palettes$Darjeeling2) +
                      
                      geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
                                 hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
                      
                      
                      #from pwdgsi plots; house style
                      ggplot2::theme(
                        #text = element_text(size = rel(2)), #size previously set to 16
                        axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                        axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                        title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                        axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                        panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                        panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                        panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                        panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                        legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                        legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_szn_sys_bplot

#by season labels
label_text <- filtered_data %>%
  dplyr::group_by(season) %>%
  summarize(season_count = n()) %>%
  ungroup() %>%
  right_join(filtered_data, by = "season") %>%
  dplyr::group_by(season, overtop) %>%
  summarize(label = paste0(round(100*n()/season_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(season,overtop,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_szn_ot_bplot <- ggplot(data = filtered_data, aes(y = rel_head_dif, x = overtop, fill = season)) +
                      geom_boxplot(size = 1.1, outlier.shape = NA) +
                      geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
                      xlab("Overtopping") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
                      ggtitle("Head Differential by Season") +
                      scale_color_manual(values = c("darkgray","black")) + 
                      # scale_fill_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE")) + 
                      scale_fill_manual(values = wes_palettes$Darjeeling2) +
                      
                      geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
                                 hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
                      
                      #from pwdgsi plots; house style
                      ggplot2::theme(
                        #text = element_text(size = rel(2)), #size previously set to 16
                        axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                        axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                        title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                        axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                        panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                        panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                        panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                        panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                        legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                        legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_szn_ot_bplot

## By inlet type
filtered_data$inlet_style <- NA
filtered_data[filtered_data$Trap == FALSE,]$inlet_style <- "New (Without Trap)"
filtered_data[filtered_data$Trap == TRUE,]$inlet_style <- "Old (with Trap)"

label_text <- filtered_data %>%
              dplyr::group_by(inlet_style) %>%
              summarize(style_count = n()) %>%
              ungroup() %>%
              right_join(filtered_data, by = "inlet_style") %>%
              dplyr::group_by(inlet_style, overtop) %>%
              summarize(label = paste0(round(100*n()/style_count,0),"%"),
                        rel_head_dif = max(rel_head_dif)) %>%
              dplyr::select(inlet_style,overtop,label, rel_head_dif) %>%
              dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_inlet_ot_bplot <- ggplot(data = filtered_data, aes(y = rel_head_dif, x = inlet_style, fill = overtop)) +
                        geom_boxplot(size = 1.1, outlier.shape = NA) +
                        geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
                        xlab("Inlet Type") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
                        ggtitle("Head Differential by Inlet Type") +
                        scale_color_manual(values = c("darkgray","black")) + 
                        scale_fill_manual(values = wes_palettes$Moonrise2) + 
                        
                        geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
                                   hjust = "center", color = "white", fontface = "bold", show.legend = FALSE, label.size = 1.2) +
                        
                        
                        #from pwdgsi plots; house style
                        ggplot2::theme(
                          #text = element_text(size = rel(2)), #size previously set to 16
                          axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                          title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                          axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                          axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                          panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                          panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                          panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                          panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                          legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                          legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_inlet_ot_bplot



## Inlet type and Sump Depth?
#By inlet type
filtered_data$inlet_style <- NA
filtered_data[filtered_data$Trap == FALSE,]$inlet_style <- "New (Without Trap)"
filtered_data[filtered_data$Trap == TRUE,]$inlet_style <- "Old (with Trap)"

label_text <- filtered_data %>%
  dplyr::group_by(inlet_style) %>%
  summarize(style_count = n()) %>%
  ungroup() %>%
  right_join(filtered_data, by = "inlet_style") %>%
  dplyr::group_by(inlet_style, overtop) %>%
  summarize(label = paste0(round(100*n()/style_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(inlet_style,overtop,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_inlet_ot_sz_bplot <- ggplot(data = filtered_data, aes(y = rel_head_dif, x = inlet_style, fill = overtop)) +
                          geom_boxplot(size = 1.1, outlier.shape = NA) +
                          geom_point(aes(size = Sump.Depth..ft.),position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7) +
                          xlab("Inlet Type") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
                          ggtitle("Head Differential by Inlet Type") +
                          scale_color_manual(values = c("darkgray","black")) + 
                          scale_fill_manual(values = wes_palettes$Moonrise2) + 
                          
                          geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
                                     hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
                          
                          
                          #from pwdgsi plots; house style
                          ggplot2::theme(
                            #text = element_text(size = rel(2)), #size previously set to 16
                            axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                            axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                            title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                            axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                            axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                            panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                            panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                            panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                            panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                            legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                            legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_inlet_ot_sz_bplot

## Sump depth and overtopping



filtered_data$Sump.Depth..ft. <- round(filtered_data$Sump.Depth..ft.,2)

hdif_sump_ot <- ggplot(data = filtered_data, aes(y = rel_head_dif, x = as.factor(Sump.Depth..ft.), fill = overtop)) + 
                geom_boxplot(size = 1.1, outlier.shape = NA)+
                geom_point(aes(size = eventdepth_in), shape = 21, alpha = 0.7) +
                xlab("Sump Depth (ft)") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
                ggtitle("Head Differential by Sump Depth") +  
                labs(fill = "Overtopping", size = "Event Depth (in)") +
                scale_fill_manual(values = wes_palettes$Moonrise2) +
                
                #from pwdgsi plots; house style
                ggplot2::theme(
                  #text = element_text(size = rel(2)), #size previously set to 16
                  axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                  axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                  title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
                  axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                  axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                  panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                  panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                  panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
                  panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
                  legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                  legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_sump_ot


## Filter Bag Type
# pretreat <- inlet_type %>% dplyr::select(ow_uid,pretreatment)
# 
# filtered_data <- filtered_data %>% left_join(pretreat, by = "ow_uid")

label_text <- filtered_data %>%
  dplyr::group_by(Filter.Bags.Used) %>%
  summarize(style_count = n()) %>%
  ungroup() %>%
  right_join(filtered_data, by = "Filter.Bags.Used") %>%
  dplyr::group_by(Filter.Bags.Used, overtop) %>%
  summarize(label = paste0(round(100*n()/style_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(Filter.Bags.Used,overtop,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_fbag_ot_bplot <- ggplot(data = filtered_data, aes(y = rel_head_dif, x = Filter.Bags.Used, fill = overtop)) +
  geom_boxplot(size = 1.1,outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7) +
  xlab("Filter Bag Type") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Head Differential by Filter Bag Type") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_fbag_ot_bplot

## System Age plots

hdif_vs_age <- ggplot(data = filtered_data) + geom_point(aes(x = Sys_Age/365, y = rel_head_dif, col = Total...Tree.Pits)) +
    xlab("System Age (years)") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
    ggtitle("Head Differential by System Age") +  
    labs(color = "Number of Trees") +
    
    #from pwdgsi plots; house style
    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_vs_age

#### 2.4 Head Dif, design storm only ####

design_data <-filtered_data %>% 
              dplyr::filter(eventdepth_in <= Storm.Size.Managed..in.)

# By pipe slope

# by pipe slope labels
label_text <- design_data %>%
  dplyr::group_by(Distrib..Slope....) %>%
  summarize(slope_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = c("Distrib..Slope....")) %>%
  dplyr::group_by(overtop, Distrib..Slope....) %>%
  summarize(label = paste0(round(100*n()/slope_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(overtop,Distrib..Slope....,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

ot_hdif_slope_bplot_des <- ggplot(data = design_data, aes(y = rel_head_dif, x = as.factor(Distrib..Slope....), fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Distribution Pipe Slope (%)") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Overtopping by Head Differential and Distribution Pipe Slope") +
  scale_fill_manual(values = c("grey41","grey82")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_hdif_slope_bplot_des

## By system

#by system labels
label_text <- design_data %>%
  dplyr::group_by(system_id) %>%
  summarize(sys_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = c("system_id")) %>%
  dplyr::group_by(overtop, system_id) %>%
  summarize(label = paste0(round(100*n()/sys_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(overtop,system_id,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

ot_hdif_system_bplot_des <- ggplot(data = design_data, aes(y = rel_head_dif, x = system_id, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("System ID") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Overtopping by Head Differential per System") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_hdif_system_bplot_des


## By Season

#by season,system labels
label_text <- design_data %>%
  dplyr::group_by(system_id) %>%
  summarize(sys_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = c("system_id")) %>%
  dplyr::group_by(season, system_id) %>%
  summarize(label = paste0(round(100*n()/sys_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(season,system_id,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_szn_sys_bplot_des <- ggplot(data = design_data, aes(y = rel_head_dif, x = system_id, fill = season)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("System ID") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Head Differential by System and Season") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Darjeeling2) +
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_szn_sys_bplot_des

#by season labels
label_text <- design_data %>%
  dplyr::group_by(season) %>%
  summarize(season_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = "season") %>%
  dplyr::group_by(season, overtop) %>%
  summarize(label = paste0(round(100*n()/season_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(season,overtop,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_szn_ot_bplot_des <- ggplot(data = design_data, aes(y = rel_head_dif, x = overtop, fill = season)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Overtopping") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Head Differential by Season") +
  scale_color_manual(values = c("darkgray","black")) + 
  # scale_fill_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE")) + 
  scale_fill_manual(values = wes_palettes$Darjeeling2) +
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_szn_ot_bplot_des

## By inlet type
design_data$inlet_style <- NA
design_data[design_data$Trap == FALSE,]$inlet_style <- "New (Without Trap)"
design_data[design_data$Trap == TRUE,]$inlet_style <- "Old (with Trap)"

label_text <- design_data %>%
  dplyr::group_by(inlet_style) %>%
  summarize(style_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = "inlet_style") %>%
  dplyr::group_by(inlet_style, overtop) %>%
  summarize(label = paste0(round(100*n()/style_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(inlet_style,overtop,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_inlet_ot_bplot_des <- ggplot(data = design_data, aes(y = rel_head_dif, x = inlet_style, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Inlet Type") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Head Differential by Inlet Type") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_inlet_ot_bplot_des



## Inlet type and Sump Depth?
#By inlet type
design_data$inlet_style <- NA
design_data[design_data$Trap == FALSE,]$inlet_style <- "New (Without Trap)"
design_data[design_data$Trap == TRUE,]$inlet_style <- "Old (with Trap)"

label_text <- design_data %>%
  dplyr::group_by(inlet_style) %>%
  summarize(style_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = "inlet_style") %>%
  dplyr::group_by(inlet_style, overtop) %>%
  summarize(label = paste0(round(100*n()/style_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(inlet_style,overtop,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_inlet_ot_sz_bplot_des <- ggplot(data = design_data, aes(y = rel_head_dif, x = inlet_style, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(aes(size = Sump.Depth..ft.),position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7) +
  xlab("Inlet Type") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Head Differential by Inlet Type") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_inlet_ot_sz_bplot_des

## Sump depth and overtopping



design_data$Sump.Depth..ft. <- round(design_data$Sump.Depth..ft.,2)

hdif_sump_ot_des <- ggplot(data = design_data, aes(y = rel_head_dif, x = as.factor(Sump.Depth..ft.), fill = overtop)) + 
  geom_boxplot(size = 1.1, outlier.shape = NA)+
  geom_point(aes(size = eventdepth_in), shape = 21, alpha = 0.7) +
  xlab("Sump Depth (ft)") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Head Differential by Sump Depth") +  
  labs(fill = "Overtopping", size = "Event Depth (in)") +
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_sump_ot_des


## Filter Bag Type
# pretreat <- inlet_type %>% dplyr::select(ow_uid,pretreatment)

# design_data <- design_data %>% left_join(pretreat, by = "ow_uid")

label_text <- design_data %>%
  dplyr::group_by(Filter.Bags.Used) %>%
  summarize(style_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = "Filter.Bags.Used") %>%
  dplyr::group_by(Filter.Bags.Used, overtop) %>%
  summarize(label = paste0(round(100*n()/style_count,0),"%"),
            rel_head_dif = max(rel_head_dif)) %>%
  dplyr::select(Filter.Bags.Used,overtop,label, rel_head_dif) %>%
  dplyr::distinct()

label_text$rel_head_dif <- max(label_text$rel_head_dif, na.rm = TRUE)

hdif_fbag_ot_bplot_des <- ggplot(data = design_data, aes(y = rel_head_dif, x = Filter.Bags.Used, fill = overtop)) +
  geom_boxplot(size = 1.1,outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7) +
  xlab("Filter Bag Type") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Head Differential by Filter Bag Type") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_fbag_ot_bplot_des

## System Age plots

hdif_vs_age_des <- ggplot(data = design_data) + geom_point(aes(x = Sys_Age/365, y = rel_head_dif, col = Total...Tree.Pits)) +
  xlab("System Age (years)") + ylab("Head Differential at Peak Water Level in Green Inlet (ft)") +
  ggtitle("Head Differential by System Age") +  
  labs(color = "Number of Trees") +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_vs_age_des

#### 2.4.5 Normalized head dif, design storms only ####

# By pipe slope
slope_x <- design_data$Distrib..Slope.... == 0.5
slope_x <- factor(slope_x, ordered = TRUE, levels = c(TRUE, FALSE))
design_data$Distribution_slope <- slope_x


# by pipe slope labels
label_text <- design_data %>%
  dplyr::group_by(Distribution_slope) %>%
  summarize(slope_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = c("Distribution_slope")) %>%
  dplyr::group_by(overtop, Distribution_slope) %>%
  summarize(label = paste0(round(100*n()/slope_count,0),"%"),
            norm_head = max(norm_head)) %>%
  dplyr::select(overtop,Distribution_slope,label, norm_head) %>%
  dplyr::distinct()

label_text$norm_head <- max(label_text$norm_head, na.rm = TRUE)


ot_hdif_norm_slope_bplot_des <- ggplot(data = design_data, aes(y = norm_head, x = Distribution_slope, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Sloped Distribution Pipe") + ylab("Normalized Head Dif. at Peak Water Level in Green Inlet") +
  ggtitle("Overtopping by Head Differential and Distribution Pipe Slope") +
  scale_fill_manual(values = c("grey41","grey82")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE, label.size = 1.2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_hdif_norm_slope_bplot_des

## By system

#by system labels
label_text <- design_data %>%
  dplyr::group_by(system_id) %>%
  summarize(sys_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = c("system_id")) %>%
  dplyr::group_by(overtop, system_id) %>%
  summarize(label = paste0(round(100*n()/sys_count,0),"%"),
            norm_head = max(norm_head)) %>%
  dplyr::select(overtop,system_id,label, norm_head) %>%
  dplyr::distinct()

label_text$norm_head <- max(label_text$norm_head, na.rm = TRUE)

ot_hdif_norm_system_bplot_des <- ggplot(data = design_data, aes(y = norm_head, x = system_id, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("System ID") + ylab("Normalized Head Dif. at Peak Water Level in Green Inlet") +
  ggtitle("Overtopping by Head Differential per System") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE, label.size = 1.2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

ot_hdif_norm_system_bplot_des


## By Season

#by season,system labels
label_text <- design_data %>%
  dplyr::group_by(system_id) %>%
  summarize(sys_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = c("system_id")) %>%
  dplyr::group_by(season, system_id) %>%
  summarize(label = paste0(round(100*n()/sys_count,0),"%"),
            norm_head = max(norm_head)) %>%
  dplyr::select(season,system_id,label, norm_head) %>%
  dplyr::distinct()

label_text$norm_head <- max(label_text$norm_head, na.rm = TRUE)

hdif_norm_szn_sys_bplot_des <- ggplot(data = design_data, aes(y = norm_head, x = system_id, fill = season)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("System ID") + ylab("Normalized Head Dif. at Peak Water Level in Green Inlet") +
  ggtitle("Head Differential by System and Season") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Darjeeling2) +
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_norm_szn_sys_bplot_des

#by season labels
label_text <- design_data %>%
  dplyr::group_by(season) %>%
  summarize(season_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = "season") %>%
  dplyr::group_by(season, overtop) %>%
  summarize(label = paste0(round(100*n()/season_count,0),"%"),
            norm_head = max(norm_head)) %>%
  dplyr::select(season,overtop,label, norm_head) %>%
  dplyr::distinct()

label_text$norm_head <- max(label_text$norm_head, na.rm = TRUE)

hdif_norm_szn_ot_bplot_des <- ggplot(data = design_data, aes(y = norm_head, x = overtop, fill = season)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Overtopping") + ylab("Normalized Head Dif. at Peak Water Level in Green Inlet") +
  ggtitle("Head Differential by Season") +
  scale_color_manual(values = c("darkgray","black")) + 
  # scale_fill_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE")) + 
  scale_fill_manual(values = wes_palettes$Darjeeling2) +
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_norm_szn_ot_bplot_des

## By inlet type
design_data$inlet_style <- NA
design_data[design_data$Trap == FALSE,]$inlet_style <- "New (Without Trap)"
design_data[design_data$Trap == TRUE,]$inlet_style <- "Old (with Trap)"
# design_data$inlet_style <- as.factor(design_data$inlet_style)
# levels(design_data$inlet_style) <- c("Old (with Trap)", "New (Without Trap)", ordered = TRUE)

inlet_style_x <- factor(design_data$inlet_style, ordered = TRUE, levels = c("Old (with Trap)", "New (Without Trap)"))
design_data$inlet_style <- inlet_style_x

label_text <- design_data %>%
  dplyr::group_by(inlet_style) %>%
  summarize(style_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = "inlet_style") %>%
  dplyr::group_by(inlet_style, overtop) %>%
  summarize(label = paste0(round(100*n()/style_count,0),"%"),
            norm_head = max(norm_head)) %>%
  dplyr::select(inlet_style,overtop,label, norm_head) %>%
  dplyr::distinct()

label_text$norm_head <- max(label_text$norm_head, na.rm = TRUE)



hdif_norm_inlet_ot_bplot_des <- ggplot(data = design_data, aes(y = norm_head, x = inlet_style, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7, size = 1.5) +
  xlab("Inlet Type") + ylab("Normalized Head Dif. at Peak Water Level in Green Inlet") +
  ggtitle("Head Differential by Inlet Type") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_norm_inlet_ot_bplot_des



## Inlet type and Sump Depth?
#By inlet type
design_data$inlet_style <- NA
design_data[design_data$Trap == FALSE,]$inlet_style <- "New (Without Trap)"
design_data[design_data$Trap == TRUE,]$inlet_style <- "Old (with Trap)"

label_text <- design_data %>%
  dplyr::group_by(inlet_style) %>%
  summarize(style_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = "inlet_style") %>%
  dplyr::group_by(inlet_style, overtop) %>%
  summarize(label = paste0(round(100*n()/style_count,0),"%"),
            norm_head = max(norm_head)) %>%
  dplyr::select(inlet_style,overtop,label, norm_head) %>%
  dplyr::distinct()

label_text$norm_head <- max(label_text$norm_head, na.rm = TRUE)

hdif_norm_inlet_ot_sz_bplot_des <- ggplot(data = design_data, aes(y = norm_head, x = inlet_style, fill = overtop)) +
  geom_boxplot(size = 1.1, outlier.shape = NA) +
  geom_point(aes(size = Sump.Depth..ft.),position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7) +
  xlab("Inlet Type") + ylab("Normalized Head Dif. at Peak Water Level in Green Inlet") +
  ggtitle("Head Differential by Inlet Type") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_norm_inlet_ot_sz_bplot_des

## Sump depth and overtopping



design_data$Sump.Depth..ft. <- round(design_data$Sump.Depth..ft.,2)

hdif_norm_sump_ot_des <- ggplot(data = design_data, aes(y = norm_head, x = as.factor(Sump.Depth..ft.), fill = overtop)) + 
  geom_boxplot(size = 1.1, outlier.shape = NA)+
  geom_point(aes(size = eventdepth_in), shape = 21, alpha = 0.7) +
  xlab("Sump Depth (ft)") + ylab("Normalized Head Dif. at Peak Water Level in Green Inlet") +
  ggtitle("Head Differential by Sump Depth") +  
  labs(fill = "Overtopping", size = "Event Depth (in)") +
  scale_fill_manual(values = wes_palettes$Moonrise2) +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_norm_sump_ot_des


## Filter Bag Type
# pretreat <- inlet_type %>% dplyr::select(ow_uid,pretreatment)

# design_data <- design_data %>% left_join(pretreat, by = "ow_uid")

label_text <- design_data %>%
  dplyr::group_by(Filter.Bags.Used) %>%
  summarize(style_count = n()) %>%
  ungroup() %>%
  right_join(design_data, by = "Filter.Bags.Used") %>%
  dplyr::group_by(Filter.Bags.Used, overtop) %>%
  summarize(label = paste0(round(100*n()/style_count,0),"%"),
            norm_head = max(norm_head)) %>%
  dplyr::select(Filter.Bags.Used,overtop,label, norm_head) %>%
  dplyr::distinct()

label_text$norm_head <- max(label_text$norm_head, na.rm = TRUE)

hdif_norm_fbag_ot_bplot_des <- ggplot(data = design_data, aes(y = norm_head, x = Filter.Bags.Used, fill = overtop)) +
  geom_boxplot(size = 1.1,outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), shape = 21, alpha = 0.7) +
  xlab("Filter Bag Type") + ylab("Normalized Head Dif. at Peak Water Level in Green Inlet") +
  ggtitle("Head Differential by Filter Bag Type") +
  scale_color_manual(values = c("darkgray","black")) + 
  scale_fill_manual(values = wes_palettes$Moonrise2) + 
  
  geom_label(data = label_text, label = label_text$label, position = position_dodge(width = .8),
             hjust = "center", color = "white", fontface = "bold", show.legend = FALSE) +
  
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_norm_fbag_ot_bplot_des

## System Age plots

hdif_norm_vs_age_des <- ggplot(data = design_data) + geom_point(aes(x = Sys_Age/365, y = norm_head, col = Total...Tree.Pits)) +
  xlab("System Age (years)") + ylab("Normalized Head Diff. at Peak Water Level in Green Inlet") +
  ggtitle("Head Differential by System Age") +  
  labs(color = "Number of Trees") +
  
  #from pwdgsi plots; house style
  ggplot2::theme(
    #text = element_text(size = rel(2)), #size previously set to 16
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
    title = ggplot2::element_text(size = ggplot2::rel(1.4), color = "black"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

hdif_norm_vs_age_des


#### 2.5 Save boxplots ####
# bplot_folder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/boxplots"
bplot_folder <- paste0(folderpath,"/boxplots/",Sys.Date())
bplot_design_folder <- paste0(bplot_folder,"/Design Storms")

if(!dir.exists(bplot_folder)){dir.create(bplot_folder)}
if(!dir.exists(bplot_design_folder)){dir.create(bplot_design_folder)}

if(plot_save == TRUE){

# overtopping, all
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intensity_by_SMP.png"), plot = ot_peak_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_avg_intensity_by_SMP.png"), plot = ot_avg_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intensity_by_inlet_type.png"), plot = ot_peak_inlet_plot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intensity_by_season.png"), plot = ot_peak_season_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_Q_by_SMP.png"), plot = ot_peak_q_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_avg_intensity_by_Trap.png"), plot = ot_trap_avg_bplot, width = 10, height = 8)

# overtopping, all
ggsave(filename = paste0(bplot_design_folder,"/Overtopping_vs_peak_intensity_by_SMP_design_storms.png"), plot = ot_peak_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Overtopping_vs_avg_intensity_by_SMP_design_storms.png"), plot = ot_avg_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Overtopping_vs_peak_intensity_by_inlet_type_design_storms.png"), plot = ot_peak_inlet_plot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Overtopping_vs_peak_intensity_by_season_design_storms.png"), plot = ot_peak_season_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Overtopping_vs_peak_Q_by_SMP_design_storms.png"), plot = ot_peak_q_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Overtopping_vs_avg_intensity_by_Trap_design_storms.png"), plot = ot_trap_avg_bplot_des, width = 10, height = 8)


# head differential, all
ggsave(filename = paste0(bplot_folder,"/Head_dif_vs_sump_depth.png"), plot = hdif_sump_ot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Head_dif_vs_filter_bag.png"), plot = hdif_fbag_ot_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Head_dif_vs_slope.png"), plot = ot_hdif_slope_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Head_dif_vs_system.png"), plot = ot_hdif_system_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Head_dif_vs_season_and_sys.png"), plot = hdif_szn_sys_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Head_dif_vs_season.png"), plot = hdif_szn_ot_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Head_dif_vs_Inlet_type.png"), plot = hdif_inlet_ot_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Head_dif_vs_Age.png"), plot = hdif_vs_age, width = 10, height = 8)

# head differential, design storm only
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_vs_sump_depth_design_storms.png"), plot = hdif_sump_ot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_vs_filter_bag_design_storms.png"), plot = hdif_fbag_ot_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_vs_slope_design_storms.png"), plot = ot_hdif_slope_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_vs_system_design_storms.png"), plot = ot_hdif_system_bplot_des, width = 16, height = 9)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_vs_season_and_sys_design_storms.png"), plot = hdif_szn_sys_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_vs_season_design_storms.png"), plot = hdif_szn_ot_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_vs_Inlet_type_design_storms.png"), plot = hdif_inlet_ot_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_vs_Age_design_storms.png"), plot = hdif_vs_age_des, width = 10, height = 8)


# normalized head differential, design storm only
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_norm_vs_sump_depth_design_storms.png"), plot = hdif_norm_sump_ot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_norm_vs_filter_bag_design_storms.png"), plot = hdif_norm_fbag_ot_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_norm_vs_slope_design_storms.png"), plot = ot_hdif_norm_slope_bplot_des, width = 3200, height = 1800, units = "px")
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_norm_vs_system_design_storms.png"), plot = ot_hdif_norm_system_bplot_des, width = 3200, height = 1800, units = "px")
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_norm_vs_season_and_sys_design_storms.png"), plot = hdif_norm_szn_sys_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_norm_vs_season_design_storms.png"), plot = hdif_norm_szn_ot_bplot_des, width = 10, height = 8)
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_norm_vs_Inlet_type_design_storms.png"), plot = hdif_norm_inlet_ot_bplot_des, width = 3200, height = 1800, units = "px")
ggsave(filename = paste0(bplot_design_folder,"/Head_dif_norm_vs_Age_design_storms.png"), plot = hdif_norm_vs_age_des, width = 10, height = 8)

}

#### 2.6 Drainage Area ####

DA_plot <- ggplot(data = filtered_data, aes(x = drainage_area_sf, y = eventpeakintensity_inhr, col = overtop, size = overtop)) +
           geom_point() + 
           ylab("Event Peak Intensity (in/hr)") + xlab("Inlet Drainage Area (sf)") +
           scale_x_continuous(limits = c(0, 24000)) +
           ggtitle("Event Peak Intesity vs Inlet Drainage Area") +
           # scale_color_manual(values = c("darkgray","black")) + 
           # scale_fill_manual(values = wes_palettes$Moonrise2) + 
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
colnames(qmax) <- c("smp_id","qmax")
qmax$qmax %<>% as.numeric()

qmax <- left_join(filtered_data, qmax, by = "smp_id") %>%
        dplyr::select(radar_event_uid,smp_id,qpeak,qmax,overtop) %>% distinct()

qpeak_perc <- 100*(sum(qmax$qpeak > qmax$qmax)/
                          length(qmax$qpeak))

#assume 50% occlusion


# same percent, overtopping only

qmax_oto <- qmax %>% dplyr::filter(overtop == TRUE)
qpeak_perc_oto <- 100*(sum(qmax_oto$qpeak > qmax_oto$qmax)/
                         length(qmax_oto$qpeak))

#### 2.8 Summary Stats on Head Differential ####

filtered_data_sum <- filtered_data %>%
                     group_by(smp_id, overtop) %>%
                     summarise(Count = n(),
                               mean_hd = mean(rel_head_dif, na.rm = TRUE),
                               var_hd = sd(rel_head_dif, na.rm = TRUE))



#### 2.9 Summary Stats with Filtered Data ####

gi_filter_summary <- filtered_data %>% group_by(ow_uid) %>% summarize(n = n(),
                                                                       overtopping_count = sum(overtop),
                                                                       avg_RPSU = mean(percentstorageused_relative, na.rm = TRUE)) %>%
  dplyr::left_join(filtered_data, by = "ow_uid") %>%
  dplyr::select(ow_uid,smp_id,ow_suffix,n,overtopping_count, avg_RPSU) %>%
  unique() %>% dplyr::filter(!is.na(smp_id))

gi_filter_summary <- gi_filter_summary %>% dplyr::mutate(overtop_perc = overtopping_count/n)


write.csv(gi_filter_summary, file = paste0(folderpath, "/", Sys.Date(), "/gi_filter_summary.csv"))

#### 3.0 STAT Model Creation - overtopping ####


# 3.1 Ind pendent variable transformations and scaling

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
filtered_data_by_site$mean_head_dif


cor_plot <- filtered_data_by_site %>% dplyr::select(Overtop_pct,
                                                    mean_head_dif,
                                                    System.Loading.Ratio,
                                                    System.Infiltration.Footprint..sf.,
                                                    Storm.Size.Managed..in.,
                                                    System.Drainage.Area..SF.,
                                                    Inlet.Drainage.Area..SF.,
                                                    Max.Flow.w..Perforations..CFS.,
                                                    Distrib..Length..FT.,
                                                    X..Distrib..Bends,
                                                    Distrib..Size.,
                                                    Distrib..Slope....)


colnames(cor_plot) <- c("Overtop_pct",
                        "head_dif",
                        "LR",
                        "Infil_fprint_sf",
                        "Storm_size_in",
                        "Sys_DA_sf",
                        "Inlet_DA_sf",
                        "Qmax_cfs",
                        "Dist_pipe_length",
                        "Number_of_dist_pipe_bends",
                        "Dist_pipe_size",
                        "Dist_pipe_slope")
# make numeric
cols <- c(1:ncol(cor_plot))
cor_plot[,cols] = apply(cor_plot[,cols], 2, function(x) as.numeric(as.character(x)))

chart.Correlation(cor_plot, histogram = TRUE)
cor_table <- cor(cor_plot, method = "pearson")
write.table(cor_table, file = "clipboard")
# png(filename = paste0(bplot_folder,"/correlation_plot.png"))


# Make a smaller 


# Storm event correlation matrix
cor_plot_storm <- filtered_data %>% dplyr::select(
                                                  rel_head_dif,
                                                  log_event_depth,
                                                  log_peak_int,
                                                  log_avg_int,
                                                  log_qpeak,
                                                  sqrt_last_jet)


colnames(cor_plot_storm) <- c("Head Diff. (ft)",
                              "Event Depth\n(in)",
                              "Peak Intensity (in/hr)",
                              "Avg. Intensity\n(in/hr)",
                              "Peak Flow Rate\n(cfs)",
                              "Days Since\nPipe Jetting")
# Remove infinite values
cor_plot_storm <- cor_plot_storm[!is.infinite(cor_plot_storm$`Peak Flow Rate\n(cfs)`),]

chart.Correlation(cor_plot_storm,
                  cex.labels = 3,
                  histogram = TRUE) 


jpeg(filename = paste0(bplot_folder,"/correlation_plot2.jpg"),
    width = 1800, height = 1440, type = "windows")



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
                            scale_color_manual(values = c("darkgray","black")) + 
                            scale_fill_manual(values = wes_palettes$Moonrise2) + 
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

# model j1



# model j2
modelj1 <- lmer(overtop ~ Trap + (1|ow_uid), data = filtered_data)

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


#model12 (selected model)

model12 <- glmer(overtop ~  log_qpeak + sqrt_last_jet + scl_inl_DA + Trap + (1|ow_uid), data = filtered_data, family = binomial())

model12_residuals <- resid(model12)
anova(model12)
hist(model12_residuals)
shapiro.test(model12_residuals)
qqnorm(model12_residuals); abline(a = 0, b = 1)
summary(model12)


# overtopping percentage vs. max distribution pipe flow way

plot(filtered_data_by_site$Dist.Pipe.Head..ft.,filtered_data_by_site$Overtop_pct)
