#### Green Inlet Monitoring
#### OT plotting
#### Written by : Brian Cruice
#### Written on: 01/10/2022

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
file_path <- "2023-01-11/overtopping_data.csv"
raw_data <- read.csv(paste0(folderpath,"/",file_path))

# Create plot path if needed
current_date <- today()
plotpath <- paste0(folderpath,"/Overtopping Graphs/",current_date)
if(!dir.exists(plotpath)){dir.create(plotpath)}

# Connect to database

mars_con <- odbc::dbConnect(odbc::odbc(), "mars14_data")

# SMP ID's
# smps <- unique(raw_data$smp_id)
# 
# test_smp <- dplyr::filter(data, smp_id == "171-1-1")
# 
# test_design_storm <- dbGetQuery(mars_con, "SELECT sys_creditedstormsizemanaged_in FROM external.mat_systembdvcache WHERE system_id = '171-1'")
# 
# 
# test_smp_data <- test_smp %>% dplyr::select(ow_uid, radar_event_uid, ow_suffix, eventdatastart_edt, smp_id, eventavgintensity_inhr, eventpeakintensity_inhr, overtop)

# list of unique monitoring locations
mon_locs <- raw_data %>% dplyr::select(smp_id, ow_suffix) %>% distinct()


#### 2.0 Plot function ####
overtopping_intensity_plot <- function(data, design_storm, event_dates = NA, event_descriptions  NA){
  
  #clean dates for x-axis
  data$eventdatastart_edt <- data$eventdatastart_edt %>% lubridate::mdy_hm()
  min_date <- min(lubridate::date(data$eventdatastart_edt))
  max_date <- max(lubridate::date(data$eventdatastart_edt))
  
  
  #Set overtop to sizes
  data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
  data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
  data$overtop_col[data$overtop == FALSE] <- "steelblue3"
  data$overtop_col[data$overtop == TRUE] <- "firebrick3"
  
  #subset of data exceeding design stor
  data_ovr_design <- data %>% dplyr::filter(eventdepth_in > design_storm)
  data <- data %>% mutate("ExceedDesignStorm" = ifelse(eventdepth_in > design_storm,"True",NA))
  # y-max value
  ymax_obs <- max(data$eventpeakintensity_inhr, na.rm = TRUE)
  
  
  plot_x <- ggplot(data,
                   aes(x = eventdatastart_edt,
                       y = eventpeakintensity_inhr)) +
    geom_point(aes(color = factor(overtop),
                   size = factor(overtop))) +
    geom_hline(yintercept = 2.5, color = "red", size = 1.5) +
    scale_y_continuous(limits = c(0,max(3,ymax_obs)), minor_breaks =seq(0,max(3,ymax_obs),0.1)) +
    scale_x_datetime(date_minor_breaks = "1 month") +
    ylab("Event Peak Intensity (in/hr)") + xlab("Event Start Date") +
    geom_text(label = "Philadelphia 1-year, 15-minute Peak Intensity: 2.5 in/hr",
              y = 2.6, color = "black", size = 12 / .pt, hjust = "left",
              x = data$eventdatastart_edt[round(0.05*length(data$eventdatastart_edt))]) +
    ggtitle(paste0("Event Peak Intesity and Overtopping vs Time for ",data$ow_suffix[1],", ",data$smp_id[1])) +
    
    #add design storm values
    geom_point(aes(x = eventdatastart_edt,
                   y = eventpeakintensity_inhr, size = factor(overtop),  color = factor(overtop), shape = factor(ExceedDesignStorm))) +
    scale_shape_manual(name = "Exceeds Design Storm", values = c(2), labels = c("True"), na.translate = FALSE) +
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
      #legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))
  
  if(!is.na(event_dates) & !is.na(event_descriptions)){
    plot_x + geom_vline(xintercept = event_dates)
  }
  
  
  
  return(plot_x)
  
}
  

#### 3.0 iterate through locations, savings plots
  
for(i in 1:nrow(mon_locs)){

# Subset plot data
plot_data <- raw_data %>%
             dplyr::filter(smp_id == mon_locs$smp_id[i]) %>%
             dplyr::filter(ow_suffix == mon_locs$ow_suffix[i]) %>%
             dplyr::select(ow_uid, radar_event_uid, ow_suffix, eventdatastart_edt,
                           smp_id, eventavgintensity_inhr, eventpeakintensity_inhr, eventdepth_in, overtop)

# Grab design storm Depth  
design_storm <- dbGetQuery(mars_con,
                          paste0("SELECT sys_creditedstormsizemanaged_in FROM external.tbl_smpbdv
                                  LEFT JOIN external.tbl_systembdv
                                  ON tbl_smpbdv.system_id = tbl_systembdv.system_id
                                  WHERE smp_id = '",mon_locs$smp_id[i],"'")) %>% pull

# create plot
location_plot <- overtopping_intensity_plot(plot_data, design_storm)    

# Save Plot
ggsave(plot = location_plot,
       filename = paste0(plotpath,"/",mon_locs$smp_id[i],"_",mon_locs$ow_suffix[i],".png"),
       width = 10, height = 8)

}



########## test zone ####

data <- test_smp_data

#clean dates for x-axis
data$eventdatastart_edt <- data$eventdatastart_edt %>% as.POSIXct()
min_date <- min(lubridate::date(data$eventdatastart_edt))
max_date <- max(lubridate::date(data$eventdatastart_edt))

date_breaks <- seq(min_date, max_date, by = "days") %>% as.POSIXct()   
mon_breaks <- seq(paste(year(min_date),month(min_date),"1",sep ="-"),
                  paste(year(max_date),month(max_date),"1",sep ="-"), by = "months") %>%
              as.POSIXct()



#Set overtop to sizes
data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
data$overtop_col[data$overtop == FALSE] <- "steelblue3"
data$overtop_col[data$overtop == TRUE] <- "firebrick3"

# y-max value
ymax_obs <- max(data$eventpeakintensity_inhr, na.rm = TRUE)


plot_x <- ggplot(data,
               aes(x = eventdatastart_edt,
                   y = eventpeakintensity_inhr)) +
  geom_point(aes(color = factor(overtop),
             size = factor(overtop))) +
  scale_color_manual(name = "Overtopping", values = c("steelblue3","firebrick3"), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
  scale_size_manual(name = "Overtopping", values = c(2,4), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 2.5, color = "red", size = 1.5) +
  scale_y_continuous(limits = c(0,max(3,ymax_obs)), minor_breaks =seq(0,max(3,ymax_obs),0.1)) +
  scale_x_datetime(date_minor_breaks = "1 month") +
  ylab("Event Peak Intensity (in/hr)") + xlab("Event Start Date") +
  geom_text(label = "Philadelphia 1-year, 15-minute Peak Intensity: 2.5 in/hr",
             y = 2.6, color = "black", size = 12 / .pt, hjust = "left",
             x = data$eventdatastart_edt[round(0.05*length(data$eventdatastart_edt))]) +
  ggtitle(paste0("Event Peak Intesity and Overtopping vs Time for ",data$ow_suffix[1],", ",data$smp_id[1])) +
  
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
    #legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))

plot_x


ggsave("C:/users/brian.cruice/desktop/overtop_test.png",x_pk, width = 7.5, height = 6)

### Junk yard
# x_avg <- ggplot(data,
#                 aes(x = eventdatastart_edt,
#                     y = eventavgintensity_inhr,
#                     col = factor(overtop_col),
#                     size = overtop_sz)) +
#   geom_point() +
#   geom_hline(yintercept = 2.5, color = "red", size = 1.5) +
#   scale_x_datetime()
