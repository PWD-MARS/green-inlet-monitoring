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
raw_data <- read.csv(paste0(folderpath,"/",file_path))

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

#### 2.0 Data Visualization ####

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


# Review trap vs. no trap
trap_status <- inlet_type %>% dplyr::select(ow_uid, Trap, drainage_area_sf)

filtered_data <- filtered_data %>% left_join(trap_status, by = "ow_uid")
filtered_data <- filtered_data %>% left_join(sys_char, by = 'system_id')

# Save Plots
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


# Save boxplots
bplot_folder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/boxplots"

ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intensity_by_SMP.png"), plot = ot_peak_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_avg_intensity_by_SMP.png"), plot = ot_avg_bplot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intesity_by_inlet_type.png"), plot = ot_peak_inlet_plot, width = 10, height = 8)
ggsave(filename = paste0(bplot_folder,"/Overtopping_vs_peak_intesity_by_season.png"), plot = ot_peak_season_bplot, width = 10, height = 8)



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





#### 2.6 PCP/z-scores ####



#### 3.0 Model Creation ####

# Look at some distributions
hist(filtered_data$eventpeakintensity_inhr)
# take ln to normalize 
hist(log(filtered_data$eventpeakintensity_inhr))


hist(filtered_data$eventavgintensity_inhr)
# take ln to normalize 
hist(log(filtered_data$eventavgintensity_inhr))

# use ln to get better residuals
filtered_data$log_peak_int <- log(filtered_data$eventpeakintensity_inhr)
filtered_data$log_avg_int <- log(filtered_data$eventavgintensity_inhr)


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
qqnorm(model7$residuals)

# model 8
model8 <- lm(overtop ~ log_peak_int, data = filtered_data)

anova(model8)
hist(model8$residuals)
shapiro.test(model8$residuals)
qqnorm(model8$residuals)

# model 9
model9 <- lm(overtop ~ log_peak_int + Trap + last_jet,data = filtered_data)

anova(model9) # ANOVA results show days since jetting being insignificant
hist(model9$residuals)
shapiro.test(model9$residuals)
qqnorm(model9$residuals); qqline(model9$residuals)

# model 10
model10 <- lmer( overtop ~ log_peak_int + Trap + last_jet + (1|ow_uid), data = filtered_data)

model10_residuals <- resid(model10)
anova(model10)
hist(model10_residuals)
shapiro.test(model10_residuals)
qqnorm(model10_residuals); qqline(model10_residuals)


#### summarize ####

filtered_data_by_site <- filtered_data %>% dplyr::group_by(ow_uid) %>%
                         summarize(Size = n(),
                                   Overtop_pct = sum(overtop)/n()) %>%
                         distinct() %>%
                         dplyr::left_join(select(filtered_data, ow_uid, system_id), by = "ow_uid") %>%  
                         left_join(sys_char, by = "system_id") %>% 
                         left_join()
                         dplyr::filter(GI == "GI1") %>% distinct()


# overtopping percentage vs. max distribution pipe flow way

plot(filtered_data_by_site$Dist.Pipe.Head..ft.,filtered_data_by_site$Overtop_pct)

model11 <- lmer(Overtop_pct ~ Trap + Dist.Pipe.Head..ft. + System.Loading.Ratio, data = filtered_data_by_site)
