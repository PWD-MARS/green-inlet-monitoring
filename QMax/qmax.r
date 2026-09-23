#Calculating theoretical maximum outflow at different system characteristics

library(readxl)
library(openxlsx)
library(tidyverse)
#Read spreadsheet values
pipetool <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/References/Pipe Perforations/ADS Distribution Pipe Sizing Tool.xls"

#AASHTO type 2 perforated corrugated pipe stats
aashtotable <- read_xls(path = pipetool,
                        sheet = "Compar Data",
                        range = "B7:H22",
                        col_names = TRUE) |> #B7:H7 are column names
  select(perforation_style = 1, #Slotted or drilled
         diam_in = 2, #Internal diameter, in inches
         perfarea_in2ft = 3, #Total perforated area per linear foot of pipe, in square inches per foot
         perfarea_ft2ft = 4, #Column 3, in square feet per foot
         corrugations_per_ft = 6, #corrugations per linear foot (perforations are in the valleys)
         perforations_per_corrugation = 7, #perforations per corrugation valley
         perfarea_ft2perf = 5) #Column 3, in square feet per perforation


#We need to implement the calculations from the sheet "Flow Rates at Incremental Head"
# to test the effects of aggregating at different areas. 
# I'll use the longest distribution pipe (270 ft at 179-5-1) to trial this

morrisleeds <- data.frame(length_ft = 270,
                          diam_ft = 8/12) |>
  bind_cols(filter(aashtotable, diam_in == 8)) |>
  mutate(perforations_per_ft = perforations_per_corrugation * corrugations_per_ft)

#Flow rates at incremental head
flowrates_morrisleeds <- data.frame(head_ft = seq(0, 6, by = 0.5)) |>
  mutate(flowrate_perperf = morrisleeds$perfarea_ft2perf * 0.62 * sqrt(2 * 32.2 * (head_ft + morrisleeds$diam_ft/2)),
         flowrate_perfoot = morrisleeds$perfarea_ft2ft * 0.62 * sqrt(2 * 32.2 * (head_ft + morrisleeds$diam_ft/2)),
         flowrate_perpipe = morrisleeds$perfarea_ft2ft * morrisleeds$length_ft * 0.62 * sqrt(2 * 32.2 * (head_ft + morrisleeds$diam_ft/2))) |>
  group_by(head_ft) |>
  summarize(summed_perperf = flowrate_perperf * morrisleeds$perforations_per_ft * morrisleeds$length_ft,
            summed_perfoot = flowrate_perfoot * morrisleeds$length_ft,
            summed_perpipe = flowrate_perpipe) |>
  mutate(identical = all.equal(summed_perperf, summed_perfoot) & #Test for floating point equality
                     all.equal(summed_perfoot, summed_perpipe) &
                     all.equal(summed_perperf, summed_perpipe))
         

#Turns out that the equation can be aggregated and still be valid, 
  #so I will calculate it at the per foot level to allow for slope considerations

#Qmax for Monitored Sites without slope considerations
#Required stats include
  # Inlet grate elevation
  # Distribution pipe invert elevation
  # Slope if applicable
  # Length
  # Diameter
#Slope, length, and diameter are all in the system characteristics sheet
syscharsheet <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/SystemCharacteristics.xlsx"

pipestats<- read.xlsx(xlsxFile = syscharsheet,
                                  sheet = "Characteristics") |>
  transmute(smp_id, ow_suffix = GI, length_ft = `Distrib..Length.(FT)`,
         diam_in = `Distrib..Size`, slope_pct = `Distrib..Slope.(%)`/100) 

inletstats <- read_csv("./gi_survey_elev.csv")

qmax_noslope <- left_join(pipestats, inletstats, by = c("smp_id", "ow_suffix")) |>
  left_join(aashtotable, by = "diam_in") |>
  mutate(qmax_perfoot_2026_cfs = perfarea_ft2ft * 0.62 * sqrt(2 * 32.2 * (grate_elev - inv_elev - diam_in/24)),
         qmax_perfoot_vusp_cfs = perfarea_ft2ft * 0.56 * sqrt(2 * 32.2 * (vusp_grate_elev - inv_elev - diam_in/24))) |>
  mutate(qmax_2026_cfs = qmax_perfoot_2026_cfs * length_ft,
         qmax_vusp_cfs = qmax_perfoot_vusp_cfs * length_ft)

write_csv(qmax_noslope, file = "./QMax/qmax_noslope.csv")

#Qmax estimate with slope consideration
qmax_estimate_cfs <- function(perfarea_ft2ft,
                          discharge_coef,
                          grate_elev,
                          inv_elev,
                          diam_in,
                          length_ft,
                          slope_pct = 0) {
  if(length_ft < 1){
    stop("Minimum pipe length of 1 foot")
  }
  
  qmax_estimate_cfs <- 0
  
  #If not sloped, calculate all at once. If sloped, iterate by foot
  if(slope_pct == 0){
    #Not sloped, calculate all at once
    qmax_estimate_cfs <- perfarea_ft2ft * discharge_coef * sqrt(2 * 32.2 * (grate_elev - inv_elev - diam_in/24)) * length_ft
    return(qmax_estimate_cfs)
  } else {
    for(i in 1:round(length_ft, 0)){ #If non-integer, we will add the partial step at the end
      #Calculate the outflow at qmax of the i'th foot of the distribution pipe
      
      #Effective invert of this pipe segment
      segment_inv_elev <- inv_elev + i * slope_pct #feet of change per foot of pipe
      segment_qmax_cfs <- perfarea_ft2ft * discharge_coef * sqrt(2 * 32.2 * (grate_elev - segment_inv_elev - diam_in/24))
      
      qmax_estimate_cfs <- qmax_estimate_cfs + segment_qmax_cfs
    }
    
    #Loop is done; add remainder segment if present
    if(length_ft %% 1 == 0){ #If pipe is integer feet in length
      return(qmax_estimate_cfs)
    } else {
      segment_length_ft <- length_ft %% 1 #Length of leftover pipe segment
      segment_inv_elev <- inv_elev + round(length_ft, 0) * slope_pct #effective invert at final pipe segment
      segment_qmax_cfs <- perfarea_ft2ft * discharge_coef * sqrt(2 * 32.2 * (grate_elev - segment_inv_elev - diam_in/24)) * segment_length_ft
      
      qmax_estimate_cfs <- qmax_estimate_cfs + segment_qmax_cfs
    }
    
    return(qmax_estimate_cfs)
  }
}

qmax_slope <- left_join(pipestats, inletstats, by = c("smp_id", "ow_suffix")) |>
  left_join(aashtotable, by = "diam_in") |> 
  rowwise() |>
  mutate(slopeqmax_2026_cfs = qmax_estimate_cfs(perfarea_ft2ft = perfarea_ft2ft,
                                           discharge_coef = 0.62,
                                           grate_elev = grate_elev,
                                           inv_elev = inv_elev,
                                           diam_in = diam_in,
                                           length_ft = length_ft,
                                           slope_pct = slope_pct),
         slopeqmax_vusp_cfs = qmax_estimate_cfs(perfarea_ft2ft = perfarea_ft2ft,
                                           discharge_coef = 0.56,
                                           grate_elev = vusp_grate_elev,
                                           inv_elev = inv_elev,
                                           diam_in = diam_in,
                                           length_ft = length_ft,
                                           slope_pct = slope_pct))
  
write_csv(qmax_slope, file = "./QMax/qmax_slope.csv")

#Validating that the unsloped pipes have the same discharge in the second run
val <- left_join(qmax_slope, qmax_noslope) |>
  mutate(slopecheck_2026 = ifelse(slope_pct != 0, 
                                  !isTRUE(all.equal(slopeqmax_2026_cfs, qmax_2026_cfs)),
                                  isTRUE(all.equal(slopeqmax_2026_cfs, qmax_2026_cfs))),
         slopecheck_vusp = ifelse(slope_pct != 0, 
                                  !isTRUE(all.equal(slopeqmax_vusp_cfs, qmax_vusp_cfs)),
                                  isTRUE(all.equal(slopeqmax_vusp_cfs, qmax_vusp_cfs))))

