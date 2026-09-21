#Calculating theoretical maximum outflow at different system characteristics

library(readxl)
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
         

#Turns out that the equation can be aggregated and still be valid, so I will calculate it at the per foot level