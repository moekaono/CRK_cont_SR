# run the wavelet Rmd first for function

dir_pwr = "G:/My Drive/Research/Projects/DC_auto/Data/Nov2024_rerun/result_pad_64d_raw/df_temporal_pwr/"
dir_diurnal = "G:/My Drive/Research/Projects/DC_auto/Data/Nov2024_rerun/result_pad_64d_raw/df_diurnal/"


### May 22 ###
wc_PAR_Ts_May22 <- coh(SR_Rh_May22, "PPFD_IN_1_1_1", "TS_1_1_1", 64)

# pwr
PAR_Ts_May22_pwr <- wc_avg(wc_PAR_Ts_May22, 25)
PAR_Ts_May22_pwr$plot

write.csv(PAR_Ts_May22_pwr$df, paste0(dir_pwr, "PAR_Ts_May22.csv"), row.names = F)

#heatmap
dev.new(width = 6, height = 5, noRStudioGD = TRUE)
wc.image(
  wc_PAR_Ts_May22,
  legend.params = 
    list(
      lab = "cross-wavelet power levels", # axis label
      lab.line = 1.5, # distance between bar label and axis label
      mar = 5
    ), 
  lvl = 0.3,
  timelab = "",
  show.date = T, 
  date.format = "%Y-%m-%d %H:%M:%S",
  periodlab = "period (days)",
  main = "PAR vs Ts"
)

# extracting diurnal angle & power patterns from heatmap
wc_PAR_Ts_May22_diurnal <- coh_result(scale_pad(SR_Rh_May22, "PPFD_IN_1_1_1", "TS_1_1_1"), wc_PAR_Ts_May22)
# save as csv
write.csv(wc_PAR_Ts_May22_diurnal, paste0(dir_diurnal,"wc_PAR_May22_Ts.csv"), row.names = F)


### Mar 23 ###
wc_PAR_Ts_Mar23 <- coh(SR_Rh_Mar23, "PPFD_IN_1_1_1", "TS_1_1_1", 64)

#pwr
PAR_Ts_Mar23_pwr <- wc_avg(wc_PAR_Ts_Mar23, 25)
PAR_Ts_Mar23_pwr$plot

write.csv(PAR_Ts_Mar23_pwr$df, paste0(dir_pwr, "PAR_Ts_Mar23.csv"), row.names = F)

#heatmap
dev.new(width = 6, height = 5, noRStudioGD = TRUE)
wc.image(
  wc_PAR_Ts_Mar23,
  legend.params = 
    list(
      lab = "cross-wavelet power levels", # axis label
      lab.line = 1.5, # distance between bar label and axis label
      mar = 5
    ), 
  lvl = 0.3,
  timelab = "",
  show.date = T, 
  date.format = "%Y-%m-%d %H:%M:%S",
  periodlab = "period (days)",
  main = "PAR vs Ts"
)

# extracting diurnal angle & power patterns from heatmap
wc_PAR_Ts_Mar23_diurnal <- coh_result(scale_pad(SR_Rh_Mar23, "PPFD_IN_1_1_1", "TS_1_1_1"), wc_PAR_Ts_Mar23)
# save as csv
write.csv(wc_PAR_Ts_Mar23_diurnal, paste0(dir_diurnal,"wc_PAR_Mar23_Ts.csv"), row.names = F)




### Jan 24 ###
wc_PAR_Ts_Jan24 <- coh(SR_Rh_Jan24, "PPFD_IN_1_1_1", "TS_1_1_1", 64)

#pwr
PAR_Ts_Jan24_pwr <- wc_avg(wc_PAR_Ts_Jan24, 25)
PAR_Ts_Jan24_pwr$plot

write.csv(PAR_Ts_Jan24_pwr$df, paste0(dir_pwr, "PAR_Ts_Jan24.csv"), row.names = F)

#heatmap
dev.new(width = 6, height = 5, noRStudioGD = TRUE)
wc.image(
  wc_PAR_Ts_Jan24,
  legend.params = 
    list(
      lab = "cross-wavelet power levels", # axis label
      lab.line = 1.5, # distance between bar label and axis label
      mar = 5
    ), 
  lvl = 0.3,
  timelab = "",
  show.date = T, 
  date.format = "%Y-%m-%d %H:%M:%S",
  periodlab = "period (days)",
  main = "PAR vs Ts"
)

# extracting diurnal angle & power patterns from heatmap
wc_PAR_Ts_Jan24_diurnal <- coh_result(scale_pad(SR_Rh_Jan24, "PPFD_IN_1_1_1", "TS_1_1_1"), wc_PAR_Ts_Jan24)
# save as csv
write.csv(wc_PAR_Ts_Jan24_diurnal, paste0(dir_diurnal,"wc_PAR_Jan24_Ts.csv"), row.names = F)




### Feb 24 ###
wc_PAR_Ts_Feb24 <- coh(SR_Rh_Feb24, "PPFD_IN_1_1_1", "TS_1_1_1", 64)

# pwr
PAR_Ts_Feb24_pwr <- wc_avg(wc_PAR_Ts_Feb24, 25)
PAR_Ts_Feb24_pwr$plot

write.csv(PAR_Ts_Feb24_pwr$df, paste0(dir_pwr, "PAR_Ts_Feb24.csv"), row.names = F)

# heatmap
dev.new(width = 6, height = 5, noRStudioGD = TRUE)
wc.image(
  wc_PAR_Ts_Feb24,
  legend.params = 
    list(
      lab = "cross-wavelet power levels", # axis label
      lab.line = 1.5, # distance between bar label and axis label
      mar = 5
    ), 
  lvl = 0.3,
  timelab = "",
  show.date = T, 
  date.format = "%Y-%m-%d %H:%M:%S",
  periodlab = "period (days)",
  main = "PAR vs Ts"
)

# extracting diurnal angle & power patterns from heatmap
wc_PAR_Ts_Feb24_diurnal <- coh_result(scale_pad(SR_Rh_Feb24, "PPFD_IN_1_1_1", "TS_1_1_1"), wc_PAR_Ts_Feb24)
# save as csv
write.csv(wc_PAR_Ts_Feb24_diurnal, paste0(dir_diurnal,"wc_PAR_Feb24_Ts.csv"), row.names = F)


### Mar 24 ###
wc_PAR_Ts_Mar24 <- coh(SR_Rh_Mar24, "PPFD_IN_1_1_1", "TS_1_1_1", 64)

#pwr
PAR_Ts_Mar24_pwr <- wc_avg(wc_PAR_Ts_Mar24, 25)
PAR_Ts_Mar24_pwr$plot

write.csv(PAR_Ts_Mar24_pwr$df, paste0(dir_pwr, "PAR_Ts_Mar24.csv"), row.names = F)

#heatmap
dev.new(width = 6, height = 5, noRStudioGD = TRUE)
wc.image(
  wc_PAR_Ts_Mar24,
  legend.params = 
    list(
      lab = "cross-wavelet power levels", # axis label
      lab.line = 1.5, # distance between bar label and axis label
      mar = 5
    ), 
  lvl = 0.3,
  timelab = "",
  show.date = T, 
  date.format = "%Y-%m-%d %H:%M:%S",
  periodlab = "period (days)",
  main = "PAR vs Ts"
)

# extracting diurnal angle & power patterns from heatmap
wc_PAR_Ts_Mar24_diurnal <- coh_result(scale_pad(SR_Rh_Mar24, "PPFD_IN_1_1_1", "TS_1_1_1"), wc_PAR_Ts_Mar24)
# save as csv
write.csv(wc_PAR_Ts_Mar24_diurnal, paste0(dir_diurnal,"wc_PAR_Mar24_Ts.csv"), row.names = F)
