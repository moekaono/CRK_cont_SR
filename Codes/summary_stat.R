# to extract summary statistic
# and plot out the results
                                                                      

process_csv <- function(filename){
  
  df <- strsplit(filename, "/")[[1]][8]
  season <- strsplit(df, "_")[[1]][3]
  resp <- strsplit(df, "_")[[1]][2]
  driver <- sub(".csv","",strsplit(df, "_")[[1]][5])
  
  read.csv(filename) %>%
    summarise(
      start_date = date(min(date)),
      end_date = date(max(date)),
      pval_period1_mean = mean(pval_period1),
      pval_period1_sd = sd(pval_period1),
      Angle_1d_mean = mean(Angle_1d, na.rm = T),
      Angle_1d_sd = sd(Angle_1d, na.rm = T),
      Angle_7d_mean = mean(Angle_7d, na.rm = T),
      Angle_7d_sd = sd(Angle_7d, na.rm = T),
      Angle_15d_mean = mean(Angle_15d, na.rm = T),
      Angle_15d_sd = sd(Angle_15d, na.rm = T),
      Power_1d_mean = mean(Power_1d, na.rm = T),
      Power_1d_sd = sd(Power_1d, na.rm = T),
      Power_7d_mean = mean(Power_7d, na.rm = T),
      Power_7d_sd = sd(Power_7d, na.rm = T),
      Power_15d_mean = mean(Power_15d, na.rm = T),
      Power_15d_sd = sd(Power_15d, na.rm = T)
    ) %>% 
    mutate(
      df = df, 
      season = season,
      resp = resp,
      driver = driver
    )
}



filenames <- list.files(dir, "*.csv", full.names = T)
result_df <- map_dfr(filenames, process_csv)
result_df %>% View()

write.csv(result_df, "G:/My Drive/Research/Projects/DC_auto/Data/summary_stat.csv", row.names = F)

 
# plot out the results

result_graph_lag <- function(var1, var2){
  result_df$season <- factor(result_df$season, 
                             levels = c("Spr22", "Wnt22", "Spr23", "Spr23-2&3", "Wnt24", "Spr24"))
  result_df %>%
  filter(resp == var1 & driver == var2) %>%
  ggplot(aes(season, 24*Angle_1d_mean/(2*pi))) + 
  geom_point(size = 5) + 
  geom_errorbar(aes(season, ymin = 24*(Angle_1d_mean - Angle_1d_sd)/(2*pi),
                    ymax = 24*(Angle_1d_mean + Angle_1d_sd)/(2*pi)), lwd=1.5, width=.2) + 
  geom_abline(slope = 0, intercept = 0) + theme_bw() + 
  ylab("Lead/Lag Hour") + ggtitle(paste0(var1, " vs ", var2)) + 
  theme(axis.title.x = element_blank(),axis.text.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),axis.text.y = element_text(size = 15),
          axis.ticks.x=element_blank())
}

result_graph_pwr <- function(var1, var2){
  result_df$season <- factor(result_df$season, 
                             levels = c("Spr22", "Wnt22", "Spr23", "Spr23-2&3", "Wnt24", "Spr24"))
  result_df %>%
    filter(resp == var1 & driver == var2) %>%
    ggplot(aes(season, Power_1d_mean)) + 
    geom_point(size = 5) + 
    geom_errorbar(aes(season, ymin = Power_1d_mean - Power_1d_sd,
                      ymax = Power_1d_mean + Power_1d_sd), lwd=1.5, width=.2) + 
    ylim(0,15) + theme_bw() + 
    ylab("Power") + ggtitle(paste0(var1, " vs ", var2)) + 
    theme(axis.title.x = element_blank(),axis.text.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),axis.text.y = element_text(size = 15),
          axis.ticks.x=element_blank())
}


# for lag/lead hour
p_SR_PAR <- result_graph_lag("SR", "PAR")
p_Rh_PAR <- result_graph_lag("Rh", "PAR")
p_Ra_PAR <- result_graph_lag("Ra", "PAR")

grid.arrange(p_SR_PAR, p_Rh_PAR, p_Ra_PAR, ncol= 3)

p_SR_Ts <- result_graph_lag("SR", "Ts")
p_Rh_Ts <- result_graph_lag("Rh", "Ts")
p_Ra_Ts <- result_graph_lag("Ra", "Ts")

grid.arrange(p_SR_Ts, p_Rh_Ts, p_Ra_Ts, ncol= 3)

p_SR_SWC <- result_graph_lag("SR", "SWC")
p_Rh_SWC <- result_graph_lag("Rh", "SWC")
p_Ra_SWC <- result_graph_lag("Ra", "SWC")

grid.arrange(p_SR_SWC, p_Rh_SWC, p_Ra_SWC, ncol= 3)

p_SR_gs <- result_graph_lag("SR", "gs")
p_Rh_gs <- result_graph_lag("Rh", "gs")
p_Ra_gs <- result_graph_lag("Ra", "gs")

grid.arrange(p_SR_gs, p_Rh_gs, p_Ra_gs, ncol= 3)



# residual
p_rSR_PAR <- result_graph_lag("rSR", "PAR")
p_rRh_PAR <- result_graph_lag("rRh", "PAR")
p_rRa_PAR <- result_graph_lag("rRa", "PAR")

grid.arrange(p_rSR_PAR, p_rRh_PAR, p_rRa_PAR, ncol= 3)

p_rSR_Ts <- result_graph_lag("rSR", "Ts")
p_rRh_Ts <- result_graph_lag("rRh", "Ts")
p_rRa_Ts <- result_graph_lag("rRa", "Ts")

grid.arrange(p_rSR_Ts, p_rRh_Ts, p_rRa_Ts, ncol= 3)

p_rSR_SWC <- result_graph_lag("rSR", "SWC")
p_rRh_SWC <- result_graph_lag("rRh", "SWC")
p_rRa_SWC <- result_graph_lag("rRa", "SWC")

grid.arrange(p_rSR_SWC, p_rRh_SWC, p_rRa_SWC, ncol= 3)

p_rSR_gs <- result_graph_lag("rSR", "gs")
p_rRh_gs <- result_graph_lag("rRh", "gs")
p_rRa_gs <- result_graph_lag("rRa", "gs")

grid.arrange(p_rSR_gs, p_rRh_gs, p_rRa_gs, ncol= 3)


# for power
p_SR_PAR_pwr <- result_graph_pwr("SR", "PAR")
p_Rh_PAR_pwr <- result_graph_pwr("Rh", "PAR")
p_Ra_PAR_pwr<- result_graph_pwr("Ra", "PAR")

grid.arrange(p_SR_PAR_pwr, p_Rh_PAR_pwr, p_Ra_PAR_pwr, ncol= 3)

p_SR_Ts_pwr <- result_graph_pwr("SR", "Ts")
p_Rh_Ts_pwr <- result_graph_pwr("Rh", "Ts")
p_Ra_Ts_pwr <- result_graph_pwr("Ra", "Ts")

grid.arrange(p_SR_Ts_pwr, p_Rh_Ts_pwr, p_Ra_Ts_pwr, ncol= 3)

p_SR_SWC_pwr <- result_graph_pwr("SR", "SWC")
p_Rh_SWC_pwr <- result_graph_pwr("Rh", "SWC")
p_Ra_SWC_pwr <- result_graph_pwr("Ra", "SWC")

grid.arrange(p_SR_SWC_pwr, p_Rh_SWC_pwr, p_Ra_SWC_pwr, ncol= 3)

p_SR_gs_pwr <- result_graph_pwr("SR", "gs")
p_Rh_gs_pwr <- result_graph_pwr("Rh", "gs")
p_Ra_gs_pwr <- result_graph_pwr("Ra", "gs")

grid.arrange(p_SR_gs_pwr, p_Rh_gs_pwr, p_Ra_gs_pwr, ncol= 3)


# residual
p_rSR_PAR_pwr <- result_graph_pwr("rSR", "PAR")
p_rRh_PAR_pwr <- result_graph_pwr("rRh", "PAR")
p_rRa_PAR_pwr<- result_graph_pwr("rRa", "PAR")

grid.arrange(p_rSR_PAR_pwr, p_rRh_PAR_pwr, p_rRa_PAR_pwr, ncol= 3)

p_rSR_Ts_pwr <- result_graph_pwr("rSR", "Ts")
p_rRh_Ts_pwr <- result_graph_pwr("rRh", "Ts")
p_rRa_Ts_pwr <- result_graph_pwr("rRa", "Ts")

grid.arrange(p_rSR_Ts_pwr, p_rRh_Ts_pwr, p_rRa_Ts_pwr, ncol= 3)

p_rSR_SWC_pwr <- result_graph_pwr("rSR", "SWC")
p_rRh_SWC_pwr <- result_graph_pwr("rRh", "SWC")
p_rRa_SWC_pwr <- result_graph_pwr("rRa", "SWC")

grid.arrange(p_rSR_SWC_pwr, p_rRh_SWC_pwr, p_rRa_SWC_pwr, ncol= 3)

p_rSR_gs_pwr <- result_graph_pwr("rSR", "gs")
p_rRh_gs_pwr <- result_graph_pwr("rRh", "gs")
p_rRa_gs_pwr <- result_graph_pwr("rRa", "gs")

grid.arrange(p_rSR_gs_pwr, p_rRh_gs_pwr, p_rRa_gs_pwr, ncol= 3)