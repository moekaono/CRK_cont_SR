# this is to calculate the daylight daily GPP avg

library(dplyr)

setwd("G:/My Drive/Research/Projects/DC_auto/Data/GPP_SR_combined")

GPP_SR_MJ22 <- read.csv("GPP_SR_MJ22.csv") %>% mutate(date = as.POSIXct(Datetime_hr_CST, tz = "Etc/GMT+6"))
GPP_SR_MA23 <- read.csv("GPP_SR_MA23.csv") %>% mutate(date = as.POSIXct(Datetime_hr_CST, tz = "Etc/GMT+6"))
GPP_SR_Mar24 <- read.csv("GPP_SR_Mar24.csv") %>% mutate(date = as.POSIXct(Datetime_hr_CST, tz = "Etc/GMT+6"))
GPP_SR_Apr24 <- read.csv("GPP_SR_Apr24.csv") %>% mutate(date = as.POSIXct(Datetime_hr_CST, tz = "Etc/GMT+6"))
GPP_SR_Sep24 <- read.csv("GPP_SR_Sep24.csv") %>% mutate(date = as.POSIXct(Datetime_hr_CST, tz = "Etc/GMT+6"))
GPP_SR_Oct24 <- read.csv("GPP_SR_Oct24.csv") %>% mutate(date = as.POSIXct(Datetime_hr_CST, tz = "Etc/GMT+6"))


daylight_GPP_avg <- 
  function(df){
    df %>% 
      mutate(
        GPP_daylight = ifelse(PPFD_IN_1_1_1 > 1, GPP, NA)) %>%
      group_by(DoY) %>% 
      summarize(GPP_daylight_day = mean(GPP_daylight, na.rm = T)) %>% 
      summarize(
        GPP_mean = mean(GPP_daylight_day, na.rm = T),
        GPP_sd = sd(GPP_daylight_day, na.rm = T)
      )
    }

# calculate
daylight_GPP_avg(GPP_SR_MJ22)
daylight_GPP_avg(GPP_SR_MA23)
daylight_GPP_avg(GPP_SR_Mar24)
daylight_GPP_avg(GPP_SR_Apr24)
daylight_GPP_avg(GPP_SR_Sep24)
daylight_GPP_avg(GPP_SR_Oct24)


# others stat
var_stat <- function(df, SR_var, Ra_var) {
  df %>%
    mutate(ratio = Rh / .data[[SR_var]]) %>%
    select(PPFD_IN_1_1_1, TS_1_1_1, SWC_1_1_1, all_of(SR_var), Rh, all_of(Ra_var), ratio) %>%
    summarise(
      across(everything(),
             list(
               mean = ~mean(.x, na.rm = TRUE),
               sd = ~sd(.x, na.rm = TRUE)
             ),
             .names = "{.col}_{.fn}")
    )
}


# calculate
var_stat(GPP_SR_MJ22, "SR1", "Ra1")
var_stat(GPP_SR_MA23, "SR1", "Ra1")
var_stat(GPP_SR_Mar24, "SR", "Ra")
var_stat(GPP_SR_Apr24, "SR", "Ra")
var_stat(GPP_SR_Sep24, "SR", "Ra")
var_stat(GPP_SR_Oct24, "SR", "Ra")

