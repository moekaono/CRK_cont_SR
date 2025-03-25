# this is to calculate the daylight daily GPP avg

library(dplyr)

dir_GPP <- "G:/My Drive/Research/Projects/DC_auto/Data/GPP/"

May2022 <- read.csv(paste0(dir_GPP, "GPP_May2022.csv"))
Mar2023 <- read.csv(paste0(dir_GPP, "GPP_Mar2023.csv"))
Feb2024 <- readxl::read_xlsx(paste0(dir_GPP, "RE_GPP_Feb2024.xlsx"))
Mar2024 <- readxl::read_xlsx(paste0(dir_GPP, "RE_GPP_Mar2024.xlsx"))

daylight_GPP_avg <- 
  function(df){
   
    df1 <- 
      df %>% 
      mutate(
        GPP_daylight = ifelse(PPFD > 1 & GPP_fqc != -9999, GPP_f, NA)
        ) %>%
      group_by(DoY) %>% 
      summarize(GPP_daylight_day = mean(GPP_daylight, na.rm = T))
    
    return(df1)
  
  }

# May2022
GPP_May2022 <- daylight_GPP_avg(May2022)

GPP_May2022 %>%
  summarise(
    AVG = mean(GPP_daylight_day, na.rm = T),
    STD = sd(GPP_daylight_day, na.rm = T)
  )


# Mar2023
GPP_Mar2023 <- daylight_GPP_avg(Mar2023)

GPP_Mar2023 %>%
  summarise(
    AVG = mean(GPP_daylight_day, na.rm = T),
    STD = sd(GPP_daylight_day, na.rm = T)
  )



# Feb2024
GPP_Feb2024 <- daylight_GPP_avg(Feb2024)

GPP_Feb2024 %>%
  summarise(
    AVG = mean(GPP_daylight_day, na.rm = T),
    STD = sd(GPP_daylight_day, na.rm = T)
    )



# Mar2024
GPP_Mar2024 <- daylight_GPP_avg(Mar2024)

GPP_Mar2024 %>%
  summarise(
    AVG = mean(GPP_daylight_day, na.rm = T),
    STD = sd(GPP_daylight_day, na.rm = T)
  )
