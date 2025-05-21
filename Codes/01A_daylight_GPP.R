# this is to calculate the daylight daily GPP avg

library(dplyr)

dir_GPP <- "G:/My Drive/Research/Projects/DC_auto/Data/GPP/"

May2022 <- read.csv(paste0(dir_GPP, "GPP_May2022.csv"))
Mar2023 <- read.csv(paste0(dir_GPP, "GPP_Mar2023.csv"))
# Feb2024 <- readxl::read_xlsx(paste0(dir_GPP, "RE_GPP_Feb2024.xlsx"))
# Mar2024 <- readxl::read_xlsx(paste0(dir_GPP, "RE_GPP_Mar2024.xlsx"))
# Sep2024 <- readxl::read_xlsx(paste0(dir_GPP, "GPP_Sep2024.xlsx"))
# Oct2024 <- readxl::read_xlsx(paste0(dir_GPP, "GPP_Oct2024.xlsx"))
MA24 <- readxl::read_xlsx(paste0(dir_GPP, "GPP_MA2024.xlsx"))
SO24 <- readxl::read_xlsx(paste0(dir_GPP, "GPP_SO2024.xlsx"))


daylight_GPP_avg <- 
  function(df){
   
    df1 <- 
      df %>% 
      mutate(
        GPP_daylight_all = ifelse(PPFD > 1 & GPP_fqc != -9999, GPP_f, NA),
        GPP_daylight_org = ifelse(PPFD > 1 & GPP_fqc == 0, GPP_f, NA),
        GPP_daylight_one = ifelse(PPFD > 1 & GPP_fqc %in% c(0, 1), GPP_f, NA),
        GPP_daylight_two = ifelse(PPFD > 1 & GPP_fqc %in% c(0, 1, 2), GPP_f, NA)
        ) %>%
      group_by(DoY) %>% 
      summarize(
        GPP_daylight_day_all = mean(GPP_daylight_all, na.rm = T),
        GPP_daylight_day_org = mean(GPP_daylight_org, na.rm = T),
        GPP_daylight_day_one = mean(GPP_daylight_one, na.rm = T),
        GPP_daylight_day_two = mean(GPP_daylight_two, na.rm = T)
        )
    
    return(df1)
  
  }

# May2022
GPP_May2022 <- daylight_GPP_avg(May2022)
write.csv(GPP_May2022, paste0(dir_GPP, "GPP_daylight/GPP_daylight_May2022.csv"), row.names = F)

GPP_May2022 %>%
  summarise(
    all_AVG = mean(GPP_daylight_day_all, na.rm = T),
    all_STD = sd(GPP_daylight_day_all, na.rm = T),
  )


# Mar2023
GPP_Mar2023 <- daylight_GPP_avg(Mar2023)
write.csv(GPP_Mar2023, paste0(dir_GPP, "GPP_daylight/GPP_daylight_Mar2023.csv"), row.names = F)

GPP_Mar2023 %>%
  summarise(
    AVG = mean(GPP_daylight_day, na.rm = T),
    STD = sd(GPP_daylight_day, na.rm = T)
  )


# NO GPP in Jan2024

# Feb2024
GPP_Feb2024 <- daylight_GPP_avg(Feb2024)
write.csv(GPP_Feb2024, paste0(dir_GPP, "GPP_daylight/GPP_daylight_Feb2024.csv"), row.names = F)

GPP_Feb2024 %>%
  summarise(
    AVG = mean(GPP_daylight_day_all, na.rm = T),
    STD = sd(GPP_daylight_day_all, na.rm = T)
    )



# Mar2024
GPP_Mar2024 <- daylight_GPP_avg(Mar2024)
write.csv(GPP_Mar2024, paste0(dir_GPP, "GPP_daylight/GPP_daylight_Mar2024.csv"), row.names = F)

GPP_Mar2024 %>%
  summarise(
    AVG = mean(GPP_daylight_day_all, na.rm = T),
    STD = sd(GPP_daylight_day_all, na.rm = T)
  )

# MA2024
GPP_MA24 <- daylight_GPP_avg(MA24)
write.csv(GPP_MA24, paste0(dir_GPP, "GPP_daylight/GPP_daylight_MA2024.csv"), row.names = F)

GPP_MA24 %>%
  summarise(
    AVG = mean(GPP_daylight_day_all, na.rm = T),
    STD = sd(GPP_daylight_day_all, na.rm = T)
  )

# SO2024
GPP_SO24 <- daylight_GPP_avg(SO24)
write.csv(GPP_SO24, paste0(dir_GPP, "GPP_daylight/GPP_daylight_SO2024.csv"), row.names = F)

GPP_SO24 %>%
  summarise(
    AVG = mean(GPP_daylight_day_all, na.rm = T),
    STD = sd(GPP_daylight_day_all, na.rm = T)
  )

