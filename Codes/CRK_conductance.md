CRK_conductance
================
Moeka
2024-03-13

This is for calculating gas conductance from EddyPro output.

### gs = (P x E x Rw)/(p x VPD x Rd)

- gs = canopy conductance(m s-1) The higher conductance is, the easier
  the flow gets

- P = Atmospheric pressure (kPa)

- E = Evapotranspiration rate (kg m-2 s-1)

- p = Air density (kg m-3))

- VPD = Vapor pressure deficit (kPa)

- Universal gas constant for water vapor (Rw) = 0.46152

- Universal gas constant for dry air (Rd) = 0.28705

``` r
setwd("G:/My Drive/Research/Projects/DC_auto/Data/Tower")


# function - skipping the first row for all the datasets
importBind <- function(x){
  temp <- list.files(pattern = paste0("*",x))
  bind <- do.call("rbind", lapply(temp, read.csv, skip = 1, header = T))
  return(bind)
}


EP_23_raw <- importBind("eddypro_CRK2023")
```

``` r
EP_23 <- 
  EP_23_raw %>% 
  filter(filename != "") %>% # remove when file name is na - the unit row
  mutate(date = as.Date(date),
         DOY = as.numeric(DOY)) %>%
  mutate(across(file_records:hit_vin_mean, as.numeric),
         Datetime = force_tz(as.POSIXct(DOY*24*3600, tz = "UTC", origin = "2022-12-31"), tzone = "America/Chicago")) %>%
  mutate(across(file_records:hit_vin_mean, na_if, -9999),
         Datetime_30min = round_date(Datetime, unit = "30 mins")) %>%
  arrange(Datetime)
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `across(file_records:hit_vin_mean, na_if, -9999)`.
    ## Caused by warning:
    ## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
    ## Supply arguments directly to `.fns` through an anonymous function instead.
    ## 
    ##   # Previously
    ##   across(a:b, mean, na.rm = TRUE)
    ## 
    ##   # Now
    ##   across(a:b, \(x) mean(x, na.rm = TRUE))

``` r
# calculate gs
EP_23_gs <- 
  EP_23 %>%
  select(Datetime_30min, air_pressure, air_density, ET, VPD) %>% 
  mutate(gs = (air_pressure*(ET/3600)*0.46152)/(air_density*VPD*0.28705)) %>%
  mutate(gs_qaqc = case_when(gs < 0 ~ NA,      # gs <0 was from negative ET, which is invalid
                             gs == Inf ~ NA,   # gs = +/- Inf was from VPD = 0, which means the air is 100% saturated 
                             is.na(gs) ~ NA,    # there was one NA in VPD  
                             TRUE ~ gs))

# stats
summary(EP_23_gs)
```

    ##  Datetime_30min                    air_pressure     air_density   
    ##  Min.   :2023-01-01 00:30:00.00   Min.   : 97386   Min.   :1.087  
    ##  1st Qu.:2023-03-11 00:52:30.00   1st Qu.: 99133   1st Qu.:1.135  
    ##  Median :2023-05-18 13:15:00.00   Median : 99397   Median :1.155  
    ##  Mean   :2023-05-16 10:26:11.78   Mean   : 99458   Mean   :1.164  
    ##  3rd Qu.:2023-07-22 15:37:30.00   3rd Qu.: 99705   3rd Qu.:1.185  
    ##  Max.   :2023-09-25 05:00:00.00   Max.   :101408   Max.   :1.298  
    ##                                                                   
    ##        ET                 VPD               gs                gs_qaqc     
    ##  Min.   :-0.857238   Min.   :   0.0   Min.   :      -Inf   Min.   :0.000  
    ##  1st Qu.:-0.001365   1st Qu.: 402.5   1st Qu.:-0.0001219   1st Qu.:0.001  
    ##  Median : 0.019832   Median : 766.3   Median : 0.0009795   Median :0.003  
    ##  Mean   : 0.102053   Mean   :1119.7   Mean   :       NaN   Mean   :0.004  
    ##  3rd Qu.: 0.156881   3rd Qu.:1529.1   3rd Qu.: 0.0048292   3rd Qu.:0.007  
    ##  Max.   : 1.231055   Max.   :5267.6   Max.   :       Inf   Max.   :0.152  
    ##                      NA's   :1        NA's   :1            NA's   :3547

``` r
# viz 
ggplot(EP_23_gs) + geom_point(aes(Datetime_30min, gs_qaqc)) + theme_bw() + 
  ggtitle("canopy conductance (m/s) in 2023") + ylab("gs(m/s)") 
```

    ## Warning: Removed 3547 rows containing missing values (`geom_point()`).

![](CRK_conductance_files/figure-gfm/calculate%20gs-1.png)<!-- -->

``` r
# making hourly data
EP_23_gs_hr <- 
  EP_23_gs %>%
  mutate(Datetime_hr = floor_date(Datetime_30min, unit = "hour")) %>%
  group_by(Datetime_hr) %>%
  summarise(gs_qaqc = mean(gs_qaqc, na.rm = T))

# stats
summary(EP_23_gs_hr)
```

    ##   Datetime_hr                        gs_qaqc      
    ##  Min.   :2023-01-01 00:00:00.00   Min.   :0.0000  
    ##  1st Qu.:2023-03-10 23:30:00.00   1st Qu.:0.0006  
    ##  Median :2023-05-18 10:00:00.00   Median :0.0023  
    ##  Mean   :2023-05-16 09:19:40.01   Mean   :0.0041  
    ##  3rd Qu.:2023-07-22 14:30:00.00   3rd Qu.:0.0063  
    ##  Max.   :2023-09-25 05:00:00.00   Max.   :0.0946  
    ##                                   NA's   :1206

``` r
# viz for hourly data
ggplot(EP_23_gs_hr) + geom_point(aes(Datetime_hr, gs_qaqc)) + theme_bw() + 
  ggtitle("Hourly canopy conductance (m/s) in 2023") + ylab("gs(m/s)") 
```

    ## Warning: Removed 1206 rows containing missing values (`geom_point()`).

![](CRK_conductance_files/figure-gfm/calculate%20gs-2.png)<!-- -->

``` r
# output
#write.csv(EP_23_gs_hr, "G:/My Drive/Research/Projects/DC_auto/Data/canopy_conductance_23.csv")
```
