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
# function - skipping the first row for all the datasets
importBind_skip <- function(x, skip){
  temp <- list.files(pattern = paste0("*",x))
  bind <- do.call("rbind", lapply(temp, read.csv, skip = skip, header = T))
  return(bind)
}

setwd("G:/My Drive/Research/Projects/DC_auto/Data/EC")
EP_23_raw <- importBind_skip("eddypro_CRK2023", 1)
```

``` r
EP_23 <- 
  EP_23_raw %>% 
  filter(filename != "") %>% # remove when file name is na - the unit row
  mutate(across(file_records:hit_vin_mean, as.numeric),
         Datetime_30min_CST = force_tz(ymd_hm(paste(date, time)), tzone = "Etc/GMT+6")) %>%
  mutate(across(file_records:hit_vin_mean, na_if, -9999)) %>%
  arrange(Datetime_30min_CST)
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
  select(Datetime_30min_CST, air_pressure, air_density, ET, VPD) %>% # select the necessary variables
  mutate(gs = (air_pressure*(ET/3600)*0.46152)/(air_density*VPD*0.28705)) %>%
  mutate(gs_qaqc = case_when(gs < 0 ~ NA,      # gs <0 was from negative ET, which is invalid
                             gs == Inf ~ NA,   # gs = +/- Inf was from VPD = 0, which means the air is 100% saturated 
                             is.na(gs) ~ NA,   # there was one NA in VPD  
                             TRUE ~ gs))

# stats
summary(EP_23_gs)
```

    ##  Datetime_30min_CST                air_pressure     air_density   
    ##  Min.   :2023-01-01 00:30:00.00   Min.   : 97386   Min.   :1.087  
    ##  1st Qu.:2023-03-11 00:52:30.00   1st Qu.: 99133   1st Qu.:1.135  
    ##  Median :2023-05-18 13:15:00.00   Median : 99397   Median :1.155  
    ##  Mean   :2023-05-16 10:10:56.92   Mean   : 99458   Mean   :1.164  
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
ggplot(EP_23_gs) + geom_point(aes(Datetime_30min_CST, gs_qaqc)) + theme_bw() + 
  ggtitle("canopy conductance (m/s) in 2023") + ylab("gs(m/s)") 
```

    ## Warning: Removed 3547 rows containing missing values (`geom_point()`).

![](CRK_conductance_files/figure-gfm/calculate%20gs-1.png)<!-- -->

``` r
# making hourly data
EP_23_gs_hr <- 
  EP_23_gs %>%
  mutate(Datetime_hr_CST = floor_date(Datetime_30min_CST, unit = "hour")) %>%
  group_by(Datetime_hr_CST) %>%
  summarise(gs_qaqc = mean(gs_qaqc, na.rm = T))

# stats
summary(EP_23_gs_hr)
```

    ##  Datetime_hr_CST                     gs_qaqc      
    ##  Min.   :2023-01-01 00:00:00.00   Min.   :0.0000  
    ##  1st Qu.:2023-03-10 23:45:00.00   1st Qu.:0.0006  
    ##  Median :2023-05-18 09:30:00.00   Median :0.0023  
    ##  Mean   :2023-05-16 08:49:13.06   Mean   :0.0041  
    ##  3rd Qu.:2023-07-22 14:15:00.00   3rd Qu.:0.0063  
    ##  Max.   :2023-09-25 05:00:00.00   Max.   :0.0946  
    ##                                   NA's   :1207

``` r
# viz for hourly data
ggplot(EP_23_gs_hr) + geom_point(aes(Datetime_hr_CST, gs_qaqc)) + theme_bw() + 
  ggtitle("Hourly canopy conductance (m/s) in 2023") + ylab("gs(m/s)") 
```

    ## Warning: Removed 1207 rows containing missing values (`geom_point()`).

![](CRK_conductance_files/figure-gfm/calculate%20gs-2.png)<!-- -->

``` r
# output
#write.csv(EP_23_gs_hr, "G:/My Drive/Research/Projects/DC_auto/Data/canopy_conductance/canopy_conductance_23.csv")
```

``` r
setwd("G:/My Drive/Research/Projects/DC_auto/Data/canopy_conductance")

# read the data that Dohee calculated
gs_22_raw <- 
  read_xlsx("canopy_conductance_22.xlsx", sheet = "Calculation", 
            range = cell_cols("D:V")) %>%
  select(c(1,13:16,19)) %>% 
  slice(6:nrow(.)) %>%
  mutate_all(as.numeric) %>% 
  rename(DOY = `EddyPro Data`, 
         P = `Required (EddyPro)`, E = `...14`, p = `...15`, VPD = `...16`, 
         gs_Dohee = `...19`)
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`

``` r
# convert DOY to datetime
# ref: https://community.rstudio.com/t/convert-day-of-year-to-datetime/88248
secs_per_day <- 24*3600

gs_22 <- 
  gs_22_raw %>%
  na.omit() %>%
  mutate(Datetime = force_tz(as.POSIXct(DOY*secs_per_day, tz = "UTC", origin = "2021-12-31"), tzone = "Etc/GMT+6"),
         VPD_clean = ifelse(VPD < 0, NA, VPD),
         E_clean = ifelse(E < 0, NA, E)) %>%
  mutate(TIMESTAMP_START = floor_date(Datetime, unit = "30minutes"),
         gs = (P*E_clean*0.46152)/(p*VPD_clean*0.28705)) %>%
  mutate(gs_qaqc = case_when(gs < 0 ~ NA,      # gs <0 was from negative ET, which is invalid
                             gs == Inf ~ NA,   # gs = +/- Inf was from VPD = 0, which means the air is 100% saturated 
                             is.na(gs) ~ NA,   # there was one NA in VPD  
                             TRUE ~ gs))


summary(gs_22)
```

    ##       DOY                P                E                    p        
    ##  Min.   :  1.021   Min.   : 97.54   Min.   :-6.265e-04   Min.   :1.104  
    ##  1st Qu.: 61.042   1st Qu.: 99.04   1st Qu.: 3.645e-07   1st Qu.:1.157  
    ##  Median :135.937   Median : 99.49   Median : 8.162e-06   Median :1.190  
    ##  Mean   :172.166   Mean   : 99.54   Mean   : 4.065e-05   Mean   :1.195  
    ##  3rd Qu.:303.458   3rd Qu.: 99.99   3rd Qu.: 4.527e-05   3rd Qu.:1.230  
    ##  Max.   :365.937   Max.   :101.97   Max.   : 2.402e-03   Max.   :1.359  
    ##                                                                         
    ##       VPD            gs_Dohee             Datetime                     
    ##  Min.   :0.0000   Min.   :-1.063e+14   Min.   :2022-01-01 00:29:57.11  
    ##  1st Qu.:0.1936   1st Qu.: 0.000e+00   1st Qu.:2022-03-02 00:59:54.24  
    ##  Median :0.4875   Median : 0.000e+00   Median :2022-05-15 22:29:42.72  
    ##  Mean   :0.6820   Mean   :-3.902e+10   Mean   :2022-06-21 03:58:34.09  
    ##  3rd Qu.:0.9553   3rd Qu.: 0.000e+00   3rd Qu.:2022-10-30 10:59:31.20  
    ##  Max.   :4.5214   Max.   : 1.293e+14   Max.   :2022-12-31 22:29:16.79  
    ##                                                                        
    ##    VPD_clean          E_clean       TIMESTAMP_START                 
    ##  Min.   :0.00001   Min.   :0.0000   Min.   :2022-01-01 00:00:00.00  
    ##  1st Qu.:0.20681   1st Qu.:0.0000   1st Qu.:2022-03-02 00:30:00.00  
    ##  Median :0.50254   Median :0.0000   Median :2022-05-15 22:00:00.00  
    ##  Mean   :0.69549   Mean   :0.0001   Mean   :2022-06-21 03:29:35.57  
    ##  3rd Qu.:0.97015   3rd Qu.:0.0001   3rd Qu.:2022-10-30 10:30:00.00  
    ##  Max.   :4.52138   Max.   :0.0024   Max.   :2022-12-31 22:00:00.00  
    ##  NA's   :214       NA's   :2483                                     
    ##        gs            gs_qaqc      
    ##  Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0017   1st Qu.:0.0017  
    ##  Median :0.0052   Median :0.0052  
    ##  Mean   :0.0200   Mean   :0.0200  
    ##  3rd Qu.:0.0128   3rd Qu.:0.0128  
    ##  Max.   :8.1577   Max.   :8.1577  
    ##  NA's   :2586     NA's   :2586

``` r
#viz
ggplot(gs_22) + geom_point(aes(TIMESTAMP_START, gs_qaqc)) + theme_bw() + 
  ggtitle("Canopy conductance (m/s) in 2022") + ylab("gs(m/s)") 
```

    ## Warning: Removed 2586 rows containing missing values (`geom_point()`).

![](CRK_conductance_files/figure-gfm/read%202022-1.png)<!-- -->

``` r
# making hourly data
gs_22_hr <- 
  gs_22 %>%
  mutate(Datetime_hr_CST = floor_date(TIMESTAMP_START, unit = "hour")) %>%
  group_by(Datetime_hr_CST) %>%
  summarise(gs_qaqc = mean(gs, na.rm = T))

#viz
ggplot(gs_22_hr) + geom_point(aes(Datetime_hr_CST, gs_qaqc)) + theme_bw() + 
  ggtitle("Hourly canopy conductance (m/s) in 2022") + ylab("gs(m/s)") 
```

    ## Warning: Removed 912 rows containing missing values (`geom_point()`).

![](CRK_conductance_files/figure-gfm/read%202022-2.png)<!-- -->

``` r
summary(gs_22_hr)
```

    ##  Datetime_hr_CST                     gs_qaqc      
    ##  Min.   :2022-01-01 00:00:00.00   Min.   :0.0000  
    ##  1st Qu.:2022-03-02 10:30:00.00   1st Qu.:0.0016  
    ##  Median :2022-05-15 21:00:00.00   Median :0.0052  
    ##  Mean   :2022-06-20 18:34:48.78   Mean   :0.0221  
    ##  3rd Qu.:2022-10-29 20:30:00.00   3rd Qu.:0.0125  
    ##  Max.   :2022-12-31 22:00:00.00   Max.   :6.3664  
    ##                                   NA's   :912

``` r
CRK_gs <- 
  rbind(gs_22_hr, EP_23_gs_hr)

#write.csv(CRK_gs, "G:/My Drive/Research/Projects/DC_auto/Data/canopy_conductance/CRK_canopy_conductance.csv")
```
