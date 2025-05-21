CRK cont SR - QAQC & plot
================
Moeka
2024-02-19

``` r
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

library(ggplot2)
library(dplyr)                  
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(googlesheets4)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(PerformanceAnalytics)
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## ######################### Warning from 'xts' package ##########################
    ## #                                                                             #
    ## # The dplyr lag() function breaks how base R's lag() function is supposed to  #
    ## # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
    ## # source() into this session won't work correctly.                            #
    ## #                                                                             #
    ## # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
    ## # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
    ## # dplyr from breaking base R's lag() function.                                #
    ## #                                                                             #
    ## # Code in packages is not affected. It's protected by R's namespace mechanism #
    ## # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
    ## #                                                                             #
    ## ###############################################################################

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

``` r
library(ggpmisc)
```

    ## Loading required package: ggpp

    ## Registered S3 methods overwritten by 'ggpp':
    ##   method                  from   
    ##   heightDetails.titleGrob ggplot2
    ##   widthDetails.titleGrob  ggplot2

    ## 
    ## Attaching package: 'ggpp'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

    ## Registered S3 method overwritten by 'ggpmisc':
    ##   method                  from   
    ##   as.character.polynomial polynom

``` r
library(Hmisc)
```

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
library(scales)
library(hms)
```

    ## 
    ## Attaching package: 'hms'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     hms

``` r
library(readxl)

# doesn't work anymore
#library(RespChamberProc)
## How to install RespChamberProc? -> Go to https://freesoft.dev/program/97026756.
```

``` r
#knitr::opts_chunk$set(eval = FALSE)
#raw <- read81xVar("G:/My Drive/research/DC_auto/DC_042922_auto.81x") # will take some time

# raw$iChunk <- as.character(raw$iChunk)

#remove incomplete observation
#df <- raw[raw$iChunk != 1,]
```

Note: - 2022-08-16 ~ 2022-08-23: shallow/deep labels were not recorded.
Have to be manually added. Port#5 = shallow; Port#14 = deep.

``` r
setwd("G:/Shared drives/Project_Crockett/Field work/Continuous SR measurement/Processed data")

# 2022-2024
# function - read and rbind the datasets
importBind <- function(x){
  temp <- list.files(pattern = paste0("*",x))
  bind <- do.call("rbind", lapply(temp, read.csv, header = T))
  return(bind)
}

# read SoilFluxPro outputs in 2022-2024
SR_cont_raw <- importBind("CRK_cont_SFP_qaqc")


str(SR_cont_raw)
```

    ## 'data.frame':    15011 obs. of  21 variables:
    ##  $ Obs.              : int  2 3 4 5 6 7 8 9 10 11 ...
    ##  $ Port.             : int  6 15 6 15 6 15 6 15 6 15 ...
    ##  $ Date_IV           : chr  "4/29/2022 15:20" "4/29/2022 15:25" "4/29/2022 16:20" "4/29/2022 16:25" ...
    ##  $ Offset            : num  10 6.3 10 6.3 10 6.3 10 6.3 10 6.3 ...
    ##  $ CrvFitStatus      : chr  "Exp" "Exp" "Exp" "Exp" ...
    ##  $ Exp_Flux          : num  0.07 0.05 0.08 0.05 0.01 0 -0.04 -0.02 -0.11 -0.03 ...
    ##  $ Exp_FluxCV        : num  6.8 4.3 5.4 5.1 68.3 ...
    ##  $ Exp_R2            : chr  "0.5862" "0.789" "0.6691" "0.6482" ...
    ##  $ Exp_Iter          : int  3 2 2 4 3 3 6 5 9 8 ...
    ##  $ Lin_Flux          : num  0.07 0.05 0.07 0.04 0.01 0 -0.04 -0.02 -0.06 -0.03 ...
    ##  $ Lin_FluxCV        : num  6.2 4.4 5.9 6.1 68.3 66.7 9.4 13.6 6.7 8.5 ...
    ##  $ Lin_R2            : num  0.6424 0.7889 0.6674 0.6548 0.0143 ...
    ##  $ Observation.Length: chr  "2:30" "2:30" "2:30" "2:30" ...
    ##  $ V1_IV             : num  0.064 0.049 0.051 0.1 0.096 0.1 0.091 0.098 0.111 0.1 ...
    ##  $ V1_Mean           : num  0.003 0.047 0.081 0.1 0.086 0.1 0.079 0.097 0.067 0.095 ...
    ##  $ Cdry_Mean         : num  566 568 593 595 609 ...
    ##  $ Tcham_Mean        : num  30 174.1 31.6 177.6 28.5 ...
    ##  $ Pressure_Mean     : num  101 101 101 101 101 ...
    ##  $ H2O_Mean          : num  22.3 22.8 27 27.4 28.1 ...
    ##  $ Label             : chr  "shallow" "deep" "shallow" "deep" ...
    ##  $ Vmux              : int  55 55 55 55 55 55 55 55 55 55 ...

``` r
unique(SR_cont_raw$Label)
```

    ## [1] "shallow"  "deep"     ""         "long"     "shallow2" "shallow3" "sh1"     
    ## [8] "sh3"      "sh2"

``` r
# function to clean up the variables
clean_cont <- function(df){  
  df %>%
  mutate(Datetime_CST = as.POSIXct(Date_IV, format = "%m/%d/%Y %H:%M", tz = "Etc/GMT+6"),
         Flux = ifelse(grepl("Exp", CrvFitStatus), Exp_Flux, Lin_Flux),
         CV = ifelse(grepl("Exp", CrvFitStatus), Exp_FluxCV, Lin_FluxCV),
         R2 = ifelse(grepl("Exp", CrvFitStatus), as.numeric(Exp_R2), as.numeric(Lin_R2))) %>%
  filter(!is.na(Datetime_CST)) %>% 
  distinct()
  }

# clean up variables
SR_cont <- clean_cont(SR_cont_raw)

# clean up Label
SR_cont_v1 <- 
  SR_cont %>%
  mutate(Collar = case_when(Label == "shallow" ~ "sh1",
                            Label == "shallow2" ~ "sh2",
                            Label == "shallow3" ~ "sh3",
                            Label == "long" ~ "deep",
                            TRUE ~ Label)) %>%
  mutate(Collar = case_when(Collar == "" & Port. == "5" ~ "sh1",
                            Collar == "" & Port. == "14" ~ "deep",
                            TRUE ~ Collar))

# should be only 4 types
unique(SR_cont_v1$Collar)
```

    ## [1] "sh1"  "deep" "sh2"  "sh3"

“Obs.”: doesn’t matter much “Port.”: Multiplexer port number (0 if not
using a multiplexer) “Date_IV”: date type default is “M/DD/YYYY HH:SS”
“Offset”: Offset (cm) used in volume calculation.

“CrvFitStatus”: Curve fit solution. “Exp” means the exponential fit was
better than the linear fit (Exp_SSN \< Lin\_ SSN). “Lin” means the
linear fit was still better after the maximum number of iterations, and
the nonlinear coefficients have therefore been derived from linear fit.

“Exp_Flux”: Flux computed from Exponential Fit. “Exp_FluxCV”:
Coefficient of variance (%) of Exp Flux “Exp_R2”: Correlation
coefficient for Exponential Fit. “Exp_Iter”: Number of iterations used
in the Exponential Fit. “Lin_Flux”: Flux computed from Linear Fit.
“Lin_FluxCV”: Coefficient of variable (%) of Lin Flux “Lin_R2”:
Correlation coefficient for the Linear Fit. “Observation.Length”: The
original observation length. “V1_IV”: Initial values of the voltage
channel  
“V1_Mean”: Initial values of the voltage channel = Flow signals
“Cdry_Mean”: Chamber CO2 concentration (μmol/mol), corrected for water
vapor dilution “Tcham_Mean”: Air temperature (C) inside the soil chamber
during an active measurement (if no measurement is active, displays
Multiplexer case temperature)

“Pressure_Mean”: Atmospheric pressure in the optical bench (kPa)
“H2O_Mean”: Chamber water vapor concentration (mmol/mol) “Label”: User
entered at time of data collection. “Vmux”: Volume of the multiplexer
(if used) (cm3)

``` r
ggplot(SR_cont_v1) + 
  geom_point(aes(Datetime_CST, Flux, col = as.character(Collar))) + 
  theme_bw()
```

![](CRK_cont_SR_qaqc_files/figure-gfm/raw%20data%20plot-1.png)<!-- -->

``` r
knitr::opts_chunk$set(eval = FALSE)
# R2
ggplot(SR_cont_v1, aes(x = R2, col = Collar)) + 
  geom_freqpoly(bins=100, size = .8, alpha = .5) + xlim(.96,1) + theme_bw()

# CV
ggplot(SR_cont_v1, aes(x = CV, col = Collar)) + 
  geom_freqpoly(bins=100, size = .8, alpha = .5) + xlim(.8,2) + theme_bw()

# Cdry_Mean
ggplot(SR_cont_v1, aes(x = Cdry_Mean, col = Collar)) + 
  geom_freqpoly(bins=100, size = .8, alpha = .5) + xlim(350,1000) + theme_bw()

# V1_Mean
ggplot(SR_cont_v1, aes(x = V1_Mean, col = Collar)) + 
  geom_freqpoly(bins=100, size = .8, alpha = .5) + theme_bw()


# V1 vs R2
ggplot(SR_cont_v1, aes(x = V1_Mean, y = CV, col = as.character(Collar))) + 
  geom_point(size = .8, alpha = .5) + theme_bw() + ylim(.8,10)

# boxplot after applying 3 variables
SR_cont_v1 %>% filter(R2 > .985 & CV < 1.25 & V1_Mean > 2) %>%
ggplot() + geom_boxplot(aes(as.character(Collar), Flux)) + theme_bw()


# cont %>% filter(Label == "shallow") %>% summarize(quantile(Cdry_Mean, .75) + 1.5*IQR(Cdry_Mean))
# the upper limit was 755.34

# raw data
# pairs(cont[,c(15,16,18,23:25)],
#       lower.panel = NULL, 
#       col = factor(cont$Label),
#       cex.labels=3)

# cont %>% 
#   filter(R2 > 0.9 & CV < 10) %>%
#   select(c(15,16,18,23:25)) %>%
#   pairs(lower.panel = NULL, 
#       col = factor(cont$Label),
#       cex.labels=3)
```

# QAQC

### Related to Flux

- R2 \> 0.975
- CV \< 1.9
- negative Flux

### Related to instrument

- V1_Mean \> 2

Note: - Negative or very low flux can be captured by CV and V1 but not
R2 as they can have clean lines - V1 values depend on chambers but all
of them should be over 2 when running properly - Deep collar still
observed very large fluxes which did not appear in other shallow collars
even after QAQC with 3 variables above. - Are they likely due to insects
or micro fauna? Can I just remove them?

``` r
cont_qaqc <- 
  SR_cont_v1 %>% 
  filter(CV < 1.9 & R2 > 0.975 & V1_Mean > 2 & Flux > 0) %>% 
  arrange(Datetime_CST)

# all the data after QAQCed
ggplot(cont_qaqc) + 
  geom_point(aes(Datetime_CST, Flux, col = Collar), alpha = .5) + 
  theme_bw() + 
  ggtitle("All data available since 2022", subtitle = "QAQC: R2 > 0.975, CV < 1.9, V1 > 2") +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/qaqc%20data-1.png)<!-- -->

``` r
#write.csv(cont_qaqc, "G:/Shared drives/Project_Crockett/Field work/Continuous SR measurement/Processed data/cont_qaqc.csv", row.names = FALSE)
```

Refer to field log for cont chambers - deep collar installation - 1.
2022-04-29 - 2. 2023-04-11 - 3. 2023-10-29

Term_1: 2022-05-22 and 2022-06-12: also available a few days around
2022-06-26

``` r
# data availability for 2022
cont_qaqc %>% 
  filter(format(as.Date(Datetime_CST),"%Y") == "2022") %>%
  ggplot() + 
  geom_point(aes(Datetime_CST, Flux, col = Collar), alpha = .5) + 
  theme_bw() + labs(color = "Collar type") + 
  ggtitle("Data avilable in 2022", subtitle = "Deep collar installed on Apr 29th") + 
  scale_x_datetime(date_breaks = "1 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/when%20the%20pure%20Rh%20is%20available-1.png)<!-- -->

``` r
# # deep available from 2022-05-22 11:25 am till 2022-06-09 15:25
# cont_qaqc %>% 
#   filter(Datetime_CST > "2022-05-22" & Datetime_CST < "2022-06-26") %>% 
#   ggplot() + geom_point(aes(Datetime_CST, Flux, col = Collar)) + theme_bw() +
#   ggtitle("1-2 months after deep collar installation", subtitle = "From 2022-05-22 to 2022-06-26")
# 
# 
# cont_qaqc %>% 
#   filter(Datetime_CST > "2022-11-22" & Datetime_CST < "2022-12-01") %>% 
#   ggplot() + geom_point(aes(Datetime_CST, Flux, col = as.character(Label))) + theme_bw() + 
#   ggtitle("7 months after deep collar installation", subtitle = "From 2022-11-23 to 2022-11-30")


# # data availability for 2023
cont_qaqc %>%
  filter(format(as.Date(Datetime_CST),"%Y") == "2023") %>%
  ggplot() +
  geom_point(aes(Datetime_CST, Flux, col = Collar), alpha = .5) +
  theme_bw() +  labs(color = "Collar type") +
  ggtitle("Data avilable in 2023", subtitle = "Deep collar installed on Apr 11th & Oct 29th") +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/when%20the%20pure%20Rh%20is%20available-2.png)<!-- -->

``` r
#  
# 
# cont_qaqc %>% 
#   filter(Datetime_CST < "2023-04-04" & Datetime_CST > "2023-03-10") %>% 
#   ggplot() + geom_point(aes(Datetime_CST, Flux, col = as.character(Label))) + theme_bw() + 
#   ggtitle("10-11 months after deep collar installation", subtitle = "From 2023-03-10 to 2023-04-04")


# REMOVE OUTLIERS >10
# cont_qaqc %>% 
#   filter(Datetime_CST > as.Date("2023-04-25") & Datetime_CST < as.Date("2023-06-08")) %>% 
#   ggplot() + geom_point(aes(Datetime_CST, Flux, col = as.character(Label))) + theme_bw()+ 
#   ggtitle("0-2 month after deep collar installation", subtitle = "From 2023-04-30 to 2023-06-08")
# 
# cont_qaqc %>% 
#   filter(Datetime_CST > as.Date("2023-05-23") & Datetime_CST < as.Date("2023-06-08")) %>% 
#   ggplot() + geom_point(aes(Datetime_CST, Flux, col = as.character(Label))) + theme_bw()+ 
#   ggtitle("0-2 month after deep collar installation", subtitle = "From 2023-05-20 to 2023-06-08")


# looking weird since end of august 2023
# cont_qaqc %>% 
#   filter(Datetime_CST < as.Date("2023-09-17") & Datetime_CST > as.Date("2023-08-10")) %>% 
#   filter(Flux < 10) %>% # did not get removed by filters
#   ggplot() + geom_point(aes(Datetime_CST, Flux, col = as.character(Label))) + theme_bw() + 
#   ggtitle("4-5 month after deep collar installation", subtitle = "From 2023-08-10 to 2023-09-17")


# # data availability for 2024
cont_qaqc %>% 
  filter(format(as.Date(Datetime_CST),"%Y") == "2024") %>%
  ggplot() + 
  geom_point(aes(Datetime_CST, Flux, col = Collar), alpha = .5) + 
  theme_bw() +  labs(color = "Collar type") + 
  ggtitle("Data avilable in 2024", subtitle = "Deep collar installed on Oct 29th, 2023") + 
  scale_x_datetime(date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/when%20the%20pure%20Rh%20is%20available-3.png)<!-- -->

``` r
# cont_qaqc %>% 
#   filter(Datetime_CST > as.Date("2024-01-24") & Datetime_CST < as.Date("2024-02-05")) %>%# filter(Label %in% c("sh2", "deep")) %>%
#   ggplot() + geom_point(aes(Datetime_CST, Flux, col = as.character(Label)), alpha = .5) + theme_bw() + 
#   ggtitle("3 months after deep collar installation", subtitle = "From 2024-01-24 to 2024-02-05")
```

``` r
#setwd("G:/Shared drives/Project_Crockett/Temp")
setwd("G:/My Drive/Research/Projects/DC_auto/Data/Biomet")

# read files: 22 & 23 are for Ameriflux and 24 for only biomet
biomet_22_raw <- read.csv("US-CRK_HH_202201010000_202301010000.csv")
biomet_23_raw <- read.csv("US-CRK_HH_202301010000_202401010000.csv")
biomet_24_raw <- read.csv("CRK_biomet_2024.csv")

# unit change function
biomet_unit <- 
  function(df){
      df %>%
      # WATCH OUT: ymd_hm set timezone as UTC as default
      # Use tzone = "Etc/GMT+6" instead of "America/Chicago"
      # the latter generates daylight saving time and NA as default and no way prevending from it
      mutate(TIMESTAMP_START = force_tz(ymd_hm(TIMESTAMP_START), tzone = "Etc/GMT+6"),
             TIMESTAMP_END = force_tz(ymd_hm(TIMESTAMP_END), tzone = "Etc/GMT+6")) %>%
      mutate(across(where(is.numeric), na_if, -9999)) # change -9999 to NA
  }

biomet_22 <- biomet_unit(biomet_22_raw)
biomet_23 <- biomet_unit(biomet_23_raw)

# function: aggregate to hourly 
biomet_hr <- function(df){
  df %>% 
    mutate(Datetime_hr_CST = floor_date(TIMESTAMP_START, unit = "hour")) %>%
    group_by(Datetime_hr_CST) %>%
    summarise(across(CO2_1_1_1:NEE_1_2_1, ~ mean(.x, na.rm = TRUE))) # aggregate to hourly
}

# applying the function
biomet_22_hr <- biomet_hr(biomet_22)
biomet_23_hr <- biomet_hr(biomet_23)


# just for biomet data - timestamp is different from Ameriflux standard
biomet_24 <- 
  biomet_24_raw[-1,] %>%
  mutate(Timestamp_1 = force_tz(ymd_hm(as.character(Timestamp_1)), tzone = "Etc/GMT+6")) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(across(SWC_1_1_1:SWC_1_2_1, ~.*100)) # changing to %

# you can check where NA values were generated
which(is.na(biomet_24$Timestamp_1), arr.ind = T)
```

    ## integer(0)

``` r
biomet_24_hr <- 
  biomet_24 %>%
  mutate(Datetime_hr_CST = floor_date(Timestamp_1, unit = "hour")) %>%
  group_by(Datetime_hr_CST) %>%
  summarise(across(Ta_1_1_1:SWC_1_2_1, ~ mean(.x, na.rm = TRUE))) %>% 
  # changing column names to fit Ameriflux standard
  rename(
    PPFD_IN_1_1_1 = PPFD_1_1_1,
    TS_1_1_1 = Ts_1_1_1,
    TS_1_2_1 = Ts_1_2_1,
    TS_2_1_1 = Ts_2_1_1,
    TS_2_2_1 = Ts_2_2_1,
    TS_2_3_1 = Ts_2_3_1
  )
  
# creating a biomet df from 2022-2024
biomet <- 
  rbind(biomet_22_hr, biomet_23_hr) %>%
  select(Datetime_hr_CST, PPFD_IN_1_1_1, TS_1_1_1, TS_1_2_1, TS_2_1_1, TS_2_2_1, TS_2_3_1, SWC_1_1_1) %>%
  rbind(biomet_24_hr %>% select(Datetime_hr_CST, PPFD_IN_1_1_1, TS_1_1_1, TS_1_2_1, TS_2_1_1, TS_2_2_1, TS_2_3_1, SWC_1_1_1)) %>%
  mutate(across(where(is.numeric), function(x) ifelse(is.nan(x), NA, x)))
```

``` r
# PPFD_IN_1_1_1
ggplot(biomet) + 
  geom_point(aes(Datetime_hr_CST, PPFD_IN_1_1_1)) + 
  theme_bw() + 
  ggtitle("Data covarage - Above canopy PAR") + 
  ylab(expression("Density ("~mu~"mol Photon m"^2~"s"^-1~")")) + 
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/basic%20plot%20for%20tower%20data-1.png)<!-- -->

``` r
# TS_1
ggplot(biomet) + 
  geom_point(aes(Datetime_hr_CST, TS_1_1_1, col = "TS_1 5cm"), alpha = .05) +
  geom_point(aes(Datetime_hr_CST, TS_1_2_1, col = "TS_1 20cm"), alpha = .05) +
  theme_bw() + 
  ggtitle("Data covarage - soil temp at 5 & 20 cm depth") + 
  ylab("Soil temperature (\u00B0C)") +
  scale_color_manual("", values = c("#F8766D", "#00BFC4")) +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/basic%20plot%20for%20tower%20data-2.png)<!-- -->

``` r
# TS_2
# 2023 data until end of Sep 2023

ggplot(biomet) + 
  geom_point(aes(Datetime_hr_CST, TS_2_1_1, col = "TS_2 5 cm"), alpha = .05) +
  geom_point(aes(Datetime_hr_CST, TS_2_2_1, col = "TS_2 10 cm"), alpha = .05) +
  geom_point(aes(Datetime_hr_CST, TS_2_3_1, col = "TS_2 30 cm"), alpha = .05) +
  theme_bw() + 
  ggtitle("Data covarage - soil temp at 5, 10, 30  cm depth") + 
  ylab("Soil temperature (\u00B0C)") +
  scale_color_manual("", values = c("#F8766D", "#00BFC4", "#00BA38")) +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/basic%20plot%20for%20tower%20data-3.png)<!-- -->

``` r
# SWC_1
# 2023 data until end of Sep 2023
ggplot(biomet) + 
  geom_point(aes(Datetime_hr_CST, SWC_1_1_1)) + 
  theme_bw() + 
  ggtitle("Data covarage - Soil moisture") + 
  ylab("Soil moisture (%)") +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/basic%20plot%20for%20tower%20data-4.png)<!-- -->

``` r
# not using in the current version
setwd("G:/My Drive/Research/Projects/DC_auto/Data/Sunlight")
sun_22_raw <- read_excel("CRK_navy_2022.xlsx") 
sun_23_raw <- read_excel("CRK_navy_2023.xlsx")

name_to_num <- data.frame(
  month_num = c(1:12),
  name = unique(sun_22_raw$Month)
)

# clean navy dataset
sun_clean <- 
  function(df){
      df %>%
         left_join(name_to_num, by = c("Month" = "name")) %>%
         mutate(sunrise_hour = as.integer(substring(Sunrise, 1, 1)), 
                sunrise_min = as.integer(substring(Sunrise, 2,3)),
                sunset_hour = as.integer(substring(Sunset, 1, 2)), 
                sunset_min = as.integer(substring(Sunset, 3,4)),
                ) %>% 
         mutate(Date = make_date(Year, month_num, DoM),
                sunrise_timestamp = make_datetime(Year, month_num, DoM, sunrise_hour, sunrise_min),
                sunset_timestamp = make_datetime(Year, month_num, DoM, sunset_hour, sunset_min))
  }

# cleaned sunrise/sunset df
sun_22 <- sun_clean(sun_22_raw)
sun_23 <- sun_clean(sun_23_raw)
```

``` r
setwd("G:/My Drive/Research/Projects/DC_auto/Data/canopy_conductance")

CRK_gs_raw <- read.csv("CRK_canopy_conductance.csv") 

CRK_gs<- 
  CRK_gs_raw %>% 
  mutate(Datetime_hr_CST = force_tz(as_datetime(Datetime_hr_CST), tzone = "Etc/GMT+6"))
```

``` r
# not including sunlight for now
cont_qaqc_hr <- 
  cont_qaqc %>%
  mutate(Datetime_hr_CST = floor_date(Datetime_CST, unit = "hour")) %>% # round it down
  select(c(Datetime_hr_CST, Flux, Collar)) %>%
  pivot_wider(names_from = Collar,
              values_from = Flux,
              values_fn = ~ mean(.x, na.rm = TRUE)) %>% # when having duplicates
  rename(SR1 = sh1, SR2 = sh2, SR3 = sh3, Rh = deep) %>%
  mutate(Ra1 = SR1 - Rh, Ra2 = SR2-Rh, Ra3 = SR3-Rh)
  
# combine 3 dfs
SR_tower <- 
  left_join(biomet, cont_qaqc_hr, by = "Datetime_hr_CST") %>%
  left_join(CRK_gs,by = "Datetime_hr_CST")
  

# write csv
#write.csv(SR_tower, "G:/My Drive/Research/Projects/DC_auto/Data/CRK_SR_tower_combined.csv", row.names = F)



# SR_biomet <- function(SR_start, SR_end, df_sun, df_biomet){
#   cont_qaqc %>% 
#   filter(Datetime_CST > SR_start & Datetime_CST < SR_end) %>%
#   mutate(Datetime_30min_CST = floor_date(Datetime_CST, unit = "30minutes")) %>% # round it down
#   select(c(Datetime_30min_CST, Flux, Label)) %>%
#   pivot_wider(names_from = Label,
#               values_from = c(Flux)) %>%
#   na.omit() %>%
#   rename(SR = shallow, 
#          Rh = deep) %>%
#   mutate(Ra = SR - Rh,
#          Date_CST = as.Date(Datetime_30min_CST)
#          ) %>%
#   left_join(df_sun %>% select(c(Date, sunrise_timestamp, sunset_timestamp)), by = c("Date_CST" = "Date")) %>%
#   left_join(df_biomet, by = c("Datetime_30min_CST" = "TIMESTAMP_START")) %>%
#   mutate(day_night = ifelse(between(Datetime_30min_CST, sunrise_timestamp,sunset_timestamp) == TRUE,"day","night")) 
# }
# 
# 
# SR_biomet_hr <- function(df_SR, SR_start, SR_end, Ra_name, df_biomet){
#   df_SR %>%
#   filter(Datetime_CST > SR_start & Datetime_CST < SR_end) %>%
#   mutate(Datetime_hr_CST = floor_date(Datetime_CST, unit = "hour")) %>% # round it down
#   select(c(Datetime_hr_CST, Flux, Label)) %>%
#   pivot_wider(names_from = Label,
#               values_from = Flux,
#               values_fn = ~ mean(.x, na.rm = TRUE)) %>% # when having duplicates
#   rename(SR = {{Ra_name}}, 
#          Rh = deep) %>%
#   mutate(Ra = SR - Rh) %>%
#   left_join(df_biomet, by = "Datetime_hr_CST")
# }
```

``` r
# Diurnal patterns for SR
cont_qaqc_hr %>% 
    filter(format(as.Date(Datetime_hr_CST),"%Y") == "2023") %>% 
    filter(SR1 < 15) %>%
    mutate(hour =  as_hms(ymd_hms(Datetime_hr_CST))) %>% 
    ggplot(aes(hour, SR1, col = as.factor(month(Datetime_hr_CST)))) + 
    geom_point(alpha = .3) + 
    geom_smooth(alpha = .3, se = F) + 
    theme_bw() + ggtitle("Diurnal SR parttern", subtitle = "2023") + 
    labs(color = "Month") + 
    ylab(expression("SR ("~mu~"mol m"^2~"s"^-1~")")) + xlab("Time of day") + 
    theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/Diurnal%20patterns-1.png)<!-- -->

``` r
# Diurnal patterns for PAR
biomet_23 %>%
    mutate(hour = as_hms(ymd_hms(TIMESTAMP_START))) %>% 
    filter(!is.na(PPFD_IN_1_1_1) & !is.na(hour)) %>%
    ggplot(aes(hour, PPFD_IN_1_1_1, col = as.factor(month(TIMESTAMP_START)))) + 
    geom_point(alpha = .3) + 
    geom_smooth(alpha = .3, se = F) + 
    theme_bw() + ggtitle("Diurnal PAR parttern", subtitle = "2023") + 
    labs(color = "Month") + 
    ylab(expression("Density ("~mu~"mol Photon m"^2~"s"^-1~")")) + xlab("Time of day") + 
    theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/Diurnal%20patterns-2.png)<!-- -->

``` r
# Diurnal patterns for TS_1_1_1
biomet_23 %>%
    mutate(hour =  as_hms(ymd_hms(TIMESTAMP_START))) %>% 
    filter(!is.na(TS_1_1_1) & !is.na(hour)) %>%
    ggplot(aes(hour, TS_1_1_1, col = as.factor(month(TIMESTAMP_START)))) + 
    geom_point(alpha = .3) + 
    geom_smooth(alpha = .3, se = F) + 
    theme_bw() + ggtitle("Diurnal Ts parttern at the depth of 5 cm ", subtitle = "2023") + 
    labs(color = "Month") +
    ylab("Soil temperature (\u00B0C)") + xlab("Time of day") + 
    theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/Diurnal%20patterns-3.png)<!-- -->

``` r
# Diurnal patterns for SWC_1_1_1
biomet_23 %>%
    mutate(hour =  as_hms(ymd_hms(TIMESTAMP_START))) %>% 
    filter(!is.na(SWC_1_1_1) & !is.na(hour)) %>%
    ggplot(aes(hour, SWC_1_1_1, col = as.factor(month(TIMESTAMP_START)))) + 
    geom_point(alpha = .3) + 
    geom_smooth(alpha = .3, se = F) + 
    theme_bw() + ggtitle("Diurnal SWC parttern at the depth of 5 cm ", subtitle = "2023") + 
    labs(color = "Month") +
    ylab("Soil moisture (%)") + xlab("Time of day") + 
    theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, face = "bold"))
```

![](CRK_cont_SR_qaqc_files/figure-gfm/Diurnal%20patterns-4.png)<!-- -->
