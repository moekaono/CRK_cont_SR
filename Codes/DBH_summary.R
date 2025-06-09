library(googlesheets4)
library(tidyr)
library(dplyr)

dbh_raw <- 
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1UdlQrsyg1of3YkNBvrIIT0OxghDJ_-MVQVi_0HSGCfA/edit?gid=0#gid=0", 
    skip = 1,
    range = c("A2:O423"),
    col_types = "cnnnnnnncncccnc"
  )
head(dbh_raw)
tail(dbh_raw)

dbh <-
  dbh_raw %>% 
  select(c(ID:DBH_2025, Plot)) %>% 
  rename(
    DBH_2021 = `2021_DBH (in)`,
    DBH_2022 = `2022_DBH (in)`,
    DBH_2023 = `2023_DBH(inch)`,
    DBH_2024 = `2024_DBH(inch)`,
  )


dbh %>%
  group_by(Plot) %>%
  summarise(
    across(
      starts_with("DBH_"),
      list(
        mean = ~ mean(as.numeric(.), na.rm = TRUE),
        count = ~ sum(!is.na(.))
      ),
      .names = "{.col}_{.fn}"
    )
  )
