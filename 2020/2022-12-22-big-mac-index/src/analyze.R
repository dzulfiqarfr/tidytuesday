dirYear <- "2020"
dirProject <- "2022-12-22-big-mac-index"

here::i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(readxl)
library(modelr)
library(tidytuesdayR)


# Data ----

## Big Mac Index ----

tidyTuesday <- tt_load("2020-12-22")

bigMacRaw <- tidyTuesday %>% .[["big-mac"]]

bigMacIndonesia <- bigMacRaw %>% 
  filter(iso_a3 == "IDN") %>% 
  select(date, iso_a3, currency_code, local_price, usd_raw, dollar_ex)

bigMacUS <- bigMacRaw %>% 
  filter(iso_a3 == "USA") %>% 
  select(date, local_price) %>% 
  rename(us_local_price = local_price)

rupiahImplied <- bigMacIndonesia %>% 
  left_join(bigMacUS, by = "date") %>% 
  mutate(implied_exchange_rate = local_price / us_local_price) %>% 
  select(-c(local_price, us_local_price))

rupiahImplied %>% 
  write_csv(here(dirYear, dirProject, "result", "idr-usd-valuation.csv"))


## Big Mac prices by income group ----

incomeGroupRaw <- read_excel(
  here(dirYear, dirProject, "data", "world-bank-classification-raw.xls"),
  sheet = "Groups"
)

incomeGroupEuro <- tribble(
  ~CountryCode, ~GroupCode, ~GroupName,
  "EUZ", "HIC", "High income"
)

incomeGroupSubset <- incomeGroupRaw %>% 
  filter(GroupCode %in% c("LIC", "LMC", "UMC", "HIC")) %>% 
  select(CountryCode, GroupCode, GroupName)
  
incomeGroupAdded <- incomeGroupSubset %>% bind_rows(incomeGroupEuro)

bigMacLatest <- bigMacRaw %>% 
  filter(date == as.Date("2020-07-01")) %>% 
  select(date, iso_a3, dollar_price)
  
bigMacIncomeGroup <- bigMacLatest %>% 
  left_join(incomeGroupAdded, by = c("iso_a3" = "CountryCode"))

bigMacIncomeGroup %>% 
  write_csv(here(dirYear, dirProject, "result", "big-mac-income-group.csv"))


## Big Mac prices and GDP per capita ----

bigMacGDP <- bigMacRaw %>% 
  filter(!is.na(gdp_dollar), date == as.Date("2020-07-01")) %>% 
  select(date, iso_a3, name, dollar_price, gdp_dollar)

model <- lm(dollar_price ~ gdp_dollar, data = bigMacGDP)

summary(model)

bigMacGDPpred <- bigMacGDP %>% add_predictions(model)

bigMacGDPpred %>% 
  write_csv(here(dirYear, dirProject, "result", "big-mac-gdp.csv"))

### Check rupiah's value against the US dollar after adjusting for GDP
rupiahValueGDPadj <- bigMacGDPpred %>% 
  filter(iso_a3 %in% c("IDN", "USA")) %>% 
  mutate(
    value_gdp_adjusted = (dollar_price[1] / pred[1]) / (dollar_price[2] / pred[2]) - 1,
  )

rupiahValueGDPadj %>% 
  write_csv(
    here(
      dirYear, 
      dirProject, 
      "result",
      "idr-usd-valuation-gdp-adjusted.csv"
    )
  )

