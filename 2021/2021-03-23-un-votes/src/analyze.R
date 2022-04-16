dirYear <- "2021"
dirProject <- "2021-03-23-un-votes"

here::i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(lubridate)
library(tidytext)
library(tidytuesdayR)
library(countrycode)


# Data ----

tidyTuesday <- tt_load("2021-03-23")

tidyTuesday %>% str()

voteRaw <- tidyTuesday[["unvotes"]] %>% 
  left_join(tidyTuesday[["roll_calls"]], by = "rcid") %>% 
  left_join(tidyTuesday[["issues"]], by = "rcid")

# Subset votes related to Israel-Palestine conflict
voteMidEast <- voteRaw %>% filter(short_name == "me")

voteIndonesia <- voteMidEast %>% 
  filter(country == "Indonesia") %>% 
  select(rcid, vote) %>% 
  rename(vote_idn = vote)

voteMidEastIdn <- voteMidEast %>% 
  filter(country != "Indonesia") %>% 
  left_join(voteIndonesia) %>% 
  mutate(year = year(date)) %>% 
  filter(year >= 1950) %>% # Year when Indonesia joined the UN
  select(rcid, unres, country, country_code, date, year, short, vote, vote_idn)

# Assign voting coincidence between Indoneis and other member states based
# on the methods used by the United States
vc <- voteMidEastIdn %>% 
  mutate(
    voting_coincidence = case_when(
      vote == vote_idn ~ "same",
      vote == "abstain" & vote_idn != "abstain" ~ "partial",
      vote != "abstain" & vote_idn == "abstain" ~ "partial",
      vote != vote_idn ~ "opposite",
      TRUE ~ vote_idn
    ),
    voting_coincidence_score = case_when(
      voting_coincidence == "same" ~ 1,
      voting_coincidence == "partial" ~ 0.5,
      voting_coincidence == "opposite" ~ 0,
      is.na(voting_coincidence) ~ 0,
      is.na(vote_idn) ~ 0
    )
  )

vcOverall <- vc %>% 
  group_by(year) %>% 
  mutate(vote_counts = n()) %>% 
  filter(country != "Indonesia") %>% 
  mutate(
    voting_coincidence_total = sum(voting_coincidence_score, na.rm = T),
    voting_coincidence_share = voting_coincidence_total / vote_counts * 100
  ) %>% 
  ungroup() %>% 
  select(year, voting_coincidence_share) %>% 
  distinct(year, .keep_all = T)

vcOverall %>% 
  write_csv(
    here(
      dirYear,
      dirProject,
      "result", 
      "voting-coincidence-overall.csv"
    )
  )

vcCountry <- vc %>% 
  group_by(country, year) %>% 
  mutate(vote_counts = n()) %>% 
  filter(country != "Indonesia") %>% 
  mutate(
    voting_coincidence_share = sum(voting_coincidence_score) / n() * 100
  ) %>%
  distinct(year, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(
    region = countrycode(country_code, "iso2c", "region"),
    region = case_when(
      country_code == "YU" ~ "Europe & Central Asia",
      country == "Namibia" ~ "Sub-Saharan Africa",
      TRUE ~ region
    )
  ) %>% 
  select(country, country_code, region, year, voting_coincidence_share) 

vcCountry %>% 
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "voting-coincidence-country.csv"
    )
  )

vcResolution <- vc %>% 
  group_by(rcid) %>% 
  mutate(vote_counts = n()) %>% 
  filter(country != "Indonesia") %>% 
  mutate(
    voting_coincidence_share = sum(voting_coincidence_score) / vote_counts * 100
  ) %>% 
  ungroup() %>% 
  select(rcid, unres, short, date, year, voting_coincidence_share) %>% 
  distinct(rcid, .keep_all = T)

vcResolution %>% 
  write_csv(
    here(
      dirYear,
      dirProject,
      "result", 
      "voting-coincidence-resolution.csv"
    )
  )

vcResolutionLackSupport <- vcResolution %>% 
  filter(voting_coincidence_share > 0) %>% 
  filter(voting_coincidence_share < 50)

vcResolutionLackSupport %>% 
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "voting-coincidence-lack-support.csv"
    )
  )
