library(tidyverse)
library(haven)
library(here)
library(janitor)

# Data were downloaded from https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/8163/datadocumentation#
# You need to register with ICPSR to download.
# Data are DS8 Age-by-Age Data, 1955 ACII+SPSS Setup.
# This script assumes you've run the SPSS file to set up 
# 08163-0003-Data.sav from the ASCII file, and that this is stored in a
# directory \data under the current working directory.

racine <- read_spss(here("data", "08163-0003-Data.sav"))

names(racine) <- map2(racine, "label", attr)

racine <- clean_names(racine)

# include only adult or juvenile misdemeanour

racine_age <- 
racine %>% 
  select(2, contains("subj_age")) %>% 
  gather(junk, age, 2:84) %>% 
  group_by(subject_id) %>% 
  mutate(conv_n = row_number()) %>% 
  ungroup() %>% 
  mutate(sub_conv_n = paste0(subject_id, "-", conv_n)) %>% 
  select(-junk)
  
racine_seriousness <- 
racine %>% 
  select(2, contains("seriousness")) %>% 
  gather(junk, seriousness, 2:84) %>% 
  group_by(subject_id) %>% 
  mutate(conv_n = row_number()) %>% 
  ungroup() %>% 
  mutate(sub_conv_n = paste0(subject_id, "-", conv_n)) %>% 
  select(-junk)

racine_seriousness <- 
racine_seriousness %>% 
  mutate(seriousness = case_when(
    seriousness == 0 ~ "INAP",
    seriousness == 1 ~ "juvenile non-adult",
    seriousness == 2 ~ "juvenile misdemeanor",
    seriousness == 3 ~ "juvenile felony",
    seriousness == 4 ~ "adult misdemeanor",
    seriousness == 5 ~ "adult felony",
    seriousness == 6 ~ "non-adult - ages 18-20",
    seriousness == 7 ~ "misdemeanor - ages 18-20",
    seriousness == 8 ~ "felony - ages 18-20",
    TRUE ~ NA_character_
  ))

racine_combined <- left_join(racine_age, racine_seriousness)

racine_combined %>% 
  count(seriousness)

racine_combined <- 
racine_combined %>% 
  filter(str_detect(seriousness, "misdemeanor") |
           str_detect(seriousness, "felony"))

racine_convs <- 
racine_combined %>% 
  select(1:2) %>% 
  arrange(subject_id) %>% 
  filter(age != 0) %>% 
  count(subject_id, age) %>% 
  spread(age, n, fill = 0) %>% 
  gather(age, n, 2:18) %>% 
  mutate(age = str_pad(age, 2, pad = "0"),
         age = paste0("y", age)) %>% 
  spread(age, n)

subjs <- 
racine %>% 
  select(subject_id)

racine_all <- left_join(subjs, racine_convs)

racine_count <- 
racine_all %>% 
  gather(age, convs, 2:18) %>% 
  mutate(convs = if_else(is.na(convs), 0, convs)) %>% 
  spread(age, convs)

saveRDS(racine_count,
        here::here("data", "racine_count.rds"))


# aggregating count
racine_count_freq <- 
racine_count %>% 
  group_by_at(vars(2:18)) %>% 
  summarise(freq = n())

# binary
racine_binary <- 
racine_count %>% 
  gather(age, n, 2:18) %>% 
  mutate(n = if_else(n > 0, 1, n)) %>% 
  spread(age, n) 

saveRDS(racine_binary,
        here::here("data", "racine_binary.rds"))

racine_binary_freq <- 
racine_count %>% 
  gather(age, n, 2:18) %>% 
  mutate(n = if_else(n > 0, 1, n)) %>% 
  spread(age, n) %>% 
  group_by_at(vars(2:18)) %>% 
  summarise(freq = n()) %>% 
  ungroup()

saveRDS(racine_binary_freq,
        here::here("data", "racine_binary_freq.rds"))
