# this file requires an Mplus installation and assumes there
# you have the folders:
# ~\data
# ~\results
# ~\mplus\bootstraps


# comparing full SOI results with censored --------------------------------
  
library(tidyverse)
library(rsample)
library(MplusAutomation)
source(here::here("script", "helper00-functions.R"))


# read in data

racine <- readRDS(here::here("data", "racine_binary.rds"))

# selecting sample of people from SOI long
# either convert to wide and sample rows or nest and sample
# rows

set.seed(nchar("I love to bootstrap") ^ 4)

# bootstrapping -----------------------------------------------------------

racine_boot <- bootstraps(racine, times = 25)

aggregate_boots <- function(df){
  
  df <- rsample::analysis(df)
  
  df %>% 
    group_by_at(vars(y06:y22)) %>% 
    summarise(freq = n()) %>% 
    ungroup()
}

racine_boot <- 
racine_boot %>% 
  mutate(boot_data = map(splits, aggregate_boots))


mplus_lcgm_probit <- function(df, k_classes, starts_1, starts_2){
  # from Mplus users guide example 8.9
  lcgm <- mplusObject(
    TITLE = glue::glue("LCGM;"),
    VARIABLE = glue::glue("USEVARIABLES ARE y06-y22 freq;
                          CLASSES = c ({k_classes});
                          CATEGORICAL ARE y06-y22;
                          FREQWEIGHT = freq;"),
    ANALYSIS = glue::glue("
                          TYPE = MIXTURE;
                          STARTS = {starts_1} {starts_2};
                          PROCESSORS = 3;
                          ESTIMATOR = ML; 
                          LINK = PROBIT;"),
    MODEL = "
    %OVERALL%
    i s | y06@0.0 y07@0.1 y08@0.2 y09@0.3 y10@0.4 y11@0.5
    y12@0.6 y13@0.7 y14@0.8 y15@0.9 y16@1.0 y17@1.1 y18@1.2
    y19@1.3 y20@1.4 y21@1.5 y22@1.6;",
    OUTPUT = "TECH1 RESIDUAL;",
    PLOT = " ",
    rdata = df)
}


# mplus calls -------------------------------------------------------------



racine_boot <- 
racine_boot %>% 
  mutate(
    k_2 = pmap(list(df = boot_data,
                              k_classes = 2,
                              starts_1 = 25,
                              starts_2 = 5), mplus_lcgm_probit),
    k_3 = pmap(list(df = boot_data,
                    k_classes = 3,
                    starts_1 = 25,
                    starts_2 = 5), mplus_lcgm_probit),
    k_4 = pmap(list(df = boot_data,
                    k_classes = 4,
                    starts_1 = 40,
                    starts_2 = 5), mplus_lcgm_probit),
    k_5 = pmap(list(df = boot_data,
                    k_classes = 5,
                    starts_1 = 50,
                    starts_2 = 8), mplus_lcgm_probit),
    k_6 = pmap(list(df = boot_data,
                    k_classes = 6,
                    starts_1 = 60,
                    starts_2 = 8), mplus_lcgm_probit)
  )

racine_boot_long <- 
racine_boot %>% 
  select(2:8) %>% 
  gather(k, model_call, 3:7)

racine_boot_long <- 
racine_boot_long %>% 
  mutate(results = 
    pmap(list(
      model_call,
      modelout = here::here("mplus", "bootstraps", glue::glue("{k}-boots.inp")),
      run = 1L,
      hash = FALSE),
      mplusModeler)
  )

saveRDS(racine_boot_long,
        here::here("results", "racine_boot_long_binary.rds"))

racine_boot_long %>% 
  select(1, k, results) %>% 
  mutate(k = str_sub(k, 3, 3)) %>% 
  mutate(trajs = map2(results, k, export_trajs)) %>% 
  unnest(trajs) %>% 
  filter(measure == "model_estimated" & category == "2") %>% 
  ggplot(aes(x = param, y = est, group = interaction(ordered_class, id), colour = ordered_class)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ k)


racine %>% 
  count(y10)
