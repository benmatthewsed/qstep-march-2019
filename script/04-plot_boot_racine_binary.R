library(tidyverse)
source(here::here("script", "helper00-functions.R"))


racine_boot <- readRDS(
        here::here("results", "racine_boot_long_binary.rds"))

racine <- readRDS(
       here::here("results", "racine_long_binary.rds"))


racine_boot_results <- 
racine_boot %>% 
  mutate(k = str_sub(k, 3, 3)) %>% 
  mutate(trajectories = map2(results, k, export_trajs)) %>% 
  unnest(trajectories) %>% 
  mutate(data = "censored") %>% 
  filter(measure == "model_estimated" & category == "2") %>% 
  mutate(age = as.numeric(str_sub(param, 2, 3)))
  

racine_results <- 
  racine %>% 
  mutate(k = str_sub(k, 3, 3)) %>% 
  mutate(trajectories = map2(results, k, export_trajs)) %>% 
  unnest(trajectories) %>% 
  mutate(data = "censored") %>% 
  filter(measure == "model_estimated" & category == "2") %>% 
  mutate(age = as.numeric(str_sub(param, 2, 3)))

# iterating plots

# nest dataset

nest_racine <- 
racine_results %>% 
  group_by(k) %>% 
  nest()


nest_racine_boot <- 
  racine_boot_results %>% 
  group_by(k) %>% 
  nest()

# plot fn


plot_racine <- function(df1, k_val, alpha_val){

df1 %>% 
  ggplot(aes(x = age, y = est)) +
  facet_wrap(vars(k)) +
  geom_line(data = filter(racine_boot_results, k == k_val),
            aes(group = interaction(ordered_class, id), colour = ordered_class),
            alpha = alpha_val) +
  geom_line(aes(group = ordered_class, colour = ordered_class), size = 1) +
  ylim(c(0, 1)) +
  labs(x = "Age",
       y = "Estimated probability of misdemeanor or felony",
       colour = "Latent class")

}


boot_plots <- pmap(list(nest_racine$data, nest_racine$k, 0.3), plot_racine)

pwalk(
  list(
    file = here::here("figures", glue::glue("racine_plots_k0{seq(2,6)}_boot.png")),
    plot = boot_plots,
    type = "cairo-png"
  ),
  ggsave
)


# just regular data (hack - set alpha to 0)



plots <- pmap(list(nest_racine$data, nest_racine$k, 0), plot_racine)

pwalk(
  list(
    file = here::here("figures", glue::glue("racine_plots_k0{seq(2,6)}.png")),
    plot = plots,
    type = "cairo-png"
  ),
  ggsave
)
