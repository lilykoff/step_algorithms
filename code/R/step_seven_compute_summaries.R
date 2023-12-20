library(tidyverse)

# clemson = readr::read_csv(here::here("results", "clemson_joined_steps.csv"))

clemson = readr::read_csv(here::here("results", "steps_clemson.csv")) %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x)))


ox_25 = readr::read_csv(here::here("results", "steps_ox.csv")) %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  filter(sample_rate == 25)

ox_100 = readr::read_csv(here::here("results", "steps_ox.csv")) %>%
  mutate(across(starts_with("steps"), ~ ifelse(is.na(.x), 0, .x))) %>%
  filter(sample_rate == 100)

# correlations
cor.mat_clem =
  clemson %>%
  select(starts_with("steps")) %>%
  cor()

cor.mat_ox25 =
  ox_25 %>%
  select(starts_with("steps")) %>%
  cor()

cor.mat_ox100 =
  ox_100 %>%
  select(starts_with("steps")) %>%
  cor()

ggcorrplot::ggcorrplot(cor.mat_clem, type = "lower", lab = TRUE)
ggcorrplot::ggcorrplot(cor.mat_ox25, type = "lower", lab = TRUE)
ggcorrplot::ggcorrplot(cor.mat_ox100, type = "lower", lab = TRUE)



ox %>%
  select(starts_with("steps")) %>%
  cor()
# summaries to compute
# MAPE
# MSE
# Correlation
# bland altman
# accuracy/f1
# plots

get_summaries = function(data){

  }


# tomorrow:: stepcount
