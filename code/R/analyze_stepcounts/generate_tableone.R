library(tidyverse)

# generate table one
clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
# just use 100 hz data from oxwalk for this part
oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz")) %>%
  filter(sample_rate == 100)
# remove running from MAREA
marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz"))


# generate a table 1 with study descriptions
marea %>%
  group_by(cat_activity_large) %>%
  summarize(subs = length(unique(id_subject)))

marea %>%
  group_by(cat_activity_large, id_subject) %>%
  summarize(n = n()/60) %>%
  group_by(cat_activity_large) %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))
marea %>%
  group_by(cat_activity_large, id_subject) %>%
  summarize(n = sum(steps_truth)) %>%
  group_by(cat_activity_large) %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))

clemson %>%
  group_by(cat_activity, id_subject) %>%
  summarize(n = n()/60) %>%
  group_by(cat_activity) %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))


clemson %>%
  group_by(cat_activity, id_subject) %>%
  summarize(n = sum(steps_truth)) %>%
  group_by(cat_activity) %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))
oxwalk %>%
  group_by( id_subject) %>%
  summarize(n = n()/60) %>%
  ungroup() %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))
oxwalk %>%
  group_by(id_subject) %>%
  summarize(n = sum(steps_truth)) %>%
  ungroup() %>%
  summarize(across(n, list(mean = ~ mean(.x),
                           sd = ~ sd(.x))))
