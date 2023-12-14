library(tidyverse)

clemson_res = readr::read_csv(here::here("results/steps_clemson.csv"))
ox_res = readr::read_csv(here::here("results/steps_ox.csv"))

stepcount_clemson = readr::read_csv(here::here("results/stepcount_all_clemson.csv"))
stepcount_ox = readr::read_csv(here::here("results/stepcount_all_ox.csv"))

# unique(clemson_res$id_subject)
# unique(stepcount_clemson$id_subject)
# unique(stepcount_clemson$cat_activity)
# unique(stepcount_clemson$sample_rate)
# unique(clemson_res$cat_activity)
#
# unique(stepcount_ox$id_subject)
# unique(stepcount_ox$sample_rate)
# unique(ox_res$id_subject)
# unique(ox_res$id_study)

# join clemson data
stepcount_clemson = stepcount_clemson %>%
  mutate(time_floored = lubridate::floor_date(time, unit = "10 seconds"))

clemson_10sec =
  clemson_res %>%
  mutate(time_10s = lubridate::floor_date(time, unit = "10 seconds")) %>%
  group_by(id_subject,
           id_study,
           cat_activity,
           sample_rate,
           time_10s) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE)))

clemson_joined = full_join(clemson_10sec, stepcount_clemson,
                           by = c("time_10s" = "time_floored",
                                  "id_subject", "id_study", "cat_activity", "sample_rate")) %>%
  select(-time)

# join oxwalk data

stepcount_ox =
  stepcount_ox %>%
  mutate(time_floored = lubridate::floor_date(time, unit = "10 seconds"))

oxwalk_10sec =
  ox_res %>%
  mutate(time_10s = lubridate::floor_date(time, unit = "10 seconds")) %>%
  group_by(id_subject,
           id_study,
           cat_activity,
           sample_rate,
           time_10s) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup()

ox_joined = full_join(oxwalk_10sec, stepcount_ox,
                           by = c("time_10s" = "time_floored",
                                  "id_subject", "id_study", "cat_activity", "sample_rate")) %>%
  select(-time)


write_csv(clemson_joined, here::here("results/clemson_joined_steps.csv"))
write_csv(ox_joined, here::here("results/ox_joined_steps.csv"))
