# goal of this file: to get 2 files - for clemson and ox
# columns: id_subject, time, id_study, cat_activity, steps_truth, steps_sc, steps_actilife_,...steps_sc_resampled, etc


library(tidyverse)
clemson_4 = readr::read_csv("results/adept_oak_vs_sdt/steps_aovs_clemson.csv")
clemson_4_resampled = readr::read_csv("results/adept_oak_vs_sdt/steps_aovs_clemson_resampled.csv")

colnames = names(clemson_4_resampled)
colnames[grep("steps", colnames)] =
  paste(colnames[grep("steps", colnames)], "_30", sep = "")
colnames(clemson_4_resampled) = colnames


clemson_acti = readr::read_csv("results/actilife/processed/steps_acti_clemson.csv")
clemson_acti_resampled = readr::read_csv("results/actilife/processed/steps_acti_clemson_resampled.csv")
colnames(clemson_acti_resampled)[grep("steps", colnames(clemson_acti_resampled))] = "steps_acti_30"

clemson_acti_all = left_join(clemson_acti_resampled %>%
                               select(-sample_rate) %>%
                               rename(sample_rate = sample_rate_old),
                             clemson_acti)

clemson_4_all = left_join(clemson_4, clemson_4_resampled, by = c("id_subject", "id_study", "time", "cat_activity")) %>%
  mutate(sample_rate = sample_rate_old) %>%
  select(-c(sample_rate.x, sample_rate.y, sample_rate_old))


clemson_all = left_join(clemson_4_all, clemson_acti_all)

readr::write_csv(clemson_all, here::here("results", "clemson_steps_1sec.csv"))


ox_4 = readr::read_csv("results/adept_oak_vs_sdt/steps_aovs_ox.csv")
ox_4_resampled = readr::read_csv("results/adept_oak_vs_sdt/steps_aovs_ox_resampled.csv")

colnames = names(ox_4_resampled)
colnames[grep("steps", colnames)] =
  paste(colnames[grep("steps", colnames)], "_30", sep = "")
colnames(ox_4_resampled) = colnames


ox_acti = readr::read_csv("results/actilife/processed/steps_acti_ox.csv")
ox_acti_resampled = readr::read_csv("results/actilife/processed/steps_acti_ox_resampled.csv")
colnames(ox_acti_resampled)[grep("steps", colnames(ox_acti_resampled))] = "steps_acti_30"

ox_acti_all = left_join(ox_acti_resampled %>%
                               select(-sample_rate) %>%
                               rename(sample_rate = sample_rate_old),
                             ox_acti)

ox_4_all = left_join(ox_4, ox_4_resampled %>%
                       select(-sample_rate) %>%
                       rename(sample_rate = sample_rate_old))


ox_all = left_join(ox_4_all, ox_acti_all)

readr::write_csv(ox_all, here::here("results", "ox_steps_1sec.csv"))


# do 10s summaries

clemson_1sec = readr::read_csv(here::here("results", "clemson_steps_1sec.csv"))
clemson_sc = readr::read_csv(here::here("results/stepcount", "processed", "steps_sc_clemson.csv"))
clemson_sc_resamp = readr::read_csv(here::here("results/stepcount", "processed", "steps_sc_clemson_resampled.csv"))

colnames(clemson_sc_resamp)[2] = "steps_sc_30"

clemson_sc_all =
  left_join(clemson_sc, clemson_sc_resamp %>%
  select(-sample_rate) %>%
  rename(sample_rate = sample_rate_old))

clemson_10sec = clemson_1sec %>%
  group_by(time = floor_date(time, unit = "10 seconds"),
           id_study, id_subject, cat_activity, sample_rate) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE)))


clemson_all_10sec = left_join(clemson_10sec, clemson_sc_all)


readr::write_csv(clemson_all_10sec, here::here("results", "clemson_steps_10sec.csv"))


ox_1sec = readr::read_csv(here::here("results", "ox_steps_1sec.csv"))
ox_sc = readr::read_csv(here::here("results/stepcount", "processed", "steps_sc_ox.csv")) %>%
  mutate(time = floor_date(time, unit = "1 seconds"))
ox_sc_resamp = readr::read_csv(here::here("results/stepcount", "processed", "steps_sc_ox_resampled.csv")) %>%
  mutate(time = floor_date(time, unit = "1 seconds"))

colnames(ox_sc_resamp)[2] = "steps_sc_30"

ox_sc_all =
  left_join(ox_sc, ox_sc_resamp %>%
              select(-sample_rate) %>%
              rename(sample_rate = sample_rate_old)) %>%
  mutate(time = floor_date(time, unit = "10 seconds"))

ox_10sec = ox_1sec %>%
  group_by(time = floor_date(time, unit = "10 seconds"),
           id_study, id_subject, cat_activity, sample_rate) %>%
  summarize(across(starts_with("steps"), ~ sum(.x, na.rm = TRUE)))


ox_all_10sec = left_join(ox_10sec, ox_sc_all)

readr::write_csv(ox_all_10sec, here::here("results", "ox_steps_10sec.csv"))



