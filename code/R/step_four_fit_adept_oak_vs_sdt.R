library(walking)
library(tidyverse)
library(readr)
options(digits.secs = 3)
clemson_ped = readr::read_csv(here::here("data/processed/clemson_ped.csv.gz"))
ox = readr::read_csv(here::here("data/processed/ox_data.csv.gz"))

source(here::here("code/R/utils.R"))


clemson_list = split(clemson_ped, f = list(clemson_ped$id_subject,
                                           clemson_ped$cat_activity))
ox_list = split(ox, f = list(ox$id_study,
                             ox$id_subject,
                             ox$sample_rate))
clemson_res =
  map(.x = clemson_list,
    .f = fit_all_algorithms) %>%
  bind_rows()

ox_res = map(.x = ox_list,
             .f = fit_all_algorithms) %>%
  bind_rows() %>%

write_csv(clemson_res, here::here("results/adept_oak_vs_sdt/steps_aovs_clemson.csv"))
write_csv(ox_res, here::here("results/adept_oak_vs_sdt/steps_aovs_ox.csv"))


rm(list = ls())
clemson_ped = readr::read_csv(here::here("data/processed/clemson_ped_resampled.csv.gz"))
ox = readr::read_csv(here::here("data/processed/ox_data_resampled.csv.gz"))

# fix sample rates
clemson_ped =
  clemson_ped %>%
  mutate(
    sample_rate_old = sample_rate,
    sample_rate = 30
  )

ox =
  ox %>%
  mutate(
    sample_rate_old = sample_rate,
    sample_rate = 30
  )

clemson_list = split(clemson_ped, f = list(clemson_ped$id_subject,
                                           clemson_ped$cat_activity))
ox_list = split(ox, f = list(ox$id_study,
                             ox$id_subject,
                             ox$sample_rate_old))
clemson_res =
  map(.x = clemson_list,
      .f = fit_all_algorithms_resampled) %>%
  bind_rows()

ox_res = map(.x = ox_list,
             .f = fit_all_algorithms_resampled) %>%
  bind_rows()

# write_csv(clemson_res, here::here("results/adept_oak_vs_sdt/steps_aovs_clemson_resampled.csv"))
# write_csv(ox_res, here::here("results/adept_oak_vs_sdt/steps_aovs_ox_resampled.csv"))


# add steps_truth to resampled data

clem_orig = readr::read_csv(here::here("results/adept_oak_vs_sdt/steps_aovs_clemson.csv"))
steps_truth = clem_orig %>%
  select(time, steps_truth, id_subject, cat_activity)
# clem_resamp = readr::read_csv(here::here("results/adept_oak_vs_sdt/steps_aovs_clemson_resampled.csv"))

clem_resamp_truth =
  clemson_res %>%
  left_join(steps_truth)

write_csv(clem_resamp_truth, here::here("results/adept_oak_vs_sdt/steps_aovs_clemson_resampled.csv"))



ox_orig = readr::read_csv(here::here("results/adept_oak_vs_sdt/steps_aovs_ox.csv"))
steps_truth = ox_orig %>%
  select(time, steps_truth, id_subject, sample_rate_old = sample_rate)
# clem_resamp = readr::read_csv(here::here("results/adept_oak_vs_sdt/steps_aovs_clemson_resampled.csv"))

ox_resamp_truth =
  ox_res %>%
  left_join(steps_truth)

write_csv(ox_resamp_truth, here::here("results/adept_oak_vs_sdt/steps_aovs_ox_resampled.csv"))

