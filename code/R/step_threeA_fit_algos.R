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
  bind_rows()

write_csv(clemson_res, here::here("results/steps_clemson.csv"))
write_csv(ox_res, here::here("results/steps_ox.csv"))




