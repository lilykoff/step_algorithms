library(tidyverse)
options(digits.secs = 3)
source(here::here("code/R/utils.R"))
files = list.files(here::here("results/actilife/original"), full.names = TRUE,
                   recursive = TRUE)

clem = readr::read_csv("results/adept_oak_vs_sdt/steps_aovs_clemson.csv")
ox = readr::read_csv("results/adept_oak_vs_sdt/steps_aovs_ox.csv")


clemson_csvs = files[grep("egular", files)]
ox_csvs = files[-grep("egular", files)]

processed_acti_clemson =
  map(.x = clemson_csvs,
      .f = process_acti_results,
      clem = clem,
      ox = ox) %>%
  bind_rows()

processed_acti_ox =
  map(.x = ox_csvs,
      .f = process_acti_results,
      clem = clem,
      ox = ox) %>%
  bind_rows()


write_csv(processed_acti_clemson, here::here("results", "actilife", "processed", "steps_acti_clemson.csv"))
write_csv(processed_acti_ox, here::here("results", "actilife", "processed", "steps_acti_ox.csv"))

# now do resampled

files = list.files(here::here("results/actilife/resampled"), full.names = TRUE,
                   recursive = TRUE)
clemson_csvs = files[grep("egular", files)]
ox_csvs = files[-grep("egular", files)]

processed_acti_clemson =
  map(.x = clemson_csvs,
      .f = process_acti_results_resampled,
      clem = clem,
      ox = ox) %>%
  bind_rows()

processed_acti_ox =
  map(.x = ox_csvs,
      .f = process_acti_results_resampled,
      clem = clem,
      ox = ox) %>%
  bind_rows()

write_csv(processed_acti_clemson, here::here("results", "actilife", "processed", "steps_acti_clemson_resampled.csv"))
write_csv(processed_acti_ox, here::here("results", "actilife", "processed", "steps_acti_ox_resampled.csv"))
