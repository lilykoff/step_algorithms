library(tidyverse)
library(purrr)
source(here::here("code/R/resample_utils.R"))
options(digits.secs = 3)
sample_rate_target = 30 # we want to resample all data to 15 Hz


# clemson data
ox = readr::read_csv(here::here("data/processed/ox_data.csv.gz"))
ox_list = split(ox, list(ox$id_subject, ox$sample_rate))


ox_resampled =
  purrr::map(
    .x = ox_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear")
      sub = x$id_subject[1]
      samp_rate = x$sample_rate[1]
      fname = paste0("acc_", sub, "_", samp_rate, "to30Hz", ".csv.gz")
      readr::write_csv(resamp, here::here("data/raw_resampled", fname))
      resamp %>%
        mutate(id_subject = sub,
               sample_rate = samp_rate,
               id_study = "oxwalk") %>%
        rename(tm_dttm = HEADER_TIMESTAMP)
    }
  ) %>%
  bind_rows()

readr::write_csv(ox_resampled, here::here("data/processed/ox_data_resampled.csv.gz"))

rm(ox_list)
rm(ox_resampled)

clem = readr::read_csv(here::here("data/processed/clemson_ped.csv.gz"))
clem_list = split(clem, list(clem$id_subject, clem$cat_activity))


clem_resampled =
  purrr::map(
    .x = clem_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear")
      sub = x$id_subject[1]
      samp_rate = x$sample_rate[1]
      activity = x$cat_activity[1]
      act = sub(".*_", "", activity)
      fname = paste0("acc_", sub, "_", act, "_", "15to30Hz", ".csv.gz")
      readr::write_csv(resamp, here::here("data/raw_resampled", fname))
      resamp %>%
        mutate(id_subject = sub,
               cat_activity = activity,
               sample_rate = samp_rate,
               id_study = "clemson_ped") %>%
        rename(tm_dttm = HEADER_TIMESTAMP)
    }
  ) %>%
  bind_rows()

readr::write_csv(clem_resampled, here::here("data/processed/clemson_ped_resampled.csv.gz"))



