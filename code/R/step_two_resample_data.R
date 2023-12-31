library(tidyverse)
library(purrr)
source(here::here("code/R/resample_utils.R"))
options(digits.secs = 3)
sample_rate_target = 30 # we want to resample all data to 15 Hz


# ox files
ox_files = list.files(here::here("data", "reorganized", "oxwalk"), full.names = TRUE,
                      recursive = TRUE)

ox_resampled =
  purrr::map(
    .x = ox_files,
    .f = function(file) {
      x = readr::read_csv(file)
      sub = x$id_subject[1]
      samp_rate = x$sample_rate[1]
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        mutate(id_subject = sub,
               id_study = "oxwalk",
               sample_rate = 30) %>%
        rename(tm_dttm = HEADER_TIMESTAMP)

      fname = paste0("oxwalk-", sub, "-resampled", samp_rate, "to30Hz", ".csv.gz")
      readr::write_csv(resamp, here::here("data", "reorganized",
                                          "oxwalk", sub, fname))
      resamp
    }
  ) %>%
  bind_rows()


# readr::write_csv(ox_resampled, here::here("data/processed/ox_data_resampled.csv.gz"))

rm(ox_files)
rm(ox_resampled)

clem_files = list.files(here::here("data", "reorganized", "clemson"), full.names = TRUE,
                     recursive = TRUE)

clem_resampled =
  purrr::map(
    .x = clem_files,
    .f = function(file) {
      x = readr::read_csv(file)
      sub = x$id_subject[1]
      samp_rate = x$sample_rate[1]
      activity = x$cat_activity[1]
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        mutate(id_subject = sub,
               id_study = "clemson",
               cat_activity = activity,
               sample_rate = 30) %>%
        rename(tm_dttm = HEADER_TIMESTAMP)

      fname = paste0("clemson-", sub, "-", activity, "-resampled", samp_rate, "to30Hz", ".csv.gz")
      readr::write_csv(resamp, here::here("data", "reorganized",
                                          "clemson", sub, fname))
      resamp

    }
  ) %>%
  bind_rows()

# readr::write_csv(clem_resampled, here::here("data/processed/clemson_ped_resampled.csv.gz"))
rm(clem_files)

# marea

marea_files = list.files(here::here("data", "reorganized", "marea"), full.names = TRUE,
                        recursive = TRUE)

marea_resampled =
  purrr::map(
    .x = marea_files,
    .f = function(file) {
      x = readr::read_csv(file)
      sub = x$id_subject[1]
      samp_rate = x$sample_rate[1]
      activity = x$cat_activity[1]
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        mutate(id_subject = sub,
               id_study = "marea",
               cat_activity = activity,
               sample_rate = 30) %>%
        rename(tm_dttm = HEADER_TIMESTAMP)

      fname = paste0("marea-", sub, "-", activity, "-resampled", samp_rate, "to30Hz", ".csv.gz")
      readr::write_csv(resamp, here::here("data", "reorganized",
                                          "marea", sub, fname))
      resamp

    }
  ) %>%
  bind_rows()

# readr::write_csv(marea_resampled, here::here("data/processed/mareason_ped_resampled.csv.gz"))
rm(marea_files)

