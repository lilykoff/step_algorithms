library(tidyverse)
library(purrr)
source(here::here("code/R/resample_utils.R"))
options(digits.secs = 3)
sample_rate_target = 30 # we want to resample all data to 30 Hz

# get raw files only
clem_files = list.files(
  here::here("data", "reorganized", "clemson"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*raw.*.csv.gz"
)

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
        mutate(
          id_subject = sub,
          id_study = "clemson",
          cat_activity = activity,
          sample_rate = sample_rate_target
        ) %>%
        rename(tm_dttm = HEADER_TIMESTAMP)

      fname = paste0(
        "clemson-",
        sub,
        "-",
        activity,
        "-resampled",
        samp_rate,
        "to",
        sample_rate_target,
        "Hz",
        ".csv.gz"
      )
      readr::write_csv(resamp,
                       here::here("data", "reorganized",
                                  "clemson", sub, fname))
      resamp

    }
  )

rm(clem_files)

# marea

marea_files = list.files(
  here::here("data", "reorganized", "marea"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*raw.*.csv.gz"
)

if(length(marea_files) > 0){
  marea_resampled =
    purrr::map(
      .x = marea_files,
      .f = function(file) {
        x = readr::read_csv(file)
        act_key = x %>%
          select(tm_dttm, cat_activity) %>%
          mutate(second = floor_date(tm_dttm, unit = "0.5 seconds")) %>%
          group_by(second, cat_activity) %>%
          summarize(n = n()) %>%
          group_by(second) %>%
          summarize(cat_activity = cat_activity[n == max(n)])
        sub = x$id_subject[1]
        samp_rate = x$sample_rate[1]
        activity = x$cat_activity_large[1]
        resamp = resample_accel_data(x,
                                     sample_rate = sample_rate_target,
                                     method = "linear") %>%
          mutate(
            id_subject = sub,
            id_study = "marea",
            cat_activity_large = activity,
            sample_rate = sample_rate_target
          ) %>%
          rename(tm_dttm = HEADER_TIMESTAMP) %>%
          mutate(second = floor_date(tm_dttm, unit = "0.5 seconds")) %>%
          left_join(act_key, by = "second") %>%
          select(-second)

        fname = paste0(
          "marea-",
          sub,
          "-",
          activity,
          "-resampled",
          samp_rate,
          "to",
          sample_rate_target,
          "Hz",
          ".csv.gz"
        )
        readr::write_csv(resamp,
                         here::here("data", "reorganized",
                                    "marea", sub, fname))
        resamp

      }
    )

  rm(marea_files)
}


# ox files
ox_files = list.files(
  here::here("data", "reorganized", "oxwalk"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*raw.*.csv.gz"
)

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
               sample_rate = sample_rate_target) %>%
        rename(tm_dttm = HEADER_TIMESTAMP)

      fname = paste0("oxwalk-",
                     sub,
                     "-resampled",
                     samp_rate,
                     "to",
                     sample_rate_target,
                     "Hz",
                     ".csv.gz")
      readr::write_csv(resamp,
                       here::here("data", "reorganized",
                                  "oxwalk", sub, fname))
      resamp
    }
  )

rm(ox_files)
