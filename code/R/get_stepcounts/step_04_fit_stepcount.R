# r version of stepcount
# conda activate stepcount
# john's version
# pip install git+https://github.com/OxWearables/stepcount.git@1afed4edaeed1d4b3483c60c0b3d8595198b863b
# hmm 3 version
# pip install git+https://github.com/OxWearables/stepcount.git@9d1cb3305d1a2dcf1dc10944120c6ee29999fd99
devtools::install_github("jhuwit/stepcount")
stepcount::unset_reticulate_python()
stepcount::use_stepcount_condaenv()
library(reticulate)
# stepcount::stepcount_check()
options(digits.secs = 3)
library(purrr)
library(tidyverse)
# clemson
files_resampled = list.files(here::here("data", "stepcount", "clemson"),
                        full.names =  TRUE,
                        recursive = TRUE,
                        pattern = "resampled")

files_raw = list.files(here::here("data", "stepcount", "clemson"),
                        full.names =  TRUE,
                        recursive = TRUE,
                        pattern = "raw")

if (stepcount::stepcount_check()) {
  ssl_res = stepcount::stepcount(file = files_resampled,
                              model_type = "ssl",
                              sample_rate =  30)
  rf_res = stepcount::stepcount(file = files_resampled,
                                model_type = "rf",
                                sample_rate =  30)

  ssl_res_raw = stepcount::stepcount(file = files_raw,
                                 model_type = "ssl",
                                 sample_rate =  15)
  rf_res_raw = stepcount::stepcount(file = files_raw,
                                model_type = "rf",
                                sample_rate =  15)
}

map(c(rf_res, rf_res_raw),
    .f = function(x){
      fname = x$info$Filename
      df_times = x$steps
      step_df = x$step_times %>%
        mutate(time = as.POSIXct(time, tz = "UTC"))
      # read in total steps file
      # read in step times files

      # expand times file to get all seconds, plus a 10s buffer at end
      start = min(df_times$time)
      end = max(df_times$time) + as.period(10, "seconds")
      time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

      # if any rows (i.e. any steps)
      if(nrow(step_df) > 0) {
        step_df = step_df %>%
          mutate(steps_scrf = 1) %>%
          mutate(time = floor_date(time, unit = "seconds")) %>%
          group_by(time) %>%
          summarize(steps_scrf = sum(steps_scrf, na.rm = TRUE)) %>%
          ungroup()
        steps = left_join(time_df, step_df, by = "time") %>%
          mutate(across(steps_scrf, ~ ifelse(is.na(.x), 0, .x)))
      } else {
        steps =
          time_df %>%
          mutate(steps_scrf = 0)
      }

      id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", fname)
      if (!file.exists(here::here("data", "reorganized", "clemson", id, "step_estimates"))) {
        dir.create(here::here("data", "reorganized", "clemson", id, "step_estimates"))
      }
      fname_root = sub(".*clemson\\/(.+).csv.gz.*", "\\1", fname)
      fname = paste0(fname_root, "-steps_stepcountrf.csv")
      readr::write_csv(steps,
                       here::here("data", "reorganized", "clemson", id, "step_estimates",
                                  fname))


    })


map(c(ssl_res, ssl_res_raw),
    .f = function(x){
      fname = x$info$Filename
      df_times = x$steps
      step_df = x$step_times %>%
        mutate(time = as.POSIXct(time))
      # read in total steps file
      # read in step times files

      # expand times file to get all seconds, plus a 10s buffer at end
      start = min(df_times$time)
      end = max(df_times$time) + as.period(10, "seconds")
      time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

      # if any rows (i.e. any steps)
      if(nrow(step_df) > 0) {
        step_df = step_df %>%
          mutate(steps_scssl = 1) %>%
          mutate(time = floor_date(time, unit = "seconds")) %>%
          group_by(time) %>%
          summarize(steps_scssl = sum(steps_scssl, na.rm = TRUE)) %>%
          ungroup()
        steps = left_join(time_df, step_df, by = "time") %>%
          mutate(across(steps_scssl, ~ ifelse(is.na(.x), 0, .x)))
      } else {
        steps =
          time_df %>%
          mutate(steps_scssl = 0)
      }

      id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", fname)
      if (!file.exists(here::here("data", "reorganized", "clemson", id, "step_estimates"))) {
        dir.create(here::here("data", "reorganized", "clemson", id, "step_estimates"))
      }
      fname_root = sub(".*clemson\\/(.+).csv.gz.*", "\\1", fname)
      fname = paste0(fname_root, "-steps_stepcountssl.csv")
      readr::write_csv(steps,
                       here::here("data", "reorganized", "clemson", id, "step_estimates",
                                  fname))
    })
rm(list = ls())

# marea
files_resampled = list.files(here::here("data", "stepcount", "marea"),
                           full.names =  TRUE,
                           recursive = TRUE,
                           pattern = "resampled")

files_raw = list.files(here::here("data", "stepcount", "marea"),
                        full.names =  TRUE,
                        recursive = TRUE,
                        pattern = "raw")
if(length(files_raw)> 0 && length(files_resampled) > 0){
  if (stepcount::stepcount_check()) {
    ssl_res = stepcount::stepcount(file = files_resampled,
                                   model_type = "ssl",
                                   sample_rate =  30)
    rf_res = stepcount::stepcount(file = files_resampled,
                                  model_type = "rf",
                                  sample_rate =  30)

    ssl_res_raw = stepcount::stepcount(file = files_raw,
                                       model_type = "ssl",
                                       sample_rate =  128)
    rf_res_raw = stepcount::stepcount(file = files_raw,
                                      model_type = "rf",
                                      sample_rate =  128)
  }

  map(c(rf_res, rf_res_raw),
      .f = function(x){
        fname = x$info$Filename
        df_times = x$steps
        step_df = x$step_times %>%
          mutate(time = as.POSIXct(time))
        # read in total steps file
        # read in step times files

        # expand times file to get all seconds, plus a 10s buffer at end
        start = min(df_times$time)
        end = max(df_times$time) + as.period(10, "seconds")
        time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

        # if any rows (i.e. any steps)
        if(nrow(step_df) > 0) {
          step_df = step_df %>%
            mutate(steps_scrf = 1) %>%
            mutate(time = floor_date(time, unit = "seconds")) %>%
            group_by(time) %>%
            summarize(steps_scrf = sum(steps_scrf, na.rm = TRUE)) %>%
            ungroup()
          steps = left_join(time_df, step_df, by = "time") %>%
            mutate(across(steps_scrf, ~ ifelse(is.na(.x), 0, .x)))
        } else {
          steps =
            time_df %>%
            mutate(steps_scrf = 0)
        }

        id = regmatches(fname, gregexpr("(?<=a\\-)[a-zA-Z0-9]{3}", fname, perl = TRUE))[[1]][1]
        if (!file.exists(here::here("data", "reorganized", "marea", id, "step_estimates"))) {
          dir.create(here::here("data", "reorganized", "marea", id, "step_estimates"))
        }
        fname_root = sub(".*marea\\/(.+).csv.gz.*", "\\1", fname)
        fname = paste0(fname_root, "-steps_stepcountrf.csv")
        readr::write_csv(steps,
                         here::here("data", "reorganized", "marea", id, "step_estimates",
                                    fname))
      })


  map(c(ssl_res, ssl_res_raw),
      .f = function(x){
        fname = x$info$Filename
        df_times = x$steps
        step_df = x$step_times %>%
          mutate(time = as.POSIXct(time))
        # read in total steps file
        # read in step times files

        # expand times file to get all seconds, plus a 10s buffer at end
        start = min(df_times$time)
        end = max(df_times$time) + as.period(10, "seconds")
        time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

        # if any rows (i.e. any steps)
        if(nrow(step_df) > 0) {
          step_df = step_df %>%
            mutate(steps_scssl = 1) %>%
            mutate(time = floor_date(time, unit = "seconds")) %>%
            group_by(time) %>%
            summarize(steps_scssl = sum(steps_scssl, na.rm = TRUE)) %>%
            ungroup()
          steps = left_join(time_df, step_df, by = "time") %>%
            mutate(across(steps_scssl, ~ ifelse(is.na(.x), 0, .x)))
        } else {
          steps =
            time_df %>%
            mutate(steps_scssl = 0)
        }

        id = regmatches(fname, gregexpr("(?<=a\\-)[a-zA-Z0-9]{3}", fname, perl = TRUE))[[1]][1]
        if (!file.exists(here::here("data", "reorganized", "marea", id, "step_estimates"))) {
          dir.create(here::here("data", "reorganized", "marea", id, "step_estimates"))
        }
        fname_root = sub(".*marea\\/(.+).csv.gz.*", "\\1", fname)
        fname = paste0(fname_root, "-steps_stepcountssl.csv")
        readr::write_csv(steps,
                         here::here("data", "reorganized", "marea", id, "step_estimates",
                                    fname))
      })
}




files_resampled = list.files(here::here("data", "stepcount", "oxwalk"),
                             full.names =  TRUE,
                             recursive = TRUE,
                             pattern = "resampled")

files_raw = list.files(here::here("data", "stepcount", "oxwalk"),
                       full.names =  TRUE,
                       recursive = TRUE,
                       pattern = "raw")
files_raw_25 = files_raw[grepl("25Hz", files_raw)]
files_raw_100 = files_raw[grepl("100Hz", files_raw)]


if (stepcount::stepcount_check()) {
  ssl_res = stepcount::stepcount(file = files_resampled,
                                 model_type = "ssl",
                                 sample_rate =  30)
  rf_res = stepcount::stepcount(file = files_resampled,
                                model_type = "rf",
                                sample_rate =  30)

  ssl_res_raw25 = stepcount::stepcount(file = files_raw_25,
                                     model_type = "ssl",
                                     sample_rate =  25)
  rf_res_raw25 = stepcount::stepcount(file = files_raw_25,
                                    model_type = "rf",
                                    sample_rate =  25)

  ssl_res_raw100 = stepcount::stepcount(file = files_raw_25,
                                       model_type = "ssl",
                                       sample_rate = 100)
  rf_res_raw100 = stepcount::stepcount(file = files_raw_25,
                                    model_type = "rf",
                                    sample_rate =  100)
}

map(c(rf_res,  rf_res_raw100, rf_res_raw25),
    .f = function(x){
      fname = x$info$Filename
      df_times = x$steps
      step_df = x$step_times %>%
        mutate(time = as.POSIXct(time))
      # read in total steps file
      # read in step times files

      # expand times file to get all seconds, plus a 10s buffer at end
      start = min(df_times$time)
      end = max(df_times$time) + as.period(10, "seconds")
      time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

      # if any rows (i.e. any steps)
      if(nrow(step_df) > 0) {
        step_df = step_df %>%
          mutate(steps_scrf = 1) %>%
          mutate(time = floor_date(time, unit = "seconds")) %>%
          group_by(time) %>%
          summarize(steps_scrf = sum(steps_scrf, na.rm = TRUE)) %>%
          ungroup()
        steps = left_join(time_df, step_df, by = "time") %>%
          mutate(across(steps_scrf, ~ ifelse(is.na(.x), 0, .x)))
      } else {
        steps =
          time_df %>%
          mutate(steps_scrf = 0)
      }

      id = sub(".*oxwalk\\-(.+)-r.*", "\\1", fname)
      if (!file.exists(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))) {
        dir.create(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))
      }
      fname_root = sub(".*oxwalk\\/(.+).csv.gz.*", "\\1", fname)
      fname = paste0(fname_root, "-steps_stepcountrf.csv")
      readr::write_csv(steps,
                       here::here("data", "reorganized", "oxwalk", id, "step_estimates",
                                  fname))
    })


map(c(ssl_res, ssl_res_raw25, ssl_res_raw100),
    .f = function(x){
      fname = x$info$Filename
      df_times = x$steps
      step_df = x$step_times %>%
        mutate(time = as.POSIXct(time))
      # read in total steps file
      # read in step times files

      # expand times file to get all seconds, plus a 10s buffer at end
      start = min(df_times$time)
      end = max(df_times$time) + as.period(10, "seconds")
      time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

      # if any rows (i.e. any steps)
      if(nrow(step_df) > 0) {
        step_df = step_df %>%
          mutate(steps_scssl = 1) %>%
          mutate(time = floor_date(time, unit = "seconds")) %>%
          group_by(time) %>%
          summarize(steps_scssl = sum(steps_scssl, na.rm = TRUE)) %>%
          ungroup()
        steps = left_join(time_df, step_df, by = "time") %>%
          mutate(across(steps_scssl, ~ ifelse(is.na(.x), 0, .x)))
      } else {
        steps =
          time_df %>%
          mutate(steps_scssl = 0)
      }

      id = sub(".*oxwalk\\-(.+)-r.*", "\\1", fname)
      if (!file.exists(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))) {
        dir.create(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))
      }
      fname_root = sub(".*oxwalk\\/(.+).csv.gz.*", "\\1", fname)
      fname = paste0(fname_root, "-steps_stepcountssl.csv")
      readr::write_csv(steps,
                       here::here("data", "reorganized", "oxwalk", id, "step_estimates",
                                  fname))
    })

