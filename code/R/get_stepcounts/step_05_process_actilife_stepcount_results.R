# process results from stepcount into same format as other algorithms
# f5896677e6e849725ef8cccea2e627f23421a548

library(tidyverse)
options(digits.secs = 3)
source(here::here("code/R/utils.R"))

# random forest first
clemson_csvs = list.files(
  here::here("results/stepcount_rf/clemson"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)

marea_csvs = list.files(
  here::here("results/stepcount_rf/marea"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)


ox_csvs = list.files(
  here::here("results/stepcount_rf/oxwalk"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)



# for these ones need to use the "steps" csv to get true length of times
map(.x = clemson_csvs,
     function(file) {
       df_times = readr::read_csv(file) # read in total steps file
       # read in step times files
       stepfile = list.files(here::here("results", "stepcount_rf", "clemson",
                                        sub(".*Hz\\/(.+)-Steps.csv.*", "\\1", file)),
                                        full.names = TRUE,
                                        recursive = TRUE,
                                        pattern = "StepTimes.csv")
       step_df = readr::read_csv(stepfile)

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
       id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", file)
       if (!file.exists(here::here("data", "reorganized", "clemson", id, "step_estimates"))) {
         dir.create(here::here("data", "reorganized", "clemson", id, "step_estimates"))
       }
       fname_root = sub(".*Hz\\/(.+)-StepTimes.*", "\\1", stepfile)
       fname = paste0(fname_root, "-steps_stepcountrf.csv")
       readr::write_csv(steps,
                        here::here("data", "reorganized", "clemson", id, "step_estimates",
                                   fname))
     })

map(.x = ox_csvs,
     function(file) {
       df_times = readr::read_csv(file)
       stepfile = list.files(here::here("results", "stepcount_rf", "oxwalk",
                                        sub(".*Hz\\/(.+)-Steps.csv.*", "\\1", file)),
                             full.names = TRUE,
                             recursive = TRUE,
                             pattern = "StepTimes.csv")
       step_df = readr::read_csv(stepfile)

       start = min(df_times$time)
       end = max(df_times$time) + as.period(10, "seconds")
       time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

       step_df = readr::read_csv(stepfile)
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
       id = sub(".*oxwalk\\-(.+)-r.*", "\\1", file)
       if (!file.exists(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))) {
         dir.create(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))
       }
       fname_root = sub(".*Hz\\/(.+)-Steps.*", "\\1", file)
       fname = paste0(fname_root, "-steps_stepcountrf.csv")
       readr::write_csv(steps,
                        here::here("data", "reorganized", "oxwalk", id, "step_estimates",
                                   fname))
     })


map(.x = marea_csvs,
     function(file) {
       df_times = readr::read_csv(file)
       stepfile = list.files(here::here("results", "stepcount_rf", "marea",
                                        sub(".*Hz\\/(.+)-Steps.csv.*", "\\1", file)),
                             full.names = TRUE,
                             recursive = TRUE,
                             pattern = "StepTimes.csv")
       step_df = readr::read_csv(stepfile)
       start = min(df_times$time)
       end = max(df_times$time) + as.period(10, "seconds")
       time_df = tibble(time = floor_date(seq(start, end, "sec"), unit =
                                            "seconds"))
       step_df = readr::read_csv(stepfile)
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
       id = regmatches(file, gregexpr("(?<=a\\-)[a-zA-Z0-9]{3}", file, perl = TRUE))[[1]][1]
       if (!file.exists(here::here("data", "reorganized", "marea", id, "step_estimates"))) {
         dir.create(here::here("data", "reorganized", "marea", id, "step_estimates"))
       }
       fname_root = sub(".*Hz\\/(.+)-Steps.*", "\\1", file)
       fname = paste0(fname_root, "-steps_stepcountrf.csv")
       readr::write_csv(steps,
                        here::here("data", "reorganized", "marea", id, "step_estimates",
                                   fname))
     })

# SSL results

clemson_csvs = list.files(
  here::here("results/stepcount_ssl/clemson"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)


marea_csvs = list.files(
  here::here("results/stepcount_ssl/marea"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)

ox_csvs = list.files(
  here::here("results/stepcount_ssl/oxwalk"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)


map(.x = clemson_csvs,
     function(file) {
       df_times = readr::read_csv(file)
       stepfile = list.files(here::here("results", "stepcount_ssl", "clemson",
                                        sub(".*Hz\\/(.+)-Steps.csv.*", "\\1", file)),
                             full.names = TRUE,
                             recursive = TRUE,
                             pattern = "StepTimes.csv")
       step_df = readr::read_csv(stepfile)

       start = min(df_times$time)
       end = max(df_times$time) + as.period(10, "seconds")
       time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

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

       id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", file)
       if (!file.exists(here::here("data", "reorganized", "clemson", id, "step_estimates"))) {
         dir.create(here::here("data", "reorganized", "clemson", id, "step_estimates"))
       }
       fname_root = sub(".*Hz\\/(.+)-StepTimes.*", "\\1", stepfile)
       fname = paste0(fname_root, "-steps_stepcountssl.csv")
       readr::write_csv(steps,
                        here::here("data", "reorganized", "clemson", id, "step_estimates",
                                   fname))
     })

map(.x = ox_csvs,
     function(file) {
       df_times = readr::read_csv(file)
       stepfile = list.files(here::here("results", "stepcount_ssl", "oxwalk",
                                        sub(".*Hz\\/(.+)-Steps.csv.*", "\\1", file)),
                             full.names = TRUE,
                             recursive = TRUE,
                             pattern = "StepTimes.csv")
       step_df = readr::read_csv(stepfile)

       start = min(df_times$time)
       end = max(df_times$time) + as.period(10, "seconds")
       time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

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

       id = sub(".*oxwalk\\-(.+)-r.*", "\\1", file)
       if (!file.exists(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))) {
         dir.create(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))
       }
       fname_root = sub(".*Hz\\/(.+)-Steps.*", "\\1", file)
       fname = paste0(fname_root, "-steps_stepcountssl.csv")
       readr::write_csv(steps,
                        here::here("data", "reorganized", "oxwalk", id, "step_estimates",
                                   fname))
     })


map(.x = marea_csvs,
     function(file) {
       df_times = readr::read_csv(file)
       stepfile = list.files(here::here("results", "stepcount_ssl", "marea",
                                        sub(".*Hz\\/(.+)-Steps.csv.*", "\\1", file)),
                             full.names = TRUE,
                             recursive = TRUE,
                             pattern = "StepTimes.csv")
       step_df = readr::read_csv(stepfile)
       start = min(df_times$time)
       end = max(df_times$time) + as.period(10, "seconds")
       time_df = tibble(time = floor_date(seq(start, end, "sec"), unit =
                                            "seconds"))

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
       id = regmatches(file, gregexpr("(?<=a\\-)[a-zA-Z0-9]{3}", file, perl = TRUE))[[1]][1]
       if (!file.exists(here::here("data", "reorganized", "marea", id, "step_estimates"))) {
         dir.create(here::here("data", "reorganized", "marea", id, "step_estimates"))
       }
       fname_root = sub(".*Hz\\/(.+)-Steps.*", "\\1", file)
       fname = paste0(fname_root, "-steps_stepcountssl.csv")
       readr::write_csv(steps,
                        here::here("data", "reorganized", "marea", id, "step_estimates",
                                   fname))
     })

rm(list = ls())

# would only need this part if we do R stepcount version
# process results from actilife into same as other algorithms
library(tidyverse)
options(digits.secs = 3)
source(here::here("code/R/utils.R"))

files = list.files(
  here::here("results/actilife/clemson"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = "resampled"
)

# only works on data above 25 Hz, so just use those

map(
  files,
  .f = function(x) {
    id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", x)
    raw_path = sub(".*\\/(.+)1sec.*", "\\1", x)
    raw_file = list.files(here::here("data", "reorganized", "clemson", id),
                          full.names = TRUE,
                          recursive = TRUE,
                          pattern = paste0(raw_path, ".csv.gz"))

    temp = readr::read_csv(x,
                           col_names = c("X", "Y", "Z", "steps_acti"),
                           skip = 10) %>%
      mutate(index = row_number())

    raw_df =
      readr::read_csv(raw_file, n_max = 1)

    start_time = floor_date(min(raw_df$tm_dttm), unit = "seconds")

    temp = temp %>%
      mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
      select(time, steps_acti)


    if (!file.exists(here::here("data", "reorganized", "clemson", id, "step_estimates"))) {
      dir.create(here::here("data", "reorganized", "clemson", id, "step_estimates"))
    }
    fname_root = sub(".*clemson\\/(.+)1sec.*", "\\1", x)
    fname = paste0(fname_root, "-steps_actilife.csv")
    readr::write_csv(temp,
                     here::here("data", "reorganized", "clemson", id, "step_estimates",
                                fname))
  }
)


files = list.files(
  here::here("results/actilife/marea"),
  full.names = TRUE,
  recursive = TRUE,
)



map(
  files,
  .f = function(x) {
    id = str_split(sub(".*marea\\-(.+)\\-.*", "\\1", x), "-")[[1]][1]

    temp = readr::read_csv(x,
                           col_names = c("X", "Y", "Z", "steps_acti"),
                           skip = 10) %>%
      mutate(index = row_number())

    raw_path = sub(".*\\/(.+)1sec.*", "\\1", x)
    raw_file = list.files(here::here("data", "reorganized", "marea", id),
                          full.names = TRUE,
                          recursive = TRUE,
                          pattern = paste0(raw_path, ".csv.gz"))
    raw_df =
      readr::read_csv(raw_file, n_max = 1)

    start_time = floor_date(min(raw_df$tm_dttm), unit = "seconds")

    temp = temp %>%
      mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
      select(time, steps_acti)


    if (!file.exists(here::here("data", "reorganized", "marea", id, "step_estimates"))) {
      dir.create(here::here("data", "reorganized", "marea", id, "step_estimates"))
    }
    fname_root = sub(".*marea\\/(.+)1sec.*", "\\1", x)
    fname = paste0(fname_root, "-steps_actilife.csv")
    readr::write_csv(temp,
                     here::here("data", "reorganized", "marea", id, "step_estimates",
                                fname))
  }
)

files = c(
  list.files(
    here::here("results/actilife/oxwalk"),
    full.names = TRUE,
    recursive = TRUE,
    pattern = "100"
  ),
  list.files(
    here::here("results/actilife/oxwalk"),
    full.names = TRUE,
    recursive = TRUE,
    pattern = "30"
  ))


map(
  files,
  .f = function(x) {
    id = sub(".*oxwalk\\-(.+)\\-r.*", "\\1", x)
    temp = readr::read_csv(x,
                           col_names = c("X", "Y", "Z", "steps_acti"),
                           skip = 10) %>%
      mutate(index = row_number())

    raw_path = sub(".*\\/(.+)1sec.*", "\\1", x)
    raw_file = list.files(here::here("data", "reorganized", "oxwalk", id),
                          full.names = TRUE,
                          recursive = TRUE,
                          pattern = paste0(raw_path, ".csv.gz"))
    raw_df =
      readr::read_csv(raw_file, n_max = 1)

    start_time = floor_date(min(raw_df$tm_dttm), unit = "seconds")

    temp = temp %>%
      mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
      select(time, steps_acti)

    if (!file.exists(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))) {
      dir.create(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))
    }
    fname_root = sub(".*oxwalk\\/(.+)1sec.*", "\\1", x)
    fname = paste0(fname_root, "-steps_actilife.csv")
    readr::write_csv(temp,
                     here::here("data", "reorganized", "oxwalk", id, "step_estimates",
                                fname))
  }
)
