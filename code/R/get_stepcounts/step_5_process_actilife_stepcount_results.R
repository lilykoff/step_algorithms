# process results from stepcount into same format as other algorithms
library(tidyverse)
options(digits.secs = 3)
source(here::here("code/R/utils.R"))

# first do ssl results

clemson_csvs = list.files(
  here::here("results/stepcount/clemson"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = "*.-Steps.csv"
)

marea_csvs = list.files(
  here::here("results/stepcount/marea"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = "*.-Steps.csv"
)

ox_csvs =  list.files(
  here::here("results/stepcount/oxwalk"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = "*.-Steps.csv"
)

map(clemson_csvs, function(file) {
  df = readr::read_csv(file) %>%
    rename(steps_sc = Steps)
  id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", file)
  if (!file.exists(here::here("data", "reorganized", "clemson", id, "step_estimates"))) {
    dir.create(here::here("data", "reorganized", "clemson", id, "step_estimates"))
  }
  fname_root = sub(".*Hz\\/(.+)-Steps.*", "\\1", file)
  fname = paste0(fname_root, "-steps_stepcount.csv")
  readr::write_csv(df,
                   here::here("data", "reorganized", "clemson", id, "step_estimates",
                              fname))
})


map(marea_csvs, function(file) {
  df = readr::read_csv(file) %>%
    rename(steps_sc = Steps)
  id = regmatches(file, gregexpr("(?<=a\\-)[a-zA-Z0-9]{3}", file, perl = TRUE))[[1]][1]
  if (!file.exists(here::here("data", "reorganized", "marea", id, "step_estimates"))) {
    dir.create(here::here("data", "reorganized", "marea", id, "step_estimates"))
  }
  fname_root = sub(".*Hz\\/(.+)-Steps.*", "\\1", file)
  fname = paste0(fname_root, "-steps_stepcount.csv")
  readr::write_csv(df,
                   here::here("data", "reorganized", "marea", id, "step_estimates",
                              fname))
})

map(ox_csvs, function(file) {
  df = readr::read_csv(file) %>%
    rename(steps_sc = Steps)
  id = sub(".*oxwalk\\-(.+)-r.*", "\\1", file)
  if (!file.exists(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))) {
    dir.create(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))
  }
  fname_root = sub(".*Hz\\/(.+)-Steps.*", "\\1", file)
  fname = paste0(fname_root, "-steps_stepcount.csv")
  readr::write_csv(df,
                   here::here("data", "reorganized", "oxwalk", id, "step_estimates",
                              fname))
})


rm(list = ls())
# random forest results
library(tidyverse)
options(digits.secs = 3)
source(here::here("code/R/utils.R"))


files = list.files(here::here("results/stepcount_rf"),
                   full.names = TRUE,
                   recursive = TRUE)

clemson_csvs = list.files(
  here::here("results/stepcount_rf/clemson"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)
clemson_csvs_steps =  list.files(
  here::here("results/stepcount_rf/clemson"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-StepTimes.csv"
)

marea_csvs = list.files(
  here::here("results/stepcount_rf/marea"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)
marea_csvs_steps =  list.files(
  here::here("results/stepcount_rf/marea"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-StepTimes.csv"
)

ox_csvs = list.files(
  here::here("results/stepcount_rf/oxwalk"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-Steps.csv"
)
ox_csvs_steps =  list.files(
  here::here("results/stepcount_rf/oxwalk"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = ".*-StepTimes.csv"
)


# for these ones need to use the "steps" csv to get true length of times
map2(.x = clemson_csvs, .y = clemson_csvs_steps,
     function(file, stepfile) {
       df_times = readr::read_csv(file)
       start = min(df_times$time)
       end = max(df_times$time) + as.period(5, "seconds")
       time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

       step_df = readr::read_csv(stepfile) %>%
         mutate(steps_scrf = 1) %>%
         mutate(time = floor_date(time, unit = "seconds")) %>%
         group_by(time) %>%
         summarize(steps_scrf = sum(steps_scrf, na.rm = TRUE)) %>%
         ungroup()

       steps = left_join(time_df, step_df, by = "time") %>%
         mutate(across(steps_scrf, ~ ifelse(is.na(.x), 0, .x)))

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

map2(ox_csvs, ox_csvs_steps,
     function(file, stepfile) {
       df_times = readr::read_csv(file)
       start = min(df_times$time)
       end = max(df_times$time) + as.period(5, "seconds")
       time_df = tibble(time = floor_date(seq(start, end, "sec"), unit = "seconds"))

       step_df = readr::read_csv(stepfile) %>%
         mutate(steps_scrf = 1) %>%
         mutate(time = floor_date(time, unit = "seconds")) %>%
         group_by(time) %>%
         summarize(steps_scrf = sum(steps_scrf, na.rm = TRUE)) %>%
         ungroup()

       steps = left_join(time_df, step_df, by = "time") %>%
         mutate(across(steps_scrf, ~ ifelse(is.na(.x), 0, .x)))

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


map2(marea_csvs, marea_csvs_steps,
     function(file, stepfile) {
       df_times = readr::read_csv(file)
       start = min(df_times$time)
       end = max(df_times$time) + as.period(5, "seconds")
       time_df = tibble(time = floor_date(seq(start, end, "sec"), unit =
                                            "seconds"))

       step_df = readr::read_csv(stepfile) %>%
         mutate(steps_scrf = 1) %>%
         mutate(time = floor_date(time, unit = "seconds")) %>%
         group_by(time) %>%
         summarize(steps_scrf = sum(steps_scrf, na.rm = TRUE)) %>%
         ungroup()

       steps = left_join(time_df, step_df, by = "time") %>%
         mutate(across(steps_scrf, ~ ifelse(is.na(.x), 0, .x)))

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

# only works on resampled data, so just use those


map(
  files,
  .f = function(x) {
    temp = readr::read_csv(x,
                           col_names = c("X", "Y", "Z", "steps_acti"),
                           skip = 10) %>%
      mutate(index = row_number())
    id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", x)
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
  pattern = "resampled"
)



map(
  files,
  .f = function(x) {
    temp = readr::read_csv(x,
                           col_names = c("X", "Y", "Z", "steps_acti"),
                           skip = 10) %>%
      mutate(index = row_number())
    id = str_split(sub(".*marea\\-(.+)\\-.*", "\\1", x), "-")[[1]][1]
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

files = list.files(
  here::here("results/actilife/oxwalk"),
  full.names = TRUE,
  recursive = TRUE,
  pattern = "resampled"
)


map(
  files,
  .f = function(x) {
    temp = readr::read_csv(x,
                           col_names = c("X", "Y", "Z", "steps_acti"),
                           skip = 10) %>%
      mutate(index = row_number())
    id = sub(".*oxwalk\\-(.+)\\-r.*", "\\1", x)
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
