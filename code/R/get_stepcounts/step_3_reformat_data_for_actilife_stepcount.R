# this part was run on computing cluster
# for actilife, data need to be in specific format, so we create specific actilife-compatible csvs

library(tidyverse)
# remotes::install_github("muschellij2/write.gt3x")
library(write.gt3x)
options(digits.secs = 3)
## convert for actilife - clemson
clemson_files = list.files(
  here::here("data", "reorganized", "clemson"),
  full.names = TRUE,
  recursive = TRUE
)

marea_files =  list.files(
  here::here("data", "reorganized", "marea"),
  full.names = TRUE,
  recursive = TRUE
)

ox_files = list.files(
  here::here("data", "reorganized", "oxwalk"),
  full.names = TRUE,
  recursive = TRUE
)

# in case this is done after other analysis (i.e. anything needs to be redone)
# remove step estimate files
clemson_files = clemson_files[!grepl("step_estimates", clemson_files)]
marea_files = marea_files[!grepl("step_estimates", marea_files)]
ox_files = ox_files[!grepl("step_estimates", ox_files)]

map(
  .x = clemson_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    samp_rate = x$sample_rate[1]
    fname = sub(".*\\/", "", filename)
    temp = x %>%
      select(time = tm_dttm, X, Y, Z)
    write.gt3x::write_actigraph_csv(
      df = temp,
      file = here::here("data", "actilife", "clemson", fname),
      sample_rate = samp_rate,
      max_g = "8",
      progress = FALSE
    )
  }
)

map(
  .x = marea_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    samp_rate = x$sample_rate[1]
    fname = sub(".*\\/", "", filename)
    temp = x %>%
      select(time = tm_dttm, X, Y, Z)
    write.gt3x::write_actigraph_csv(
      df = temp,
      file = here::here("data", "actilife", "marea", fname),
      sample_rate = samp_rate,
      max_g = "8",
      progress = FALSE
    )
  }
)

map(
  .x = ox_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    samp_rate = x$sample_rate[1]
    fname = sub(".*\\/", "", filename)
    temp = x %>%
      select(time = tm_dttm, X, Y, Z)
    write.gt3x::write_actigraph_csv(
      df = temp,
      file = here::here("data", "actilife", "oxwalk", fname),
      sample_rate = samp_rate,
      max_g = "8",
      progress = FALSE
    )
  }
)




rm(list = ls())

# for stepcount, we also need specific csvs
# column names need to be time, x, y, z; time in character format
library(tidyverse)
options(digits.secs = 3)

clemson_files = list.files(
  here::here("data", "reorganized", "clemson"),
  full.names = TRUE,
  recursive = TRUE
)

marea_files =  list.files(
  here::here("data", "reorganized", "marea"),
  full.names = TRUE,
  recursive = TRUE
)

ox_files = list.files(
  here::here("data", "reorganized", "oxwalk"),
  full.names = TRUE,
  recursive = TRUE
)

# in case this is done after other analysis (i.e. anything needs to be redone)
# remove step estimate files
clemson_files = clemson_files[!grepl("step_estimates", clemson_files)]
marea_files = marea_files[!grepl("step_estimates", marea_files)]
ox_files = ox_files[!grepl("step_estimates", ox_files)]

map(
  .x = clemson_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    fname = sub(".*\\/", "", filename)
    temp = x %>%
      mutate(time = as.character(tm_dttm)) %>%
      select(time, x = X, y = Y, z = Z)
    write_csv(temp, here::here("data", "stepcount", "clemson", fname))
  }
)


map(
  .x = ox_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    fname = sub(".*\\/", "", filename)
    temp = x %>%
      mutate(time = as.character(tm_dttm)) %>%
      select(time, x = X, y = Y, z = Z)
    write_csv(temp, here::here("data", "stepcount", "oxwalk", fname))
  }
)


map(
  .x = marea_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    fname = sub(".*\\/", "", filename)
    temp = x %>%
      mutate(time = as.character(tm_dttm)) %>%
      select(time, x = X, y = Y, z = Z)
    write_csv(temp, here::here("data", "stepcount", "marea", fname))
  }
)
