library(tidyverse)
options(digits.secs = 3)
source(here::here("code/R/utils.R"))
files = list.files(here::here("results/actilife/clemson"), full.names = TRUE,
                   recursive = TRUE)

# only works on resampled data, so just use those
files = files[grep("resampled", files)]

map(files, .f = function(x){
  temp = readr::read_csv(x,
                         col_names = c("X", "Y", "Z", "steps_acti"),
                         skip = 10) %>%
    mutate(index = row_number())
  id = sub(".*clemson\\-(.+)\\-walk.*", "\\1", x)
  if(!file.exists(here::here("data", "reorganized", "clemson", id, "step_estimates"))){
    dir.create(here::here("data", "reorganized", "clemson", id, "step_estimates"))
  }
  fname_root = sub(".*clemson\\/(.+)1sec.*", "\\1", x)
  fname = paste0(fname_root, "-steps_actilife.csv")
  readr::write_csv(temp, here::here("data", "reorganized", "clemson", id, "step_estimates",
                              fname))
})

files = list.files(here::here("results/actilife/oxwalk"), full.names = TRUE,
                   recursive = TRUE)

# only works on resampled data, so just use those
files = files[grep("resampled", files)]

map(files, .f = function(x){
  temp = readr::read_csv(x,
                         col_names = c("X", "Y", "Z", "steps_acti"),
                         skip = 10) %>%
    mutate(index = row_number())
  id = sub(".*oxwalk\\-(.+)\\-r.*", "\\1", x)
  if(!file.exists(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))){
    dir.create(here::here("data", "reorganized", "oxwalk", id, "step_estimates"))
  }
  fname_root = sub(".*oxwalk\\/(.+)1sec.*", "\\1", x)
  fname = paste0(fname_root, "-steps_actilife.csv")
  readr::write_csv(temp, here::here("data", "reorganized", "oxwalk", id, "step_estimates",
                                    fname))
})

files = list.files(here::here("results/actilife/marea"), full.names = TRUE,
                   recursive = TRUE)

# only works on resampled data, so just use those
files = files[grep("resampled", files)]

map(files, .f = function(x){
  temp = readr::read_csv(x,
                         col_names = c("X", "Y", "Z", "steps_acti"),
                         skip = 10) %>%
    mutate(index = row_number())
  id = str_split(sub(".*marea\\-(.+)\\-.*", "\\1", x), "-")[[1]][1]
  if(!file.exists(here::here("data", "reorganized", "marea", id, "step_estimates"))){
    dir.create(here::here("data", "reorganized", "marea", id, "step_estimates"))
  }
  fname_root = sub(".*marea\\/(.+)1sec.*", "\\1", x)
  fname = paste0(fname_root, "-steps_actilife.csv")
  readr::write_csv(temp, here::here("data", "reorganized", "marea", id, "step_estimates",
                                    fname))
})
