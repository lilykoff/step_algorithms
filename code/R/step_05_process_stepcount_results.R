library(tidyverse)
options(digits.secs = 3)
source(here::here("code/R/utils.R"))

files = list.files(here::here("results/stepcount"), full.names = TRUE,
                     recursive = TRUE)

clemson_csvs = files[grepl("\\-Steps.csv", files) == TRUE & grepl("clemson", files) == TRUE]
ox_csvs =  files[grepl("\\-Steps.csv", files) == TRUE & grepl("oxwalk", files) == TRUE]
marea_csvs = files[grepl("\\-Steps.csv", files) == TRUE & grepl("marea", files) == TRUE]

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


# fix b/c of resampling fix
# marea_csvs = files[grepl("\\-Steps.csv", files) == TRUE & grepl("marea", files) == TRUE
#                    & grepl("raw", files)==TRUE & grepl("separated", files) == FALSE]

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
