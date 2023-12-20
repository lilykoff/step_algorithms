# this part was run on computing cluster
library(tidyverse)
# remotes::install_github("muschellij2/write.gt3x")
library(write.gt3x)
options(digits.secs = 3)
## convert for actilife - clemson
ped_files <-
  list.files(here::here("raw/clemson"),
             recursive = TRUE,
             full.names = TRUE)
acc_ped_files <- ped_files[-grep("steps.txt", ped_files)]


start = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")

write_to_csv = function(file) {
  temp = readr::read_table(
    file,
    col_names = c(
      "wrist_x",
      "wrist_y",
      "wrist_z",
      "hip_x",
      "hip_y",
      "hip_z",
      "ankle_x",
      "ankle_y",
      "ankle_z"
    )
  ) %>%
    select(1:3) %>%
    mutate(
      time_s = (row_number() - 1) / 15,
      time = start + as.period(time_s, unit = "seconds")
    ) %>%
    select(time, X = wrist_x, Y = wrist_y, Z = wrist_z)
  fname = paste0("acc_", sub(".*\\/(.+).txt.*", "\\1", file), ".csv.gz")
  # readr::write_csv(temp, here::here("acc_data", fname))
  write.gt3x::write_actigraph_csv(
    df = temp,
    file = here::here("acc_data", fname),
    sample_rate = 15L,
    max_g = "8",
    progress = FALSE
  )
  # doing this so .Last.value isn't maintained
  rm(temp)

}

map(.x = acc_ped_files, .f = write_to_csv)

# convert for actilife- oxwalk
wrist_files_100 = list.files(
  here::here("raw/oxwalk/OxWalk_Dec2022/Wrist_100Hz"),
  recursive = TRUE,
  full.names = TRUE
)
wrist_files_25 = list.files(
  here::here("raw/oxwalk/OxWalk_Dec2022/Wrist_25Hz"),
  recursive = TRUE,
  full.names = TRUE
)


write_to_csv_ow = function(file, sample_rate) {
  temp =
    readr::read_csv(file) %>%
    select(time = timestamp,
           X = x,
           Y = y,
           Z = z)
  fname = paste0("acc_", sub(".*\\/(.+).csv.*", "\\1", file), ".csv.gz")
  write.gt3x::write_actigraph_csv(
    df = temp,
    file = here::here("acc_data", fname),
    sample_rate = sample_rate,
    max_g = "8",
    progress = FALSE
  )
  # doing this so .Last.value isn't maintained
  rm(temp)
}

map(.x = wrist_files_100,
    .f = write_to_csv_ow,
    sample_rate = 100L)

map(.x = wrist_files_25,
    .f = write_to_csv_ow,
    sample_rate = 25L)

# this part was run locally

ped_files <-
  list.files(here::here("data/raw_resampled"),
             recursive = TRUE,
             full.names = TRUE)


write_to_csv = function(file) {
  temp = readr::read_csv(file) %>%
    rename(time = HEADER_TIMESTAMP)
  fname = sub(".*resampled/", "", file)
  write.gt3x::write_actigraph_csv(
    df = temp,
    file = here::here("data/actilife/resampled", fname),
    sample_rate = 30L,
    max_g = "8",
    progress = FALSE
  )
  # doing this so .Last.value isn't maintained
  rm(temp)

}

map(.x = ped_files, .f = write_to_csv)


