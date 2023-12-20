library(tidyverse)

# for original files
clemson_ped = readr::read_csv(here::here("data/processed/clemson_ped.csv.gz"))
ox = readr::read_csv(here::here("data/processed/ox_data.csv.gz"))

clemson_list = split(clemson_ped, f = list(clemson_ped$id_subject,
                                           clemson_ped$cat_activity))
ox_list = split(ox, f = list(ox$id_study,
                             ox$id_subject,
                             ox$sample_rate))
map(.x = clemson_list,
    .f = function(df){
      id = df$id_subject[1]
      type = sub(".*\\_", "", df$cat_activity[1])
      sample_rate = df$sample_rate[1]
      fname = paste0("acc_", id, "_", type, "_", sample_rate, "Hz.csv")
      temp = df %>%
        mutate(time = as.character(tm_dttm)) %>%
        select(time, x = X, y = Y, z = Z)
      write_csv(temp, here::here("data", "stepcount", "original", fname))
    })


map(.x = ox_list,
    .f = function(df){
      id = df$id_subject[1]
      sample_rate = df$sample_rate[1]
      fname = paste0("acc_", id, "_", sample_rate, "Hz.csv")
      temp = df %>%
        mutate(time = as.character(tm_dttm)) %>%
        select(time, x = X, y = Y, z = Z)
      write_csv(temp, here::here("data", "stepcount", "original", fname))
    })

files = list.files(here::here("data/stepcount/original"),
                   full.names = TRUE)
write_csv(as_tibble(paste("stepcount", files,
      "-o results/stepcount/original")), "data/stepcount/command_table.csv")

library(tidyverse)
options(digits.secs = 3)
# get files for stepcount

files = list.files(here::here("data", "raw_resampled"), full.names = TRUE)

lapply(files, function(file){
  temp = readr::read_csv(file) %>%
    mutate(time = as.character(HEADER_TIMESTAMP)) %>%
    select(time, x = X, y = Y, z = Z)
  fname = sub(".*resampled\\/(.+).gz.*", "\\1", file)
  readr::write_csv(temp, here::here("data", "stepcount", "resampled", fname))
})


files = list.files(here::here("data/stepcount/resampled"),
                   full.names = TRUE)
readr::write_csv(as_tibble(paste("stepcount", files,
                          "-o results/stepcount/resampled")), "data/stepcount/command_table_resampled.csv")

