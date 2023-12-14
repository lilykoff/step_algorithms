library(tidyverse)

# get files for stepcount

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
      type = df$cat_activity[1]
      sample_rate = df$sample_rate[1]
      id_study = df$id_study[1]
      fname = paste(id, type, sample_rate, id_study, ".csv", sep = "_")
      temp = df %>%
        mutate(time = as.character(tm_dttm)) %>%
        select(time, x = X, y = Y, z = Z)
      write_csv(temp, here::here("data", "processed", "stepcount_data", fname))
    })

ox_list = split(ox, f = list(ox$id_study,
                             ox$id_subject,
                             ox$sample_rate))
map(.x = ox_list,
    .f = function(df){
      id = df$id_subject[1]
      sample_rate = df$sample_rate[1]
      id_study = df$id_study[1]
      fname = paste(id,sample_rate, id_study, ".csv", sep = "_")
      temp = df %>%
        mutate(time = as.character(tm_dttm)) %>%
        select(time, x = X, y = Y, z = Z)
      write_csv(temp, here::here("data", "processed", "stepcount_data", fname))
    })

files = list.files(here::here("data/processed/stepcount_data"),
                   full.names = TRUE)
write_csv(as_tibble(paste("stepcount", files,
      "-o results/stepcount")), "data/command_table.csv")
