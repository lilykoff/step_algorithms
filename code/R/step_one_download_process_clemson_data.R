library(here)
library(purrr)
library(tidyverse)
library(readr)
# download here https://cecas.clemson.edu/~ahoover/pedometer/
url = "https://cecas.clemson.edu/tracking/Pedometer/Data.zip"

options(timeout=300)
options(digits.secs = 3)
downloader::download(url, dest="clemsonped.zip", mode="wb")
unzip("clemsonped.zip", exdir = here::here("data/raw/clemson"))


ped_files <- list.files(here::here("data/raw/clemson"), recursive = TRUE, full.names = TRUE)
acc_ped_files <- ped_files[-grep("steps.txt", ped_files)]
step_files <- ped_files[grep("steps.txt", ped_files)]
# 9 columns are wrist xyz, hip xyz and ankle xyz

start = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")

get_and_join_files <-
  function(acc_ped_file){
    temp = readr::read_table(
      acc_ped_file,
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
        id_type = sub("\\.txt.*", "", sub(".*\\/P", "", acc_ped_file)),
        id = str_split(id_type, "_", simplify = TRUE)[1],
        loc = str_split(id_type, "_", simplify = TRUE)[, 2],
        ind = row_number()
      )
    id = temp$id[1]
    loc = paste0("/", temp$loc[1]) # to distinguish regular and semi regular
    loc = ifelse(loc == "/Semiregular", "/SemiRegular", loc) # annoying difference in spelling
    step_file = step_files[grep(id, step_files)][grep(loc,step_files[grep(id, step_files)])]
    step_locs = read_table(step_file, col_names = c("ind", "foot"))

    temp =
      temp %>%
      left_join(step_locs) %>%
      mutate(ind_step = case_when(is.na(foot) ~ 0,
                                  foot == "right" ~ 1,
                                  foot == "left" ~ 1,
                                  TRUE ~ 0)) %>%
      mutate(
        time_s = (ind - 1) / 15,
        time = start + as.period(time_s, unit = "seconds"),
        cat_activity = paste0("walk_", tolower(loc)),
        cat_step_type = ifelse(is.na(foot), "none", foot),
        id_study = "clemson",
        id_subject = sprintf("P%02.f", as.numeric(id)),
        sample_rate = 15
      ) %>%
      select(
        X = wrist_x,
        Y = wrist_y,
        Z = wrist_z,
        tm_dttm = time,
        id_subject,
        ind_step,
        cat_step_type,
        cat_activity,
        id_study,
        sample_rate
      )
    id_new = temp$id_subject[1]
    activity =  temp$cat_activity[1]
    fname = paste0("clemson-", id_new, "-", activity,
                   "-", "raw15Hz.csv.gz")
    if(!file.exists(here::here("data", "reorganized", "clemson", id_new))){
      dir.create(here::here("data", "reorganized", "clemson", id_new))
    }
    readr::write_csv(temp, here::here("data", "reorganized", "clemson", id_new, fname))
    temp
  }

ped_data <- map(.x = acc_ped_files,
                .f = get_and_join_files) %>%
  bind_rows()


write_csv(ped_data, here::here("data/processed/clemson.csv.gz"))

# eda
# ped_data %>%
#   filter(id_subject == "P01" & cat_activity == "walk_irregular") %>%
#   mutate(vm = sqrt(X^2  + Y^2  + Z^2)) %>%
#   ggplot(aes(x = tm_dttm, y = vm))+
#   geom_line()+
#   geom_point(aes(x = tm_dttm, y =ind_step))

system("rm clemsonped.zip")

