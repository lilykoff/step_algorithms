# run this script to download all raw files from the three datasets: clemson, marea, oxwalk
# process data into unified formats and output individual files to reorganized data folder

# clemson first
library(here)
library(purrr)
library(tidyverse)
library(readr)
# download here https://cecas.clemson.edu/~ahoover/pedometer/
url = "https://cecas.clemson.edu/tracking/Pedometer/Data.zip"

options(timeout = 300)
options(digits.secs = 3)
downloader::download(url, dest = "clemsonped.zip", mode = "wb")
unzip("clemsonped.zip", exdir = here::here("data/raw/clemson"))


ped_files = list.files(here::here("data/raw/clemson"),
                       recursive = TRUE,
                       full.names = TRUE)
acc_ped_files = ped_files[-grep("steps.txt", ped_files)]
step_files = ped_files[grep("steps.txt", ped_files)]
# 9 columns are wrist xyz, hip xyz and ankle xyz

start = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")

get_and_join_files =
  function(acc_ped_file) {
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
    step_file = step_files[grep(id, step_files)][grep(loc, step_files[grep(id, step_files)])]
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
    if (!file.exists(here::here("data", "reorganized", "clemson", id_new))) {
      dir.create(here::here("data", "reorganized", "clemson", id_new))
    }
    readr::write_csv(temp,
                     here::here("data", "reorganized", "clemson", id_new, fname))
    temp
  }

ped_data = map(.x = acc_ped_files,
               .f = get_and_join_files) %>%
  bind_rows()


write_csv(ped_data, here::here("data/processed/clemson.csv.gz"))

# system("rm clemsonped.zip")
rm(list = ls())

# marea data
library(readr)
library(purrr)
library(tidyverse)

options(digits.secs = 3)
# download raw data from https://wiki.hh.se/caisr/index.php/Gait_database
# sign use agreement first

# get files from wrist
sub_files = list.files(
  here::here("data/raw/MAREA_dataset/Subject Data_txt format"),
  recursive = TRUE,
  full.names = TRUE
)
sub_files = sub_files[grepl("Wrist", sub_files)]

# get activity start and ends
timings = list.files(
  here::here("data/raw/MAREA_dataset/Activity Timings"),
  recursive = TRUE,
  full.names = TRUE
)
timings = timings[grepl(".txt", timings)]

# indoor
indoor = read_csv(
  here::here(
    "data/raw/MAREA_dataset/Activity Timings/Indoor Experiment Timings.txt"
  ),
  col_names = FALSE
)

# column names (from readme)
colnames(indoor) = c(
  "start_treadmill_walk",
  "end_treadmill_walk",
  "end_treadmill_run",
  "start_treadmill_slopewalk",
  "end_treadmill_slopewalk",
  "start_indoor_walk",
  "end_indoor_walk",
  "end_indoor_run"
)
# add subject ID column and start of running
indoor = indoor %>%
  mutate(
    start_treadmill_run = end_treadmill_walk + 1,
    start_indoor_run = end_indoor_walk + 1,
    id_subject = seq(1, 11, 1)
  )
indoor = indoor %>%
  pivot_longer(cols = -id_subject)

# outdoor start/end times
outdoor = read_csv(
  here::here(
    "data/raw/MAREA_dataset/Activity Timings/Outdoor Experiment Timings.txt"
  ),
  col_names = FALSE
)
colnames(outdoor) = c("start_outdoor_walk", "end_outdoor_walk",
                      "end_outdoor_run")
outdoor = outdoor %>%
  mutate(start_outdoor_run = end_outdoor_walk + 1,
         id_subject = seq(12, 20, 1))
outdoor = outdoor %>%
  pivot_longer(cols = -id_subject)

start_ends =
  bind_rows(indoor, outdoor) %>%
  mutate(type = sub("_.*", "", name),
         activity = sub("^([^_]+)_", "", name)) %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value)

start_ends_long =
  start_ends %>%
  group_by(id_subject, activity) %>%
  tidyr::expand(index = seq(start, end, 1))


marea_dat = map(
  .x = sub_files,
  .f = function(file) {
    data = readr::read_csv(file) %>%
      rename(X = accX,
             Y = accY,
             Z = accZ) %>%
      mutate(index = row_number(),
             id_subject = as.numeric(sub(".*Sub(.+)\\_Wrist.*", "\\1", file)))
  }
) %>%
  bind_rows()

marea_dat_labeled = marea_dat %>%
  left_join(start_ends_long, by = c("id_subject", "index"))

# SubIdx - Subject number. Ranges from Sub1 - Sub20
# LF_HS - Sample numbers of Heel-Strike event from Left Foot FSR signal.
# LF_TO - Sample numbers of Toe-Off event from Left Foot FSR signal.
# RF_HS - Sample numbers of Heel-Strike event from Right Foot FSR signal.
# RF_TO - Sample numbers of Toe-Off event from Right Foot FSR signal.

# for our purposes, we want to count left foot heel strike or right foot heel strike
mat_dat = R.matlab::readMat(here::here("data/raw/MAREA_dataset/GroundTruth.mat"))

# need to do this somewhat manually just bc of matlab nested structure
# pull left and right foot heelstrike and indices (which are by activity)
new_list = list()
for (subject in c(seq(1, 68, 1), seq(71, 75, 1))) {
  temp  = mat_dat[[1]][[subject]]
  id = temp[[1]] %>% data.frame() %>%  pull(.)
  lf_hs = temp[[2]][, 1] %>%
    data.frame() %>%
    mutate(type = "left_heelstrike") %>%
    rename(activity_index = ".")
  rf_hs = temp[[4]][, 1] %>%
    data.frame() %>%
    mutate(type = "right_heelstrike") %>%
    rename(activity_index = ".")
  df = bind_rows(lf_hs, rf_hs) %>%
    mutate(id_subject = id,
           key = subject)
  new_list[[subject]] = df
}

gait_events = bind_rows(new_list) %>%
  mutate(id_subject = as.numeric(sub(".*Sub", "", id_subject)))

# get key for activities
key_df = gait_events %>%
  select(id_subject, key) %>%
  distinct() %>%
  ungroup() %>%
  arrange(id_subject)

# for subs 1-11, indoor activities
# subs 12-20, outdoor activities
act_key = c(rep(
  c(
    "treadmill_walk",
    "treadmill_slopewalk",
    "treadmill_walkrun",
    "indoor_walk",
    "indoor_walkrun"
  ),
  11
),
rep(c("outdoor_walk", "outdoor_walkrun"), 9))

key_df$activity = act_key
gait_events = gait_events %>%
  left_join(key_df, by = c("id_subject", "key")) %>%
  select(-key)

# get rid of treadmill_walk and indoor_walk and outdoor_walk bc redundant
gait_events_small =
  gait_events %>% filter(activity != "treadmill_walk" &
                           activity != "indoor_walk" &
                           activity != "outdoor_walk") %>%
  rename(activity_large = activity)

# join w events by making "larger activities" and indices
marea_dat_labeled_steps =
  marea_dat_labeled %>%
  mutate(
    activity_large = case_when(
      (activity == "treadmill_walk" |
         activity == "treadmill_run") ~ "treadmill_walkrun",
      (activity == "indoor_walk" |
         activity == "indoor_run") ~ "indoor_walkrun",
      (activity == "outdoor_walk" |
         activity == "outdoor_run") ~ "outdoor_walkrun",
      TRUE ~ activity
    )
  ) %>%
  group_by(id_subject, activity_large) %>%
  mutate(activity_index = row_number()) %>%
  left_join(gait_events_small,
            by = c("id_subject", "activity_index", "activity_large"))


# marea_dat_labeled_steps %>%
#   filter(id_subject == 1 & activity == "indoor_walk") %>%
#   slice(1:1000) %>%
#   mutate(step = ifelse(is.na(type), 0, 1)) %>%
#   ggplot(aes(x = index, y = sqrt(X^2  +Y^2 + Z^2)))+
#   geom_line()+
#   geom_point(aes(x = index, y = step, col = as.factor(type)))

start_time = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")
# finally, add fake timestamps
# sample rate is 128 Hz

marea_dat_labeled_steps =
  marea_dat_labeled_steps %>%
  mutate(
    time_s = (activity_index - 1) / 128,
    time_min = floor(time_s / 60),
    tm_dttm = start_time + as.period(time_s, unit = "seconds")
  )



## add in speeds and inclines

marea_dat_labeled_steps =
  marea_dat_labeled_steps %>%
  mutate(
    speed = case_when(
      activity_large == "treadmill_walkrun" & time_min == 0 ~ "4.0 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 1 ~ "4.4 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 2 ~ "4.8 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 3 ~ "5.2 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 4 ~ "5.6 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 5 ~ "6.0 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 6 ~ "6.4 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 7 ~ "6.8 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 8 ~ "7.2 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min == 9 ~ "7.6 km/hr",
      activity_large == "treadmill_walkrun" &
        time_min >= 10 ~ "8.0 km/hr",
      TRUE ~ "self_selected"
    ),
    slope = case_when(
      activity_large == "treadmill_slopewalk" &
        time_min %in% c(0, 1) ~ "5 deg",
      activity_large == "treadmill_slopewalk" &
        time_min %in% c(4, 5) ~ "10 deg",
      activity_large == "treadmill_slopewalk" &
        time_min %in% c(8, 9) ~ "15 deg",
      TRUE ~ "0 deg"
    )
  )

final_dat =
  marea_dat_labeled_steps %>%
  filter(!is.na(activity)) %>%
  rename(
    cat_activity = activity,
    cat_activity_large = activity_large,
    num_row_subject = index,
    num_row_subject_activity = activity_index,
    cat_step_type = type,
    tm_time_s = time_s,
    tm_time_min = time_min
  ) %>%
  mutate(
    sample_rate = 128,
    ind_step = ifelse(is.na(cat_step_type), 0, 1),
    id_study = "marea"
  ) %>%
  mutate(across(X:Z,  ~ .x * 0.101971621)) %>%  # convert to g
  mutate(id_subject = sprintf("P%02.f", id_subject)) %>%
  select(-c(num_row_subject, num_row_subject_activity, tm_time_s))

final_dat %>%
  group_by(cat_activity_large, id_subject) %>%
  summarize(n = n () / (128 * 60))

# do some basic EDA
# final_dat %>%
#   filter(id_subject == "P20" & cat_activity == "outdoor_run") %>%
#   mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
#   ggplot(aes(x= tm_dttm, y = vm))+
#   geom_line()+
#   geom_point(aes(x = tm_dttm, y = ind_step))


write_csv(final_dat, here::here("data/processed/marea.csv.gz"))

# split and write like other data

marea_list = split(final_dat,
                   f = list(final_dat$id_subject,
                            final_dat$cat_activity_large)) %>%
  vctrs::list_drop_empty()

lapply(marea_list, function(item) {
  id_new = item$id_subject[1]
  activity =  item$cat_activity_large[1]
  fname = paste0("marea-", id_new, "-", activity,
                 "-", "raw128Hz.csv.gz")
  if (!file.exists(here::here("data", "reorganized", "marea", id_new))) {
    dir.create(here::here("data", "reorganized", "marea", id_new))
  }
  readr::write_csv(item,
                   here::here("data", "reorganized", "marea", id_new, fname))
})

rm(list = ls())

# oxwalk data

library(here)
library(purrr)
library(tidyverse)
library(readr)
options(digits.secs = 3)
# data download here https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7

url = "https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7/files/dcj82k7829"

options(timeout = 300)
downloader::download(url, dest = "oxwalk.zip", mode = "wb")
unzip("oxwalk.zip", exdir = here::here("data/raw/oxwalk"))


wrist_files_100 = list.files(
  here::here("data/raw/oxwalk/OxWalk_Dec2022/Wrist_100Hz"),
  recursive = TRUE,
  full.names = TRUE
)
wrist_files_25 = list.files(
  here::here("data/raw/oxwalk/OxWalk_Dec2022/Wrist_25Hz"),
  recursive = TRUE,
  full.names = TRUE
)



get_and_join_files =
  function(wrist_file) {
    id = sub(".*Hz\\/(.+)\\_wrist.*", "\\1", wrist_file)
    sample_rate = as.numeric(sub(".*wrist(.+).csv.*", "\\1", wrist_file))
    temp =
      readr::read_csv(wrist_file) %>%
      select(
        tm_dttm = timestamp,
        X = x,
        Y = y,
        Z = z,
        ind_step = annotation
      ) %>%
      mutate(id_study = "oxwalk",
             id_subject = id,
             sample_rate = sample_rate)
    fname = paste0("oxwalk-", id,
                   "-", "raw", sample_rate, "Hz.csv.gz")
    if (!file.exists(here::here("data", "reorganized", "oxwalk", id))) {
      dir.create(here::here("data", "reorganized", "oxwalk", id))
    }
    readr::write_csv(temp, here::here("data", "reorganized", "oxwalk", id, fname))
    temp
  }

ox_data = map(.x = c(wrist_files_100, wrist_files_25),
              .f = get_and_join_files) %>%
  bind_rows()


write_csv(ox_data, here::here("data/processed/ox_data.csv.gz"))

# system("rm oxwalk.zip")
