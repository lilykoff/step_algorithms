library(readr)
library(purrr)
library(tidyverse)

options(digits.secs = 3)
# download raw data from https://wiki.hh.se/caisr/index.php/Gait_database
# sign use agreement first

# get files from wrist
sub_files = list.files(here::here("data/raw/MAREA_dataset/Subject Data_txt format"),
                       recursive = TRUE,
                       full.names = TRUE)
sub_files = sub_files[grepl("Wrist", sub_files)]

# get activity start and ends
timings = list.files(here::here("data/raw/MAREA_dataset/Activity Timings"),
                     recursive = TRUE,
                     full.names = TRUE)
timings = timings[grepl(".txt", timings)]

# indoor
indoor = read_csv(here::here("data/raw/MAREA_dataset/Activity Timings/Indoor Experiment Timings.txt"),
                  col_names = FALSE)

# column names (from readme)
colnames(indoor) = c("start_treadmill_walk",
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
  mutate(start_treadmill_run = end_treadmill_walk+1,
         start_indoor_run = end_indoor_walk+1,
         id_subject = seq(1,11,1))
indoor = indoor %>%
  pivot_longer(cols = -id_subject)

# outdoor start/end times
outdoor = read_csv(here::here("data/raw/MAREA_dataset/Activity Timings/Outdoor Experiment Timings.txt"),
                   col_names = FALSE)
colnames(outdoor) = c("start_outdoor_walk", "end_outdoor_walk",
                      "end_outdoor_run")
outdoor = outdoor %>%
  mutate(start_outdoor_run = end_outdoor_walk + 1,
         id_subject = seq(12,20,1))
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
for(subject in c(seq(1, 68, 1), seq(71,75,1))){
  temp  = mat_dat[[1]][[subject]]
  id = temp[[1]] %>% data.frame() %>%  pull(.)
  lf_hs = temp[[2]][,1] %>%
    data.frame() %>%
    mutate(type = "left_heelstrike") %>%
    rename(activity_index = ".")
  rf_hs = temp[[4]][,1] %>%
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
act_key = c(rep(c("treadmill_walk", "treadmill_slopewalk",
                  "treadmill_walkrun", "indoor_walk",
                  "indoor_walkrun"), 11),
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
  mutate(activity_large = case_when(
    activity == "treadmill_walk" | activity == "treadmill_run" ~ "treadmill_walkrun",
    activity == "indoor_walk" | activity == "indoor_run" ~ "indoor_walkrun",
    activity == "outdoor_walk"| activity == "outdoor_run" ~ "outdoor_walkrun",
    TRUE ~ activity)) %>%
  group_by(id_subject, activity_large) %>%
  mutate(activity_index = row_number()) %>%
  left_join(gait_events_small, by = c("id_subject", "activity_index", "activity_large"))


start_time = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")
# finally, add fake timestamps
# sample rate is 128 Hz

marea_dat_labeled_steps =
  marea_dat_labeled_steps %>%
  mutate(time_s = (activity_index-1) / 128,
         tm_dttm = start_time + as.period(time_s, unit = "seconds"))

final_dat =
  marea_dat_labeled_steps %>%
  rename(cat_activity = activity,
         cat_activity_large = activity_large,
         num_row_subject = index,
         num_row_subject_activity = activity_index,
         cat_step_type = type,
         tm_time_s = time_s) %>%
  mutate(sample_rate = 128,
         ind_step = ifelse(is.na(cat_step_type), 0, 1),
         id_study = "marea") %>%
  mutate(across(X:Z,  ~ .x * 0.101971621)) %>%  # convert to g
  mutate(id_subject = sprintf("P%02.f", id_subject)) %>%
  select(-c(num_row_subject, num_row_subject_activity, tm_time_s))


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
                            final_dat$cat_activity)) %>%
  vctrs::list_drop_empty()

lapply(marea_list, function(item){
  id_new = item$id_subject[1]
  activity =  item$cat_activity[1]
  fname = paste0("marea-", id_new, "-", activity,
                 "-", "raw128Hz.csv.gz")
  if(!file.exists(here::here("data", "reorganized", "marea", id_new))){
    dir.create(here::here("data", "reorganized", "marea", id_new))
  }
  readr::write_csv(item, here::here("data", "reorganized", "marea", id_new, fname))
})
