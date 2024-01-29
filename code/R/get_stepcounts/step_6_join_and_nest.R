library(tidyverse)
# step six join and nest
# read in raw files, nest datetime, then join with steps
all_clem_files =  list.files(here::here("data", "reorganized", "clemson"), full.names = TRUE,
                             recursive = TRUE)
ids = sub(".*clemson\\-(.+)\\-walk.*", "\\1", all_clem_files) %>% unique()

all_clem =
  map(.x = ids,
    .f = function(id){
      if(!file.exists(here::here("data", "processed", "clemson", id))){
        dir.create(here::here("data", "processed", "clemson", id))
      }
      subj_files = list.files(here::here("data", "reorganized", "clemson", id),
                              full.names = TRUE,
                            recursive = TRUE)
      activities = paste0("walk_", sub(".*walk\\_(.+)\\-r.*", "\\1", subj_files)) %>% unique()
      map(.x = activities,
          .f = function(activity){
            sa_files = subj_files[grepl(activity, subj_files)]
            raw_file =  sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("raw", sa_files) == TRUE]
            resamp_file = sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("resampled", sa_files) == TRUE]

            raw_df =
              readr::read_csv(raw_file)  %>%
              mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
              tidyr::nest(raw_data = c(tm_dttm, X, Y, Z, ind_step, cat_step_type)) %>%
              select(-c(sample_rate))

            resampled_df =
              readr::read_csv(resamp_file) %>%
              mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
              tidyr::nest(resampled_data = c(tm_dttm, X, Y, Z)) %>%
              select(-c(sample_rate))

            both =
              left_join(raw_df, resampled_df, by = c("time", "id_subject", "id_study", "cat_activity"))

            step_files = sa_files[grepl("steps", sa_files)]
            step_files_raw = step_files[grepl("raw", step_files)]
            step_files_resamp = step_files[grepl("resampled", step_files)]
            # get actilife - only resampled data for now - need to make same format as others
            acti_file = step_files_resamp[grepl("acti", step_files_resamp)]
            acti = readr::read_csv(acti_file)
            start_time = floor_date(min(raw_df$time), unit = "seconds")

            acti = acti %>%
              mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
              select(time, steps_acti)

            df = map(step_files_raw,
                     .f = function(x){
                       readr::read_csv(x) %>%
                       right_join(raw_df %>% select(time))}) %>%
              Reduce(left_join, .)


            df_resampled = map(step_files_resamp[-grep("acti", step_files_resamp)],
                               .f = function(x){
                                 readr::read_csv(x) %>%
                                   right_join(raw_df %>% select(time))}) %>%
              Reduce(left_join, .) %>%
              left_join(acti) %>%
              rename_with(~str_c(., "_30"), .cols = starts_with("steps"))

            step_df = left_join(df, df_resampled, by = "time")
            both_w_steps = left_join(both, step_df, by = "time")
            fname = paste0("clemson-", id, "-", activity, "-nested.rds")
            saveRDS(both_w_steps, here::here("data", "processed", "clemson", id,
                                             fname))
            rm(df)
            rm(df_resampled)
            rm(both)
            both_w_steps
          })
    }) %>%
  bind_rows()
saveRDS(all_clem, here::here("data/processed", "clemson_nested_all.rds"))
rm(all_clem)

all_oxwalk_files =  list.files(here::here("data", "reorganized", "oxwalk"), full.names = TRUE,
                             recursive = TRUE)

ids = sub(".*oxwalk-(.+)\\-r.*", "\\1", all_oxwalk_files) %>% unique()
all_ox =
  map(.x = ids,
      .f = function(id){
        if(!file.exists(here::here("data", "processed", "oxwalk", id))){
          dir.create(here::here("data", "processed", "oxwalk", id))
        }
        subj_files = list.files(here::here("data", "reorganized", "oxwalk", id),
                                full.names = TRUE,
                                recursive = TRUE)
        sample_rates = c(25,100)
        map(.x = sample_rates,
            .f = function(sample_rate){
              sa_files = subj_files[grepl(paste0(sample_rate, "to"), subj_files)|
                                      grepl(paste0("raw",sample_rate), subj_files)]
              raw_file =  sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("raw", sa_files) == TRUE]
              resamp_file = sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("resampled", sa_files) == TRUE]
              raw_df =
                readr::read_csv(raw_file)  %>%
                mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
                tidyr::nest(raw_data = c(tm_dttm, X, Y, Z, ind_step)) %>%
                select(-c(sample_rate))

              resampled_df =
                readr::read_csv(resamp_file) %>%
                mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
                tidyr::nest(resampled_data = c(tm_dttm, X, Y, Z)) %>%
                select(-c(sample_rate))

              both =
                left_join(raw_df, resampled_df, by = c("time", "id_subject", "id_study")) %>%
                mutate(sample_rate = sample_rate)

              step_files = sa_files[grepl("steps", sa_files)]
              step_files_raw = step_files[grepl("raw", step_files)]
              step_files_resamp = step_files[grepl("resampled", step_files)]
              # get actilife - only resampled data for now - need to make same format as others
              acti_file = step_files_resamp[grepl("acti", step_files_resamp)]
              acti = readr::read_csv(acti_file)
              start_time = floor_date(min(raw_df$time), unit = "seconds")

              acti = acti %>%
                mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
                select(time, steps_acti)

              # sc_file_raw = step_files_raw[grepl("stepcount.csv", step_files_raw)]
              # sc_raw = readr::read_csv(sc_file_raw) %>%
              #   mutate(time = floor_date(time, unit = "seconds")) # need to floor sc to second level
              #
              # sc_file_raw_rf = step_files_raw[grepl("stepcountrf.csv", step_files_raw)]
              # sc_raw_rf = readr::read_csv(sc_file_raw_rf) %>%
              #   mutate(time = floor_date(time, unit = "seconds")) # need to floor sc to second level
              #
              #
              # sc_file_resamp = step_files_resamp[grepl("stepcount.csv", step_files_resamp)]
              # sc_resamp = readr::read_csv(sc_file_resamp) %>%
              #   mutate(time = floor_date(time, unit = "seconds")) # need to floor sc to second level
              #
              # sc_file_resamp_rf = step_files_resamp[grepl("stepcountrf.csv", step_files_resamp)]
              # sc_resamp_rf = readr::read_csv(sc_file_resamp_rf) %>%
              #   mutate(time = floor_date(time, unit = "seconds")) # need to floor sc to second level

              ad = readr::read_csv(step_files_raw[1])
              oak = readr::read_csv(step_files_raw[2])
              sdt = readr::read_csv(step_files_raw[3])
              sc = readr::read_csv(step_files_raw[4])
              scrf = readr::read_csv(step_files_raw[5])
              truth = readr::read_csv(step_files_raw[6])
              vs = readr::read_csv(step_files_raw[7])

              df = map(step_files_raw,
                       .f = function(x){
                         readr::read_csv(x) %>%
                           right_join(raw_df %>% select(time))}) %>%
                Reduce(left_join, .)
                # left_join(sc_raw) %>%
                # left_join(sc_raw_rf)

              df_resampled = map(step_files_resamp[grepl("acti", step_files_resamp) == FALSE],
                                                     # grepl("stepcount", step_files_resamp)==FALSE],
                                 .f = function(x){
                                   readr::read_csv(x) %>%
                                     right_join(raw_df %>% select(time))}) %>%
                Reduce(left_join, .) %>%
                left_join(acti) %>%
                # left_join(sc_resamp) %>%
                # left_join(sc_resamp_rf) %>%
                rename_with(~str_c(., "_30"), .cols = starts_with("steps"))

              step_df = left_join(df, df_resampled, by = "time")
              both_w_steps = left_join(both, step_df, by = "time")
              fname = paste0("oxwalk-", id, "-", sample_rate, "Hz-nested.rds")
              saveRDS(both_w_steps, here::here("data", "processed", "oxwalk", id,
                                               fname))
              rm(df)
              rm(df_resampled)
              rm(both)
              both_w_steps
            })
      }) %>%
  bind_rows()

saveRDS(all_ox, here::here("data/processed", "oxwalk_nested_all.rds"))
rm(all_ox)

all_marea_files =  list.files(here::here("data", "reorganized", "marea"), full.names = TRUE,
                             recursive = TRUE)
ids = substr(sub(".*marea\\/(.+)\\/.*", "\\1", all_marea_files), 1, 3) %>% unique()
# find ./ -name "*indoor_run*" | xargs rm -r
# start here

all_marea =
  map(.x = ids,
      .f = function(id){
        if(!file.exists(here::here("data", "processed", "marea", id))){
          dir.create(here::here("data", "processed", "marea", id))
        }
        subj_files = list.files(here::here("data", "reorganized", "marea", id),
                                full.names = TRUE,
                                recursive = TRUE)
        activities = sub(".*\\-(.+)-r.*", "\\1", subj_files) %>% unique()
        map(.x = activities,
            .f = function(activity){
              sa_files = subj_files[grepl(activity, subj_files)]
              raw_file =  sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("raw", sa_files) == TRUE]
              resamp_file = sa_files[grepl(".csv.gz", sa_files) == TRUE & grepl("resampled", sa_files) == TRUE][1]
              raw_df =
                readr::read_csv(raw_file)  %>%
                mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
                tidyr::nest(raw_data = c(tm_dttm, X, Y, Z, ind_step, cat_step_type)) %>%
                select(-c(sample_rate))

              resampled_df =
                readr::read_csv(resamp_file) %>%
                mutate(time = floor_date(tm_dttm, unit = "seconds")) %>%
                tidyr::nest(resampled_data = c(tm_dttm, X, Y, Z)) %>%
                select(-c(sample_rate))

              both =
                left_join(raw_df, resampled_df, by = c("time", "id_subject", "id_study", "cat_activity", "cat_activity_large"))

              step_files = sa_files[grepl("steps", sa_files)]
              step_files_raw = step_files[grepl("raw", step_files)]
              step_files_resamp = step_files[grepl("resampled", step_files)]
              # get actilife - only resampled data for now - need to make same format as others
              acti_file = step_files_resamp[grepl("acti", step_files_resamp)]
              acti = readr::read_csv(acti_file)
              start_time = floor_date(min(raw_df$time), unit = "seconds")

              acti = acti %>%
                mutate(time = start_time + as.period(index-1, unit = "seconds")) %>%
                select(time, steps_acti)

              df = map(step_files_raw,
                       .f = function(x){
                         readr::read_csv(x) %>%
                           right_join(raw_df %>% select(time))}) %>%
                Reduce(left_join, .)

              df_resampled = map(step_files_resamp[-grep("acti", step_files_resamp)],
                                 .f = function(x){
                                   readr::read_csv(x) %>%
                                     right_join(raw_df %>% select(time))}) %>%
                Reduce(left_join, .) %>%
                left_join(acti) %>%
                rename_with(~str_c(., "_30"), .cols = starts_with("steps"))

              step_df = left_join(df, df_resampled, by = "time")
              both_w_steps = left_join(both, step_df, by = "time")
              fname = paste0("marea-", id, "-", activity, "-nested.rds")
              saveRDS(both_w_steps, here::here("data", "processed", "marea", id,
                                               fname))
              rm(df)
              rm(df_resampled)
              rm(both)
              both_w_steps
            })
      }) %>%
  bind_rows()
saveRDS(all_marea, here::here("data/processed", "marea_nested_all.rds"))
rm(all_marea)


# make datasets without nested data
# start here

clem = readRDS(here::here("data/processed/clemson_nested_all.rds")) %>%
  select(-c(raw_data, resampled_data)) %>%
  mutate(across(contains("rf"), ~ifelse(is.na(.x), 0, .x)))

readr::write_csv(clem, here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
clem10 = clem %>%
  mutate(time_10 = floor_date(time, unit = "10 seconds")) %>%
  group_by(time_10, id_subject, cat_activity) %>%
  mutate(n_seconds = n()) %>%
  group_by(id_subject, cat_activity, time_10, n_seconds) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE)))

readr::write_csv(clem10, here::here("results/all_algorithms/clemson_step_estimates_10sec.csv.gz"))


ox = readRDS(here::here("data/processed/oxwalk_nested_all.rds")) %>%
  select(-c(raw_data, resampled_data)) %>%
  mutate(across(contains("rf"), ~ifelse(is.na(.x), 0, .x)))

readr::write_csv(ox, here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))
ox10 = ox %>%
  mutate(time_10 = floor_date(time, unit = "10 seconds")) %>%
  group_by(time_10, id_subject, sample_rate) %>%
  mutate(n_seconds = n()) %>%
  group_by(id_subject, sample_rate, time_10, n_seconds) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE)))

readr::write_csv(ox10, here::here("results/all_algorithms/oxwalk_step_estimates_10sec.csv.gz"))

marea = readRDS(here::here("data/processed/marea_nested_all.rds")) %>%
  select(-c(raw_data, resampled_data)) %>%
  mutate(across(contains("rf"), ~ifelse(is.na(.x), 0, .x)))

readr::write_csv(marea, here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz"))
marea10 = marea %>%
  mutate(time_10 = floor_date(time, unit = "10 seconds")) %>%
  group_by(time_10, id_subject, cat_activity, cat_activity_large, slope, speed) %>%
  mutate(n_seconds = n()) %>%
  group_by(id_subject, cat_activity, cat_activity_large, slope, speed, time_10, n_seconds) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE)))

readr::write_csv(marea10, here::here("results/all_algorithms/marea_step_estimates_10sec.csv.gz"))

