
# set adept templates
all_wrist_templates = adeptdata::stride_template$left_wrist
template_list = do.call(rbind, all_wrist_templates)
template_list = apply(template_list, 1, identity, simplify = FALSE)

estimate_steps_adept = function(data, sample_rate, templates){
  step_result = adept::segmentWalking(
    xyz = data[, c("X", "Y", "Z")],
    xyz.fs = sample_rate,
    template = templates,
    compute.template.idx = FALSE,
    run.parallel = TRUE,
    run.parallel.cores = 8,
    sim_MIN = 0.6,
    dur_MIN = 0.8,
    dur_MAX = 1.4,
    ptp_r_MIN = 0.5,
    ptp_r_MAX = 2,
    vmc_r_MIN = 0.05,
    vmc_r_MAX = 0.5,
    mean_abs_diff_med_p_MAX = 0.7,
    mean_abs_diff_med_t_MAX = 0.2,
    mean_abs_diff_dur_MAX = 0.3
  ) %>%
    filter(is_walking_i == 1) %>%
    mutate(steps = 2 / (T_i / sample_rate))
  steps_bysecond =
    data %>%
    mutate(row_ind = row_number()) %>%
    left_join(., step_result, by = c("row_ind" = "tau_i")) %>%
    mutate(steps = ifelse(is.na(steps), 0, steps),
           time = lubridate::floor_date(HEADER_TIME_STAMP, unit = "seconds")) %>%
    group_by(time) %>%
    summarize(steps = sum(steps))
}
estimate_steps_sdt <-
  function(data, sample_rate, order = 4, high = 0.25, low = 2.5, loc = "wrist"){
    if(!"vm" %in% colnames(data)){
      data = data %>%
        mutate(vm = sqrt(X^2 + Y^2 + Z^2))
    }
    # vm threshold based on location
    threshold = ifelse(loc == "wrist", 0.0267, 0.0359)
    # create coefficients for a 4th order bandpass Butterworth filter
    b <-
      signal::butter(
        n = order,
        W = c(high, low) / (sample_rate / 2),
        type = 'pass',
        plane = c('z')
      )

    # demean and filter data with dual pass filter to avoid signal shift
    data <-
      data %>%
      mutate(demean_vm = vm - mean(vm),
             filt_vm = signal::filtfilt(b, demean_vm))

    # find indices in which the value immediately before and immediately after the value is smaller
    # and vm is above threshold
    data <-
      data %>%
      mutate(peak = ifelse(
        filt_vm > lag(filt_vm) &
          filt_vm > lead(filt_vm) &
          filt_vm > threshold,
        1,
        0
      ))

    # return steps by second

    data %>%
      group_by(time = lubridate::floor_date(HEADER_TIME_STAMP)) %>%
      summarize(steps = sum(peak, na.rm = TRUE))
  }
fit_all_algorithms = function(data) {
  sample_rate = data$sample_rate[1]
  id_subject = data$id_subject[1]
  id_study = data$id_study[1]
  cat_activity = ifelse("cat_activity" %in% colnames(data), data$cat_activity[1], NA)
  if (!"HEADER_TIME_STAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIME_STAMP = tm_dttm)
  }

  oak_res = estimate_steps_forest(data) %>%
    rename(steps_oak = steps)
  message("oak completed")
  vs_res = estimate_steps_verisense(data,
                                    method = "revised",
                                    sample_rate = sample_rate) %>%
    rename(steps_vs = steps)
  message("vs completed")
  adept_res = estimate_steps_adept(data,
                                   sample_rate = sample_rate,
                                   templates = template_list) %>%
    rename(steps_adept = steps)
  message("adept completed")
  sdt_res = estimate_steps_sdt(data,
                               sample_rate = sample_rate) %>%
    rename(steps_sdt = steps)
  message("sdt completed")
  truth = data %>%
    group_by(time = lubridate::floor_date(HEADER_TIME_STAMP)) %>%
    summarize(steps_truth = sum(ind_step, na.rm = TRUE))
  out =
    oak_res %>%
    full_join(vs_res) %>%
    full_join(adept_res) %>%
    full_join(sdt_res) %>%
    full_join(truth) %>%
    mutate(id_subject = id_subject,
           id_study = id_study,
           cat_activity = cat_activity,
           sample_rate = sample_rate)
  out
}

fit_all_algorithms_resampled = function(data) {
  sample_rate = data$sample_rate[1]
  id_subject = data$id_subject[1]
  id_study = data$id_study[1]
  sample_rate_old = data$sample_rate_old[1]
  cat_activity = ifelse("cat_activity" %in% colnames(data), data$cat_activity[1], NA)
  if (!"HEADER_TIME_STAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIME_STAMP = tm_dttm)
  }

  oak_res = estimate_steps_forest(data) %>%
    rename(steps_oak = steps)
  message("oak completed")
  vs_res = estimate_steps_verisense(data,
                                    method = "revised",
                                    sample_rate = sample_rate) %>%
    rename(steps_vs = steps)
  message("vs completed")
  adept_res = estimate_steps_adept(data,
                                   sample_rate = sample_rate,
                                   templates = template_list) %>%
    rename(steps_adept = steps)
  message("adept completed")
  sdt_res = estimate_steps_sdt(data,
                               sample_rate = sample_rate) %>%
    rename(steps_sdt = steps)
  message("sdt completed")

  out =
    oak_res %>%
    full_join(vs_res) %>%
    full_join(adept_res) %>%
    full_join(sdt_res) %>%
    mutate(id_subject = id_subject,
           id_study = id_study,
           cat_activity = cat_activity,
           sample_rate = sample_rate,
           sample_rate_old = sample_rate_old)
  out
}

process_sc_results = function(file){
  df = readr::read_csv(file)
  id_study = ifelse(grepl("egular", file), "clemson_ped","oxwalk")
  id_subject =  ifelse(grepl("egular", file),
                       str_split(sub(".*acc\\_(.+)\\_.*", "\\1", file), "_")[[1]][1],
                       sub(".*acc\\_(.+)\\_.*", "\\1", file))
  cat_activity = ifelse(grepl("egular", file),
                        paste0("walk_", tolower(sub(".*\\_(.+)\\_15.*", "\\1", file))),
                        NA)
  sample_rate = ifelse(grepl("egular", file),
                       15,
                       sub(".*_(.+)Hz.*", "\\1", file))
  df %>%
    rename(steps_sc = Steps) %>%
    mutate(id_study = id_study,
           id_subject = id_subject,
           cat_activity = cat_activity,
           sample_rate = sample_rate)
}

process_sc_results_resampled = function(file){
  df = readr::read_csv(file)
  id_study = ifelse(grepl("egular", file), "clemson_ped","oxwalk")
  id_subject =  ifelse(grepl("egular", file),
                       str_split(sub(".*acc\\_(.+)\\_.*", "\\1", file), "_")[[1]][1],
                       sub(".*acc\\_(.+)\\_.*", "\\1", file))
  cat_activity = ifelse(grepl("egular", file),
                        paste0("walk_", tolower(sub(".*\\_(.+)\\_15to.*", "\\1", file))),
                        NA)
  sample_rate = 30
  sample_rate_old =  ifelse(grepl("egular", file),
                       15,
                       sub(".*_(.+)to30.*", "\\1", file))
  df %>%
    rename(steps_sc = Steps) %>%
    mutate(id_study = id_study,
           id_subject = id_subject,
           cat_activity = cat_activity,
           sample_rate = sample_rate,
           sample_rate_old = sample_rate_old)
}


process_acti_results = function(file, clem, ox){
  id_study = ifelse(grepl("egular", file), "clemson_ped", "oxwalk")
  subject =  ifelse(grepl("egular", file),
                      sub(".*P(.+)\\_.*", "\\1", file),
                      sub(".*acc\\_(.+)\\_.*", "\\1", file))
  activity = ifelse(grepl("egular", file),
                        paste0("walk_", tolower(sub(".*\\_(.+)1sec.*", "\\1", file))),
                        NA)
  rate = ifelse(grepl("egular", file),
                       15,
                       sub(".*wrist(.+)1sec.*", "\\1", file))
  start_time = ifelse(id_study == "clemson_ped",
                      clem %>% filter(id_subject == subject &
                                      cat_activity == activity) %>%
                        arrange(time) %>%
                        slice(1) %>%
                        select(time),
           ox %>% filter(id_subject == subject &
                         sample_rate == rate) %>%
             arrange(time) %>%
             slice(1) %>% select(time))[[1]]

  readr::read_csv(file,
                  col_names = c("X", "Y", "Z", "steps_acti"),
                  skip = 10) %>%
    mutate(
      id_subject = subject,
      cat_activity = activity,
      sample_rate = rate,
      time = lubridate::floor_date(start_time + as.period(row_number()-1, unit = "seconds"),
                                   unit = "1 seconds")
    ) %>%
    select(-c(X,Y,Z))


}

process_acti_results_resampled = function(file, clem, ox){
  id_study = ifelse(grepl("egular", file), "clemson_ped", "oxwalk")
  subject =  ifelse(grepl("egular", file),
                    str_split(sub(".*acc\\_(.+)\\_.*", "\\1", file), "_")[[1]][1],
                    sub(".*acc\\_(.+)\\_.*", "\\1", file))
  activity = ifelse(grepl("egular", file),
                    paste0("walk_", tolower(sub(".*\\_(.+)\\_15to.*", "\\1", file))),
                    NA)
  rate_orig = ifelse(grepl("egular", file),
                     15,
                     sub(".*\\_(.+)to30.*", "\\1", file))
  rate_new = 30
  start_time = ifelse(id_study == "clemson_ped",
                      clem %>% filter(id_subject == subject &
                                        cat_activity == activity) %>%
                        arrange(time) %>%
                        slice(1) %>%
                        select(time),
                      ox %>% filter(id_subject == subject &
                                      sample_rate == rate_orig) %>%
                        arrange(time) %>%
                        slice(1) %>% select(time))[[1]]

  readr::read_csv(file,
                  col_names = c("X", "Y", "Z", "steps_acti"),
                  skip = 10) %>%
    mutate(
      id_subject = subject,
      cat_activity = activity,
      sample_rate = rate_new,
      sample_rate_old = rate_orig,
      time = lubridate::floor_date(start_time + as.period(row_number()-1, unit = "seconds"),
                                   unit = "1 seconds")
    ) %>%
    select(-c(X,Y,Z))

}
