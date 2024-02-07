devtools::install_github("https://github.com/muschellij2/walking")

# set adept templates
all_wrist_templates = adeptdata::stride_template$left_wrist
template_list = do.call(rbind, all_wrist_templates)
template_list = apply(template_list, 1, identity, simplify = FALSE)

fit_adept = function(data, sample_rate, templates = template_list) {
  if (!"HEADER_TIME_STAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIME_STAMP = tm_dttm)
  }
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
    mutate(
      steps = ifelse(is.na(steps), 0, steps),
      time = lubridate::floor_date(HEADER_TIME_STAMP, unit = "seconds")
    ) %>%
    group_by(time) %>%
    summarize(steps_adept = sum(steps)) %>%
    select(time, steps_adept)
  message("adept completed")
  steps_bysecond
}

fit_sdt =
  function(data,
           sample_rate) {
    if (!"vm" %in% colnames(data)) {
      data = data %>%
        mutate(vm = sqrt(X ^ 2 + Y ^ 2 + Z ^ 2))
    }
    if (!"HEADER_TIME_STAMP" %in% colnames(data)) {
      data = data %>%
        rename(HEADER_TIME_STAMP = tm_dttm)
    }
    # vm threshold based on location
    srate = sample_rate
    walking::sdt_count_steps(data, sample_rate = srate) %>%
      rename(steps_sdt = steps)
  }

fit_oak = function(data) {
  if (!"HEADER_TIME_STAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIME_STAMP = tm_dttm)
  }
  oak_res =
    estimate_steps_forest(data) %>%
    rename(steps_oak = steps)

  message("oak completed")
  oak_res
}

fit_vs = function(data, sample_rate) {
  if (!"HEADER_TIME_STAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIME_STAMP = tm_dttm)
  }
  vs_res = estimate_steps_verisense(
    data,
    method = "revised",
    resample_to_15hz = FALSE,
    sample_rate = sample_rate
  ) %>%
    rename(steps_vs = steps)
  message("vs completed")
  vs_res
}

# function to get ground truth step count
get_truth = function(data) {
  if (!"HEADER_TIME_STAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIME_STAMP = tm_dttm)
  }
  truth = data %>%
    group_by(time = lubridate::floor_date(HEADER_TIME_STAMP)) %>%
    summarize(steps_truth = sum(ind_step, na.rm = TRUE))
  truth
}

