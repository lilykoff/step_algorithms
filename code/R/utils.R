devtools::install_github("https://github.com/muschellij2/walking")

# set adept templates
all_wrist_templates = adeptdata::stride_template$left_wrist
template_list = do.call(rbind, all_wrist_templates)
template_list = apply(template_list, 1, identity, simplify = FALSE)

fit_adept = function(data, sample_rate, templates = template_list) {
  if (!"HEADER_TIMESTAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIMESTAMP = tm_dttm)
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
      time = lubridate::floor_date(HEADER_TIMESTAMP, unit = "seconds")
    ) %>%
    group_by(time) %>%
    summarize(steps_adept = sum(steps)) %>%
    select(time, steps_adept)
  message("adept completed")
  steps_bysecond
}

estimate_steps_sdtnew = function(
    data,
    sample_rate,
    order = 4L,
    high = 0.25,
    low = 2.5,
    location = c("wrist", "waist"),
    verbose = TRUE
) {

  location = match.arg(location, choices = c("wrist", "waist"))
  threshold = ifelse(location == "wrist", 0.0359, 0.0267)

  # vm threshold based on location
  # create coefficients for a 4th order bandpass Butterworth filter
  b <- signal::butter(
    n = order,
    W = c(high, low) / (sample_rate / 2),
    type = "pass",
    plane = "z"
  )

  # demean and filter data with dual pass filter to avoid signal shift
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      vm = sqrt(X^2 + Y^2 + Z^2),
      demean_vm = vm - mean(vm),
      filt_vm = signal::filtfilt(b, demean_vm))

  # find indices in which the value immediately before and immediately
  # after the value is smaller and vm is above threshold
  data <- data %>%
    dplyr::mutate(peak =
                    filt_vm > dplyr::lag(filt_vm) &
                    filt_vm > dplyr::lead(filt_vm) &
                    filt_vm > threshold
    )

  if (verbose) {
    # return steps by second
    message("sdt completed")
  }
  data %>%
    dplyr::group_by(time = lubridate::floor_date(HEADER_TIMESTAMP)) %>%
    dplyr::summarize(steps = sum(peak, na.rm = TRUE))

}
fit_sdt_new =
  function(data,
           sample_rate,
           loc = "wrist") {
    if (!"vm" %in% colnames(data)) {
      data = data %>%
        mutate(vm = sqrt(X ^ 2 + Y ^ 2 + Z ^ 2))
    }
    if (!"HEADER_TIMESTAMP" %in% colnames(data)) {
      data = data %>%
        rename(HEADER_TIMESTAMP = tm_dttm)
    }
    # vm threshold based on location
    srate = sample_rate

    estimate_steps_sdtnew(data, sample_rate = srate, location = loc) %>%
      rename(steps_sdtnew = steps)
}

fit_sdt =
  function(data, sample_rate){
    if (!"vm" %in% colnames(data)) {
      data = data %>%
        mutate(vm = sqrt(X ^ 2 + Y ^ 2 + Z ^ 2))
    }
    if (!"HEADER_TIMESTAMP" %in% colnames(data)) {
      data = data %>%
        rename(HEADER_TIMESTAMP = tm_dttm)
    }
    # vm threshold based on location
    srate = sample_rate
    walking::estimate_steps_sdt(data, sample_rate = srate) %>%
      rename(steps_sdt = steps)
}
fit_oak = function(data) {
  if (!"HEADER_TIMESTAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIMESTAMP = tm_dttm)
  }
  oak_res =
    estimate_steps_forest(data) %>%
    rename(steps_oak = steps)

  message("oak completed")
  oak_res
}

fit_vs = function(data, sample_rate, resample = FALSE, method_type = "revised") {
  if (!"HEADER_TIMESTAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIMESTAMP = tm_dttm)
  }
  vs_res = estimate_steps_verisense(
    data,
    method = method_type,
    resample_to_15hz = resample,
    sample_rate = sample_rate
  ) %>%
    rename(steps_vs = steps)
  message("vs completed")
  vs_res
}

# function to get ground truth step count
get_truth = function(data) {
  if (!"HEADER_TIMESTAMP" %in% colnames(data)) {
    data = data %>%
      rename(HEADER_TIMESTAMP = tm_dttm)
  }
  truth = data %>%
    group_by(time = lubridate::floor_date(HEADER_TIMESTAMP)) %>%
    summarize(steps_truth = sum(ind_step, na.rm = TRUE))
  truth
}

