library(tidyverse)


clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))
if(file.exists(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz"))){
  marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz")) %>%
    filter(grepl("run", cat_activity)==FALSE)



  # PLOTS truth vs predicted
  labs = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)",
           "Verisense (revised)")
  names(labs) = c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vsores", "vsrres")


  cor_df =
    oxwalk %>%
    filter(sample_rate == 25) %>%
    mutate(id_study = "oxwalk25", cat_activity2 = "ox") %>%
    bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
                mutate(id_study = "oxwalk100")) %>%
    bind_rows(marea %>% mutate(id_study = "marea")) %>%
    bind_rows(clemson %>% mutate(id_study = "clemson"))  %>%
    group_by(id_subject, id_study) %>%
    summarize(across(starts_with("steps"), ~sum(.x))) %>%
    pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
    select(id_subject, id_study, name, value) %>%
    mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
           method = ifelse(type == "resampled",
                           sub(".*steps\\_(.+)\\_.*", "\\1", name),
                           sub(".*\\_", "", name))) %>%
    ungroup() %>%
    select(-name) %>%
    pivot_wider(names_from = type, values_from = value,
                id_cols = c(id_subject, id_study, method))  %>%
    group_by(id_study, method) %>%
    summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
    ungroup() %>%
    pivot_wider(names_from = id_study, values_from = pearson)


  dat.labs = c("Clemson (15 Hz)", "MAREA (128 Hz)", "OxWalk (100 Hz)", "OxWalk (25 Hz)")
  names(dat.labs) = c("clemson", "marea", "oxwalk100", "oxwalk25")
  oxwalk %>%
    filter(sample_rate == 25) %>%
    mutate(id_study = "oxwalk25") %>%
    bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
                mutate(id_study = "oxwalk100")) %>%
    bind_rows(marea %>% mutate(id_study = "marea")) %>%
    bind_rows(clemson %>% mutate(id_study = "clemson")) %>%
    group_by(id_subject, cat_activity, id_study) %>%
    summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
    mutate(sample_rate = ifelse(grepl("30", name), "resampled", "original"),
           method = ifelse(sample_rate == "resampled",
                           sub(".*steps\\_(.+)\\_.*", "\\1", name),
                           sub(".*\\_", "", name))) %>%
    select(-name) %>%
    pivot_wider(names_from = sample_rate, values_from = value) %>%
    mutate(resampled =
             ifelse(method == "acti" & (id_study == "oxwalk25" | id_study == "clemson"), NA, resampled)) %>%
    ggplot(aes(x = original, y = resampled, col = method))+
    geom_point() +
    facet_grid(id_study ~ method)+
    # labeller = labeller(method = labs, id_study = dat.labs))+
    # scale_color_brewer(palette = "Dark2")+
    theme_bw()+
    geom_abline()+
    scale_x_continuous(limits = c(0, 6200))+
    scale_y_continuous(limits=c(0,6200))+
    theme(legend.position = "none",
          axis.title = element_text(size = 12))+
    labs(x = "Total Steps Estimated from Raw Data", y = "Total Steps Estimated from Data Resampled to 30 Hz")+
    coord_equal()+
    geom_text(data = cor_df %>%
                pivot_longer(cols = -method) %>% rename(id_study = name),
              aes(x = 2500, y = 5000,
                  label = paste("\u03c1 =", formatC(signif(value, digits=3), digits=2, format="fg", flag="#"))),
              inherit.aes = FALSE)

  # correlation table, between total steps estimated for ea person
  cor_df_separate =
    clemson %>%
    group_by(id_subject, cat_activity) %>%
    summarize(across(starts_with("steps"), ~sum(.x))) %>%
    pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
    select(id_subject, cat_activity, name, value) %>%
    mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
           method = ifelse(type == "resampled",
                           sub(".*steps\\_(.+)\\_.*", "\\1", name),
                           sub(".*\\_", "", name))) %>%
    ungroup() %>%
    select(-name) %>%
    pivot_wider(names_from = type, values_from = value,
                id_cols = c(id_subject, cat_activity, method))  %>%
    group_by(cat_activity,  method) %>%
    summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
    pivot_wider(names_from = cat_activity, values_from = pearson)

  # overall
  cor_df_overall =
    oxwalk %>%
    filter(sample_rate == 25) %>%
    mutate(id_study = "oxwalk25", cat_activity2 = "ox") %>%
    bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
                mutate(id_study = "oxwalk100")) %>%
    bind_rows(marea %>% mutate(id_study = "marea")) %>%
    bind_rows(clemson %>% mutate(id_study = "clemson"))  %>%
    group_by(id_subject, id_study) %>%
    summarize(across(starts_with("steps"), ~sum(.x))) %>%
    pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
    select(id_subject, id_study, name, value) %>%
    mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
           method = ifelse(type == "resampled",
                           sub(".*steps\\_(.+)\\_.*", "\\1", name),
                           sub(".*\\_", "", name))) %>%
    ungroup() %>%
    select(-name) %>%
    pivot_wider(names_from = type, values_from = value,
                id_cols = c(id_subject, id_study, method))  %>%
    group_by(method) %>%
    summarize(pearson = cor(raw, resampled, method = "pearson", use = "na.or.complete")) %>%
    mutate(id_study = "overall") %>%
    ungroup() %>%
    pivot_wider(names_from = id_study, values_from = pearson)

  cor_df %>%
    left_join(cor_df_separate) %>%
    left_join(cor_df_overall) %>%
    select(-starts_with("walk")) %>%
    filter(!grepl("res", method)) %>%
    mutate(method = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense, Original",
                      "Verisense, Revised")) %>%
    arrange(method) %>%
    magrittr::set_colnames(c("Algorithm", "Clemson (15Hz)", "MAREA (128Hz)",
                             "OxWalk (25Hz)", "OxWalk (100Hz)", "Overall")) %>%
    kableExtra::kable(digits = 3, align = "llll", format = "latex", booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = "scale_down")


  # bland altman type plots for stepcount and verisense to show impact of resampling

  summary_df =
    clemson %>%
    group_by(id_subject) %>%
    select(id_subject, contains("truth"), contains("sc")) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(type = ifelse(strsplit(name, "_")[[1]][3]=="30", "resampled", "raw"),
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  labs = c("Stepcount (RF)", "Stepcount (SSL)")
  names(labs) = c("scrf", "scssl")

  # means = summary_df  %>%
  #   group_by(algorithm) %>%
  #   summarize(mean = mean(value),
  #             sd  = sd(value),
  #             ub = mean + 1.96 * sd,
  #             lb = mean - 1.96 * sd)

  plt = ggplot(summary_df) +
    geom_point(aes(x = steps_truth, y = value, col = type), size = 2.5)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = labs)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "")+
    scale_color_manual(values = c("#5773CC", "#FFB900"), labels = c("15Hz", "30Hz"), name = "Sample Rate")+
    theme(legend.position = c(.2, .9),
          legend.text = element_text(size = 12),
          legend.margin = margin(1,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))

  svg(here::here("manuscript/figures", "bland_altman_sc.svg"))
  plt
  dev.off()
  clemson %>%
    group_by(id_subject) %>%
    select(id_subject, contains("truth"), contains("sc"), "steps_vsoraw", "steps_vsoraw_30", "steps_vsrraw", "steps_vsrraw_30") %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    pivot_longer(cols = steps_scssl:steps_vsrraw_30) %>%
    rowwise() %>%
    mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    pivot_wider(names_from=type,values_from=value,id_cols=c(id_subject,algorithm)) %>%
    ggplot(aes(x = raw,y=resampled, col = algorithm))+
    facet_grid(.~algorithm)+
    geom_point()+
    theme_bw()+
    geom_abline()

  summary_df =
    oxwalk %>% filter(sample_rate==100) %>%
    select(id_subject, id_study, contains("truth"), contains("vs")) %>%
    group_by(id_subject) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    select(-c(ends_with("res_30"))) %>%
    magrittr::set_colnames(c("id_subject", "steps_truth", "steps_orig_100hz", "steps_rev_100hz",
                             "steps_orig_15hz", "steps_rev_15hz", "steps_orig_30hz","steps_rev_30hz")) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(srate = strsplit(name, "_")[[1]][3],
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  meth = c("Original", "Revised")
  names(meth) = c("orig", "rev")
  summary_df %>%
    mutate(srate = factor(srate, levels = c("15hz", "30hz", "100hz"))) %>%
    ggplot() +
    geom_point(aes(x = steps_truth, y = value, col = srate), size = 2)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = meth)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "Bland Altman Plot for Steps Estimated by
       Verisense on Raw vs. Resampled Data")+
    scale_color_manual(values = c("#5773CC", "#FFB900", "#802268"),
                       name = "Sample Rate", labels = c("15", "30", "100")) +
    theme(legend.position = c(.2, .92),
          legend.text = element_text(size = 12),
          legend.margin = margin(0,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))

  summary_df =
    oxwalk %>% filter(sample_rate==25) %>%
    select(id_subject, id_study, contains("truth"), contains("vs")) %>%
    group_by(id_subject) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    select(-c(ends_with("res_30"))) %>%
    magrittr::set_colnames(c("id_subject", "steps_truth", "steps_orig_25hz", "steps_rev_25hz",
                             "steps_orig_15hz", "steps_rev_15hz", "steps_orig_30hz","steps_rev_30hz")) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(srate = strsplit(name, "_")[[1]][3],
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  meth = c("Original", "Revised")
  names(meth) = c("orig", "rev")
  summary_df %>%
    mutate(srate = factor(srate, levels = c("15hz", "25hz", "30hz"))) %>%
    ggplot() +
    geom_point(aes(x = steps_truth, y = value, col = srate), size = 2)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = meth)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "Bland Altman Plot for Steps Estimated by
       Verisense on Raw vs. Resampled Data")+
    scale_color_manual(values = c("#5773CC", "#FFB900", "#802268"),
                       name = "Sample Rate", labels = c("15", "25", "30")) +
    theme(legend.position = c(.2, .92),
          legend.text = element_text(size = 12),
          legend.margin = margin(0,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))


  summary_df =
    clemson %>%
    select(id_subject, id_study, contains("truth"), contains("vs")) %>%
    group_by(id_subject) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    select(-c(ends_with("res_30"), ends_with("res"))) %>%
    magrittr::set_colnames(c("id_subject", "steps_truth", "steps_orig_15hz", "steps_rev_15hz",
                             "steps_orig_30hz","steps_rev_30hz")) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(srate = strsplit(name, "_")[[1]][3],
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  meth = c("Original", "Revised")
  names(meth) = c("orig", "rev")
  plot = summary_df %>%
    mutate(srate = factor(srate, levels = c("15hz", "30hz"))) %>%
    ggplot() +
    geom_point(aes(x = steps_truth, y = value, col = srate), size = 2)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = meth)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "")+
    scale_color_manual(values = c("#5773CC", "#FFB900"),
                       name = "Sample Rate", labels = c("15", "30")) +
    theme(legend.position = c(.2, .92),
          legend.text = element_text(size = 12),
          legend.margin = margin(1,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))


  svg(here::here("manuscript/figures", "bland_altman_vs.svg"))
  plot
  dev.off()


  summary_df =
    marea %>%
    select(id_subject, id_study, contains("truth"), contains("vs")) %>%
    group_by(id_subject) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    select(-c(ends_with("res_30"))) %>%
    magrittr::set_colnames(c("id_subject", "steps_truth", "steps_orig_128hz", "steps_rev_128hz",
                             "steps_orig_15hz","steps_rev_15hz", "steps_orig_30hz", "steps_rev_30hz")) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(srate = strsplit(name, "_")[[1]][3],
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  meth = c("Original", "Revised")
  names(meth) = c("orig", "rev")
  summary_df %>%
    mutate(srate = factor(srate, levels = c("15hz", "30hz", "128hz"))) %>%
    ggplot() +
    geom_point(aes(x = steps_truth, y = value, col = srate), size = 2)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = meth)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "Bland Altman Plot for Steps Estimated by Verisense on Raw vs. Resampled Data")+
    scale_color_manual(values = c("#5773CC", "#FFB900", "#802268"),
                       name = "Sample Rate", labels = c("15", "30", "128")) +
    theme(legend.position = c(.2, .92),
          legend.text = element_text(size = 12),
          legend.margin = margin(1,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))
} else{



  # PLOTS truth vs predicted
  labs = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)",
           "Verisense (revised)")
  names(labs) = c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vsores", "vsrres")


  cor_df =
    oxwalk %>%
    filter(sample_rate == 25) %>%
    mutate(id_study = "oxwalk25", cat_activity2 = "ox") %>%
    bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
                mutate(id_study = "oxwalk100")) %>%
    bind_rows(clemson %>% mutate(id_study = "clemson"))  %>%
    group_by(id_subject, id_study) %>%
    summarize(across(starts_with("steps"), ~sum(.x))) %>%
    pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
    select(id_subject, id_study, name, value) %>%
    mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
           method = ifelse(type == "resampled",
                           sub(".*steps\\_(.+)\\_.*", "\\1", name),
                           sub(".*\\_", "", name))) %>%
    ungroup() %>%
    select(-name) %>%
    pivot_wider(names_from = type, values_from = value,
                id_cols = c(id_subject, id_study, method))  %>%
    group_by(id_study, method) %>%
    summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
    ungroup() %>%
    pivot_wider(names_from = id_study, values_from = pearson)


  dat.labs = c("Clemson (15 Hz)", "OxWalk (100 Hz)", "OxWalk (25 Hz)")
  names(dat.labs) = c("clemson",  "oxwalk100", "oxwalk25")
  oxwalk %>%
    filter(sample_rate == 25) %>%
    mutate(id_study = "oxwalk25") %>%
    bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
                mutate(id_study = "oxwalk100")) %>%
    bind_rows(clemson %>% mutate(id_study = "clemson")) %>%
    group_by(id_subject, cat_activity, id_study) %>%
    summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
    mutate(sample_rate = ifelse(grepl("30", name), "resampled", "original"),
           method = ifelse(sample_rate == "resampled",
                           sub(".*steps\\_(.+)\\_.*", "\\1", name),
                           sub(".*\\_", "", name))) %>%
    select(-name) %>%
    pivot_wider(names_from = sample_rate, values_from = value) %>%
    mutate(resampled =
             ifelse(method == "acti" & (id_study == "oxwalk25" | id_study == "clemson"), NA, resampled)) %>%
    ggplot(aes(x = original, y = resampled, col = method))+
    geom_point() +
    facet_grid(id_study ~ method)+
    # labeller = labeller(method = labs, id_study = dat.labs))+
    # scale_color_brewer(palette = "Dark2")+
    theme_bw()+
    geom_abline()+
    scale_x_continuous(limits = c(0, 6200))+
    scale_y_continuous(limits=c(0,6200))+
    theme(legend.position = "none",
          axis.title = element_text(size = 12))+
    labs(x = "Total Steps Estimated from Raw Data", y = "Total Steps Estimated from Data Resampled to 30 Hz")+
    coord_equal()+
    geom_text(data = cor_df %>%
                pivot_longer(cols = -method) %>% rename(id_study = name),
              aes(x = 2500, y = 5000,
                  label = paste("\u03c1 =", formatC(signif(value, digits=3), digits=2, format="fg", flag="#"))),
              inherit.aes = FALSE)

  # correlation table, between total steps estimated for ea person
  cor_df_separate =
    clemson %>%
    group_by(id_subject, cat_activity) %>%
    summarize(across(starts_with("steps"), ~sum(.x))) %>%
    pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
    select(id_subject, cat_activity, name, value) %>%
    mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
           method = ifelse(type == "resampled",
                           sub(".*steps\\_(.+)\\_.*", "\\1", name),
                           sub(".*\\_", "", name))) %>%
    ungroup() %>%
    select(-name) %>%
    pivot_wider(names_from = type, values_from = value,
                id_cols = c(id_subject, cat_activity, method))  %>%
    group_by(cat_activity,  method) %>%
    summarize(pearson = cor(raw, resampled, method = "pearson")) %>%
    pivot_wider(names_from = cat_activity, values_from = pearson)

  # overall
  cor_df_overall =
    oxwalk %>%
    filter(sample_rate == 25) %>%
    mutate(id_study = "oxwalk25", cat_activity2 = "ox") %>%
    bind_rows(oxwalk %>% filter(sample_rate == 100) %>%
                mutate(id_study = "oxwalk100")) %>%
    bind_rows(clemson %>% mutate(id_study = "clemson"))  %>%
    group_by(id_subject, id_study) %>%
    summarize(across(starts_with("steps"), ~sum(.x))) %>%
    pivot_longer(cols = starts_with("steps") & !contains("truth") ) %>%
    select(id_subject, id_study, name, value) %>%
    mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
           method = ifelse(type == "resampled",
                           sub(".*steps\\_(.+)\\_.*", "\\1", name),
                           sub(".*\\_", "", name))) %>%
    ungroup() %>%
    select(-name) %>%
    pivot_wider(names_from = type, values_from = value,
                id_cols = c(id_subject, id_study, method))  %>%
    group_by(method) %>%
    summarize(pearson = cor(raw, resampled, method = "pearson", use = "na.or.complete")) %>%
    mutate(id_study = "overall") %>%
    ungroup() %>%
    pivot_wider(names_from = id_study, values_from = pearson)

  cor_df %>%
    left_join(cor_df_separate) %>%
    left_join(cor_df_overall) %>%
    select(-starts_with("walk")) %>%
    filter(!grepl("res", method)) %>%
    mutate(method = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense, Original",
                      "Verisense, Revised")) %>%
    arrange(method) %>%
    magrittr::set_colnames(c("Algorithm", "Clemson (15Hz)",
                             "OxWalk (25Hz)", "OxWalk (100Hz)", "Overall")) %>%
    kableExtra::kable(digits = 3, align = "llll", format = "latex", booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = "scale_down")


  # bland altman type plots for stepcount and verisense to show impact of resampling

  summary_df =
    clemson %>%
    group_by(id_subject) %>%
    select(id_subject, contains("truth"), contains("sc")) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(type = ifelse(strsplit(name, "_")[[1]][3]=="30", "resampled", "raw"),
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  labs = c("Stepcount (RF)", "Stepcount (SSL)")
  names(labs) = c("scrf", "scssl")

  # means = summary_df  %>%
  #   group_by(algorithm) %>%
  #   summarize(mean = mean(value),
  #             sd  = sd(value),
  #             ub = mean + 1.96 * sd,
  #             lb = mean - 1.96 * sd)

  plt = ggplot(summary_df) +
    geom_point(aes(x = steps_truth, y = value, col = type), size = 2.5)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = labs)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "")+
    scale_color_manual(values = c("#5773CC", "#FFB900"), labels = c("15Hz", "30Hz"), name = "Sample Rate")+
    theme(legend.position = c(.2, .9),
          legend.text = element_text(size = 12),
          legend.margin = margin(1,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))

  svg(here::here("manuscript/figures", "bland_altman_sc.svg"))
  plt
  dev.off()
  clemson %>%
    group_by(id_subject) %>%
    select(id_subject, contains("truth"), contains("sc"), "steps_vsoraw", "steps_vsoraw_30", "steps_vsrraw", "steps_vsrraw_30") %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    pivot_longer(cols = steps_scssl:steps_vsrraw_30) %>%
    rowwise() %>%
    mutate(type = ifelse(grepl("30", name), "resampled", "raw"),
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    pivot_wider(names_from=type,values_from=value,id_cols=c(id_subject,algorithm)) %>%
    ggplot(aes(x = raw,y=resampled, col = algorithm))+
    facet_grid(.~algorithm)+
    geom_point()+
    theme_bw()+
    geom_abline()

  summary_df =
    oxwalk %>% filter(sample_rate==100) %>%
    select(id_subject, id_study, contains("truth"), contains("vs")) %>%
    group_by(id_subject) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    select(-c(ends_with("res_30"))) %>%
    magrittr::set_colnames(c("id_subject", "steps_truth", "steps_orig_100hz", "steps_rev_100hz",
                             "steps_orig_15hz", "steps_rev_15hz", "steps_orig_30hz","steps_rev_30hz")) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(srate = strsplit(name, "_")[[1]][3],
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  meth = c("Original", "Revised")
  names(meth) = c("orig", "rev")
  summary_df %>%
    mutate(srate = factor(srate, levels = c("15hz", "30hz", "100hz"))) %>%
    ggplot() +
    geom_point(aes(x = steps_truth, y = value, col = srate), size = 2)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = meth)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "Bland Altman Plot for Steps Estimated by
       Verisense on Raw vs. Resampled Data")+
    scale_color_manual(values = c("#5773CC", "#FFB900", "#802268"),
                       name = "Sample Rate", labels = c("15", "30", "100")) +
    theme(legend.position = c(.2, .92),
          legend.text = element_text(size = 12),
          legend.margin = margin(0,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))

  summary_df =
    oxwalk %>% filter(sample_rate==25) %>%
    select(id_subject, id_study, contains("truth"), contains("vs")) %>%
    group_by(id_subject) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    select(-c(ends_with("res_30"))) %>%
    magrittr::set_colnames(c("id_subject", "steps_truth", "steps_orig_25hz", "steps_rev_25hz",
                             "steps_orig_15hz", "steps_rev_15hz", "steps_orig_30hz","steps_rev_30hz")) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(srate = strsplit(name, "_")[[1]][3],
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  meth = c("Original", "Revised")
  names(meth) = c("orig", "rev")
  summary_df %>%
    mutate(srate = factor(srate, levels = c("15hz", "25hz", "30hz"))) %>%
    ggplot() +
    geom_point(aes(x = steps_truth, y = value, col = srate), size = 2)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = meth)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "Bland Altman Plot for Steps Estimated by
       Verisense on Raw vs. Resampled Data")+
    scale_color_manual(values = c("#5773CC", "#FFB900", "#802268"),
                       name = "Sample Rate", labels = c("15", "25", "30")) +
    theme(legend.position = c(.2, .92),
          legend.text = element_text(size = 12),
          legend.margin = margin(0,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))


  summary_df =
    clemson %>%
    select(id_subject, id_study, contains("truth"), contains("vs")) %>%
    group_by(id_subject) %>%
    summarize(across(starts_with("steps"),
                     ~ sum(.x))) %>%
    select(-c(ends_with("res_30"), ends_with("res"))) %>%
    magrittr::set_colnames(c("id_subject", "steps_truth", "steps_orig_15hz", "steps_rev_15hz",
                             "steps_orig_30hz","steps_rev_30hz")) %>%
    rowwise() %>%
    mutate(across(starts_with("steps") & !contains("truth"),
                  list(difference = ~steps_truth - .x,
                       truth = ~steps_truth))) %>%
    select(id_subject, steps_truth, ends_with("difference")) %>%
    pivot_longer(cols = ends_with("difference")) %>%
    rowwise() %>%
    mutate(srate = strsplit(name, "_")[[1]][3],
           algorithm = strsplit(name, "_")[[1]][2]) %>%
    select(-name)
  meth = c("Original", "Revised")
  names(meth) = c("orig", "rev")
  plot = summary_df %>%
    mutate(srate = factor(srate, levels = c("15hz", "30hz"))) %>%
    ggplot() +
    geom_point(aes(x = steps_truth, y = value, col = srate), size = 2)+
    facet_wrap(.~algorithm, labeller = labeller(algorithm = meth)) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
    labs(x = "True Steps ",
         y = "Difference (True Steps - Predicted Steps)",
         title = "")+
    scale_color_manual(values = c("#5773CC", "#FFB900"),
                       name = "Sample Rate", labels = c("15", "30")) +
    theme(legend.position = c(.2, .92),
          legend.text = element_text(size = 12),
          legend.margin = margin(1,1,1,1),
          axis.text= element_text(size = 10),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size = 13))


  svg(here::here("manuscript/figures", "bland_altman_vs.svg"))
  plot
  dev.off()


  # summary_df =
  #   marea %>%
  #   select(id_subject, id_study, contains("truth"), contains("vs")) %>%
  #   group_by(id_subject) %>%
  #   summarize(across(starts_with("steps"),
  #                    ~ sum(.x))) %>%
  #   select(-c(ends_with("res_30"))) %>%
  #   magrittr::set_colnames(c("id_subject", "steps_truth", "steps_orig_128hz", "steps_rev_128hz",
  #                            "steps_orig_15hz","steps_rev_15hz", "steps_orig_30hz", "steps_rev_30hz")) %>%
  #   rowwise() %>%
  #   mutate(across(starts_with("steps") & !contains("truth"),
  #                 list(difference = ~steps_truth - .x,
  #                      truth = ~steps_truth))) %>%
  #   select(id_subject, steps_truth, ends_with("difference")) %>%
  #   pivot_longer(cols = ends_with("difference")) %>%
  #   rowwise() %>%
  #   mutate(srate = strsplit(name, "_")[[1]][3],
  #          algorithm = strsplit(name, "_")[[1]][2]) %>%
  #   select(-name)
  # meth = c("Original", "Revised")
  # names(meth) = c("orig", "rev")
  # summary_df %>%
  #   mutate(srate = factor(srate, levels = c("15hz", "30hz", "128hz"))) %>%
  #   ggplot() +
  #   geom_point(aes(x = steps_truth, y = value, col = srate), size = 2)+
  #   facet_wrap(.~algorithm, labeller = labeller(algorithm = meth)) +
  #   theme_bw()+
  #   geom_hline(aes(yintercept = 0), col = "darkgrey", linetype  = 2)+
  #   labs(x = "True Steps ",
  #        y = "Difference (True Steps - Predicted Steps)",
  #        title = "Bland Altman Plot for Steps Estimated by Verisense on Raw vs. Resampled Data")+
  #   scale_color_manual(values = c("#5773CC", "#FFB900", "#802268"),
  #                      name = "Sample Rate", labels = c("15", "30", "128")) +
  #   theme(legend.position = c(.2, .92),
  #         legend.text = element_text(size = 12),
  #         legend.margin = margin(1,1,1,1),
  #         axis.text= element_text(size = 10),
  #         axis.title = element_text(size = 12),
  #         strip.text = element_text(size = 12),
  #         plot.title = element_text(size = 13))
}
