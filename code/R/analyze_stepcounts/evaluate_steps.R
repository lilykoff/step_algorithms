library(tidyverse)

clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))


# check to make sure NAs are just happening at end
clemson %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  filter(position != 1)

# now get rid of last row
clemson = clemson %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position)

oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz"))

# for now just use 100 hz
oxwalk = oxwalk %>%
  filter(sample_rate == 100)


# check to make sure NAs are just happening at end
oxwalk %>%
  group_by(id_subject) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  filter(position != 1)

# now get rid of last row
oxwalk = oxwalk %>%
  group_by(id_subject) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position)

marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz"))

# check to make sure NAs are just happening at end
marea %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x))) %>%
  select(time, steps_truth, steps_oak, steps_oak_30, steps_acti_30, position) %>%
  filter(position != 1)

# now get rid of last row
marea = marea %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time)) %>%
  mutate(position = row_number()) %>%
  filter(position != 1) %>%
  ungroup() %>%
  select(-position) %>%
  filter(grepl("run", cat_activity)==FALSE)


clemson = clemson %>%
  select(time, id_study, id_subject, cat_activity, ends_with("30"),
         steps_truth) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular", "walk_semiregular", "walk_irregular")))

oxwalk = oxwalk %>%
  select(time, id_study, id_subject,  ends_with("30"),
         steps_truth)

marea = marea %>%
  select(time, id_study, id_subject,  ends_with("30"),
         steps_truth, cat_activity)



# manuscript table
tab_individual =
  oxwalk %>%
  mutate(id_study = "oxwalk",
        cat_activity = "oxwalk") %>%
  bind_rows(clemson %>% mutate(id_study = "clemson")) %>%
  bind_rows(marea %>% mutate(cat_activity = "marea",
                             id_study = "marea")) %>%
  mutate(id_study = factor(id_study, levels = c("clemson", "marea", "oxwalk"))) %>%
  group_by(id_subject, id_study, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, cat_activity, id_study, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*30\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_30.*", "\\1", name))  %>%
  select(-name) %>%
  group_by(metric, algorithm, id_study) %>%
  summarize(across(value,
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  rowwise() %>%
  mutate(mean = case_when(
    metric == "bias" ~  paste0(sprintf(value_mean, fmt = "%.0f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"),
    metric == "ape" ~ paste0(sprintf(value_mean*100, fmt = "%#.1f"), " (", sprintf(value_sd*100, fmt = "%.0f"), ")"))
  ) %>%
  select(algorithm, id_study, mean, metric) %>%
  pivot_wider(names_from = c(id_study,metric), values_from = mean) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")) %>%
  arrange(algorithm)

tab_overall =
  oxwalk %>%
  mutate(id_study = "oxwalk",
         cat_activity = "oxwalk") %>%
  bind_rows(clemson %>% mutate(id_study = "clemson")) %>%
  bind_rows(marea %>% mutate(cat_activity = "marea",
                             id_study = "marea")) %>%
  mutate(id_study = factor(id_study, levels = c("clemson", "marea", "oxwalk"))) %>%
  group_by(id_subject, id_study, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, cat_activity, id_study, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*30\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_30.*", "\\1", name))  %>%
  select(-name) %>%
  group_by(metric, algorithm) %>%
  summarize(across(value,
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  rowwise() %>%
  mutate(mean = case_when(
    metric == "bias" ~  paste0(sprintf(value_mean, fmt = "%.0f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"),
    metric == "ape" ~ paste0(sprintf(value_mean*100, fmt = "%#.1f"), " (", sprintf(value_sd*100, fmt = "%.0f"), ")"))
  ) %>%
  select(algorithm, mean, metric) %>%
  mutate(name = "overall") %>%
  pivot_wider(names_from = c(name, metric), values_from = mean) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")) %>%
  arrange(algorithm)


tab_individual %>%
  left_join(tab_overall) %>%
  select(algorithm, ends_with("ape"), ends_with("bias")) %>%
  kableExtra::kable(align = "llll",booktabs = TRUE, format = "latex", col.names =
                      c("Algorithm", rep(c("Clemson", "MAREA", "Owalk", "Overall"), 2))) %>%
  kableExtra::add_header_above(c(" " = 1, "APE" = 4, "Bias" = 4)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

oxwalk %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson %>% mutate(id_study = "clemson")) %>%
  bind_rows(marea %>% mutate(cat_activity = "marea",
                               id_study = "marea")) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular",
                                                        "walk_semiregular",
                                                        "walk_irregular", "marea", "oxwalk"))) %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, cat_activity, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*30\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_30.*", "\\1", name))  %>%
  select(-name) %>%
  group_by(metric, algorithm, cat_activity) %>%
  summarize(across(value,
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)))) %>%
  rowwise() %>%
  mutate(mean = case_when(
    metric == "bias" ~  paste0(sprintf(value_mean, fmt = "%.0f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"),
    metric == "ape" ~ paste0(sprintf(value_mean*100, fmt = "%#.1f"), " (", sprintf(value_sd*100, fmt = "%.0f"), ")"))
  ) %>%
  select(algorithm, cat_activity, mean, metric) %>%
  pivot_wider(names_from = cat_activity, values_from = mean)  %>%
  ungroup() %>%
  select(metric, algorithm, marea, walk_regular, walk_semiregular, walk_irregular, oxwalk) %>%
  kableExtra::kable(align = "llll", booktabs = TRUE, format = "latex",  col.names =
                      c("Metric", "Algorithm", rep(c("Regular", "Regular", "Semiregular", "Irregular", "Free-Living"), 1)))  %>%
  kableExtra::collapse_rows(columns = 1:2, valign = "top") %>%
  kableExtra::add_header_above(c(" " = 2, "MAREA" = 1,
                                 "Clemson" = 3, "OxWalk" = 1))  %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# figures



# PLOTS truth vs predicted
labs = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")

names(labs) = c("acti", "adept", "oak",  "scrf", "scssl","sdt", "vs")


clem2 = clemson %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method, shape = cat_activity))+
  geom_point() +
  facet_grid(.~method,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2", name = "", guide = "none")+
  scale_shape_discrete(name = "")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1250))+
  scale_y_continuous(limits=c(0,1250))+
  theme(legend.position = "bottom")+
  labs(x = "True Steps", y = "Predicted Steps", title = "Clemson")+
  coord_equal()


ox= oxwalk %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2", name = "", guide = "none")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 6500))+
  scale_y_continuous(limits=c(0,6500))+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps", title = "OxWalk")+
  coord_equal()




mar2 = marea %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method, shape = cat_activity))+
  geom_point() +
  facet_grid(.~method,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2", name = "", guide = "none")+
  scale_shape_discrete(name = "")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1600))+
  scale_y_continuous(limits=c(0,1600))+
  theme(legend.position = "bottom")+
  labs(x = "True Steps", y = "Predicted Steps", title = "MAREA")+
  coord_equal()


cowplot::plot_grid(clem2, mar2, ox, nrow = 3, rel_heights = c(0.35, 0.35, 0.3))


