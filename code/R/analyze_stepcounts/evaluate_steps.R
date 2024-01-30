library(tidyverse)

clemson10 = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, cat_activity, ends_with("30"), steps_truth, time_10) %>%
  mutate(cat_activity = factor(cat_activity, levels = c("walk_regular", "walk_semiregular", "walk_irregular")))



# check to make sure NAs are just happening at end
clemson10%>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time_10)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x)))
#   filter(position != 1)


oxwalk10 = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, sample_rate, ends_with("30"), steps_truth, time_10) %>%
  filter(sample_rate == 100) %>%
  select(-sample_rate)


# check to make sure NAs are just happening at end
oxwalk10 %>%
  group_by(id_subject) %>%
  arrange(desc(time_10)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x)))


marea10 = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_10sec.csv.gz")) %>%
  select(id_subject, cat_activity, ends_with("30"), steps_truth, time_10) %>%
  filter(grepl("run", cat_activity)==FALSE)

# check to make sure NAs are just happening at end
marea10 %>%
  group_by(id_subject, cat_activity) %>%
  arrange(desc(time_10)) %>%
  mutate(position = row_number()) %>%
  filter(if_any(starts_with("steps"), ~is.na(.x)))




# manuscript table
oxwalk10 %>%
  mutate(id_study = "oxwalk",
        cat_activity = "oxwalk") %>%
  bind_rows(clemson10 %>% mutate(id_study = "clemson")) %>%
  bind_rows(marea10 %>% mutate(cat_activity = "marea",
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
    metric == "bias" ~ paste0(round(value_mean, 0), " (", round(value_sd, 0), ")"),
    metric == "ape" ~ paste0(round(value_mean*100, 1), " (", round(value_sd*100, 1), ")"))
  ) %>%
  select(algorithm, id_study, mean, metric) %>%
  pivot_wider(names_from = c(id_study,metric), values_from = mean) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (SSL)", "Stepcount (RF)", "SDT", "Verisense")) %>%
  arrange(algorithm) %>%
  kableExtra::kable(align = "llll",booktabs = TRUE, format = "latex", col.names =
                      c("Algorithm", rep(c("Clemson", "MAREA", "Owalk"), 2))) %>%
  kableExtra::add_header_above(c(" " = 1, "APE" = 3, "Bias" = 3)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# supplemental table
# oxwalk10 %>%
#   mutate(cat_activity = "oxwalk") %>%
#   bind_rows(clemson10 %>% mutate(id_study = "clemson")) %>%
#   bind_rows(marea10 %>% mutate(cat_activity = "marea",
#                                id_study = "marea")) %>%
#   mutate(cat_activity = factor(cat_activity, levels = c("walk_regular",
#                                                         "walk_semiregular",
#                                                         "walk_irregular", "marea", "oxwalk"))) %>%
#   group_by(id_subject, cat_activity) %>%
#   summarize(across(starts_with("steps"),
#                    ~sum(.x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   rowwise() %>%
#   mutate(across(starts_with("steps") & !contains("truth"),
#                 list(ape = ~ abs((steps_truth - .x)/steps_truth),
#                      bias = ~ .x - steps_truth))) %>%
#   select(id_subject, cat_activity, contains("bias"), contains("ape")) %>%
#   pivot_longer(cols  = starts_with("steps")) %>%
#   mutate(metric = sub(".*30\\_", "", name),
#          algorithm = sub(".*steps_(.+)\\_30.*", "\\1", name))  %>%
#   select(-name) %>%
#   group_by(metric, algorithm, cat_activity) %>%
#   summarize(across(value,
#                    list(mean = ~ mean(.x, na.rm = TRUE),
#                         sd = ~ sd(.x, na.rm = TRUE)))) %>%
#   rowwise() %>%
#   mutate(mean = case_when(
#     metric == "bias" ~ paste0(round(value_mean, 0), " (", round(value_sd, 0), ")"),
#     metric == "ape" ~ paste0(round(value_mean*100, 1), " (", round(value_sd*100, 1), ")"))
#   ) %>%
#   select(algorithm, cat_activity, mean, metric) %>%
#   pivot_wider(names_from = c(cat_activity,metric), values_from = mean) %>%
#   mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (SSL)", "Stepcount (RF)", "SDT", "Verisense")) %>%
#   arrange(algorithm) %>%
#   kableExtra::kable(align = "llll",booktabs = TRUE, format = "latex", col.names =
#                       c("Algorithm", rep(c("Clemson - Regular", "Clemson - Semiregular", "Clemson - Irregular", "MAREA", "Owalk"), 2))) %>%
#   kableExtra::add_header_above(c(" " = 1, "APE" = 5, "Bias" = 5)) %>%
#   kableExtra::kable_styling(latex_options = "scale_down")


oxwalk10 %>%
  mutate(cat_activity = "oxwalk") %>%
  bind_rows(clemson10 %>% mutate(id_study = "clemson")) %>%
  bind_rows(marea10 %>% mutate(cat_activity = "marea",
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
    metric == "bias" ~ paste0(round(value_mean, 0), " (", round(value_sd, 0), ")"),
    metric == "ape" ~ paste0(round(value_mean*100, 1), " (", round(value_sd*100, 1), ")"))
  ) %>%
  select(algorithm, cat_activity, mean, metric) %>%
  pivot_wider(names_from = cat_activity, values_from = mean)  %>%
  ungroup() %>%
  select(metric, algorithm, walk_regular, walk_semiregular, walk_irregular, marea, oxwalk) %>%
  kableExtra::kable(align = "llll", booktabs = TRUE,  format = "latex", col.names =
                      c("Metric", "Algorithm", rep(c("Regular", "Semiregular", "Irregular", "Regular", "Free-Living"), 1)))  %>%
  kableExtra::collapse_rows(columns = 1:2, valign = "top") %>%
  kableExtra::add_header_above(c(" " = 2, "Clemson" = 3,
                                 "MAREA" = 1, "OxWalk" = 1))  %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# figures



# PLOTS truth vs predicted
labs = c("ActiLife", "ADEPT", "Oak", "Stepcount (SSL)", "Stepcount (RF)", "SDT", "Verisense")


names(labs) = c("acti", "adept", "oak", "sc", "scrf", "sdt", "vs")


clem2 = clemson10 %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method, shape = cat_activity))+
  geom_point() +
  facet_grid(.~method,labeller = labeller(method = labs))+
  scale_color_manual(values= c("#1B9E77FF",
                              "#D95F02FF", "#7570B3FF",  "#A6761DFF","#E7298AFF", "#66A61EFF", "#E6AB02FF"),
    name = "", guide = "none")+
  scale_shape_discrete(name = "")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1250))+
  scale_y_continuous(limits=c(0,1250))+
  theme(legend.position = "bottom")+
  labs(x = "True Steps", y = "Predicted Steps", title = "Clemson")+
  coord_equal()


ox= oxwalk10 %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point() +
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_manual(values= c("#1B9E77FF",
                               "#D95F02FF", "#7570B3FF",  "#A6761DFF","#E7298AFF", "#66A61EFF", "#E6AB02FF"),
                     name = "", guide = "none")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 6500))+
  scale_y_continuous(limits=c(0,6500))+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps", title = "OxWalk")+
  coord_equal()




mar2 = marea10 %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method, shape = cat_activity))+
  geom_point() +
  facet_grid(.~method,labeller = labeller(method = labs))+
  scale_color_manual(values= c("#1B9E77FF",
                               "#D95F02FF", "#7570B3FF",  "#A6761DFF","#E7298AFF", "#66A61EFF", "#E6AB02FF"),
                     name = "", guide = "none")+
  scale_shape_discrete(name = "")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1600))+
  scale_y_continuous(limits=c(0,1600))+
  theme(legend.position = "bottom")+
  labs(x = "True Steps", y = "Predicted Steps", title = "MAREA")+
  coord_equal()


cowplot::plot_grid(clem2, mar2, ox, nrow = 3, rel_heights = c(0.35, 0.35, 0.3))


