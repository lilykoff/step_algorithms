library(tidyverse)
clemson = readr::read_csv(here::here("results/all_algorithms/clemson_step_estimates_1sec.csv.gz"))
# just use 100 hz data from oxwalk for this part
oxwalk = readr::read_csv(here::here("results/all_algorithms/oxwalk_step_estimates_1sec.csv.gz")) %>%
  filter(sample_rate == 100)
# remove running from MAREA
marea = readr::read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz")) %>%
  filter(grepl("run", cat_activity)==FALSE)


# just use steps estimated from 30hz data for all algorithms
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
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  mutate(id_study = factor(id_study, levels = c("clemson", "marea", "oxwalk"))) %>%
  group_by(id_subject, id_study) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth*100),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, id_study, contains("bias"), contains("ape")) %>%
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
    metric == "ape" ~ paste0(sprintf(value_mean, fmt = "%#.1f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"))
  ) %>%
  select(algorithm, id_study, mean, metric) %>%
  pivot_wider(names_from = c(id_study,metric), values_from = mean) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")) %>%
  arrange(algorithm)

tab_overall =
  oxwalk %>%
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  mutate(id_study = factor(id_study, levels = c("clemson", "marea", "oxwalk"))) %>%
  group_by(id_subject, id_study) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth*100),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, id_study, contains("bias"), contains("ape")) %>%
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
    metric == "ape" ~ paste0(sprintf(value_mean, fmt = "%#.1f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"))
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

# supplemental table (long)
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
  geom_point(size = 2) +
  facet_grid(.~method,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2", name = "", guide = "none")+
  scale_shape_discrete(name = "", labels = c("Walk regular", "Walk semiregular", "Walk irregular"))+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1250))+
  scale_y_continuous(limits=c(0,1250))+
  theme(legend.position = c(.5, .5),
        legend.margin = margin(1,1,1,1),
        legend.title = element_blank(),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))+
  labs(x = "True Steps", y = "Predicted Steps", title = "Clemson")+
  coord_equal()+
  guides(shape = guide_legend(nrow = 1))


ox = oxwalk %>%
  group_by(id_subject) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name)) %>%
  ggplot(aes(x = steps_truth, y = value, col = method))+
  geom_point(size = 2) +
  facet_wrap(method~.,nrow = 1,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2", name = "", guide = "none")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 6500))+
  scale_y_continuous(limits=c(0,6500))+
  theme(legend.position= "none",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))+
  labs(x = "True Steps", y = "Predicted Steps", title = "OxWalk")+
  coord_equal()



mar2 = marea %>%
  group_by(id_subject, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(starts_with("steps") & !contains("truth"))) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name),
         activity = case_when(
           cat_activity %in% c("indoor_walk", "outdoor_walk") ~ "Walk indoor/outdoor",
           TRUE ~ "Walk treadmill"
         )) %>%
  ggplot(aes(x = steps_truth, y = value, col = method, shape = activity))+
  geom_point(size = 2) +
  facet_grid(.~method,labeller = labeller(method = labs))+
  scale_color_brewer(palette = "Dark2", name = "", guide = "none")+
  scale_shape_discrete(name = "")+
  theme_bw()+
  geom_abline()+
  scale_x_continuous(limits = c(0, 1600))+
  scale_y_continuous(limits=c(0,1600))+
  theme(legend.position = c(.5, .5),
        legend.margin = margin(1,1,1,1),
        legend.title = element_blank(),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))+
  labs(x = "True Steps", y = "Predicted Steps", title = "MAREA")+
  coord_equal()+
  guides(shape = guide_legend(nrow = 1))

cowplot::plot_grid(clem2, mar2, ox, nrow = 3)

# p values
stats =
  oxwalk %>%
  bind_rows(clemson) %>%
  bind_rows(marea) %>%
  mutate(id_study = factor(id_study, levels = c("clemson", "marea", "oxwalk"))) %>%
  group_by(id_subject, id_study) %>%
  summarize(across(starts_with("steps"),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("steps") & !contains("truth"),
                list(ape = ~ abs((steps_truth - .x)/steps_truth*100),
                     bias = ~ .x - steps_truth))) %>%
  select(id_subject, id_study, contains("bias"), contains("ape")) %>%
  pivot_longer(cols  = starts_with("steps")) %>%
  mutate(metric = sub(".*30\\_", "", name),
         algorithm = sub(".*steps_(.+)\\_30.*", "\\1", name))  %>%
  select(-name) %>%
  pivot_wider(names_from = "metric", values_from = value)

overall_stats = stats %>% mutate(id_study = "overall")

# create overall df
recog_stats_test =
 bind_rows(overall_stats, stats) %>%
  mutate(algorithm = paste0("steps_", algorithm, "_30"))

expand.grid.unique <- function(x, y, include.equals=FALSE)
{
  x <- unique(x)

  y <- unique(y)

  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])

    if(length(z)) cbind(x[i], z, deparse.level=0)
  }

  do.call(rbind, lapply(seq_along(x), g))
}
# significance testing
pairs = expand.grid.unique(x = c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vs"),
                           y =  c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vs")) %>%
  as_tibble() %>%
  magrittr::set_colnames(c("var1", "var2")) %>%
  mutate(var1 = paste0("steps_", var1, "_30"),
         var2 = paste0("steps_", var2, "_30")) %>%
  mutate(z = paste(var1, var2))
key = recog_stats_test %>%
  select(algorithm, id_study, id_subject, bias) %>%
  group_by(algorithm, id_study) %>%
  nest(data = c(id_subject, bias))

expanded = expand_grid(pair = pairs$z,id_study= unique(key$id_study)) %>%
  rowwise() %>%
  mutate(pair1 = str_split(pair, " ")[[1]][1],
         pair2 = str_split(pair, " ")[[1]][2])

t_test_res =
  expanded %>%
  select(-pair) %>%
  left_join(key, by = c("pair1" ="algorithm", "id_study" = "id_study")) %>%
  rename(bias_pair1 = data) %>%
  left_join(key, by = c("pair2" ="algorithm", "id_study" = "id_study")) %>%
  rename(bias_pair2 = data) %>%
  unnest() %>%
  group_by(id_study, pair1, pair2) %>%
  nest(data = c(id_subject, id_subject1, bias, bias1)) %>%
  mutate(pval = map(data, ~ t.test(.x$bias, .x$bias1, data = .x, paired = TRUE)$p.value)) %>%
  unnest(pval) %>%
  mutate(pval_char =as.character(signif(pval,digits=2)))


# p value plot
bias = t_test_res %>%
  ggplot(aes(x = pair1, y = pair2, fill = pval, label = pval_char))+
  geom_tile()+
  geom_text(parse = TRUE)+
  facet_wrap(.~id_study)+
  theme_bw()+
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn",
               palette = function(x) c("lightgreen", "lightblue", "white", "red"),
               breaks = c(0,0.001, 0.01, 0.05, 1),
               limits = c(0, 1),
               show.limits = TRUE,
               guide = "colorsteps"
  ) +
  scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt")) +
  scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vs"))+
  labs(title = "p values for bias score")

key = recog_stats_test %>%
  select(algorithm, id_study, id_subject, ape) %>%
  group_by(algorithm, id_study) %>%
  nest(data = c(id_subject, ape))

expanded = expand_grid(pair = pairs$z,id_study= unique(key$id_study)) %>%
  rowwise() %>%
  mutate(pair1 = str_split(pair, " ")[[1]][1],
         pair2 = str_split(pair, " ")[[1]][2])

t_test_res =
  expanded %>%
  select(-pair) %>%
  left_join(key, by = c("pair1" ="algorithm", "id_study" = "id_study")) %>%
  rename(ape_pair1 = data) %>%
  left_join(key, by = c("pair2" ="algorithm", "id_study" = "id_study")) %>%
  rename(ape_pair2 = data) %>%
  unnest() %>%
  group_by(id_study, pair1, pair2) %>%
  nest(data = c(id_subject, id_subject1, ape, ape1)) %>%
  mutate(pval = map(data, ~ t.test(.x$ape, .x$ape1, data = .x, paired = TRUE)$p.value)) %>%
  unnest(pval) %>%
  mutate(pval_char =as.character(signif(pval,digits=2)))


# p value plot
ape = t_test_res %>%
  ggplot(aes(x = pair1, y = pair2, fill = pval, label = pval_char))+
  geom_tile()+
  geom_text(parse = TRUE)+
  facet_wrap(.~id_study)+
  theme_bw()+
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn",
               palette = function(x) c("lightgreen", "lightblue", "white", "red"),
               breaks = c(0, 0.001, 0.01, 0.05, 1),
               limits = c(0, 1),
               show.limits = TRUE,
               guide = "colorsteps"
  ) +
  scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt")) +
  scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vs"))+
  labs(title = "p values for ape score")



cowplot::plot_grid(bias, ape)


