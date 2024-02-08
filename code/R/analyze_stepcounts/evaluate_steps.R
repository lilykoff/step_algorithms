library(tidyverse)
`%notin%` = Negate(`%in%`)
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

step_df =
  readRDS(here::here("results", "all_algorithms", "step_stats_bysubject.rds")) %>%
  filter(cat_activity != "oxwalk25" &
         grepl("30", algorithm)) %>%
  filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30"))

# manuscript table
tab_individual =
  step_df %>%
  filter(cat_activity %in% c("clemson_overall", "oxwalk100", "marea")) %>%
  pivot_longer(cols = c("bias", "ape"), names_to = "metric") %>%
  group_by(metric, algorithm, cat_activity) %>%
  summarize(across(value,
                   list(mean = ~ mean(.x),
                        sd = ~ sd(.x)))) %>%
  rowwise() %>%
  mutate(mean = case_when(
    metric == "bias" ~  paste0(sprintf(value_mean, fmt = "%.0f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"),
    metric == "ape" ~ paste0(sprintf(value_mean, fmt = "%#.1f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"))
  ) %>%
  select(algorithm, cat_activity, mean, metric) %>%
  pivot_wider(names_from = c(cat_activity,metric), values_from = mean) %>%
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT",
                       "Verisense (original)", "Verisense (revised)"))

tab_overall =
  step_df %>%
  filter(cat_activity %in% c("clemson_overall", "oxwalk100", "marea")) %>%
  pivot_longer(cols = c("bias", "ape"), names_to = "metric") %>%
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
  mutate(algorithm = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT",
                       "Verisense (original)", "Verisense (revised)"))

tab_individual %>%
  left_join(tab_overall) %>%
  select(algorithm, ends_with("ape"), ends_with("bias")) %>%
  kableExtra::kable(align = "llll",booktabs = TRUE, format = "latex", col.names =
                      c("Algorithm", rep(c("Clemson", "MAREA", "OxWalk", "Overall"), 2))) %>%
  kableExtra::add_header_above(c(" " = 1, "APE" = 4, "Bias" = 4)) %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# supplemental table (long)
step_df %>%
  filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>% # remove the raw 30 hz verisense, just use reampled
  filter(cat_activity != "clemson_overall") %>%
  pivot_longer(cols = c("bias", "ape"), names_to = "metric") %>%
  group_by(metric, algorithm, cat_activity) %>%
  summarize(across(value,
                   list(mean = ~ mean(.x),
                        sd = ~ sd(.x)))) %>%
  rowwise() %>%
  mutate(mean = case_when(
    metric == "bias" ~  paste0(sprintf(value_mean, fmt = "%.0f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"),
    metric == "ape" ~ paste0(sprintf(value_mean, fmt = "%#.1f"), " (", sprintf(value_sd, fmt = "%.0f"), ")"))
  ) %>%
  select(algorithm, cat_activity, mean, metric) %>%
  pivot_wider(names_from = cat_activity, values_from = mean)  %>%
  ungroup() %>%
  select(metric, algorithm, marea, clemson_walk_regular, clemson_walk_semiregular, clemson_walk_irregular, oxwalk100) %>%
  kableExtra::kable(align = "llll", booktabs = TRUE, format = "latex",  col.names =
                      c("Metric", "Algorithm", rep(c("Regular", "Regular", "Semiregular", "Irregular", "Free-Living"), 1)))  %>%
  kableExtra::collapse_rows(columns = 1:2, valign = "top") %>%
  kableExtra::add_header_above(c(" " = 2, "MAREA" = 1,
                                 "Clemson" = 3, "OxWalk" = 1))  %>%
  kableExtra::kable_styling(latex_options = "scale_down")

# figures

# PLOTS truth vs predicted
total_steps = readRDS(here::here("results", "all_algorithms", "total_steps_bysubject.rds")) %>%
  filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
  filter(cat_activity != "oxwalk25" &
           (grepl("30", algorithm) | grepl("truth", algorithm)))
labs = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense (original)",
         "Verisense (revised)")

names(labs) = c("acti", "adept", "oak",  "scrf", "scssl","sdt", "vsores", "vsrres")

clem2 =
  total_steps %>%
  filter(cat_activity != "clemson_overall" & id_study == "clemson") %>%
  pivot_wider(names_from = algorithm, values_from = total_steps) %>%
  pivot_longer(cols = c(steps_adept_30:steps_acti_30)) %>%
  select(id_subject, cat_activity, steps_truth, name, value) %>%
  mutate(method = sub(".*steps\\_(.+)\\_30.*", "\\1", name),
         cat_activity = factor(cat_activity,
                               levels = c("clemson_walk_regular",
                                          "clemson_walk_semiregular",
                                          "clemson_walk_irregular"))) %>%
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


ox =
  total_steps %>%
  filter(id_study == "oxwalk") %>%
  pivot_wider(names_from = algorithm, values_from = total_steps) %>%
  pivot_longer(cols = c(steps_adept_30:steps_acti_30)) %>%
  select(id_subject, cat_activity, steps_truth, name, value) %>%
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



mar2 =
  total_steps %>%
  filter(id_study == "marea") %>%
  pivot_wider(names_from = algorithm, values_from = total_steps) %>%
  pivot_longer(cols = c(steps_adept_30:steps_acti_30)) %>%
  select(id_subject, cat_activity, steps_truth, name, value) %>%
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

svg(here::here("manuscript_figures", "truth_v_predicted.svg"))
cowplot::plot_grid(clem2, mar2, ox, nrow = 3)
dev.off()
# p values
stats =
  step_df %>%
  filter(cat_activity %in% c("oxwalk100", "marea", "clemson_overall") &
           grepl("30", algorithm)) %>%
  filter(algorithm %notin% c("steps_vsoraw_30", "steps_vsrraw_30")) %>%
  select(bias, ape, id_subject, id_study = cat_activity, algorithm)


overall_stats = stats %>% mutate(id_study = "overall")

# create overall df
recog_stats_test =
 bind_rows(overall_stats, stats)

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
pairs = expand.grid.unique(x = c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vsores", "vsrres"),
                           y =  c("acti", "adept", "oak", "scrf", "scssl", "sdt", "vsores", "vsrres")) %>%
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
  scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt", "vso")) +
  scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vso", "vsr"))+
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
  scale_x_discrete(labels = c("actilife", "adept", "oak", "scrf", "scssl", "sdt", "vso")) +
  scale_y_discrete(labels = c( "adept", "oak", "scrf","scssl", "sdt", "vso", "vsr"))+
  labs(title = "p values for ape score")



cowplot::plot_grid(bias, ape)


