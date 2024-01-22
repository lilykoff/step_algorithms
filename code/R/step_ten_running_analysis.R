library(tidyverse)

marea_nested  = readRDS("~/Documents/step_algorithms/data/processed/marea_nested_all.rds")
marea_10sec = read_csv(here::here("results/all_algorithms/marea_step_estimates_10sec.csv.gz"))


marea_10sec %>%
  filter(grepl("treadmill_walkrun", cat_activity_large)) %>%
  filter(speed != "self_selected") %>%
  mutate(speed = ifelse(speed == "self_selected", "8.0 km/hr", speed)) %>%
  pivot_longer(cols = starts_with("steps") & contains("30")) %>%
  mutate(algorithm = sub("\\_30.*", "", name)) %>%
  ggplot(aes(x = steps_truth, y = value))+
  geom_point(alpha = .5, aes(col = speed))+
  facet_grid(algorithm  ~ speed)+
  geom_abline(intercept = 0)+
  theme_bw()+
  coord_equal()+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps")


# something like x axis: speed, y axis: bias?
library(ggpmisc)

marea_10sec %>%
  filter(grepl("treadmill_walkrun", cat_activity_large)) %>%
  filter(speed != "self_selected") %>%
  select(contains("30") | contains("truth"), id_subject, cat_activity,
               slope, speed) %>%
  mutate(across(starts_with("steps")& !contains("truth"),
                list(bias = ~steps_truth - .x))) %>%
  group_by(id_subject, speed) %>%
  summarize(across(ends_with("bias"), ~mean(.x))) %>%
  pivot_longer(cols = ends_with("bias")) %>%
  mutate(algorithm = sub("\\_30.*", "", name),
         speed = as.numeric(sub("km/hr.*", "", speed))) %>%
  ggplot(aes(x = speed, y = value, col = algorithm))+
  geom_jitter(width = .15, alpha = .8)+
  facet_grid(. ~ algorithm)+
  geom_smooth(method = "lm", aes(color = algorithm), se = FALSE)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Speed (km/hr)", y = "Bias: Truth - Predicted")+
  scale_x_continuous(breaks=seq(4,8,0.4))+
  geom_hline(aes(yintercept = 0))+
  scale_color_brewer(palette  = "Dark2")

# current figure
labs = c("ActiLife", "ADEPT", "Oak", "Stepcount", "SDT", "Verisense")
names(labs)  = c("steps_acti", "steps_adept" , "steps_oak", "steps_sc", "steps_sdt", "steps_vs")
marea_10sec %>%
  filter(grepl("treadmill_walkrun", cat_activity_large)) %>%
  filter(speed != "self_selected") %>%
  select(contains("30") | contains("truth"), id_subject, cat_activity,
         slope, speed) %>%
  mutate(across(starts_with("steps")& !contains("truth"),
                list(bias = ~steps_truth - .x))) %>%
  group_by(id_subject, speed) %>%
  summarize(across(ends_with("bias"), ~mean(.x))) %>%
  pivot_longer(cols = ends_with("bias")) %>%
  mutate(algorithm = sub("\\_30.*", "", name),
         speed = as.numeric(sub("km/hr.*", "", speed))) %>%
  ggplot(aes(x = speed, y = value, col = algorithm))+
  geom_jitter(width = .15, alpha = .8)+
  facet_grid(. ~ algorithm, labeller = labeller(algorithm = labs))+
  stat_poly_line(aes(col = algorithm), se = FALSE)+
  stat_poly_eq(use_label(c("eq", "p")), col = "black", label.y = .2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Speed (km/hr)", y = "Bias: Truth - Predicted")+
  scale_x_continuous(breaks=seq(4,8,0.4))+
  geom_hline(aes(yintercept = 0))+
  scale_color_brewer(palette  = "Dark2")

## THIS IS FINAL FIG
marea_10sec %>%
  filter(grepl("treadmill_walk", cat_activity_large)) %>%
  select(contains("30") | contains("truth"), id_subject, cat_activity,
         speed) %>%
  group_by(id_subject, speed)  %>%
  summarize(across(starts_with("steps"), ~sum(.x))) %>%
  mutate(across(starts_with("steps")& !contains("truth"),
                list(bias = ~steps_truth - .x))) %>%
  pivot_longer(cols = ends_with("bias")) %>%
  mutate(algorithm = sub("\\_30.*", "", name),
         speed = as.numeric(sub("km/hr.*", "", speed))) %>%
  ggplot(aes(x = speed, y = value, col = algorithm))+
  geom_jitter(width = .15, alpha = .8)+
  facet_grid(. ~ algorithm, labeller = labeller(algorithm = labs))+
  stat_poly_line(aes(col = algorithm), se = FALSE)+
  stat_poly_eq(use_label(c("eq", "p")), col = "black", label.y = .2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Speed (km/hr)", y = "Bias: Truth - Predicted")+
  scale_x_continuous(breaks=seq(4,8,0.4))+
  geom_hline(aes(yintercept = 0))+
  scale_color_brewer(palette  = "Dark2")




marea_10sec %>%
  filter(grepl("treadmill_walkrun", cat_activity_large)) %>%
  filter(speed != "self_selected") %>%
  mutate(speed = ifelse(speed == "self_selected", "8.0 km/hr", speed)) %>%
  pivot_longer(cols = starts_with("steps") & contains("30")) %>%
  mutate(algorithm = sub("\\_30.*", "", name)) %>%
  ggplot(aes(x = steps_truth, y = value))+
  geom_point(alpha = .5, aes(col = speed))+
  facet_grid(speed ~ algorithm)+
  geom_abline(intercept = 0)+
  theme_bw()+
  coord_equal()+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps")

marea_10sec %>%
  filter(grepl("treadmill_walkrun", cat_activity_large)) %>%
  pivot_longer(cols = starts_with("steps") & contains("30")) %>%
  mutate(algorithm = sub("\\_30.*", "", name)) %>%
  ggplot(aes(x = steps_truth, y = value))+
  geom_point(alpha = .5, aes(col = cat_activity))+
  facet_grid(cat_activity ~ algorithm)+
  geom_abline(intercept = 0)+
  theme_bw()+
  coord_equal()+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps")


# second current fig
indoor = marea_10sec %>%
  filter(grepl("indoor_walkrun", cat_activity_large)) %>%
  pivot_longer(cols = starts_with("steps") & contains("30")) %>%
  mutate(algorithm = sub("\\_30.*", "", name)) %>%
  ggplot(aes(x = steps_truth, y = value))+
  geom_point(alpha = .5, aes(col = cat_activity))+
  facet_grid(cat_activity ~ algorithm)+
  geom_abline(intercept = 0)+
  theme_bw()+
  coord_equal()+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps")

outdoor = marea_10sec %>%
  filter(grepl("outdoor_walkrun", cat_activity_large)) %>%
  pivot_longer(cols = starts_with("steps") & contains("30")) %>%
  mutate(algorithm = sub("\\_30.*", "", name)) %>%
  ggplot(aes(x = steps_truth, y = value))+
  geom_point(alpha = .5, aes(col = cat_activity))+
  facet_grid(cat_activity ~ algorithm)+
  geom_abline(intercept = 0)+
  theme_bw()+
  coord_equal()+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps")
cowplot::plot_grid(indoor, outdoor, nrow = 2)

indoor = marea_10sec %>%
  filter(grepl("indoor_walkrun", cat_activity_large)) %>%
  pivot_longer(cols = starts_with("steps") & contains("30")) %>%
  mutate(algorithm = sub("\\_30.*", "", name))
outdoor = marea_10sec %>%
  filter(grepl("outdoor_walkrun", cat_activity_large)) %>%
  pivot_longer(cols = starts_with("steps") & contains("30")) %>%
  mutate(algorithm = sub("\\_30.*", "", name))

act_labs = c("Indoor Run", "Indoor Walk", "Outdoor Run", "Outdoor Walk")
names(act_labs) = c("indoor_run", "indoor_walk", "outdoor_run", "outdoor_walk")
bind_rows(indoor, outdoor) %>%
  ggplot(aes(x = steps_truth, y = value))+
  geom_point(alpha = .5, aes(col = algorithm))+
  facet_grid(cat_activity ~ algorithm, labeller = labeller(algorithm = labs,
                                                           cat_activity = act_labs))+
  geom_abline(intercept = 0)+
  theme_bw()+
  coord_equal()+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps")+
  scale_color_brewer(palette  = "Dark2")


cowplot::plot_grid(indoor, outdoor, nrow = 2)


marea_10sec %>%
  filter(grepl("treadmill_slopewalk", cat_activity_large)) %>%
  mutate(cat_slope = factor(slope, levels = c("0 deg", "5 deg", "10 deg", "15 deg"))) %>%
  pivot_longer(cols = starts_with("steps") & contains("30")) %>%
  mutate(algorithm = sub("\\_30.*", "", name)) %>%
  ggplot(aes(x = steps_truth, y = value))+
  geom_point(alpha = .5, aes(col = cat_slope))+
  geom_abline(intercept = 0)+
  facet_grid(algorithm  ~ cat_slope)+
  theme_bw()+
  coord_equal()+
  theme(legend.position = "none")+
  labs(x = "True Steps", y = "Predicted Steps")

