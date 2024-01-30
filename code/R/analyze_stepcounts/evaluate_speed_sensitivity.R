library(tidyverse)
library(ggpmisc)

marea_nested  = readRDS("~/Documents/step_algorithms/data/processed/marea_nested_all.rds")
marea_10sec = read_csv(here::here("results/all_algorithms/marea_step_estimates_10sec.csv.gz"))


# speed vs bias figure
# bias is estimated per minute
marea_10sec %>%
  filter(cat_activity_large == "treadmill_walkrun") %>%
  group_by(id_subject, time_10, cat_activity_large, speed) %>%
  summarize(across(c(n_seconds, starts_with("steps")), ~sum(.x))) %>% # sum "boundary" seconds
  ungroup() %>%
  filter(n_seconds == 10) %>%
  select(contains("30") | contains("truth"), id_subject,
         speed, time_10) %>%
  group_by(id_subject, speed)  %>%
  mutate(n = n()) %>%
  group_by(id_subject, speed, n) %>%
  summarize(across(starts_with("steps"), ~sum(.x))) %>%
  mutate(across(starts_with("steps")& !contains("truth"),
                list(bias = ~(steps_truth - .x) * (n*6/60)))) %>% # adjust to get bias per minute so eceryone on same scale
  pivot_longer(cols = ends_with("bias")) %>%
  mutate(algorithm = sub("\\_30.*", "", name),
         speed = as.numeric(sub("km/hr.*", "", speed))) %>%
  ggplot(aes(x = speed, y = value, col = algorithm))+
  geom_jitter(width = .15, alpha = .8)+
  facet_grid(. ~ algorithm, labeller = labeller(algorithm = labs))+
  stat_poly_line(aes(col = algorithm), se = FALSE)+
  # stat_poly_eq(use_label(c("eq", "p")), col = "black", label.y = .2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Speed (km/hr)", y = "Estimated Bias per Minute")+
  scale_x_continuous(breaks=seq(4,8,0.4))+
  geom_hline(aes(yintercept = 0))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF","#A6761DFF", "#E7298AFF", "#66A61EFF",  "#E6AB02FF"))

# with shapes for each activity
marea_10sec %>%
  filter(cat_activity_large == "treadmill_walkrun") %>%
  group_by(id_subject, time_10, cat_activity_large, cat_activity, speed) %>%
  summarize(across(c(n_seconds, starts_with("steps")), ~sum(.x))) %>% # sum "boundary" periods
  ungroup() %>%
  filter(n_seconds == 10) %>%
  select(contains("30") | contains("truth"), id_subject,
         speed, time_10, cat_activity) %>%
  group_by(id_subject, speed, cat_activity)  %>%
  mutate(n = n()) %>%
  group_by(id_subject, speed, n, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x))) %>%
  mutate(across(starts_with("steps")& !contains("truth"),
                list(bias = ~(steps_truth - .x) * (n*6/60)))) %>% # adjust to get bias per minute so eceryone on same scale
  pivot_longer(cols = ends_with("bias")) %>%
  mutate(algorithm = sub("\\_30.*", "", name),
         speed = as.numeric(sub("km/hr.*", "", speed))) %>%
  ggplot(aes(x = speed, y = value, col = algorithm))+
  geom_jitter(width = .15, alpha = .8, aes(shape = cat_activity))+
  facet_grid(. ~ algorithm, labeller = labeller(algorithm = labs))+
  stat_poly_line(aes(col = algorithm), se = FALSE)+
  # stat_poly_eq(use_label(c("eq", "p")), col = "black", label.y = .2)+
  theme_bw()+
  labs(x = "Speed (km/hr)", y = "Estimated Bias per Minute")+
  scale_x_continuous(breaks=seq(4,8,0.8))+
  scale_shape_manual(name = "", values = c(17, 16), labels = c("Run", "Walk"))+
  geom_hline(aes(yintercept = 0))+
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF","#A6761DFF", "#E7298AFF", "#66A61EFF",  "#E6AB02FF"),
                     guide = "none")+
  theme(legend.position = "bottom")



# one point for every 10s
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
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF","#A6761DFF", "#E7298AFF", "#66A61EFF",  "#E6AB02FF"),
                      guide = "none")


cowplot::plot_grid(indoor, outdoor, nrow = 2)

# look at slope
# marea_10sec %>%
#   filter(grepl("treadmill_slopewalk", cat_activity_large)) %>%
#   mutate(cat_slope = factor(slope, levels = c("0 deg", "5 deg", "10 deg", "15 deg"))) %>%
#   pivot_longer(cols = starts_with("steps") & contains("30")) %>%
#   mutate(algorithm = sub("\\_30.*", "", name)) %>%
#   ggplot(aes(x = steps_truth, y = value))+
#   geom_point(alpha = .5, aes(col = cat_slope))+
#   geom_abline(intercept = 0)+
#   facet_grid(algorithm  ~ cat_slope)+
#   theme_bw()+
#   coord_equal()+
#   theme(legend.position = "none")+
#   labs(x = "True Steps", y = "Predicted Steps")

