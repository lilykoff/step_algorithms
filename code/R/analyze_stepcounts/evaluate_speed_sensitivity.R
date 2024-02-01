library(tidyverse)
library(ggpmisc)

# marea_nested  = readRDS("~/Documents/step_algorithms/data/processed/marea_nested_all.rds")
marea = read_csv(here::here("results/all_algorithms/marea_step_estimates_1sec.csv.gz"))


# speed vs bias figure
# bias is estimated per minute
labs = c("ActiLife", "ADEPT", "Oak", "Stepcount (RF)", "Stepcount (SSL)", "SDT", "Verisense")

names(labs) = c("steps_acti", "steps_adept", "steps_oak",  "steps_scrf", "steps_scssl","steps_sdt", "steps_vs")

marea %>%
  filter(cat_activity_large == "treadmill_walkrun") %>%
  group_by(id_subject, cat_activity_large, speed) %>%
  mutate(n_sec = n()) %>%
  select(contains("30") | contains("truth"), id_subject,
         speed, n_sec) %>%
  group_by(id_subject, speed, n_sec) %>%
  summarize(across(starts_with("steps"), ~sum(.x))) %>%
  ungroup() %>%
  mutate(across(starts_with("steps")& !contains("truth"),
                list(bias = ~(steps_truth - .x) * (n_sec/60)))) %>% # adjust to get bias per minute so eceryone on same scale
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
  scale_color_brewer(palette = "Dark2")

# with shapes for each activity
marea %>%
  filter(cat_activity_large == "treadmill_walkrun") %>%
  group_by(id_subject, cat_activity_large, cat_activity, speed) %>%
  mutate(n_sec = n()) %>%
  select(contains("30") | contains("truth"), id_subject,
         speed, n_sec, cat_activity) %>%
  group_by(id_subject, speed, n_sec, cat_activity) %>%
  summarize(across(starts_with("steps"), ~sum(.x))) %>%
  ungroup() %>%
  mutate(across(starts_with("steps")& !contains("truth"),
                list(bias = ~(steps_truth - .x) * (n_sec/60)))) %>% # adjust to get bias per minute so eceryone on same scale
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
  scale_color_brewer(palette = "Dark2", guide = "none")+
  theme(legend.position = c(.09, .2),
        legend.title = element_blank(),
        legend.margin = margin(1,1,1,1),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


