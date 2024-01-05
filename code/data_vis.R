# do some basic visaulization

clem_01_reg =
  readRDS("~/Documents/step_algorithms/data/processed/clemson/P01/clemson-P01-walk_regular-nested.rds")


clem_01_reg %>%
  slice(40:60) %>%
  unnest(cols = raw_data) %>%
  pivot_longer(cols = starts_with("steps") & !contains("30")) %>%
  ggplot(aes(x = tm_dttm, y = sqrt(X^2 + Y^2 + Z^2)))+
  geom_line()+
  geom_jitter(aes(x = time,y = value, col = value), width = .1, alpha= .5)+
  theme_bw()+
  facet_grid(.~name)

clem_01_reg %>%
  slice(40:50) %>%
  unnest(cols = raw_data) %>%
  pivot_longer(cols = starts_with("steps") & !contains("30") & !contains("sc")) %>%
  ggplot()+
  geom_rect(aes(xmin = time, xmax = time + .9, fill = as.factor(round(value, 1)), ymin = 0, ymax = 2))+
  geom_line(aes(x = tm_dttm, y = sqrt(X^2 + Y^2 + Z^2)))+
  theme_bw()+
  facet_grid(.~name)+
  labs(x = "Time (s)", y = "VM")+
  theme(legend.position = "bottom")+
  paletteer::scale_fill_paletteer_d("colorBlindness::LightBlue2DarkBlue7Steps", name = "Steps")

clem_01_reg %>%
  slice(40:50) %>%
  unnest(cols = resampled_data) %>%
  pivot_longer(cols = starts_with("steps") & (contains("30") | contains("truth")) & !contains("sc")) %>%
  ggplot()+
  geom_rect(aes(xmin = time, xmax = time + .9, fill = as.factor(round(value, 1)), ymin = 0, ymax = 2))+
  geom_line(aes(x = tm_dttm, y = sqrt(X^2 + Y^2 + Z^2)))+
  theme_bw()+
  facet_grid(.~name)+
  labs(x = "Time (s)", y = "VM")+
  theme(legend.position = "bottom")+
  paletteer::scale_fill_paletteer_d("colorBlindness::LightBlue2DarkBlue7Steps", name = "Steps")

