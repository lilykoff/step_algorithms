## TO DO - make example data figure
# "%m:%S
library(tidyverse)
clem = readRDS(here::here("data", "processed", "clemson_nested_all.rds"))

temp =
  clem %>%
  filter(id_subject == "P15")

temp %>%
  filter(time %in% time[30]:time[40] &  cat_activity == "walk_regular") %>%
  unnest(raw_data) %>%
  pivot_longer(cols=X:Z) %>%
  mutate(ind_step = ifelse(ind_step == 1, 0.5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = value, col = name))+
  geom_line()+
  geom_point(aes(x = tm_dttm, y = ind_step), col = "black")+
  theme_bw()+
  paletteer::scale_color_paletteer_d("ggthemes::Superfishel_Stone", name = "")+
  labs(x = "Time (s)", y = "Acceleration (g)")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(-1.5,2,.5), labels = c(-1.5,-1.0, -0.5, 0, "Step", 1.0,  1.5, 2.0))



cmlr = temp %>%
  filter(time %in% time[30]:time[40] &  cat_activity == "walk_regular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line()+
  theme_bw()+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "Clemson Regular Walking")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5), labels = c("Step",1, 1.5,2),
                     limits=c(.5,2.2))


clms = temp %>%
  filter(time %in% time[30]:time[40] & cat_activity == "walk_semiregular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line()+
  theme_bw()+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "Clemson Semiregular Walking")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5), labels = c("Step",1, 1.5,2),
                     limits=c(.4,2.2))


clmi = temp %>%
  filter(time %in% time[11]:time[21] & cat_activity == "walk_irregular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line()+
  theme_bw()+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "Clemson Irregular Walking")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5), labels = c("Step",1, 1.5,2),
                     limits=c(.4,2.2))

library(tidyverse)
marea = readRDS(here::here("data", "processed", "marea_nested_all.rds"))
temp = marea %>% filter(id_subject == "P03")
temp %>%
  filter(cat_activity == "indoor_walk") %>%
  filter(time %in% time[10]:time[20]) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line()+
  theme_bw()+
  labs(x = "Time (s)", y = "Vector Magnitude (g)")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5), labels = c("Step",1, 1.5,2),
                     limits=c(.4,2.2))

mart = temp %>%
  filter(cat_activity == "treadmill_walk") %>%
  filter(time %in% time[240]:time[250]) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line()+
  theme_bw()+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "MAREA Treadmill Walking")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5), labels = c("Step",1, 1.5,2),
                     limits=c(.4,2.2))

ox = readRDS(here::here("data", "processed", "oxwalk_nested_all.rds"))
temp = ox %>% filter(id_subject == "P11" & sample_rate==100)
ox100 =
  temp %>%
  filter(time >= as.POSIXct("2019-07-22 10:46:59.000", tz = "UTC") &
           time <= as.POSIXct("2019-07-22 10:46:59.000", tz = "UTC") + as.period(10, "seconds")) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line()+
  theme_bw()+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "OxWalk Free-Living")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5), labels = c("Step",1, 1.5,2),
                     limits=c(.4,2.2))

svg(here::here("manuscript", "figures", "raw_data.svg"))
cowplot::plot_grid(clms, clmi, ox100, mart)
dev.off()
