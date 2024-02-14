# make example data figure
# "%m:%S
options(digits.secs = 3)
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
  theme_classic()+
  paletteer::scale_color_paletteer_d("ggthemes::Superfishel_Stone", name = "")+
  labs(x = "Time (s)", y = "Acceleration (g)")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(-1.5,2,.5), labels = c(-1.5,-1.0, -0.5, 0, "Step", 1.0,  1.5, 2.0))

step_temp =
  temp %>%
  filter(time %in% time[30]:time[40] &  cat_activity == "walk_regular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  filter(!is.na(ind_step))

cmlr = temp %>%
  filter(time %in% time[30]:time[40] &  cat_activity == "walk_regular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  # geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line(col = "#333333")+
  theme_classic()+
  geom_vline(data = step_temp, aes(xintercept = tm_dttm), col = "#999999")+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "Clemson Regular Walking")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5),
                     limits=c(.5,2.2))

step_temp =
  temp %>%
  filter(time %in% time[30]:time[40] & cat_activity == "walk_semiregular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  filter(!is.na(ind_step))
clms = temp %>%
  filter(time %in% time[30]:time[40] & cat_activity == "walk_semiregular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  # geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line(col = "#333333")+
  theme_classic()+
  geom_vline(data = step_temp, aes(xintercept = tm_dttm), col = "#999999")+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "Clemson Semiregular Walking")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5),
                     limits=c(.4,2.2))

step_temp =
  temp %>%
  filter(time %in% time[11]:time[21] & cat_activity == "walk_irregular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  filter(!is.na(ind_step))
clmi = temp %>%
  filter(time %in% time[11]:time[21] & cat_activity == "walk_irregular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  # geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line(col = "#333333")+
  theme_classic()+
  geom_vline(data = step_temp, aes(xintercept = tm_dttm), col = "#999999")+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "Clemson Irregular Walking")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5),
                     limits=c(.4,2.2))

library(tidyverse)
marea = readRDS(here::here("data", "processed", "marea_nested_all.rds"))
temp = marea %>% filter(id_subject == "P03")
step_temp =
  temp %>%
  filter(cat_activity == "indoor_walk") %>%
  filter(time %in% time[10]:time[20]) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  filter(!is.na(ind_step))

temp %>%
  filter(cat_activity == "indoor_walk") %>%
  filter(time %in% time[10]:time[20]) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  # geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line(col = "#333333")+
  theme_classic()+
  geom_vline(data = step_temp, aes(xintercept = tm_dttm), col = "#999999")+
  labs(x = "Time (s)", y = "Vector Magnitude (g)")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5),
                     limits=c(.4,2.2))

step_temp =
  temp %>%
  filter(cat_activity == "treadmill_walk") %>%
  filter(time %in% time[240]:time[250]) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  filter(!is.na(ind_step))

mart = temp %>%
  filter(cat_activity == "treadmill_walk") %>%
  filter(time %in% time[240]:time[250]) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  # geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line(col = "#333333")+
  theme_classic()+
  geom_vline(data = step_temp, aes(xintercept = tm_dttm), col = "#999999")+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "MAREA Treadmill Walking")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5),
                     limits=c(.4,2.2))

ox = readRDS(here::here("data", "processed", "oxwalk_nested_all.rds"))
temp = ox %>% filter(id_subject == "P11" & sample_rate==100)

step_temp =
  temp %>%
  filter(time >= as.POSIXct("2019-07-22 10:46:59.000", tz = "UTC") &
           time <= as.POSIXct("2019-07-22 10:46:59.000", tz = "UTC") + as.period(10, "seconds")) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  filter(!is.na(ind_step))

ox100 =
  temp %>%
  filter(time >= as.POSIXct("2019-07-22 10:46:59.000", tz = "UTC") &
           time <= as.POSIXct("2019-07-22 10:46:59.000", tz = "UTC") + as.period(10, "seconds")) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  # geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line(col = "#333333")+
  theme_classic()+
  geom_vline(data = step_temp, aes(xintercept = tm_dttm), col = "#999999")+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "OxWalk Free-Living")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5),
                     limits=c(.4,2.2))

svg(here::here("manuscript", "figures", "raw_data.svg"))
cowplot::plot_grid(clms, clmi, ox100, mart)
dev.off()

# make example data figure
# "%m:%S
library(tidyverse)
clem = readRDS(here::here("data", "processed", "clemson_nested_all.rds"))

temp =
  clem %>%
  filter(id_subject == "P15")


step_temp =
  temp %>%
  filter(time %in% time[30]:time[40] & cat_activity == "walk_semiregular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  mutate(type = "Clemson Semiregular Walking")


step_temp2 =
  temp %>%
  filter(time %in% time[11]:time[21] & cat_activity == "walk_irregular") %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  mutate(type = "Clemson Irregular Walking")

marea = readRDS(here::here("data", "processed", "marea_nested_all.rds"))
temp = marea %>% filter(id_subject == "P03")
step_temp3 =
  temp %>%
  filter(cat_activity == "treadmill_walk") %>%
  filter(time %in% time[240]:time[250]) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  mutate(type = "MAREA Treadmill Walking")

ox = readRDS(here::here("data", "processed", "oxwalk_nested_all.rds"))
temp = ox %>% filter(id_subject == "P11" & sample_rate==100)

step_temp4 =
  temp %>%
  filter(time >= as.POSIXct("2019-07-22 10:46:59.000", tz = "UTC") &
           time <= as.POSIXct("2019-07-22 10:46:59.000", tz = "UTC") + as.period(10, "seconds")) %>%
  unnest(raw_data) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2),
         ind_step = ifelse(ind_step == 1, .5, NA)) %>%
  mutate(type = "OxWalk Free Living")

st =
  bind_rows(step_temp, step_temp2, step_temp3, step_temp4) %>%
  filter(!is.na(ind_step))

plot = bind_rows(step_temp, step_temp2, step_temp3, step_temp4) %>%
  ggplot(aes(x = tm_dttm, y = vm))+
  # geom_point(aes(x = tm_dttm, y = ind_step))+
  geom_line(col = "#333333")+
  theme_classic()+
  facet_wrap(.~type, scales = "free_x") +
  geom_vline(data = st, aes(xintercept = tm_dttm), col = "#999999")+
  labs(x = "Time (s)", y = "Vector Magnitude (g)", title = "")+
  scale_x_datetime(date_breaks = "1 sec", date_labels = "%S")+
  scale_y_continuous(breaks=seq(.5,2,.5),
                     limits=c(.4,2.2))

svg(here::here("manuscript", "figures", "raw_data.svg"))
plot
dev.off()
