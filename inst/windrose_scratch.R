
library(openair)

dat <- polar_data %>%
  dplyr::group_split(site) %>%
  rlang::set_names(unique(polar_data$site))

nott <- openair::importAURN(site = "sunr", year = 2020)
openair::polarPlot(nott)
gg_polar(nott, "nox")

openair::windRose(mydata)$data -> data


library(tidyverse)

plot_dat <-
  data %>%
  filter(wd >= 0) %>%
  pivot_longer(matches("Interval\\d+"), names_prefix = "Interval",
               names_transform = list(name = as.integer)) %>%
  group_by(default, wd) %>%
  mutate(value = if_else(!is.na(lag(value)),
                         value - lag(value),
                         value)) %>%
  mutate(name = factor(name) %>% fct_inseq() %>% fct_rev(),
         wd = factor(wd)) %>%
  ungroup(wd) %>%
  mutate(lab = str_glue("mean = {panel.fun} | calm = {calm}%"))

data %>%
  filter(wd >= 0) %>%
  pivot_longer(matches("Interval\\d+"), names_prefix = "Interval",
               names_transform = list(name = as.integer)) %>%
  arrange(name) %>%
  group_by(default, wd) %>%
  mutate(value = if_else(!is.na(lag(value)),
                         value - lag(value),
                         value))  %>%
  ungroup(wd) %>%
  mutate(lab = str_glue("mean = {panel.fun} | calm = {calm}%"),
         name = if_else(name == 8, 9L, name))%>%
  ggplot(aes(x = wd)) +
  geom_col(aes(y = value, fill = name), width = 30/(3/2), color = "white") +
  scale_fill_stepsn(breaks = -1:9, show.limits = T, colors = openColours(), right = F) +
  coord_polar(start = 15 / 360 * 2 * pi) +
  scale_x_continuous(breaks = c(90, 180, 270, 360),
                     limits = c(15, 375),
                     labels = c("E", "S", "W", "N")) +
  ggopenair::theme_polar(guides = T) +
  theme(legend.key.height = unit(1.5, "cm")) +
  # scale_fill_manual(values = rev(openColours(n = 8))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(vars(default, lab)) +
  expand_limits(y = -2)

plot_dat %>%
  ggplot(aes(x = wd, y = value)) +
  geom_col(aes(fill = factor(name)), width = .75) +
  coord_polar(start = pi / 12) +
  scale_fill_manual(values = rev(openColours(n = 4))) +
  theme_minimal() +
  facet_wrap(vars(default, lab)) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_discrete(breaks = c(90, 180, 270, 360),
                   labels = c("E", "S", "W", "N"))
