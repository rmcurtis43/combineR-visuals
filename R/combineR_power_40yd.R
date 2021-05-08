#devtools::install_github("rmcurtis43/combineR")
library(combineR)
library(tidyverse)
library(ggpubr)
library(hrbrthemes)


# pull all combineR data
data <- pull_combine_data()

####################################


ex <- data %>%
  #convert broad jump to peak power using Mann et al. (2021) equation (Power [W] = 32.49·Broad Jump [cm] + 39.69·Wt [kg] − 7608)
  mutate(power_w = (32.49*broad_jump_cm) + (39.69*weight_kg) - 7608) %>%
  select(position2, power_w, x40yd) %>%
  drop_na(power_w, x40yd) %>%
  filter(!position2 %in% c('LS', 'PK')) %>%
  bind_rows(
    data %>%
      filter(!position2 %in% c('LS', 'PK')) %>%
      mutate(power_w = (32.49*broad_jump_cm) + (39.69*weight_kg) - 7608) %>%
      select(power_w, x40yd) %>%
      drop_na(power_w, x40yd) %>%
      mutate(position2='All Positions') %>%
      select(position2, power_w, x40yd)
  )


ggscatter(ex, x = "power_w", y = "x40yd", add = "reg.line", color = 'position2', fullrange = TRUE,alpha = 0.3) +
  scale_color_ipsum() +
  scale_fill_ipsum() +
  facet_wrap(~position2, scales="free") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  stat_cor(label.x = 2500, label.y = 6.2) +
  #stat_regline_equation(label.x = 2500, label.y = 6.3) +
  labs(x="Peak Power [W]", y="40 yd Time [s]",
       title="Relationship between Predicted Peak Power [W] and 40 yd Time",
       subtitle="2000 - 2021 NFL Draft Combine Data",
       caption="@RyanM_Curtis | data: {combineR} | Pro Football Reference") +
  theme_ipsum_pub() +
  theme(
    legend.position = "none",
    panel.grid.major=element_blank(),
    panel.grid.minor = element_blank())


ggsave("images/power_40yd.png", dpi = 320, width = 14, height = 10)
