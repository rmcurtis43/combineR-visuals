#devtools::install_github("rmcurtis43/combineR")
library(combineR)

# pull combineR data
data <- pull_combine_data()

####################################
library(tidyverse)
library(hrbrthemes)

slj_data <- data %>%
  select(player, draft_year, position2, school, weight_kg, broad_jump_cm, draft_overall_pick) %>%
  drop_na(weight_kg, broad_jump_cm) %>%
  #convert broad jump to peak power using Mann et al. (2021) equation (Power [W] = 32.49·Broad Jump [cm] + 39.69·Wt [kg] − 7608)
  mutate(power_w = (32.49*broad_jump_cm) + (39.69*weight_kg) - 7608) %>%
  filter(!position2 %in% c("PK", "LS")) %>%
  group_by(position2) %>%
  mutate(power_w_position2 = mean(power_w, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(position2 = fct_reorder(position2, power_w_position2))

slj_avg <- slj_data %>%
  summarize(avg = mean(power_w, na.rm = T)) %>%
  pull(avg)

ggplot(slj_data, aes(x = position2, y = power_w, color = position2)) +
  coord_flip() +
  geom_jitter(size = 1.5, alpha = 0.06, width = 0.3) +
  geom_hline(aes(yintercept = slj_avg), color = "gray70", size = 1.5) +
  geom_segment(
    aes(x = position2, xend = position2,
        y = slj_avg, yend = power_w_position2),
    size = 1
  ) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  scale_color_brewer(palette = "Spectral") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE),
                     limits = c(3500, 9000), breaks = c(seq(0, 10000, 1000))) +
  labs(x="Position", y="Peak Power [W]",
       title="Predicted Peak Power [W] by Position vs. Combine Average",
       subtitle="2000 - 2021 NFL Combine Standing Long Jump (SLJ) (n = 5,605)",
       caption="@RyanM_Curtis | data: {combineR} | Pro Football Reference") +
  theme_ft_rc() +
  theme(
    legend.position = "none")


  ggsave("images/power_position.png", dpi = 320, width = 14, height = 10)

