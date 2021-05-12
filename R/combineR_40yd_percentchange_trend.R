#devtools::install_github("rmcurtis43/combineR")
library(combineR)
library(tidyverse)
library(hrbrthemes)

# pull combineR data
data <- pull_combine_data()

####################################


slj_data <- data %>%
  select(player, draft_year, position2, x40yd) %>%
  drop_na(x40yd) %>%
  filter(!position2 %in% c("PK", "LS")) %>%
  group_by(position2) %>%
  mutate(x40yd_position2 = mean(x40yd, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(position2 = fct_reorder(position2, x40yd_position2))



slj_year_position2_mean_change <- slj_data %>%
  ungroup() %>%
  group_by(draft_year, position2) %>%
  summarise(avg = mean(x40yd, na.rm = T)) %>%
  drop_na() %>%
  ungroup() %>%
  bind_rows(slj_data %>%
              ungroup() %>%
              group_by(draft_year) %>%
              summarise(avg = mean(x40yd, na.rm = T)) %>%
              drop_na() %>%
              ungroup() %>%
              mutate(position2 = 'All Positions')) %>%
  arrange(position2, draft_year) %>%
  left_join(slj_data %>%
              ungroup() %>%
              filter(draft_year == 2000) %>%
              group_by(position2) %>%
              summarise(avg_2000 = mean(x40yd, na.rm = T)) %>%
              bind_rows(
                slj_data %>%
                  ungroup() %>%
                  filter(draft_year == 2000) %>%
                  summarise(avg_2000 = mean(x40yd, na.rm = T)) %>%
                  mutate(position2='All Positions')
              ), "position2") %>%
  ungroup() %>%
  mutate(change_from_2000 = (avg - avg_2000)/avg_2000)


slj_year_position2_mean_change %>%
  nest(-position2) %>%
  mutate(model = map(data, ~ lm(change_from_2000 ~ draft_year, data = .x)),
         slope = map_dbl(model, ~ signif(.x$coef[[2]], 5))) %>%
  select(-data, -model) %>%
  left_join(slj_year_position2_mean_change) %>%
  ggplot(aes(draft_year, change_from_2000, color = position2)) +
  geom_point() +
  geom_line() +
  #scale_color_brewer(palette = "Spectral") +
  scale_y_continuous(labels = scales::percent) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dashed", color = 'grey') +
  facet_wrap(~position2) +
  geom_text(aes(2010, .03, label = paste(round(slope*100, 2), '% per year'))) +
  theme_ft_rc() +
  theme(legend.position = "none",
        panel.grid.minor=element_blank()) +
  labs(x="Draft Year", y="% Change from 2000",
       title="Rate of Change in 40yd Time [s]",
       subtitle="2000 - 2021 NFL Draft Combine",
       caption="@RyanM_Curtis | data: {combineR} | Pro Football Reference") 


ggsave("images/x40yd_percent_trend.png", dpi = 320, width = 14, height = 10)
