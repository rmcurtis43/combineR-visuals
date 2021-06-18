#devtools::install_github("rmcurtis43/combineR")
library(combineR)
library(tidyverse)
library(hrbrthemes)

# pull combineR data
data <- pull_combine_data()

####################################


broad_jump_in_data <- data %>%
  select(player, draft_year, position2, broad_jump_in) %>%
  drop_na(broad_jump_in) %>%
  filter(!position2 %in% c("PK", "LS")) %>%
  group_by(position2) %>%
  mutate(broad_jump_in_position2 = mean(broad_jump_in, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(position2 = fct_reorder(position2, broad_jump_in_position2))



broad_jump_in_year_position2_mean_change <- broad_jump_in_data %>%
  ungroup() %>%
  group_by(draft_year, position2) %>%
  summarise(avg = mean(broad_jump_in, na.rm = T)) %>%
  drop_na() %>%
  ungroup() %>%
  bind_rows(broad_jump_in_data %>%
              ungroup() %>%
              group_by(draft_year) %>%
              summarise(avg = mean(broad_jump_in, na.rm = T)) %>%
              drop_na() %>%
              ungroup() %>%
              mutate(position2 = 'All Positions')) %>%
  arrange(position2, draft_year) %>%
  left_join(broad_jump_in_data %>%
              ungroup() %>%
              filter(draft_year == 2000) %>%
              group_by(position2) %>%
              summarise(avg_2000 = mean(broad_jump_in, na.rm = T)) %>%
              bind_rows(
                broad_jump_in_data %>%
                  ungroup() %>%
                  filter(draft_year == 2000) %>%
                  summarise(avg_2000 = mean(broad_jump_in, na.rm = T)) %>%
                  mutate(position2='All Positions')
              ), "position2") %>%
  ungroup() %>%
  mutate(change_from_2000 = (avg - avg_2000)/avg_2000)


broad_jump_in_year_position2_mean_change %>%
  nest(-position2) %>%
  mutate(model = map(data, ~ lm(change_from_2000 ~ draft_year, data = .x)),
         slope = map_dbl(model, ~ signif(.x$coef[[2]], 5))) %>%
  select(-data, -model) %>%
  left_join(broad_jump_in_year_position2_mean_change) %>%
  ggplot(aes(draft_year, change_from_2000, color = position2)) +
  geom_point() +
  geom_line() +
  #scale_color_brewer(palette = "Spectral") +
  scale_y_continuous(labels = scales::percent) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dashed", color = 'grey') +
  facet_wrap(~position2) +
  geom_text(aes(2010, .09, label = paste(round(slope*100, 2), '% per year'))) +
  theme_ft_rc() +
  theme(legend.position = "none",
        panel.grid.minor=element_blank()) +
  labs(x="Draft Year", y="% Change from 2000",
       title="Rate of Change in Broad Jump [in]",
       subtitle="2000 - 2021 NFL Draft Combine",
       caption="@RyanM_Curtis | data: {combineR} | Pro Football Reference") 




ggsave("images/broad_jump_in_percent_trend.png", dpi = 320, width = 14, height = 10)



broad_jump_in_year_position2_mean_change %>%
  nest(-position2) %>%
  mutate(model = map(data, ~ lm(avg ~ draft_year, data = .x)),
         slope = map_dbl(model, ~ signif(.x$coef[[2]], 5))) %>%
  select(-data, -model) %>%
  left_join(broad_jump_in_year_position2_mean_change) %>%
  ggplot(aes(draft_year, avg, color = position2)) +
  geom_point() +
  geom_line() +
  #scale_color_brewer(palette = "Spectral") +
  scale_y_continuous(name = 'Broad Jump [in]', limits = c(100, 140)) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dashed", color = 'grey') +
  facet_wrap(~position2) +
  geom_text(aes(2010, 130, label = paste(round(slope, 2), '[in] per year'))) +
  theme_ft_rc() +
  theme(legend.position = "none",
        panel.grid.minor=element_blank()) +
  labs(x="Draft Year", y="Avg",
       title="Rate of Change in Broad Jump [in]",
       subtitle="2000 - 2021 NFL Draft Combine",
       caption="@RyanM_Curtis | data: {combineR} | Pro Football Reference") 


ggsave("images/broad_jump_in_percent_trend.png", dpi = 320, width = 14, height = 10)
