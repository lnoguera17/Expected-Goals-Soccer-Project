source("Packages Script.R")



game_xG <- Sevilla_vs_Celta %>% 
  arrange(time) %>% 
  mutate(team_name = case_when(team_id == 138 ~ "Sevilla", 
                               TRUE ~ "Celta de Vigo")) %>% 
  group_by(team_name) %>% 
  summarize(xG_sum = cumsum(xG),
            time = time,
            goals = goals,
            shots = shots,
            player = player,
            xG = xG) %>% 
  ungroup() 

?str_c()

game_xG %>% 
  ggplot(aes(time, xG_sum, color = team_name)) + 
  geom_line(aes(alpha = 1,
                size = 0.1)) +
  guides(size = F, alpha = F, color = guide_legend("Team Name")) +
  geom_point(aes(size = goals)) +
  geom_text_repel(aes(label = glue("{round(xG,2)}", "xG")), color = 'black', show.legend = F, vjust = -0.7) +
  scale_size_continuous(range = c(0,5)) +
  scale_x_continuous(breaks = seq(0,90, 5),
                     labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, "HT", 50, 55, 60, 65, 70, 75, 80, 85, "FT")) +
  labs(y = "Expected Goals",
       x = "Time") +
  theme_minimal(base_size = 12) + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")



