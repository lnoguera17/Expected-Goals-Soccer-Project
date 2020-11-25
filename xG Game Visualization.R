source("Packages Script.R")



game_xG <- Sevilla_vs_Celta %>% 
  arrange(time) %>% 
  mutate(team_name = case_when(team_id == 138 ~ "Sevilla", 
                               TRUE ~ "Celta de Vigo")) %>% 
  group_by(team_name) %>% 
  summarize(xG_sum = cumsum(xG),
            time = time,
            goals = goals,
            shots = shots) %>% 
  ungroup() 

game_xG %>% 
  ggplot(aes(time, xG_sum, color = team_name)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(0,90, 5),
                     labels = c(0,  5, 10, 15, 20, 25, 30, 35, 40, "HT", 50, 55, 60, 65, 70, 75, 80, 85, "FT")) +
  labs(title = "Match Summary",
       y = "Expected Goals",
       x = "Time")




