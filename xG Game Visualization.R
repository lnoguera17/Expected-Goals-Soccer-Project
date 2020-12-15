source("Packages Script.R")
library(ggrepel)
library(stringr)



# Data Wrangling
home_color <- "#bb1628"
away_color <- "#2d3252"



game_xG <- shots_Sevilla_Celta %>% 
  mutate(team = as.factor(case_when(h_a == "h" ~ h_team,
                          TRUE ~ a_team))) %>% 
  mutate(across(c(minute, xG, X, Y, 
                  player_id, match_id, season), as.numeric)) %>% 
  mutate(xG = if_else(is.na(xG), 0, xG)) %>%  
  ## 6. Add team colors to the row depending on the team.
  mutate(team_color = if_else(team == h_team, home_color, away_color)) %>% 
  arrange(id) %>% 
  separate(player, into = c("firstname", "player"), 
           sep = "\\s", extra = "merge") %>% 
  ## players like Fabinho are listed without a last name "Tavares"
  ## so just add their name in again if NA
  mutate(player = if_else(is.na(player), firstname, player),
         ## 11. Set a new and cleaner ID for shots so that it starts at 1 and goes to `n`.
         id = row_number()) 

sum_xG <- game_xG %>% 
  group_by(team) %>% 
  summarize(xG_sum = cumsum(xG),
            id = id) %>% 
  arrange(id) %>% 
  ungroup()
  

# Joinin the two tables
game_xG <- game_xG %>% 
  left_join(sum_xG) %>% 
  mutate(goals = case_when(result == "Goal" ~ 1,
                          TRUE ~ 0))


game_xG <- game_xG %>% 
  mutate(xG = round(xG,2),
         scorer_xG = case_when(goals == 1 ~ 
                                 select(., firstname, player, xG)  %>%
                                 reduce(str_c, sep=" "),
                               TRUE ~ "No Goal")) %>%  
  mutate(scorer_xG = na_if(scorer_xG, "No Goal"))


# Time line xG Visualization
game_xG %>% 
  ggplot(aes(minute, xG_sum, color = team)) + 
  geom_line(aes(alpha = 1,
                size = 0.1)) +
  guides(size = F, alpha = F, color = guide_legend("Team Name")) +
  geom_point(aes(size = goals)) +
  geom_text_repel(aes(label = scorer_xG), show.legend = F, vjust = -0.7, color = "black") +
  scale_size_continuous(range = c(0,5)) +
  scale_x_continuous(breaks = seq(0,90, 5),
                     labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, "HT", 50, 55, 60, 65, 70, 75, 80, 85, "FT")) +
  labs(y = "Expected Goals",
       x = "Time") +
  theme_minimal(base_size = 12) + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")


