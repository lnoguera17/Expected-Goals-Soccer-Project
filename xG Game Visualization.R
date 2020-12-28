source("Packages Script.R")
library(ggrepel)
library(stringr)
library(tidyverse)


line_xg_graph <- function(match_id) {

  game <- understatr::get_match_shots(match_id = match_id)
  
  game_clean <- game %>% 
    mutate(across(c(minute, xG, X, Y, 
                    player_id, match_id, season), as.numeric)) %>% 
    mutate(xG = if_else(is.na(xG), 0, xG),
           team = case_when(h_a == "h" ~ h_team,
                            TRUE ~ a_team),
           xG = round(xG,2)) %>%
    arrange(id) %>% 
    separate(player, into = c("firstname", "player"), 
             sep = "\\s", extra = "merge") %>% 
    mutate(player = if_else(is.na(player), firstname, player),
           id = row_number()) 
  
  
  sum_xG <- game_clean %>% 
    group_by(team) %>% 
    summarize(xG_sum = cumsum(xG),
              minute = minute,
              id = id) %>% 
    arrange(id) %>% 
    ungroup() 
  
  game_xG <- game_clean %>% 
    left_join(sum_xG) %>% 
    mutate(goals = case_when(result == "Goal" ~ 1,
                             TRUE ~ 0))
  
  game_xG <- game_xG %>% 
    mutate(scorer_xG = case_when(goals == 1 ~ 
                                   select(., firstname, player, xG)  %>%
                                   reduce(str_c, sep=" "),
                                 TRUE ~ "No Goal")) %>%  
    mutate(scorer_xG = na_if(scorer_xG, "No Goal"))
  
  
  # Building the Visz
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

}

# Expected Goal line graph by team
line_xg_graph(14876)
