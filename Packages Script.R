# Load necessary packages

#install.packages('pacmman')
# remotes::install_github('ewenme/understatr')

pacman::p_load(dplyr, tidyr, janitor, 
               purrr, tibble, lubridate, 
               glue, rlang, ggplot2,
               understatr)


Liverpool_vs_Leicester <- get_match_stats(match_id = 14519)


#write.csv(Liverpool_vs_Leicester, "Games Data/Liverpool_vs_Leicester.csv")

Sevilla_vs_Celta <- get_match_stats(match_id = 14873)

#write.csv(Sevilla_vs_Celta, "Games Data/Sevilla_vs_Celta.csv")

shots_Sevilla_Celta <- understatr::get_match_shots(match_id = 14873)
