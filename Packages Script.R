# Load necessary packages

#install.packages('pacmman')
# remotes::install_github('ewenme/understatr')

pacman::p_load(dplyr, tidyr, janitor, 
               purrr, tibble, lubridate, 
               glue, rlang, ggplot2,
               understatr)


# Testing the understatr API
Sevilla_vs_Celta <- get_match_shots(match_id = 14873)


