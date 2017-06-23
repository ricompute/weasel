library(tidyverse)

sim_results <- readRDS("sim_results.rds")

sim_results %>% 
    add_column(Iteration = seq(200)) %>% 
    unnest(Scores) %>% 
    group_by(Iteration) %>% 
    mutate(Generation = seq(Generations),
           Score_pct = Scores / 28 * 100) -> sr

(ggplot(sr, 
        aes(x = Generation, 
            y = Score_pct, 
            color = Iteration)) +
        geom_point())
