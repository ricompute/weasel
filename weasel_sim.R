library(dplyr)

set.seed(2718)
seeds <- round(runif(200) * 10000)

sim_results <- data_frame()

for (seed in seeds) {
    temp_time <- system.time(temp_result <- weasel(seed = seed))
    sim_results <- bind_rows(sim_results,
                             data_frame(Results = list(String = temp_result$String),
                                        Scores = list(Score = temp_result$Score),
                                        Generations = nrow(temp_result),
                                        Time = temp_time[3]))
}

saveRDS(sim_results, "sim_results.rds")
