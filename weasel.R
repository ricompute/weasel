weasel <- function(seed = 2718, string = "METHINKS IT IS LIKE A WEASEL", 
                   mutation_prob = 0.05, n_per_generation = 100) {
    library(dplyr)
    
    set.seed(seed)
    
    wtemp <- unlist(strsplit(string, ""))
    wtl <- length(wtemp)
    LETTERSS <- c(LETTERS, " ")
    initial_string <- sample(LETTERSS, 
                             wtl, 
                             replace = TRUE) %>% 
        paste(collapse = "-") %>% 
        gsub("-", "", .)
    
    
    mutate_string <- function(string, prob = mutation_prob) {
        string <- unlist(strsplit(string, ""))
        new_string <- rep("", length(string))
        for (i in seq_along(string)) {
            if (runif(1) <= prob) {
                new_string[i] <- sample(LETTERSS, 1)
            } else {
                new_string[i] <- string[i]
            }
        }
        new_string <- new_string %>% 
            paste(collapse = "-") %>% 
            gsub("-", "", .)
        new_string
    }
    score_string <- function(string, template) {
        string <- unlist(strsplit(string, ""))
        score <- 0
        for(i in seq_along(string)) {
            if (string[i] == template[i]) {
                score <- score + 1
            }
        }
        score
    }
    iterate_string <- function(string, template, n = n_per_generation) {
        res <- data_frame()
        for (i in seq(n)) {
            tstring <- mutate_string(string)
            tscore <- score_string(tstring, template)
            res <- bind_rows(res,
                             data_frame(String = tstring,
                                        Score = tscore))
        }
        res[which.max(res$Score), ]
    }
    
    current <- data_frame(String = initial_string,
                          Score = score_string(initial_string, 
                                               template = wtemp))
    res <- current
    while (current$Score != wtl) {
        current <- iterate_string(current$String, wtemp)
        res <- bind_rows(res, current)
    }
    res
}

