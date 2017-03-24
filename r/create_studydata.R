# Write study.txt

library(dplyr)

set.seed(100)

study <- data.frame("id" = 1:150,
                    "sex" = sample(c("m", "f"), size = 150, replace = TRUE),
                    "age" = round(rnorm(n = 150, mean = 25, sd = 2), 0),
                    "condition" = rep(c("A", "B", "C"), times = 50),
                    stringsAsFactors = FALSE)

study$response[study$condition == "A"] <- rnorm(sum(study$condition == "A"), mean = 40, sd = 10)
study$response[study$condition == "B"] <- rnorm(sum(study$condition == "B"), mean = 40, sd = 10)
study$response[study$condition == "C"] <- rnorm(sum(study$condition == "C"), mean = 60, sd = 10)

study$accuracy[study$condition == "A"] <- rnorm(sum(study$condition == "A"), mean = 50, sd = 10)
study$accuracy[study$condition == "B"] <- rexp(50, rate = 1 / 50)
study$accuracy[study$condition == "C"] <- c(rnorm(30, mean = 20, sd = 5), rnorm(20, mean = 100, sd = 5))


study$time <- (study$response + rnorm(150, mean = 0, sd = 10)) * 5

study$response <- round(study$response, 0)
study$time <- round(study$time, 0)
study$accuracy <- round(study$accuracy, 0)

setwd(rprojroot::is_rstudio_project$find_file())

write.table(x = study, file = "data/study.txt", sep = "\t")
