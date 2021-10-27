setwd("D://tutor//ma//covariate adaptive trial//covariate-adaptive-design//carat simulation")
source("caratHelper.R")
library(carat)
# a simple use
## Real Data
## create a dataframe
df <- data.frame("gender" = sample(c(1, 2), 1000, TRUE, c(1 / 3, 2 / 3)),
                 "age" = sample(c(1, 2, 3), 1000, TRUE),
                 "jobs" = sample(c(1, 2, 3), 1000, TRUE))

design = myAdjBCD(df,1:3)
df$assignment = design$assignment
df$strata = design$strata

## ma2015
patients_num = 100
times = 1000
df  = simulateDataMa2015(patients_num, 5, 5, 1, 0.5, 2, 0.3)
mean_difference_adaptive = numeric(times)
mean_difference_simple = numeric(times)
mean_difference_complete = numeric(times)

for (i in 1:times) {
    assignment_adaptive <- myAdjBCD(df, 1:2)$assignment
    assignment_simple <- sample(c("A", "B"),patients_num ,TRUE)
    assignment_complete <- sample(c(rep("A",patients_num / 2), 
                              rep("B",patients_num / 2)), patients_num , FALSE)
    mean_difference_adaptive[i] = simulateMeanDifference(df, 
                                                         assignment_adaptive)
    mean_difference_simple[i] = simulateMeanDifference(df, 
                                                       assignment_simple)
    mean_difference_complete[i] = simulateMeanDifference(df, 
                                                         assignment_complete)
}

var(mean_difference_adaptive)
var(mean_difference_simple)
var(mean_difference_complete)
