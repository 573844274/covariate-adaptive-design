setwd("D://tutor//ma//covariate adaptive trial")
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
df  = simulateDataMa2015(100, 5, 5, 1, 0.5, 2, 0.3)
mean_difference = numeric(1000)
for (i in 1:1000) {
    df$assignment <- myAdjBCD(df, 1:2)$assignment
    #df$assignment <- sample(c("A", "B"),100 ,TRUE)
    mean_difference[i] = simulateMeanDifference(df)
}
hist(mean_difference)
var(mean_difference)
