setwd("D://tutor//ma//covariate adaptive trial//covariate-adaptive-design//carat simulation")
source("caratHelper.R")
library(carat)
## Random Walk Test
patients_num = 100
df  = simulateDataMa2015(patients_num, 8, 5, 1, 0.5, 2, 0.3)
assignment_adaptive <- myAdjBCD(df, 1:2)$assignment
assignment_simple <- sample(c("A", "B"),patients_num ,TRUE)
df$assignment = assignment_simple

## Observed outcome
df$R_obs = 0
df$R_obs[which(df$assignment == "A")] = df$Y1[which(df$assignment == "A")]
df$R_obs[which(df$assignment == "B")] = df$Y0[which(df$assignment == "B")]

plot(df$X, df$R_obs)
mean_treatment = mean(df$R_obs[which(df$assignment=="A")])
mean_control = mean(df$R_obs[which(df$assignment=="B")])
## remove the global treatment effect
df$R_hat = 0
df$R_hat[which(df$assignment == "A")] = df$R_obs[which(df$assignment == "A")] - mean_treatment
df$R_hat[which(df$assignment == "B")] = df$R_obs[which(df$assignment == "B")] - mean_control
## interaction effect
df$Y_hat = 0
df$Y_hat[which(df$assignment == "A")] = df$R_hat[which(df$assignment == "A")]*1
df$Y_hat[which(df$assignment == "B")] = df$R_hat[which(df$assignment == "B")] * (-1)
plot(df$X,df$Y_hat)

plot(df$Y_hat[order(df$X)])
C = cumsum(df$Y_hat[order(df$X)])
plot(C)

M = max(abs(C))/sqrt(length(C) * var(C))
M
