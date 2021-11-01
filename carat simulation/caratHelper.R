library(carat)
### Refine the ADjBCD
## Randomize based on the target columns
myAdjBCD <- function(df, target_cols, a = 2) {
    ## df: dataframe used for covariate adaptive design
    ## target_cols: columns used for randomization
    
    ## assignment: treatment/control, labeled as "A"/"B"
    ## strata: stratified based on target_cols
    Res <- AdjBCD(df[,target_cols], a = 2)
    assignment = Res$assignments
    
    ## strata
    smallDf = as.matrix(df[,target_cols])
    levelsNum = apply(smallDf, 2,function(x) length(unique(x)))
    culLevelsNum = cumprod(c(1,levelsNum))[1:length(levelsNum)]
    strata = (smallDf - 1) %*% culLevelsNum + 1
    strata = strata
    
    return(list("assignment" = assignment, 
                "strata" = strata))
}

### Simulation methods
simulateDataMa2015 <- function(N, mu1, mu2, beta1, p1, beta2, p2) {
    ## N: simulated sample size
    ## mu1: base potential outcome for treatment group
    ## mu1: base potential outcome for control group
    ## beta1: coefficient for Z1
    ## p1: assigenment probability for Z1 = 1
    ## beta2: coefficient for Z2
    ## p2: assigenment probability for Z1 = 1
    
    ## df: a dataframe of size N, containing Z1, Z2 and potential outcomes 
    df <- data.frame("Z1" = sample(c(1, 2), N, TRUE, c(p1, 1 - p1)),
                     "Z2" = sample(c(1, 2), N, TRUE, c(p1, 1 - p2)),
                     "X" = runif(N,max = 4))
    error = rnorm(N)
    base = beta1 * df$Z1 + beta2 * df$Z2 + error
    df$Y1 = mu1 + base + 1 * df$X  
    df$Y0 = mu2 + base + 1 * df$X  
    return(df)
}

simulateMeanDifference <- function(df, assignment) {
    ## df: a dataframe
    ## assignment: treatment/control, labeled as "A" and "B"
    ## and potential outcomes, labeled as "Y1" and "Y0"
    
    ## mean_difference: mean difference between 
    ## treatment group and control group
    mean_difference =  mean(df[assignment == "A", "Y1"])-  
        mean(df[assignment == "B", "Y0"])
}

