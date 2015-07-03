# code courtesy user Dason on stackoverflow (https://stackoverflow.com/users/1003565/dason)
#https://stackoverflow.com/questions/29260139/r-function-to-perform-anova-and-tukeyhsd-from-sample-mean-sd-and-n

# how to run an ANOVA when you want to only input the mean, sd and N for each group

# Mean <- c(90,85,92,100,102,106)
# SD <- c(9.035613,11.479667,9.760268,7.662572,9.830258,9.111457)
# SampleSize <- c(9,9,9,9,9,9)
# 
# 
# gen_data <- function(means, sds, samplesizes){
#       n.grp <- length(means)
#       grps <- factor(rep(1:n.grp, samplesizes))
#       dat <- lapply(1:n.grp, function(i) {scale(rnorm(samplesizes[i]))*sds[i] + means[i]})
#       y <- do.call(rbind, dat)
#       out <- data.frame(group = grps, y = y)
#       out
# }
# 
# simulated_data <- gen_data(Mean, SD,SampleSize)
# av <- aov(y ~ group, data = simulated_data)
# summary(av)
# TukeyHSD(av)

gen_data_aov_onlymeansdN <- function(means, sds, samplesizes){
      n.grp <- length(means)
      grps <- factor(rep(1:n.grp, samplesizes))
      dat <- lapply(1:n.grp, function(i) {scale(rnorm(samplesizes[i]))*sds[i] + means[i]})
      y <- do.call(rbind, dat)
      out <- data.frame(group = grps, y = y)
      out
}
