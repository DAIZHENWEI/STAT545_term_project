library(survival)

setwd("C:/STAT 545/Term Project")
dat = read.csv('./Data2012 without AWT(v3).csv')

dat_sub = dat[dat$TotalNo<100,]

m1 = coxph(Surv(Ranking, Status) ~ Eta1+LogClaim, data = dat_sub)
summary(m1)

m2 = coxph(Surv(Ranking, Status) ~ Eta1+LogClaim+strata(NoRace), data = dat_sub)
summary(m2)



## model is the fitted coxph model
## dat_test includes the features and Rank (Rank in the last column)
## Note that the order of features in the dat_test is the same as that in the model

CalLikelihood = function(model, dat_test, TotalNo, topk=3){
  coef = as.matrix(model$coefficients)
  dat_split = split(dat_test, TotalNo)
  prob_list = c()
  for (ix in c(1:length(dat_split))){
    race_dat = dat_split[[ix]]
    ## Sort the horses by rank
    race_feature = as.matrix(race_dat[order(race_dat$Rank),-ncol(race_dat)])
    race_logit = exp(race_feature %*% coef)
    prob = 1
    for (i in c(1:topk)){
      prob = prob*(race_logit[i]/ sum(race_logit[-c(1:i)]))
    }
    prob_list = c(prob_list, prob)
  }
  return(prob_list)
}


## Example 

dat_test = data.frame(Eta1 = dat_sub$Eta1, LogClaim = dat_sub$LogClaim, 
                      Rank=dat_sub$Rank)
TotalNo = dat_sub$TotalNo

CalLikelihood(m2, dat_test, TotalNo, topk = 1)







