rm(list=ls())
source("R_functions/sim.norm.data.R")
source("R_functions/sample.demo.unif.R")
source("R_functions/sample.demo.cond.R")
source("R_functions/sample.coef.unif.R")
source("R_functions/model_transf_text.R")
source("R_functions/formula_transf_text.R")
source("R_functions/sample.transf.R")
source("R_functions/adjscores_C1987_v2.R")
source("R_functions/transf_functions.R")
source("R_functions/adjscores_A2024_v3.R")

library(effects)
library(car)

set.seed(NULL)


n.subj.train = 600 # number of subjects in each train set (i.e., the normative data)
Adj.score_sd = 1 # a multiplier to have an effect of noise (in terms of error), not influenced by the observed score range in the simulation.

## coefficients parameters
c.range = list(c(18, 20), c(0, -4), c(0, 4), c(-1, 1))
c.steps = c(100,100,100,100)


age.include.transf =  NULL
age.exclude.transf = "poly2" #c("quadr", "cube")
edu.include.transf =  NULL #"quadr"# NULL
edu.exclude.transf = "poly2" #c("quadr", "cube")


## SIMULATE NORMATIVE DATA
## 1) simulate data with real adj score
train.demo.data = sample.demo.cond(n.subj.train)
head(train.demo.data)
train.coefs = sample.coef.unif(c.range = c.range,
                               c.steps=c.steps)

train.age_transf = sample.transf(include_transf = age.include.transf, exclude_transf = age.exclude.transf) 
train.edu_transf = sample.transf(include_transf = edu.include.transf, exclude_transf = edu.exclude.transf) 



train.res = sim.norm.data(n = n.subj.train,
                             coefs = train.coefs,
                             P_score.sd = Adj.score_sd,
                             age_values_o =  train.demo.data$age_values_o,
                             edu_values_o = train.demo.data$edu_values_o,
                             sex_values_o = train.demo.data$sex_values_o,
                             age_transf = train.age_transf,
                             edu_transf = train.edu_transf, 
                             details=TRUE)


train.dat = train.res[[1]]

range(train.dat$Obs_score) 

## print ground truth Formula
print(train.res[[2]])

head(train.dat[1,])


# Results with Capitani fitting method
CAP.res = adjscores_C1987(df=train.dat, dep="Obs_score", 
                          age="Age", edu="Edu", sex="Sex",
                          dep.range = c(0, max(train.dat$Obs_score)))

summary(CAP.res$lm.model)
print(CAP.res$model_text)

plot(allEffects(CAP.res$lm.model, partial.residuals=T), residuals.cex=0.2)


ARC.res = adjscores_A2024(df = train.dat, dep="Obs_score", 
                          age="Age", edu="Edu", sex="Sex",
                          dep.range = c(0, max(train.dat$Obs_score)))


AIC(ARC.res$lm.model)
print(ARC.res$model_text)


plot(allEffects(ARC.res$lm.model, partial.residuals=T), residuals.cex=0.2)
plot(allEffects(CAP.res$lm.model, partial.residuals=T), residuals.cex=0.2)
paste("Capitani1987 - R-squared =", round(summary(CAP.res$lm.model)$r.squared, 3))
paste("Arcara2024 - R-squared=", round(summary(ARC.res$lm.model)$r.squared, 3))
paste("Capitani1987 AIC =", round(AIC(CAP.res$lm.model), 3))
paste("ARC AIC=", round(AIC(ARC.res$lm.model), 3))


plot(allEffects(CAP.res$lm.model, partial.residuals=T), residuals.cex=0.2)

