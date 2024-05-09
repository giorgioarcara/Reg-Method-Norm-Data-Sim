# this script generates figures the Summary Table reported in the manuscript.

# Arcara G. (2024) Improving ES. 

library(xtable)

rm(list=ls())

dir.create("Results/Manuscript")

#### ANALYSIS 1
load("Results/Analysis1/Summary_Av3_Cv2.RData")

###########################
# CONDITIONED DISTRIBUTION
#############################

### TRAIN RES (NORMATIVA DATA) MANUSCRIPT
dep_oth = c("n.subj.train", "P_score.sd", "Method")
dep_num = c("TRAIN_MSE","TRAIN_SPECIFICITY",  "TRAIN_SENSITIVITY",  "TRAIN_PRECISION", "TRAIN_F1")

TRAIN_RES_COND = res[res$sample.demo.fun=="sample.demo.cond", c(dep_oth, dep_num)]
TRAIN_RES_COND = TRAIN_RES_COND[order(TRAIN_RES_COND$n.subj.train, TRAIN_RES_COND$P_score.sd), c(dep_oth, dep_num)]
TRAIN_RES_COND[, dep_num] = apply(TRAIN_RES_COND[, dep_num], 2, function(x){round(x, 2)})
print(TRAIN_RES_COND)

names(TRAIN_RES_COND) = c("Norm. Data Sample Size", "Noise Level", "Method", "MSE", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")
write.table(TRAIN_RES_COND, file = "Results/Manuscript/TRAIN_RES_COND.txt", sep="\t", row.names = F, quote=F)

### TEST RES (NEW PARTICIPANTS) MANUSCRIPT
dep_oth = c("n.subj.train", "P_score.sd", "Method")
dep_num = c("TEST_SPECIFICITY", "TEST_SENSITIVITY", "TEST_PRECISION", "TEST_F1")

TEST_RES_COND = res[res$sample.demo.fun=="sample.demo.cond", c(dep_oth, dep_num)]
TEST_RES_COND = TEST_RES_COND[order(TEST_RES_COND$n.subj.train, TEST_RES_COND$P_score.sd), c(dep_oth, dep_num)]
TEST_RES_COND[, dep_num] = apply(TEST_RES_COND[, dep_num], 2, function(x){round(x, 2)})
print(TEST_RES_COND)

names(TEST_RES_COND) = c("Norm. Data Sample Size", "Noise Level",  "Method", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")
write.table(TEST_RES_COND, file = "Results/Manuscript/TEST_RES_COND.txt", sep="\t", row.names = F, quote=F)

#######################
# UNIFORM DISTRIBUTION
#######################

### TRAIN RES (NORMATIVA DATA) MANUSCRIPT
dep_oth = c("n.subj.train", "P_score.sd", "Method")
dep_num = c("TRAIN_MSE","TRAIN_SPECIFICITY",  "TRAIN_SENSITIVITY",  "TRAIN_PRECISION", "TRAIN_F1")

TRAIN_RES_UNIF = res[res$sample.demo.fun=="sample.demo.unif", c(dep_oth, dep_num)]
TRAIN_RES_UNIF = TRAIN_RES_UNIF[order(TRAIN_RES_UNIF$n.subj.train, TRAIN_RES_UNIF$P_score.sd), c(dep_oth, dep_num)]
TRAIN_RES_UNIF[, dep_num] = apply(TRAIN_RES_UNIF[, dep_num], 2, function(x){round(x, 2)})
print(TRAIN_RES_UNIF)

names(TRAIN_RES_UNIF) = c("Norm. Data Sample Size", "Noise Level", "Method", "MSE", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")
write.table(TRAIN_RES_UNIF, file = "Results/Manuscript/TRAIN_RES_UNIF.txt", sep="\t", row.names = F, quote=F)

### TEST RES (NEW PARTICIPANTS) MANUSCRIPT
dep_oth = c("n.subj.train", "P_score.sd", "Method")
dep_num = c("TEST_SPECIFICITY", "TEST_SENSITIVITY", "TEST_PRECISION", "TEST_F1")

TEST_RES_UNIF = res[res$sample.demo.fun=="sample.demo.unif", c(dep_oth, dep_num)]
TEST_RES_UNIF = TEST_RES_UNIF[order(TEST_RES_UNIF$n.subj.train, TEST_RES_UNIF$P_score.sd), c(dep_oth, dep_num)]
TEST_RES_UNIF[, dep_num] = apply(TEST_RES_UNIF[, dep_num], 2, function(x){round(x, 2)})
print(TEST_RES_UNIF)

names(TEST_RES_UNIF) = c("Norm. Data Sample Size", "Noise Level", "Method", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")  
write.table(TEST_RES_UNIF, file = "Results/Manuscript/TEST_RES_UNIF.txt", sep="\t", row.names = F, quote=F)



print(xtable(TRAIN_RES_COND, type = "latex"), file = "Results/Manuscript/TRAIN_RES_COND.tex", include.rownames = F)
print(xtable(TRAIN_RES_UNIF, type = "latex"), file = "Results/Manuscript/TRAIN_RES_UNIF.tex", include.rownames = F)
print(xtable(TEST_RES_UNIF, type = "latex"), file = "Results/Manuscript/TEST_RES_UNIF.tex", include.rownames = F)
print(xtable(TEST_RES_COND, type = "latex"), file = "Results/Manuscript/TEST_RES_COND.tex", include.rownames = F)


######################
### AGGREGATE RESULTS (OTHER RESULTS NOT SHOWN ON MANUSCRIPT)
######################

### TRAIN RESULTS - aggregate sample size and True Adj Score SD levels 
train_agg = aggregate(cbind(TRAIN_MSE, TRAIN_SENSITIVITY, TRAIN_SPECIFICITY, TRAIN_PRECISION, TRAIN_F1)~Method+sample.demo.fun, data=res, FUN=mean)
train_agg[, c("TRAIN_MSE", "TRAIN_SENSITIVITY", "TRAIN_SPECIFICITY","TRAIN_PRECISION",  "TRAIN_F1")] = apply(train_agg[, c("TRAIN_MSE", "TRAIN_SENSITIVITY", "TRAIN_SPECIFICITY", "TRAIN_PRECISION", "TRAIN_F1")], 2, function(x){round(x, digits=2)})
print(t(train_agg))

## TEST RESULTS - aggregate sample size and True Adj Score SD levels  
test_agg = aggregate(cbind(TEST_SENSITIVITY, TEST_SPECIFICITY, TEST_PRECISION,TEST_F1)~Method+sample.demo.fun, data=res, FUN=mean)
test_agg[, c("TEST_SENSITIVITY", "TEST_SPECIFICITY", "TEST_PRECISION","TEST_F1")] = apply(test_agg[, c("TEST_SENSITIVITY", "TEST_SPECIFICITY", "TEST_PRECISION", "TEST_F1")], 2, function(x){round(x, digits=2)})
print(t(test_agg))








