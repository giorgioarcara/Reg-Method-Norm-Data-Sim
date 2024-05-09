# this script generates figures the Summary Table reported in the manuscript.

# Arcara G. (2024) Improving ES. 

library(xtable)

rm(list=ls())

#### ANALYSIS 1
dir.create("Results/Supplementary")
dir.create("Results/Supplementary/Analysis4")

Capitani_vers = c("v2")

Arcara_vers = c("v3")

for (Capitani_ver in Capitani_vers){
  for (Arcara_ver in Arcara_vers){
    
    load(paste("Results/Analysis1/Summary_A", Arcara_ver, "_C", Capitani_ver, ".RData", sep=""))
    
    
    ###########################
    # CONDITIONED DISTRIBUTION
    #############################
    
    ### TRAIN RES (NORMATIVA DATA) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TRAIN_MSE","TRAIN_SPECIFICITY",  "TRAIN_SENSITIVITY",  "TRAIN_PRECISION", "TRAIN_F1")
    
    TRAIN_RES_COND = res[res$sample.demo.fun=="sample.demo.cond", c(dep_oth, dep_num)]
    TRAIN_RES_COND = TRAIN_RES_COND[order(TRAIN_RES_COND$n.subj.train, TRAIN_RES_COND$P_score.sd), c(dep_oth, dep_num)]
    TRAIN_RES_COND[, dep_num] = apply(TRAIN_RES_COND[, dep_num], 2, function(x){round(x, 2)})
    print(TRAIN_RES_COND)
    
    names(TRAIN_RES_COND) = c("Norm. Data Sample Size", "True Adj Score SD", "Method",  "Version", "MSE", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")

    ### TEST RES (NEW PARTICIPANTS) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TEST_SPECIFICITY", "TEST_SENSITIVITY", "TEST_PRECISION", "TEST_F1")
    
    TEST_RES_COND = res[res$sample.demo.fun=="sample.demo.cond", c(dep_oth, dep_num)]
    TEST_RES_COND = TEST_RES_COND[order(TEST_RES_COND$n.subj.train, TEST_RES_COND$P_score.sd), c(dep_oth, dep_num)]
    TEST_RES_COND[, dep_num] = apply(TEST_RES_COND[, dep_num], 2, function(x){round(x, 2)})
    print(TEST_RES_COND)
    
    names(TEST_RES_COND) = c("Norm. Data Sample Size", "True Adj Score SD",  "Method", "Version", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")

    #######################
    # UNIFORM DISTRIBUTION
    #######################
    
    ### TRAIN RES (NORMATIVA DATA) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TRAIN_MSE","TRAIN_SPECIFICITY",  "TRAIN_SENSITIVITY",  "TRAIN_PRECISION", "TRAIN_F1")
    
    TRAIN_RES_UNIF = res[res$sample.demo.fun=="sample.demo.unif", c(dep_oth, dep_num)]
    TRAIN_RES_UNIF = TRAIN_RES_UNIF[order(TRAIN_RES_UNIF$n.subj.train, TRAIN_RES_UNIF$P_score.sd), c(dep_oth, dep_num)]
    TRAIN_RES_UNIF[, dep_num] = apply(TRAIN_RES_UNIF[, dep_num], 2, function(x){round(x, 2)})
    print(TRAIN_RES_UNIF)
    
    names(TRAIN_RES_UNIF) = c("Norm. Data Sample Size", "True Adj Score SD", "Method", "Version", "MSE", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")
    
    ### TEST RES (NEW PARTICIPANTS) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TEST_SPECIFICITY", "TEST_SENSITIVITY", "TEST_PRECISION", "TEST_F1")
    
    TEST_RES_UNIF = res[res$sample.demo.fun=="sample.demo.unif", c(dep_oth, dep_num)]
    TEST_RES_UNIF = TEST_RES_UNIF[order(TEST_RES_UNIF$n.subj.train, TEST_RES_UNIF$P_score.sd), c(dep_oth, dep_num)]
    TEST_RES_UNIF[, dep_num] = apply(TEST_RES_UNIF[, dep_num], 2, function(x){round(x, 2)})
    print(TEST_RES_UNIF)
    
    names(TEST_RES_UNIF) = c("Norm. Data Sample Size", "True Adj Score SD", "Method", "Version", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")  


    print(xtable(TRAIN_RES_UNIF, type = "latex"), file = paste("Results/Supplementary/Analysis1/TRAIN_RES_UNIF_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    print(xtable(TRAIN_RES_COND, type = "latex"), file = paste("Results/Supplementary/Analysis1/TRAIN_RES_COND_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    print(xtable(TEST_RES_UNIF, type = "latex"), file = paste("Results/Supplementary/Analysis1/TEST_RES_UNIF_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    print(xtable(TEST_RES_COND, type = "latex"), file = paste("Results/Supplementary/Analysis1/TEST_RES_COND_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    
    
  }
}


