# this script generates figures the Summary Table reported in the manuscript.

# Arcara G. (2024) Improving ES. 

library(xtable)

rm(list=ls())

#### ANALYSIS 1
dir.create("Results/Supplementary")
dir.create("Results/Supplementary/Analysis2")

Capitani_vers = c("v2")

Arcara_vers = c("v3")

for (Capitani_ver in Capitani_vers){
  for (Arcara_ver in Arcara_vers){
    
    load(paste("Results/Analysis2/Summary_A", Arcara_ver, "_C", Capitani_ver, ".RData", sep=""))
    
    
    ###########################
    # MOCA
    #############################
    
    ### TRAIN RES (NORMATIVA DATA) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TRAIN_MSE","TRAIN_SPECIFICITY",  "TRAIN_SENSITIVITY",  "TRAIN_PRECISION", "TRAIN_F1")
    
    TRAIN_RES_MOCA = res[res$sample.demo.val=="MOCA", c(dep_oth, dep_num)]
    TRAIN_RES_MOCA = TRAIN_RES_MOCA[order(TRAIN_RES_MOCA$n.subj.train, TRAIN_RES_MOCA$P_score.sd), c(dep_oth, dep_num)]
    TRAIN_RES_MOCA[, dep_num] = apply(TRAIN_RES_MOCA[, dep_num], 2, function(x){round(x, 2)})
    print(TRAIN_RES_MOCA)
    
    names(TRAIN_RES_MOCA) = c("Norm. Data Sample Size", "True Adj Score SD", "Method",  "Version", "MSE", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")

    ### TEST RES (NEW PARTICIPANTS) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TEST_SPECIFICITY", "TEST_SENSITIVITY", "TEST_PRECISION", "TEST_F1")
    
    TEST_RES_MOCA = res[res$sample.demo.val=="MOCA", c(dep_oth, dep_num)]
    TEST_RES_MOCA = TEST_RES_MOCA[order(TEST_RES_MOCA$n.subj.train, TEST_RES_MOCA$P_score.sd), c(dep_oth, dep_num)]
    TEST_RES_MOCA[, dep_num] = apply(TEST_RES_MOCA[, dep_num], 2, function(x){round(x, 2)})
    print(TEST_RES_MOCA)
    
    names(TEST_RES_MOCA) = c("Norm. Data Sample Size", "True Adj Score SD",  "Method", "Version", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")

    ###########################
    # GEMS
    #############################
    
    ### TRAIN RES (NORMATIVA DATA) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TRAIN_MSE","TRAIN_SPECIFICITY",  "TRAIN_SENSITIVITY",  "TRAIN_PRECISION", "TRAIN_F1")
    
    TRAIN_RES_GEMS = res[res$sample.demo.val=="GEMS", c(dep_oth, dep_num)]
    TRAIN_RES_GEMS = TRAIN_RES_GEMS[order(TRAIN_RES_GEMS$n.subj.train, TRAIN_RES_GEMS$P_score.sd), c(dep_oth, dep_num)]
    TRAIN_RES_GEMS[, dep_num] = apply(TRAIN_RES_GEMS[, dep_num], 2, function(x){round(x, 2)})
    print(TRAIN_RES_GEMS)
    
    names(TRAIN_RES_GEMS) = c("Norm. Data Sample Size", "True Adj Score SD", "Method",  "Version", "MSE", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")

    ### TEST RES (NEW PARTICIPANTS) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TEST_SPECIFICITY", "TEST_SENSITIVITY", "TEST_PRECISION", "TEST_F1")
    
    TEST_RES_GEMS = res[res$sample.demo.val=="GEMS", c(dep_oth, dep_num)]
    TEST_RES_GEMS = TEST_RES_GEMS[order(TEST_RES_GEMS$n.subj.train, TEST_RES_GEMS$P_score.sd), c(dep_oth, dep_num)]
    TEST_RES_GEMS[, dep_num] = apply(TEST_RES_GEMS[, dep_num], 2, function(x){round(x, 2)})
    print(TEST_RES_GEMS)
    
    names(TEST_RES_GEMS) = c("Norm. Data Sample Size", "True Adj Score SD",  "Method", "Version", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")

    ###########################
    # TELEGEMS
    #############################
    
    ### TRAIN RES (NORMATIVA DATA) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TRAIN_MSE","TRAIN_SPECIFICITY",  "TRAIN_SENSITIVITY",  "TRAIN_PRECISION", "TRAIN_F1")
    
    TRAIN_RES_TELEGEMS = res[res$sample.demo.val=="TELEGEMS", c(dep_oth, dep_num)]
    TRAIN_RES_TELEGEMS = TRAIN_RES_TELEGEMS[order(TRAIN_RES_TELEGEMS$n.subj.train, TRAIN_RES_TELEGEMS$P_score.sd), c(dep_oth, dep_num)]
    TRAIN_RES_TELEGEMS[, dep_num] = apply(TRAIN_RES_TELEGEMS[, dep_num], 2, function(x){round(x, 2)})
    print(TRAIN_RES_TELEGEMS)
    
    names(TRAIN_RES_TELEGEMS) = c("Norm. Data Sample Size", "True Adj Score SD", "Method",  "Version", "MSE", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")

    ### TEST RES (NEW PARTICIPANTS) MANUSCRIPT
    dep_oth = c("n.subj.train", "P_score.sd", "Method", "Version")
    dep_num = c("TEST_SPECIFICITY", "TEST_SENSITIVITY", "TEST_PRECISION", "TEST_F1")
    
    TEST_RES_TELEGEMS = res[res$sample.demo.val=="TELEGEMS", c(dep_oth, dep_num)]
    TEST_RES_TELEGEMS = TEST_RES_TELEGEMS[order(TEST_RES_TELEGEMS$n.subj.train, TEST_RES_TELEGEMS$P_score.sd), c(dep_oth, dep_num)]
    TEST_RES_TELEGEMS[, dep_num] = apply(TEST_RES_TELEGEMS[, dep_num], 2, function(x){round(x, 2)})
    print(TEST_RES_TELEGEMS)
    
    names(TEST_RES_TELEGEMS) = c("Norm. Data Sample Size", "True Adj Score SD",  "Method", "Version", "SPECIFICITY", "SENSITIVITY", "PRECISION", "F1")

    

    print(xtable(TRAIN_RES_MOCA, type = "latex"), file = paste("Results/Supplementary/Analysis2/TRAIN_RES_MOCA_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    print(xtable(TEST_RES_MOCA, type = "latex"), file = paste("Results/Supplementary/Analysis2/TEST_RES_MOCA_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    
    print(xtable(TRAIN_RES_GEMS, type = "latex"), file = paste("Results/Supplementary/Analysis2/TRAIN_RES_GEMS_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    print(xtable(TEST_RES_GEMS, type = "latex"), file = paste("Results/Supplementary/Analysis2/TEST_RES_GEMS_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    
    
    print(xtable(TRAIN_RES_TELEGEMS, type = "latex"), file = paste("Results/Supplementary/Analysis2/TRAIN_RES_TELEGEMS_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    print(xtable(TEST_RES_TELEGEMS, type = "latex"), file = paste("Results/Supplementary/Analysis2/TEST_RES_TELEGEMS_A", Arcara_ver,"_C", Capitani_ver, ".tex", sep=""), include.rownames = F)
    
    
  }
}


