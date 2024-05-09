# this script is used to returns some simple results for a fast comparison across methods.

# Arcara G. (2024) Improving ES. 

rm(list=ls())

## SET PARAMETERS
sample_size = 100
Noise_level = 1.5
Arcara_ver = "v2"
Capitani_ver = "v3"
sample_demo_fun = "sample.demo.unif"
tag=""

curr_file = paste("Results/Analysis1/Sim_Nsize_", sample_size, "_Noise_", Noise_level, "_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".RData", sep="")
load(curr_file)

# CALCULATE TYPE 1 ERROR
mean(TEST_CAP_NBELOW/sim.params$n.subj.test)
mean(TEST_ARC_NBELOW/sim.params$n.subj.test)
mean(TEST_GT_NBELOW/sim.params$n.subj.test)

mean(TRAIN_CAP_F1, na.rm=TRUE)
mean(TRAIN_ARC_F1, na.rm=TRUE)

mean(TRAIN_CAP_SENSITIVITY, na.rm=T)
mean(TRAIN_ARC_SENSITIVITY, na.rm=T)

mean(TRAIN_CAP_SPECIFICITY, na.rm=T)
mean(TRAIN_ARC_SPECIFICITY, na.rm=T)

mean(TRAIN_CAP_PRECISION, na.rm=T)
mean(TRAIN_ARC_PRECISION, na.rm=T)

mean(TEST_CAP_SENSITIVITY, na.rm=T)
mean(TEST_ARC_SENSITIVITY, na.rm=T)

mean(TEST_CAP_SPECIFICITY, na.rm=T)
mean(TEST_ARC_SPECIFICITY, na.rm=T)

mean(TEST_CAP_PRECISION, na.rm=T)
mean(TEST_ARC_PRECISION, na.rm=T)

mean(TEST_CAP_F1, na.rm=T)
mean(TEST_ARC_F1, na.rm=T)


##
plot(TRAIN_ARC_RS, TRAIN_ARC_PRECISION) # as in the Train  there are only 5% by definition values are few
plot(TRAIN_ARC_RS, TEST_ARC_PRECISION)




