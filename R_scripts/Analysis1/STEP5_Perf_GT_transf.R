# This script is used to generate figures to check the performance of the methods depeding on the GT transformation.
# These figures are reported in the Supplementary Materials.

# Arcara G. (2024) Improving ES. 

rm(list=ls())


sample_size = 300
Noise_level = 1
Arcara_ver = "v3"
Capitani_ver = "v2"
sample_demo_fun = "sample.demo.cond"
tag=""

curr_file = paste("Results/Analysis1/Sim_Nsize_", sample_size, "_Noise_", Noise_level, "_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".RData", sep="")
load(curr_file)

# createa dataframe for GT TRANSF
TRANSF_DAT = as.data.frame(matrix(unlist(TRAIN_GT_TRANSF), ncol=2, byrow = T))
names(TRANSF_DAT) = c("age_transf", "edu_transf")

# create dataset for correspondence between GT transformation and performance
TRANSF_DAT$CAP_F1 = TEST_CAP_F1
TRANSF_DAT$ARC_F1 = TEST_ARC_F1

AGE_TRANSF_CAP_F1 = with(TRANSF_DAT, tapply(CAP_F1, age_transf, mean, na.rm=T))
AGE_TRANSF_ARC_F1 = with(TRANSF_DAT, tapply(ARC_F1, age_transf, mean, na.rm=T))

EDU_TRANSF_CAP_F1 = with(TRANSF_DAT, tapply(CAP_F1, edu_transf, mean, na.rm=T))
EDU_TRANSF_ARC_F1 = with(TRANSF_DAT, tapply(ARC_F1, edu_transf, mean, na.rm=T))

##########################
# AGE TRANSFORMATION PLOT
##########################
jpeg(paste("Figures/Analysis1/AGE_TRANSF_PERF_", "Nsize_", sample_size, "_Noise_", 
           Noise_level, "_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".jpeg", sep=""), res=100, width=1400, height=600)

blue = rgb(23/255, 107/255, 239/255)
yellow = rgb(247/255, 181/255, 41/255)

par(mfrow=c(1,2), mar = c(6.1, 4.1, 4.1, 2.1))

barplot(AGE_TRANSF_CAP_F1, col=yellow, main="Capitani 1987 - v2\n Age transformation", xlab="", ylab="F1", ylim=c(0,1), las=2) 
barplot(AGE_TRANSF_ARC_F1, col=blue, main="Arcara 2024 - v3\n Age transformation", xlab="", ylab="F1", ylim=c(0,1), las=2) 

dev.off()

##########################
# EDUCATION TRANSFORMATION PLOT
##########################
jpeg(paste("Figures/Analysis1/EDU_TRANSF_PERF_", "Nsize_", sample_size, "_Noise_", 
           Noise_level, "_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".jpeg", sep=""), res=100, width=1400, height=600)

blue = rgb(23/255, 107/255, 239/255)
yellow = rgb(247/255, 181/255, 41/255)

par(mfrow=c(1,2), mar = c(6.1, 4.1, 4.1, 2.1))

barplot(EDU_TRANSF_CAP_F1, col=yellow, main="Capitani 1987 - v2\n Education transformation", xlab="", ylab="F1", ylim=c(0,1), las=2) 
barplot(EDU_TRANSF_ARC_F1, col=blue, main="Arcara 2024 - v3\n Education transformation", xlab="", ylab="F1", ylim=c(0,1), las=2) 

dev.off()
