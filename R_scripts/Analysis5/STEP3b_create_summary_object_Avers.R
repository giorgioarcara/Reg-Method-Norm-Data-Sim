# This script generates some R objects used to compare Arcara and Arcara methods. The intermediate step is necessary as the time to assemble the object necessary for figures is quite long.
# Launch the script STEP4b to actually generate the Figures from the objects that result as output from this step.


# Arcara G. (2024) Improving ES. 

rm(list=ls())

## create a major loop for every combination of ARC and ARC method and give an output object for each combination
# (this makes sense currently, because they are coupled)
Arcara_ver_comb = combn(c("v1", "v2", "v3"), 2)

Arcara1_ver_values = Arcara_ver_comb[1,]
Arcara2_ver_values = Arcara_ver_comb[2,]

input_folder="Analysis5/"

dir.create(paste("Figures/", input_folder, sep=""))


for (iARC in 1:length(Arcara1_ver_values)){
  Arcara1_ver = Arcara1_ver_values[iARC]
  Arcara2_ver = Arcara2_ver_values[iARC]
  
  
  # the possible values of number of subjects on which normative data are built
  sample_size_values = c(100, 300, 500, 700) # Number of participant of normative Data
  noise_values = c(0.5, 1, 1.5) 
  sample_demo_fun_values = c("sample.demo.cond", "sample.demo.unif")
  tag=""
  
  ## Create empty results
  res_size = length(sample_size_values)*length(noise_values)*length(sample_demo_fun_values)
  #res = data.frame(n.sim=rep(NA, res_size*2), n.subj.train=rep(NA, res_size*2), n.subj.test=rep(NA, res_size*2), P_score.sd=rep(NA, res_size*2), sample.demo.fun=rep(NA, res_size*2),  Method = rep(NA, res_size*2),
  #                 TRAIN_FP = rep(NA, res_size*2), TRAIN_FN = rep(NA, res_size*2), TRAIN_TP = rep(NA, res_size*2), TRAIN_TN = rep(NA, res_size*2), TRAIN_SENSITIVITY = rep(NA, res_size*2), TRAIN_SPECIFICITY = rep(NA, res_size*2), TRAIN_PRECISION = rep(NA, res_size*2), TRAIN_F1 = rep(NA, res_size*2), 
  #                 TEST_FP = rep(NA, res_size*2), TEST_FN = rep(NA, res_size*2), TEST_TP = rep(NA, res_size*2), TEST_TN = rep(NA, res_size*2), TEST_SENSITIVITY = rep(NA, res_size*2), TEST_SPECIFICITY = rep(NA, res_size*2), TEST_PRECISION = rep(NA, res_size*2), TEST_F1 = rep(NA, res_size*2), TEST_NBELOW = rep(NA, res_size*2)
  #)
  
  res = data.frame(NULL)
  
  iL = 1
  ### Create loop list
  for (sample_size in sample_size_values){
    n.subj.train = sample_size # use other name for consistency
    for (noise_level in noise_values){
      P_score.sd = noise_level # use the other name for consistency-+
      for (sample_demo_fun in sample_demo_fun_values){
        
        
        load(paste("Results/", input_folder, "Sim_Nsize_", n.subj.train, "_Noise_", P_score.sd,"_A", Arcara1_ver, "_Cv2_", sample_demo_fun, tag, ".RData", sep=""))
        ### one file for each combination
        
        
        res[c(iL, iL+1), "n.sim"] = sim.params$n.sim
        res[c(iL, iL+1), "n.subj.train"] = sim.params$n.subj.train
        res[c(iL, iL+1), "n.subj.test"] = sim.params$n.subj.test
        res[c(iL, iL+1), "P_score.sd"] = sim.params$P_score.sd
        res[c(iL, iL+1), "sample.demo.fun"]=sample_demo_fun
        
        res[iL, "Method"] = "Arcara"
        res[iL, "Version"] = Arcara1_ver
        res[iL, "TRAIN_MSE"] = mean(TRAIN_ARC_RESID_ERR)
        res[iL, "TRAIN_FP"] = mean(TRAIN_ARC_FP)
        res[iL, "TRAIN_FN"] = mean(TRAIN_ARC_FN)
        res[iL, "TRAIN_TP"] = mean(TRAIN_ARC_TP)
        res[iL, "TRAIN_TN"] = mean(TRAIN_ARC_TN)
        res[iL, "TRAIN_PRECISION"] = mean(TRAIN_ARC_PRECISION, na.rm=T)
        res[iL, "TRAIN_SENSITIVITY"] = mean(TRAIN_ARC_SENSITIVITY, na.rm=T)
        res[iL, "TRAIN_SPECIFICITY"] = mean(TRAIN_ARC_SPECIFICITY, na.rm=T)
        res[iL, "TRAIN_F1"] = mean(TRAIN_ARC_F1, na.rm=T)
        res[iL, "TEST_FP"] = mean(TEST_ARC_FP)
        res[iL, "TEST_FN"] = mean(TEST_ARC_FN)
        res[iL, "TEST_TP"] = mean(TEST_ARC_TP)
        res[iL, "TEST_TN"] = mean(TEST_ARC_TN)
        res[iL, "TEST_PRECISION"] = mean(TEST_ARC_PRECISION, na.rm=T)
        res[iL, "TEST_SENSITIVITY"] = mean(TEST_ARC_SENSITIVITY, na.rm=T)
        res[iL, "TEST_SPECIFICITY"] = mean(TEST_ARC_SPECIFICITY, na.rm=T)
        res[iL, "TEST_F1"] = mean(TEST_ARC_F1, na.rm=T)
        res[iL, "TEST_NBELOW"] = mean(TEST_ARC_NBELOW)
        res[iL, "TEST_PRECISION"] = mean(TEST_ARC_PRECISION, na.rm=T)
        res[iL, "TEST_SENSITIVITY"] = mean(TEST_ARC_SENSITIVITY, na.rm=T)
        res[iL, "TEST_SPECIFICITY"] = mean(TEST_ARC_SPECIFICITY, na.rm=T)
        res[iL, "TEST_F1"] = mean(TEST_ARC_F1, na.rm=T)
        
        res[iL, "TRAIN_PRECISION_NASUM"] = sum(is.na(TRAIN_ARC_PRECISION))
        res[iL, "TRAIN_SENSITIVITY_NASUM"] = sum(is.na(TRAIN_ARC_SENSITIVITY))
        res[iL, "TRAIN_SPECIFICITY_NASUM"] = sum(is.na(TRAIN_ARC_SPECIFICITY))
        res[iL, "TRAIN_F1_NASUM"] = sum(is.na(TRAIN_ARC_F1))
        res[iL, "TEST_PRECISION_NASUM"] = sum(is.na(TEST_ARC_PRECISION))
        res[iL, "TEST_SENSITIVITY_NASUM"] = sum(is.na(TEST_ARC_SENSITIVITY))
        res[iL, "TEST_SPECIFICITY_NASUM"] = sum(is.na(TEST_ARC_SPECIFICITY))
        res[iL, "TEST_F1_NASUM"] = sum(is.na(TEST_ARC_F1))
        
        GT_TRANSF_MAT = matrix(unlist(TRAIN_GT_TRANSF), ncol=2, byrow=T)
        ARC_TRANSF_MAT = matrix(unlist(TRAIN_ARC_TRANSF), ncol=2, byrow=T)
        
        res[iL, "TRAIN_AGE_TRANSF_CORRESP"]= sum(ARC_TRANSF_MAT[,1] == GT_TRANSF_MAT[,1])/length(TRAIN_GT_TRANSF)
        res[iL, "TRAIN_EDU_TRANSF_CORRESP"]= sum(ARC_TRANSF_MAT[,2] == GT_TRANSF_MAT[,2])/length(TRAIN_GT_TRANSF)
        
        
        load(paste("Results/", input_folder, "Sim_Nsize_", n.subj.train, "_Noise_", P_score.sd,"_A", Arcara2_ver, "_Cv2_", sample_demo_fun, tag, ".RData", sep=""))
        
        
        res[iL+1, "Method"] = "Arcara"
        res[iL+1, "Version"] = Arcara2_ver
        res[iL+1, "TRAIN_MSE"] = mean(TRAIN_ARC_RESID_ERR)
        res[iL+1, "TRAIN_FP"] = mean(TRAIN_ARC_FP)
        res[iL+1, "TRAIN_FN"] = mean(TRAIN_ARC_FN)
        res[iL+1, "TRAIN_TP"] = mean(TRAIN_ARC_TP)
        res[iL+1, "TRAIN_TN"] = mean(TRAIN_ARC_TN)
        res[iL+1, "TRAIN_PRECISION"] = mean(TRAIN_ARC_PRECISION, na.rm=T)
        res[iL+1, "TRAIN_SENSITIVITY"] = mean(TRAIN_ARC_SENSITIVITY, na.rm=T)
        res[iL+1, "TRAIN_SPECIFICITY"] = mean(TRAIN_ARC_SPECIFICITY, na.rm=T)
        res[iL+1, "TRAIN_F1"] = mean(TRAIN_ARC_F1, na.rm=T)
        res[iL+1, "TEST_FP"] = mean(TEST_ARC_FP)
        res[iL+1, "TEST_FN"] = mean(TEST_ARC_FN)
        res[iL+1, "TEST_TP"] = mean(TEST_ARC_TP)
        res[iL+1, "TEST_TN"] = mean(TEST_ARC_TN)
        res[iL+1, "TEST_PRECISION"] = mean(TEST_ARC_PRECISION, na.rm=TRUE)
        res[iL+1, "TEST_SENSITIVITY"] = mean(TEST_ARC_SENSITIVITY, na.rm=TRUE)
        res[iL+1, "TEST_SPECIFICITY"] = mean(TEST_ARC_SPECIFICITY, na.rm=TRUE)
        res[iL+1, "TEST_F1"] = mean(TEST_ARC_F1, na.rm=TRUE)
        res[iL+1, "TEST_NBELOW"] = mean(TEST_ARC_NBELOW)
        
        res[iL+1, "TRAIN_PRECISION_NASUM"] = sum(is.na(TRAIN_ARC_PRECISION))
        res[iL+1, "TRAIN_SENSITIVITY_NASUM"] = sum(is.na(TRAIN_ARC_SENSITIVITY))
        res[iL+1, "TRAIN_SPECIFICITY_NASUM"] = sum(is.na(TRAIN_ARC_SPECIFICITY))
        res[iL+1, "TRAIN_F1_NASUM"] = sum(is.na(TRAIN_ARC_F1))
        res[iL+1, "TEST_PRECISION_NASUM"] = sum(is.na(TEST_ARC_PRECISION))
        res[iL+1, "TEST_SENSITIVITY_NASUM"] = sum(is.na(TEST_ARC_SENSITIVITY))
        res[iL+1, "TEST_SPECIFICITY_NASUM"] = sum(is.na(TEST_ARC_SPECIFICITY))
        res[iL+1, "TEST_F1_NASUM"] = sum(is.na(TEST_ARC_F1))
        
        ARC_TRANSF_MAT = matrix(unlist(TRAIN_ARC_TRANSF), ncol=2, byrow=T) # NOTE (GT_TRANSF_MAT IS CREATED ABOVE )
        
        
        res[iL+1, "TRAIN_AGE_TRANSF_CORRESP"]= sum(ARC_TRANSF_MAT[,1] == GT_TRANSF_MAT[,1])/length(TRAIN_GT_TRANSF)
        res[iL+1, "TRAIN_EDU_TRANSF_CORRESP"]= sum(ARC_TRANSF_MAT[,2] == GT_TRANSF_MAT[,2])/length(TRAIN_GT_TRANSF)
        
        
        
        iL = iL+2
      }
    }
  }
  save(res, file=paste("Results/", input_folder, "Summary_A", Arcara1_ver, "_A", Arcara2_ver, ".RData", sep="") )
} 

