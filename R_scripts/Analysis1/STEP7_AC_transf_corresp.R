# this script is used to generate the figures that show the correspondence between the transformation guessed by Capitani (1987) and Arcara (2024) Methods.
# These figures are reported in the Supplementary Materials.

# Arcara G. (2024) Improving ES. 

rm(list=ls())

input_folder="Analysis1/"

## create a major loop for every combination of CAP and ARC method and give an output object for each combination
# (this makes sense currently, because they are coupled)

Capitani_ver_values = c("v2")
Arcara_ver_values = c("v3")
# the possible values of number of subjects on which normative data are built
sample_size_values = c(100, 300, 500, 700) # Number of participant of normative Data
noise_values = c(0.5, 1, 1.5) 
sample_demo_fun_values = c("sample.demo.cond", "sample.demo.unif")
tag=""

res_size = length(sample_size_values)*length(noise_values)*length(sample_demo_fun_values)
res = data.frame(NULL)

k=1

for (iCAP in 1:length(Capitani_ver_values)){
  Capitani_ver = Capitani_ver_values[iCAP]
  for (iARC in 1:length(Arcara_ver_values)){
    Arcara_ver = Arcara_ver_values[iARC]
    for (sample_size in sample_size_values){
      n.subj.train = sample_size # use other name for consistency
      for (noise_level in noise_values){
        P_score.sd = noise_level # use the other name for consistency-+
        for (sample_demo_fun in sample_demo_fun_values){
          
          
          load(paste("Results/", input_folder, "Sim_Nsize_", n.subj.train, "_Noise_", P_score.sd,"_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".RData", sep=""))
          ### Un file per ogni combinazione
          CAP_TRANSF_MAT = matrix(unlist(TRAIN_CAP_TRANSF), ncol=2, byrow=T)
          ARC_TRANSF_MAT = matrix(unlist(TRAIN_ARC_TRANSF), ncol=2, byrow=T)
          
          AGE_TRANSF_CORRESP = sum(CAP_TRANSF_MAT[,1] == ARC_TRANSF_MAT[,1])/length(TRAIN_CAP_TRANSF)
          EDU_TRANSF_CORRESP = sum(CAP_TRANSF_MAT[,2] == ARC_TRANSF_MAT[,2])/length(TRAIN_CAP_TRANSF)
          AGEEDU_TRANSF_CORRESP = sum((CAP_TRANSF_MAT[,2] == ARC_TRANSF_MAT[,2])&(CAP_TRANSF_MAT[,1] == ARC_TRANSF_MAT[,1]))/length(TRAIN_CAP_TRANSF)
          
          res[k, "n.sim"] = sim.params$n.sim
          res[k, "n.subj.train"] = sim.params$n.subj.train
          res[k, "n.subj.test"] = sim.params$n.subj.test
          res[k, "P_score.sd"] = sim.params$P_score.sd
          res[k, "sample.demo.fun"]=sample_demo_fun
          res[k, "AGE_TRANSF_CORRESP"] = AGE_TRANSF_CORRESP
          res[k, "EDU_TRANSF_CORRESP"] = EDU_TRANSF_CORRESP
          res[k, "AGEEDU_TRANSF_CORRESP"] = AGEEDU_TRANSF_CORRESP
          res[k, "Capitani_Version"]= Capitani_ver
          res[k, "Arcara_Version"] = Arcara_ver
          k = k+1
        }
      }
    } 
    save(res, file=paste("Results/", input_folder, "TransfCorresp_A", Arcara_ver, "_C", Capitani_ver, ".RData", sep="") )
  }
}
