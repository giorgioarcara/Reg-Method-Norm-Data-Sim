### MAIN SCRIPT
# this script is used to set the main parameters used for an additional simulation that uses real datasets to estimate empirical distribution of demographic variable.
# launch this script and then launch STEP3, STEP4, STEP5, and so on, to replicate the results presented in the article.


# Arcara G. (2024) Improving ES. 

rm(list=ls())

# the possible values of number of subjects on which normative data are built
sample_size_values = c(100, 300, 500, 700) # Number of participant of normative Data
noise_values = c(0.5, 1, 1.5) # the noise values (i.e., P_score sd values)
sample_demo_values = c("MOCA", "GEMS", "TELEGEMS") # the reference dataset to generate the distribution of normative data.
Capitani_ver_values = c("v2")
Arcara_ver_values = c("v3")

output_folder = "Analysis2/"

dir.create(paste("Results/", output_folder, sep=""))

## create lists of combinations before running the script (useful if it kills to restart)
loop_list = list(NULL)
length(loop_list) = length(sample_size_values)*length(noise_values)*length(Capitani_ver_values)*length(Arcara_ver_values)*length(sample_demo_values)


k = 1
### Create loop list
for (sample_size in sample_size_values){
  for (noise_level in noise_values){
    for (Arcara_ver in Arcara_ver_values){
      for (Capitani_ver in Capitani_ver_values){
        for (sample_demo_val in sample_demo_values){
          
          loop_list[[k]] = list(sample_size = sample_size, noise_level=noise_level, 
                             Arcara_ver=Arcara_ver, Capitani_ver=Capitani_ver, sample_demo_val=sample_demo_val)
          
          print(k)
          k=k+1
          
        }
      }
    }
  }
}


n_start=1

for (iLoop in n_start:length(loop_list)){
  
  curr_loop = loop_list[[iLoop]]
  
  assign("n.subj.train", curr_loop$sample_size, env=.GlobalEnv)
  assign("P_score.sd", curr_loop$noise_level, env=.GlobalEnv)
  assign("sample_demo_val", curr_loop$sample_demo_val, env=.GlobalEnv )
  assign("Arcara_ver", curr_loop$Arcara_ver, env=.GlobalEnv)
  assign("Capitani_ver", curr_loop$Capitani_ver, env=.GlobalEnv)
  
  print(iLoop)

  source("R_scripts/Analysis2/STEP2_simulation.R", local=F, verbose = F)
  
  
}
