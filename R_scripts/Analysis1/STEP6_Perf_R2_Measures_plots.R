# This script is used to generate the figures that show the performance of methods and the relationship with R^2 values.
# These figures are reported in the Supplementary Materials.

# Arcara G. (2024) Improving ES. 
rm(list=ls())


sample_size = 500
Noise_level = 1
Arcara_ver = "v3"
Capitani_ver = "v2"
sample_demo_fun = "sample.demo.cond"
tag=""

curr_file = paste("Results/Analysis1/Sim_Nsize_", sample_size, "_Noise_", Noise_level, "_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".RData", sep="")
load(curr_file)


jpeg(paste("Figures/Analysis1/TRAINR2_PERF_", "Nsize_", sample_size, "_Noise_", 
           Noise_level, "_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".jpeg", sep=""), res=120, width=1200, height=600)

blue = rgb(23/255, 107/255, 239/255)
yellow = rgb(247/255, 181/255, 41/255)

par(mfrow=c(1,2))
plot(TRAIN_CAP_RS, TEST_CAP_F1, pch=19, col=yellow, main="Capitani 1987 - v2", xlab="R squared", ylab="F1") 
plot(TRAIN_ARC_RS, TEST_ARC_F1, pch=19, col=blue, main="Arcara 2024 - v3", xlab="R squared", ylab="F1") 

dev.off()

Noise_level_values = c(0.5,1,1.5)
Arcara_ver = "v3"
Capitani_ver = "v2"
sample_demo_fun = "sample.demo.cond"
sample_size_values = c(100,300,500,700)


blue_alpha = rgb(23/255, 107/255, 239/255, alpha = 0.3)
yellow_alpha = rgb(247/255, 181/255, 41/255, alpha=0.3)

n_rows = length(sample_size_values)
n_cols = length(Noise_level_values)

### Figure to assess methods distribution of R^2 across different sample size and True Adj Score SD.
jpeg(paste("Figures/Analysis1/TRAIN_R2_DISTR_", sample_demo_fun, tag, ".jpeg", sep=""), res=200, width=600*n_cols, height=600*n_rows)

par(mfrow=c(n_rows, n_cols), oma=c(0,3,2,0))

for(sample_size in sample_size_values){
  for (Noise_level in Noise_level_values){
    curr_file = paste("Results/Analysis1/Sim_Nsize_", sample_size, "_Noise_", Noise_level, "_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".RData", sep="")
    load(curr_file)
    a = hist(TRAIN_ARC_RS, col=blue_alpha, xlim=c(0,1), main="", xlab="")
    b = hist(TRAIN_CAP_RS, col=yellow_alpha, xlim=c(0,1), add=T, xlab="")
  }
}
par(new=TRUE)
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0))
plot(0, type="n", xlab="", ylab="", axes=F, xlim=c(-1, 1), ylim=c(-1,1))
text(0, 1, labels="True Adj Score SD", cex=1.5, font=2) 
text(-1, 0, labels="Sample Size", cex=1.5, font=2, srt=90) 

dev.off()

### Figure to assess Ground Truth Distribution of R^2
### Figure to assess methods distribution of R^2 across different sample size and True Adj Score SD.

jpeg(paste("Figures/Analysis1/GT_R2_DISTR_", sample_demo_fun, tag, ".jpeg", sep=""), res=200, width=600*n_cols, height=600*n_rows)

par(mfrow=c(n_rows, n_cols), oma=c(0,3,2,0))

for(sample_size in sample_size_values){
  for (Noise_level in Noise_level_values){
    curr_file = paste("Results/Analysis1/Sim_Nsize_", sample_size, "_Noise_", Noise_level, "_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".RData", sep="")
    load(curr_file)
    a = hist(TRAIN_GT_RS, col="gray", xlim=c(0,1), main="", xlab="")
  }
}
par(new=TRUE)
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0))
plot(0, type="n", xlab="", ylab="", axes=F, xlim=c(-1, 1), ylim=c(-1,1))
text(0, 1, labels="True Adj Score SD", cex=1.5, font=2) 
text(-1, 0, labels="Sample Size", cex=1.5, font=2, srt=90) 

dev.off()


