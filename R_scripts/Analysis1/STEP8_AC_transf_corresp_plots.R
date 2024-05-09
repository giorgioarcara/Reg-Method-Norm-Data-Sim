# This script generate all plots from the objects created in STEP3a.
# These figures are reported partly in the Manuscript and partly in the Supplementary Materials.

# curr_res G. (2024) Improving ES. 

rm(list=ls())

Arcara_ver_values = c("v3")
Capitani_ver_values = c("v2")

input_folder="Analysis1/"

dir.create(paste("Figures/", input_folder, sep=""))

sample_demo_fun_values = c("sample.demo.unif", "sample.demo.cond")

deps = c("AGE_TRANSF_CORRESP", "EDU_TRANSF_CORRESP", "AGEEDU_TRANSF_CORRESP")


## select variables

for (dep in deps){
  for (sample_demo_fun in sample_demo_fun_values){
    for (Arcara_ver in Arcara_ver_values){
      for (Capitani_ver in Capitani_ver_values){
        
        ### load file
        load(paste("Results/", input_folder, "TransfCorresp_A", Arcara_ver, "_C", Capitani_ver, ".RData", sep=""))
        
        curr_res = res[res$sample.demo.fun == sample_demo_fun, ]

        my_col = rgb(113/255, 137/255, 160/255) # this color result from blending (with 9 steps ) the color chosen for 
        # Arcara and Capitani.
        
        #yrange = range(curr_res[, dep])
        yrange = c(min(curr_res[, dep]), 1)
        xrange = range(curr_res[, "n.subj.train"])

        jpeg(file=paste("Figures/", input_folder, "Transf_Corresp_A", Arcara_ver,"_C", Capitani_ver, "_", sample_demo_fun, "_", dep, ".jpeg", sep=""), width=1800, height=600, res = 180)
        par(mfrow=c(1,3))
        plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
        
        curr_noise = 0.5
        pt_size = 1
        lines(curr_res[curr_res$P_score.sd==curr_noise, "n.subj.train"], curr_res[curr_res$P_score.sd==curr_noise, dep])
        points(curr_res[curr_res$P_score.sd==curr_noise, "n.subj.train"], curr_res[curr_res$P_score.sd==curr_noise, dep], pch=22, col=my_col, bg = my_col, cex = pt_size)
        
        plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
        curr_noise = 1
        pt_size = 2
        lines(curr_res[curr_res$P_score.sd==curr_noise, "n.subj.train"]+10, curr_res[curr_res$P_score.sd==curr_noise, dep])
        points(curr_res[curr_res$P_score.sd==curr_noise, "n.subj.train"]+10, curr_res[curr_res$P_score.sd==curr_noise, dep], pch=22, col=my_col, bg = my_col, cex = pt_size)
        
        
        plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
        curr_noise = 1.5
        pt_size = 3
        lines(curr_res[curr_res$P_score.sd==curr_noise, "n.subj.train"]+20, curr_res[curr_res$P_score.sd==curr_noise, dep])
        points(curr_res[curr_res$P_score.sd==curr_noise, "n.subj.train"]+20, curr_res[curr_res$P_score.sd==curr_noise, dep], pch=22, col=my_col, bg = my_col, cex = pt_size)
       
        
        par(new=TRUE)
        par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0))
        plot(0, type="n", xlab="", ylab="", axes=F, xlim=c(-1, 1), ylim=c(-1,1))
        text(0, 0.95, labels=gsub("_", " ", dep), cex=1.5, font=2) # remove underscore from dep names
        text(0, 0.80, labels=paste("Arcara: ", Arcara_ver, " - Capitani: ", Capitani_ver, " ", sample_demo_fun, sep=""), cex=1, font=1)
        #legend("topright", pch=c(22, 22), legend=c("Capitani", "curr_res"), col=c(yellow, my_col), pt.bg = c(yellow, my_col))
        
        dev.off()
        
      }
    }
  }
}
