# This script generate all plots from the objects created in STEP3a.
# These figures are reported partly in the Manuscript and partly in the Supplementary Materials.

# Arcara G. (2024) Improving ES. 

rm(list=ls())

Arcara_ver_values = c("v3")
Capitani_ver_values = c("v2")

input_folder="Analysis4/"

dir.create(paste("Figures/", input_folder, sep=""))

sample_demo_fun_values = c("sample.demo.cond")

deps = c("TRAIN_MSE", "TRAIN_PRECISION", "TRAIN_SENSITIVITY", "TRAIN_SPECIFICITY", "TRAIN_F1", 
         "TEST_PRECISION", "TEST_SENSITIVITY", "TEST_SPECIFICITY", "TEST_F1", "TRAIN_AGE_TRANSF_CORRESP", "TRAIN_EDU_TRANSF_CORRESP"
)


## select variables

for (dep in deps){
  for (sample_demo_fun in sample_demo_fun_values){
    for (Arcara_ver in Arcara_ver_values){
      for (Capitani_ver in Capitani_ver_values){
        
        ### load file
        load(paste("Results/", input_folder, "Summary_A", Arcara_ver, "_C", Capitani_ver, ".RData", sep=""))
        
        
        #dep = "TRAIN_SENSITIVITY"
        #sample_demo_fun = "sample.demo.cond"
        curr_res = res[res$sample.demo.fun == sample_demo_fun, ]
        
        #yrange = range(curr_res[, dep])
        yrange = c(min(curr_res[, dep]), 1)
        if (dep=="TRAIN_MSE"){
          yrange = range(curr_res[, dep])
        }
        xrange = range(curr_res[, "n.subj.train"])
        
        arcara = curr_res[curr_res$Method=="Arcara", ]
        capitani = curr_res[curr_res$Method=="Capitani", ]
        null_model = curr_res[curr_res$Method=="NULL", ]
        
        blue = rgb(23/255, 107/255, 239/255)
        yellow = rgb(247/255, 181/255, 41/255)
        
        jpeg(file=paste("Figures/", input_folder, "Performance_A", Arcara_ver,"_C", Capitani_ver, "_", sample_demo_fun, "_", dep, ".jpeg", sep=""), width=1800, height=800, res = 200)
        par(mfrow=c(1,3), mar=c(5.1, 4.1, 4.1+3, 2.1))
        plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
        curr_noise = 0.5
        pt_size = 1
        mtext(paste("True Adj Score SD = ", curr_noise, sep=""), side=3, line=1, cex=0.8)
        
        lines(arcara[arcara$P_score.sd==curr_noise, "n.subj.train"], arcara[arcara$P_score.sd==curr_noise, dep])
        points(arcara[arcara$P_score.sd==curr_noise, "n.subj.train"], arcara[arcara$P_score.sd==curr_noise, dep], pch=22, col=blue, bg = blue, cex = pt_size)
        
        curr_noise = 0.5
        pt_size = 1
        lines(capitani[capitani$P_score.sd==curr_noise, "n.subj.train"], capitani[capitani$P_score.sd==curr_noise, dep])
        points(capitani[capitani$P_score.sd==curr_noise, "n.subj.train"], capitani[capitani$P_score.sd==curr_noise, dep], pch=22, col=yellow, bg = yellow, cex = pt_size)
        
        curr_noise = 0.5
        pt_size = 1
        lines(null_model[null_model$P_score.sd==curr_noise, "n.subj.train"]+10, null_model[null_model$P_score.sd==curr_noise, dep])
        points(null_model[null_model$P_score.sd==curr_noise, "n.subj.train"]+10, null_model[null_model$P_score.sd==curr_noise, dep], pch=22, col="gray", bg = "gray", cex = pt_size)
        
        
        
        plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
        curr_noise = 1
        pt_size = 2
        mtext(paste("True Adj Score SD = ", curr_noise, sep=""), side=3, line=1, cex=0.8)
        
        lines(arcara[arcara$P_score.sd==curr_noise, "n.subj.train"]+10, arcara[arcara$P_score.sd==curr_noise, dep])
        points(arcara[arcara$P_score.sd==curr_noise, "n.subj.train"]+10, arcara[arcara$P_score.sd==curr_noise, dep], pch=22, col=blue, bg = blue, cex = pt_size)
        
        curr_noise = 1
        pt_size = 2
        lines(capitani[capitani$P_score.sd==curr_noise, "n.subj.train"]+10, capitani[capitani$P_score.sd==curr_noise, dep])
        points(capitani[capitani$P_score.sd==curr_noise, "n.subj.train"]+10, capitani[capitani$P_score.sd==curr_noise, dep], pch=22, col=yellow, bg = yellow, cex = pt_size)
        
        curr_noise = 1
        pt_size = 2
        lines(null_model[null_model$P_score.sd==curr_noise, "n.subj.train"]+10, null_model[null_model$P_score.sd==curr_noise, dep])
        points(null_model[null_model$P_score.sd==curr_noise, "n.subj.train"]+10, null_model[null_model$P_score.sd==curr_noise, dep], pch=22, col="gray", bg = "gray", cex = pt_size)
        
        
        plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
        curr_noise = 1.5
        pt_size = 3
        mtext(paste("True Adj Score SD = ", curr_noise, sep=""), side=3, line=1, cex=0.8)
        
        lines(arcara[arcara$P_score.sd==curr_noise, "n.subj.train"]+20, arcara[arcara$P_score.sd==curr_noise, dep])
        points(arcara[arcara$P_score.sd==curr_noise, "n.subj.train"]+20, arcara[arcara$P_score.sd==curr_noise, dep], pch=22, col=blue, bg = blue, cex = pt_size)
        curr_noise = 1.5
        pt_size = 3
        lines(capitani[capitani$P_score.sd==curr_noise, "n.subj.train"]+20, capitani[capitani$P_score.sd==curr_noise, dep])
        points(capitani[capitani$P_score.sd==curr_noise, "n.subj.train"]+20, capitani[capitani$P_score.sd==curr_noise, dep], pch=22, col=yellow, bg = yellow, cex = pt_size)
        
        curr_noise = 1.5
        pt_size = 3
        lines(null_model[null_model$P_score.sd==curr_noise, "n.subj.train"]+10, null_model[null_model$P_score.sd==curr_noise, dep])
        points(null_model[null_model$P_score.sd==curr_noise, "n.subj.train"]+10, null_model[null_model$P_score.sd==curr_noise, dep], pch=22, col="gray", bg ="gray", cex = pt_size)
        
        
        par(new=TRUE)
        par(mfrow=c(1,1), oma=c(0.1,0.1,0.1,0.1), mar=c(0,0,0,0))
        plot(0, type="n", xlab="", ylab="", axes=F, xlim=c(-1, 1), ylim=c(-1,1))
        text(0, 0.95, labels=gsub("_", " ", dep), cex=1.5, font=2) # remove underscore from dep names
        text(0, 0.80, labels=paste("Arcara: ", Arcara_ver, " - Capitani: ", Capitani_ver, " ", sample_demo_fun, sep=""), cex=1, font=1)
        legend("topright", pch=c(22, 22), legend=c("Capitani", "Arcara", "Null"), col=c(yellow, blue, "gray"), pt.bg = c(yellow, blue, "gray"), cex=0.7)
        
        dev.off()
        
      }
    }
  }
}