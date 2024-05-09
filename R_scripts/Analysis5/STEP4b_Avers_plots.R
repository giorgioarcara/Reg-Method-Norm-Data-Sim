# This script generate all plots from the objects created in STEP3b.
# These figures are reported in the Supplementary Materials.

# Arcara G. (2024) Improving ES. 

Arcara_ver_comb = combn(c("v1", "v2", "v3"), 2)

Arcara1_ver_values = Arcara_ver_comb[1,]
Arcara2_ver_values = Arcara_ver_comb[2,]


input_folder="Analysis5/"

dir.create(paste("Figures/", input_folder, sep=""))


sample_demo_fun_values = c("sample.demo.unif", "sample.demo.cond")

deps = c("TRAIN_MSE", "TRAIN_PRECISION", "TRAIN_SENSITIVITY", "TRAIN_SPECIFICITY", "TRAIN_F1", 
         "TEST_PRECISION", "TEST_SENSITIVITY", "TEST_SPECIFICITY", "TEST_F1", "TRAIN_AGE_TRANSF_CORRESP", "TRAIN_EDU_TRANSF_CORRESP"
)
## select variables

for (dep in deps){
  for (sample_demo_fun in sample_demo_fun_values){
    for (iARC in 1:length(Arcara1_ver_values)){
      
      Arcara1_ver = Arcara1_ver_values[iARC]
      Arcara2_ver = Arcara2_ver_values[iARC]
      
      ### load file
      load(paste("Results/", input_folder, "Summary_A", Arcara1_ver, "_A", Arcara2_ver, ".RData", sep=""))
      
      
      #dep = "TRAIN_SENSITIVITY"
      #sample_demo_fun = "sample.demo.cond"
      curr_res = res[res$sample.demo.fun == sample_demo_fun, ]
      
      #yrange = range(curr_res[, dep])
      yrange = c(min(curr_res[, dep]), 1)
      if (dep=="TRAIN_MSE"){
        yrange = range(curr_res[, dep])
      }
      xrange = range(curr_res[, "n.subj.train"])
      
      Arcara1 = curr_res[curr_res$Method=="Arcara" & curr_res$Version == Arcara1_ver, ]
      Arcara2 = curr_res[curr_res$Method=="Arcara" & curr_res$Version == Arcara2_ver, ]
      blue1 = rgb(23/255, 107/255, 239/255)
      blue2 = rgb(107/255, 154/255, 235 /255)
      
      jpeg(file=paste("Figures/", input_folder, "Performance_A", Arcara1_ver,"_A", Arcara2_ver, "_", sample_demo_fun, "_", dep, ".jpeg", sep=""),  width=1800, height=800,res = 200)
      par(mfrow=c(1,3), mar=c(5.1, 4.1, 4.1+3, 2.1))
      plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
      curr_noise = 0.5
      pt_size = 1

      mtext(paste("True Adj Score SD = ", curr_noise, sep=""), side=3, line=1, cex=0.8)
      
      lines(Arcara1[Arcara1$P_score.sd==curr_noise, "n.subj.train"], Arcara1[Arcara1$P_score.sd==curr_noise, dep])
      points(Arcara1[Arcara1$P_score.sd==curr_noise, "n.subj.train"], Arcara1[Arcara1$P_score.sd==curr_noise, dep], pch=22, col=blue1, bg = blue1, cex = pt_size)
      curr_noise = 0.5
      pt_size = 1
      lines(Arcara2[Arcara2$P_score.sd==curr_noise, "n.subj.train"], Arcara2[Arcara2$P_score.sd==curr_noise, dep])
      points(Arcara2[Arcara2$P_score.sd==curr_noise, "n.subj.train"], Arcara2[Arcara2$P_score.sd==curr_noise, dep], pch=22, col=blue2, bg = blue2, cex = pt_size)
      
      plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
      curr_noise = 1
      pt_size = 2

      mtext(paste("True Adj Score SD = ", curr_noise, sep=""), side=3, line=1, cex=0.8)
      
      lines(Arcara1[Arcara1$P_score.sd==curr_noise, "n.subj.train"], Arcara1[Arcara1$P_score.sd==curr_noise, dep])
      points(Arcara1[Arcara1$P_score.sd==curr_noise, "n.subj.train"], Arcara1[Arcara1$P_score.sd==curr_noise, dep], pch=22, col=blue1, bg = blue1, cex = pt_size)
      curr_noise = 1
      pt_size = 2

      lines(Arcara2[Arcara2$P_score.sd==curr_noise, "n.subj.train"], Arcara2[Arcara2$P_score.sd==curr_noise, dep])
      points(Arcara2[Arcara2$P_score.sd==curr_noise, "n.subj.train"], Arcara2[Arcara2$P_score.sd==curr_noise, dep], pch=22, col=blue2, bg = blue2, cex = pt_size)
      
      plot(NA, xlim = c(xrange), ylim=yrange, ylab=gsub("_", " ", dep), xlab="Sample Size", main="")
      curr_noise = 1.5
      pt_size = 3

      mtext(paste("True Adj Score SD = ", curr_noise, sep=""), side=3, line=1, cex=0.8)
      lines(Arcara1[Arcara1$P_score.sd==curr_noise, "n.subj.train"]+10, Arcara1[Arcara1$P_score.sd==curr_noise, dep])
      points(Arcara1[Arcara1$P_score.sd==curr_noise, "n.subj.train"]+10, Arcara1[Arcara1$P_score.sd==curr_noise, dep], pch=22, col=blue1, bg = blue1, cex = pt_size)
      curr_noise = 1.5
      pt_size = 3

      lines(Arcara2[Arcara2$P_score.sd==curr_noise, "n.subj.train"]+10, Arcara2[Arcara2$P_score.sd==curr_noise, dep])
      points(Arcara2[Arcara2$P_score.sd==curr_noise, "n.subj.train"]+10, Arcara2[Arcara2$P_score.sd==curr_noise, dep], pch=22, col=blue2, bg = blue2, cex = pt_size)
      
      par(new=TRUE)
      par(mfrow=c(1,1), oma=c(0.1,0.1,0.1,0.1), mar=c(0,0,0,0))
      plot(0, type="n", xlab="", ylab="", axes=F, xlim=c(-1, 1), ylim=c(-1,1))
      text(0, 0.95, labels=gsub("_", " ", dep), cex=2, font=2)
      text(0, 0.80, labels=paste("Arcara: ", Arcara1_ver, " - Arcara: ", Arcara2_ver, " ", sample_demo_fun, sep=""), cex=1, font=1)
      legend("topright", pch=c(22, 22), legend=c(Arcara1_ver, Arcara2_ver), col=c(blue2, blue1), pt.bg = c(blue2, blue1), cex=0.7)
      
      dev.off()
      
    }
    
  }
}
