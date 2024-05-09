#rm(list=ls())
### this script launch the simulation with parameters you can either use this script from STEP1_loop.R
# Alternatively, you can set specific parameter value for a single combination by modifying the parameters in the lines below
# (see "SET SIMULATION PARAMETERS" paragraph)

# Arcara G. (2024) Improving ES. 

# for debug purposes or to run a simulation on a specific combination of parameter values, set the blow below to TRUE
########################
if (FALSE){
  
  n.subj.train = 100
  P_score.sd = 1
  sample_demo_fun = "sample.demo.cond"
  Arcara_ver="v3"
  Capitani_ver="v2"
  output_folder="CHECK/"
  dir.create(paste("Results/", output_folder, sep=""))
  
}
#######################

# LIBRARIES
library(car)

# SOURCE FUNCTIONS
source(paste("R_functions/adjscores_A2024_", Arcara_ver, ".R", sep=""))
source(paste("R_functions/adjscores_C1987_", Capitani_ver, ".R", sep=""))
source("R_functions/sim.norm.data.R")
source("R_functions/ES.R")
source("R_functions/tolLimits.obs.R")
source("R_functions/progress_bar.R")
source("R_functions/sample.demo.cond.R")
source("R_functions/sample.demo.unif.R")
source("R_functions/sample.coef.unif.R")
source("R_functions/sample.transf.R")
source("R_functions/formula_transf_text.R")
source("R_functions/transf_functions.R")
source("R_functions/model_text.R")
source("R_functions/model_transf_text.R")

#################################
## SET SIMULATION PARAMETERS ####
#################################



tag = "" # optional tag for the simulation (added before .RData)
n.sim = 1000 # number of simulations
#n.subj.train = 500 # number of subjects in each train set (i.e., the normative data)
n.subj.test = 1000 # number of subjects in each test set (i.e., the participant/patients tested)
# P_score.sd = 1 # defines the True Adj Score variability. It is also conceivable as the noise (in terms of error), with respect of prediction by demographic variables.

## coefficients parameters
c.range = list(c(18, 20), c(0, -4), c(0, 4), c(-1, 1))
c.steps = c(100,100,100,100)


age.include.transf = NULL
age.exclude.transf = c("poly2")
edu.include.transf = NULL
edu.exclude.transf = c("poly2") 

# define sampling type (uniform or conditioned)
sample.demo = eval(parse(text=sample_demo_fun, file=""))

sim.params = list(n.sim = n.sim, n.subj.train=n.subj.train, n.subj.test = n.subj.test,
                  P_score.sd = P_score.sd, c.range = c.range, c.steps = c.steps, 
                  transf=list(age.include.transf = age.include.transf, age.exclude.transf=age.exclude.transf, edu.include.transf=edu.include.transf, edu.exclude.transf=edu.exclude.transf, 
                              Arcara_ver = Arcara_ver, Capitani_ver = Capitani_ver))


## SET OUTPUT NAME
file_name = paste("Results/", output_folder, "Sim_Nsize_", n.subj.train, "_Noise_", P_score.sd,"_A", Arcara_ver, "_C", Capitani_ver, "_", sample_demo_fun, tag, ".RData", sep="")
print(file_name)

#################################
#################################
#### INITIALIZE OUTPUT OBJECTS
#################################
#################################
# note
# object with all Capital letters denotes objects that collect results of the simulation
# (i.e., typically one dimension is equal to n.sim)
# objects with Sentence case (only first word is Capital), denote objects created for that specific iS of the simulation

############################
## GROUND TRUTH ###########
############################
# ### TRAIN SET PERFORMANCE
TRAIN_GT_RESID = NULL

TRAIN_GT_TRANSF = list(NULL)
length(TRAIN_GT_TRANSF)=n.sim

TRAIN_GT_RS = NULL

TRAIN_GT_SCORE_RANGE = list(NULL)
length(TRAIN_GT_SCORE_RANGE)=n.sim

## TEST SET PERFORMANCE
TEST_GT_NBELOW = NULL

SIM_NORM_DATA_COR = NULL

#############
# CAPITANI #
#############
# TRAIN SET PERFORMANCE
TRAIN_CAP_RESID_ERR = NULL 
## calculate training set classification error
TRAIN_CAP_FP = NULL 
TRAIN_CAP_FN = NULL
TRAIN_CAP_TP = NULL 
TRAIN_CAP_TN = NULL

TRAIN_CAP_TRANSF = list(NULL)
length(TRAIN_CAP_TRANSF)=n.sim

TRAIN_CAP_RS = NULL

TRAIN_GT_MODEL_TEXT = NULL
TRAIN_CAP_MODEL_TEXT = NULL

# object containing the formula of the models
TRAIN_CAP_FORMULA = list(NULL)
length(TRAIN_CAP_FORMULA)=n.sim

## TEST SET PERFORMANCE
TEST_CAP_NBELOW = NULL
TEST_CAP_RESID_ERR = NULL
TEST_CAP_FP = NULL 
TEST_CAP_FN = NULL
TEST_CAP_TP = NULL 
TEST_CAP_TN = NULL

#############
# ARCARA #
#############
# TRAIN SET PERFORMANCE
TRAIN_ARC_RESID_ERR = NULL 
## calculate training set classification error
TRAIN_ARC_FP = NULL 
TRAIN_ARC_FN = NULL
TRAIN_ARC_TP = NULL 
TRAIN_ARC_TN = NULL

TRAIN_ARC_TRANSF = list(NULL)
length(TRAIN_ARC_TRANSF)=n.sim

TRAIN_ARC_RS = NULL

TRAIN_GT_MODEL_TEXT = NULL
TRAIN_ARC_MODEL_TEXT = NULL

# object containing the formula of the models
TRAIN_ARC_FORMULA = list(NULL)
length(TRAIN_ARC_FORMULA)=n.sim

## TEST SET PERFORMANCE
TEST_ARC_NBELOW = NULL
TEST_ARC_RESID_ERR = NULL
TEST_ARC_FP = NULL 
TEST_ARC_FN = NULL
TEST_ARC_TP = NULL 
TEST_ARC_TN = NULL

#iS=1
for (iS in 1:n.sim){
  
  ## CREATE TRAIN SET (NORMATIVE DATA)
  
  train.demo.data = sample.demo(n.subj.train)
  train.coefs = sample.coef.unif(c.range = c.range,
                                 c.steps=c.steps)
  
  train.age_transf = sample.transf(include_transf = age.include.transf, exclude_transf = age.exclude.transf) 
  train.edu_transf = sample.transf(include_transf = edu.include.transf, exclude_transf = edu.exclude.transf) 
  
  train.res = sim.norm.data(n = n.subj.train,
                            coefs = train.coefs,
                            P_score.sd = P_score.sd,
                            eff_mult = eff_mult,
                            age_values_o =  train.demo.data$age_values_o,
                            edu_values_o = train.demo.data$edu_values_o,
                            sex_values_o = train.demo.data$sex_values_o,
                            age_transf = train.age_transf,
                            edu_transf = train.edu_transf
  )
  
  
  train.dat = train.res[[1]]
  
  TRAIN_GT_MODEL_TEXT[[iS]]=train.res[[2]]
  
  SIM_NORM_DATA_COR[iS]=cor(train.dat$Age, train.dat$Edu)
  
  # small plots for check
  if (F){
    par(mfrow=c(1,2))
    plot(train.dat$Age, train.dat$Obs_score)
    plot(train.dat$Edu, train.dat$Obs_score)
  }
  
  TRAIN_GT_SCORE_RANGE[[iS]]=range(train.dat$Demo_score) # to check actual range, for debugging and checks
  TRAIN_GT_TRANSF[[iS]] = c(train.age_transf, train.edu_transf)
  
  #  1a) calculate ground truth below cut-off obs
  Train_Adj.GT= train.dat$P_score
  Train_ES.GT = ES(adjscores= train.dat$P_score)$Adjusted_Scores
  Train_Below.GT = train.dat$P_score<=Train_ES.GT[1]
  Train_Above.GT = train.dat$P_score > Train_ES.GT[1]
  
  #  calculate R squadred for the Ground Truth.
  TRAIN_GT_RS[iS] = 1 - sum(train.dat$P_score^2)/sum((train.dat$Obs_score-mean(train.dat$Obs_score))^2)
  TRAIN_GT_RESID[iS] = mean(Train_Adj.GT) # this is just for check: it is expected to be = 0.
  
  progress_bar(iS, n.sim, 10)
  
  
  ## CREATE TEST SET (NEW PARTICIPANTS)
  test.demo.data = sample.demo(n.subj.test)
  test.res = sim.norm.data(n = n.subj.test,
                           coefs = train.coefs,
                           P_score.sd = P_score.sd,
                           eff_mult = eff_mult,
                           age_values_o =  test.demo.data$age_values_o,
                           edu_values_o = test.demo.data$edu_values_o,
                           sex_values_o = test.demo.data$sex_values_o,
                           age_transf = train.age_transf,
                           edu_transf = train.edu_transf
  )
  
  
  test.dat = test.res[[1]]
  
  Test_Below.GT = test.dat$P_score <= Train_ES.GT[1]
  Test_Above.GT = test.dat$P_score > Train_ES.GT[1]
  
  # Ground truth performance
  Pred.GT =  with(test.dat, eval(parse(text=train.res[[2]])) )
  TEST_GT_NBELOW[iS] = sum ( (test.dat$Obs_score - Pred.GT) <=  Train_ES.GT[1], na.rm=T )
  
  
  #######################
  ### CAPITANI PERFORMANCE 
  #######################
  # compute adj score with Capitani method
  CAP.res = adjscores_C1987(df=train.dat, dep="Obs_score", 
                            age="Age", edu="Edu", sex="Sex",
                            dep.range = c(0, max(train.dat$Obs_score))
  )
  
  dat.adj.CAP = CAP.res$new.df
  TRAIN_CAP_TRANSF[[iS]] = CAP.res$transfs
  TRAIN_CAP_RS[iS]=summary(CAP.res$lm.model)$r.squared
  TRAIN_CAP_FORMULA[[iS]]=formula(CAP.res$lm.model)
  TRAIN_CAP_MODEL_TEXT[iS]=CAP.res$model_text
  
  # 2a) calculate Capitani's below cut-off obs
  Train_Adj.CAP = dat.adj.CAP$RESIDUALS
  Train_ES.CAP = ES(adjscores= dat.adj.CAP$RESIDUALS)$Adjusted_Scores
  Train_Below.CAP = dat.adj.CAP$RESIDUALS<=Train_ES.CAP[1]
  Train_Above.CAP = dat.adj.CAP$RESIDUALS > Train_ES.CAP[1]
  
  ###########################
  # COMPUTE TRAIN SET PERFORMANCE
  # RESID ERR (mean squared error)
  TRAIN_CAP_RESID_ERR[iS] = mean((Train_Adj.CAP-Train_Adj.GT)^2)
  # FALSE POSITIVES: values below for the method, but not below for the GT
  TRAIN_CAP_FP[iS] = length( setdiff(which(Train_Below.CAP), which(Train_Below.GT)) )
  # FALSE NEGATIVES: values below for the method, but not below for the GT
  TRAIN_CAP_FN[iS] = length(setdiff(which(Train_Above.CAP),  which(Train_Above.GT)))
  # TRUE POSITIVES: values below for the method and bewlow for the GT
  TRAIN_CAP_TP[iS] = length( intersect(which(Train_Below.CAP), which(Train_Below.GT)) )
  # TRUE NEGATIVES: values below for the method, but not below for the GT
  TRAIN_CAP_TN[iS] = length(intersect(which(Train_Above.CAP),  which(Train_Above.GT)))
  
  ### TEST SET PERFORMANCE
  CAP.test_resid = test.dat$Obs_score - predict(CAP.res$lm.model, 
                                                newdata = list(Age=test.dat$Age, 
                                                               Edu=test.dat$Edu,  
                                                               Sex=test.dat$Sex))
  
  TEST_CAP_RESID_ERR[iS] = mean((CAP.test_resid - test.dat$P_score)^2)
  
  Test_Below.CAP = CAP.test_resid <= Train_ES.CAP[1]
  Test_Above.CAP = CAP.test_resid > Train_ES.CAP[1]
  
  ## COMPUTE TEST SET PERFORMANCE
  TEST_CAP_NBELOW[iS] = sum ( Test_Below.CAP )
  
  # FALSE POSITIVES: values below for the method, but not below for the GT
  TEST_CAP_FP[iS] = length( setdiff(which(Test_Below.CAP), which(Test_Below.GT)) )
  # FALSE NEGATIVES: values below for the method, but not below for the GT
  TEST_CAP_FN[iS] = length(setdiff(which(Test_Above.CAP),  which(Test_Above.GT)))
  # TRUE POSITIVES: values below for the method and bewlow for the GT
  TEST_CAP_TP[iS] = length( intersect(which(Test_Below.CAP), which(Test_Below.GT)) )
  # TRUE NEGATIVES: values below for the method, but not below for the GT
  TEST_CAP_TN[iS] = length(intersect(which(Test_Above.CAP),  which(Test_Above.GT)))
  
  #######################
  ### ARCARA PERFORMANCE 
  #######################
  # compute adj score with Capitani method
  ARC.res = adjscores_A2024(df=train.dat, dep="Obs_score", 
                            age="Age", edu="Edu", sex="Sex",
                            dep.range = c(0, max(train.dat$Obs_score))
  )
  
  dat.adj.ARC = ARC.res$new.df
  TRAIN_ARC_TRANSF[[iS]] = ARC.res$transfs
  TRAIN_ARC_RS[iS]=summary(ARC.res$lm.model)$r.squared
  TRAIN_ARC_FORMULA[[iS]]=formula(ARC.res$lm.model)
  TRAIN_ARC_MODEL_TEXT[iS]=ARC.res$model_text
  
  # 2a) calculate Capitani's below cut-off obs
  Train_Adj.ARC = dat.adj.ARC$RESIDUALS
  Train_ES.ARC = ES(adjscores= dat.adj.ARC$RESIDUALS)$Adjusted_Scores
  Train_Below.ARC = dat.adj.ARC$RESIDUALS<=Train_ES.ARC[1]
  Train_Above.ARC = dat.adj.ARC$RESIDUALS > Train_ES.ARC[1]
  
  ###########################
  # COMPUTE TRAIN SET PERFORMANCE
  # RESID ERR (mean squared error)
  TRAIN_ARC_RESID_ERR[iS] = mean((Train_Adj.ARC-Train_Adj.GT)^2)
  # FALSE POSITIVES: values below for the method, but not below for the GT
  TRAIN_ARC_FP[iS] = length( setdiff(which(Train_Below.ARC), which(Train_Below.GT)) )
  # FALSE NEGATIVES: values below for the method, but not below for the GT
  TRAIN_ARC_FN[iS] = length(setdiff(which(Train_Above.ARC),  which(Train_Above.GT)))
  # TRUE POSITIVES: values below for the method and bewlow for the GT
  TRAIN_ARC_TP[iS] = length( intersect(which(Train_Below.ARC), which(Train_Below.GT)) )
  # TRUE NEGATIVES: values below for the method, but not below for the GT
  TRAIN_ARC_TN[iS] = length(intersect(which(Train_Above.ARC),  which(Train_Above.GT)))
  
  ### TEST SET PERFORMANCE
  ARC.test_resid = test.dat$Obs_score - predict(ARC.res$lm.model, 
                                                newdata = list(Age=test.dat$Age, 
                                                               Edu=test.dat$Edu,  
                                                               Sex=test.dat$Sex))
  
  TEST_ARC_RESID_ERR[iS] = mean((ARC.test_resid - test.dat$P_score)^2)
  
  Test_Below.ARC = ARC.test_resid <= Train_ES.ARC[1]
  Test_Above.ARC = ARC.test_resid > Train_ES.ARC[1]
  
  ## COMPUTE TEST SET PERFORMANCE
  TEST_ARC_NBELOW[iS] = sum ( Test_Below.ARC )
  
  # FALSE POSITIVES: values below for the method, but not below for the GT
  TEST_ARC_FP[iS] = length( setdiff(which(Test_Below.ARC), which(Test_Below.GT)) )
  # FALSE NEGATIVES: values below for the method, but not below for the GT
  TEST_ARC_FN[iS] = length(setdiff(which(Test_Above.ARC),  which(Test_Above.GT)))
  # TRUE POSITIVES: values below for the method and bewlow for the GT
  TEST_ARC_TP[iS] = length( intersect(which(Test_Below.ARC), which(Test_Below.GT)) )
  # TRUE NEGATIVES: values below for the method, but not below for the GT
  TEST_ARC_TN[iS] = length(intersect(which(Test_Above.ARC),  which(Test_Above.GT)))
  
  
  
  
  
  
}  # for iSim 


## CAPITANI - PERFORMANCE
# TRAIN
TRAIN_CAP_SENSITIVITY =  TRAIN_CAP_TP/(TRAIN_CAP_TP+TRAIN_CAP_FN)
TRAIN_CAP_PRECISION = TRAIN_CAP_TP/(TRAIN_CAP_TP+TRAIN_CAP_FP)
TRAIN_CAP_SPECIFICITY = TRAIN_CAP_TN/(TRAIN_CAP_TN+TRAIN_CAP_FP)
TRAIN_CAP_F1 = 2*(TRAIN_CAP_SENSITIVITY*TRAIN_CAP_PRECISION)/(TRAIN_CAP_SENSITIVITY+TRAIN_CAP_PRECISION)
### TEST 
TEST_CAP_SENSITIVITY =  TEST_CAP_TP/(TEST_CAP_TP+TEST_CAP_FN)
TEST_CAP_PRECISION = TEST_CAP_TP/(TEST_CAP_TP+TEST_CAP_FP)
TEST_CAP_SPECIFICITY = TEST_CAP_TN/(TEST_CAP_TN+TEST_CAP_FP)
TEST_CAP_F1 = 2*(TEST_CAP_SENSITIVITY*TEST_CAP_PRECISION)/(TEST_CAP_SENSITIVITY+TEST_CAP_PRECISION)

## ARCARA - PERFORMANCE
# TRAIN
TRAIN_ARC_SENSITIVITY =  TRAIN_ARC_TP/(TRAIN_ARC_TP+TRAIN_ARC_FN)
TRAIN_ARC_PRECISION = TRAIN_ARC_TP/(TRAIN_ARC_TP+TRAIN_ARC_FP)
TRAIN_ARC_SPECIFICITY = TRAIN_ARC_TN/(TRAIN_ARC_TN+TRAIN_ARC_FP)
TRAIN_ARC_F1 = 2*(TRAIN_ARC_SENSITIVITY*TRAIN_ARC_PRECISION)/(TRAIN_ARC_SENSITIVITY+TRAIN_ARC_PRECISION)
### TEST 
TEST_ARC_SENSITIVITY =  TEST_ARC_TP/(TEST_ARC_TP+TEST_ARC_FN)
TEST_ARC_PRECISION = TEST_ARC_TP/(TEST_ARC_TP+TEST_ARC_FP)
TEST_ARC_SPECIFICITY = TEST_ARC_TN/(TEST_ARC_TN+TEST_ARC_FP)
TEST_ARC_F1 = 2*(TEST_ARC_SENSITIVITY*TEST_ARC_PRECISION)/(TEST_ARC_SENSITIVITY+TEST_ARC_PRECISION)

# add SessionInfo
sim.params$sessionInfo = sessionInfo()

#  full results save
save(sim.params, adjscores_A2024,
     TRAIN_GT_RS, TRAIN_GT_RESID, TRAIN_GT_TRANSF,
     TEST_GT_NBELOW,
     TRAIN_CAP_RS,TRAIN_CAP_FORMULA, TRAIN_GT_MODEL_TEXT, TRAIN_CAP_RESID_ERR,  TRAIN_CAP_TRANSF, TRAIN_CAP_MODEL_TEXT,
     TRAIN_CAP_FN, TRAIN_CAP_FP,TRAIN_CAP_TP, TRAIN_CAP_TN, TRAIN_CAP_PRECISION, TRAIN_CAP_SENSITIVITY, TRAIN_CAP_SPECIFICITY, TRAIN_CAP_F1,
     TEST_CAP_FN, TEST_CAP_FP,TEST_CAP_TP, TEST_CAP_TN, TEST_CAP_PRECISION, TEST_CAP_SENSITIVITY, TEST_CAP_SPECIFICITY, TEST_CAP_F1,     
     TEST_CAP_NBELOW, 
     TRAIN_ARC_RS,TRAIN_ARC_FORMULA, TRAIN_GT_MODEL_TEXT, TRAIN_ARC_RESID_ERR,  TRAIN_ARC_TRANSF, TRAIN_ARC_MODEL_TEXT,
     TRAIN_ARC_FN, TRAIN_ARC_FP,TRAIN_ARC_TP, TRAIN_ARC_TN, TRAIN_ARC_PRECISION, TRAIN_ARC_SENSITIVITY, TRAIN_ARC_SPECIFICITY, TRAIN_ARC_F1,
     TEST_ARC_FN, TEST_ARC_FP,TEST_ARC_TP, TEST_ARC_TN, TEST_ARC_PRECISION, TEST_ARC_SENSITIVITY, TEST_ARC_SPECIFICITY, TEST_ARC_F1,     
     TEST_ARC_NBELOW, 
     file=file_name)




