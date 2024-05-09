# install required packages
# install.packages("readxl")
#install.packages("effects")
source("R_functions/adjscores_A2024_v3.R")
source("R_functions/adjscores_C1987_v1.R")
source("R_functions/formula_transf_text.R")
source("R_functions/model_transf_text.R")
source("R_functions/sample.transf.R")
source("R_functions/transf_functions.R")


# load required packages
require(effects)
require(car)
require(MASS)

MOCA.dat = read.csv("Original_Data/MOCA_Dataset.csv", sep=",", dec=".")
head(MOCA.dat)

MOCA.dat = na.omit(MOCA.dat)

cor.test(MOCA.dat$Age, MOCA.dat$Education)

# 2) compute adj score with Capitani method
MOCA.CAP.res = adjscores_C1987(df=MOCA.dat, dep="MOCA", 
                          age="Age", edu="Education", sex="Sex",
                          dep.range = c(0, 30))


# check equation results
print(MOCA.CAP.res$model_text)

# re-fit manually the model for better results inspection with the effect package
MOCA.CAP.lm.res = MOCA.CAP.res$lm.model

summary(MOCA.CAP.lm.res)
plot(allEffects(MOCA.CAP.lm.res, partial.residuals=T), residuals.cex=0.2)


MOCA.ARC.res = adjscores_A2024(df = MOCA.dat, dep="MOCA", 
                             age="Age", edu="Education", sex="Sex",
                             dep.range = c(0, 30))

print(MOCA.ARC.res$model_text)

MOCA.ARC.lm.res = MOCA.ARC.res$lm.model
summary(MOCA.ARC.lm.res)

plot(allEffects(MOCA.ARC.lm.res, partial.residuals=T), residuals.cex=0.2)

AIC(MOCA.ARC.lm.res)
AIC(MOCA.CAP.lm.res)


