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
require(MASS)
require(car)

Tele_GEMS.dat = read.csv("Original_Data/TeleGEMS_Dataset.csv", sep=",", dec=".")
head(Tele_GEMS.dat)

cor.test(Tele_GEMS.dat$Age, Tele_GEMS.dat$Education)

# fix people with zero Education
Tele_GEMS.dat[Tele_GEMS.dat$Education==0, "Education"] = 1

Tele_GEMS.dat = na.omit(Tele_GEMS.dat)

# 2) compute adj score with Capitani method
Tele_GEMS.CAP.res = adjscores_C1987(df=Tele_GEMS.dat, dep="Tele_GEMS", 
                          age="Age", edu="Education", sex="Sex",
                          dep.range = c(0, 100))


# check equation results
print(Tele_GEMS.CAP.res$model_text)

# re-fit manually the model for better results inspection with the effect package
Tele_GEMS.CAP.lm.res = Tele_GEMS.CAP.res$lm.model

summary(Tele_GEMS.CAP.lm.res)
plot(allEffects(Tele_GEMS.CAP.lm.res, partial.residuals=T), residuals.cex=0.2)


Tele_GEMS.ARC.res = adjscores_A2024(df = Tele_GEMS.dat, dep="Tele_GEMS", 
                             age="Age", edu="Education", sex="Sex",
                             dep.range = c(0, 100))

print(Tele_GEMS.ARC.res$model_text)

Tele_GEMS.ARC.lm.res = Tele_GEMS.ARC.res$lm.model
summary(Tele_GEMS.ARC.lm.res)

plot(allEffects(Tele_GEMS.ARC.lm.res, partial.residuals=T), residuals.cex=0.2)

AIC(Tele_GEMS.ARC.lm.res)
AIC(Tele_GEMS.CAP.lm.res)


