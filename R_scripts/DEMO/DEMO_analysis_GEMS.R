# install required packages
# install.packages("readxl")
#install.packages("effects")
source("R_functions/adjscores_A2024_v3.R")
source("R_functions/adjscores_C1987_v1.R")
source("R_functions/formula_transf_text.R")
source("R_functions/model_transf_text.R")
source("R_functions/sample.transf.R")
source("R_functions/group_v.R")

source("R_functions/transf_functions.R")


# load required packages
require(effects)
require(MASS)
require(car)

GEMS.dat = read.csv("Original_Data/GEMS_Dataset.csv", sep=",", dec=".")
head(GEMS.dat)

# divide in bins to facilitate a check of age and education distribution
GEMS.dat$age_group = group_v(GEMS.dat$Age,v_bins = list(c(18, 22), c(23, 40), c(41, 50), c(51, 60), c(61, 70), c(71, 80), c(81, 90)) )
GEMS.dat$edu_group = group_v(GEMS.dat$Education, v_bins = list(c(0, 5), c(6, 8), c(9, 13), c(14, 18), c(19, Inf)))
GEMS.dat$edu_group = ordered(GEMS.dat$edu_group, c("0-5", "6-8","9-13", "14-18", "19-Inf"))

prop.table(table(GEMS.dat$age_group, GEMS.dat$edu_group))

# inspect correlation between 
cor.test(GEMS.dat$Age, GEMS.dat$Education)


# fix values for participants with zero Education (it would create problems with some transformations, as log or inv)
GEMS.dat[GEMS.dat$Education==0, "Education"] = 1

GEMS.dat = na.omit(GEMS.dat)

# 2) compute adj score with Capitani method
GEMS.CAP.res = adjscores_C1987(df=GEMS.dat, dep="GEMS", 
                          age="Age", edu="Education", sex="Sex",
                          dep.range = c(0, 100))


# check equation results
print(GEMS.CAP.res$model_text)

# re-fit manually the model for better results inspection with the effect package
GEMS.CAP.lm.res = GEMS.CAP.res$lm.model

summary(GEMS.CAP.lm.res)
plot(allEffects(GEMS.CAP.lm.res, partial.residuals=T), residuals.cex=0.2)


GEMS.ARC.res = adjscores_A2024(df = GEMS.dat, dep="GEMS", 
                             age="Age", edu="Education", sex="Sex",
                             dep.range = c(0, 100))

print(GEMS.ARC.res$model_text)

GEMS.ARC.lm.res = GEMS.ARC.res$lm.model
summary(GEMS.ARC.lm.res)

plot(allEffects(GEMS.ARC.lm.res, partial.residuals=T), residuals.cex=0.2)

AIC(GEMS.ARC.lm.res)
AIC(GEMS.CAP.lm.res)


