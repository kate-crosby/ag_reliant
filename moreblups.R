rm(list =ls())
library(lme4)
library(broom)
# Read in phenotypes

pheno <- read.csv('PhenotypicData.csv', header = TRUE)
dim(pheno)

# Chose not to transform - I guess you could for yield
hist(pheno$YLD, col="gold")
hist(pheno$H2O, col="tomato")
boxplot(pheno$YLD~pheno$Location, xlab="Location", ylab="Yield", main="Yield by Location")
boxplot(pheno$H2O~pheno$Location, xlab="Location", ylab="Drought traits")


# Explanatory
YLD = as.numeric(pheno$YLD)
water = as.numeric(pheno$H2O)

# Predictors randoms
LOC= as.factor(pheno$Location)
tgrp = as.factor(pheno$Trial_Group)
subS <- as.factor(pheno$Subset)
REP = as.factor(pheno$REP)

PED = as.numeric(pheno$Pedigree)

# Linear Model with random effects for variance components - no three way
wmodel = lmer(water ~ (1|LOC) + (1|PED) + (1|tgrp) +
                (1:subS%in%tgrp:LOC:REP) + (1|PED:LOC) + (1|PED:tgrp))


# Extract variance components
summary(wmodel)


# Example heritability for water - too high? Model specification probably wrong
# I assumed pedigree was line effect

nherit = 2.43/(2.43 + (0.01787/6) + (0.12547/13) + (3.39881/78))


# Linear Model with random effects for variance components
ymodel = lmer(YLD ~ (1|LOC) + (1|PED) + (1|tgrp) +
                (1:subS%in%tgrp:LOC:REP) + (1|PED:LOC) + (1|PED:tgrp))

                  
# Extract variance components - yield is less heritable than water
summary(ymodel)

nherit = 28.7923/(28.7923 + (51.3228/6) + (0.8675/13) + (761.4369/78))






