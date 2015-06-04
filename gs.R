rm(list =ls())
library(rrBLUP)
# Read in both files

geno <- read.csv('GenotypicData.csv', header = TRUE, stringsAsFactors = TRUE)
dim(geno)
pheno <- read.csv('PhenotypicData.csv', header = TRUE)
dim(pheno)

# How many missing markers?
sum(is.na(geno))

# Remove the names column
geno <- geno[,-1]

# Impute the various missing markers 
impute=A.mat(geno,max.missing=0.5,impute.method="mean",return.imputed=T)
Markers_impute=impute$imputed


# Check to see if there are any markers that still have more than 50% missing data
sum(is.na(Markers_impute))

# There is no missing data so we can proceed on

# Define the additive matrix
addMat <- impute$A

# Define training - 60% of inds. chosen at random and validation dataset
train  <- as.matrix(sample(1:252, 151))

test <- setdiff(1:252, train)

# Train each dataset
pheno_train = pheno[train,]
geno_train = Markers_impute[train,]

pheno_test = pheno[test,]
geno_test = Markers_impute[test,]

# Run mix solved
yield = (pheno_train[,6])

# Sans/without kinship
yield_answer <- mixed.solve(yield, Z=geno_train, K=NULL)

# Marker effects - probably want to iterate this a bunch 
YLD = yield_answer$u
e = as.matrix(YLD)

prediction_yld_valid <- geno_test %*% e

yield_valid = pheno_test[,6]

YLD_accuracy <- cor(prediction_yld_valid, yield_valid, use = "complete")
YLD_accuracy

