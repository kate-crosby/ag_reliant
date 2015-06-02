rm(list =ls())

# Read in both files

geno <- read.csv('GenotypicData.csv', header = TRUE, stringsAsFactors = TRUE)

pheno <- read.csv('PhenotypicData.csv', header = TRUE)