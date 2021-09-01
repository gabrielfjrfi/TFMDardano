library(missMDA)
data("vnf")
dim(vnf)

summary(vnf)

library(FactoMineR)
res <- MCA(vnf)

if(!require("caret")) {
  install.packages("caret")
  library("caret")
}
library(parallel)

source("newgit paralel.R")

## Estimate the number of components
nb <- estim_ncpMCA2par(vnf)
nb



## Impute the dataset
complete <- imputeMCA(vnf, ncp=nb$ncp)
names(complete)

head(complete$tab.disj)

res <- MCA(vnf, tab.disj = complete$tab.disj)
