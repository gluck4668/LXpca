
setwd("D:/R-lin study/R-packages/Xpca")
library(openxlsx)

pca_data_example <- read.xlsx("PCA_example.xlsx")

usethis::use_data(pca_data_example,overwrite = T)

rm(list=ls())

data(pca_data_example)

