\name{LXpca}
\alias{LXpca}
\title{PXpca: PCA(principal component analysis)}
\usage{
LXpac(data_file)
}
\description{
LXpca can be used for the principal component analysis (PCA)
}
\examples{

if(!requireNamespace("devtools"))
  install.packages("devtools")
  library(devtools)

install_github("gluck4668/LXpca")

library(LXpca)

#-----------------------------
data(pca_data_example)

#----------------------------


rm(list=ls())

setwd("D:/R-lin study/R-PCA")

data_file <- c("PCA_data.xlsx")

LXpca(data_file)

}
