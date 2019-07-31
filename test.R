
source("ShinyApp/funct_4dataquality.R")
source("ShinyApp/funct_5CVNaiveBayes.R")

#df <- read.csv2("ShinyApp/CSV_copie/RangesDataCopie.csv", header = TRUE, sep = ";", quote = "")

df <- read.csv2("ShinyApp/CSV/risk_factors_cervical_cancer_Original.csv", header = TRUE, sep = ",", quote = "")
for (col in names(df )) {
  column <- as.character(df[,col])
  df[,col] <-  ifelse(column == "?", "", column)
}
types <- read.csv2("ShinyApp/CSV/TypesDataOriginal.csv", header = TRUE, sep = ";", quote = "")
ranges <- read.csv2("ShinyApp/CSV/RangesDataOriginal.csv", header = TRUE, sep = ";", quote = "")


matrix <- function.matrixBooleanMissingValues(df)
matrix

res <- function.removeConsistency(df,matrix)
res




nouv <- data.frame(A = c(1,2,3),
                   B = c(4,5.5,6),
                   C = c(7,8,9))

nouv[,"C"] <- nouv[,"A"] + nouv[,"B"]
nouv

library(pROC)

real <- c(1,0,1,0,1)
pred <- c(0,0,0,0,0)

res <- accuracy(real,pred)
res <- auc(real,pred)
res

df <- function.as_factor(df)

tabcosts <- function.tabNaiveBayes(df,"Biopsy")
tabcosts
res <- function.CVNaiveBayes(df,"Biopsy",tabcosts,30,ranges)
res
