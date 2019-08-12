library(pROC)

list <- c(0,0,1,0,1)
list2 <- c(0,1,1,1,1)



source("ShinyApp/funct_4dataquality.R")

library(shiny)

df <- read.csv("ShinyApp/CSV/risk_factors_cervical_cancer_Original.csv", header = TRUE, sep = ",")

for (col in names(df )) {
  column <- as.character(df[,col])
  df[,col] <-  ifelse(column == "?", "", column)
}
df <- function.as_factor(df)
col <- "Biopsy"
tab <- function.tabNaiveBayes(df,col)
library(pROC)
source("ShinyApp/funct_5CVNaiveBayes.R")
res <- function.CVNaiveBayes(df,col,tab,2)
res
































