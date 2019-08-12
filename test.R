library(pROC)

list <- c(0,0,1,0,1)
list2 <- c(0,1,1,1,1)

list <- ordered(list)
list2 <- ordered(list2)

auc(list,list2)
auc(list2,list)

log <- as.logical(list)
log2 <- as.logical(list2)

log <- ordered(log)
log2 <- ordered(log2)

a <- auc(log,log2)


ot <- c("red","red","blue","blue","red")
ordered(ot)
order

oth <- c("b","a")
ordered(oth)


source("ShinyApp/funct_4dataquality.R")

library(shiny)

df <- read.csv("ShinyApp/CSV/risk_factors_cervical_cancer_Original.csv", header = TRUE, sep = ",")
for (col in names(df )) {
  column <- as.character(df[,col])
  df[,col] <-  ifelse(column == "?", "", column)
}
df <- function.as_factor(df)
types <- read.csv("ShinyApp/CSV/TypesDataOriginal.csv", header = TRUE, sep = ";")
ranges <- read.csv("ShinyApp/CSV/RangesDataOriginal.csv", header = TRUE, sep = ";")
col <- "Biopsy"
tab <- function.tabNaiveBayes(df,col)
source("ShinyApp/funct_5CVNaiveBayes.R")
res <- function.CVNaiveBayes(df,col,tab,2,ranges)
res
































