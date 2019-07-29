library(plotly)

source("ShinyApp/funct_4dataquality.R")
source("ShinyApp/funct_5CVNaiveBayes.R")

df <- read.csv("ShinyApp/CSV/risk_factors_cervical_cancer_Original.csv", header = TRUE, sep = ",")

  for (col in names(df )) {
    column <- as.character(df[,col])
    df[,col] <-  ifelse(column == "?", "", column)
  }

df <- function.as_factor(df)

#dfPerfect <- read.csv("ShinyApp/CSV_NoProblem/MyDataDQ.csv", header = TRUE, sep = ",")
#dfPerfect <- function.as_factor(dfPerfect)

types <- read.csv("ShinyApp/CSV/TypesDataOriginal.csv", header = TRUE, sep = ";")
ranges <- read.csv("ShinyApp/CSV/RangesDataOriginal.csv", header = TRUE, sep = ";")
matrix <- function.matrixBooleanConsistency(df,types,ranges)

target <- "Hinselmann"
otherTargets <- c("Citology","Schiller","Biopsy")
df <- df[,!names(df)%in%otherTargets]
#dfPerfect <- dfPerfect[,!names(dfPerfect)%in%otherTargets]
matrix <- matrix[,!names(matrix)%in%otherTargets]

dfNa <- function.barChartInconsistency(matrix)
res <- sort(dfNa, decreasing = TRUE)
# dfNa trié OK
minimum <- names(which(res > 20))  # Mettre le résulat du slider min
maximum <- names(which(res < 100)) # Mettre le résulat du slider max

nomCol <- intersect(minimum,maximum)
res <- res[nomCol]

tabCosts <- function.tabNaiveBayes(df,target)
tabCosts[2,"Cost"] <- 50
tabCosts[3,"Cost"] <- 100

source("ShinyApp/funct_loopResults.R")

res <- function.loopResultsDQ(df,matrix,tabCosts,target,ranges,10,res)
res

res[, c("Accuracy (%)", "Sensitivity (%)","Specificity (%)","Cost (per patient)")]


















