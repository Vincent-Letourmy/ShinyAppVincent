
library(e1071)
library(caret) 
library(dplyr)


# Each column as factor

function.as_factor <- function(df){
  for (i in names(df)) {
    df[,i] <- as.factor(df[,i])
  }
  return(df)
}


function.CVNaiveBayes <- function(df,col,tabCosts,fold,ranges){
  
  resultats <- list()
  cost <- 0
  resultats$restab <- data.frame(tabCosts[,-3],cost)
  nbRowTabCosts <- nrow(tabCosts)
  rangesFirst <- ranges[,col][1] # ATTENTION, le premier élément dans ranges pour les booléens doit être FALSE, 0, non ... La négation
  
  withProgress( message = "Naive Bayes prediction ...", value = 0, {
    
    for (i in 1:fold) {
      
      # Train and test partition
      training.samples <- df[,col] %>% 
        caret::createDataPartition(p = 0.8, list = FALSE)
      train.data <- df[training.samples, ]
      test.data <- df[-training.samples, ]
      Reality <- test.data[,col]
      
      # Naive Bayes prediction
      Naive_Bayes_Model=naiveBayes(train.data[,col] ~., data = train.data)
      NB_Predictions=predict(Naive_Bayes_Model,test.data[,!names(test.data)%in%col])
      res <- data.frame(table(NB_Predictions,Reality))
      
      if (nbRowTabCosts == 4){
        stat <- funct.eval_metrics_binomial(res$Freq)
        
        resultats$sensitivity[i] <- stat$sensitivity*100
        resultats$specificity[i] <- stat$specificity*100
        resultats$moy[i] <- stat$accuracy*100
        resultats$auc[i] <- auc(ordered(res$Reality),ordered(res$NB_Predictions), quiet = TRUE)*100
        
      }
      else{
        # Create mean
        aux <- 0
        for(j in row.names(res)){
          if (as.integer(res[j,c("NB_Predictions")]) == as.integer(res[j,c("Reality")])) {
            aux[j] = res[j,c("Freq")]
          }
        }
        aux <- as.data.frame(aux)
        resultats$moy[i]<- sum(aux)/sum(res$Freq)*100
      }
      
      # Create frequences tab
      resultats$restab[,"cost"] <- ( resultats$restab[,"cost"] + res[,"Freq"] ) 
      
      incProgress(1/fold)
    }
  })
  
  
  # Frequences tab retraité en fonction du fold
    resultats$restab[,"cost"] <- resultats$restab[,"cost"] / fold
  
  
  return(resultats)
  
}


# Costs tab from NB

function.tabNaiveBayes <- function(df, colName){
  col <- df[,colName]
  otherCol <- names(df)[1]
  dfRed <- df[,c(otherCol,colName)]
  Naive_Bayes_Model=e1071::naiveBayes(col ~., data = dfRed)
  NB_Predictions=predict(Naive_Bayes_Model,dfRed)
  tab <- data.frame(table(NB_Predictions,col))
  tab <- data.frame(Prediction = tab[,1], Reality = tab[,2])

  cost <- 0

  return( data.frame(as.data.frame(tab),Cost = cost) )
  
}


# Sensitivity and specificity due to 

funct.eval_metrics_binomial <- function(vect) {
  
  results <- list()
  
  if (sum(vect) != 0) results$accuracy = ( vect[1] + vect[4] ) / sum(vect)
  else results$accuracy = 0
  
  sens <- vect[4] + vect[3]
  if (sens != 0) results$sensitivity = vect[4] / sens
  else results$sensitivity = 0
  
  spe <- vect[1] + vect[2]
  if (spe != 0) results$specificity = vect[1] / spe
  else results$specificity = 0
  
  results
}










