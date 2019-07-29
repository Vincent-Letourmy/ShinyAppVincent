
function.nbMV <- function(df){
  comp <- 0
  for (col in df) {
    for(val in col){
      if (val == 1) comp = comp + 1
    }
  }
  return(comp)
}

function.nbMissingValues <- function(df){
  comp <- 0
  for (i in df){
    for (j in i){
      if (j == "" || is.na(j)) comp = comp + 1
    }
  }
  return(comp)
}

function.nbRowInconsistency <- function(df){
  comp <- 0
  for (row in rownames(df)) {
    if (1 %in% df[row,]) comp = comp + 1
  }
  return(comp)
}


function.matrixBoolean <- function(df){
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- matrix (rep(0, n1*n2), n1, n2)
  a <- data.frame(a)
  names(a) <- names(df)
  
  for (col in names(df)) {
    ligne <- 1
    for (val in df[,col]) {
      if (is.na(val) || val == "" || val == "?"){
        a[ligne,col] <- 1
      }
      ligne <- ligne + 1
    }
  }
  return(a)
}


### Bar chart Missing values

function.barChartMissingValues <- function(df){
  res <- 0
  for (i in names(df)) {
    col <- df[,i]
    
    a <- 0
    for (j in col) {
      if(is.na(j) || j == "" || j == "?") a = a + 1
    }
    res[i] = round(a / length(col) * 100,digits = 2)
  }
  res <- res[-1]
  return(res)
}



# NAIVE BAYES

# Autre méthode : library(mlr)


function.CVNaiveBayesBis <- function(df,col,tabCosts,fold){
  
  colNA <- is.na(df[,col])
  df_noNAs <<- df[!colNA,]
  
  moy <- 0
  cost <- 0
  restab <- data.frame(tabCosts[,-3],cost)
  
  for (i in 1:fold) {
    
    training.samples <- df_noNAs[,col] %>% 
      caret::createDataPartition(p = 0.8, list = FALSE)
    train.data <- df_noNAs[training.samples, ]
    test.data <- df_noNAs[-training.samples, ]
    
    task <<- makeClassifTask(data = train.data, target = col)
    selected_model <<- makeLearner("classif.naiveBayes")
    
    NB_mlr <<- mlr::train(selected_model, task)
    
    predictions_mlr <<- as.data.frame(predict(NB_mlr, newdata = test.data[,!names(df_noNAs) %in% c(col)]))
    resultNaiveBayes <<- table(predictions_mlr[,1],test.data[,col])
    res <- as.data.frame(resultNaiveBayes)
    
    
    # Création du tableau de fréquences
    for (row in row.names(restab)) {
      restab[row,"cost"] = restab[row,"cost"] + res[row,"Freq"] 
    }
    
    #Création moyenne
    aux <- 0
    for(j in row.names(res)){
      if (as.integer(res[j,c("Var1")]) == as.integer(res[j,c("Var2")])) {
        aux[j] = res[j,c("Freq")]
      }
    }
    aux <- as.data.frame(aux)
    moy[i]<- sum(aux)/sum(res$Freq)*100
    
  }
  
  # Tableau de fréquences 
  for (row in row.names(restab)) {
    restab[row,"cost"] = restab[row,"cost"] / fold
  }
  resultats <- list("restab" = restab, "moy" = moy)
  return(resultats)
  
}

function.tabNaiveBayesBis <- function(df, colName){
  
  col <- df[,c(colName)]
  colNA <- is.na(col)
  df <- df[!colNA,]
  
  task = makeClassifTask(data = df, target = colName)
  selected_model = makeLearner("classif.naiveBayes")
  NB_mlr = mlr::train(selected_model, task)
  
  NB_mlr$learner.model
  predictions_mlr = as.data.frame(predict(NB_mlr, newdata = df[,!names(df) %in% c(colName)]))
  tab <- table(predictions_mlr[,1],df[,c(colName)])
  cost <- 0
  
  return( data.frame(as.data.frame(tab)[,-3],cost) )
  
}






























