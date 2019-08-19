
function.tabRes <- function(tabRes, row,colRemoved, badValues, nbcol, nbrow, res, tabCosts,costFixing){
  
  moy <- res$moy
  sens <- res$sensitivity
  spe <- res$specificity
  auc <- res$auc
  resCost <- res$restab$cost
  costs <- tabCosts$Cost
  
  if (!is.null(colRemoved)){
    tabRes[row,"Column removed (inconsistency %)"] <- paste(colRemoved,"(",badValues,"% )")
    #tabRes[row,"Inconsistency (%)"] <- badValues
  }
  tabRes[row,"Nb Col"] <- nbcol
  tabRes[row,"Nb Row"] <- nbrow
  tabRes[row,"Accuracy (%)"] <- round(mean(moy), digits = 2)
  tabRes[row,"Sensitivity (%)"] <- round(mean(sens), digits = 2)
  tabRes[row,"Specificity (%)"] <- round(mean(spe), digits = 2)
  tabRes[row,"AUC (%)"] <- round(mean(auc), digits = 2)
  cost <- round(sum(resCost * costs) * 5 / nbrow, digits = 2)
  tabRes[row,"Cost (per patient)"] <- cost
  
  if (!is.null(costFixing)){
    costFix <- costFixing / nbrow
    tabRes[row,"Fixing cost"] <- costFix
    tabRes[row,"Total Cost"] <- cost + costFix
  }
  
  return(tabRes)
  
}

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°


function.uniqueResults <- function(name, df, tabCosts, target, fold, costFixing){
  
  withProgress(message = "Initial DB ...", detail = "Don't worry :)", value = 0, {
    
    tabRes <- data.frame()
    row <- name
    incProgress(0.5)
    res <- function.CVNaiveBayes(df,target,tabCosts,fold)
    div <- nrow(df)
    
    
    tabRes <- function.tabRes(tabRes, row, 
                              NULL,
                              NULL,
                              ncol(df),
                              div,
                              res,
                              tabCosts,
                              costFixing
    )
    incProgress(0.5)
    return(tabRes)
    
  })
}

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°


function.loopResultsDQ <- function(df, matrix , tabCosts, target, fold, tabCol, removeCol){
  
  withProgress(message = "Progressing ...", detail = "Don't worry :)", value = 0, {
    
    tabRes <- data.frame()
    nomCol <- names(tabCol)
    
    
    
    # DQ with all columns
    
    if (removeCol){
      row <- "Data Quality 0"
      rowRemove <- function.removeConsistency(df,matrix)
      dfClean <- df[!row.names(df)%in%rowRemove , ]
    }
    else{
      row <- "DataBase - Initial"
      dfClean <- df
    }
    
    
    incProgress(0.5)
    res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold)
    
    div <- nrow(dfClean)
    
    tabRes <- function.tabRes(tabRes, row, 
                              "",
                              "",
                              ncol(dfClean),
                              div,
                              res,
                              tabCosts,
                              NULL
    )
    incProgress(0.5)
  })
  
  l <- "-"
  n <- 0
  progress <- 1/length(nomCol)
  withProgress(message = "Progressing ...", detail = "Don't worry :)", value = 0, {
    
    for (col in nomCol) {
      n <- n + 1
      row = paste(n)
      
      df <- df[,!names(df)%in%col]
      matrix <- matrix[,!names(matrix)%in%col]
      
      
      if (removeCol){
        rowRemove <- function.removeConsistency(df,matrix)
        dfClean <- df[!row.names(df)%in%rowRemove , ]
      }
      else{
        dfClean <- df
      }
      
      incProgress(progress/3) # Progress bar
      
      res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold)
      
      div <- nrow(dfClean)
      incProgress(progress/3) # Progress bar
      tabRes <- function.tabRes(tabRes, row, 
                                col,
                                tabCol[col],
                                ncol(dfClean),
                                div,
                                res,
                                tabCosts,
                                NULL
      )
      incProgress(progress/3) # Progress bar
    }
    
  })
  
  
  return(tabRes)
  
}

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°


function.outputLineChart <- function(tabOnlyCol,tabDQ,colName,y){
  renderPlotly({
    x <- rownames(tabDQ)
    p <- plot_ly(
      tabDQ,x = factor(x,levels = x), y = ~tabOnlyCol[,colName], type = "scatter", mode = "lines", name = "Col removed"
    ) %>% 
      layout(xaxis = list(title = "Number of columns removed"),
             yaxis = list(title = y))
    layout
    p <- add_trace(p,x = factor(x,levels = x), y = ~tabDQ[,colName], mode = "lines", name = "Col/rows rem.")
  })
}


function.resLineChart <- function(title, status, lineChart, width){
  
  renderUI({
    box(title = title,
        status = status,
        solidHeader = TRUE,
        width = width,
        withSpinner(plotlyOutput(lineChart))
        
    )
  })

}

function.boxresTab <- function(name, tableName){
  box(title = name,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      column(12, align = "center",
             withSpinner(tableOutput(tableName))
      )
  )
}



























