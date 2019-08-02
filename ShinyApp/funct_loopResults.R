
function.tabRes <- function(tabRes, row,colRemoved, badValues, nbcol, nbrow, res, tabCosts,costFixing){
  
  moy <- res$moy
  sens <- res$sensitivity
  spe <- res$specificity
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
  cost <- round(sum(resCost * costs) * 5 / nbrow, digits = 2)
  tabRes[row,"Cost (per patient)"] <- cost
  
  if (!is.null(costFixing)){
    costFix <- costFixing / nbrow
    tabRes[row,"Fixing cost"] <- costFix
    tabRes[row,"Total Cost"] <- cost + costFix
  }
  
  return(tabRes)
  
}


function.uniqueResults <- function(name, df, tabCosts, target, ranges, fold, costFixing){
  
  withProgress(message = "Initial DB ...", detail = "Don't worry :)", value = 0, {
    
    tabRes <- data.frame()
    row <- name
    incProgress(0.5)
    res <- function.CVNaiveBayes(df,target,tabCosts,fold,ranges)
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



function.loopResultsDQ <- function(df, matrix , tabCosts, target, ranges, fold, tabCol, removeCol){
  
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
    res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold,ranges)
    
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
  
  l <- "DQ"
  n <- 0
  row <- paste(l,n)
  progress <- 1/length(nomCol)
  withProgress(message = "Progressing ...", detail = "Don't worry :)", value = 0, {
    
    for (col in nomCol) {
      n <- n + 1
      row = paste(l,n)
      
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
      
      res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold,ranges)
      
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


function.outputLineChart <- function(tabOnlyCol,tabDQ,colName,y){
  renderPlotly({
    x <- rownames(tabDQ)
    p <- plot_ly(
      tabDQ,x = factor(x,levels = x), y = ~tabOnlyCol[,colName], type = "scatter", mode = "lines"
    ) %>% 
      layout(xaxis = list(title = "Step"),
             yaxis = list(title = y))
    p <- add_trace(p,x = factor(x,levels = x), y = ~tabDQ[,colName], mode = "lines")
  })
}


function.resLineChart <- function(title, status, lineChart){
  
  renderUI({
    box(title = title,
        status = status,
        solidHeader = TRUE,
        width = 6,
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





























