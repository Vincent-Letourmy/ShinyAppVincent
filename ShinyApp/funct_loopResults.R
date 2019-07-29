
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
  tabRes <- data.frame()
  row <- name
  
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
  
  
  
  return(tabRes)
}



function.loopResultsDQ <- function(df, matrix , tabCosts, target, ranges, fold, tabCol){
  
  tabRes <- data.frame()
  nomCol <- names(tabCol)
  
  row <- "Data Quality 0"
  
  # DQ with all columns
  
  rowRemove <- function.removeConsistency(df,matrix)
  dfClean <- df[!row.names(df)%in%rowRemove , ]
  
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
  
  
  l <- "DQ"
  n <- 0
  row <- paste(l,n)
  
 
  
  for (col in nomCol) {
    
    n <- n + 1
    row = paste(l,n)
    
    df <- df[,!names(df)%in%col]
    matrix <- matrix[,!names(matrix)%in%col]
    
    rowRemove <- function.removeConsistency(df,matrix)
    dfClean <- df[!row.names(df)%in%rowRemove , ]
    
    res <- function.CVNaiveBayes(dfClean,target,tabCosts,fold,ranges)
    
    div <- nrow(dfClean)
    
    tabRes <- function.tabRes(tabRes, row, 
                              col,
                              tabCol[col],
                              ncol(dfClean),
                              div,
                              res,
                              tabCosts,
                              NULL
    )
    
    
    
  }
  
  return(tabRes)
  
}







function.resLineChart <- function(title, status, tab, colName, y){
  
  renderUI({
    box(title = title,
        status = status,
        solidHeader = TRUE,
        width = 6,
        
        renderPlotly({
          x <- rownames(tab)
          plot_ly(
            tab,x = factor(x,levels = x), y = ~tab[,colName], type = "scatter", mode = "lines"
          ) %>% 
            layout(xaxis = list(title = "Step"),
                   yaxis = list(title = y))
        })
        
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





























