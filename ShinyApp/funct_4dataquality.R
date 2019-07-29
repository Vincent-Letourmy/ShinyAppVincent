
#--- Consistency --------------------------------------------------------------------------------------------------------#

### Data for bar chart inconsistency

function.barChartInconsistency <- function(matrixBool){
  res <- 0
  for (col in names(matrixBool)){
    column <- matrixBool[,col]
    res[col] <- round ( sum(column == 1) / length(column) * 100 , digits = 2 )
  }
  res <- res[-1]
  return(res)
}


# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

### 0/1 file inconsistency

function.matrixBooleanConsistency <- function(df,types,ranges){
  
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- data.frame(matrix (rep(0, n1*n2), n1, n2))
  names(a) <- names(df)
  rownames(a) <- rownames(df)
  
  for (col in names(df)) {
    typ <- types[,col]
    
    if (typ == "string"){
      df[,col] <- as.character(df[,col])
      rang <- ranges[,col]
    }
    else if (typ == "numeric" || typ == "integer") {
      df[,col] <- as.character(df[,col])
      df[,col] <- as.numeric(df[,col])
      rangMin <- ranges[1,col]
      rangMax <- ranges[2,col]
    }
    
    for (ligne in row.names(df)){
      
      val <- df[ligne,col]
      
      # TEST NUMERIC/INTEGER
      
      if (typ == "numeric" || typ == "integer") {
        if (! is.na(val)){
          if (val < rangMin || val > rangMax) {
            a[ligne,col] <- 1
          }
        }
        else a[ligne,col] <- 1
      }
      ###
      
      # TEST STRING
      
      else if (typ == "string") {
        if (! is.na(val)){
          if (val %in% rang && val != ""){}
          else a[ligne,col] <- 1
        }
        else a[ligne,col] <- 1
      }
      ###
    }
  }
  return(a)
}

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

function.removeConsistency <- function(df, a){
  rem <- 0
  for (row in row.names(a)) {
    if (1 %in% a[row,]) rem[row] = row
  }
  return(rem[-1])
}


# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

function.matching <- function(df1, df2, nameFile){
  
  nbCol <- ncol(df1)
  
  if ( is.null(df2) ) {
    valueBox(value = nameFile, subtitle = paste("Please upload",nameFile, "file"), icon = icon("question",lib='font-awesome'), color = "yellow")
  }
  else if ( nbCol != ncol(df2) ) {
    valueBox(value = paste(nameFile," not match"), subtitle = "Number of columns doesn't match", icon = icon("thumbs-down",lib='font-awesome'), color = "red")
  }
  else if ( length(union(names(df1),names(df2))) != nbCol){
      valueBox(value = paste(nameFile," not match"), subtitle = "Column names don't match", icon = icon("thumbs-down",lib='font-awesome'), color = "red")
  }
  else 
    valueBox(value = paste(nameFile," match"), subtitle = "Number and names of columns match", icon = icon("thumbs-up",lib='font-awesome'), color = "green")
    
}



































