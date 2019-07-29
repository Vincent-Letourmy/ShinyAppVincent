source("funct_5CVNaiveBayes.R")

#----------------------------------------------------------------------------------------- INITIALISATION


# Upload Files CSV

function.fileInput <- function(fileCSV, fileName){
  fileInput(fileCSV, paste("CSV File",fileName),
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
            )
}

function.loadFile <- function(file, header, sep, quote){
  df <- read.csv(file,
           header = header, 
           sep = sep,
           quote = quote)
  return(df)
}


# Parameters box

function_parametersBox <- function(header,sep,quote,collapsed){
  
  renderUI({
    box(width = 12,
        title = "Parameters (CSV)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = collapsed,
        column(6,
               checkboxInput(header, "Header", TRUE),
               radioButtons(sep, "Separator",
                            choices = c("Comma" = ",",
                                        "Semicolon" = ';',
                                        "Tab" = "\t"),
                            selected = ';')
        ),
        column(6,
               radioButtons(quote, "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"')
        )
    )
  })
  
}


#----------------------------------------------------------------------------------------- TARGET

# Selection column

function.selectionColumn <- function(df){
  if (is.null(df)) {
    return (h4("Please upload a file and then select a column"))
  }
  items=rev(names(df))
  names(items)=items
  selectInput("selectcolumn", "Choose a column",items)
}




















