
### Download tab costs

function.downloadFile <- function(tabCosts) {
  downloadHandler(
    
    filename = function() {
      paste("MydataDownload", "csv", sep = ".")
    },
    
    content = function(file) {
      write.table(tabCosts, file, sep = ",",
                  row.names = FALSE)
    }
  )
}


### Save data

function.saveDataInFile <- function(costsTab, file){
  write.csv(hot_to_r(costsTab), file,row.names = FALSE)
  return(as.data.frame(read.csv(file)))
}
