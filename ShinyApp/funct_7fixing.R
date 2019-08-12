
# ------------------------------------------------ Results Fixed ----------------------------------------------------------------#

# Result of cost : prediction fail + fixing 

function.costsResultsValueFixed <- function(resultData, fixingCost){
  result <- round(resultData + fixingCost, digits = 0)
  valueBox(
    value = paste("Cost : ",result)
    ,paste('Cost of prediction fail :',round(resultData,digits = 2), " + cost of fixing :", round(fixingCost,digits = 2))
    ,icon = icon("menu-hamburger",lib='glyphicon')
    ,color = "green")
}