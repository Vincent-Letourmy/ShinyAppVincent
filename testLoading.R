library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(plotly)
source("ShinyApp/funct_loopResults.R")

header <- function() {
  dashboardHeader(title = "TEST")
}

sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Website", icon = icon("send",lib='glyphicon'), 
               href = "https://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29")
    )
  )
}


body <- function(){
  dashboardBody(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
          plotlyOutput("linechart")
        )
      )
    )
  )
}

ui <- dashboardPage(title = 'test', header(), sidebar(), body())


server <- function(input, output, session) {
  
  output$linechart <- renderPlotly({
    x = c("A","B","C")
    y = c(10,15,12)
    z = c(13,11,14)
    
    tab <- data.frame(x,col = y, row.names = x)
    tab
    tab2 <- data.frame(x,col = z, row.names = x)
    tab2
    p <- function.lineChart(tab2,tab,"col")
    p
  })
  
}

shinyApp(ui = ui, server = server)















