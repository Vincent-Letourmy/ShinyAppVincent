library(shiny)
library(shinycssloaders)
library(shinydashboard)

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
          valueBox(value = "Bonjour", subtitle = "Number and names of columns", icon = icon("thumbs-up",lib='font-awesome'), color = "green")
        ),
        mainPanel(
          
        )
      )
    )
  )
}

ui <- dashboardPage(title = 'test', header(), sidebar(), body())


server <- function(input, output, session) {
  
  
}

shinyApp(ui = ui, server = server)

