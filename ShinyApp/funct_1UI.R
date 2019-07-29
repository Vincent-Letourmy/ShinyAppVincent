
function.header <- function() {
  dashboardHeader(title = "Naive Bayes")
}

function.sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Initialisation", tabName = "initialisation"),
      menuItem("Data Quality Config", tabName = "dqconfig"),
      menuItem("Naive Bayes Config",tabName = "naivebayesconfig"),
      menuItem("Costs Config", tabName = "costsconfig"),
      menuItem("Results", tabName = "results"),
      menuItem("Website", icon = icon("send",lib='glyphicon'), 
               href = "https://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29")
    )
  )
}

function.body <- function(){
  dashboardBody(
    tabItems(
      
#________________________________________________________ Initialisation _______________________________________________________________________________________#
      
      tabItem(
        tabName = "initialisation",
        
        sidebarLayout(
          sidebarPanel(
            h1("Initialisation"),
            
            tabsetPanel(
              id = "tabsetInitialisation",
              
              tabPanel(
                "Upload your file",
                value = "load",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfile"),
                      uiOutput("parametersbox"),
                      fluidRow(
                        column(6, uiOutput("uploadbutton")),
                        column(6, uiOutput("demobutton"))
                      ),
                      tags$br()
                  )
                ),
                uiOutput("fromLoadToNextTab")
              ),
              
              tabPanel(
                "Define NAs",
                value = "defineNas",
                tags$br(),
                fluidRow(
                    box(width = 12,
                        status = "primary",
                        title = "Define NAs",
                        solidHeader = TRUE,
                        uiOutput("checkBoxInterogation"),
                        uiOutput("checkBoxEmpty"),
                        uiOutput("checkBoxNa"),
                        uiOutput("confirmNAs")
                    ),
                    uiOutput("fromDefineToNextTab")
                )
              ),
              
              tabPanel(
                "Optional",
                value = "optional",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfileOptional"),
                      uiOutput("parametersboxOptional"),
                      fluidRow(
                        column(6, uiOutput("uploadbuttonOptional"))
                      ),
                      uiOutput("costFixingSelection")
                  ),
                  uiOutput("fromInitToNextButton"),
                  uiOutput("skipOptional")
                )
                
              )
            )
          ),
          mainPanel(
            box(
              width = 12,
              
              tabsetPanel(
                id = "tabsetinit",
                
                tabPanel(
                  "Database",
                  value = "database",
                  dataTableOutput("tabLoadedInitialisation")
                ),
                
                tabPanel(
                  "Optional",
                  value = "databaseFixed",
                  fluidRow(valueBoxOutput("matchFixing", width = 12)),
                  dataTableOutput("tabfixing")
                  
                )
                
              )
            )
          )
        )
      ),

      
#________________________________________________________ DataQuality Config _______________________________________________________________________________________#
        
      tabItem(
        tabName = "dqconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            fluidPage(
              h1("Data quality Config"),
              
              tabsetPanel(
                id = "tabsetDQ",
                
                tabPanel(
                  title = "Load DQ Files",
                  value = "loadDQ",
                  
                  fluidRow(
                    tags$br(),
                    box(width = 12,
                        uiOutput("selectionfileTypes"),
                        uiOutput("parametersboxTypes")
                    ),
                    box(width = 12,
                        uiOutput("selectionfileRanges"),
                        uiOutput("parametersboxRanges")
                    ),
                    uiOutput("typesrangesButton"),
                    uiOutput("typesrangesDemo"),
                    tags$br(),
                    uiOutput("fromLoadDQtoNextTab")
                    
                  )
                  
                ),
                
                tabPanel(
                  title = "Columns to remove",
                  value = "removeDQ",
                  tags$br(),
                  uiOutput("sliderDQ"),
                  uiOutput("tooMuchColRemoved"),
                  tags$br(),
                  
                  uiOutput("fromRangesToNextButton")
                )
              )
              
              
            )
          ),
          mainPanel(
            fluidPage(
              box(
                width = 12,
                
                tabsetPanel(
                  id = "tabsetMainDQ",
                  
                  tabPanel(
                    title = "Types/Ranges",
                    value = "typesranges",
                    
                    tags$br(),
                    fluidRow(valueBoxOutput("matchTypes", width = 12))
                    ,
                    dataTableOutput("typesFile"),
                    tags$hr(),
                    fluidRow(valueBoxOutput("matchRanges", width = 12))
                    ,
                    dataTableOutput("rangesFile")
                    
                  ),
                  
                  tabPanel(
                    title = "Selection of columns",
                    value = "selectcolumns",
                    
                    tags$br(),
                    uiOutput("boxBarchart")
                    
                  )
                )
              )
            )
          )
        )
        ),
  
  
  
#__________________________________________________________ Naive Bayes Config _________________________________________________________________________________________#
        
      tabItem(
        tabName = "naivebayesconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Naive Bayes Config"),
            
            tabsetPanel(
              id = "tabSetTarget",
              
              tabPanel(
                "Target",
                value = "column",
                tags$br(),
                fluidRow(
                  box(
                    width = 12,
                    uiOutput("selectcolumn"),
                    fluidRow(valueBoxOutput("noMV", width = 12)),
                    tags$hr(),
                    uiOutput("foldselection"),
                    uiOutput("fromTargetTonextTabButton")
                  )
                )
              ),
              
              tabPanel(
                "Remove other targets",
                value = "removecolumn",
                tags$br(),
                box(width = 12,
                    uiOutput("checkBoxOtherTargets"),
                    uiOutput("ValidCheckBox")
                ),
                uiOutput("fromTargetToNextButton")
              )
              
            )
          ),
          mainPanel(
            box(
              width = 12,
              dataTableOutput("tabLoadedTargetConfig")
            )
          )
        )
      ), 
        
        
#__________________________________________________________ Costs Config _________________________________________________________________________________________#
        
      tabItem(
        tabName = "costsconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Costs Config"),
            tags$br(),
            
            tabsetPanel(
              id = "tabsetcosts",
              
              tabPanel(
                "Prediction",
                value = "prediction",
                fluidRow(
                  box(width = 12,
                      helpText("Editable table : Choose costs and validate"),
                      rHandsontableOutput("costsTab"),
                      tags$br(),
                      uiOutput("validate"),
                      uiOutput("downloadCostsButton")
                  ),
                  tags$hr(),
                  uiOutput("fromCostsToNextButton")
                )
              )
            )
          ),
          
          mainPanel(
            box(
              width = 12,
              
              dataTableOutput("tabLoadedCostsConfig")
            )
          )
        )
      ),
        
#_____________________________________________________________ Results ___________________________________________________________________________________________#
        
      tabItem(
        tabName = "results",
        fluidPage(
          uiOutput("boxresInitial"),
          uiOutput("boxresDQ"),
          uiOutput("boxresFixed"),
          tags$br(),
          uiOutput("boxPlotCost"),
          uiOutput("boxPlotAccuracy"),
          uiOutput("boxPlotSensitivity"),
          uiOutput("boxPlotSpecificity")
        )
      )
    )
  )
}