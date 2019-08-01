
function.header <- function() {
  dashboardHeader(title = "Naive Bayes")
}

function.sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Initialisation", tabName = "initialisation"),
      menuItem("Data Quality Config", tabName = "DQgeneral", startExpanded = TRUE,
               menuSubItem("DQ config", "dqconfig"),
               menuSubItem("Load DQ Files", "loaddqfiles"),
               menuSubItem("Create DQ Files","createdqfiles"),
               menuSubItem("Remove columns","removecolumns")
               ),
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
                      uiOutput("split"),
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
        

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° DQ config  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#

     tabItem(
        tabName = "dqconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            fluidPage(
              h1("DQ config"),
              tags$br(),
              box(
                title = "Choose a DQ option",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                tags$br(),
                uiOutput("fromDQconfigToLoad"),
                tags$br(),
                uiOutput("fromDQconfigToCreate"),
                tags$br(),
                uiOutput("fromDQconfigToMissing"),
                tags$br()
              ),
              tags$br(),
              box(
                title = "Why use DQ files ?",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                
                h4( "These files allow you to identify if a value is inconsistent or not*."),
                paste("*Missing values are always identify as inconsistent.")
              )
            )
            
            
          ),
          
          mainPanel(
            
              # tuto explicatif
            
            box(
              title = "What are the DQ files ?",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              
              h3(" Types : CSV file which allows to know what is the type of each column"),
              uiOutput("boxTypesExample"),
              
              tags$br(),
              h3(" Ranges : CSV file which allows to know what are the valable values of each column"),
              uiOutput("boxRangesExample")
              
            )
          )
        )
      ),


# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° DQ Load Files °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#
      tabItem(
        tabName = "loaddqfiles",
        
        sidebarLayout(
          
          sidebarPanel(
            fluidPage(
              h1("DQ Load Files"),
              
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
                    uiOutput("fromRangesToNextButton")
                    
                  )
                  
                )
                
              )
              
            )
          ),
          mainPanel(
            fluidPage(
              box(
                width = 12,
                    
                    tags$br(),
                    fluidRow(valueBoxOutput("matchTypes", width = 12))
                    ,
                    dataTableOutput("typesFile"),
                    tags$hr(),
                    fluidRow(valueBoxOutput("matchRanges", width = 12))
                    ,
                    dataTableOutput("rangesFile")
                    
                  
              )
            )
          )
        )
      ),

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° DQ Create Files °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#

      tabItem(
        tabName = "createdqfiles",
        
        sidebarLayout(
          
          sidebarPanel(
            
            uiOutput("fromCreateToNext")
            
          ),
          
          mainPanel(
            
            
            
          )
        )
        
        
      ),

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° DQ Remove columns  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#

      tabItem(
        tabName = "removecolumns",
        
        sidebarLayout(
          
          sidebarPanel(
            fluidPage(
              h1("DQ Remove columns"),
              
              tabsetPanel(
                id = "tabsetremove",
                
                tabPanel(
                  title = "Columns to remove",
                  value = "removeDQ",
                  tags$br(),
                  uiOutput("sliderDQ"),
                  uiOutput("tooMuchColRemoved"),
                  tags$br()
                )
              ),
              uiOutput("fromDQToNextButton")
            )
          ),
          
          mainPanel(
            
            uiOutput("boxBarchart")
            
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
          uiOutput("boxPlotCost"),
          uiOutput("boxPlotAccuracy"),
          uiOutput("boxPlotSensitivity"),
          uiOutput("boxPlotSpecificity"),
          tags$br(),
          uiOutput("boxresInitial"),
          uiOutput("boxresDQ"),
          uiOutput("boxresFixed")
          
        )
      )
    )
  )
}