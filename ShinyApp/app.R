
# app.R

library(shiny)
require(shinydashboard)
library(e1071) 
library(caret) 
library(dplyr)
library(plotly) 
library(rhandsontable) 
library(shinycssloaders)
library(pROC)

source("funct_0downloadFile.R")
source("funct_1UI.R")
source("funct_2reactivevalues.R")
source("funct_3initStep.R")
source("funct_4dataquality.R")
source("funct_5CVNaiveBayes.R")
source("funct_6loopResults.R")
source("funct_7fixing.R")


ui <- dashboardPage(title = 'ShinyAppVincent', function.header(), function.sidebar(), function.body(), skin='blue')


server <- function(input, output, session) {
    
    options(shiny.maxRequestSize=30*1024^2)
    
    v <- function_reactiveValues()
    
#________________________________________________________ Initialisation _____________________________________________________________________________________________________________________________________________#
    
    
# Upload file °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Selection file
    
    output$selectionfile <- renderUI({
        function.fileInput("fileCSV","")
    })
    
    
    ### Box parameters
    
    output$parametersbox <- function_parametersBox("header","sep","quote",FALSE)
    
    
    ### Upload File Button
    
    output$uploadbutton <- renderUI({
        actionButton("uploadbutton","Upload")
    })
    observeEvent(input$uploadbutton,{
        infile <- input$fileCSV
        if (is.null(infile)) return (NULL)
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile(infile$datapath, input$header , input$sep , input$quote)
    })
    
    
    ### Upload a demo button
    
    output$demobutton <- renderUI({
        actionButton("demobutton","Upload a Demo")
    })
    observeEvent(input$demobutton,{
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile("CSV/risk_factors_cervical_cancer_Original.csv", input$header ,"," , input$quote)
    })
    
    #output$split <- renderUI({
        #numericInput("split","Split the table ? (%)", 
         #            value = 100,min = 0,max = 100, 
         #            step = 1)
        
    #})
    
    
    ### Next tab button
    
    output$fromLoadToNextTab <- renderUI({
        if (is.null(v$dataframe_initialisation)) return (NULL)
        actionButton("fromLoadToNextTab", "Next")
    })
    observeEvent(input$fromLoadToNextTab, {
        
        #training.samples <- v$dataframe_initialisation %>%
            #caret::createDataPartition(p = input$split/100, list = FALSE)
        #v$dataframe_initialisationBis <- v$dataframe_initialisation <- v$dataframe_initialisation[c(training.samples), ]
        
        updateTabsetPanel(session, "tabsetInitialisation", "defineNas")
    })
    
    
    
# Define NAs  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Check boxes ?/""/NA
    
    output$checkBoxInterogation <- renderUI({
        checkboxInput("interrogation", "?")
    })
    
    output$checkBoxEmpty <- renderUI({
        checkboxInput("empty", "\" \"")
    })
    
    output$checkBoxNa <- renderUI({
        checkboxInput("na", "NA")
    })
    
    
    ### Validate parameters button
    
    output$confirmNAs <- renderUI({
        actionButton("confirmNAs", "OK")
    })
    observeEvent(input$confirmNAs, {
        v$dataframe_initialisation <- v$dataframe_initialisationBis
        if (input$interrogation){
            for (col in names(v$dataframe_initialisation )) {
                column <- as.character(v$dataframe_initialisation[,col])
                v$dataframe_initialisation[,col] <-  ifelse(column == "?", "", column)
            }
        }
        if (input$na){
            for (col in names(v$dataframe_initialisation )) {
                column <- as.character(v$dataframe_initialisation[,col])
                v$dataframe_initialisation[,col] <-  ifelse(is.na(column), "", column)
            }
        }
        
    })
    
    
    
    ### Next tab button
    
    output$fromDefineToNextTab <- renderUI({
        actionButton("fromDefineToNextTab", "Next")
    })
    observeEvent(input$fromDefineToNextTab, {
        updateTabsetPanel(session, "tabsetInitialisation", "optional")
    })
    
    
# Upload file Optional °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Selection file Optional
    
    output$selectionfileOptional <- renderUI({
        function.fileInput("fileCSVOptional", "Fixed")
    })
    
    
    ### Box parameters
    
    output$parametersboxOptional <- function_parametersBox("headerOptional","sepOptional","quoteOptional",FALSE)
    
    
    ### Upload File Button Optional
    
    output$uploadbuttonOptional <- renderUI({
        actionButton("uploadbuttonOptional","Upload")
    })
    observeEvent(input$uploadbuttonOptional,{
        infile <- input$fileCSVOptional
        if (is.null(infile)) return (NULL)
        v$dataframe_fixing <- function.loadFile(infile$datapath, input$headerOptional , input$sepOptional , input$quoteOptional)
        updateTabsetPanel(session, "tabsetinit", "databaseFixed")
    })
    
    
    
    ### Selection Cost Fixing
    
    output$costFixingSelection <- renderUI({
        numericInput("costFixingSelection", label = "Enter total cost of fixing values ", value = 1000,min = 0,max = 100000,step = 10)
    })
    
    
    
    
# DataBase initial
    
    output$tabLoadedInitialisation <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,
                       pageLength = 14,
                       lengthChange = FALSE,
                       searching = FALSE,
                       info = FALSE)
    )
    
    
    
# Match of fixing d
    
    output$matchFixing <- renderValueBox({
        function.matching(v$dataframe_initialisation,v$dataframe_fixing, "Fixed database")
    })
    
    
# Database fixing
    
    output$tabfixing <- renderDataTable(
        v$dataframe_fixing,
        options = list(scrollX = TRUE,pageLength = 14, lengthChange = FALSE, searching = FALSE, info = FALSE)
    )
    
    
    
# Skip button °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$skipOptional <- renderUI({
        if (!is.null(v$dataframe_fixing)) return (NULL)
        actionButton("skipOptional","SKIP")
    })
    observeEvent(input$skipOptional,{
        updateTabItems(session,"sidebarmenu", "dqconfig")
    })
    
    
# Next Panel button °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$fromInitToNextButton <- renderUI({
        if (is.null(v$dataframe_fixing)) return (NULL)
        actionButton("fromInitToNextButton","Next step")
    })
    observeEvent(input$fromInitToNextButton,{
        updateTabItems(session,"sidebarmenu", "dqconfig")
    })
    
    
    
#_______________________________________________________ DQ Config GENERAL __________________________________________________________________________________________________________________________________________#
    
    
# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° DQ config  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#
    
# Load BUTTON
    
    output$fromDQconfigToLoad <- renderUI({
        actionButton("fromDQconfigToLoad","LOAD DQ FILES")
    })
    observeEvent(input$fromDQconfigToLoad, {
        updateTabItems(session, "sidebarmenu", "loaddqfiles")
    })
    

    
# Missing values only BUTTON
    
    output$fromDQconfigToMissing <- renderUI({
        actionButton("fromDQconfigToMissing","MISSING VALUES ONLY")
    })
    observeEvent(input$fromDQconfigToMissing, {
        v$matrixBool <- function.matrixBooleanMissingValues(v$dataframe_initialisation)
        updateTabItems(session, "sidebarmenu", "removecolumns")
    })
    
    
    
### Main panel 
    
# Examples
    
    output$typesExample <- renderDataTable(
        function.loadFile("CSV_copie/TypesDataCopie.csv", header = TRUE, sep = ";", quote = ""),
        options = list(scrollX = TRUE,paging = FALSE, searching = FALSE, info = FALSE)
    )
    output$boxTypesExample <- renderUI({
        box(title = "Types example",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("typesExample"))
    })
    
    output$rangesExample <- renderDataTable(
        function.loadFile("CSV_copie/RangesDataCopie.csv", header = TRUE, sep = ";", quote = ""),
        options = list(scrollX = TRUE,paging = FALSE, searching = FALSE, info = FALSE)
    )
    output$boxRangesExample <- renderUI({
        box(title = "Ranges example",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("rangesExample"))
    })
    
    
# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° DQ Load Files °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#
    
    
### SIDE BAR PANEL

# Upload Types File *******************************************************************************************************************************************************
    
    
    ### TYPES selection file
    
    output$selectionfileTypes <- renderUI({
        function.fileInput("fileCSVTypes", "Types")
    })
    
    
    ### Box Types
    
    output$parametersboxTypes <- function_parametersBox("headerTypes","sepTypes","quoteTypes",TRUE)
    
    
# Upload Ranges File *******************************************************************************************************************************************************
    
    ### RANGES selection file
    
    output$selectionfileRanges <- renderUI({
        function.fileInput("fileCSVRanges","Ranges")
    })
    
    
    ### Box ranges
    
    output$parametersboxRanges <- function_parametersBox("headerRanges","sepRanges","quoteRanges",TRUE)
    
    
    
# TYPES/RANGES upload file *******************************************************************************************************************************************************
    
    output$typesrangesButton <- renderUI({
        infileRanges <- input$fileCSVRanges
        infileTypes <- input$fileCSVTypes
        if (is.null(infileRanges) || is.null(infileTypes)) return (NULL)
        actionButton("typesrangesButton", "Upload Types/Ranges")
    })
    observeEvent(input$typesrangesButton,{
        infileRanges <- input$fileCSVRanges
        infileTypes <- input$fileCSVTypes
        if (is.null(infileRanges) || is.null(infileTypes)) return (NULL)
        v$df_types <- function.loadFile(infileTypes$datapath, input$headerTypes , input$sepTypes , input$quoteTypes)
        v$df_ranges <- function.loadFile(infileRanges$datapath, input$headerRanges , input$sepRanges , input$quoteRanges)
    })
    
    
    output$typesrangesDemo <- renderUI({
        infileRanges <- input$fileCSVRanges
        infileTypes <- input$fileCSVTypes
        if (! is.null(infileRanges) && ! is.null(infileTypes)) return (NULL)
        actionButton("typesrangesDemo", "Demo")
    })
    observeEvent(input$typesrangesDemo,{
        v$df_types <- function.loadFile("CSV/TypesDataOriginal.csv", input$headerTypes , input$sepTypes , input$quoteTypes)
        v$df_ranges <- function.loadFile("CSV/RangesDataOriginal.csv", input$headerRanges , input$sepRanges , input$quoteRanges)
    })
    
    
# Matches
    
    output$matchTypes <- renderValueBox({
        function.matching(v$dataframe_initialisation,v$df_types, "Types")
    })
    
    output$matchRanges <- renderValueBox({
        function.matching(v$dataframe_initialisation,v$df_ranges, "Ranges")
    })
    
    
    
    
### MAIN PANEL
    
# Types and Ranges tables *******************************************************************************************************************************************************
    
    output$typesFile <- renderDataTable(
        v$df_types,
        options = list(scrollX = TRUE,paging = FALSE, searching = FALSE, info = FALSE)
    )
    
    output$rangesFile <- renderDataTable(
        v$df_ranges,
        options = list(scrollX = TRUE,paging = FALSE, searching = FALSE, info = FALSE)
    )
    
    
    
    
# TYPES/RANGES next panel button *******************************************************************************************************************************************************
    
    output$fromRangesToNextButton <- renderUI({
        if (is.null(v$df_types) || is.null(v$df_ranges)) return(NULL)
        actionButton("fromRangesToNextButton","Next")
    })
    observeEvent(input$fromRangesToNextButton,{
        
        # Matrix boolean Consistencies values
        v$matrixBool <- function.matrixBooleanConsistency(v$dataframe_initialisation, v$df_types, v$df_ranges)
        
        updateTabItems(session, "sidebarmenu", "removecolumns")
    })
    
    
    
    
# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° DQ Create Files °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#

    
# NEXT BUTTON
    
    output$fromCreateToNext <- renderUI({
        actionButton("fromCreateToNext","NEXT")
    })
    observeEvent(input$fromCreateToNext, {
        updateTabItems(session, "sidebarmenu", "removecolumns")
    })
    
    
    
# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° DQ Remove columns  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#
    
    
# Slider DQ
    
    output$sliderDQ <- renderUI({
        sliderInput(
            inputId = "sliderDQ",
            label = "Choose columns you want to remove",
            min = 0, max = 100,
            value = c(0,100)
        )
    })
    
    
# Check if too much columns are removed
    
    output$tooMuchColRemoved <- renderUI({
        if (is.null(v$dataframe_initialisation) || is.null(v$tabColumnToRemove)) return(NULL)
        valueBoxOutput("valueBoxColRemoved", width = 12)
    })
    
    output$valueBoxColRemoved <- renderValueBox({
        colTotal <- ncol(v$dataframe_initialisation)
        colRemoved <- length(v$tabColumnToRemove)
        value <- paste(colRemoved, "/",colTotal)
        
        if (colTotal - colRemoved < 2) {
            v$tooMuchColRemoved <- TRUE
            valueBox(value = value, 
                     subtitle = paste("Too much columns removed"), 
                     icon = icon("thumbs-down",lib='font-awesome'), 
                     color = "red")
        }
        else {
            v$tooMuchColRemoved <- FALSE
            valueBox(value = value, 
                     subtitle = paste("Columns which will be removed"), 
                     icon = icon("thumbs-up",lib='font-awesome'), 
                     color = "green")
        }
    })
    
    
    
# Bar chart remove columns
    
    output$barchartRemoveCol <- renderPlotly({
        
        min <- input$sliderDQ[1]
        max <- input$sliderDQ[2]
        if (is.null(min)) return(NULL)
        
        res <- sort(function.barChartInconsistency(v$matrixBool), decreasing = TRUE)
        col_names <- names(res)
        colMin <- names(which(res > min)) 
        colMax <- names(which(res <= max))
        columnToRemove <- intersect(colMin,colMax)
        v$tabColumnToRemove <- res[columnToRemove]
        
        plot_ly(x = factor(col_names, levels = col_names), 
                y = res, 
                type = "bar",
                color = col_names%in%columnToRemove , colors = c("#132B43","#132B43","#56B1F7")
        )
        
        
    })
    
    output$boxBarchart <- renderUI({
        box(
            title = "Inconsistency bar chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput("barchartRemoveCol"),
            tags$br(),
            h4(" x  :  Column's name which at least one inconsistency"),
            h4(" y  :  Pourcentage of inconsistencies")
            
        )
    })
    
    
# DQ next panel button  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$fromDQToNextButton <- renderUI({
        actionButton("fromDQToNextButton","Next")
    })
    observeEvent(input$fromDQToNextButton,{  
        if (v$tooMuchColRemoved) return(NULL)
        updateTabItems(session, "sidebarmenu", "naivebayesconfig")
    })
    
    
    
    
    
#________________________________________________________ Naive Bayes Config _________________________________________________________________________________________________________________________________________#
    
    
# Choice of naive bayes parameters (target and fold) °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Selection of target
    
    output$selectcolumn <- renderUI(
        function.selectionColumn(v$dataframe_initialisation)
    )
    observeEvent(input$selectcolumn,{
        v$columnSelected <- input$selectcolumn
    })
    
    ### Check column
    
    output$noMV <- renderValueBox({
        col <- v$dataframe_initialisation[,v$columnSelected]
        t <- FALSE
        for (val in col) {
            if (is.na(val) || val == "") t <- TRUE
        }
        if (t) valueBox(value = v$columnSelected , subtitle = "Inconsistencies are detected", icon = icon("thumbs-down",lib='font-awesome'), color = "red",width = "4000px")
        else valueBox(value = v$columnSelected, subtitle = "No inconsistencies", icon = icon("thumbs-up",lib='font-awesome'), color = "green",width = "4000px")

    })
    
    
    ### Selection of fold for Naive Bayes
    
    output$foldselection <- renderUI({
        sliderInput("foldselection","Number of fold for Cross Validation", 1,50,10)
    })
    
    
    ### Next tab
    
    output$fromTargetTonextTabButton <- renderUI({
        actionButton("fromTargetTonextTabButton","Next")
    })
    observeEvent(input$fromTargetTonextTabButton,{
        updateTabsetPanel(session, "tabSetTarget", "removecolumn")
    })
    
    
    
# Remove other targets °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Selection of other targets for removing
    
    output$checkBoxOtherTargets <- renderUI({ 
        
        v$dataframe_withoutcolselected <- v$dataframe_initialisation[,!names(v$dataframe_initialisation)%in%v$columnSelected]
        newList <- rev(names(v$dataframe_withoutcolselected))
        checkboxGroupInput("targets",
                           label = "Select target(s)",
                           choices = newList)
    })
    
    
    ### Remove other targets button
    
    output$ValidCheckBox <- renderUI({
        actionButton("OK","Remove")
    })
    observeEvent(input$OK,{
        if (!is.null(input$targets)){
            
            list <- data.frame(Column = input$targets)
            v$dataframe_initialisation <- v$dataframe_initialisation[,!names(v$dataframe_initialisation)%in%list$Column]
            v$dataframe_fixing <- v$dataframe_fixing[,!names(v$dataframe_fixing)%in%list$Column]
        }
    })
    
    
# DataBase Naive Bayes config °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$tabLoadedTargetConfig <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,pageLength = 14, lengthChange = FALSE, searching = FALSE, info = FALSE)
    )
    
    
    
# Next Panel button °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$fromTargetToNextButton <- renderUI({
        actionButton("fromTargetToNextButton","Next Step")
    })
    observeEvent(input$fromTargetToNextButton,{
        
        v$dataframe_initialisation <- function.as_factor(v$dataframe_initialisation)
        v$tabCosts <- function.tabNaiveBayes(v$dataframe_initialisation, v$columnSelected)
        updateTabItems(session,"sidebarmenu", "costsconfig")
    })
    
   
#_______________________________________________________ Costs Config __________________________________________________________________________________________________________________________________________#
    
    
# Creation costs tab °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Costs tab editable
    
    output$costsTab <- renderRHandsontable({
        rhandsontable(v$tabCosts)
    })
    
    
    
    ### Validate costs button
    
    output$validate <- renderUI(
        actionButton("validate","Validate"),
    )
    observeEvent(input$validate,{
        v$tabCosts <- function.saveDataInFile(input$costsTab, "MyData.csv")
        v$validate <- TRUE
    })
    
    
    ### Download costs button
    
    output$downloadCostsButton <- renderUI({
        if (v$validate == FALSE) return(NULL)
        downloadButton('downloadData', 'Download Costs Tab')
    })
    output$downloadData <- function.downloadFile(v$tabCosts)
    
    
# Database cost °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$tabLoadedCostsConfig <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,pageLength = 14, lengthChange = FALSE, searching = FALSE, info = FALSE)
    )
    
    
    
# Naive Bayes LOOP °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Next panel button
    
    output$fromCostsToNextButton <- renderUI({
        if (v$validate == FALSE) return (NULL)
        actionButton("fromCostsToNextButton","Results")
    })
    observeEvent(input$fromCostsToNextButton,{
        
        #As factor to run naive Bayes
        if (! is.null(v$dataframe_fixing) ) v$dataframe_fixing <- function.as_factor(v$dataframe_fixing)
        v$matrixBool <- v$matrixBool[,names(v$dataframe_initialisation)]
        
        updateTabItems(session,"sidebarmenu", "results")
        
        
    })
    
    
#________________________________________________________ Results  ____________________________________________________________________________________________#
    

### Results
    
# LOOP °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$resultsDQOnlyCol <- renderTable ({
        v$resDQOnlyCol <- function.loopResultsDQ(
            v$dataframe_initialisation
            ,v$matrixBool
            ,v$tabCosts
            ,v$columnSelected
            ,input$foldselection
            ,v$tabColumnToRemove
            ,FALSE)
    },
    rownames = TRUE,
    striped = TRUE,
    hover = TRUE
    )
    
    output$boxresDQOnlyCol <- renderUI({
        function.boxresTab("Results - Removing only Columns (Blue line)","resultsDQOnlyCol")
    })
    
    output$resultsDQ <- renderTable ({
            v$resDQ <- function.loopResultsDQ(
                v$dataframe_initialisation
                ,v$matrixBool
                ,v$tabCosts
                ,v$columnSelected
                ,input$foldselection
                ,v$tabColumnToRemove
                ,TRUE)
    },
    rownames = TRUE,
    striped = TRUE,
    hover = TRUE
    )
    
    output$boxresDQ <- renderUI({
        function.boxresTab("Results - Removing Columns and Rows (Orange line)","resultsDQ")
    })
    
    output$resultsFixed <- renderTable ({
        if (is.null(v$dataframe_fixing)) return(NULL)
        v$resFixed <- function.uniqueResults( "Database - Fixed"
            ,v$dataframe_fixing
            ,v$tabCosts
            ,v$columnSelected
            ,input$foldselection
            ,input$costFixingSelection)
    },
    rownames = TRUE,
    striped = TRUE,
    hover = TRUE
    )
    
    output$boxresFixed <- renderUI({
        if (is.null(v$dataframe_fixing)) return(NULL)
        function.boxresTab("Results - Fixed Database","resultsFixed")
    })
    
    
# Line charts °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$costRes <- function.outputLineChart(
        
        v$resultsTabOC <- rbind(v$resDQOnlyCol[,c("Accuracy (%)","Sensitivity (%)","Specificity (%)","AUC (%)","Cost (per patient)")],
                                v$resFixed[,c("Accuracy (%)","Sensitivity (%)","Specificity (%)","AUC (%)","Cost (per patient)")]
                                ),
        
        v$resultsTab <- rbind(v$resDQ[,c("Accuracy (%)","Sensitivity (%)","Specificity (%)","AUC (%)","Cost (per patient)")],
                              v$resFixed[,c("Accuracy (%)","Sensitivity (%)","Specificity (%)","AUC (%)","Cost (per patient)")]
                              ),
                                               "Cost (per patient)", "Cost")
    output$boxPlotCost <- function.resLineChart("Cost", "danger", "costRes",12 )
    
    output$accRes <- function.outputLineChart(v$resultsTabOC, v$resultsTab, "Accuracy (%)", "Pourcentage %")
    output$boxPlotAccuracy <- function.resLineChart("Accuracy", "info", "accRes",6)
    
    output$sensRes <- function.outputLineChart(v$resultsTabOC, v$resultsTab, "Sensitivity (%)", "Pourcentage %")
    output$boxPlotSensitivity <- function.resLineChart("Sensitivity", "info", "sensRes",6)
        
    output$speRes <- function.outputLineChart(v$resultsTabOC, v$resultsTab, "Specificity (%)", "Pourcentage %")
    output$boxPlotSpecificity <- function.resLineChart("Specificity", "info", "speRes",6)
    
    output$aucRes <- function.outputLineChart(v$resultsTabOC, v$resultsTab, "AUC (%)", "Pourcentage %")
    output$boxPlotAUC <- function.resLineChart("AUC", "info", "aucRes",6)
        
}

# Run the application 
shinyApp(ui = ui, server = server)
