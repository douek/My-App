#--------clean enviroment-------------
rm(list=ls(all.names=TRUE))

#---------------functions-------------
#--------- packeges required,if miss one, please install first
loadLibraries <- function(){
    library(shiny)
    library(shinyFiles)
    library(shinyjs)
    library(ggplot2)
    library(readxl)
    library(dplyr)
    library(shinydashboard)
    library(VIM)
    library(DT)
    library(data.table)
    library(shinyalert)
    library(tidyverse)
    
    options(shiny.maxRequestSize=3000*1024^2) 
}

completeNAvalues <- function(dataset, method){
    DT <- dataset
    for (j in seq_len(ncol(DT))) {
        if(is.numeric(DT[[j]])){
            switch (method,
                    "0" = set(DT,which(is.na(DT[[j]])),j,0),
                    "-9999" = set(DT,which(is.na(DT[[j]])),j,-9999),
                    "mean" = set(DT,which(is.na(DT[[j]])),j, mean(DT[[j]], na.rm = TRUE)),
                    "mad" = set(DT,which(is.na(DT[[j]])),j,median(DT[[j]], na.rm = TRUE))
            )
        }
        else{
            set(DT,which(is.na(DT[[j]])),j,"Missing Values")
        } 
    }
    return(DT)
}

feature_transformation <- function(dataset,feature,action){
    newFeatureName <- paste(action, feature, sep = "_")
    switch(action, 
           "log"={# standardized log 
               dataset[, (newFeatureName) := log(dataset[[feature]] + min(dataset[[feature]]) + 1)]
           },
           "square"={#square
               dataset[, (newFeatureName) := ((dataset[[feature]])^2) ] 
           },
           "rootSquare"={ #root square
               dataset[, (newFeatureName) := sqrt(dataset[[feature]])]
           }
    )
    ans <- as.data.table(dataset[[newFeatureName]])
    colnames(ans) <- c(newFeatureName)
    return(dataset)
}

variableType <- function(data, feature){
    #if(is.factor(data[[feature]])) return('factor')
    if(is.integer(data[[feature]])) {return('int')}
    else {
        if(is.character(data[[feature]]) || is.facet(data[[feature]])){
            return('factor')
        }
        else {
            return('double')
        }
    }
}

#-------run this line to load the libraries--------
loadLibraries()

#------------UI----------------
ui <- fluidPage(
    useShinyjs(),
    useShinyalert(),
    # Application title
    titlePanel("Exploratory Data Analysis"),
    # page ui
    tabsetPanel(id = "myApp",
                #----------------------Upload data
                tabPanel("Upload data", 
                         fluidPage(
                             fluidRow(
                                 # Sidebar with a file input 
                                 sidebarLayout(
                                     sidebarPanel( wellPanel(
                                         fileInput("filePath", "Choose CSV File",
                                                   multiple = FALSE,
                                                   placeholder = "No file selected",
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv"),
                                                   width = "80%"),
                                         tags$hr(),
                                         checkboxInput("header", "Header", TRUE)
                                     )),
                                     # Show a table with all the data
                                     mainPanel(
                                         DT::dataTableOutput("allDataTable")
                                     )
                                 )# sidebarlayout
                             ),
                             hr(),
                             h4("Data Shape"),
                             verbatimTextOutput("dataShape"),
                             h4("Features"),
                             verbatimTextOutput("dataColumns"),
                             h4("Data types"),
                             verbatimTextOutput("dataSTR"),
                             h4("Variables summary"),
                             verbatimTextOutput("dataSummary"),
                             div(style="display:inline-block; font-size: 150%;",actionButton("next1", "Continue to Missing Values examination"), style="float:right")
                         )#fluid page
                ), #tabset panel upload data
                type = "tabs"), #all tabsets panel
    fluidRow(
        tags$footer(id="adVisu",
                    'Ayelet Davidi, EDA Application')
    )#footer
)

#-----------server function------------
server <- function(input, output, session){
    options(shiny.maxRequestSize=3000*1024^2) 
    values <- reactiveValues(loadFlag=TRUE, nextB1=FALSE, varsInModel=list())
    
    #------------upload data
    read_batch_with_progress <- function(file_path,nrows,no_batches){
        progress = Progress$new(session, min = 1,max = no_batches)
        progress$set(message = "Reading ...")
        seq_length = ceiling(seq.int(from = 2, to = nrows-2,length.out = no_batches+1))
        seq_length = seq_length[-length(seq_length)]
        values$csFlag <- FALSE
        #read the first line
        df = read.csv(file_path,skip = 0,nrows = 1)
        col_names = colnames(df)
        
        for(i in seq_along(seq_length)){
            progress$set(value = i)
            if(i == no_batches) chunk_size = -1 else chunk_size = seq_length[i+1] - seq_length[i]
            
            df_temp = fread(file_path, skip = seq_length[i], nrows = chunk_size,header = FALSE,stringsAsFactors = TRUE)
            colnames(df_temp) = col_names
            df = rbind(df,df_temp)
        }
        
        progress$close()
        return(setDT(df))
    }
    
    getData <- reactive({
        if ((is.null(input$filePath))) return(NULL)
        values$nextB1 <- TRUE
        n_rows = length(count.fields(input$filePath$datapath))
        out <- read_batch_with_progress(input$filePath$datapath,n_rows,15)
        out[out=='NA'] <- NA
        return(out)
    })
    
    observe({
        values$data <- getData()
    })
    
    output$allDataTable <- DT::renderDataTable({
        DT::datatable(
            values$data, options = list(
                lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),
                pageLength = 10, 
                rownames=FALSE  
            )
        )
    })
    
    output$dataSummary <- renderPrint({
        req(input$filePath)
        summary(values$data)
    })
    
    output$dataSTR <- renderPrint({
        req(input$filePath)
        str(values$data)
    })
    
    output$dataShape <- renderPrint({
        req(input$filePath)
        paste(nrow(values$data), ",", ncol(values$data))
    })
    
    output$dataColumns <- renderPrint({
        req(input$filePath)
        colnames(values$data)
    })
    
    observe({
        shinyjs::toggle(id = "next1", condition = values$nextB1)
    })
    
    #-----inset tab # 2 - missing valuse
    observeEvent(input$next1, {
        if(is.null(input$omitNA)){
            appendTab(inputId = "myApp", tab = tabPanel("Missing Values", fluidPage(
                h2("Visualization"),
                fluidRow(
                    column(10, offset = 2,
                           h2("Proprtion of missing values: "),
                           p("while blue is for non missing values"),
                           plotOutput("missingValues")) 
                ),
                hr(),
                h2("Treatment"),
                fluidRow(
                    tabsetPanel(id = "missingValuesTreatment",
                                tabPanel("Omit", fluidPage(
                                    h5("Omit all rows in dataset with missing values"),
                                    flowLayout(
                                        actionButton("omitNA", "Execute!"),
                                        actionButton("resetOmitNA", "Undo")
                                    ),
                                    br(),
                                    verbatimTextOutput("omitText1")
                                )),
                                tabPanel("Assign Specific Value", fluidPage(
                                    h5("Choose a specific method / value to assign in missing values"),
                                    h6("Only numeric features will be assigned with your choice while categorial features will get 'Missing Value' value."),
                                    radioButtons("TypeToFillNA", " ", 
                                                 choices = c("0", "-9999","mean", "median")),
                                    flowLayout(
                                        actionButton("specificValueGo", "GO"),
                                        actionButton("resetSpecificValueGo", "Undo")
                                    ),
                                    br(),
                                    verbatimTextOutput("specificValueNAText1")
                                )),
                                # tabPanel("Mice", fluidPage(
                                #   toodo
                                # )),  
                                type = "tabs")#, style = "height: 420px"
                ),
                div(style="display:inline-block; font-size: 150%;",actionButton("next2", "Continue to Choose Variable"), style="float:right")
            )) 
            )#append tab
        }
        updateTabsetPanel(session, "myApp", selected = "Missing Values")
    }) #observed event for creating second tab
    
    #--------------missing values---------------
    values <- reactiveValues(omitFlag = FALSE, csFlag = FALSE, omit="", csResetFlag=FALSE, svText= "")
    
    output$missingValues <- renderPlot({
        req(input$filePath)
        aggr(values$data,prop = T, numbers = T)
    })
    
    #-------------------------------------------omit-----------------------------------------------
    completedData <- reactive({
        req(input$filePath)
        
        new_DF <- na.omit(values$data)
        return(new_DF)
    })
    
    NAdata <- reactive({
        req(input$filePath)
        
        new_DF <- values$data[rowSums(is.na(values$data)) > 0,]
        return(new_DF)    
    })
    
    observe({
        req(input$filePath)
        values$completedData <- completedData()
        values$NAdata <- NAdata()
        if(is.null(NAdata())){
            values$omit <- "Your data is completed! :)"
            values$svText <- "Your data is completed! :)"
            values$ready <- TRUE
        } else {
            values$omit <- "Your data contains missing values.."
            values$svText <- "Your data contains missing values.."
            values$ready <- FALSE
        }
    })

    observeEvent(input$omitNA, {
        req(input$filePath)
        if(!values$ready){
            if(values$csFlag){
                values$omit <- "You allready completed your data. No NA in the data.."
            } else {
                values$omit <- "All rows with missing values omitted sucssesfuly"
                values$omitFlag <- TRUE
            }
        } else{
            values$omit <- "You don't have missing values, remember? ;"
        }
    })
    
    observeEvent(input$resetOmitNA, {
        req(input$filePath)
        if(values$omitFlag){
            values$omit <- "Undo action, current data is the origin dataset"
            values$omitFlag <- FALSE
        }
    })
    
    output$omitText1 <- renderPrint({
        req(input$filePath)
        values$omit
    })
  
    #-------------------------------------------Specific Value completion-----------
    observeEvent(input$specificValueGo, {
        req(input$filePath)
        if(!values$ready){
            if(values$omitFlag){
                values$svText <- "You allready ommited your missing data. No NA in the data.."
            } else {
                values$svText <- paste("All rows with missing values omitted sucssesfuly with", input$TypeToFillNA)
                values$csFlag <- TRUE
            }
        } else{
            values$svText <- "You don't have missing values, remember? ;"
        }
    })
    
    observeEvent(input$resetSpecificValueGo, {
        req(input$filePath)
        if(values$csFlag){
            values$svText <- "Undo action, current data is the origin dataset"
            values$csFlag <- FALSE
        }
    })
    
    output$specificValueNAText1 <- renderPrint({
        req(input$filePath)
        values$svText
    })
    
    #---------- dataset after missing values -------------
    df <- reactive({
        if(values$ready){
            out <- values$data
        }else{ # 3 possible case: 
            if(!values$omitFlag){
                if(!values$csFlag){ # The user didn't act at any method with the missing values
                    out <- values$data
                }else { # the user completed missing values
                    data <- rbindlist(list(values$completedData, values$NAdata)) 
                    out <- completeNAvalues(data, input$TypeToFillNA)
                }
            } else { # The user ommited rows with missing values
                out <- values$completedData
            }
        }
        return(setDT(out))
    })
    
    #-----inset tab # 3 - Target Variable
    observeEvent(input$next2, {
        if(is.null(input$yVar)){
            appendTab(inputId = "myApp", tab = tabPanel("Variables", fluidPage(
                tabsetPanel(id = "varsPanel",
                            tabPanel("Target Variable", fluidPage(
                                fluidRow(
                                    column(6, offset=4, h2("Target / Y variable"))
                                ),
                                fluidRow(
                                    flowLayout(
                                        varSelectInput("yVar", "Select Y variable: ", df(), multiple = FALSE,
                                                       selectize = TRUE, width = "300px"),
                                        fluidRow(
                                            actionButton("plotYDist", "Plot"),
                                            checkboxInput("isYaFactor", "Check if factor")
                                        ),
                                        fluidRow(
                                            actionButton("selectY", "Choose"),
                                            br(),
                                            checkboxInput("selectYasFactor", "Choose as Categorial"),
                                            span('check this to add variable as factor in',
                                                 'case of numeric variable'),
                                            
                                        ),
                                        fluidRow(
                                            actionButton("resetY", "Reset"),
                                            br(),
                                            p("Rest button reset the hole model")
                                        )
                                    )),
                                fluidRow(
                                    column(5, offset = 3, wellPanel(
                                        plotOutput("yPlot", width = "100%")))
                                )),
                            ),#tabset panel Target Variable
                            tabPanel("Feature Selection and Engineering",fluidPage(
                                wellPanel(
                                    h2("The Model:"), 
                                    # wellPanel(
                                    fluidRow(
                                        column(11, verbatimTextOutput("modelFormula")),
                                        column(1, actionButton("next3", "Continue"))
                                    )
                                ), 
                                hr(),
                                h2("Select Features"),
                                fluidRow(
                                    column(3, 
                                           flowLayout(
                                               selectInput("xVar", "Feature: ", choices = c("None"), width = "300px"),
                                               checkboxInput("isXaFactor", "Check if factor")),
                                           actionButton("examineX", "Examine"),
                                           tags$br(),
                                           p(""),
                                           checkboxInput(inputId = "transX", label = "Check to examine the variable's transformation", FALSE),
                                           radioButtons("transforma", "Choose Transformation", 
                                                        choices = c(Log="log", Square="square", Root.Square="rootSquare")),
                                           
                                           actionButton("examineTransX", "Examine"),
                                           br(),
                                           flowLayout(
                                               actionButton("AddXvar", "Add Variable"),
                                               actionButton("addTxVar", "Add Transofmed"),
                                               actionButton("undoXadd", "Undo")
                                           )
                                    ),
                                    column(9,
                                           verbatimTextOutput("outliersText"),
                                           fluidRow(
                                               column(6,
                                                      wellPanel(
                                                          h4("Distribution Examination"),
                                                          plotOutput("xPlot", width = "100%"))),
                                               column(6, 
                                                      wellPanel(
                                                          h4("Outliers Examination"),
                                                          #verbatimTextOutput("outliersText"),
                                                          plotOutput("outliersPlot", width = "100%"))
                                               )
                                           ),
                                           hr(),
                                           wellPanel(
                                               plotOutput("transXplot", width = "100%"))
                                           # ))
                                    )
                                )
                            )),#tabset features
                            type = "tabs")#tabset panel vars
            )))
        }
        updateTabsetPanel(session, "myApp", selected = "Variables")
    }) #observed event for creating second tab
    #----------------Target variable ----------------
    observeEvent(input$selectY, {
        shinyjs::disable("yVar")
        values$formula <- paste(input$yVar, " ~ ")
        values$isYselected <- TRUE
        updateTabsetPanel(session, "varsPanel", selected = "Feature Selection and Engineering")
    })
    
    observeEvent(input$ok, {
        # Check that data object exists and is data frame.
        if (!is.null(df())) {
            isolate({
                df()[[input$yVar]] <- factor(df()[[input$yVar]])
            })
            removeModal()
        } else {
            showModal(yModal(failed = TRUE))
        }
    })
    
    observeEvent(input$resetY, {
        shinyjs::enable("yVar")
        values$formula <- paste("")
        values$isYselected <- FALSE
        values$doYplot <- FALSE
        values$doXplot <- FALSE
        values$doTXplot <- FALSE
        v$examineX <- FALSE
        values$varsInModel <- list()
    })
    
    observeEvent(input$plotYDist, {
        # 0 will be coerced to FALSE
        # 1+ will be coerced to TRUE
        values$doYplot <- input$plotYDist
    })
    
    output$yPlot <- renderPlot({
        req(input$plotYDist)
        if (values$doYplot == FALSE) return()
        isolate({
            if((!input$isYaFactor) & (is.numeric(df()[[input$yVar]]))){
                hist(df()[[input$yVar]], main = paste(input$yVar),  xlab = paste(input$yVar), ylab = 'Frequency',col="lightblue" , freq=FALSE)
                lines(density(df()[[input$yVar]], na.rm = TRUE),col="red",lwd=2)
            } else {
                barplot(prop.table(table(df()[[input$yVar]])), main = input$yVar)
            }
        })
    })
    
    #------------------feature selection & engineering --------------
    #------- Model Formula    
    modelFormula <- reactive({
        req(input$selectY)
        if (values$isYselected){
            formula <-  paste(input$yVar, " ~ ")
            if(length(values$varsInModel) > 0){#there are X's in the model
                for (i in c(1:length(values$varsInModel))){
                    if (i < length(values$varsInModel)){
                        formula <- paste(formula, values$varsInModel[i], " + ")
                    } else {
                        formula <- paste(formula, values$varsInModel[i])
                    }
                }
            } 
        } else {
            formula <- ""
        }
        return(formula)
    })
    
    output$modelFormula <- renderPrint({
        if (values$isYselected){
            modelFormula()
        }
    })
    
    #----variable's plots
    v <- reactiveValues(examineX = FALSE, examineTransformation = FALSE, newFeature = "none", ok3 = FALSE)
    
    observeEvent(input$examineX, {
        # 0 will be coerced to FALSE
        # 1+ will be coerced to TRUE
        v$examineX <- input$examineX
    })
    
    #----outliers info and boxplot
    output$outliersText <- renderPrint({
        req(input$filePath)
        if (v$examineX == FALSE) return()
        else{
            isolate({
                if(is.numeric(df()[[input$xVar]])){
                    summary(df()[[input$xVar]])
                } else{
                    summary(as.factor(df()[[input$xVar]]))
                }
            }) 
        }
    })
    
    #xVar
    output$outliersPlot <- renderPlot({
        req(input$filePath)
        if (v$examineX == FALSE) return()
        isolate({
            if((!input$isXaFactor) & (is.numeric(df()[[input$xVar]]))){ #factorial -  length(unique(df()[[input$xVar]]))<20
                boxplot(df()[[input$xVar]])
            }
            else{
                barplot(prop.table(table(df()[[input$xVar]])), main = input$xVar)
            }
        })
    })
    
    #------------------feature selection & engineering 
    xVarsForInput <- reactive({
        req(input$selectY)
        if (values$isYselected){
            all <- colnames(df())
            return(all[!all %in% c(input$yVar)])
        } 
        else{
            return(c("Define Y first"))
        }
    })
    
    observe({
        curr <- xVarsForInput()[!xVarsForInput() %in% values$varsInModel]
        updateSelectInput(session, "xVar", 
                          choices = curr,
                          selected = head(curr, 1))
        
    })
    
    output$xPlot <- renderPlot({
        if (v$examineX == FALSE) return()
        isolate({
            if((!input$isXaFactor) & (is.numeric(df()[[input$xVar]])) ){
                hist(df()[[input$xVar]], main = paste(input$xVar),  xlab = paste(input$xVar), ylab = 'Frequency',col="lightblue" , freq=FALSE)
                lines(density(df()[[input$xVar]], na.rm = TRUE),col="red",lwd=2)   
            } else {
                barplot(prop.table(table(df()[[input$xVar]])), main = input$xVar)
            }
        })
    })
    
    observeEvent(input$AddXvar, {
        if (values$isYselected){
            values$varsInModel <- append(values$varsInModel, c(input$xVar))
            v$ok3 <- TRUE
        }
    })
    
    observeEvent(input$undoXadd, {
        if(values$isYselected){
            if (length(values$varsInModel) > 0){
                values$varsInModel <- values$varsInModel[-length(values$varsInModel)]
                if (length(values$varsInModel) == 0){
                    v$ok3 <- FALSE
                }
            }
        }
    })
    
    observe({
        shinyjs::toggle(id = "transforma", condition = input$transX)
        shinyjs::toggle(id = "addTxVar", condition = v$examineTransformation)
        shinyjs::toggle(id = "examineTransX", condition = input$transX)
        shinyjs::toggle(id = "transXplot", condition = input$transX)
    })
    
    
    observeEvent(input$examineTransX, {
        v$examineTransformation <- input$examineTransX
    })
    
    observe({
        if(v$examineTransformation){
            v$newFeature <- paste(input$transforma, input$xVar, sep = "_")
            if(!(v$newFeature %in% colnames(df()))){
                df() <- feature_transformation(df(), input$xVar, input$transforma)
            }
        }
    })
    
    output$transXplot <- renderPlot({
        if (v$examineTransformation == FALSE) return()
        isolate({
            if((!input$isXaFactor) & (is.numeric(df()[[v$newFeature]])) ){ 
                hist(df()[[v$newFeature]], main = v$newFeature,  xlab = v$newFeature, ylab = 'Frequency',col="lightblue" , freq=FALSE)
                lines(density(df()[[v$newFeature]], na.rm = TRUE),col="red",lwd=2)   
            } else {
                barplot(prop.table(table(df()[[v$newFeature]])), main = v$newFeature)
            }
        })
    })
    
    observeEvent(input$addTxVar,{
        if (values$isYselected){
            newFe <- paste(input$transforma, input$xVar, sep = "_")
            if(!(newFe %in% colnames(df()))){
                observe(
                    df() <- feature_transformation(df(), input$xVar, input$transforma)
                )
            }
            values$varsInModel <- append(values$varsInModel, c(newFe))
            updateCheckboxInput(session, "transX", value = FALSE)
            v$examineTransformation <- FALSE
        }
    })
    
    observe({
        if(v$ok3){
            shinyjs::enable("next3") 
        } else {
            shinyjs::disable("next3")}
    })
    
    #-------------outliers ui------------------
    observeEvent(input$next3,{
        v$data <- df()[ ,ouliersVars()]
        if(is.null(input$outlierVar)){
            appendTab(inputId = "myApp", 
                      tab = tabPanel("Outliers", fluidPage(
                          h2("Outliers Examination & Treatment"),
                          h4("Choose a variable from your features"),
                          selectInput(inputId = "outlierVar", label = "Variable", choices = ouliersVars(), width = "300px"),
                          h4("and examine with one of the following methods:"),
                          div("1. Conditional Filtering - select a condition and check if there are observetion fitts"),
                          div("2. Visual Filtering - Visual analyze with scatter plot allows select points"),
                          h5("in both cases, if observations are founds, they will be display in the table below"),
                          tabsetPanel(id = "outliersVarsPanel",
                                      tabPanel("Conditional Filtering", value = "A", fluidPage(
                                          fluidRow( 
                                              wellPanel(
                                                  h5("define a constraint you would like to examine and press go!"),
                                                  inputPanel(
                                                      radioButtons("outlierVarType", "Variable Type", choices = c("int", "double", "factor"), inline = TRUE),
                                                      selectInput("outlierOpeationType", "Constraint Type", choices = c("None")),
                                                      uiOutput("outlierCnstValue"),
                                                      # conditionalPanel(
                                                      #   condition = "input.outlierOpeationType "
                                                      # )
                                                      actionButton("outlierGo", "GO")
                                                  )
                                              )),
                                      )),
                                      tabPanel("Visual Filtering", value = "B", fluidPage(
                                          helpText("You can see your data in the plot abowe as y ~ x. The plot type determines by the variables' type combination. Double click for choosing data"),
                                          helpText("Current bug - after omit row, serch manual condition and then try to select visual points again"),
                                          plotOutput("outlierPlot", dblclick = "plotOne_dblclick", brush = brushOpts(id = "plot1_brush", clip = TRUE,
                                                                                                                     resetOnNew = TRUE, 
                                                                                                                     delay = 5000), height = "500px")
                                      )), type = "tabs"
                          ),
                          hr(),
                          DT::dataTableOutput("outliersTable"),
                          fluidRow(
                              actionButton("omit_ouliers_rows", "Omit rows"),
                              checkboxInput("selectAlloutliers", "Select All rows", value = FALSE),
                              #textOutput("rows"),
                              verbatimTextOutput("omitSumary")
                          ),
                          div(style="display:inline-block; font-size: 150%;",actionButton("next6", "Continue"), style="float:right")
                      ))
            )
        }
        updateTabsetPanel(session, "myApp", selected = "Outliers")
        updateSelectInput(session, "outlierVar", 
                          choices = ouliersVars(),
                          selected = head(ouliersVars(), 1)
        )
        shinyjs::disable("outlierVarType")
    })
    
    #---------------Outliers functions--------------
    operations_options <- reactive({
        req(input$outlierVar)
        if(is.numeric(df()[[input$outlierVar]])){
            ops <- c(">", ">=", "<", "<=", "==", "!=")
        } else {
            ops <- c("==", "!=")
        }
    })
    
    ouliersVars <- reactive({
        return(c(values$varsInModel, input$yVar))
    })
    
    observe({
        updateSelectInput(session, "outlierOpeationType", 
                          choices = operations_options(),
                          selected = head(operations_options(), 1)
        )
        updateRadioButtons(session, "outlierVarType",
                           choices =  c("int", "double", "factor"),
                           selected = variableType(df(), input$outlierVar))
    })
    
    outType <- reactive({
        variableType(df(), input$outlierVar)
    })
    
    dat<-reactive({
        if(is.numeric(df()[[input$outlierVar]])){
            me <- mean(df()[[input$outlierVar]])
            #cat(c('mean: ', me))
            out1 <- "Numeric variable"
        } else {
            #cat("Unique valuse: \n")
            out1 <-sort(unique(df()[[input$outlierVar]]))
            #cat(out1)
            #cat("\n")
        }
        return(out1)
    })
    
    meanValue <- reactive({
        if(is.numeric(df()[[input$outlierVar]])){
            me <- mean(df()[[input$outlierVar]], na.rm = TRUE)
            #cat(c('mean: ', me))
        } else {
            me <- -99
        }
        return(me)
    })
    output$outlierCnstValue <- renderUI({
        if(outType() == 'factor'){
            selectInput("outlierCnstValueFactor", 
                        'Choose Attribute(s) Value(s)', 
                        dat(), 
                        selected=head(dat(), 1), 
                        multiple = TRUE)
        } else {
            numericInput("outlierCnstValueNumeric", "Value",value = meanValue() )
        }
    })
    
    output$outlierPlot <- renderPlot({
        req(data_reactive_elements$final_data)
        data <- data_reactive_elements$final_data
        yType <- variableType(data, input$yVar)
        xType <- variableType(data, input$outlierVar)
        if (yType == 'factor'){
            if (xType == 'factor'){
                dat <- data.frame(table(data[[input$yVar]],data[[input$outlierVar]]))
                names(dat) <- c(input$outlierVar,input$yVar,"Count")
                
                ggplot(data=dat, aes_string(x=input$outlierVar, y='Count', fill=input$yVar)) + geom_bar(stat="identity")
            }
            else{ # X numeric, Y factor - boxplot
                ggplot(data, aes_string(x=input$yVar, y=input$outlierVar, fill=input$yVar)) + 
                    geom_boxplot()
            }
        } 
        else {
            if (xType == 'factor'){
                ggplot(data, aes_string(x=input$outlierVar, y=input$yVar, fill=input$outlierVar)) + 
                    geom_boxplot()
            }
            else{ #both numerics - scatter plot
                ggplot(data, aes_string(x=input$outlierVar, y=input$yVar)) + geom_point() +
                    theme_bw()
            }
        }
    })
    
    output$outliersTable <- DT::renderDataTable({
        datatable( data = outliersTableContent(),
                   filter = "top", # location of column filters
                   selection = 'multiple'       
        )
    })  
    
    data_reactive_elements <- reactiveValues(firstOUT=TRUE, condition_outlier_flag=FALSE, omitRawFlag=FALSE, omited_data=data.table(), last_conditional_row_batch=data.table())
    
    observe({
        req(input$outlierVar)
        if(data_reactive_elements$firstOUT){
            print(typeof(df()))
            data_reactive_elements$final_data <- df()
        }
    })
    
    getBrushedPoints <- reactive({
        return(data_reactive_elements$brush)
    })
    
    observeEvent(input$plotOne_dblclick, {
        #data_reactive_elements$omitRawFlag <- FALSE
        
        data_reactive_elements$brush <- brushedPoints(data_reactive_elements$final_data, input$plot1_brush)
        
    })
    
    outliersTableContent <- reactive({
        if(data_reactive_elements$omitRawFlag){
            out <- data.table()
        }else {
            out <- switch(input$outliersVarsPanel, 
                          "A" = data_reactive_elements$last_conditional_row_batch,
                          "B" = getBrushedPoints())
        }
        return(out)
    })
  
    #--------manual filtering function------------
    observeEvent(input$outlierGo, { # user manual searching
        #need to filter our data (final_data) with the user condition. 
        data_reactive_elements$last_conditional_row_batch <- data.table()
        if(outType() == 'factor'){ # multiple choices option
            if(length(input$outlierCnstValueFactor) > 0){ #user validation
                data_reactive_elements$condition_outlier_flag <- TRUE
                for (i in (1:length(input$outlierCnstValueFactor))) {
                    current_condition <- paste(input$outlierVar, input$outlierOpeationType,"'",input$outlierCnstValueFactor[i],"'", sep = "")
                    last_condition_data <- data_reactive_elements$final_data[which(eval(parse(text=current_condition))), ]
                    data_reactive_elements$last_conditional_row_batch <- rbindlist(list(data_reactive_elements$last_conditional_row_batch, last_condition_data))
                }
                #}
            } else { #pop-up message to user
                shinyalert("Oops!", "You didn't choose any valuse to compare to.", type = "error")
            }
        } else { #single value condition
            current_condition <- paste(input$outlierVar, input$outlierOpeationType, "as.numeric(" ,input$outlierCnstValueNumeric, ")", sep = "")            
            data_reactive_elements$condition_outlier_flag <- TRUE
            last_condition_data <- data_reactive_elements$final_data[which(eval(parse(text=current_condition))), ]
            data_reactive_elements$last_conditional_row_batch <- last_condition_data
        }
        data_reactive_elements$omitRawFlag <- FALSE
    })
    
    rowsToOmit <- reactive({
        if(input$selectAlloutliers){ #all rows in table
            out <- switch(input$outliersVarsPanel,
                          "A" = data_reactive_elements$last_conditional_row_batch,
                          "B" = brushedPoints(data_reactive_elements$final_data, input$plot_brush, allRows = TRUE)) 
        } else{
            out <- input$outliersTable_rows_selected
        }
        return(out)
    })
    
    observe({
        req(input$outliersTable_rows_selected)
        output$rows <- renderPrint({
            s = input$outliersTable_rows_selected
            if (length(s)) {
                print('These rows selected in table:\n\n')
                cat(s, sep = ', ')
                print("\n from rowsToOmit:")
                print(rowsToOmit())
            }
        })
        output$omitSumary<-renderPrint({
            print("Summary omited and final data:")
            tempSummary()
        })
    })
    
    observeEvent(input$omit_ouliers_rows ,{
        session$resetBrush("plot1_brush")
        data_reactive_elements$omitRawFlag <- TRUE
        data_reactive_elements$firstOUT <- FALSE
        data_reactive_elements$omited_data <- rbindlist(list(data_reactive_elements$omited_data, data_reactive_elements$final_data[rowsToOmit(),]))
        data_reactive_elements$final_data <- data_reactive_elements$final_data[-rowsToOmit(), ]
    })
    
    tempSummary <- reactive({
        out <- paste0("Omit nrow:" , str(nrow(data_reactive_elements$omited_data)),
                      "Final nrow:", str(nrow(data_reactive_elements$final_data)), sep = " ")
        return(out)
    })
    
    observeEvent(input$next6, {
        if(is.null(input$saveData)){
            appendTab(inputId = "myApp", 
                      tab = tabPanel("Save", fluidPage(
                          wellPanel(
                              helpText("After quite understandinf the features on your dataset, explored missing values, distribations, outliers and integrity issuse you can save the customized dataset in a csv file and continue to fit a model. \n you can check your final dataset below"),
                              flowLayout(
                                  #textInput("userFileName", "File Name"),
                                  downloadButton("downloadResults","Download Manipulated Data"),
                                  #actionButton("saveData", "Save"),
                                  actionButton("nextToModelFit", "Fit A Model")
                              )
                          ),
                          h3("Final Data"),
                          DT::dataTableOutput("finalDataTable")
                      ))
            )
        }
        updateTabsetPanel(session, "myApp", selected = "Save")
        
    })

    output$downloadResults <-  downloadHandler(
        filename = function(){paste("", ".csv", sep = "") },
        content = function(file){write.csv(data_reactive_elements$final_data, file, row.names = FALSE)}
    )
    
    output$finalDataTable <- DT::renderDataTable({
        req(data_reactive_elements$final_data)
        DT::datatable(
            data_reactive_elements$final_data, options = list(
                lengthMenu = list(c(10, 20, 50, -1), c('10', '20', '50', 'All')),
                pageLength = 10, 
                rownames=FALSE  
            )
        )
    })
}


#--------- Run the application -----------
shinyApp(ui = ui, server = server)




