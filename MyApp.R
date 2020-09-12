#--------clean enviroment-------------
rm(list=ls(all.names=TRUE))
#---------load this packeges,if miss one, please install first
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

loadLibraries()
ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("Descriptive statistics of choosen Data"),
  tabsetPanel(id = "myApp",
              #----------------------Demographics
              tabPanel("Upload data", 
                       fluidPage(
                         fluidRow(
                           # Sidebar with a file input 
                           sidebarLayout(
                             
                             sidebarPanel( wellPanel(
                               #fileInput("filePath","Choose File", buttonLabel = "Browse...",
                               #          placeholder = "No file selected"),
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
                         fluidRow(
                           uiOutput("FEtitle0"),
                           uiOutput("FEtitle1"),
                           verbatimTextOutput("dataSummary"),
                           uiOutput("FEtitle2"),
                           verbatimTextOutput("dataSTR")
                           
                         ),
                         div(style="display:inline-block; font-size: 150%;",actionButton("next1", "Continue to Missing Values examination"), style="float:right")
                         
                       )#fluid page
              ), #tabset panel upload data
              #conditionalPanel(condition = "input.next1",
              # , #tabset panel Missing Values
              type = "tabs"), #all tabsets panel
  fluidRow(
    tags$footer(id="adVisu",
                'Ayelet Davidi, Descriptive statistics Application')
  )#footer
)


server <- function(input, output, session){
  options(shiny.maxRequestSize=3000*1024^2) 
  values <- reactiveValues(loadFlag=TRUE, nextB1=FALSE)
  
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
      
      df_temp = fread(file_path, skip = seq_length[i], nrows = chunk_size,header = FALSE,stringsAsFactors = FALSE)
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
    return(read_batch_with_progress(input$filePath$datapath,n_rows,15))
  })
  
  observe({
    values$originData <- getData()
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
  
  output$FEtitle0 <- renderUI({
    req(input$filePath)
    h2("First Examination")
  })
  output$FEtitle1 <- renderUI({
    req(input$filePath)
    h4("Summary")
  })
  output$FEtitle2 <- renderUI({
    req(input$filePath)
    h4("Str")
  })
  output$dataSummary <- renderPrint({
    req(input$filePath)
    summary(values$data)
  })
  
  output$dataSTR <- renderPrint({
    req(input$filePath)
    str(values$data)
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
                        verbatimTextOutput("omitText1"),
                        verbatimTextOutput("omitText2"),
                        verbatimTextOutput("omitText3")
                      )),
                      tabPanel("Assign Specific Value", fluidPage(
                        h5("Choose a specific method / value to assign in missing values"),
                        h6("Only numeric features will be assigned with your choice while categorial features will get 'Missing Value' value."),
                        tags$b("This action can't be undone, re-upload data for cancle this action"),
                        radioButtons("TypeToFillNA", " ", 
                                     choices = c("0", "-9999","mean", "median")),
                        flowLayout(
                          actionButton("specificValueGo", "GO")
                          #actionButton("resetSpecificValueGo", "Undo")
                        ),
                        br(),
                        verbatimTextOutput("specificValueNAText1"),
                        verbatimTextOutput("specificValueNAText2"),
                        verbatimTextOutput("specificValueNAText3")
                        
                      )),
                      # tabPanel("Mice", fluidPage(
                      #   
                      # )),  
                      type = "tabs"), style = "height: 420px"
        ),
        div(style="display:inline-block; font-size: 150%;",actionButton("next2", "Continue to Choose Variable"), style="float:right")
      )) 
      )#append tab
    }
    updateTabsetPanel(session, "myApp", selected = "Missing Values")
    #shinyjs::disable("next1")
  }) #observed event for creating second tab
  
  #--------------missing values---------------
  values <- reactiveValues(omitFlag = FALSE, csFlag = FALSE, omit="", csResetFlag=FALSE, svText= "")
  
  
  output$missingValues <- renderPlot({
    req(input$filePath)
    aggr(values$data,prop = T, numbers = T)
  })
  
  #-------------------------------------------omit-----------------------------------------------
  df <- reactive({
    ifelse(values$omitFlag,
           out <- na.omit(values$data),
           ifelse(values$csFlag,
                  out <- completeNAvalues(values$data, input$TypeToFillNA),
                  out <- values$data))
    return(out)
  })
  
  observeEvent(input$omitNA, {
    req(input$filePath)
    if(values$csFlag){
      values$omit <- "You already completed your data!"
    }else {
      if(any(is.na(df()))){
        values$omit <- "All rows with missing values omitted sucssesfuly"
        values$omitFlag <- TRUE
      }
      else{
        values$omit <- "Your data is complete! No NA in the data.."
      }
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
    if(values$omitFlag){
      values$svText <- "You omitted your missing data, you can undo this action first and try again"
    }else{
      if(any(is.na(df()))){
        values$svText <- paste("All rows with missing values omitted sucssesfuly with", input$TypeToFillNA)
        values$csFlag <- TRUE
      }else{
        values$svText <- "Your data is complete! No NA in the data.."
      }
    }
  })
  
  output$specificValueNAText1 <- renderPrint({
    req(input$filePath)
    values$svText
  })
  
  #-----inset tab # 3 - Target Variable
  observeEvent(input$next2, {
    if(is.null(input$yVar)){
      appendTab(inputId = "myApp", tab = tabPanel("Variables", fluidPage(
        tabsetPanel(id = "varsPanel",
                    tabPanel("Target Variable", fluidPage(
                      column(6, offset=4, h2("Target / Y variable")),
                      column(12, 
                             flowLayout(
                               varSelectInput("yVar", "Select Y variable: ", df(), multiple = FALSE,
                                              selectize = TRUE, width = "300px"),
                               fluidRow(
                                 column(12, offset = 1,
                                        actionButton("plotYDist", "Plot"),
                                        checkboxInput("isYaFactor", "Check if factor"))
                               ),
                               actionButton("selectY", "Choose"),
                               fluidRow(
                                 actionButton("resetY", "Reset"),
                                 br(),
                                 p("Rest button reset the hole model")
                               )
                             )),
                      column(5, offset = 3,
                             wellPanel(
                               plotOutput("yPlot", width = "100%")
                             ))
                    )
                    
                    ),#tabset panel Target Variable
                    tabPanel("Feature Selection and Engineering",fluidPage(
                      wellPanel(
                        h2("The Model:"), 
                        # wellPanel(
                        verbatimTextOutput("modelFormula")
                        # verbatimTextOutput("test")
                        # ),
                        # style = "z-index: 1000;", width = "750px",
                        # fixed = FALSE, height="300px"
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
                               h4("Outliers Examination"),
                               wellPanel(fluidRow(
                                 verbatimTextOutput("outliersText"),
                                 plotOutput("outliersPlot", width = "500px")
                               )),
                               hr(),
                               h4("Distribution Examination"),
                               fluidRow(
                                 column(6, wellPanel(
                                   plotOutput("xPlot", width = "100%"))),
                                 column(6, wellPanel(
                                   plotOutput("transXplot", width = "100%")))
                               )
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
  
  observeEvent(input$resetY, {
    shinyjs::enable("yVar")
    values$formula <- paste("")
    values$isYselected <- FALSE
    values$doYplot <- FALSE
    values$doXplot <- FALSE
    values$doTXplot <- FALSE
    v$examineX == FALSE
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
      if((!input$isYaFactor) & (is.numeric(df()[[input$yVar]])) ){
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
      #print(values$varsInModel)
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
  v <- reactiveValues(examineX = FALSE, examineTransformation = FALSE, newFeature = "none")
  
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
    }
  })
  
  observeEvent(input$undoXadd, {
    if(values$isYselected){
      if (length(values$varsInModel) > 0){
        values$varsInModel <- values$varsInModel[-length(values$varsInModel)]
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
}


#--------- Run the application 
shinyApp(ui = ui, server = server)
