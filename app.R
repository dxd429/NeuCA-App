library(shiny)
library(NeuCA)
library(shinycssloaders)
library(BiocManager)
library(pryr)
library(shinydashboard)
# Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")
# library(reticulate)
# library(tensorflow)
# library(keras)
#
# reticulate::virtualenv_create("myenv", python="/usr/bin/python3")
# reticulate::use_virtualenv("myenv", required=TRUE)
#
# if (!is_keras_available()) {
#   install_keras(method="virtualenv", envname="myenv")
#   reticulate::use_virtualenv("myenv", required=TRUE)
#   library(keras)
#   library(reticulate)
# }
options(repos = BiocManager::repositories())
#Sys.setenv(RETICULATE_PYTHON = "D:\\Anaconda3")
#######
##########
##############UI
options(shiny.maxRequestSize = 8000*1024^2)
#Function to load RData into environment
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}
ui <- dashboardPage(
  
  # App title ----
  dashboardHeader(
    title= "NeuCA"
  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(menuItem("Analysis", tabName = "analysis", icon = icon("table"))
    ),
    sidebarMenu(menuItem("Guide", tabName = "guide", icon = icon("question-circle")))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          font-size: 13px;
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }'))),
    
    tabItems(
      tabItem("analysis",
              
              
              chose_file_box<- box(title = "Choose a .RData file",
                                   status = "info", solidHeader = TRUE,
                                   width = 12,
                                   
                                   # Input: Select input Rdata file ----
                                   fileInput("inputfile", "Choose a .RData file",
                                             multiple = FALSE,
                                             accept = c(".RData"))),
              
              training_data_box<- box(title = "Choose SingleCellExperiment object",
                                      status = "primary", solidHeader = TRUE,
                                      width = 12,
                                      selectInput("training_data", "Choose the training SingleCellExperiment object",
                                                  choices = NULL,
                                                  selected = NULL,
                                                  multiple = FALSE,
                                                  selectize = TRUE),
                                      selectInput("testing_data", "Choose the testing SingleCellExperiment object",
                                                  choices = NULL,
                                                  selected = NULL,
                                                  multiple = FALSE,
                                                  selectize = TRUE)),
              # Horizontal line ----
              model_size_box <- box(title = "Choose the model size",
                                    status = "primary", solidHeader = TRUE,
                                    width = 12,
                                    selectInput("size", "Choose the model size",
                                                choices = c("small", "medium", "big"),
                                                selected = NULL,
                                                multiple = FALSE,
                                                selectize = TRUE),
                                    # generate result
                                    tags$head(
                                      tags$style(HTML('#res{background-color:yellow}'))
                                    ),
                                    column(5,offset=1,actionButton("res", "Generate Predicted Labels",icon("check"))),
                                    tags$head(
                                      tags$style(HTML('#downloadData{background-color:yellow}'))
                                    ),
                                    column(6,downloadButton("downloadData", "Download Predicted Labels"))),
              column(5,h3(textOutput("res", container = span)))
              # Output
              #download_box <- box(title = "Download Predicted Labels",
              #                      status = "success", solidHeader = TRUE,
              #                       width = 12,
              #    tags$head(
              #      tags$style(HTML('#downloadData{background-color:yellow}'))
              #  ),
              #  column(12,align = "center",offset = 0, downloadButton("downloadData", "Download Predicted Labels"))),
              
              
              
              
              # Main panel for displaying outputs ----
              #mainPanel(
              #  withSpinner(h3(textOutput("res", container = span)))
              # Output: Data files ----
              # tabsetPanel(
              #   id = 'dataset',
              #   tabPanel("Training Data", DT::dataTableOutput("Train_table")),
              #   tabPanel("Testing Data", DT::dataTableOutput("Test_table"))
              # )
      ),
      tabItem("guide",
              chose_file_box<- box(title = "User's Guidance",
                                   status = "primary", solidHeader = TRUE,
                                   width = 12)
      ))))


##############
#####################
########################SERVER
server <- function(input, output, session) {
  # Use a reactiveFileReader to read the file on change, and load the content into a new environment
  filepath<- reactive({
    paste0(input$inputfile$datapath)
  })
  train_dat<- reactive({
    input$training_data
  })
  test_dat<- reactive({
    input$testing_data
  })
  observe({
    validate(
      need(filepath(), "Please upload a .RData file")
    )
    reader<- reactiveFileReader(500, session, filepath(), LoadToEnvironment)
    select_input_choices <- names(reader())
    
    updateSelectInput(session, inputId = "training_data", choices = select_input_choices)
    updateSelectInput(session, inputId = "testing_data", choices = select_input_choices)
    rm(reader)
    gc()
  })
  res_job<- eventReactive(input$res,{
    showModal(modalDialog("Working on scRNA-seq data cell label training and testing...", footer=NULL))
    validate(
      need(filepath(), "Please upload a .RData file")
    )
    validate(
      need(train_dat(), "Please select a train data")
    )
    validate(
      need(test_dat(), "Please select a test data")
    )
    reader<- reactiveFileReader(500, session, filepath(), LoadToEnvironment)
    print(pryr::mem_used())
    print(ls())
    #reader()[[train_dat()]]<- reader()[[train_dat()]]
    #reader()[[test_dat()]]<- reader()[[test_dat()]]
    #rm(reader)
    gc()
    print(pryr::mem_used())
    print(ls())
    res_label<- NeuCA(train = reader()[[train_dat()]], test = reader()[[test_dat()]],
                      model.size = input$size, verbose = FALSE)
    #rm(reader()[[train_dat()]])
    print("1")
    print(pryr::mem_used())
    names(res_label)<- reader()[[test_dat()]]@colData@rownames
    print("2")
    print(pryr::mem_used())
    res_sce<- SingleCellExperiment(reader()[[test_dat()]],
                                   colData = DataFrame(Predicted_Label = res_label)
    )
    #rm(reader()[[test_dat()]])
    print("3")
    print(pryr::mem_used())
    removeModal()
    return(res_sce)
  })
  # mess1<- eventReactive(input$res,{
  #   return("Started")
  # })
  mess<- eventReactive(res_job(),{
    return("Job Done! Ready to Download")
  })
  output$res<- renderText({
    mess()
  })
  output$downloadData <- downloadHandler(
    filename <- function(){
      paste("Testing Data with Predicted Labels.RData")
    },
    
    content = function(file) {
      dat<- res_job()
      save(dat, file = file)
    }
  )
}
shinyApp(ui, server)




