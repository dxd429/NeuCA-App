#setwd("D:/Research/harry/NeuCA")
library(shiny)
library(NeuCA)
library(shinycssloaders)
library(BiocManager)
library(pryr)
library(shinythemes)
library(shinyalert)
library(shinyjs)
library(rclipboard)
library(fontawesome)
library(shinyBS)
library(viridis)
library(gplots)
library(htmlTable)
library(kableExtra)
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
options(shiny.maxRequestSize = 1000*1024^2)
#Function to load RData into environment
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

ui <- navbarPage(strong("NeuCA web server"),windowTitle = "NeuCA web server", theme = shinytheme("cosmo"),position = c("fixed-top"),
                 
                 
                 
                 # App title ----
                 
                 intro_page<- tabPanel(title = "Home",
                                       fluidRow(
                                         #############Any modification
                                         
                                         shinyjs::useShinyjs(),
                                         tags$style("#copy {background-color:gainsboro;border:gainsboro}"),
                                         tags$style(".fa-copy {color:black}"),
                                         rclipboardSetup(),
                                         tags$style(type="text/css", "body {padding-top: 70px;}"),
                                                useShinyjs(), 
                                                extendShinyjs(text = "shinyjs.dataexplain = function() {window.scrollTo(0, 550)}", functions = "dataexplain"),
                                          ################      
                                         column(tags$img(src="NeuCA_hex.png",width="100px",height="116px"),width=1,style="text-align:left"),
                                         column(width = 11, offset = 0, h1(strong("NeuCA: Neural-network based Cell Annotation tool")))),

                                       fluidRow(column(width=8,offset=0, h2(strong("Introduction"))),
                                                
                                                
                                                column(width =8,offset = 0,
                                                       hr(),
                                                       p("NeuCA is a cell annotation tool in scRNA-seq data. 
                                                  It is a supervised cell label assignment method that uses existing scRNA-seq data with 
                                                  known labels to train a neural network-based classifier, and then predict cell labels 
                                                  in single-cell RNA-seq data of interest. NeuCA web server is based on the ",a(href="https://bioconductor.org/packages/NeuCA/", 
                                                                                                                                strong("Bioconductor package NeuCA "), target="_blank"),". 
                                                  Here, NeuCA web server provides GUI for users who want to use NeuCA to predict cell types,
                                                  without configuring and deploying deep learning environment/API in local computers.",
                                                         style="text-align:left;color:black;font-size:18px;"),
                                                       tags$img(src="workflow.PNG",width="800px",height="440px")),
                             
                                         
                                         column(width=4,offset=0,
                                                br(),
                                                tags$div(
                                                  p("Links",style = "font-size:25px;color:#663300"),
                                                  
                                                  a(href="https://bioconductor.org/packages/NeuCA/", strong("NeuCA As a Bioconductor Package", style = "color:#663300"), target="_blank"),
                                                  br(),
                                                  a(href="https://github.com/haoharryfeng/NeuCA", strong("Github Page", style = "color:#663300"), target="_blank"),
                                                  br(),
                                                  a(href="https://hfenglab.org/", strong("Our Group's Website", style = "color:#663300"), target="_blank"),
                                                  style="text-align:left;color:black;background-color:#FFFFCC;padding:20px;border-radius:10px;font-size:18px;"),
                                                br(),br(),br(),br(),br(),
                                                tags$div(
                                                  p("Contact",style = "font-size:25px;color:#330033"),
                                                  p("Author: Daoyu Duan(Maintainer), Sijia He",style = "color:#330033"),
                                                  
                                                  p("Email:", a(href="dxd429@case.edu","dxd429@case.edu",style = "color:#330033"),style = "color:#330033"),
                                                  p("Department of Population and Quantitative Health Sciences, Case Western Reserve University, Cleveland, OH 44106, USA.",style = "color:#330033"),
                                                  #a(href="sxh957@case.edu","sxh957@case.edu"),
                                                  style="text-align:left;color:black;background-color:lavender;padding:20px;border-radius:10px;font-size:18px;")),
                                 
                                         
                                         column(width = 8, offset=0,
                                                br(),
                                                h2(strong("How to use"))),
                              
                                         
                              column(width =8,offset = 0, 
                                     hr(),
                                     p("Follow instructions provided at the", strong("Tutorial", style = "color:#003366"), "tab. 
                                       This process can be broken down into two major steps:",
                                       style="text-align:left;color:black;font-size:18px;"),
                                     p(strong("Step 1.")," Data Preparation: Prepare the data for upload as an R object. 
                                        See", strong("Tutorial", style = "color:#003366"), "for details.",
                                       style="text-align:left;color:black;font-size:18px;"),
                                     tags$ul(tags$li("If using your own training data, both labeled training data and unlabeled testing data need to be converted to ", strong("SingleCellExperiment", style = "color:#003366"), "objects in R. While only testing data is required if using built-in classifiers.",
                                       style="text-align:left;color:black;font-size:18px;")),
                                     # tags$ul(tags$li("If you are using your own training data, both training data (labeled, cell type known) and testing data (unlabeled, cell type unknown) 
                                     #   should be converted to one ",strong("SingleCellExperiment", style = "color:#003366"), "object in R.",style="text-align:left;color:black;font-size:18px;"),
                                     #   
                                     #   tags$li("If you decide to use our built-in pre-trained classifiers, only testing data (unlabeled, cell type unknown) 
                                     #   need to be converted to a ",strong("SingleCellExperiment", style = "color:#003366"), "object in R.",style="text-align:left;color:black;font-size:18px;")),
                                     p(strong("Step 2."),"NeuCA Analysis: Upload and run your dataset. See", strong("Tutorial", style = "color:#003366"), "for details.",
                                       style="text-align:left;color:black;font-size:18px;"),
                                     tags$ul(tags$li("Once your input data is ready, follow the procedure described in  ", strong("Tutorial", style = "color:#003366"), "to run NeuCA analysis and download your results.",
                                       style="text-align:left;color:black;font-size:18px;")),
                                     p("We obtained 20 publicly available scRNA-seq datasets from UCSC Cell Browser, trained each of them accordingly, and included results in NeuCA web server as built-in classifiers. Links to data description can be found on ", strong("Run NeuCA", style = "color:#003366"), "page.",
                                       style="text-align:left;color:black;font-size:18px;"),
                                     column(width = 12,align="center",
                                            
                                            htmlOutput('tbl')
                                     )
                                     # tags$ul(tags$li("If you are using your own training data, upload your .RData/.rda file and select your training object and testing object, and get the cell types predicted! 
                                     #           Click ",strong("Generate Predicted Labels", style = "color:#003366"), "to run NeuCA. Once the prediction is complete, 
                                     #           click the ",strong("Download Predicted Labels", style = "color:#003366"), "to download the prediction results.",style="text-align:left;color:black;font-size:18px;"),
                                     # 
                                     # tags$li("If you decide to use our built-in pre-trained classifiers, just pick the corresponding pre-loaded ", strong("data type", style = "color:#003366"), " if exists and upload your testing data. The rest will be the same as above.",style="text-align:left;color:black;font-size:18px;"))
                                     )
                              
                              
                                       )),
                 
                 guide_page<- tabPanel(title = "Tutorial",
                                       fluidRow(
                                         column(width = 6, offset = 0,h2(strong("Run cell annotation in NeuCA app")),hr(),
                                                h2(strong("Using built-in classifier:", style = "font-size:20px;")), 
                                                p(strong("1."), "Choose the ", strong("data type", style = "color:#003366"), "that corresponds to your testing data",
                                                  "object.", tags$br(),
                                                  strong("2."), "Browse and upload the .RData/.rda file containing the testing", strong("SingleCellExperiment", style = "color:#003366"),
                                                  "objects.", tags$br(),
                                                  strong("3."), "Determine the", strong("model size", style = "color:#003366"),"with the",strong("drop-down", style = "color:#003366"),"menu.", tags$br(),
                                                  HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Users have the option to determine the complexity of the neural network used in NeuCA
                                       by specifying the desired", strong("model.size", style = "color:#003366"), "argument. Here,", "'", strong("big", style = "color:#003366"),"'", ",'", strong("medium", style = "color:#003366"),"'", "and", "'", strong("small", style = "color:#003366"),"'", "are 
                                       3 potential choices, reflecting large, medium and small number of nodes and layers 
                                       in neural-network, respectively.",tags$br(),
                                                  strong("4."), "Click the",strong("Generate Predicted Labels",style = "color:#003366"),"button, and once the computation completes, use the",strong("Download Predicted Labels", style = "color:#003366"), "button to save your results.", 
                                                  style="text-align:left;color:black;font-size:18px;")),
                                       column(width = 6, offset = 0,h2(strong("Run cell annotation in NeuCA app",style = "color:white")),hr(),
                                              h2(strong("Using own training data:", style = "font-size:20px;")), 
                                         p(strong("1."), "Browse and upload the .RData/.rda file containing both training and testing", strong("SingleCellExperiment", style = "color:#003366"),
                                       "objects.", tags$br(),
                                       strong("2."), "Choose training and testing object with the",strong("drop-down", style = "color:#003366"),"menu.", tags$br(),
                                       strong("3."), "Determine the", strong("model size", style = "color:#003366"),"with the",strong("drop-down", style = "color:#003366"),"menu.", tags$br(),
                                       HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Users have the option to determine the complexity of the neural network used in NeuCA
                                       by specifying the desired", strong("model.size", style = "color:#003366"), "argument. Here,", "'", strong("big", style = "color:#003366"),"'", ",'", strong("medium", style = "color:#003366"),"'", "and", "'", strong("small", style = "color:#003366"),"'", "are 
                                       3 potential choices, reflecting large, medium and small number of nodes and layers 
                                       in neural-network, respectively.",tags$br(),
                                       strong("4."), "Click the",strong("Generate Predicted Labels",style = "color:#003366"),"button, and once the computation completes, use the",strong("Download Predicted Labels", style = "color:#003366"), "button to save your results.", 
                                       style="text-align:left;color:black;font-size:18px;")),
                                       
                                       column(width = 7, offset = 0, 
                                              h2(strong("Data Preparation")),hr(), 
                                              p("Example raw data (not ready for uploading) can be downloaded from ",a(href="https://github.com/dxd429/NeuCA-App/tree/main/Data/Raw%20Example%20Data", "Here", target="_blank"),".", tags$br(), " Example data(", strong("Baron", style = "color:#003366")," ,", strong("Seg", style = "color:#003366"), ") were human pancreas datasets obtained from 2 studies, using inDrop and Smart-Seq2, respectively. Data were collected from 4 and 6 human donors, respectively. ", strong("Baron", style = "color:#003366"), " datasets were downloaded from GEO with accession", a(href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE84133", "GSE84133", target="_blank"), ", and ", strong("Seg", style = "color:#003366"), " dataset was downloaded from the European Molecular Biology Laboratory (EMBL-EBI,
https://www.ebi.ac.uk/, accession number", a(href="https://www.ebi.ac.uk/gxa/sc/experiments/E-MTAB-5061/experiment-design", "E-MTAB-5061", target="_blank"), "). Both were processed by excluding rare cell types.", tags$br(), 
                                                "Package", strong("SingleCellExperiment", style = "color:#003366"), "is required, please refer to",a(href = "https://bioconductor.org/packages/SingleCellExperiment/", "this Bioconductor SingleCellExperiment page", target="_blank"), "for installation if necessary.", tags$br(),
                                              "Data is required to be processed into the ", strong("SingleCellExperiment", style = "color:#003366"), "object(s) in your local computer using R or Rstudio. Processed sce object(s) should be saved into a .RData or .rda file.",  
                                                tags$br(),"Example code for data preparation:",
                                                style="text-align:left;color:black;font-size:18px;"),
                                              tags$div(
                                                "setwd('')   #Set your working dictionary",rclipButton("copy", label = "",
                                                                                                        clipText = as.character(paste0("setwd('')   #Set your working dictionary", "\n", "library(SingleCellExperiment)", "\n", "load('Baron_scRNA.RData') #Load raw data as a large matrix")), 
                                                                                                        icon = icon("copy",lib = "font-awesome"), modal = TRUE, width = "1%"),tags$br(),
                                                "library(SingleCellExperiment)", tags$br(),
                                                "load('Baron_scRNA.RData') #Load raw data as a large matrix",tags$br(),
                                                style="text-align:left;color:black;background-color:gainsboro;padding:10px;border-radius:10px;font-size:16px;"),tags$br(),
                                              tags$div(
                                                "#Prepare Data as a Sce object",rclipButton("copy", label = "",
                                                                                            clipText = as.character(paste0("#Prepare Data as a Sce object", "\n", "Baron_anno = data.frame(Baron_true_cell_label, row.names = colnames(Baron_counts))", "\n", "Baron_sce = SingleCellExperiment(assays = list(normcounts = as.matrix(Baron_counts)),colData = Baron_anno)", "\n", "# use gene names as feature symbols", "\n", "rowData(Baron_sce)$feature_symbol <- rownames(Baron_sce)", "\n", "# remove features with duplicated names", "\n", "Baron_sce <- Baron_sce[!duplicated(rownames(Baron_sce)), ]","\n","#After being transformed into Sce objects, data should look like screenshot on the right")), 
                                                                                            icon = icon("copy",lib = "font-awesome"), modal = TRUE, width = "1%"), tags$br(),
                                                "Baron_anno = data.frame(Baron_true_cell_label, row.names = colnames(Baron_counts))", tags$br(),
                                                "Baron_sce = SingleCellExperiment(assays = list(normcounts = as.matrix(Baron_counts)),colData = Baron_anno)", tags$br(),
                                                "# use gene names as feature symbols",tags$br(),
                                                "rowData(Baron_sce)$feature_symbol <- rownames(Baron_sce)" ,tags$br(),
                                                "# remove features with duplicated names",tags$br(),
                                                "Baron_sce <- Baron_sce[!duplicated(rownames(Baron_sce)), ]",tags$br(),
                                                "#After being transformed into Sce objects, data should look like screenshot on the right",
                                                style="text-align:left;color:black;background-color:gainsboro;padding:10px;border-radius:10px;font-size:16px;"),tags$br(),
                                              tags$br(),
                                              tags$div(
                                                "#Save as a RData file, save training and testing data together when using your own training data", rclipButton("copy", label = "",
                                                                                                                                                                clipText = as.character(paste0("#Save as a RData file, save training and testing data together when using your own training data", "\n", "save(Baron_sce, file = 'sc_example.RData')")), 
                                                                                                                                                                icon = icon("copy",lib = "font-awesome"), modal = TRUE, width = "1%"), tags$br(),
                                                "save(Baron_sce, file = 'sc_example.RData')",
                                                style="text-align:left;color:black;background-color:gainsboro;padding:10px;border-radius:10px;font-size:16px;"),tags$br()),
                                       column(width = 5, offset = 0,tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                              tags$img(src="workflow1.png",width="555px",height="312px"),
                                              tags$img(src="workflow2.png",width="555px",height="312px")),
                                       column(width = 7, offset = 0, 
                                              h2(strong("Data Ready for Uploading")),hr(),
                                              p("Following steps above, you should be able to get a file like", a(href="https://github.com/dxd429/NeuCA-App/tree/main/Data/Processed%20Example%20Data%20for%20Upload", "this", target="_blank"), ", and it is ready to be uploaded.",
                                                style="text-align:left;color:black;font-size:18px;") ),
                                       column(width = 7, offset = 0,
                                              h2(strong("Running Time")),hr(),
                                              p("Using a local PC(CPU: Intel i7-8700, Memory: 32 GB, GPU: NVIDIA 1080 16 GB): ", 
                                                style="text-align:left;color:black;font-size:18px;"),
                                              tags$ul(
                                                tags$li(p(strong("Data Preparation", style = "color:#003366"),"took 4.25s.",style="text-align:left;color:black;font-size:18px;")),
                                                tags$li(p(strong("Data Upload", style = "color:#003366"),"took 4.82s.",style="text-align:left;color:black;font-size:18px;")),
                                                tags$li(p(strong("Cell Labels Prediction", style = "color:#003366"),"took 7.09s.",style="text-align:left;color:black;font-size:18px;")),
                                                tags$li(p(strong("Results Download", style = "color:#003366"),"took 4.10s.",style="text-align:left;color:black;font-size:18px;"))
                                                
                                              ),
                                              
                                       
                                              
                                              )
                                                
                                       )),
                 
                 analysis_page<- tabPanel(title = "Run NeuCA",
                                          
                                          
                                          # Sidebar layout with input and output definitions ----
                                          sidebarLayout(
                                            
                                            # Sidebar panel for inputs ----
                                            sidebarPanel(
                                              
                                              width = 12,
                                              radioButtons("radio", 
                                                           label = "",
                                                           choiceNames = list(HTML('<b>Built-in Pre-trained Classifier</b>'),
                                                                               HTML('<b>Upload My Own Training Data</b>')),
                                                           choiceValues = c(1, 2),
                                                           selected = 1,
                                                           inline = T,
                                                           width = "100%"),
                                              conditionalPanel(condition = "input.radio == '2'",
                                                               
                                              # Input: Select input Rdata file ----
                                              fileInput("inputfile", "Choose a .RData/.rda file",
                                                        multiple = FALSE,
                                                        accept = c(".RData", ".rda")),
                                              
                                              selectInput("training_data", "Choose the training SingleCellExperiment object",
                                                          choices = NULL,
                                                          selected = NULL,
                                                          multiple = FALSE,
                                                          selectize = TRUE),
                                              selectInput("testing_data", "Choose the testing SingleCellExperiment object",
                                                          choices = NULL,
                                                          selected = NULL,
                                                          multiple = FALSE,
                                                          selectize = TRUE),
                                              # Horizontal line ----
                                              tags$hr(),
                                              selectInput("size", "Choose the model size",
                                                          choices = c("small", "medium", "big"),
                                                          selected = "big",
                                                          multiple = FALSE,
                                                          selectize = TRUE),
                                              # generate result
                                              actionButton("res", "Generate Predicted Labels",icon("check")),
                                              shinyjs::hidden(
                                                div(id = "plot_original", style="display:inline-block",
                                                    actionButton("plot_original_t",  "View Correlation Plot", icon=icon("bar-chart"))
                                                )),
                                              bsModal(id= "Correlation", title = "Correlation", trigger = "plot_original_t", size="large",
                                                      withSpinner(plotOutput("plot_cor")),
                                                      downloadButton("cor_plot", "Save Plot")),
                                              tags$hr(),
                                              # Output
                                              useShinyalert(),
                                              downloadButton("downloadData", "Download Predicted Labels as .RData"),
                                              downloadButton("downloadData_csv", "Download Predicted Labels as .csv"),
                                              downloadButton("downloadData_txt", "Download Predicted Labels as .txt")
                                              ),
                                              
                                              conditionalPanel(condition = "input.radio == '1'",
                                                               
                                          div(style="display: inline-block;vertical-align:top; width: 600px;", selectizeInput("training_type", "Choose the data type",
                                                             choices = c("(Human) PBMC" = "PBMC", "(Human) Pancreas" = "Pancreas", "(Human) Autism" = "ASD", "(Human) Molar" = "Molar", "(Human) Choroid Plexus" = "Choroid_Plexus", "(Human) Healthy Lung" = "Lung", "(Human) Aging Skin" = "Skin"
                                                                         , "(Human) Fetal Maternal Decidual" = "Fetal_Maternal_Decidual", "(Human) Muscle" = "Muscle", "(Human) Bronchoalveolar from COVID-19 Patients" = "Bronchoalveolar_COVID19", "(Human) Adult Retina" = "Adult_Retina"
                                                                         , "(Human) Fetal Gut" = "Fetal_Gut", "(Human) Ovary" = "Human_Ovary", "(Human) Glioblastoma" = "Human_Glioblastoma", "(Mouse) Lung" = "Mouse_Lung_Immune", "(Mouse) Enteric" = "Enteric", "(Mouse) Hippocampus" = "Hippocampus", "(Mouse) Medulla" = "Medulla", "(Mouse) Spinal Cord" = "Spinal_Cord", "Drosophila Ovary" = "Drosophila_Ovary"),
                                                             options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                             )
                                                           )),
                                          div(style="display: inline-block;vertical-align:top; width: 200px;",HTML("<br>"), actionButton("dataexplain", "What are these data?", 
                                                                                                                            style="color: blue; background-color: #00ff0000; border-color: #00ff0000")),
                                              # Input: Select input Rdata file ----
                                              fileInput("inputfile_pbmc", "Choose your testing file(.RData/.rda)",
                                                        multiple = FALSE,
                                                        accept = c(".RData", ".rda")),
                                               
                                              # Horizontal line ----
                                              tags$hr(),
                                              selectInput("size", "Choose the model size",
                                                          choices = c("small", "medium", "big"),
                                                          selected = "big",
                                                          multiple = FALSE,
                                                          selectize = TRUE),
                                              # generate result
                                              actionButton("res_pbmc", "Generate Predicted Labels",icon("check")),
                                          shinyjs::hidden(
                                            div(id = "plot_original_pbmc", style="display:inline-block",
                                                actionButton("plot_original_t_pbmc",  "View Correlation Plot", icon=icon("bar-chart"))
                                            )),
                                          bsModal(id= "Correlation_pbmc", title = "Correlation", trigger = "plot_original_t_pbmc", size="large",
                                                  withSpinner(plotOutput("plot_cor_pbmc")),
                                                  downloadButton("cor_plot_pbmc", "Save Plot")),
                                              tags$hr(),
                                              # Output
                                              useShinyalert(),
                                              downloadButton("downloadData_pbmc", "Download Predicted Labels as .RData"),
                                              downloadButton("downloadData_pbmc_csv", "Download Predicted Labels as .csv"),
                                              downloadButton("downloadData_pbmc_txt", "Download Predicted Labels as .txt")               
                                              ),
                                          
                                          column(width = 12, offset = 0,
                                                 HTML("<br>"),
                                                 HTML("<br>"),
                                                 p(strong("Direct link to description of each type of data:"))
                                                 
                                                 ,a(href="https://csoneson.github.io/DuoClustering2018/reference/sce_full_Zhengmix.html", 
                                                    strong("(Human) PBMC"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE84133", 
                                                    strong("(Human) Pancreas"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://www.science.org/doi/abs/10.1126/science.aav8130", 
                                                    strong("(Human) Autism"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=dental-cells+human-adult-molars", 
                                                   strong("(Human) Molar"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=chporg", 
                                                    strong("(Human) Choroid Plexus"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=lung-pf-control", 
                                                    strong("(Human) Healthy Lung"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=aging-human-skin", 
                                                    strong("(Human) Aging Skin"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=xena+hca-fetal-maternal", 
                                                    strong("(Human) Fetal Maternal Decidual"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=muscle-cell-atlas", 
                                                    strong("(Human) Muscle"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=covid19-balf", 
                                                    strong("(Human) Bronchoalveolar from COVID-19 Patients"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=adult-retina", 
                                                    strong("(Human) Adult Retina"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=gut-cell-atlas+fetal", 
                                                    strong("(Human) Fetal Gut"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=scarface+adult_ovary", 
                                                    strong("(Human) Human_Ovary"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=quake-gbm", 
                                                    strong("(Human) Human_Glioblastoma"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=mouse-lung-immune", 
                                                    strong("(Mouse) Lung"), target="_blank"),HTML("<br>")
              
                                                 ,a(href="https://cells.ucsc.edu/?ds=mouse-nervous-system+enteric", 
                                                    strong("(Mouse) Enteric"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=mouse-nervous-system+hippocampus", 
                                                    strong("(Mouse) Hippocampus"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=mouse-nervous-system+medulla", 
                                                    strong("(Mouse) Medulla"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=mouse-nervous-system+spinalcord", 
                                                    strong("(Mouse) Spinal Cord"), target="_blank"),HTML("<br>")
                                                 ,a(href="https://cells.ucsc.edu/?ds=dros-ovary+ovary-high-quality", 
                                                    strong("Drosophila Ovary"), target="_blank")
                                                 
                                          )
                                              
                                            ),
                                            
                                            # Main panel for displaying outputs ----
                                            mainPanel(h3(plotOutput("res")),
                                                      h3(plotOutput("res_pbmc")))
                                            
                                            # Output: Data files ----
                                            # tabsetPanel(
                                            #   id = 'dataset',
                                            #   tabPanel("Training Data", DT::dataTableOutput("Train_table")),
                                            #   tabPanel("Testing Data", DT::dataTableOutput("Test_table"))
                                            # )
                                            
                                          )),
                 faq_page<- tabPanel(title = "FAQ",
                                       fluidRow(
                                         column(width = 8, offset = 1,
                                                p(strong("What is training and testing?", style="color:black;font-size:22px;"),tags$br(),
                                                  "Training data are the data with known labels, we use them to train our model. Testing data are the data you are interested and trying to get predicted labels.",style="text-align:left;color:black;font-size:18px;"),
                                                p(strong("What data format can I upload?", style="color:black;font-size:22px;"),tags$br(),
                                                  "Only .RData/.rda file is supported for its convenience.",style="text-align:left;color:black;font-size:18px;"),
                                                p(strong("How to obtain predicted labels?", style="color:black;font-size:22px;"),tags$br(),
                                                  "As you are following",strong("Tutorial", style = "color:#003366"), ", you should be able to save predicted labels using",strong("Download Predicted Labels", style = "color:#003366"),"button.", 
                                                  style="text-align:left;color:black;font-size:18px;"),
                                                tags$div(
                                                  "setwd('Directory/With/Results')",rclipButton("copy", label = "",
                                                                                                         clipText = as.character(paste0("setwd('Directory/With/Results')", "\n", "load('Testing Data with Predicted Labels.RData')", "\n", "dat@colData")), 
                                                                                                         icon = icon("copy",lib = "font-awesome"), modal = TRUE, width = "1%"),tags$br(),
                                                  "load('Testing Data with Predicted Labels.RData')", tags$br(),
                                                  "dat@colData",tags$br(),
                                                  style="text-align:left;color:black;background-color:gainsboro;padding:10px;border-radius:10px;font-size:16px;"),
                                                p(strong("What format is the result file I download?", style="color:black;font-size:22px;"),tags$br(),
                                                  "The predicted label file is .RData file containing the testing", strong("SingleCellExperiment", style = "color:#003366"), "object with predicted labels.",
                                                  style="text-align:left;color:black;font-size:18px;")
                                                ),
                                       )),
                 
                 about_page<- tabPanel(title = "About",
                                     fluidRow(
                                       column(width = 8, offset=0,
                                              br(),
                                              h2(strong("Citation"))),
                                       column(width =8,offset = 0, 
                                              hr(),
                                              p("Pending",
                                                style="text-align:left;color:black;font-size:18px;")),
                                       column(width=8,offset=0, h2(strong("Funding"))),
                                       column(width =8,offset = 0, 
                                              hr(),
                                              p("This study was supported by the Corinne L. Dodero Foundation for the Arts and Sciences and the CWRU Program for Autism Education and Research.",
                                                style="text-align:left;color:black;font-size:18px;"))
                                     )))



##############
#####################
########################SERVER
server <- function(input, output, session) {
  # Use a reactiveFileReader to read the file on change, and load the content into a new environment
  observeEvent(input$dataexplain,{
    js$dataexplain()
  })
  observeEvent(res_job(),
               shinyjs::show("plot_original"))
  observeEvent(res_job_pbmc(),
               shinyjs::show("plot_original_pbmc"))
  selectedData <- reactive({

   # Create the table (using table from htmlTables doc as example)
    HTML(
      matrix(as.matrix(read.csv("Table1.csv", header = F)),
             ncol = 2,
             dimnames = list(c("", "", "", "", "", "", "", "", "", ""),
                             c(" ", " "))) %>%
        addHtmlTableStyle(align = "c",  css.table = "font-size: 18px ; color: black") %>%
      htmlTable(
              caption="Available Pre-trained Datasets and Classifiers",
                tfoot="")
      
    )

  })

  output$tbl <- renderUI({selectedData()})
  # table1<- reactive({
  #   tb<- read.csv("Table1.csv", header = F)
  # 
  #   print(as.matrix(tb,dimnames = list(c("", "", "", "", "", "", "", "", "", ""),c("Data", "Data"))))
  #   tb
  # 
  #   })
  # output$tbl <- renderTable({
  #   table1()
  #   }, caption = "Pre-trained datasets from UCSC Cell Browser", caption.placement = "top")
  filepath<- reactive({
    paste0(input$inputfile$datapath)
  })
  train_dat<- reactive({
    input$training_data
  })
  test_dat<- reactive({
    input$testing_data
  })
  filepath_pbmc<- reactive({
    paste0(input$inputfile_pbmc$datapath)
  })
  train_tp<- reactive({
    input$training_type
  })
  cor.det = function(dat, lb){
    #dat is a SingleCellExperiment training object
    ct = dat
    lb = lb
    lname = unique(lb)
    mp.ge = matrix(0, nrow = nrow(ct), ncol = length(lname))
    for(i in seq_along(lname)){
      #extract all cells GE, for each cell type category
      cge = ct[, which(lb == lname[i])]
      #average the profiles
      tmp = rowMeans(cge, na.rm = TRUE)
      #store them in mp.ge
      mp.ge[,i] = tmp
    }
    ac = cor(mp.ge)
    #diag(ac) = 0
    #return 1 if all cor < 0.95, return 2 of at least 1 cor >= 0.95
   # cd = ifelse(sum(ac<0.95) == length(ac), 1, 2)
    return(ac)
  }
  observe({
    validate(
      need(filepath(), "Please upload a .RData/.rda file")
    )
    reader<-  reactiveFileReader(500, session, filepath(), LoadToEnvironment)
    select_input_choices <- names(reader())
    rm(reader)
    gc()
    updateSelectInput(session, inputId = "training_data", choices = select_input_choices)
    updateSelectInput(session, inputId = "testing_data", choices = select_input_choices)
  })
  res_job<- eventReactive(input$res,{
    mem1<- mem_used()
    showModal(modalDialog("Working on scRNA-seq data cell label training and testing...", footer=NULL))
    validate(
      need(filepath(), "Please upload a .RData/.rda file")
    )
    validate(
      need(train_dat(), "Please select a train data")
    )
    validate(
      need(test_dat(), "Please select a test data")
    )
    reader<-  reactiveFileReader(500, session, filepath(), LoadToEnvironment)
    
    res_label<- NeuCA(train = reader()[[train_dat()]], test = reader()[[test_dat()]],
                      model.size = input$size, verbose = FALSE)
    res_anno<<- data.frame(Predicted_Label = res_label, row.names = reader()[[test_dat()]]@colData@rownames)
    res_sce<- SingleCellExperiment(assays = list(
      normcounts = as.matrix(reader()[[test_dat()]]@assays@data@listData$normcounts)), 
      colData = res_anno)
    mem2<- mem_used()
    mem_u<- mem2 - mem1
    print(mem_u)
    ###########
    
    
    ###########
    removeModal()
    rm(reader)
    gc()
    
    return(res_sce)
    
  })
  output$plot_cor<- renderPlot({
    cm<- cor.det(res_job()@assays@data@listData$normcounts, res_job()$Predicted_Label)
    rownames(cm)<- unique(res_job()$Predicted_Label)
    colnames(cm)<- unique(res_job()$Predicted_Label)
    heatmap.2(cm, # data frame a matrix
                    cellnote = round(cm,2),
                    notecol="black",
                    notecex = 2,
                    margins = c(6,15), # Adds margins below and to the right
                    density.info = "none", # Remove density legend lines
                    trace = "none", # Remove the blue trace lines from heatmap
                    Rowv = FALSE, # Do not reorder the rows
                    Colv = FALSE,
                    scale = "none",
                    dendrogram = "none", # Only plot column dendrogram
                    colsep=1:nrow(cm), # Add vertical grid lines
                    rowsep=1:nrow(cm), # Add horizontal grid lines
                    sepcolor = "black", # Color gridlines black
                    col = viridis::viridis_pal(),
                    breaks = 20)
    
  })
  res_job_pbmc<- eventReactive(input$res_pbmc,{
    mem1<- mem_used()
    showModal(modalDialog("Working on scRNA-seq data cell label testing...", footer=NULL))
    validate(
      need(filepath_pbmc(), "Please upload a .RData/.rda testing file")
    )
    validate(
      need(train_tp(), "Please select a training data")
    )
    
    reader<-  reactiveFileReader(500, session, filepath_pbmc(), LoadToEnvironment)
    
    
    load(paste0("./Data/", train_tp(), "_model.RData"))
    
    SampleNorm_test <- function(test_count = NULL) {

      ### sample normalization
      
      cmax <- apply(test_count, 2, max)
      cmin <- apply(test_count, 2, min)
      tmp1 <- sweep(test_count, 2, cmin, "-")
      tmp2 <- sweep(tmp1, 2, cmax-cmin, "/")

      ### let's try some feature selection here
      #cname <- intersect(colnames(train_count_out), rownames(tmp2))
      
      tmp3 <- tmp2[intersect(train_genename, rownames(tmp2)), ]
      
      matNA <- matrix(0,nrow = length(setdiff(train_genename, intersect(train_genename, rownames(tmp2)))), ncol = ncol(tmp3))
      rownames(matNA) <-setdiff(train_genename, intersect(train_genename, rownames(tmp2)))
      tmp3 <- rbind(tmp3,matNA)
      
      tmp3 <- tmp3[train_genename,]
      
      test_count_out<- t(tmp3)
      
      rm(tmp1, tmp2, tmp3)
      return(test_count_out)
    }
    
    
    test<- reader()[[names(reader())[1]]]
    rm(reader)
    test_count_out<- SampleNorm_test(test_count = assay(test))
    #print(dim(test_count_out))
    
    findm = function(a){
      tmp = which(a == max(a))
      return(tmp)
    }
    
    CTname = unique(train_col)
    
    assignCT <- function(mycount,
                         allsensvec2,
                         bestmarker2,
                         csthreshold = 3) {
      message("Assignning cell types based on marker genes...")

      estlabels <- rep(NA, ncol(mycount))

      for(i in seq_along(allsensvec2)) {
        nidx <- match(bestmarker2[[i]], rownames(mycount))
        onecount <- mycount[na.omit(nidx), ]
        if(!is.null(nrow(onecount))) {
          cs <- colSums(onecount)
        } else {
          cs <- onecount
        }
        estlabels[which(cs>=csthreshold)] <- names(allsensvec2)[i]
      }
      return(estlabels)
    }
    
    HNN_assign <- function(input_count, estlabels, train_SCH_out) {

      treelabels = train_SCH_out$treelabels
      treeCellTypes = train_SCH_out$treeCellTypes
      HNNtrees = train_SCH_out$HNNtrees
      allfeatures = train_SCH_out$allfeatures
      rownames(treelabels) <- paste0("Step", nrow(treelabels):1)

      message("Assign labels through the hierarchical Neural Network tree:")
      message(paste0("A total of ", nrow(treelabels), "steps!"))
      
      pb <- txtProgressBar(min = 0, max = nrow(treelabels))
      
      for(iter in 1:nrow(treelabels)) {
        setTxtProgressBar(pb, iter)
        
        input_count2 <- input_count[allfeatures[[iter]], ]
        
        if(iter == 1) {
          int_indx <- which(is.na(estlabels))
          
        } else {
          int_indx <- which(estlabels == rownames(treelabels)[iter])
          
        }
        
        tmpcell <- as.matrix(input_count2[, int_indx])
        
        if(!is.null(ncol(tmpcell)) & is.matrix(tmpcell) & length(int_indx)>0) {
          tmplab <- rep(NA, ncol(tmpcell))
          
          thismodel <- HNNtrees[[iter]]
          
          pred_y <- predict(thismodel, x = as.matrix(t(tmpcell)),
                            batch_size = 64,
                            verbose = 0)
          

          pred <- predict(thismodel, t(as.matrix(tmpcell)), decision.value = TRUE)
          
          newpred <- apply(pred, 1, which.max) - 1
          
          tmplab[which(newpred == 1)] <- treelabels[iter, 1]
          
          tmplab[which(newpred == 0)] <- treelabels[iter, 2]
         
          estlabels[int_indx] <- tmplab
          
        } else if(is.vector(tmpcell)) {
          tmplab <- NA
         
          thismodel <- HNNtrees[[iter]]
          
          tmpcell2 <- matrix(tmpcell,ncol = 1)
          
          rownames(tmpcell2) <- allfeatures[[iter]]
          
          pred <- predict(thismodel, t(tmpcell2), decision.value = TRUE)
          
          tmplab[which(pred == 1)] <- treelabels[iter, 1]
          
          tmplab[which(pred == 0)] <- treelabels[iter, 2]
          
          estlabels[int_indx] <- tmplab
        }
      }
      
      close(pb)

      return(estlabels)
    }
    
    ####
    
    if(exists("outmodel_small")){
      
      files_big<- list.files(path = "./Data", pattern = paste0("outmodel_", train_tp(), "_big"))
      files_medium<- list.files(path = "./Data", pattern = paste0("outmodel_", train_tp(), "_medium"))
      files_small<- list.files(path = "./Data", pattern = paste0("outmodel_", train_tp(), "_small"))
      outmodel_big<- load_model_hdf5(paste0("./Data/",files_big))
      outmodel_medium<- load_model_hdf5(paste0("./Data/",files_medium))
      outmodel_small<- load_model_hdf5(paste0("./Data/",files_small))
      preres <- predict(eval(parse(text = paste0("outmodel_", input$size))), x = as.matrix(test_count_out),
                        batch_size = 256,
                        verbose = FALSE)
     
      prenum = apply(preres, 1, findm)
      
      predict.label = CTname[prenum]
      
      rm(outmodel_small)
      rm(outmodel_medium)
      rm(outmodel_big)
    } else {
      
      test_count<- t(as.matrix(test_count_out))
      estlabels <- assignCT(test_count, MarkOut$allsensvec,
                            MarkOut$bestmarker, csthreshold = 3)
      test_count_normed <- test_count/max(test_count)
      matidx <- match(train_rownames, rownames(test_count_normed))
      test_count_normed <- test_count_normed[matidx, ]
     
      files_big<- list.files(path = "./Data", pattern = paste0("HNNtrees_", train_tp(), "_big"))
      files_medium<- list.files(path = "./Data", pattern = paste0("HNNtrees_", train_tp(), "_medium"))
      files_small<- list.files(path = "./Data", pattern = paste0("HNNtrees_", train_tp(), "_small"))
      mhnn_model_big$HNNtrees<- lapply(files_big, function(dat) load_model_hdf5(paste0("./Data/",dat)))
      mhnn_model_medium$HNNtrees<- lapply(files_medium, function(dat) load_model_hdf5(paste0("./Data/",dat)))
      mhnn_model_small$HNNtrees<- lapply(files_small, function(dat) load_model_hdf5(paste0("./Data/",dat)))
      predict.label <- HNN_assign(test_count_normed,
                                  estlabels,
                                  eval(parse(text = paste0("mhnn_model_", input$size))))
      
      rm(mhnn_model_small)
      rm(mhnn_model_medium)
      rm(mhnn_model_big)
    }
    res_anno<<- data.frame(Predicted_Label = predict.label, row.names = test@colData@rownames)
    res_sce<- SingleCellExperiment(assays = list(
      normcounts = as.matrix(test@assays@data@listData$normcounts)), 
      colData = res_anno)
    mem2<- mem_used()
    mem_u<- mem2-mem1
    print(mem_u)
    removeModal()
    rm(test)
    rm(test_count_out)
    
    gc()
    return(res_sce)
  })
  output$plot_cor_pbmc<- renderPlot({
    cm<- cor.det(res_job_pbmc()@assays@data@listData$normcounts, res_job_pbmc()$Predicted_Label)
    rownames(cm)<- unique(res_job_pbmc()$Predicted_Label)
    colnames(cm)<- unique(res_job_pbmc()$Predicted_Label)
    heatmap.2(cm, # data frame a matrix
              cellnote = round(cm,2),
              notecol="black",
              notecex = 2,
              margins = c(6,15), # Adds margins below and to the right
              density.info = "none", # Remove density legend lines
              trace = "none", # Remove the blue trace lines from heatmap
              Rowv = FALSE, # Do not reorder the rows
              Colv = FALSE,
              scale = "none",
              dendrogram = "none", # Only plot column dendrogram
              colsep=1:nrow(cm), # Add vertical grid lines
              rowsep=1:nrow(cm), # Add horizontal grid lines
              sepcolor = "black", # Color gridlines black
              col = viridis::viridis_pal(),
              breaks = 20)
    
  })
  # mess1<- eventReactive(input$res,{
  #   return("Started")
  # })
  #mess<- eventReactive(res_job(),{
  # return("Job Done! Ready to Download")
  #})
  mess<- eventReactive(res_job(),{
    shinyalert("Job Done! Ready to Download", confirmButtonCol = "black")
  })
  mess_pbmc<- eventReactive(res_job_pbmc(),{
    shinyalert("Job Done! Ready to Download", confirmButtonCol = "black")
  })
  output$res<- renderText({
    mess()
  })
  output$res_pbmc<- renderText({
    mess_pbmc()
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
  output$cor_plot <- downloadHandler(
    filename <- function(){
      paste("Correlation Plot.png")
    },
    
    content = function(file) {
      cm<- cor.det(res_job()@assays@data@listData$normcounts, res_job()$Predicted_Label)
      rownames(cm)<- unique(res_job()$Predicted_Label)
      colnames(cm)<- unique(res_job()$Predicted_Label)
      png(filename = file, width = 12, height = 9, units = "in", res = 300)
      heatmap.2(cm, # data frame a matrix
                cellnote = round(cm,2),
                notecol="black",
                notecex = 2,
                margins = c(6,15), # Adds margins below and to the right
                density.info = "none", # Remove density legend lines
                trace = "none", # Remove the blue trace lines from heatmap
                Rowv = FALSE, # Do not reorder the rows
                Colv = FALSE,
                scale = "none",
                dendrogram = "none", # Only plot column dendrogram
                colsep=1:nrow(cm), # Add vertical grid lines
                rowsep=1:nrow(cm), # Add horizontal grid lines
                sepcolor = "black", # Color gridlines black
                col = viridis::viridis_pal(),
                breaks = 20)
      dev.off()
    }
  )
  output$downloadData_pbmc <- downloadHandler(
    filename <- function(){
      paste("Testing Data with Predicted Labels.RData")
    },
    
    content = function(file) {
      dat<- res_job_pbmc()
      save(dat, file = file)
    }
  )
  output$cor_plot_pbmc <- downloadHandler(
    filename <- function(){
      paste("Correlation Plot.png")
    },
    
    content = function(file) {
      cm<- cor.det(res_job_pbmc()@assays@data@listData$normcounts, res_job_pbmc()$Predicted_Label)
      rownames(cm)<- unique(res_job_pbmc()$Predicted_Label)
      colnames(cm)<- unique(res_job_pbmc()$Predicted_Label)
      png(filename = file, width = 12, height = 9, units = "in", res = 300)
      heatmap.2(cm, # data frame a matrix
                cellnote = round(cm,2),
                notecol="black",
                notecex = 2,
                margins = c(6,15), # Adds margins below and to the right
                density.info = "none", # Remove density legend lines
                trace = "none", # Remove the blue trace lines from heatmap
                Rowv = FALSE, # Do not reorder the rows
                Colv = FALSE,
                scale = "none",
                dendrogram = "none", # Only plot column dendrogram
                colsep=1:nrow(cm), # Add vertical grid lines
                rowsep=1:nrow(cm), # Add horizontal grid lines
                sepcolor = "black", # Color gridlines black
                col = viridis::viridis_pal(),
                breaks = 20)
      dev.off()
    }
  )
  output$downloadData_csv <- downloadHandler(
    filename <- function(){
      paste("Predicted Labels.csv")
    },
    
    content = function(file) {
      dat<- res_anno
      write.csv(dat, file = file)
    }
  )
  output$downloadData_pbmc_csv <- downloadHandler(
    filename <- function(){
      paste("Predicted Labels.csv")
    },
    
    content = function(file) {
      dat<- res_anno
      write.csv(dat, file = file)
    }
  )
  output$downloadData_txt <- downloadHandler(
    filename <- function(){
      paste("Predicted Labels.txt")
    },
    
    content = function(file) {
      dat<- res_anno
      write.table(dat, file = file)
    }
  )
  output$downloadData_pbmc_txt <- downloadHandler(
    filename <- function(){
      paste("Predicted Labels.txt")
    },
    
    content = function(file) {
      dat<- res_anno
      write.table(dat, file = file)
    }
  )
}


shinyApp(ui, server)


