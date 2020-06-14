library(shiny)
library(shinydashboard)
library(shinythemes)
library(datasets)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(cluster)
library(factoextra)
library(clustertend)
library(NbClust)
library(fpc)
library(clValid)
library(pvclust)
library(dendextend)
library(gplots)
library(pheatmap)
library(d3heatmap)
library(FactoMineR)

theme_set(theme_bw())

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        
        # Application title
        dashboardHeader(title = "Project"),
        
        # Sidebar with a slider input for number of bins 
        dashboardSidebar( 
            sidebarMenu(
                menuItem("Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
                menuItem(text = 'Regression',
                         tabName = "regression",
                         icon = icon("clipboard"),
                         menuSubItem(text = "Simple Regression", 
                                     tabName = 'simpleregression',
                                     icon = icon('line-chart')),
                         menuSubItem(text = "Multiple Regression", 
                                     tabName = 'multipleregression',
                                     icon = icon('line-chart'))
                ),
                menuItem(text = 'Classification',
                         tabName = "classification",
                         icon = icon("clipboard"),
                         menuSubItem(text = "Logistic Regression", 
                                     tabName = 'logisticregression',
                                     icon = icon('line-chart'))
                ),
                menuItem(text = 'Clustering',
                         tabName = "clustering",
                         icon = icon("clipboard"),
                         menuSubItem(text = "Partitional Clustering", 
                                     tabName = 'partitionalclustering',
                                     icon = icon('line-chart')),
                         menuSubItem(text = "Hierarchical Clustering", 
                                     tabName = 'hierarchicalclustering',
                                     icon = icon('line-chart'))
                ),
                menuItem(text = 'Dimension Reduction',
                         tabName = "dimensionalityreduction",
                         icon = icon("clipboard"),
                         menuSubItem(text = "PCA", 
                                     tabName = 'pca',
                                     icon = icon('line-chart'))
                )
                
            )
            , collapsed = TRUE
        ),
        dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
            ),
            tabItems(
                tabItem(tabName = "dashboard",
                        # First Row
                        # fluidRow(
                        #     valueBox(2, "Number of Algorithms", icon = icon("bezier-curve"), color = "purple"),
                        #     valueBox(10 * 2, "Coffee", icon = icon("coffee"), color = "yellow"),
                        #     valueBox(5, "Team Members", icon = icon("users")),
                        # ),
                        fluidRow(
                            box(title = 'Regression', width = 3, htmlOutput('plot1'),
                                status = "info", solidHeader = T, collapsible = T),
                            box(title = 'Classification', width = 3, htmlOutput('plot2'),
                                status = "success", solidHeader = T, collapsible = T),
                            box(title = 'Clustering', width = 3, htmlOutput('plot3'),
                                status = "warning", solidHeader = T, collapsible = T),
                            box(title = 'Dimension Reduction', width = 3, htmlOutput('plot4'),
                                status = "danger", solidHeader = T, collapsible = T),
                            
                            column(10,offset = 1,
                                tags$img(src="images/dashboard.png", width="100%")
                            )
                        ),
                        
                        helpText("Note: The tool is in the beta version and is tested for only for specific datasets. Please refresh the tool if you find any discrepancy.")
                        
                ),
                ### Simple Regression starts here
                tabItem(tabName = "simpleregression",
                        style = "height:auto",
                        div(class = "well regression", "Simple Regression"),
                        fluidRow(class="uploadfeature",
                            # column allocation for widgets
                            column(6,
                                   fileInput("simpleregfile","Upload the file"), # fileinput() function is used to get the file upload contorl option
                                   helpText("Default max. file size is 5MB"),
                            ),
                            column(3,
                                   h5(helpText("Select the read.table parameters below")),
                                   checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                                   checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                   
                            ),
                            column(3,
                                   radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                            )
                            
                        ),
                        
                        conditionalPanel(
                            condition = "output.fileUploaded",
                            fluidRow(
                                # column allocation for widgets
                                column(5,
                                       uiOutput("varx")
                                       
                                ),
                                column(5,
                                       uiOutput("vary")
                                ),
                                column(2,
                                       actionButton("simpleregrun", "Run Model"),
                                )
                            )
                            
                        ),
                        
                        conditionalPanel(
                            condition = ("input.simpleregrun > 0"),
                            hr(),
                            fluidRow(
                                class="lrmodeloutput",
                                # column allocation for widgets
                                box(title = "Model Summary", width=6, solidHeader = TRUE,
                                    verbatimTextOutput("simpleregsum")
                                ),
                                box(title = "Visualization", width=6, solidHeader = TRUE,
                                    plotOutput("simpleregvis")
                                ),
                                
                                
                            )
                                
                            ),
                        
                        ),
                ### Simple Regression ends here
                
                ### Multiple Regression starts here
                tabItem(tabName = "multipleregression", 
                        div(class = "well regression", "Multiple Regression"),
                        fluidRow(class="uploadfeature",
                            # column allocation for widgets
                            column(6,
                                   fileInput("multiregfile","Upload the file"),
                                   helpText("Default max. file size is 5MB"),
                                   ),
                            column(3,
                                   h5(helpText("Select the read.table parameters below")),
                                   checkboxInput(inputId = 'multiheader', label = 'Header', value = TRUE),
                                   checkboxInput(inputId = "multistringAsFactors", "stringAsFactors", FALSE),
                                   ),
                            column(3,
                                   radioButtons(inputId = 'multisep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                   )
                            ),
                        
                        conditionalPanel(
                            condition = "output.multifileUploaded == true",
                            hr(),
                            fluidRow(
                                # column allocation for widgets
                                column(5,
                                       uiOutput("multivarx")
                                       
                                ),
                                column(5,
                                       uiOutput("multivary")
                                ),
                                column(2,
                                       actionButton("multiregrun", "Run Model"),
                                ),
                                column(5,
                                       textOutput("multiregvarx")
                                ),
                                column(5,
                                       textOutput("multiregvary")
                                ),
                            )
                            
                        ),
                
                        conditionalPanel(
                            condition = ("input.multiregrun > 0"),
                            
                            tags$div('style' = "clear:both;padding-top: 10px;padding-bottom: 10px;") ,
                            
                            hr(),
                            
                            fluidRow(
                                box(title = "Model Summary", width=12, solidHeader = TRUE,
                                    verbatimTextOutput("multiregsum")
                                )
                            )
                        ),
                       
                        ),
                ### Multiple Regression ends here
                
                ### Hierarchical Clustering starts here
                tabItem(tabName = "hierarchicalclustering",
                        div(class = "well cluster", "Hierarchical Clustering"),
                        fluidRow(class="uploadfeature",
                                 # column allocation for widgets
                                 column(6,
                                        fileInput("hcfile","Upload the file"),
                                        helpText("Default max. file size is 5MB"),
                                 ),
                                 column(3,
                                        h5(helpText("Select the read.table parameters below")),
                                        checkboxInput(inputId = 'hcheader', label = 'Header', value = TRUE),
                                        checkboxInput(inputId = "hcstringAsFactors", "stringAsFactors", FALSE),
                                 ),
                                 column(3,
                                        radioButtons(inputId = 'hcsep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                 )
                        ),
                        
                        conditionalPanel(
                            condition = "output.hcfileUploaded",
                            hr(),
                            fluidRow(
                                class="selectfeature",
                                # column allocation for widgets
                                column(6,
                                       uiOutput("hcx")
                                       
                                ),
                                column(3,
                                       h5(helpText("Scale the dataset")),
                                       checkboxInput(inputId = 'hcscale', label = 'Scale', value = TRUE)
                                ),
                                
                                column(2,
                                       actionButton("hcnumberclusters", "Compute the number of clusters"),
                                ),
                                
                                column(12,
                                       uiOutput("hcvarx")
                                )
                            )
                        ),
                        
                        conditionalPanel(
                            condition = "input.hcnumberclusters > 0",
                            hr(),
                            div(class = "well cluster", "Optimal Number of clusters"),
                            fluidRow(
                                
                                box(title = "The Best Number Of Clusters",solidHeader = TRUE, width = 12,
                                    plotOutput("hcnbclust", height = 350)
                                ),
                                box(title = "Elbow method",solidHeader = TRUE,
                                    plotOutput("hcelbowmethood", height = 350)
                                ),
                                
                                box(title = "Silhouette Methood",solidHeader = TRUE,
                                    plotOutput("hcsilhouettemethood", height = 350)
                                )
                            ),
                            
                            tags$div('style' = "clear:both;padding-top: 10px;padding-bottom: 10px;") ,
                            
                            fluidRow(
                                class="hcinput",
                                column(4,
                                       
                                       textInput("hcnumberofclusters", "Enter the number of clusters"),
                                ),
                                
                                column(4,
                                       actionButton("hcnumberofclustersaction", "Run Model"),
                                ),
                            ),
                            tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;")
                            
                        ),
                        
                        conditionalPanel(
                            condition = ("input.hcnumberofclustersaction > 0"),
                            hr(),
                            fluidRow(
                                class="hcoutput",
                                # column allocation for widgets
                                
                                ### Single Linkage
                                div(class = "well cluster", "Single Linkage"),
                                box(title = "Dendrogram - Single Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("singlelinkagedendrogram", height = 350)
                                ),
                                box(title = "Cluster Plot - Single Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("singlelinkagecluster", height = 350)
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;"),
                                
                                ### Complete Linkage
                                div(class = "well cluster", "Complete Linkage"),
                                box(title = "Dendrogram - Complete Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("completelinkagedendrogram", height = 350)
                                ),
                                box(title = "Cluster Plot - Complete Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("completelinkagecluster", height = 350)
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;"),
                                
                                ### Average Linkage
                                div(class = "well cluster", "Average Linkage"),
                                box(title = "Dendrogram - Average Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("averagelinkagedendrogram", height = 350)
                                ),
                                box(title = "Cluster Plot - Average Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("averagelinkagecluster", height = 350)
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;"),
                                
                                ### Ward Linkage
                                div(class = "well cluster", "Ward Linkage"),
                                box(title = "Dendrogram - Ward Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("wardlinkagedendrogram", height = 350)
                                ),
                                box(title = "Cluster Plot - Ward Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("wardlinkagecluster", height = 350)
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;")
                                
                            )
                        ),
                        
                        
                ),
                ### Hierarchical Clustering starts here
                
                ### Partitional Clustering starts here
                tabItem(tabName = "partitionalclustering",
                            div(class = "well cluster", "Partitional Clustering"),
                            fluidRow(class="uploadfeature",
                                # column allocation for widgets
                                column(6,
                                       fileInput("kmeansfile","Upload the file"),
                                       helpText("Default max. file size is 5MB"),
                                ),
                                column(3,
                                       h5(helpText("Select the read.table parameters below")),
                                       checkboxInput(inputId = 'kmeansheader', label = 'Header', value = TRUE),
                                       checkboxInput(inputId = "kmeansstringAsFactors", "stringAsFactors", FALSE),
                                ),
                                column(3,
                                       radioButtons(inputId = 'kmeanssep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                )
                            ),
                            conditionalPanel(
                                condition = "output.kmeansfileUploaded",
                                hr(),
                                fluidRow(
                                    class="selectfeature",
                                    # column allocation for widgets
                                    column(6,
                                           uiOutput("kmeansx")
                                           
                                    ),
                                    
                                    column(3,
                                           h5(helpText("Scale the dataset")),
                                           checkboxInput(inputId = 'kmeansscale', label = 'Scale', value = TRUE)
                                    ),
                                    
                                    column(2,
                                           actionButton("kmeansevaluate", "Evaluate the dataset"),
                                    ),
                                    column(12,
                                           uiOutput("kmeansvarx")
                                    )
                                    
                                ),
                            ),
                            conditionalPanel(
                                condition = ("input.kmeansevaluate == 1"),
                            hr(),
                                fluidRow(
                                    # column allocation for widgets
                                    column(6,
                                           uiOutput("Hopkins")
                                    ),
                                    column(6,
                                           plotOutput("kmeansheatmap")
                                    ),
                                ),
                                
                            ),
                           
                        conditionalPanel(
                            condition = ("input.getclusterbutton > 0"),
                            hr(),
                            fluidRow(
                                class="getnumberofcluster",
                                # column allocation for widgets
                                div(class = "well cluster", "Optimal Number of clusters"),
                                       box(title = "Elbow method",solidHeader = TRUE,
                                           plotOutput("elbowmethood", height = 250)
                                       ),
                                
                                       box(title = "Silhouette Methood",solidHeader = TRUE,
                                           plotOutput("silhouettemethood", height = 250)
                                       ),
                            ),
                                       
                            tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;") ,
                            fluidRow(
                                    class="kmeansinput",
                                    column(12,
                                           helpText("Enter the details for K-Means"),
                                    ),
                                column(4,
                                       
                                       textInput("numberofclusters", "Enter the number of clusters"),
                                ),
                                column(4,
                                       textInput("numberofnstarts", "Enter the number of nstart "),
                                ),
                                column(4,
                                       actionButton("numberofclustersaction", "Run K-Means Model"),
                                ),
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;") ,
                                fluidRow(
                                    class="kmedoidinput",
                                    column(12,
                                    helpText("Enter the details for K-Medoid"),
                                    ),
                                    column(4,
                                       textInput("kmedoidnumberofclusters", "Enter the number of clusters"),
                                ),
                                
                                column(4,
                                       actionButton("kmedoidnumberofclustersaction", "Run K-Medoid Model"),
                                )
                                ),
                            ),
                        
                        conditionalPanel(
                            condition = ("input.numberofclustersaction > 0"),
                            hr(),
                            fluidRow(
                                class="knnoutput",
                                # column allocation for widgets
                                div(class = "well cluster", "K-Means"),
                                column(6,
                                       verbatimTextOutput("knnoutputsummary") 
                                ),
                                box(title = "K-Means: Cluster Plot", width = 6, solidHeader = TRUE,
                                    plotOutput("knnoutputplot", height = 350)
                                ),
                                       
                            )
                        ),
                        conditionalPanel(
                            condition = ("input.kmedoidnumberofclustersaction > 0"),
                            hr(),
                            fluidRow(
                                class="knnoutput",
                                # column allocation for widgets
                                div(class = "well cluster", "K-Medoids"),
                                column(6,
                                       verbatimTextOutput("kmedoidsummary") 
                                ),
                                box(title = "K-Medoids: Cluster Plot", width = 6, solidHeader = TRUE,
                                    plotOutput("kmedoidplot", height = 350)
                                ),
                                
                            )
                        )
                        
                        
                        ),
                
                ### Partitional Clustering ends here
                
                ### Logistic Regression starts here
                tabItem(tabName = "logisticregression",
                        div(class = "well classification", "Logistic Regression"),
                        fluidRow(class="uploadfeature",
                                 # column allocation for widgets
                                 column(6,
                                        fileInput("lrfile","Upload the file"),
                                        helpText("Default max. file size is 5MB"),
                                 ),
                                 column(3,
                                        h5(helpText("Select the read.table parameters below")),
                                        checkboxInput(inputId = 'lrheader', label = 'Header', value = TRUE),
                                        checkboxInput(inputId = "lrstringAsFactors", "stringAsFactors", FALSE),
                                 ),
                                 column(3,
                                        radioButtons(inputId = 'lrsep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                 )
                        ),
                        
                        conditionalPanel(
                            condition = "output.lrfileUploaded",
                            hr(),
                            div(class = "well classification", "Explanatory Data Analysis"),
                            
                            fluidRow(
                                class="selectfeature",
                                # column allocation for widgets
                                column(5,
                                       uiOutput("lrx") 
                                ),
                                
                                
                                column(5,
                                       uiOutput("lry")
                                       
                                ),
                                
                                column(2,
                                       actionButton("lrmodelbutton", "Run the model"),
                                )
                                
                                
                            ),
                            hr(),
                            fluidRow(
                            box(title = "Structure", width = 6, solidHeader = TRUE,
                                verbatimTextOutput("edastr")
                            ),
                            box(title = "Summary", width = 6, solidHeader = TRUE,
                                verbatimTextOutput("edasummary")
                            )
                            
                            ),
                            
                        ),
                        
                        conditionalPanel(
                            condition = ("input.lrmodelbutton > 0"),
                            hr(),
                            #helpText("Output will be displayed here"),
                            fluidRow(
                                class="lrmodeloutput",
                                # column allocation for widgets
                                box(title = "Model Summary", width=12, solidHeader = TRUE,
                                    verbatimTextOutput("lrmodelsummary")
                                )
                                
                            )
                        ),
                        tags$div('style' = "clear:both;padding-top: 10px;padding-bottom: 10px;") ,
                        
                ),
                
                ### PCA starts here
                tabItem(tabName = "pca",
                        div(class = "well dimred", "PCA"),
                        fluidRow(class="uploadfeature",
                                 # column allocation for widgets
                                 column(6,
                                        fileInput("pcafile","Upload the file"), # fileinput() function is used to get the file upload contorl option
                                        helpText("Default max. file size is 5MB"),
                                 ),
                                 column(3,
                                        h5(helpText("Select the read.table parameters below")),
                                        checkboxInput(inputId = 'pcaheader', label = 'Header', value = TRUE),
                                        checkboxInput(inputId = "pcastringAsFactors", "stringAsFactors", FALSE),
                                        
                                 ),
                                 
                                 column(3,
                                        radioButtons(inputId = 'pcasep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                 )
                                
                                 
                        ),
                        conditionalPanel(
                            condition = "output.pcafileUploaded",
                            hr(),
                            fluidRow(
                                class="selectfeature",
                                # column allocation for widgets
                                column(6,
                                       uiOutput("pcax") 
                                ),
                                column(2,
                                       actionButton("pcavarbutton", "Run the model"),
                                )
                            )
                        ),
                        
                        conditionalPanel(
                            condition = ("input.pcavarbutton > 0"),
                            hr(),
                            #helpText("Output will be displayed here"),
                            fluidRow(
                                class="pcavaroutput",
                                # column allocation for widgets
                                box(title = "Eigenvalues", width=12, solidHeader = TRUE,
                                    verbatimTextOutput("pcaigenvalues")
                                ),
                                box(width=6, solidHeader = TRUE,
                                    plotOutput("pcascreetree")
                                ),
                                box(title = "Correlation circle", width=6, solidHeader = TRUE,
                                    plotOutput("pcacorrcircle")
                                ),
                                box(title = "Contributions of variables 1", width=6, solidHeader = TRUE,
                                    plotOutput("pcacontrivar1")
                                ),
                                box(title = "Contributions of variables 2", width=6, solidHeader = TRUE,
                                    plotOutput("pcacontrivar2")
                                )
                                
                            )
                        ),
                        tags$div('style' = "clear:both;padding-top: 10px;padding-bottom: 10px;") ,
                )
                ### PCA starts here
            ),
            
            
        )
    )
)