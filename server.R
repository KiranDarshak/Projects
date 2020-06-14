library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session){
    
    #### PCA starts here
    
    pcadata <- reactive({
        file1 <- input$pcafile
        if(is.null(file1)){return()} 
        df <- read.csv(file=file1$datapath, 
                       sep=input$pcasep, 
                       header = input$pcaheader, 
                       stringsAsFactors = input$pcastringAsFactors)
        
        return(df)
    })
    
    output$pcafileUploaded <- reactive({
        return(!is.null(pcadata()))
    })
    
    outputOptions(output, "pcafileUploaded", suspendWhenHidden=FALSE)
    
    output$pcax <- renderUI({
        selectInput("pcavarx",
                    "Select Variables",
                    choices=names(pcadata()),
                    multiple = T)
    })
    
    output$pcaigenvalues <- renderPrint({
        res.pca <- PCA(pcadata()[input$pcavarx], graph = FALSE,scale.unit = T)
        eig.val <- get_eigenvalue(res.pca)
        eig.val
    })
    
    output$pcascreetree <- renderPlot({
        res.pca <- PCA(pcadata()[input$pcavarx], graph = FALSE)
        eig.val <- get_eigenvalue(res.pca)
        fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
    })
    
    output$pcacorrcircle <- renderPlot({
        res.pca <- PCA(pcadata()[input$pcavarx], graph = FALSE)
        eig.val <- get_eigenvalue(res.pca)
        fviz_pca_var(res.pca, col.var = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                     repel = TRUE # Avoid text overlapping
        )
    })
    
    output$pcacontrivar1 <- renderPlot({
        res.pca <- PCA(pcadata()[input$pcavarx], graph = FALSE)
        eig.val <- get_eigenvalue(res.pca)
        fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
    })
    
    output$pcacontrivar2 <- renderPlot({
        res.pca <- PCA(pcadata()[input$pcavarx], graph = FALSE)
        eig.val <- get_eigenvalue(res.pca)
        fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
    })
    
    #### PCA starts here
    
    #### Logistic Regression starts here
    lrdata <- reactive({
        file1 <- input$lrfile
        if(is.null(file1)){return()} 
        df <- read.csv(file=file1$datapath, 
                       sep=input$lrsep, 
                       header = input$lrheader, 
                       stringsAsFactors = input$lrstringAsFactors,
                       na.strings=c(""),
                       fileEncoding="UTF-8-BOM")
        col_names <- sapply(df, function(col) length(unique(col)) < 4)
        df[ , col_names] <- lapply(df[ , col_names] , factor)
        
        return(df)
    })
    
    output$lrfileUploaded <- reactive({
        return(!is.null(lrdata()))
    })
    
    outputOptions(output, "lrfileUploaded", suspendWhenHidden=FALSE)
    
    output$edastr <- renderPrint({
        str(lrdata())
    })
    
    output$edasummary <- renderPrint({
        summary(lrdata())
    })
    
    output$edadata <- renderTable({
        head(lrdata())
    })
    
    
    output$lrx <- renderUI({
        selectInput("lrvarx",
                    "Select Independent Variable",
                    choices=names(lrdata()),
                    multiple = T)
    })
    
    output$lry <- renderUI({
        selectInput("lrvary",
                    "Select Dependent Variable",
                    choices=names(lrdata())
                    )
    })
    
    
    lrmodel <- reactive({
        req(lrdata())
        if(input$lrmodelbutton == 0)
            return()
        else
            lrmodel <- glm(as.formula(paste(input$lrvary," ~ ",paste(input$lrvarx,collapse="+"))),
                       family = binomial,
                       data = lrdata())
            return(lrmodel)
        
    })
    
    output$lrmodelsummary <- renderPrint({
        summary(lrmodel())
    })
    
   
    #### Logistic Regression starts here
    
    #### Hierarchical Clustering starts here
    
    hcdata <- reactive({
        file1 <- input$hcfile
        if(is.null(file1)){return()} 
        df <- read.csv(file=file1$datapath, 
                       sep=input$hcsep, 
                       header = input$hcheader, 
                       stringsAsFactors = input$hcstringAsFactors)
        return(df)
    })
    
    output$hcfileUploaded <- reactive({
        return(!is.null(hcdata()))
    })
    
    outputOptions(output, "hcfileUploaded", suspendWhenHidden=FALSE)
    
    # Pulling the list of variable for choice of variable x
    output$hcx <- renderUI({
        selectInput("hcvarx",
                    "Select the inputs for the Independent Variable",
                    choices=names(hcdata()),
                    multiple = T)
    })
    
    output$hcvarx <- renderUI({
        hcxvals <- paste(input$hcvarx, collapse = ", ")
        tags$p(paste("Your ideal(s) value(s) are", hcxvals), class="")
    })
    
    output$hcelbowmethood <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        fviz_nbclust(df.data, kmeans, method = "wss")
        
    })
    
    output$hcsilhouettemethood <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        fviz_nbclust(df.data, pam, method = "silhouette")
        
    })
    
    output$hcnbclust <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        nb.res <- NbClust(df.data, distance = "euclidean", min.nc = 2,
                          max.nc = 10, method = "kmeans")
        
        fviz_nbclust(nb.res)
        
    })
    
    #### Single Linkage Dendrogram
    output$singlelinkagedendrogram <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        single.res.hc <- hclust(d = res.dist, method = "single")
        
        fviz_dend(single.res.hc, k = as.numeric(input$hcnumberofclusters),
                  cex = 0.5,
                  k_colors = "jco",
                  color_labels_by_k = TRUE,
                  rect = TRUE
        )
        
    })
    
    #### Single Linkage Cluster
    output$singlelinkagecluster <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        single.res.hc <- hclust(d = res.dist, method = "single")
        
        fviz_cluster(list(data = df.data, cluster = cutree(single.res.hc, k = as.numeric(input$hcnumberofclusters))),
                     palette = "jco", 
                     ellipse.type = "convex",
                     repel = TRUE,
                     show.clust.cent = FALSE,
                     ggtheme = theme_minimal()
        )
        
    })
    
    #### Complete Linkage Dendrogram
    output$completelinkagedendrogram <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        complete.res.hc <- hclust(d = res.dist, method = "complete")
        
        fviz_dend(complete.res.hc, k = as.numeric(input$hcnumberofclusters),
                  cex = 0.5,
                  k_colors = "jco",
                  color_labels_by_k = TRUE,
                  rect = TRUE
        )
        
    })
    
    #### Complete Linkage Cluster
    output$completelinkagecluster <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        complete.res.hc <- hclust(d = res.dist, method = "complete")
        
        fviz_cluster(list(data = df.data, cluster = cutree(complete.res.hc, k = as.numeric(input$hcnumberofclusters))),
                     palette = "jco", 
                     ellipse.type = "convex",
                     repel = TRUE,
                     show.clust.cent = FALSE,
                     ggtheme = theme_minimal()
        )
        
    })
    
    #### Average Linkage Dendrogram
    output$averagelinkagedendrogram <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        average.res.hc <- hclust(d = res.dist, method = "average")
        
        fviz_dend(average.res.hc, k = as.numeric(input$hcnumberofclusters),
                  cex = 0.5,
                  k_colors = "jco",
                  color_labels_by_k = TRUE,
                  rect = TRUE
        )
        
    })
    
    #### Average Linkage Cluster
    output$averagelinkagecluster <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        average.res.hc <- hclust(d = res.dist, method = "average")
        
        fviz_cluster(list(data = df.data, cluster = cutree(average.res.hc, k = as.numeric(input$hcnumberofclusters))),
                     palette = "jco", 
                     ellipse.type = "convex",
                     repel = TRUE,
                     show.clust.cent = FALSE,
                     ggtheme = theme_minimal()
        )
        
    })
    
    #### ward.D2 Linkage Dendrogram
    output$wardlinkagedendrogram <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        ward.res.hc <- hclust(d = res.dist, method = "ward.D2")
        
        fviz_dend(ward.res.hc, k = as.numeric(input$hcnumberofclusters),
                  cex = 0.5,
                  k_colors = "jco",
                  color_labels_by_k = TRUE,
                  rect = TRUE
        )
        
    })
    
    #### ward.D2 Linkage Cluster
    output$wardlinkagecluster <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        ward.res.hc <- hclust(d = res.dist, method = "ward.D2")
        
        fviz_cluster(list(data = df.data, cluster = cutree(ward.res.hc, k = as.numeric(input$hcnumberofclusters))),
                     palette = "jco", 
                     ellipse.type = "convex",
                     repel = TRUE,
                     show.clust.cent = FALSE,
                     ggtheme = theme_minimal()
        )
        
    })
    
    
    #### Hierarchical Clustering Ends here
    
    #### Partitional Clustering starts here
    kmeansdata <- reactive({
        file1 <- input$kmeansfile
        if(is.null(file1)){return()} 
        df <- read.csv(file=file1$datapath, 
                       sep=input$kmeanssep, 
                       header = input$kmeansheader, 
                       stringsAsFactors = input$kmeansstringAsFactors)
        return(df)
    })
    
    output$kmeansfileUploaded <- reactive({
        return(!is.null(kmeansdata()))
    })
    
    outputOptions(output, "kmeansfileUploaded", suspendWhenHidden=FALSE)
    
    # Pulling the list of variable for choice of variable x
    output$kmeansx <- renderUI({
        selectInput("kmeansvarx",
                    "Select the inputs for the Independent Variable",
                    choices=names(kmeansdata()),
                    multiple = T)
    })
    
    output$kmeansvarx <- renderUI({
        kmeansxvals <- paste(input$kmeansvarx, collapse = ", ")
        tags$p(paste("Your ideal(s) value(s) are", kmeansxvals), class="")
    })
    
    output$Hopkins <- renderUI({
        req(kmeansdata())
        if(input$kmeansevaluate == 0)
            return()
        else
            if(input$kmeansscale == TRUE)
                df.data <- scale(kmeansdata()[,input$kmeansvarx])
            else
                df.data <- kmeansdata()[,input$kmeansvarx]
                
            res <- get_clust_tendency(df.data, n = nrow(df.data)-1, graph = FALSE)
            
            
            if(res$hopkins_stat > 0.5){
                withTags({
                    div(
                        p("The null and the alternative hypotheses are defined as follow:"),
                        ul(
                            li("Null hypothesis: the data set D is uniformly distributed (i.e., no meaningful clusters)"),
                            li("Alternative hypothesis: the data set D is not uniformly distributed (i.e., contains meaningful clusters)")
                            
                        ),
                        p(paste("The Hopkins statistic states ", round(res$hopkins_stat, 3)), class = "hopkinssuccess"),
                        actionButton("getclusterbutton", "Compute the number of clusters", class="getclusterbutton")
                    )
                    
                })
            }
            else{
                withTags({
                    div(
                        p("The null and the alternative hypotheses are defined as follow:"),
                        ul(
                            li("Null hypothesis: the data set D is uniformly distributed (i.e., no meaningful clusters)"),
                            br(),
                            li("Alternative hypothesis: the data set D is not uniformly distributed (i.e., contains meaningful clusters)")
                            
                        ),
                        br(),
                        p(paste("The Hopkins statistic states ", round(res$hopkins_stat, 3)), class = "hopkinerror")
                    )
                    
                })
                
            }
    })
    
    output$kmeansheatmap <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        fviz_pca_ind(prcomp(df.data), title = "PCA - Dataset",
                     geom = "point", ggtheme = theme_classic(),
                     legend = "bottom")
        
    })
    
    output$elbowmethood <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        fviz_nbclust(df.data, kmeans, method = "wss")
            
    })
    
    output$silhouettemethood <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        fviz_nbclust(df.data, pam, method = "silhouette")
        
    })
    
    output$knnoutputsummary <- renderPrint({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        input$numberofclustersaction
        
        km.res <- kmeans(df.data, as.numeric(input$numberofclusters), nstart = as.numeric(input$numberofnstarts) )
        
        print(km.res)
        
    })
    
    output$knnoutputplot <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        km.res <- kmeans(df.data, as.numeric(input$numberofclusters), nstart = as.numeric(input$numberofnstarts) )
        
        fviz_cluster(km.res, data = df.data,
                     palette = 'jco', 
                     ellipse.type = "euclid", # Concentration ellipse
                     star.plot = TRUE, # Add segments from centroids to items
                     repel = TRUE, # Avoid label overplotting (slow)
                     ggtheme = theme_minimal()
        )
        
    })
    
    output$kmedoidsummary <- renderPrint({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        
        pam.res <- pam(df.data, as.numeric(input$kmedoidnumberofclusters))
        
        print(pam.res)
        
    })
    
    output$kmedoidplot <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        pam.res <- pam(df.data, as.numeric(input$kmedoidnumberofclusters))
        
        fviz_cluster(pam.res, 
                     palette = "jco", # color palette
                     ellipse.type = "t", # Concentration ellipse
                     repel = TRUE, # Avoid label overplotting (slow),
                     ggtheme = theme_classic()
        )
        
    })
    
    #### Partitional Clustering ends here
    
    #### Multi Regression starts here
    multiregdata <- reactive({
        file1 <- input$multiregfile
        if(is.null(file1)){return()} 
        df <- read.csv(file=file1$datapath, 
                       sep=input$multisep, 
                       header = input$multiheader, 
                       stringsAsFactors = input$multistringAsFactors)
        return(df)
    })
    
    output$multifileUploaded <- reactive({
        return(!is.null(multiregdata()))
    })
    
    outputOptions(output, "multifileUploaded", suspendWhenHidden=FALSE)
    
    # Pulling the list of variable for choice of variable x
    output$multivarx <- renderUI({
        selectInput("multiregvarx",
                    "Select the inputs for the Independent Variable",
                    choices=names(multiregdata()),
                    multiple = T)
    })
    
    # Pulling the list of variable for choice of variable y
    output$multivary <- renderUI({
        selectInput("multiregvary", 
                    "Select the inputs for the Dependent Variable", 
                    choices=names(multiregdata()))
        
    })
    
    output$multiregvarx <- renderText({
        xvals <- paste(input$multiregvarx, collapse = ", ")
        paste("Your ideal(s) value(s) are", xvals)
        #paste(input$multiregvarx)
        #multiregdata()[,input$multiregvarx]
        
    })
    
    output$multiregvary <- renderText({
        yvals <- paste(input$multiregvary, collapse = " ")
        paste("Your ideal(s) value(s) are", yvals)
        #paste(input$multiregvary)
        #multiregdata()[,input$multiregvary]
    })
    
    output$multiregsum <- renderPrint({
        req(multiregdata())
        if(input$multiregrun == 0)
            return()
        else
            x <- multiregdata()[,input$multiregvarx]
            y <- multiregdata()[,input$multiregvary]
            lm1 <- lm(as.formula(paste(input$multiregvary," ~ ",paste(input$multiregvarx,collapse="+"))),data=multiregdata())
            #lm1 <- lm(multiregdata()[,input$multiregvary] ~ multiregdata()[,input$multiregvarx], data = multiregdata())
            summary(lm1)
    })
    
    output$multiregdata <- renderTable({
        req(multiregdata())
        if(input$multiregrun == 0)
            return()
        else
            head(multiregdata())
    })
    
    #### Multi Regression starts here
    
    #### Simple Regression starts here
    simpleregdata <- reactive({
        file1 <- input$simpleregfile
        if(is.null(file1)){return()} 
        df <- read.csv(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
        return(df)
    })
    
    output$fileUploaded <- reactive({
        return(!is.null(simpleregdata()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    
    # Pulling the list of variable for choice of variable x
    output$varx <- renderUI({
        selectInput("simpleregvarx",
                    "Select the input for the Independent Variable",
                    choices=names(simpleregdata()))
    })
    
    # Pulling the list of variable for choice of variable y
    output$vary <- renderUI({
        selectInput("simpleregvary", 
                    "Select the input for the Dependent Variable", 
                    choices=names(simpleregdata()))
        
    })
    
    
    output$simpleregvarx <- renderText({
        paste(input$simpleregvarx)
    })
    
    output$simpleregvary <- renderText({
        paste(input$simpleregvary)
    })
    
    output$simpleregsum <- renderPrint({
        req(simpleregdata())
        if(input$simpleregrun == 0)
            return()
        else
            x <- simpleregdata()[,input$simpleregvarx]
            y <- simpleregdata()[,input$simpleregvary]
            lm1 <- lm(as.formula(paste(input$simpleregvary," ~ ",paste(input$simpleregvarx,collapse="+"))),data=simpleregdata())
            summary(lm1)
    })
    
    output$simpleregvis <- renderPlot({
        req(simpleregdata())
        if(input$simpleregrun == 0)
            return()
        else
            xvar <- simpleregdata()[,input$simpleregvarx]
            yvar <- simpleregdata()[,input$simpleregvary]
            
            xvarlab <- names(simpleregdata()[,input$simpleregvarx])
            yvarlab <- names(simpleregdata()[,input$simpleregvary])
            
            ggplot(simpleregdata(), aes(x = xvar, y = yvar)) +
                geom_point() +
                xlab(paste(input$simpleregvarx)) +
                ylab(paste(input$simpleregvary)) +
                stat_smooth(method = lm)
    })
    
    output$simpleregdata <- renderTable({
        req(simpleregdata())
        if(input$simpleregrun == 0)
            return()
        else
            head(simpleregdata())
    })
    
    #### Simple Regression ends here
    
    output$plot1 <- renderText({
        paste("<ul>
                    <li>Simple Regression</li>
                    <li>Multiple Regression</li>
              </ul>"
              )
    })
    
    output$plot2 <- renderText({
        paste("<ul>
                    <li>Logistic Regression</li>
              </ul>"
              )
    })
    output$plot3 <- renderText({
        paste("<ul>
                    <li>Partitional Clustering</li>
                    <li>Hierarchical Clustering</li>
              </ul>"
              )
    })
    output$plot4 <- renderText({
        paste("<ul>
                    <li>PCA</li>
              </ul>"
        )
    })
    
})