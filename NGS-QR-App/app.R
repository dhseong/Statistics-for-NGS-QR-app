# Library
library(shiny)
library(curl)
library(ggplot2)
library(pastecs)
library(doBy)
library(psych)
library(descriptr)

# Import data using REST API
source('./fetch.R')
data <- fetchData()



# ==============================================================================
# ui
# ==============================================================================
ui <- fluidPage(
    titlePanel(""),
    headerPanel(""),
    
    # --------------------------------------------------------------------------
    # sidebar
    # --------------------------------------------------------------------------
    sidebarLayout(
        sidebarPanel(
            div(style = "font-size:16px;",
                tags$style(type='text/css', 
                           ".selectize-input { font-size: 14px; line-height: 14px; }
                            .selectize-dropdown { font-size: 14px; line-height: 14px; }"),
        # ----------------------------------------------------------------------
        # graphSelected
        # ----------------------------------------------------------------------
                selectInput(
                    inputId = "graphSelected",
                    label = "1. Select graph:",
                    choices = c("Density", "Boxplot", "PR Score", "Summary")
                ),
    
        # ----------------------------------------------------------------------
        # datasetSelected
        # ----------------------------------------------------------------------
                conditionalPanel(
                    condition = "input.graphSelected === 'Density'",
                    selectInput(
                        inputId = "datasetSelected1",
                        label = "2. Select a dataset:",
                        choices = c("OD 260/280", "OD 260/230", "DNA Integrity",
                            "Library Input Amount", "Library Insert Size", "Read Length",
                            "Total Reads", "Mean Coverage", "Uniformity",
                            "on-Target-Rate", "Q30", "PR Score"
                        )
                    )
                ),
                conditionalPanel(
                    condition = "input.graphSelected === 'Boxplot' ",
                    selectInput(
                        inputId = "datasetSelected2",
                        label = "2. Select a dataset:",
                        choices = c("OD 260/280", "OD 260/230", "DNA Integrity",
                                    "Library Input Amount", "Library Insert Size", "Read Length",
                                    "Total Reads", "Mean Coverage", "Uniformity",
                                    "on-Target-Rate", "Q30", "PR Score"
                        )
                    )
                ),
    
        # ----------------------------------------------------------------------
        # specimenSelected
        # ----------------------------------------------------------------------
                conditionalPanel(
                    condition = "input.graphSelected === 'Density'",
                    selectInput(
                        inputId = "specimenSelected1",
                        label = "3. Select a specimen type:",
                        choices = c("Fresh", "FFPE", "Cell Line")
                    )
                ),
                conditionalPanel(
                    condition = "input.graphSelected === 'Boxplot' ",
                    selectInput(
                        inputId = "specimenSelected2",
                        label = "3. Select a specimen type:",
                        choices = c("Fresh", "FFPE", "Cell Line")
                    )
                ),
                conditionalPanel(
                    condition = "input.graphSelected === 'PR Score'",
                    selectInput(
                        inputId = "specimenSelected3",
                        label = "2. Select a specimen type:",
                        choices = c("Fresh", "FFPE")
                    )
                ),
    
        # ----------------------------------------------------------------------
        # idSelected
        # ----------------------------------------------------------------------
                tags$style("#idSelected1 {font-size: 14px; color: #00acc1;}"),
                conditionalPanel(
                    condition = "input.graphSelected === 'Density'",
                    uiOutput("idSelected1")
                ),
                tags$style("#idSelected2 {font-size: 14px; color: #00acc1;}"),
                conditionalPanel(
                    condition = "input.graphSelected === 'Boxplot'",
                    uiOutput("idSelected2")
                ),
                tags$style("#idSelected3 {font-size: 14px; color: #00acc1;}"),
                conditionalPanel(
                    condition = "input.graphSelected === 'PR Score'",
                    uiOutput("idSelected3")
                )
            )
        ),
    
    # --------------------------------------------------------------------------
    # mainPanel
    # --------------------------------------------------------------------------
        mainPanel(
            conditionalPanel(
                condition = "input.graphSelected === 'Density'",
                plotOutput("density"),
                verbatimTextOutput("stat1")
            ),
            conditionalPanel(
                condition = "input.graphSelected === 'Boxplot'",
                plotOutput("boxplot"),
                verbatimTextOutput("stat2")
            ),
            conditionalPanel(condition = "input.graphSelected === 'PR Score'",
                             plotOutput("prscore")),
            conditionalPanel(condition = "input.graphSelected === 'Summary'",
                             tableOutput("summary"))
        )
    ))


# ==============================================================================
# server
# ==============================================================================
server <- function(input, output) {

    # --------------------------------------------------------------------------
    # Set variables
    #
    # input: input$datasetSelected1, input$datasetSelected2
    # output: datasetSelected1(), datasetSelected2()
    # --------------------------------------------------------------------------
    datasetSelected1 <- renderText({
        req(input$datasetSelected1)
        
        if (input$datasetSelected1 == "OD 260/280") return("od260280")
        else if (input$datasetSelected1 == "OD 260/230") return("od260230")
        else if (input$datasetSelected1 == "DNA Integrity") return("dnaIntegrity")
        else if (input$datasetSelected1 == "Library Input Amount") return("libraryInputAmount")
        else if (input$datasetSelected1 == "Library Insert Size") return("libraryInsertSize")
        else if (input$datasetSelected1 == "Read Length") return("readLength")
        else if (input$datasetSelected1 == "Total Reads") return("totalReads")
        else if (input$datasetSelected1 == "Mean Coverage") return("meanCoverage")
        else if (input$datasetSelected1 == "Uniformity") return("uniformity")
        else if (input$datasetSelected1 == "on-Target-Rate") return("onTargetRate")
        else if (input$datasetSelected1 == "Q30") return("q30")
        else if (input$datasetSelected1 == "PR Score") return("prScore")
    })
    
    datasetSelected2 <- renderText({
        req(input$datasetSelected2)
        
        if (input$datasetSelected2 == "OD 260/280") return("od260280")
        else if (input$datasetSelected2 == "OD 260/230") return("od260230")
        else if (input$datasetSelected2 == "DNA Integrity") return("dnaIntegrity")
        else if (input$datasetSelected2 == "Library Input Amount") return("libraryInputAmount")
        else if (input$datasetSelected2 == "Library Insert Size") return("libraryInsertSize")
        else if (input$datasetSelected2 == "Read Length") return("readLength")
        else if (input$datasetSelected2 == "Total Reads") return("totalReads")
        else if (input$datasetSelected2 == "Mean Coverage") return("meanCoverage")
        else if (input$datasetSelected2 == "Uniformity") return("uniformity")
        else if (input$datasetSelected2 == "on-Target-Rate") return("onTargetRate")
        else if (input$datasetSelected2 == "Q30") return("q30")
        else if (input$datasetSelected2 == "PR Score") return("prScore")
    })

    # --------------------------------------------------------------------------
    # Set variables
    #
    # input: input$specimenSelected1, input$specimenSelected3
    # output: specimenInput1(), specimenInput3()
    # --------------------------------------------------------------------------        
    specimenInput1 <- reactive({
        req(input$specimenSelected1)
        
        data[(data$specimenType == input$specimenSelected1),]
    })
    
    specimenInput3 <- reactive({
        req(input$specimenSelected3)
        
        data[(data$specimenType == input$specimenSelected3),]
    })

    # --------------------------------------------------------------------------
    # Set variables
    #
    # input: input$specimenSelected1, input$specimenSelected2, 
    #        datasetSelected1(), datasetSelected2()
    # output: datasetInput1(), datasetInput2()
    # --------------------------------------------------------------------------
    datasetInput1 <- reactive({
        req(input$specimenSelected1)
        
        if (input$specimenSelected1 == "All")
            return(na.omit(data[, c(datasetSelected1())]))
        else if (input$specimenSelected2 != "All")
            return(na.omit(data[which(data$specimenType == input$specimenSelected1), c(datasetSelected1())]))
    })
    
    datasetInput2 <- reactive({
        req(input$specimenSelected2)
        
        if (input$specimenSelected2 == "All")
            return(na.omit(data[, c(datasetSelected2())]))
        else if (input$specimenSelected2 != "All")
            return(na.omit(data[which(data$specimenType == input$specimenSelected2), c(datasetSelected2())]))
    })

    # --------------------------------------------------------------------------
    # Set variables
    #
    # input: input$idSelected1, input$idSelected2, input$idSelected3
    # output: idSelected1(), idSelected2(), idSelected3()
    # --------------------------------------------------------------------------        
    idSelected1 <- renderText({
        req(input$idSelected1)
        
        input$idSelected1
    })
    
    idSelected2 <- renderText({
        req(input$idSelected2)
        
        input$idSelected2
    })
    
    idSelected3 <- renderText({
        req(input$idSelected3)
        
        input$idSelected3
    })
    
    
    # --------------------------------------------------------------------------
    # Set variables
    #
    # input: idSelected1(), datasetSelected1()
    # output: dataSelected1(), dataSelected2(), dataSelected3()
    # --------------------------------------------------------------------------
    dataSelected1 <- reactive({
        req(datasetSelected1())
        
        data[which(data$identifier == idSelected1()), c(datasetSelected1())]
    })

    dataSelected2 <- reactive({
        req(datasetSelected2())
        
        data[which(data$identifier == idSelected2()), c(datasetSelected2())]
    })
    
    dataSelected3 <- reactive({
        req(datasetSelected3())
        
        data[which(data$identifier == idSelected3()), c(datasetSelected3())]
    })

    # --------------------------------------------------------------------------
    # Set variables
    #
    # input: idSelected3()
    # output: uniformitySelected()
    # --------------------------------------------------------------------------
    uniformitySelected <- reactive({
        req(idSelected3())
        
        data[which(data$identifier == idSelected3()), c("uniformity")]
    })
    
    # --------------------------------------------------------------------------
    # Set variables
    #
    # input: idSelected3()
    # output: meanCoverageSelected()
    # --------------------------------------------------------------------------
    meanCoverageSelected <- reactive({
        req(idSelected3())
        
        data[which(data$identifier == idSelected3()), c("meanCoverage")]
    })
    

    # --------------------------------------------------------------------------
    # Set output
    #
    # idSelected1, idSelected2, idSelected3
    # --------------------------------------------------------------------------
    output$idSelected1 <- renderUI({
        selectInput(
            inputId = "idSelected1",
            label = "Order ID:",
            choices = data[which(data$specimenType == input$specimenSelected1), c("identifier")],
            selected = NULL
        )
    })
    
    output$idSelected2 <- renderUI({
        selectInput(
            inputId = "idSelected2",
            label = "Order ID:",
            choices = data[which(data$specimenType == input$specimenSelected2), c("identifier")],
            selected = NULL
        )
    })
    
    output$idSelected3 <- renderUI({
        selectInput(
            inputId = "idSelected3",
            label = "Order ID:",
            choices = data[which(data$specimenType == input$specimenSelected3), c("identifier")],
            selected = NULL
        )
    })
    
    # --------------------------------------------------------------------------
    # Density Plot
    # --------------------------------------------------------------------------
    output$density <- renderPlot({
        obj <- specimenInput1()
        dataset <- datasetSelected1()
        annotation <- data.frame(
            x = dataSelected1(),
            y = Inf,
            label = idSelected1()
        )
        
        # threshold <- data.frame(matrix(ncol=4, nrow=0))
        # colnames(threshold) <- c("d","x1", "x2", "label")
        
        threshold <- data.frame("dataset"=character(0),"x1"=numeric(0), "x2"=numeric(0), "label"=character(0))
        
        threshold <- rbind(threshold, data.frame("dataset"="od260280", "x1"=1.8, "x2"=2.1, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="od260230", "x1"=1.8, "x2"=2.5, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="dnaIntegrity", "x1"=350, "x2"=Inf, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="libraryInputAmount", "x1"=200, "x2"=Inf, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="libraryInsertSize", "x1"=150, "x2"=200, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="readLength", "x1"=-Inf, "x2"=Inf, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="totalReads", "x1"=-Inf, "x2"=Inf, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="meanCoverage", "x1"=200, "x2"=Inf, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="uniformity", "x1"=-Inf, "x2"=Inf, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="onTargetRate", "x1"=-Inf, "x2"=Inf, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="q30", "x1"=-Inf, "x2"=Inf, "label"="Threshold"))
        threshold <- rbind(threshold, data.frame("dataset"="prScore", "x1"=80, "x2"=Inf, "label"="Threshold"))
        
        # min(obj[dataset], na.rm = TRUE
        # max(obj[dataset], na.rm = TRUE)
        
        if (is.infinite(threshold[threshold$dataset==dataset,]$x1)) {
            ggplot(obj) + 
                geom_density(aes_string(dataset), na.rm = TRUE) +
                xlim(c(min(obj[dataset], na.rm = TRUE) * 0.9, max(obj[dataset], na.rm = TRUE) * 1.1)) +
                geom_vline(xintercept = dataSelected1(), colour = "#00acc1") +
                geom_text(data=annotation, aes(x=x, y=y, label=label), size=5, col="#00acc1", hjust = 0, vjust = 1) + 
                theme_bw() +
                theme(
                    axis.title = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15),
                    legend.title = element_blank(),
                    legend.key = element_blank(),
                    legend.text = element_text(size = 15),
                    plot.title = element_text(hjust = 0.5, size = 17)
                ) +
                ggtitle("Kernal Density Function") +
                labs(y = "Density")    
        } else if (is.infinite(threshold[threshold$dataset==dataset,]$x2)) {
            ggplot(obj) + 
                geom_density(aes_string(dataset), na.rm = TRUE) +
                xlim(c(min(obj[dataset], na.rm = TRUE) * 0.9, max(obj[dataset], na.rm = TRUE) * 1.1)) +
                geom_vline(data=threshold[threshold$dataset==dataset,], aes(xintercept=x1), colour = "#FF0000") +
                geom_segment(data=threshold[threshold$dataset==dataset,], aes(x=x1, xend=Inf, y=0, yend=0), col="#FF0000", arrow=arrow(length=unit(0.03, "npc"), ends="last")) +
                geom_vline(xintercept = dataSelected1(), colour = "#00acc1") +
                geom_text(data=threshold[threshold$dataset==dataset,], aes(x=x1, y=0, label=label), size=5, col="#FF0000", hjust = 0, vjust = 0) +
                geom_text(data=annotation, aes(x=x, y=y, label=label), size=5, col="#00acc1", hjust = 0, vjust = 1) +
                theme_bw() +
                theme(
                    axis.title = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15),
                    legend.title = element_blank(),
                    legend.key = element_blank(),
                    legend.text = element_text(size = 15),
                    plot.title = element_text(hjust = 0.5, size = 17)
                ) +
                ggtitle("Kernal Density Function") +
                labs(y = "Density")    
        } else {
            ggplot(obj) + 
                geom_density(aes_string(dataset), na.rm = TRUE) +
                xlim(c(min(obj[dataset], na.rm = TRUE) * 0.9, max(obj[dataset], na.rm = TRUE) * 1.1)) +
                geom_vline(data=threshold[threshold$dataset==dataset,], aes(xintercept=x1), colour = "#FF0000") +
                geom_vline(data=threshold[threshold$dataset==dataset,], aes(xintercept=x2), colour = "#FF0000") +
                geom_segment(data=threshold[threshold$dataset==dataset,], aes(x=x1, xend=x2, y=0, yend=0), col="#FF0000", arrow=arrow(length=unit(0.03, "npc"), ends="both")) +
                geom_vline(xintercept = dataSelected1(), colour = "#00acc1") +
                geom_text(data=threshold[threshold$dataset==dataset,], aes(x=(x1+x2)/2, y=0, label=label), size=5, col="#FF0000", vjust = 0) +
                geom_text(data=annotation, aes(x=x, y=y, label=label), size=5, col="#00acc1", hjust = 0, vjust = 1) + 
                theme_bw() +
                theme(
                    axis.title = element_text(size = 15),
                    axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15),
                    legend.title = element_blank(),
                    legend.key = element_blank(),
                    legend.text = element_text(size = 15),
                    plot.title = element_text(hjust = 0.5, size = 17)
                ) +
                ggtitle("Kernal Density Function") +
                labs(y = "Density")    
        }
    })
    
    # --------------------------------------------------------------------------
    # Box Plot
    # --------------------------------------------------------------------------
    output$boxplot <- renderPlot({
        boxplot(datasetInput2(), main = 'Boxplot')
        points(dataSelected2(),
               col = "#00acc1",
               pch = 16,
               cex = 1.5)
        
        threshold = c(-Inf, Inf)
        if (datasetSelected2() == "od260280") threshold = c(1.8, 2.1)
        else if (datasetSelected2() == "od260230") threshold = c(1.8, 2.5)
        else if (datasetSelected2() == "dnaIntegrity") threshold = c(350, Inf)
        else if (datasetSelected2() == "libraryInputAmount") threshold = c(200, Inf)
        else if (datasetSelected2() == "libraryInsertSize") threshold = c(150, 200)
        else if (datasetSelected2() == "meanCoverage") threshold = c(200, Inf)
        else if (datasetSelected2() == "prScore") threshold = c(80, Inf)
        
        if (all.equal(threshold, c(-Inf, Inf)) != TRUE) {
            if (threshold[1] == -Inf) threshold[1] = min(datasetInput2(), na.rm = TRUE)
            if (threshold[2] == Inf) threshold[2] = max(datasetInput2(), na.rm = TRUE)
            abline(h = threshold, col = "#FF0000", lty = 2)
            axis(c(2, 2), threshold, threshold, col = "#FF0000")
        }
    })
    
    
    # --------------------------------------------------------------------------
    # PR score
    # --------------------------------------------------------------------------
    output$prscore <- renderPlot({
        obj3 <- specimenInput3()
        annotation3 <- data.frame(
            x1 = uniformitySelected()-0.1,
            x2 = uniformitySelected(),
            y1 = meanCoverageSelected()+300,
            y2 = meanCoverageSelected(),
            label = idSelected3()
        )
        ggplot(obj3, aes(uniformity, meanCoverage)) +
            geom_point(aes(colour = prScore), size = 2) +
            geom_point(data = subset(obj3, prScore < 100), aes(colour = prScore), size = 2) +
            geom_point(data = subset(obj3, prScore < 90), aes(colour = prScore), size = 2) +
            geom_point(data = subset(obj3, prScore < 80), aes(colour = prScore), size = 2) +
            scale_colour_gradient(limits = c(80, 100), low = "red", high = "forestgreen", na.value = "red") +
            geom_hline(yintercept = meanCoverageSelected(), colour = "#00acc1") +
            geom_vline(xintercept = uniformitySelected(), colour = "#00acc1") +
            geom_segment(data=annotation3, aes(x=x1, xend=x2, y=y1, yend=y2), col="#00acc1", arrow=arrow(length=unit(0.03, "npc"), ends="last", type = "closed")) +
            geom_text(data=annotation3, aes(x=x1, y=y1, label=label), size=5, col="#00acc1", hjust = 1, vjust = 0) + 
            coord_cartesian(xlim = c(0.2, 1), ylim = c(0, 1600)) +
            theme_bw() +
            theme(
                axis.title = element_text(size = 15),
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                legend.title = element_blank(),
                legend.key = element_blank(),
                legend.text = element_text(size = 15),
                plot.title = element_text(hjust = 0.5, size = 17)
            ) +
            ggtitle("Evaluate the Quality of Clnical Gene-Panel Sequencing") +
            labs(x = "Uniformity", y = "Mean Depth (x)")
    })
    
    # --------------------------------------------------------------------------
    # Stat
    # --------------------------------------------------------------------------
    output$stat1 <- renderPrint({
        dataset <- datasetInput1()
        summary(dataset)
    })
    
    output$stat2 <- renderPrint({
        dataset <- datasetInput2()
        summary(dataset)
    })
    
    # --------------------------------------------------------------------------
    # Summary
    # --------------------------------------------------------------------------
    # output$summary <- renderPrint({
    #     summaryData <- data[which(data$specimenType == "FFPE" | data$specimenType == "Fresh"),]
    #     describeBy(summaryData[c("od260280", "od260230", "dnaIntegrity", "libraryInputAmount",
    #         "libraryInsertSize", "readLength", "totalReads","meanCoverage",
    #         "uniformity", "onTargetRate", "q30", "prScore")],
    #     summaryData$specimenType,
    #     mat = FALSE,
    #     skew = FALSE)
    # })
    # output$summary <- renderTable({
    #     summaryData <- data[which(data$specimenType == "FFPE"), c("od260280", "od260230")]
    #     variable <- colnames(summaryData)["od260280"]
    #     mean <- mean(summaryData[, "od260280"], na.rm=TRUE)
    #     std_dev <- sd(summaryData[, "od260280"], na.rm=TRUE)
    #     median <- median(summaryData[, "od260280"], na.rm=TRUE)
    #     min <- min(summaryData[, "od260280"], na.rm=TRUE)
    #     max <- max(summaryData[, "od260280"], na.rm=TRUE)
    #     obs_num <- nrow(summaryData)
    #     quartile_1 <- quantile(summaryData[, "od260280"], 0.25, na.rm=TRUE)
    #     quartile_3 <- quantile(summaryData[, "od260280"], 0.75, na.rm=TRUE)
    #     
    #     desc_summary <- data.frame(variable, 
    #                                mean, std_dev, 
    #                                median, min, max, 
    #                                obs_num, 
    #                                quartile_1, quartile_3)
    #     
    #     # render Table
    #     desc_summary[, c("variable", "mean"), drop = FALSE]
    # })
    output$summary <- renderTable({
        summaryData <- data[which(data$specimenType == "FFPE"), c("od260280")]
        df1_summary<-as.data.frame(apply(summaryData,2,summary))
    })
    
}


# ==============================================================================
# deploy
# ==============================================================================

shinyApp(ui = ui, server = server)
