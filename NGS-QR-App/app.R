# Library
library(shiny)
library(curl)
library(ggplot2)
library(pastecs)
library(doBy)
library(psych)

# Import data using REST API
source('./fetch.R')
data <- fetchData()

ui <- fluidPage(titlePanel(""),
                headerPanel(""),
                sidebarLayout(
                    sidebarPanel(
                        div(style = "font-size:16px;",
                            tags$style(type='text/css', 
                                       ".selectize-input { font-size: 14px; line-height: 14px; }
                                        .selectize-dropdown { font-size: 14px; line-height: 14px; }"),
                            selectInput(
                                inputId = "graphSelected",
                                label = "1. Select graph:",
                                choices = c("Density", "Boxplot", "PR Score", "Summary")
                            ),
                            conditionalPanel(
                                condition = "input.graphSelected === 'Density' || input.graphSelected === 'Boxplot' ",
                                selectInput(
                                    inputId = "datasetSelected",
                                    label = "2. Select a dataset:",
                                    choices = c(
                                        "OD 260/280",
                                        "OD 260/230",
                                        "DNA Integrity",
                                        "Library Input Amount",
                                        "Library Insert Size",
                                        "Read Length",
                                        "Total Reads",
                                        "Mean Coverage",
                                        "Uniformity",
                                        "on-Target-Rate",
                                        "Q30",
                                        "PR Score"
                                    )
                                ),
                                selectInput(
                                    inputId = "specimenSelected",
                                    label = "3. Select a specimen type:",
                                    choices = c("Fresh", "FFPE")
                                    # choices = c("All", "Fresh", "FFPE")
                                )
                            ),
                            conditionalPanel(
                                condition = "input.graphSelected === 'PR Score'",
                                selectInput(
                                    inputId = "specimenSelected",
                                    label = "3. Select a specimen type:",
                                    choices = c("Fresh", "FFPE")
                                )
                            ),
                            tags$style("#idSelected {font-size: 14px; color: #00acc1;}"),
                            conditionalPanel(condition = "input.graphSelected != 'Summary'",
                                             uiOutput("idSelected"))
                        )
                    ),
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
                                         verbatimTextOutput("summary"))
                    )
                ))


server <- function(input, output) {
    datasetSelected <- renderText({
        req(input$datasetSelected)
        
        if (input$datasetSelected == "OD 260/280")
            return("od260280")
        else if (input$datasetSelected == "OD 260/230")
            return("od260230")
        else if (input$datasetSelected == "DNA Integrity")
            return("dnaIntegrity")
        else if (input$datasetSelected == "Library Input Amount")
            return("libraryInputAmount")
        else if (input$datasetSelected == "Library Insert Size")
            return("libraryInsertSize")
        else if (input$datasetSelected == "Read Length")
            return("readLength")
        else if (input$datasetSelected == "Total Reads")
            return("totalReads")
        else if (input$datasetSelected == "Mean Coverage")
            return("meanCoverage")
        else if (input$datasetSelected == "Uniformity")
            return("uniformity")
        else if (input$datasetSelected == "on-Target-Rate")
            return("onTargetRate")
        else if (input$datasetSelected == "Q30")
            return("q30")
        else if (input$datasetSelected == "PR Score")
            return("prScore")
    })
    
    
    datasetInput <- reactive({
        req(input$specimenSelected)
        
        if (input$specimenSelected == "All")
            return(na.omit(data[, c(datasetSelected())]))
        else if (input$specimenSelected != "All")
            return(na.omit(data[which(data$specimenType == input$specimenSelected), c(datasetSelected())]))
    })
    
    
    specimenInput <- reactive({
        req(input$specimenSelected)
        
        data[(data$specimenType == input$specimenSelected),]
    })
    
    
    datasetInput <- reactive({
        req(input$specimenSelected)
        
        if (input$specimenSelected == "All")
            return(na.omit(data[, c(datasetSelected())]))
        else if (input$specimenSelected != "All")
            return(na.omit(data[which(data$specimenType == input$specimenSelected), c(datasetSelected())]))
    })
    
    
    idSelected <- renderText({
        req(input$idSelected)
        
        input$idSelected
    })
    
    
    dataSelected <- reactive({
        req(datasetSelected())
        
        data[which(data$identifier == idSelected()), c(datasetSelected())]
    })
    
    
    uniformitySelected <- reactive({
        req(idSelected())
        
        data[which(data$identifier == idSelected()), c("uniformity")]
    })
    
    
    meanCoverageSelected <- reactive({
        req(idSelected())
        
        data[which(data$identifier == idSelected()), c("meanCoverage")]
    })
    
    
    output$idSelected <- renderUI({
        selectInput(
            inputId = "idSelected",
            label = "Order ID:",
            choices = data[which(data$specimenType == input$specimenSelected), c("identifier")],
            selected = NULL
        )
    })
    
    
    output$density <- renderPlot({
        obj <- specimenInput()
        dataset <- datasetSelected()
        annotation <- data.frame(
            x = dataSelected(),
            y = Inf,
            label = idSelected()
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
                geom_vline(xintercept = dataSelected(), colour = "#00acc1") +
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
                geom_vline(xintercept = dataSelected(), colour = "#00acc1") +
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
                geom_density(
                    aes_string(dataset), na.rm = TRUE
                ) +
                xlim(c(min(obj[dataset], na.rm = TRUE) * 0.9, max(obj[dataset], na.rm = TRUE) * 1.1)) +
                geom_vline(data=threshold[threshold$dataset==dataset,], aes(xintercept=x1), colour = "#FF0000") +
                geom_vline(data=threshold[threshold$dataset==dataset,], aes(xintercept=x2), colour = "#FF0000") +
                geom_segment(data=threshold[threshold$dataset==dataset,], aes(x=x1, xend=x2, y=0, yend=0), col="#FF0000", arrow=arrow(length=unit(0.03, "npc"), ends="both")) +
                geom_vline(xintercept = dataSelected(), colour = "#00acc1") +
                geom_text(data=threshold[threshold$dataset==dataset,], aes(x=(x1+x2)/2, y=0, label=label), size=5, col="#FF0000", vjust = 0) +
                geom_text(data=annotation, aes(x=x, y=y, label=label), size=5, col="#00acc1", hjust = 0, vjust = 1) + 
                
                # geom_line(aes(x=threshold[1], y=0), size=5, col="#FF0000", arrow = arrow(length=unit(0.30,"cm"), ends="both", type = "closed")) +
                # geom_segment(aes(x=threshold[1], xend=threshold[2], y=0, yend=0), col="#FF0000", arrow=arrow(ends='both')) +
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
        
            

        # plot(density(datasetInput()), main = 'Kernal Density Function')
        # abline(v = dataSelected(), col = "#00acc1")
        # text(x = dataSelected(), y = 1, labels = idSelected(), col = "#00acc1")


            # abline(v = threshold,
            #        col = "#FF0000",
            #        lty = 2)
            # axis(c(1, 1), threshold, threshold, col = "#FF0000")
            # axis(dataSelected(), dataSelected(), col = "#00acc1")
            # arrows(
            #     x0 = threshold[1],
            #     y0 = 0,
            #     x1 = threshold[2],
            #     y1 = 0,
            #     col = "#FF0000",
            #     angle = 30,
            #     length = 0.1,
            #     code = 3
            # )
            # text(x=(threshold[1]+threshold[2])/2, y=0.5, labels="Threshold", col='#FF0000')
    })
    
    
    output$boxplot <- renderPlot({
        boxplot(datasetInput(), main = 'Boxplot')
        points(dataSelected(),
               col = "#00acc1",
               pch = 16,
               cex = 1.5)
        
        threshold = c(-Inf, Inf)
        if (datasetSelected() == "od260280")
            threshold = c(1.8, 2.1)
        else if (datasetSelected() == "od260230")
            threshold = c(1.8, 2.5)
        else if (datasetSelected() == "dnaIntegrity")
            threshold = c(350, Inf)
        else if (datasetSelected() == "libraryInputAmount")
            threshold = c(200, Inf)
        else if (datasetSelected() == "libraryInsertSize")
            threshold = c(150, 200)
        else if (datasetSelected() == "meanCoverage")
            threshold = c(200, Inf)
        else if (datasetSelected() == "prScore")
            threshold = c(80, Inf)
        
        if (all.equal(threshold, c(-Inf, Inf)) != TRUE) {
            if (threshold[1] == -Inf)
                threshold[1] = min(datasetInput(), na.rm = TRUE)
            if (threshold[2] == Inf)
                threshold[2] = max(datasetInput(), na.rm = TRUE)
            abline(h = threshold,
                   col = "#FF0000",
                   lty = 2)
            axis(c(2, 2), threshold, threshold, col = "#FF0000")
        }
    })
    
    
    output$prscore <- renderPlot({
        ggplot(specimenInput(), aes(uniformity, meanCoverage)) +
            geom_point(aes(colour = prScore), size = 2) +
            geom_point(
                data = subset(specimenInput(), prScore < 100),
                aes(colour = prScore),
                size = 2
            ) +
            geom_point(
                data = subset(specimenInput(), prScore < 90),
                aes(colour = prScore),
                size = 2
            ) +
            geom_point(
                data = subset(specimenInput(), prScore < 80),
                aes(colour = prScore),
                size = 2
            ) +
            scale_colour_gradient(
                limits = c(80, 100),
                low = "red",
                high = "forestgreen",
                na.value = "red"
            ) +
            geom_hline(yintercept = meanCoverageSelected(), colour = "#00acc1") +
            geom_vline(xintercept = uniformitySelected(), colour = "#00acc1") +
            coord_cartesian(xlim = c(0.2, 1), ylim = c(0, 1600)) +
            theme_bw() +
            theme(
                axis.title = element_text(size = 13),
                axis.text.x = element_text(size = 13),
                axis.text.y = element_text(size = 13),
                legend.title = element_blank(),
                legend.key = element_blank(),
                legend.text = element_text(size = 13),
                plot.title = element_text(hjust = 0.5, size = 15)
            ) +
            ggtitle("Evaluate the Quality of Clnical Gene-Panel Sequencing") +
            labs(x = "Uniformity", y = "Mean Depth (x)")
    })
    
    
    output$stat1 <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    output$stat2 <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    output$summary <- renderPrint({
        describeBy(data[c(
            "od260280",
            "od260230",
            "dnaIntegrity",
            "libraryInputAmount",
            "libraryInsertSize",
            "readLength",
            "totalReads",
            "meanCoverage",
            "uniformity",
            "onTargetRate",
            "q30",
            "prScore"
        )],
        data$specimenType,
        mat = FALSE,
        skew = FALSE)
        
        # summaryBy(data$od260280 + data$od260230 ~ data$specimenType, data=data, FUN=c(mean, sd))
        
        # with(data, tapply(data$od260280, data$specimenType, summary))
        
        # summary(data[, c("od260280", "od260230", "dnaIntegrity", "libraryInputAmount", "libraryInsertSize", "libraryInsertSize", "readLength",
        #                  "totalReads", "meanCoverage", "uniformity", "onTargetRate", "q30", "prScore")])
    })
}

shinyApp(ui = ui, server = server)
