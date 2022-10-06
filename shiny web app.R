library(DT)
library(shiny)
library(shinythemes)
library(summarytools)
library(ggplot2)
library(GGally)

ui <- fluidPage(theme = shinytheme("flatly"),
                tags$style('.container-fluid {
                             background-color: #d2d9d7
                }'),
                titlePanel(
                    h2("Statistical Calculator", align = "center")
                ),
                br(),
                br(),
                sidebarPanel(" ",
                             fileInput(
                                 "dataset",
                                 "Choose a CSV file",
                                 multiple = FALSE,
                                 accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'),
                                 width = NULL,
                                 buttonLabel = "Browse",
                                 placeholder = "No file selected"
                             ),
                             checkboxInput("header", "Header", TRUE),
                             uiOutput("columns")),
                
                mainPanel(
                    tabsetPanel(
                        tabPanel("Data",
                                 h4("Data Table", align = "center"),
                                 dataTableOutput("data")),
                        
                        tabPanel("Summary",
                                 h4("Structure", align = "center"),
                                 br(),
                                 verbatimTextOutput("stc"),
                                 br(),
                                 h4("Summary", align = "center"),
                                 br(),
                                 verbatimTextOutput("summary")),
                        
                        tabPanel("Descriptive Statistics",
                                 h4("Descriptive Statistics of Data", align = "center"),
                                 br(),
                                 verbatimTextOutput("des")),
                        
                        tabPanel("Visualizations",
                                 h4("Histogram", align = "center"),
                                 br(),
                                 plotOutput("hist"),
                                 h4("Box plot", align = "center"),
                                 br(),
                                 plotOutput("box"),
                                 h4("Scatter Plot", align = "center"),
                                 br(),
                                 plotOutput("scatter"),
                                 h4("QQ-Plot", align = "center"),
                                 br(),
                                 plotOutput("qq")),
                        
                        tabPanel("Statistics",
                                 h4("Mean", align = "center"),
                                 br(),
                                 verbatimTextOutput("mean"),
                                 br(),
                                 h4("Median", align = "center"),
                                 verbatimTextOutput("median"),
                                 br(),
                                 h4("Mode", align = "center"),
                                 verbatimTextOutput("mode"),
                                 br(),
                                 h4("Standard deviation", align = "center"),
                                 verbatimTextOutput("sd")),
                        tabPanel("About Us",
                                 br(),
                                 h5("Dishwa Shah: dishwa123shah@gmail.com"),
                                 br(),
                                 h5("Ashil Shah: ashilshah2001@gmail.com"),
                                 br(),
                                 h5("Akshat Shah: akshat2000shah@gmail.com"),
                                 br()))))


server <- function(input, output,session) {
    df <- reactive(read.csv(input$dataset$datapath, header = input$header))
    data <- reactive(na.omit(df()))
    mod <-function(x){which.max(tabulate(x))}
    output$data <- renderDataTable(
        data(),
        server = TRUE
    )
    output$stc <- renderPrint({
        str(data())
    })
    output$columns <- renderUI({
        selectInput("columns", "Choose a column", 
                    choices  <- colnames(data()),
                    multiple = F)
    })
    output$summary <- renderPrint({
        dfSummary(data())
    })
    output$des <- renderPrint({
        descr(data())
    })
    
    
    output$mean <- renderPrint({
        mean(data()[,input$columns])
    })
    output$median <- renderPrint({
        median(data()[,input$columns])
    })
    output$mode <- renderPrint({
        mod(data()[,input$columns])
    })
    output$sd <- renderPrint({
        sd(data()[,input$columns])
    })
    
    
    output$hist <- renderPlot({
        hist(data()[,input$columns])
    })
    output$box <- renderPlot({
        boxplot(data()[,input$columns])
    })
    output$scatter <- renderPlot({
        plot(data()[,input$columns])
    })
    output$qq <- renderPlot({
        qqnorm(data()[,input$columns])
        qqline(data()[,input$columns])
    })
}

shinyApp(ui = ui, server = server)