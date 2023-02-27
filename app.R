library(shiny)
library(DT)
source("config/appDevelop.R")

ui <- fluidPage(
  titlePanel("Research center for infectious disease - RCID"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select model and machine learning algorithm"),
      selectInput("var1", 
        label = "Choose Model",
        choices = list("4-CpGs model", "5-CpGs model", "8-CpGs model", "12-CpGs model"), 
        selected = "8-CpGs model"),
      selectInput("var2", 
        label = "Choose Algorithm",
        choices = list("Logistic regression", "SVM", "Random forest", "KNN", "Adaboost"), 
        selected = "Logistic regression"),
      helpText("Now, upload your CpG methylation level here"),
      fileInput("file1", "Choose CSV File", 
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
      checkboxInput("header", "Header", TRUE)
      ),
    mainPanel(
      tags$head(
        tags$style(HTML("
          .sep {
            width: 20px; height: 1px; float: left;
          }
          "))
        ),
      h1("Shiny App for using DNA methylation predict TB condition"),
      textOutput("var1"),
      textOutput("var2"),
      DT::dataTableOutput("tablePred"),
      h3("Citation: "),
      h4("For more Information about RCID: ")
      )
    )
  )

# Define server logic ----
server <- function(input, output) {
  output$var1 <- renderText({ 
    paste("You have selected", input$var1)
    })
  output$var2 <- renderText({ 
  paste("You have selected", input$var2)
  })
  output$tablePred=DT::renderDataTable ({
    x <- switch(input$var1, 
      "4-CpGs model" = "LASSO", 
      "5-CpGs model" = "MUVR_PLS",
      "8-CpGs model" = "BMA",
      "12-CpGs model" = "MUVR_rf")
    y <- switch(input$var2, 
      "Logistic regression" = "logit", 
      "SVM" = "svm",
      "Random forest" = "rf",
      "KNN" = "knn",
      "Adaboost" = "adaboost")
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    valDT=read.csv(file$datapath, header = input$header)
    my_data=app(x, y, valDT)
    DT::datatable(my_data, 
      extensions = "Buttons",
      options = list(
        paging = TRUE,
        rownames = FALSE,
        scrollX=TRUE,
        searching = TRUE,
        ordering = TRUE,
        dom = 'l<"sep">Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength=10,
        lengthMenu=c(10,20,50,100)
        )
      )
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)  
