library(shiny)
library(plotly)
library(DT)

mobility <- read.csv("all-states-history.csv", sep = ',')
mobility$date <- as.Date(mobility$date)
mobility$state <- as.factor(mobility$state)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("COVID-19 Data"),
      selectInput(inputId = "dv", label = "Category",
                  choices = c("death",	"hospitalized", "positive"),
                  selected = "Death"),
      selectInput(inputId = "state", "State(s)",
                  choices = levels(mobility$state),
                  multiple = TRUE,
                  selected = c("MO", "KS")),
      dateRangeInput(inputId = "date", "Date range",
                     start = min(mobility$date),
                     end   = max(mobility$date)),
      downloadButton(outputId = "download_data", label = "Download"),
    ),
    mainPanel(
      plotlyOutput(outputId = "plot"), br(),
      em("The COVID Tracking Project has ended all data collection as of March 7, 2021. These files are still available, but will only include data up to March 7, 2021."),
      br(), br(), br(),
      DT::dataTableOutput(outputId = "table")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    subset(mobility,
           state %in% input$state &
             date >= input$date[1] & date <= input$date[2])})
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="date", y=input$dv, color="state")) +
        geom_point(alpha=0.5) + theme(legend.position = "none") +
        ylab("people")
      
      p
    })
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)

#https://tilburgsciencehub.com/examples/google-covid-shiny-app/