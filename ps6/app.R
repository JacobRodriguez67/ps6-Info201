library(shiny)
library(ggplot2)

# Load dataset
my_data <- read.csv("car_data.csv")

# Define UI for the app
ui <- navbarPage(
  "Car Data",
  tabPanel(
    "Table",
    fluidPage(
      selectInput("Year", "Select a Year to filter by:", choices = sort(unique(my_data$Year))),
      tableOutput("my_table"),
    )
  ),
  tabPanel(
    "Plot",
    fluidPage(
      sliderInput("Price_range", "Select Price range:", min = 0, max = 100000, value = c(0, 50000)),
      plotOutput("my_plot")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  # Define filtered data based on input values
  filtered_data <- reactive({
    subset(my_data, Year == input$Year & Price >= input$Price_range[1] & Price <= input$Price_range[2])
  })
  
  # Define server-side code for table output
  output$my_table <- renderTable({
    filtered_data()
  })
  
  # Define server-side code for print output
  output$filtered_data <- renderPrint({
    filtered_data()
  })
  
  # Define server-side code for plot output
  output$my_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Price)) + 
      geom_histogram() +
      xlim(input$Price_range)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

