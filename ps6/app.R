library(shiny)
library(ggplot2)

# Load dataset
my_data <- read.csv("car_data.csv")

# Define UI for the app
ui <- fluidPage(
  navbarPage(
    "Car Data App",
    tabPanel(
      "About",
      fluidRow(
        column(
          width = 12,
          h1("About the Car Data App"),
          p("This dataset contains information about cars, including their make, model, year, mileage, and whether they are new or used."),
          p("Use the tabs above to explore the dataset further."),
          tableOutput("data_table")
        )
      )
    ),
    tabPanel(
      "Table",
      sidebarLayout(
        sidebarPanel(
          selectInput("Model", "Select a Model to filter by:", choices = unique(my_data$Model))
        ),
        mainPanel(
          tableOutput("my_table")
        )
      )
    ),
    tabPanel(
      "Plot",
      sidebarLayout(
        sidebarPanel(
          selectInput("x_var", "X-Axis Variable", choices = names(my_data)),
          selectInput("y_var", "Y-Axis Variable", choices = names(my_data)),
          radioButtons("status", "Car Status:", choices = c("New", "Used"), selected = "New"),
          selectInput("plot_type", "Select Plot Type", choices = c("Scatter", "Line", "Bar"), selected = "Scatter")
        )
        ,
        mainPanel(
          plotOutput("my_plot")
        )
      )
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  # Define filtered data based on input values for table output
  filtered_data <- reactive({
    if (input$Model == "All") {
      my_data_filtered <- my_data
    } else {
      my_data_filtered <- subset(my_data, Model == input$Model)
    }
    my_data_filtered
  })
  
  # Define filtered data based on input values for plot output
  filtered_data_plot <- reactive({
    if (input$Status == "All") {
      my_data_filtered_plot <- subset(my_data, Model %in% c(input$Model1, input$Model2))
    } else {
      my_data_filtered_plot <- subset(my_data, Model %in% c(input$Model1, input$Model2) & Status == input$Status)
    }
    my_data_filtered_plot
  })
  # Displays head of data for about page
  output$data_table <- renderTable({
    head(my_data, 5)
  })
  # Define server logic for table output
  output$my_table <- renderTable({
    filtered_data()
  })
  
  # Define server logic for plot output
  output$my_plot <- renderPlot({
    x_var <- input$x_var
    y_var <- input$y_var
    plot_type <- input$plot_type
    
    filtered_data <- my_data %>%
      filter(Status == input$status)
    
    ggplot(filtered_data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
      geom_point() +
      labs(x = x_var, y = y_var, title = paste0(plot_type, " Plot"))
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

