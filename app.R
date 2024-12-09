library(rsconnect)
library(shinydashboard)
library(shiny)
library(quantmod)
library(randomForest)
library(xts)
library(ggplot2)
library(DT)

# Set the cryptocurrency symbol
symbol <- "BTC-USD"

# Get historical price data
getSymbols(symbol, src = "yahoo", from = "2014-10-17", to = Sys.Date())

# Handle missing values using linear interpolation
data <- na.approx(Cl(get(symbol)))

# Extract closing prices
BTC_data <- Cl(data)

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Bitcoin Price Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Charts", tabName = "Charts", icon = icon("th")),
      menuItem("Data", tabName = "Data", icon = icon("database")),
      menuItem("Team", tabName = "Team", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("Bitcoin Price Prediction Using Random Forest"),
              fluidRow(
                box(
                  title = "Introduction",
                  status = "success",
                  solidHeader = TRUE,
                  "Explore the fascinating world of Bitcoin price dynamics. This web app employs a random forest model to forecast the future value of Bitcoin based on historical data.",
                  br(),
                  "Discover trends, patterns, and potential changes in the cryptocurrency market. The predictions are made with the help of a machine learning model that leverages lagged values of Bitcoin prices.",
                  br(), br(),
                  width = 12,
                  fluidRow(
                    box(
                      title = "Objectives",
                      status = "info",
                      "Predict the future value of Bitcoin based on historical price data.",
                      br(), 
                      "Provide insights into the potential trends and changes in the cryptocurrency market.",
                      width = 12
                    )
                  ))
              ),
              
              fluidRow(
                box(
                  title = "Bitcoin Price Plot",
                  status = "warning",
                  solidHeader = TRUE,
                  plotOutput("chartSeriesPlot"),
                  width = 12
                ),
                box(
                  title = "Random Forest Forecast",
                  status = "danger",
                  solidHeader = TRUE,
                  plotlyOutput("rfPlot"),
                  width = 12
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Charts",
              h2("CHARTS & ASSESSMENT"),
              fluidRow(
                box(title = "Bitcoin Time Series Plot",
                    status = "success",
                    solidHeader = TRUE,
                    plotOutput("xtsplot2"),
                    width = 12),
                box(title = "Random Forest Forecast",
                    status = "danger",
                    solidHeader = TRUE,
                    plotlyOutput("rfPlot2"),
                    width = 12)
              )
      ),
      # third tab content
      tabItem(tabName = "Data",
              h2("Bitcoin Price Data"),
              fluidRow(
                box(title = "Bitcoin Price Data",
                    status = "info",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput("BTCData")),
                box(title = "Bitcoin Time Series",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    plotOutput("xtsplot")
                )
              )
      ),
      # forth tab content
      tabItem(tabName = "Team",
              fluidRow(
                box(
                  title = "THE TEAM (grp4)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  br(),
                  "Meet the team behind this Bitcoin price prediction application. Each member has contributed unique skills to create a tool that can provide valuable insights into the cryptocurrency market.",
                  br(),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Elian Kimani",
                        status = "danger",
                        "Building this system has been a thrilling experience. Witnessing our collaborative effort come to life as a tool that could influence cryptocurrency decisions is truly rewarding.",
                        width = 12)
                  )
                )
              )
      )
    )
  )
)
# Shiny server
# Shiny server
server <- function(input, output) {
  # Define forecast_dates and forecast_values globally
  forecast_dates <- reactiveVal(character(0))
  forecast_values <- reactiveVal(numeric(0))
  
  # Train Random Forest model
  rf_model <- reactive({
    # Differencing to make the data stationary
    diff_BTC_data <- diff(BTC_data, lag = 1, differences = 1)
    
    # Check if diff_BTC_data is not NULL and has sufficient length
    if (!is.null(diff_BTC_data) && length(diff_BTC_data) > 1) {
      # Create lagged variables with proper column names
      lagged_BTC_data <- cbind(
        lag(BTC_data$BTC.USD.Close, 1),
        lag(BTC_data$BTC.USD.Close, 2),
        lag(BTC_data$BTC.USD.Close, 3),
        diff_BTC_data = diff_BTC_data
      )
      
      # Combine lagged variables with the response variable
      lagged_data <- na.omit(data.frame(lagged_BTC_data))
      
      # Check if lagged_data is not empty
      if (!is.null(lagged_data) && nrow(lagged_data) > 0) {
        # Fit Random Forest model
        set.seed(123)
        randomForest(formula = diff_BTC_data ~ ., data = lagged_data, ntree = 100)
      } else {
        # Return a placeholder if lagged_data is empty
        NULL
      }
    } else {
      # Return a placeholder if diff_BTC_data is NULL or has insufficient length
      NULL
    }
  })
  
  # Forecasting
  observe({
    forecast_horizon <- 30
    # Check if rf_model() is not NULL
    if (!is.null(rf_model())) {
      forecast_values(predict(rf_model(), newdata = as.data.frame(lag(BTC_data, 1))[nrow(data), , drop = FALSE]))
    } else {
      # Return a placeholder if rf_model() is NULL
      forecast_values(numeric(0))
    }
    
    # Convert numerical indices to dates
    forecast_dates(index(BTC_data)[length(BTC_data)] + seq(1, forecast_horizon))
  })
  
  output$forecast <- renderText({
    # Get the forecast_dates
    dates <- forecast_dates()
    
    # Print or return the forecast
    paste("Forecast Dates:", dates)
  })
  
  output$rfPlot <- renderPlotly({
    # Get the forecast_values
    values <- forecast_values()
    
    if (!is.null(values) && length(values) > 0) {
      # Create the Random Forest forecast plot using plotly
      p <- plot_ly()
      
      # Add Random Forest forecast
      p <- add_lines(p, x = forecast_dates(), y = values, name = "Random Forest Forecast", type = "scatter", mode = "lines", line = list(color = 'blue'))
      
      # Customize the layout
      p <- layout(p, title = "Random Forest Forecast for the Next 30 Days", yaxis = list(title = "Price"), xaxis = list(title = "Date"))
      
      p
    } else {
      # Return a placeholder plot if values is NULL or empty
      plot_ly() 
    }
  })
  
  output$chartSeriesPlot <- renderPlot({
    # Create the chartSeries plot
    chartSeries(data, name = "Bitcoin Price", theme = chartTheme("white"))
  })
  
  output$xtsplot <- renderPlot({
    # Create the chartSeries plot
    chartSeries(data, name = "Bitcoin Price", theme = chartTheme("white"))
  })
  
  output$xtsplot2 <- renderPlot({
    # Create the chartSeries plot
    chartSeries(data, name = "Bitcoin Price", theme = chartTheme("black"))
  })
  
  output$BTCData <- renderDataTable({
    # Assuming 'BTC_data' is an xts object
    colnames <- colnames(BTC_data)
    data <- data.frame(Date = format(index(BTC_data), "%Y-%m-%d"), Price = BTC_data[, colnames[1]])
    
    # Create DataTable with pagination
    datatable(data, options = list(pageLength = 20))
  }, col.names = c("Date", "Price"))

  
  output$rfPlot2 <- renderPlotly({
    # Get the forecast_dates and forecast_values
    dates <- forecast_dates()
    values <- forecast_values()
    
    # Check if both dates and values are not NULL
    if (!is.null(dates) && !is.null(values)) {
      # Create the Random Forest forecast plot using plotly
      p <- plot_ly()
      
      # Add Random Forest forecast
      p <- add_lines(p, x = dates, y = values, name = "Random Forest Forecast", type = "scatter", mode = "lines", line = list(color = 'blue'))
      
      # Customize the layout
      p <- layout(p, title = "Random Forest Forecast for the Next 30 Days", yaxis = list(title = "Price"), xaxis = list(title = "Date"))
      
      p
    } else {
      # Return a placeholder plot if either dates or values is NULL
      plot_ly() 
    }
  })
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
