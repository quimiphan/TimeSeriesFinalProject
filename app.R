library(shiny)
library(shinythemes)
library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)
library(feasts)
library(DT)

# Load and prepare the data
load_data <- function() {
  df <- read.csv("TimeSeriesData.csv")
  
  # Ensure the Dt_Customer column is of Date type and create a tsibble
  df <- df %>%
    mutate(Dt_Customer = as.Date(Dt_Customer, format = "%Y-%m-%d")) %>%
    as_tsibble(index = Dt_Customer)
  
  # Add log-transformed columns
  df <- df %>%
    mutate(log_TotalSpend = log(TotalSpend),
           log_Income = log(Income))
  
  return(df)
}

# UI for Shiny App
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(
    div(
      h1("Time Series - Final Project", style = "margin-bottom: 0;color: #2C3E50;"),
      h4("TSLM Raw vs Log Transformation", style = "margin-top: 5px; color: #666;"),
      br(),
      p("🎯 Objective: This interactive tool allows users to compare forecasting performance of raw vs log-transformed data using time series linear models (TSLM).", 
        style = "font-size: 16px; color: #34495E; max-width: 800px;"),
      p("🔍 You can select the model type and forecast horizon to observe how predictions change over time.", 
        style = "font-size: 16px; color: #34495E; max-width: 800px;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("model_type", "Choose Model Type:",
                   choices = c("Raw" = "raw", "Log Transformed" = "log")),
      numericInput("h", "Forecast Horizon (months):", value = 12, min = 1),
      actionButton("run", "Run Model & Forecast")
    ),
    
    mainPanel(
      DTOutput("model_summary"),
      uiOutput("interpretation"),  
      plotOutput("forecast_plot")
    )
  )
)

# Server logic for Shiny App
server <- function(input, output) {
  # Load the data 
  data <- load_data()
  
  # Fit the model when the button is pressed
  model_fit <- eventReactive(input$run, {
    if (input$model_type == "raw") {
      model(data, tslm = TSLM(TotalSpend ~ Income))
    } else {
      model(data, tslm = TSLM(log_TotalSpend ~ log_Income))
    }
  })
  
  # Generate forecast based on the fitted model
  forecast_scenarios <- reactive({
    req(model_fit())
    fit_cons <- model_fit()
    h <- input$h  # get forecast horizon
    
    # Generate new data for the specified horizon
    new_cons <- scenarios(
      "Average increase" = new_data(data, h) %>%
        mutate(log_Income = mean(data$log_Income, na.rm = TRUE),
               Income = exp(mean(data$log_Income, na.rm = TRUE))),
      "Extreme increase" = new_data(data, h) %>%
        mutate(log_Income = 12,
               Income = exp(12)),
      names_to = "Scenario"
    )
    
    forecast(fit_cons, new_cons)
  })
  
  # Display model summary as an interactive table
  output$model_summary <- renderDT({
    req(model_fit())
    
    # Get model summary
    model_glance <- glance(model_fit())
    
    # Select only desired columns 
    cols_to_select <- c("r_squared", "adj_r_squared", "sigma2", 
                        "statistic", "p_value", "AIC", "BIC", 
                        "log_lik", "AICc")
    cols_present <- intersect(cols_to_select, names(model_glance))
    model_glance <- model_glance %>% select(.model, all_of(cols_present))
    
    # Transpose while keeping it safe
    df_transposed <- model_glance %>%
      pivot_longer(-.model, names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = .model, values_from = Value)
    
    # Show with DT
    datatable(
      df_transposed,
      rownames = FALSE,
      options = list(
        pageLength = nrow(df_transposed),
        autoWidth = TRUE,
        scrollX = TRUE
      )
    )
  })
  
  # Render the forecast plot
  output$forecast_plot <- renderPlot({
    req(forecast_scenarios())
    
    # Plot based on user input (Raw or Log)
    if (input$model_type == "raw") {
      data %>%
        autoplot(TotalSpend) +
        autolayer(forecast_scenarios()) +
        labs(title = "Forecasting Total Spend (Raw)", y = "Total Spend", x = "")
    } else {
      data %>%
        autoplot(log_TotalSpend) +
        autolayer(forecast_scenarios()) +
        labs(title = "Forecasting Total Spend (Log)", y = "Log of Total Spend", x = "")
    }
  })
  
  # Interpretation for Coefficient and Standard Error
  output$interpretation <- renderUI({
    req(model_fit())  # Ensure the model is fitted
    
    # Interpretation based on model type
    if (input$model_type == "raw") {
      # Extract coefficient and standard error for the raw model
      coeff <- tidy(model_fit()) %>% filter(term == "Income") %>% pull(estimate)
      std_err <- tidy(model_fit()) %>% filter(term == "Income") %>% pull(std.error)
      
      HTML(
        paste(
          "<h4>Interpretation for Raw Model:</h4>",
          "<p>The results from the time series linear regression model indicate that Income has a strong and statistically significant relationship with Total Spend. The estimated coefficient for Income is ", 
          round(coeff, 4), 
          ", meaning that for every one-unit increase in Income, the predicted Total Spend increases by ", 
          round(coeff, 4), 
          ". This relationship is highly significant, as indicated by the extremely small p-value, confirming that Income is a strong predictor of spending behavior. The standard error is ", 
          round(std_err, 4), 
          ", suggesting that the estimate is precise.</p>",
          sep = ""
        )
      )
    } else {
      # Extract coefficient and standard error for the log model
      coeff <- tidy(model_fit()) %>% filter(term == "log_Income") %>% pull(estimate)
      std_err <- tidy(model_fit()) %>% filter(term == "log_Income") %>% pull(std.error)
      
      HTML(
        paste(
          "<h4>Interpretation for Log Model:</h4>",
          "<p>The coefficient for log_Income suggests how changes in income (on a logarithmic scale) influence total spending (also on a logarithmic scale). For a 1% increase in Income, the total spending is expected to increase by ", 
          round(coeff, 2), 
          "%, assuming other factors are held constant. p-value < 0.05 indicates that the relationship between log_Income and log_TotalSpend is statistically significant. This shows that changes in income have a strong and reliable impact on total spending, and the model effectively captures this relationship. The standard error is ", 
          round(std_err, 4), 
          ", suggesting that the estimate is precise.</p>",
          sep = ""
        )
      )
    }
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
