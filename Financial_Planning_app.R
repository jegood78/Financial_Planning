library(shiny)

ui <- fluidPage(
  
  numericInput(inputId = "housing_buffer",
               label = "Housing Cost Estimate",
               value = 3000,
               min = 0,
               max = 6000), #end numericInput housing_buffer
  
  numericInput(inputId = "bah",
               label = "Basic Allowance for Housing",
               value = 2928,
               min = 0,
               max = 6000), #end numericInput bah
  
  sliderInput(inputId = "annual_safe_withdrawal",
              label = "Annual Safe Withdrawal Rate (%)",
              value = 4,
              min = 0,
              max = 10), #end sliderInput annual_safe_withdrawal
  
  numericInput(inputId = "tsp",
               label = "TSP Contributions",
               value = 23000,
               min = 0,
               max = 20500), #end numericInput tsp
  
  dateInput(inputId = "pebd",
            label = "Pay Entry Base Date",
            value = "1999-02-19",
            min = "1990-01-01"), #end dateInput pebd
  
  selectInput(inputId = "c_rank",
              label = "Current Rank",
              choices = c('O1','O1E','O2','O2E','O3','O3E','O4','O5','O6','O7','O8','O9','O10'),
              selected = 'O5'), #end selectInput c_rank
  
  dateInput(inputId = "c_dor",
            label = "Current Date of Rank",
            value = "2023-09-01",
            min = "1990-01-01"), #end dateInput c_dor
  
  dateInput(inputId = "est_promotion",
            label = "Estimated Promotion Date",
            value = "2029-09-01",
            min = "1990-01-01"), #end dateInput est_promotion
  
  dateInput(inputId = "est_navy_retire_date",
            label = "Estimated Navy Retirement Date",
            value = "2026-09-01",
            min = "1990-01-01"), #end dateInput est_retire_date
  
  textOutput(outputId = "est_annual_income"), #end textOutput est_annual_income
  
  textOutput(outputId = "est_annual_expenses"), #end textOutput est_annual_expenses
  
  textOutput(outputId = "total_annual_invest"), #end textOutput total_annual_invest
  
  textOutput(outputId = "investment_value"), #end textOutput investment_value
  
  textOutput(outputId = "avg_annual_returns"), #end textOutput avg_annual_returns
  
  textOutput(outputId = "est_annual_pension"), #end textOutput est_annual_pension
  
  textOutput(outputId = "est_annual_net_pension"), #end textOutput est_annual_net_pension
  
  textOutput(outputId = "naive_fi_count_current"), #end textOutput naive_fi_count_current
  
  textOutput(outputId = "pension_fi_count_current"), #end textOutput pension_fi_count_current
  
  plotOutput(outputId = "simple_math_plot_compare") #end plotOutput simple_math_plot_compare
  
) #end fluidPage


server <- function(input, output, session) {
  
  #create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  }) #end dataset
  
  output$summary <- renderPrint({
    summary(dataset())
  }) #end output$summary
  
  output$table <- renderTable({
    dataset()
  }) #end output$table
  
} #end server

shinyApp(ui, server)