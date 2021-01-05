server <- function(input, output) {
  multiple_dataset <- reactive({
    ds = c();
    
    if (!is.null(input$group_check))
      for (i in 1:length(input$group_check)){
        if (input$group_check[i] == "TEN.MI")
          ds <- cbind(ds, stocks.compoundReturns$TEN.MI)
        else if (input$group_check[i] == "ENI.MI")
          ds <- cbind(ds, stocks.compoundReturns$ENI.MI)
        else if (input$group_check[i] == "EXO.MI")
          ds <- cbind(ds, stocks.compoundReturns$EXO.MI)
        else if (input$group_check[i] == "AZM.MI")
          ds <- cbind(ds, stocks.compoundReturns$AZM.MI)
        else if (input$group_check[i] == "REC.MI")
          ds <- cbind(ds, stocks.compoundReturns$REC.MI)
        else if (input$group_check[i] == "DIA.MI")
          ds <- cbind(ds, stocks.compoundReturns$DIA.MI)
      }
    
    ds
  })
  
  settore <- reactive({
    ds = c();
    if (input$group_radio == "OILGAS")
      ds <- cbind(stocks.compoundReturns$TEN.MI, stocks.compoundReturns$ENI.MI)
    else if (input$group_radio == "FINANCE")
      ds <- cbind(stocks.compoundReturns$EXO.MI, stocks.compoundReturns$AZM.MI)
    else if (input$group_radio == "BIOTECH")
      ds <- cbind(stocks.compoundReturns$REC.MI, stocks.compoundReturns$DIA.MI)
    
    ds <- window(ds, start = input$window[1], end = input$window[2])
    ds
  })
  
  output$plot_returns <- renderPlot({
    heatmap(cor(multiple_dataset()), main = "Heatmap della matrice di correlazione tra i CCR dei titoli in tutto il periodo", xlab = "Titoli", ylab = "Titoli") 
  })
  
  output$plot_oilgas <- renderPlot({
    plot(settore(), xlab = "Data", ylab = "CCR (%)", main = paste0("CCR (%) Mensile OIL&GAS dal ", input$window[1], " al ",  input$window[2]))
  })
  
  output$plot_fin <- renderPlot({
    plot(settore(), xlab = "Data", ylab = "CCR (%)", main = paste0("CCR (%) Mensile FINANCE dal ", input$window[1], " al ",  input$window[2]))
  })
  
  output$plot_bio <- renderPlot({
    plot(settore(), xlab = "Data", ylab = "CCR (%)", main = paste0("CCR (%) Mensile BIOTECH dal ", input$window[1], " al ",  input$window[2]))
  })
}