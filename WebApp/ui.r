ui <- fluidPage(
  titlePanel("Progetto BISF - TEN.MI ENI.MI EXO.MI AZM.MI REC.MI DIA.MI"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "group_check",
        label = "Selezione Stocks", 
        selected = stockNames,
        choices = stockNames
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "plot_returns", height = "500px")
    )
  ),
  
  #####
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        inputId = "window", 
        label = "Finestra Temporale", 
        start = index(stocks.compoundReturns[1,]), 
        end = Sys.Date()
      ),
      
      radioButtons(
        inputId = "group_radio", 
        label="Settore Selezionato",
        choices = c("OILGAS", "FINANCE", "BIOTECH")
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.group_radio == 'OILGAS'",
        plotOutput(outputId = "plot_oilgas", height = "400px")
      ),
      conditionalPanel(
        condition = "input.group_radio == 'FINANCE'",
        plotOutput(outputId = "plot_fin", height = "400px")
      ),
      conditionalPanel(
        condition = "input.group_radio == 'BIOTECH'",
        plotOutput(outputId = "plot_bio", height = "400px")
      )
    )
  ),
)