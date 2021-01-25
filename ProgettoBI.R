rm(list=ls())

library(quantmod)
library(Amelia)
library(knitr)
library(dygraphs)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(shiny)
library(reshape2)
library(tseries)
library(scales)
library(data.table)
library(zoo)
library(datasets)
library(forecast)
library(lubridate)

#-------------------------------DESCRIPTIVE ANALYTICS-------------------------------
# Nomi degli stocks
stockNames <- c("TEN.MI", "ENI.MI", "EXO.MI", "AZM.MI", "REC.MI", "DIA.MI")

# Retrieve da yahoo finance degli stocks

for (s in stockNames) {
  getSymbols(s, source="yahoo")
}
rm(s)

# Finestra temporale dal 1 Ottobre 2018 al 1 Ottobre 2020
startDate <- as.Date("2018-10-1")
endDate <- as.Date("2020-10-1")

# Salvo gli adjusted di tutta la ts
TEN.MI.All <- window(TEN.MI, end = endDate)[,6]
ENI.MI.All <- window(ENI.MI, end = endDate)[,6]
EXO.MI.All <- window(EXO.MI, end = endDate)[,6]
AZM.MI.All <- window(AZM.MI, end = endDate)[,6]
DIA.MI.All <- window(DIA.MI, end = endDate)[,6]
REC.MI.All <- window(REC.MI, end = endDate)[,6]



# Salvo gli adjusted della finestra 
TEN.MI <- window(TEN.MI, start = startDate, end = endDate)[,6]
ENI.MI <- window(ENI.MI, start = startDate, end = endDate)[,6]
EXO.MI <- window(EXO.MI, start = startDate, end = endDate)[,6]
AZM.MI <- window(AZM.MI, start = startDate, end = endDate)[,6]
DIA.MI <- window(DIA.MI, start = startDate, end = endDate)[,6]
REC.MI <- window(REC.MI, start = startDate, end = endDate)[,6]

# Merging dei dati in una struttura unica
stocks <- round(x = merge(TEN.MI, ENI.MI, EXO.MI, AZM.MI, REC.MI, DIA.MI ), digits = 2)
colnames(stocks) <- stockNames
tformat(stocks) <- "%d %B %y"

# Missmap per visualizzare i dati mancanti

par(mfrow=c(1,1))
missmap(stocks, legend = FALSE)
# Omissione degli NA
stocks <- na.omit(stocks)
# Rivisualizzazione della missmap senza NA
missmap(stocks, legend = FALSE)

#Summary dei dati aggregati
kable(summary(stocks))

# Funzione per il plot degli adjusted close prices di uno stock
valueChart <- function(adjusteds, title) {
  dygraph(
    adjusteds, 
    main = paste("Andamento del valore del titolo", title, "dal", format(startDate, "%d-%m-%y"), "al",  format(endDate, "%d-%m-%y"), sep=" ", collapse=NULL), 
    ylab = "Valore (EUR)", 
    xlab = "Data") %>% 
      dyRangeSelector()
}

# Plot dei grafici degli adjusted close prices
valueChart(adjusteds = stocks$TEN.MI, title="Tenaris S.A.")
valueChart(adjusteds = stocks$ENI.MI, title="Eni S.p.A.")
valueChart(adjusteds = stocks$EXO.MI, title="Exor N.V.")
valueChart(adjusteds = stocks$AZM.MI, title="Azimut Holding S.p.A.")
valueChart(adjusteds = stocks$REC.MI, title="Recordati S.p.A.")
valueChart(adjusteds = stocks$DIA.MI, title="DiaSorin S.p.A.")

# Calcolo di CCR Mensili e Annuali

TEN.MI.CCReturnMonthly <- round(x=Return.calculate(to.monthly(stocks$TEN.MI), method = "compound"), digits=2)[,4]
# Impostazione del ritorno del primo mese (ottobre 2018) a 0%
TEN.MI.CCReturnMonthly <- na.fill(TEN.MI.CCReturnMonthly, 0)
TEN.MI.CCReturnYearly <-  round(x=Return.calculate(to.yearly(stocks$TEN.MI), method = "compound"), digits=2)[,4]
TEN.MI.CCReturnYearly <- na.fill(TEN.MI.CCReturnYearly, 0)

ENI.MI.CCReturnMonthly <- round(x=Return.calculate(to.monthly(stocks$ENI.MI), method = "compound"), digits=2)[,4]
ENI.MI.CCReturnMonthly <- na.fill(ENI.MI.CCReturnMonthly, 0)
ENI.MI.CCReturnYearly <-  round(x=Return.calculate(to.yearly(stocks$ENI.MI), method = "compound"), digits=2)[,4]
ENI.MI.CCReturnYearly <- na.fill(ENI.MI.CCReturnYearly, 0)

EXO.MI.CCReturnMonthly <- round(x=Return.calculate(to.monthly(stocks$EXO.MI), method = "compound"), digits=2)[,4]
EXO.MI.CCReturnMonthly <- na.fill(EXO.MI.CCReturnMonthly, 0)
EXO.MI.CCReturnYearly <-  round(x=Return.calculate(to.yearly(stocks$EXO.MI), method = "compound"), digits=2)[,4]
EXO.MI.CCReturnYearly <- na.fill(EXO.MI.CCReturnYearly, 0)

AZM.MI.CCReturnMonthly <- round(x=Return.calculate(to.monthly(stocks$AZM.MI), method = "compound"), digits=2)[,4]
AZM.MI.CCReturnMonthly <- na.fill(AZM.MI.CCReturnMonthly, 0)
AZM.MI.CCReturnYearly <-  round(x=Return.calculate(to.yearly(stocks$AZM.MI), method = "compound"), digits=2)[,4]
AZM.MI.CCReturnYearly <- na.fill(AZM.MI.CCReturnYearly, 0)

DIA.MI.CCReturnMonthly <- round(x=Return.calculate(to.monthly(stocks$DIA.MI), method = "compound"), digits=2)[,4]
DIA.MI.CCReturnMonthly <- na.fill(DIA.MI.CCReturnMonthly, 0)
DIA.MI.CCReturnYearly <-  round(x=Return.calculate(to.yearly(stocks$DIA.MI), method = "compound"), digits=2)[,4]
DIA.MI.CCReturnYearly <- na.fill(DIA.MI.CCReturnYearly, 0)

REC.MI.CCReturnMonthly <- round(x=Return.calculate(to.monthly(stocks$REC.MI), method = "compound"), digits=2)[,4]
REC.MI.CCReturnMonthly <- na.fill(REC.MI.CCReturnMonthly, 0)
REC.MI.CCReturnYearly <-  round(x=Return.calculate(to.yearly(stocks$REC.MI), method = "compound"), digits=2)[,4]
REC.MI.CCReturnYearly <- na.fill(REC.MI.CCReturnYearly, 0)

#Visualizzazione degli adjusted close prices degli stock considerati insieme
dygraph(stocks, 
        main = paste("Andamento del valore dei titoli azionari dal", format(startDate, "%d %B %y"), "al",  format(endDate, "%d %B %y"), sep=" ", collapse=NULL), 
        ylab = "Valore (EUR)", 
        xlab = "Data") %>% 
    dyRangeSelector()

#VARIAZIONE % DEI RITORNI SEMPLICI DEI TITOLI AZIONARI
stocks.simpleReturns <- round(Return.calculate(to.monthly(stocks, OHLC = FALSE), method = "simple"), digits=2)
stocks.simpleReturns <- na.fill(stocks.simpleReturns, 0)
kable(summary(stocks.simpleReturns))
print(stocks.simpleReturns)

dygraph(stocks.simpleReturns, 
        main = paste("Variazione % mensile del valore dei titoli azionari dal", format(startDate, "%d %B %y"), "al",  format(endDate, "%d %B %y"), sep=" ", collapse=NULL), 
        ylab = "Variazione (%)", 
        xlab = "Mese")%>% 
  dyRangeSelector()

#VARIAZIONE % CCR DEI TITOLI AZIONARI
stocks.compoundReturns <- round(Return.calculate(to.monthly(stocks, OHLC = FALSE), method = "compound"), digits=2)
stocks.compoundReturns <- na.fill(stocks.compoundReturns, 0)
kable(summary(stocks.compoundReturns))
print(stocks.compoundReturns)

dygraph(stocks.compoundReturns, 
        main = paste("Variazione % dei CCR mensili dei titoli azionari dal", format(startDate, "%d %B %y"), "al",  format(endDate, "%d %B %y"), sep=" ", collapse=NULL), 
        ylab = "Variazione (%)", 
        xlab = "Data")%>% 
  dyRangeSelector()

# Funzione per la visualizzazione contemporanea dei ritorni di due stock di settori distinti
sectorCCRgraph <- function(adjustedsOne, companyOne, adjustedsTwo, companyTwo, sectorTitle) {
  sector.compoundReturns <- round(Return.calculate(to.monthly(merge(adjustedsOne, adjustedsTwo), OHLC = FALSE), method = "compound"), digits=2)[-1,]
  colnames(sector.compoundReturns) <- c(companyOne, companyTwo)
  dygraph(sector.compoundReturns, 
          main = paste("Variazione % dei CCR mensili del settore", sectorTitle, "dal", format(startDate, "%d %B %y"), "al",  format(endDate, "%d %B %y"), sep=" ", collapse=NULL), 
          ylab = "Variazione (%)", 
          xlab = "Data")%>% 
    dyOptions(fillGraph =TRUE) %>%
    dyRangeSelector()
}


#COMPARAZIONE CCR MENSILE TITOLI SETTORE OIL&GAS
sectorCCRgraph(stocks$TEN.MI, "TEN.MI", stocks$ENI.MI, "ENI.MI", "Oil&Gas")

#COMPARAZIONE CCR MENSILE TITOLI SETTORE FINANCE
sectorCCRgraph(stocks$EXO.MI, "EXO.MI", stocks$AZM.MI, "AZM.MI", "Finance")

#COMPARAZIONE CCR MENSILE TITOLI SETTORE BIOTECH
sectorCCRgraph(stocks$DIA.MI, "DIA.MI", stocks$REC.MI, "REC.MI", "BioTech")


#GRAFICI DIAGNOSTICI
library(ggplot2)
CCRdiagnosticGraphs <- function(CCRmonthly, title) {
  par(mfrow=c(2,2))
  # Istogramma per la visualizzazione della frequenza delle occorenze dei CCR 
  hist(CCRmonthly, 
       breaks = 15, 
      freq = FALSE, 
      main=paste("Distribuzione dei CCR dello stock", title, sep=" ", collapse=NULL),
      xlab = "CCR (%)",
      ylab = "Frequenza del CCR (%)")
  # Linea di densità
  lines(density(CCRmonthly), 
       col = "red", 
       lwd = 3)
  # Boxplot per analisi della distribuzione dei CCR
   boxplot(as.numeric(CCRmonthly), 
           horizontal = TRUE, 
           col="orange",
           xlab="% CCR", 
           main=paste("Boxplot dei CCR mensili dello stock", title, sep=" ", collapse=NULL))
   # Smoothed Density dei CCR
   plot(density(CCRmonthly), 
        type = "l", 
        xlab =  paste("CCR mensili dello stock", title, sep=" ", collapse=NULL), 
        col = "purple", 
        lwd = 3, 
        ylab = "Densità di probabilità", 
        main = paste("Densità di probabilità del rel. valore di CCR", title, sep=" ", collapse=NULL))
   
   # Plot per confronto con distribuzione normale
   qqnorm(CCRmonthly, col="blue", lwd=2)
   qqline(CCRmonthly, col="red", lwd=2)
}

# Plot dei grafici diagnostici
CCRdiagnosticGraphs(TEN.MI.CCReturnMonthly, title = "TEN.MI")
CCRdiagnosticGraphs(ENI.MI.CCReturnMonthly, title = "ENI.MI")
CCRdiagnosticGraphs(EXO.MI.CCReturnMonthly, title = "EXO.MI")
CCRdiagnosticGraphs(AZM.MI.CCReturnMonthly, title = "AZM.MI")
CCRdiagnosticGraphs(REC.MI.CCReturnMonthly, title = "REC.MI")
CCRdiagnosticGraphs(DIA.MI.CCReturnMonthly, title = "DIA.MI")


#BOXPLOTS PER CONFRONTO
par(mfrow=c(1,1))
boxplot(list( TEN.MI=as.numeric(TEN.MI.CCReturnMonthly),
              ENI.MI=as.numeric(ENI.MI.CCReturnMonthly),
              EXO.MI=as.numeric(EXO.MI.CCReturnMonthly),
              AZM.MI=as.numeric(AZM.MI.CCReturnMonthly),
              REC.MI=as.numeric(REC.MI.CCReturnMonthly),
              DIA.MI=as.numeric(DIA.MI.CCReturnMonthly)), 
        col = "orange",
        ylab="Variazione % mensile (%)", 
        xlab="Titoli azionari", 
        main="Boxplots per analisi dei CC returns mensili dei titoli azionari")
abline(h=0.0, lty=2, lwd=2, col="blue")

# Calcolo delle statistiche di media, varianza, dev. standard, kurtosis, skewness e quantili
TEN.MI.media <- mean(TEN.MI.CCReturnMonthly)
TEN.MI.varianza <- var(TEN.MI.CCReturnMonthly)
TEN.MI.stddev <- sd(TEN.MI.CCReturnMonthly)
TEN.MI.kurtosis <- kurtosis(TEN.MI.CCReturnMonthly)
TEN.MI.skewness <- skewness(TEN.MI.CCReturnMonthly)
quantile(TEN.MI.CCReturnMonthly)

ENI.MI.media <- mean(ENI.MI.CCReturnMonthly)
ENI.MI.varianza <- var(ENI.MI.CCReturnMonthly)
ENI.MI.stddev <- sd(ENI.MI.CCReturnMonthly)
ENI.MI.kurtosis <- kurtosis(ENI.MI.CCReturnMonthly)
ENI.MI.skewness <- skewness(ENI.MI.CCReturnMonthly)
quantile(ENI.MI.CCReturnMonthly)

EXO.MI.media <- mean(EXO.MI.CCReturnMonthly)
EXO.MI.varianza <- var(EXO.MI.CCReturnMonthly)
EXO.MI.stddev <- sd(EXO.MI.CCReturnMonthly)
EXO.MI.kurtosis <- kurtosis(EXO.MI.CCReturnMonthly)
EXO.MI.skewness <- skewness(EXO.MI.CCReturnMonthly)
quantile(EXO.MI.CCReturnMonthly)

AZM.MI.media <- mean(AZM.MI.CCReturnMonthly)
AZM.MI.varianza <- var(AZM.MI.CCReturnMonthly)
AZM.MI.stddev <- sd(AZM.MI.CCReturnMonthly)
AZM.MI.kurtosis <- kurtosis(AZM.MI.CCReturnMonthly)
AZM.MI.skewness <- skewness(AZM.MI.CCReturnMonthly)
quantile(AZM.MI.CCReturnMonthly)

REC.MI.media <- mean(REC.MI.CCReturnMonthly)
REC.MI.varianza <- var(REC.MI.CCReturnMonthly)
REC.MI.stddev <- sd(REC.MI.CCReturnMonthly)
REC.MI.kurtosis <- kurtosis(REC.MI.CCReturnMonthly)
REC.MI.skewness <- skewness(REC.MI.CCReturnMonthly)
quantile(REC.MI.CCReturnMonthly)

DIA.MI.media <- mean(DIA.MI.CCReturnMonthly)
DIA.MI.varianza <- var(DIA.MI.CCReturnMonthly)
DIA.MI.stddev <- sd(DIA.MI.CCReturnMonthly)
DIA.MI.kurtosis <- kurtosis(DIA.MI.CCReturnMonthly)
DIA.MI.skewness <- skewness(DIA.MI.CCReturnMonthly)
quantile(DIA.MI.CCReturnMonthly)

# Statistiche aggregate di media, varianza, dev. standard, kurtosis, skewness e quantili
aggr.media <- c(TEN.MI.media, 
               ENI.MI.media, 
               EXO.MI.media, 
               AZM.MI.media, 
               REC.MI.media, 
               DIA.MI.media)
aggr.varianza <- c(TEN.MI.varianza, 
                   ENI.MI.varianza, 
                   EXO.MI.varianza, 
                   AZM.MI.varianza, 
                   REC.MI.varianza, 
                   DIA.MI.varianza)
aggr.stddev <- c(TEN.MI.stddev, 
                 ENI.MI.stddev, 
                 EXO.MI.stddev, 
                 AZM.MI.stddev, 
                 REC.MI.stddev, 
                 DIA.MI.stddev)
aggr.kurtosis <- c(TEN.MI.kurtosis, 
                   ENI.MI.kurtosis, 
                   EXO.MI.kurtosis, 
                   AZM.MI.kurtosis, 
                   REC.MI.kurtosis, 
                   DIA.MI.kurtosis)
aggr.skewness <- c(TEN.MI.skewness, 
                   ENI.MI.skewness, 
                   EXO.MI.skewness, 
                   AZM.MI.skewness, 
                   REC.MI.skewness, 
                   DIA.MI.skewness)

stats <- data.frame(aggr.media, aggr.varianza, aggr.stddev, aggr.kurtosis, aggr.skewness)
rownames(stats) <- stockNames

# Sommario statistiche aggregate
kable(summary(stats))
# Visualizzazione delle statistiche
print(stats)

# Record con media/dev std massima/minima
media.max <- stats[which.max(stats$aggr.media),] 
media.min <-stats[which.min(stats$aggr.media),] 
stddev.max <-stats[which.max(stats$aggr.stddev),] 
stddev.min <-stats[which.min(stats$aggr.stddev),]  

#Visualizzazione statistiche media/dev std massima/minima
print(paste0("Titolo con Media MAX: ", rownames(media.max), " --> ", media.max$aggr.media, "%"))
print(paste0("Titolo con Media MIN: ", rownames(media.min), " --> ", media.min$aggr.media, "%"))
print(paste0("Titolo con Dev STD MAX: ", rownames(stddev.max), " --> ", stddev.max$aggr.media))
print(paste0("Titolo con Dev STD MIN: ", rownames(stddev.min), " --> ", stddev.min$aggr.media))

# Volatilità media dei titoli
par(mfrow = c(1, 1))
names(aggr.stddev) <- stockNames
barplot(aggr.stddev, col="red", main = "Volatilità Dei Titoli (DEV STD)")

# QQPlots per confronto con distribuzioni normali
par(mfrow=c(3,2))
qqnorm(TEN.MI.CCReturnMonthly, main = "QQ-PLOT TEN.MI", col = "red")
qqline(TEN.MI.CCReturnMonthly)

qqnorm(ENI.MI.CCReturnMonthly, main = "QQ-PLOT ENI.MI", col = "green")
qqline(ENI.MI.CCReturnMonthly)

qqnorm(EXO.MI.CCReturnMonthly, main = "QQ-PLOT EXO.MI", col = "blue")
qqline(EXO.MI.CCReturnMonthly)

qqnorm(AZM.MI.CCReturnMonthly, main = "QQ-PLOT AZM.MI", col = "purple")
qqline(AZM.MI.CCReturnMonthly)

qqnorm(REC.MI.CCReturnMonthly, main = "QQ-PLOT REC.MI", col = "black")
qqline(REC.MI.CCReturnMonthly)
   
qqnorm(DIA.MI.CCReturnMonthly, main = "QQ-PLOT DIA.MI", col = "brown")
qqline(DIA.MI.CCReturnMonthly)

# Conversione numerica dei ritorni mensili per la visualizzazione nella matrice di correlazione
TEN.MI.mat <- coredata(TEN.MI.CCReturnMonthly)
ENI.MI.mat <- coredata(ENI.MI.CCReturnMonthly)
EXO.MI.mat <- coredata(EXO.MI.CCReturnMonthly)
AZM.MI.mat <- coredata(AZM.MI.CCReturnMonthly)
REC.MI.mat <- coredata(REC.MI.CCReturnMonthly)
DIA.MI.mat <- coredata(DIA.MI.CCReturnMonthly)

bindedvalues <- cbind(TEN.MI.mat,ENI.MI.mat, EXO.MI.mat, AZM.MI.mat, REC.MI.mat, DIA.MI.mat)
colnames(bindedvalues) <- stockNames

# Matrice di correlazione semplice
pairs(bindedvalues, pch=18, col="blue", main="Matrice di Correlazione tra i titoli azionari")

# Calcolo della matrice di Covarianza
cov(bindedvalues)

par(mfrow = c(1, 1))
# Correlazione
library(heatmaply)
# Calcolo della matrice di Correlazione
cor_matrix <- cor(bindedvalues)
# Heatmap di Correlazione
heatmaply_cor(
  cor_matrix,
  xlab = "Titoli",
  k_col = 2,
  k_row = 2
)


#-------------------------------PREDICTIVE ANALYTICS AND PORTFOLIO MANAGEMENT-------------------------------

# Dataset contenente tutti gli RMSE dei vari modelli ARMA predittivi, 
#   verrà selezionato quello minore per ogni stock
rmseColl <- c()

# Funzione di Forecast, con n -> length training set, m -> length test set, l -> length final set
generateForecastModelsArima <- function(stockName, stock, n, m, l) {
  # Riscarico la ts completa per il forecast con granularità mensile
  #dataset.z <- get.hist.quote( instrument=stockName, start="2007-01-01", quote="AdjClose", provider="yahoo", origin="1970-01-01", compression="month")
  dataset.z <- na.omit(stock)
  # Cambio l'indice della ts utilizzando mese/anno
  index(dataset.z) <- as.yearmon(index(dataset.z))
  colnames(dataset.z) <- stockName
  
  dataset.z <- aggregate(dataset.z, index(dataset.z), tail, 1)
  index(dataset.z) <- as.yearmon(index(dataset.z))
  #Calcolo i CC returns
  returns <- diff(log(dataset.z[,1]))
  # Plotto la seasonal decomposition dello stock per analisi sulla periodicità
  fitRet <- stl(returns[,1], s.window="period")
  plot(fitRet, main=paste("Seasonal Decomposition of, ", stockName ," CC Returns Time Series"))
  
  # Divido tra training e test set
  returnsTrain <- returns[(length(returns) - (n + m + l)):(length(returns) - (m + l))]
  returnsTest <- returns[(length(returns) - (m + l)):(length(returns) - (l))]  
  
  # Computo tutti i modelli di ARMA prediction per 0<=ar<=10 i=0  0<=ma<=10 
  for(ar in 1:10){
    for(ma in 1:10){
      fit <- NULL
      # Provo a trovare un modello per la corrente configurazione di arima, se non lo trovo avrò null
      try (fit <- arima(returnsTrain, order = c(ar, 0, ma)))
      
      # Se il modello è diverso da null plotto la predizione e il relativo confronto con i ritorni di test
      if(!is.null(fit)){
        arma.forecast <- forecast(fit, h = length(returnsTest),level = c(95, 80))
        plot(arma.forecast, main = paste0("ARMA (ar=",ar,", ma=",ma,") forecasts for ", stockName, " returns  Predicted Test vs Test"), xlab = "Data", ylab = "Ritorno (%)")
        grid()
        lines(returnsTest)
        # Appendo al dataset degli RMSE la configurazione attuale di ARIMA con il relativo RMSE sui dati di set
        rmseColl <<- rbind(rmseColl, c(stockName, accuracy(arma.forecast, returnsTest)[2,2], ar, 0, ma))
      }
    }
  }
}
n <- 80
m <- 30
l <- 10
#Forecasting su tutti gli stocks con 80 mesi in training, 30 in test e 10 nella porzione finale
generateForecastModelsArima("TEN.MI", TEN.MI.All, n, m, l)
generateForecastModelsArima("ENI.MI", ENI.MI.All, n, m, l)
generateForecastModelsArima("EXO.MI", EXO.MI.All, n, m, l)
generateForecastModelsArima("AZM.MI", AZM.MI.All, n, m, l)
generateForecastModelsArima("REC.MI", REC.MI.All, n, m, l)
generateForecastModelsArima("DIA.MI", DIA.MI.All, n, m, l)

rmseColl <- as.data.frame(rmseColl)
colnames(rmseColl) <- c("Stock", "RMSE", "ar", "i", "ma")
rmseColl$RMSE <-as.numeric(as.character(rmseColl$RMSE))

# Dataset di analisi di output con i relativi parametri ARMA da utilizzare per la previsione negli L mesi
minErrorModel <- c()

TEN.MI.Models <- rmseColl[rmseColl$Stock == "TEN.MI",]
minErrorModel <- rbind(minErrorModel, TEN.MI.Models[which.min(TEN.MI.Models$RMSE),])

ENI.MI.Models <- rmseColl[rmseColl$Stock == "ENI.MI",]
minErrorModel <- rbind(minErrorModel, ENI.MI.Models[which.min(ENI.MI.Models$RMSE),])

EXO.MI.Models <- rmseColl[rmseColl$Stock == "EXO.MI",]
minErrorModel <- rbind(minErrorModel, EXO.MI.Models[which.min(EXO.MI.Models$RMSE),])

AZM.MI.Models <- rmseColl[rmseColl$Stock == "AZM.MI",]
minErrorModel <- rbind(minErrorModel, AZM.MI.Models[which.min(AZM.MI.Models$RMSE),])

REC.MI.Models <- rmseColl[rmseColl$Stock == "REC.MI",]
minErrorModel <- rbind(minErrorModel, REC.MI.Models[which.min(REC.MI.Models$RMSE),])

DIA.MI.Models <- rmseColl[rmseColl$Stock == "DIA.MI",]
minErrorModel <- rbind(minErrorModel, DIA.MI.Models[which.min(DIA.MI.Models$RMSE),])

# RMSE minimi per stock
print(kable(minErrorModel))

par(mfrow = c(1, 1))
predictL <- function(stockName, stock, n, m, l, ar, i, ma) {
  dataset.z <- na.omit(stock)
  # Cambio l'indice della ts utilizzando mese/anno
  index(dataset.z) <- as.yearmon(index(dataset.z))
  colnames(dataset.z) <- stockName
  
  dataset.z <- aggregate(dataset.z, index(dataset.z), tail, 1)
  index(dataset.z) <- as.yearmon(index(dataset.z))
  returns <- diff(log(dataset.z[,1]))
  returns <- returns[(length(returns) - (n + m + l)):(length(returns) - (l))]
  fit <- arima(returns,order=c(ar,i,ma))
  predictions <- predict(fit, l)
  new_data <- rbind(returns, predictions$pred)
  upper <- predictions$pred + predictions$se 
  lower <- predictions$pred - predictions$se
  
  plot(new_data[(length(new_data)-(m + l)):length(new_data)], xlab="Year", ylab="CC Return", 
       main=paste0("ARMA (ar=",ar,", ma=",ma,") forecasts for ", stockName, " CCR for ", l, " months (TailSet)"))
  
  # Add element of confidence interval
  polygon(c(index(upper), rev(index(upper))), 
          c(upper, rev(lower)), col="lightblue", border=NA) 
  lines(lower, col='green')
  lines(upper, col='green')
  lines(predictions$pred,col='red')
  grid()
  return(predictions$pred)
}

# Funzione per il calcolo "manuale" dell'RMSE (deviazione standard degli scarti)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

plotReturns <- function(predicted, real){
  print(paste("Ritorno Previsto:", predicted, "%"))
  print(paste("Ritorno Reale:", real, "%"))
  barplot(c(predicted, real), ylim = c(-0.6,0.5), col=c("orange", "green"))
  abline(h=0.0, lty=2, lwd=2, col="red")
  grid()
  legend("topleft",
         c("Ritorno Previsto","Ritorno Reale"),
         fill = c("orange", "green")
  )
  print(paste("RMSE:", RMSE(predicted[,1], real[,1])))
}
values <- data.frame()
# Per ogni stock confronto i valori di previsione e quelli reali, determinandone l'RMSE
row <- minErrorModel[minErrorModel$Stock == "TEN.MI",]
TEN.MI.predR <- as.xts(predictL(row$Stock, TEN.MI.All, n, m, l,row$ar, row$i, row$ma))
TEN.MI.predR[,1] <- round(TEN.MI.predR[,1], 2)
real <- window(TEN.MI.CCReturnMonthly, start = as.Date(index(TEN.MI.predR)[1]))
values <- cbind(TEN.MI.predR[,1], real[,1])
colnames(values) <- c("Predetti", "Reali")
print(values)
TEN.MI.rP <- Return.cumulative(values$Predetti)
TEN.MI.rR <- Return.cumulative(values$Reali)
plotReturns(TEN.MI.rP, TEN.MI.rR)


row <- minErrorModel[minErrorModel$Stock == "ENI.MI",]
ENI.MI.predR <- as.xts(predictL(row$Stock, ENI.MI.All, n, m, l,row$ar, row$i, row$ma))
ENI.MI.predR[,1] <- round(ENI.MI.predR[,1], 2)
real <- window(ENI.MI.CCReturnMonthly, start = as.Date(index(ENI.MI.predR)[1]))
values <- cbind(ENI.MI.predR[,1], real[,1])
colnames(values) <- c("Predetti", "Reali")
print(values)
ENI.MI.rP <- Return.cumulative(values$Predetti)
ENI.MI.rR <- Return.cumulative(values$Reali)
plotReturns(ENI.MI.rP, ENI.MI.rR)

row <- minErrorModel[minErrorModel$Stock == "EXO.MI",]
EXO.MI.predR <- as.xts(predictL(row$Stock, EXO.MI.All, n, m, l,row$ar, row$i, row$ma))
EXO.MI.predR[,1] <- round(EXO.MI.predR[,1], 2)
real <- window(EXO.MI.CCReturnMonthly, start = as.Date(index(EXO.MI.predR)[1]))
values <- cbind(EXO.MI.predR[,1], real[,1])
colnames(values) <- c("Predetti", "Reali")
print(values)
EXO.MI.rP <- Return.cumulative(values$Predetti)
EXO.MI.rR <- Return.cumulative(values$Reali)
plotReturns(EXO.MI.rP, EXO.MI.rR)

row <- minErrorModel[minErrorModel$Stock == "AZM.MI",]
AZM.MI.predR <- as.xts(predictL(row$Stock, AZM.MI.All, n, m, l,row$ar, row$i, row$ma))
real <- window(AZM.MI.CCReturnMonthly, start = as.Date(index(AZM.MI.predR)[1]))
values <- cbind(AZM.MI.predR[,1], real[,1])
colnames(values) <- c("Predetti", "Reali")
print(values)
AZM.MI.rP <- Return.cumulative(values$Predetti)
AZM.MI.rR <- Return.cumulative(values$Reali)
plotReturns(AZM.MI.rP, AZM.MI.rR)

row <- minErrorModel[minErrorModel$Stock == "REC.MI",]
REC.MI.predR <- as.xts(predictL(row$Stock, REC.MI.All, n, m, l,row$ar, row$i, row$ma))
real <- window(REC.MI.CCReturnMonthly, start = as.Date(index(REC.MI.predR)[1]))
values <- cbind(REC.MI.predR[,1], real[,1])
colnames(values) <- c("Predetti", "Reali")
print(values)
REC.MI.rP <- Return.cumulative(values$Predetti)
REC.MI.rR <- Return.cumulative(values$Reali)
plotReturns(REC.MI.rP, REC.MI.rR)

row <- minErrorModel[minErrorModel$Stock == "DIA.MI",]
DIA.MI.predR <- as.xts(predictL(row$Stock, DIA.MI.All, n, m, l,row$ar, row$i, row$ma))
real <- window(DIA.MI.CCReturnMonthly, start = as.Date(index(DIA.MI.predR)[1]))
values <- cbind(DIA.MI.predR[,1], real[,1])
colnames(values) <- c("Predetti", "Reali")
print(values)
DIA.MI.rP <- Return.cumulative(values$Predetti)
DIA.MI.rR <- Return.cumulative(values$Reali)
plotReturns(DIA.MI.rP, DIA.MI.rR)
#----------------------------------BETA----------------------------------

# Funzione per il calcolo dinamico di beta
beta_function <- function(stock, market_index){
  beta <- cov(stock, market_index)/var(market_index)
  return(beta)
}

# Per il compute dei beta faccio il retrieve dei ritorni compounded mensili dell'indice di mercato di
#   riferimento (FTSE MIB)
getSymbols("FTSEMIB.MI", source="yahoo")
FTSEMIB.MI <- window(FTSEMIB.MI, start = startDate, end = endDate)[,6]
FTSEMIB.MI.CCReturnMonthly <- round(x=Return.calculate(to.monthly(FTSEMIB.MI), method = "compound"), digits=2)[,4]
FTSEMIB.MI.CCReturnMonthly <- na.fill(FTSEMIB.MI.CCReturnMonthly, 0)


# Calcolo dei Beta semplici per la finestra temporale
beta_TEN.MI <- beta_function(TEN.MI.CCReturnMonthly, FTSEMIB.MI.CCReturnMonthly)
beta_ENI.MI <- beta_function(ENI.MI.CCReturnMonthly, FTSEMIB.MI.CCReturnMonthly)
beta_EXO.MI <- beta_function(EXO.MI.CCReturnMonthly, FTSEMIB.MI.CCReturnMonthly)
beta_AZM.MI <- beta_function(AZM.MI.CCReturnMonthly, FTSEMIB.MI.CCReturnMonthly)
beta_REC.MI <- beta_function(REC.MI.CCReturnMonthly, FTSEMIB.MI.CCReturnMonthly)
beta_DIA.MI <- beta_function(DIA.MI.CCReturnMonthly, FTSEMIB.MI.CCReturnMonthly)


# Beta aggregati
disp <- cbind(beta_TEN.MI, beta_ENI.MI, beta_EXO.MI, beta_AZM.MI, beta_REC.MI, beta_DIA.MI)
colnames(disp) <- stockNames
rownames(disp) <- "Beta"
kable(disp)

# ---Calcolo di beta per diverse finestre temporali---
TEN.MI_betas.xts <- NULL 
ENI.MI_betas.xts <- NULL
EXO.MI_betas.xts <- NULL 
AZM.MI_betas.xts <- NULL
DIA.MI_betas.xts <- NULL 
REC.MI_betas.xts <- NULL

# Finestra temporale (mesi)
delta_t <- 6 
length_period = dim(FTSEMIB.MI.CCReturnMonthly)[1] 
start <- delta_t + 1 

par(mfrow = c(1, 1))
for (i in start:length_period){
  beta_val_TEN.MI <- beta_function(TEN.MI.CCReturnMonthly[(i-delta_t):(i-1)], FTSEMIB.MI.CCReturnMonthly[(i-delta_t):(i-1)])
  beta_val_ENI.MI <- beta_function(ENI.MI.CCReturnMonthly[(i-delta_t):(i-1)], FTSEMIB.MI.CCReturnMonthly[(i-delta_t):(i-1)])
  beta_val_EXO.MI <- beta_function(EXO.MI.CCReturnMonthly[(i-delta_t):(i-1)], FTSEMIB.MI.CCReturnMonthly[(i-delta_t):(i-1)])
  beta_val_AZM.MI <- beta_function(AZM.MI.CCReturnMonthly[(i-delta_t):(i-1)], FTSEMIB.MI.CCReturnMonthly[(i-delta_t):(i-1)])
  beta_val_REC.MI <- beta_function(REC.MI.CCReturnMonthly[(i-delta_t):(i-1)], FTSEMIB.MI.CCReturnMonthly[(i-delta_t):(i-1)])
  beta_val_DIA.MI <- beta_function(DIA.MI.CCReturnMonthly[(i-delta_t):(i-1)], FTSEMIB.MI.CCReturnMonthly[(i-delta_t):(i-1)])
  beta_xts_TEN.MI <- as.xts(beta_val_TEN.MI, order.by = index(TEN.MI.CCReturnMonthly[(i-1)]))
  beta_xts_ENI.MI <- as.xts(beta_val_ENI.MI, order.by = index(ENI.MI.CCReturnMonthly[(i-1)]))
  beta_xts_EXO.MI <- as.xts(beta_val_EXO.MI, order.by = index(EXO.MI.CCReturnMonthly[(i-1)]))
  beta_xts_AZM.MI <- as.xts(beta_val_AZM.MI, order.by = index(AZM.MI.CCReturnMonthly[(i-1)]))
  beta_xts_REC.MI <- as.xts(beta_val_REC.MI, order.by = index(REC.MI.CCReturnMonthly[(i-1)]))
  beta_xts_DIA.MI <- as.xts(beta_val_DIA.MI, order.by = index(DIA.MI.CCReturnMonthly[(i-1)]))
  
  if(is.null(TEN.MI_betas.xts)){
    TEN.MI_betas.xts <- beta_xts_TEN.MI
    ENI.MI_betas.xts <- beta_xts_ENI.MI
    EXO.MI_betas.xts <- beta_xts_EXO.MI
    AZM.MI_betas.xts <- beta_xts_AZM.MI
    REC.MI_betas.xts <- beta_xts_REC.MI
    DIA.MI_betas.xts <- beta_xts_DIA.MI
  }else{
    TEN.MI_betas.xts <- rbind(TEN.MI_betas.xts,beta_xts_TEN.MI)
    ENI.MI_betas.xts <- rbind(ENI.MI_betas.xts,beta_xts_ENI.MI)
    EXO.MI_betas.xts <- rbind(EXO.MI_betas.xts,beta_xts_EXO.MI)
    AZM.MI_betas.xts <- rbind(AZM.MI_betas.xts,beta_xts_AZM.MI)
    REC.MI_betas.xts <- rbind(REC.MI_betas.xts,beta_xts_REC.MI)
    DIA.MI_betas.xts <- rbind(DIA.MI_betas.xts,beta_xts_DIA.MI)
  }
}

# Visualizzazione Beta computati
plot(TEN.MI_betas.xts)
plot(ENI.MI_betas.xts)
plot(EXO.MI_betas.xts)
plot(AZM.MI_betas.xts)
plot(REC.MI_betas.xts)
plot(DIA.MI_betas.xts)

TEN.MI_betas <- as.xts(c(rep(NA,delta_t), as.numeric(TEN.MI_betas.xts)), order.by = index(TEN.MI.CCReturnMonthly))
ENI.MI_betas <- as.xts(c(rep(NA,delta_t), as.numeric(ENI.MI_betas.xts)), order.by = index(TEN.MI.CCReturnMonthly))
EXO.MI_betas <- as.xts(c(rep(NA,delta_t), as.numeric(EXO.MI_betas.xts)), order.by = index(TEN.MI.CCReturnMonthly))
AZM.MI_betas <- as.xts(c(rep(NA,delta_t), as.numeric(AZM.MI_betas.xts)), order.by = index(TEN.MI.CCReturnMonthly))
REC.MI_betas <- as.xts(c(rep(NA,delta_t), as.numeric(REC.MI_betas.xts)), order.by = index(TEN.MI.CCReturnMonthly))
DIA.MI_betas <- as.xts(c(rep(NA,delta_t), as.numeric(DIA.MI_betas.xts)), order.by = index(TEN.MI.CCReturnMonthly))

# Ritorno atteso degli stocks utilizzando Beta
tasso_risk <- 0.04
RISKFREE.ret <- (tasso_risk+1)^(10/12)
FTSEMIB.MI <- to.monthly(FTSEMIB.MI)

FTSE.MI.ret <- as.numeric(FTSEMIB.MI$FTSEMIB.MI.Close[as.yearmon("ott 2020")])/as.numeric(FTSEMIB.MI$FTSEMIB.MI.Close[as.yearmon("gen 2020")])
TEN.MI_betas.EXP_RET <- RISKFREE.ret+as.numeric(TEN.MI_betas[as.yearmon("gen 2020")])*(FTSE.MI.ret - RISKFREE.ret)
ENI.MI_betas.EXP_RET <- RISKFREE.ret+as.numeric(ENI.MI_betas[as.yearmon("gen 2020")])*(FTSE.MI.ret - RISKFREE.ret)
EXO.MI_betas.EXP_RET <- RISKFREE.ret+as.numeric(EXO.MI_betas[as.yearmon("gen 2020")])*(FTSE.MI.ret - RISKFREE.ret)
AZM.MI_betas.EXP_RET <- RISKFREE.ret+as.numeric(AZM.MI_betas[as.yearmon("gen 2020")])*(FTSE.MI.ret - RISKFREE.ret)
REC.MI_betas.EXP_RET <- RISKFREE.ret+as.numeric(REC.MI_betas[as.yearmon("gen 2020")])*(FTSE.MI.ret - RISKFREE.ret)
DIA.MI_betas.EXP_RET <- RISKFREE.ret+as.numeric(DIA.MI_betas[as.yearmon("gen 2020")])*(FTSE.MI.ret - RISKFREE.ret)

exp_rets <- cbind(TEN.MI_betas.EXP_RET, ENI.MI_betas.EXP_RET, EXO.MI_betas.EXP_RET, AZM.MI_betas.EXP_RET, REC.MI_betas.EXP_RET, DIA.MI_betas.EXP_RET)
kable(exp_rets)

par(mfrow=c(1,1))
plot(TEN.MI_betas, type="l", main="Beta TEN.MI")
plot(ENI.MI_betas, type="l", main="Beta ENI.MI")
plot(EXO.MI_betas, type="l", main="Beta EXO.MI")
plot(AZM.MI_betas, type="l", main="Beta AZM.MI")
plot(REC.MI_betas, type="l", main="Beta REC.MI")
plot(DIA.MI_betas, type="l", main="Beta DIA.MI")

#-------------------------PORTFOLIO OPTIMIZATION-------------------------

# Giorno prima della costituzione del portfolio (L mesi prima della costituzione del portfolio, per L=10 in questo caso
#   al 1 dicembre 2019, ma visto che non ci sono dati per questa data porto in avanti di 3 giorni (il primo dato disponibile))
yesterday <- index(stocks[dim(stocks)[1]]) %m+% months(-l) %m+% days(3)

# Computo i CAGR per gli stocks 
TEN.MI.r <- periodReturn( x=TEN.MI.All, period="yearly", subset=paste0("/",yesterday) )
ENI.MI.r <- periodReturn( x=ENI.MI.All, period="yearly", subset=paste0("/",yesterday) )
EXO.MI.r <- periodReturn( x=EXO.MI.All, period="yearly", subset=paste0("/",yesterday) )
AZM.MI.r <- periodReturn( x=AZM.MI.All, period="yearly", subset=paste0("/",yesterday) )
REC.MI.r <- periodReturn( x=REC.MI.All, period="yearly", subset=paste0("/",yesterday) )
DIA.MI.r <- periodReturn( x=DIA.MI.All, period="yearly", subset=paste0("/",yesterday) )

# Aggrego i dati
bindedvalues <- cbind(TEN.MI.r, ENI.MI.r, EXO.MI.r, AZM.MI.r, REC.MI.r, DIA.MI.r)
index(bindedvalues) <- as.yearmon(index(bindedvalues))
colnames(bindedvalues) <- stockNames

# Calcolo del modello ottimale dei pesi
Mop <- portfolio.optim(x=bindedvalues, shorts = FALSE)
Mop$pw <- round(Mop$pw, 2)

# Visualizzazione grafica dei pesi
bar<- round(100*Mop$pw/sum(Mop$pw), 1)
pie(Mop$pw, bar, main = "Rappresentazione dei pesi del portfolio alla costituzione (%)",col = rainbow(6))
legend("bottomleft", stockNames, cex = 0.8,fill = rainbow(6))

cat("peso di TEN.MI:",Mop$pw[1],"\n")
cat("peso di ENI.MI:",Mop$pw[2],"\n")
cat("peso di EXO.MI:",Mop$pw[3],"\n")
cat("peso di AZM.MI:",Mop$pw[4],"\n")
cat("peso di REC.MI:",Mop$pw[5],"\n")
cat("peso di DIA.MI:",Mop$pw[6],"\n")
cat("Somma dei pesi:",sum(Mop$pw),"\n")

# Costituzione della frontiera efficiente
rs <- seq(0.0,1.0,length.out=150)
risk <- numeric(length(rs))+NA
for( i in 1:length(rs) ) {
  p <- NULL
  try( p <- portfolio.optim( x=bindedvalues, pm=rs[i] ) )
  if( is.null(p) ) {
    risk[i] <- NA
  } else {
    risk[i] <- p$ps
  }
}

plot(risk, rs, pch=20, col="blue", xlab="risk (sigma)", ylab="return (mean)")
points(Mop$ps, Mop$pm, pch=17, col="red")

# Simulazione di strategia buy and hold con budget di 5000 EUR con costi di transazione dell'1% per share 
#   dall'inzio degli L month all'1 Ottobre 2020

# Budget
budget <- 5000 #V
trans_perc = 0.03

# Computo il numero di shares in base al peso trovato dalla configurazione ottimale del portfolio
TEN.MI.shares <- floor((budget * Mop$pw[1])/( as.numeric(TEN.MI.All[yesterday]) + (as.numeric(TEN.MI.All[yesterday])*trans_perc) ) ) 
ENI.MI.shares <- floor((budget * Mop$pw[2])/( as.numeric(ENI.MI.All[yesterday]) + (as.numeric(ENI.MI.All[yesterday])*trans_perc) ) )
EXO.MI.shares <- floor((budget * Mop$pw[3])/( as.numeric(EXO.MI.All[yesterday]) + (as.numeric(EXO.MI.All[yesterday])*trans_perc) ) )
AZM.MI.shares <- floor((budget * Mop$pw[4])/( as.numeric(AZM.MI.All[yesterday]) + (as.numeric(AZM.MI.All[yesterday])*trans_perc) ) )
REC.MI.shares <- floor((budget * Mop$pw[5])/( as.numeric(REC.MI.All[yesterday]) + (as.numeric(REC.MI.All[yesterday])*trans_perc) ) )
DIA.MI.shares <- floor((budget * Mop$pw[6])/( as.numeric(DIA.MI.All[yesterday]) + (as.numeric(DIA.MI.All[yesterday])*trans_perc) ) )


# Calcolo la spesa totale per ogni stock, considerando i costi di transazione per share dell'1%
spesa.TEN.MI <- TEN.MI.shares * (as.numeric(TEN.MI.All[yesterday])+(as.numeric(TEN.MI.All[yesterday])*trans_perc))
spesa.ENI.MI <- ENI.MI.shares * (as.numeric(ENI.MI.All[yesterday])+(as.numeric(ENI.MI.All[yesterday])*trans_perc))
spesa.EXO.MI <- EXO.MI.shares * (as.numeric(EXO.MI.All[yesterday])+(as.numeric(EXO.MI.All[yesterday])*trans_perc))
spesa.AZM.MI <- AZM.MI.shares * (as.numeric(AZM.MI.All[yesterday])+(as.numeric(AZM.MI.All[yesterday])*trans_perc))
spesa.REC.MI <- REC.MI.shares * (as.numeric(REC.MI.All[yesterday])+(as.numeric(REC.MI.All[yesterday])*trans_perc))
spesa.DIA.MI <- DIA.MI.shares * (as.numeric(DIA.MI.All[yesterday])+(as.numeric(DIA.MI.All[yesterday])*trans_perc))

# Composizione del portfolio alla data di costituzione (shares e spesa)
cat("Composizione del Markowitz optimal portfolio:\n")
cat(" - TEN:",TEN.MI.shares,"quote a",as.numeric(TEN.MI.All[yesterday]),"EUR cad. -->", spesa.TEN.MI,"EUR\n")
cat(" - ENI:",ENI.MI.shares,"quote a",as.numeric(ENI.MI.All[yesterday]),"EUR cad. -->", spesa.ENI.MI,"EUR\n")
cat(" - EXO:",EXO.MI.shares,"quote a",as.numeric(EXO.MI.All[yesterday]),"EUR cad. -->", spesa.EXO.MI,"EUR\n")
cat(" - AZM:",AZM.MI.shares,"quote a",as.numeric(AZM.MI.All[yesterday]),"EUR cad. -->", spesa.AZM.MI,"EUR\n")
cat(" - REC:",REC.MI.shares,"quote a",as.numeric(REC.MI.All[yesterday]),"EUR cad. -->", spesa.REC.MI,"EUR\n")
cat(" - DIA:",DIA.MI.shares,"quote a",as.numeric(DIA.MI.All[yesterday]),"EUR cad. -->", spesa.DIA.MI,"EUR\n")

# Spesa totale alla data di costituzione
investimento <- spesa.TEN.MI + spesa.ENI.MI + spesa.EXO.MI +spesa.AZM.MI +spesa.REC.MI+spesa.DIA.MI
cat("\nInvestimento totale per il Markowitz optimal portfolio:",investimento,"EUR\n")
# Differenza tra budget e investimento
cat("Residuo:",budget-investimento,"EUR\n")

# Ripartizione Investimento-Residuo mostrata su grafico a torta
pie(c(investimento, (budget-investimento)), labels=c(round(investimento, 2), round((budget-investimento), 2)), col = rainbow(2), main = "Investimento totale e residuo non utilizzato")
legend("topright", c("Investimento", "Residuo"), cex = 0.8,fill = rainbow(2))

# Prezzi degli stocks alla data finale (1 ottobre 2020)
TEN.MI.p1 <- as.numeric(TEN.MI.All[endDate])
ENI.MI.p1 <- as.numeric(ENI.MI.All[endDate])
EXO.MI.p1 <- as.numeric(EXO.MI.All[endDate])
AZM.MI.p1 <- as.numeric(AZM.MI.All[endDate])
REC.MI.p1 <- as.numeric(REC.MI.All[endDate])
DIA.MI.p1 <- as.numeric(DIA.MI.All[endDate])

# Prezzi degli stocks alla data iniziale, prima di L mesi (3 dicembre 2019 NB -> 1 dicembre NA)
TEN.MI.p0 <- as.numeric(TEN.MI.All[yesterday])
ENI.MI.p0 <- as.numeric(ENI.MI.All[yesterday])
EXO.MI.p0 <- as.numeric(EXO.MI.All[yesterday])
AZM.MI.p0 <- as.numeric(AZM.MI.All[yesterday])
REC.MI.p0 <- as.numeric(REC.MI.All[yesterday])
DIA.MI.p0 <- as.numeric(DIA.MI.All[yesterday])

# Valore del portfolio alla end date
Mop.value <- TEN.MI.shares*TEN.MI.p1 + ENI.MI.shares*ENI.MI.p1 + EXO.MI.shares*EXO.MI.p1 + AZM.MI.shares*AZM.MI.p1 + REC.MI.shares*REC.MI.p1 + DIA.MI.shares*DIA.MI.p1
cat("\nValore del Markowitz optimal portfolio alla fine del periodo:",Mop.value,"EUR\n")

# Ritorno del portfolio alla end date
Mop.ret <- Mop$pw[1]*(TEN.MI.p1/TEN.MI.p0-1) + Mop$pw[2]*(ENI.MI.p1/ENI.MI.p0-1) + Mop$pw[3]*(EXO.MI.p1/EXO.MI.p0-1) + Mop$pw[4]*(AZM.MI.p1/AZM.MI.p0-1) + Mop$pw[5]*(REC.MI.p1/REC.MI.p0-1) + Mop$pw[6]*(DIA.MI.p1/DIA.MI.p0-1) - trans_perc
# Ritorno del portfolio utilizzando i pesi della portfolio optimization e i ritorni predetti con ARMA
Mop.forecastRet <- Mop$pw[1]*TEN.MI.rP[1,1] + Mop$pw[2]*ENI.MI.rP[1,1] + Mop$pw[3]*EXO.MI.rP[1,1] + Mop$pw[4]*AZM.MI.rP[1,1] +Mop$pw[5]*REC.MI.rP[1,1] +Mop$pw[6]*DIA.MI.rP[1,1] - trans_perc
# Ritorno atteso del Mop, parametro di ritorno di portfolio optim
cat("Ritorno atteso del Mop:",Mop$pm-trans_perc,"[",round(100*Mop$pm-trans_perc,2),"% ]\n")
# Ritorno del portfolio alla end date
cat("Ritorno effettivo del Mop:",Mop.ret,"[",round(100*Mop.ret,2),"% ]")
# Ritorno del portfolio utilizzando i pesi della portfolio optimization e i ritorni predetti con ARMA
cat("Ritorno forecast con pesi del Mop:", Mop.forecastRet,"[",round(100*Mop.forecastRet,2),"% ]" )
# Confronto grafico dei ritorni
pairret <- cbind(round(100*Mop$pm-trans_perc,2),round(100*Mop.ret,2),round(100*Mop.forecastRet,2))
colnames(pairret) <- c("Ritorno Atteso", "Ritorno Effettivo", "Ritorno forecasted")
barplot(pairret, main="Ritorni del portfolio (atteso, effettivo e forecast) a confronto", ylab = "Ritorno (%)", ylim=c(-5,15))
abline(h=0.0, lty=2, lwd=2, col="blue")
