rm(list=ls())
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(tseries)
library(shiny)

stockNames <- c("TEN.MI", "ENI.MI", "EXO.MI", "AZM.MI", "REC.MI", "DIA.MI")
for (s in stockNames) {
  getSymbols(s, source="yahoo")
}
rm(s)

startDate <- as.Date("2018-10-1")
endDate <- as.Date("2020-10-1")

TEN.MI <- window(TEN.MI, start = startDate, end = endDate)[,6]
ENI.MI <- window(ENI.MI, start = startDate, end = endDate)[,6]
EXO.MI <- window(EXO.MI, start = startDate, end = endDate)[,6]
AZM.MI <- window(AZM.MI, start = startDate, end = endDate)[,6]
DIA.MI <- window(DIA.MI, start = startDate, end = endDate)[,6]
REC.MI <- window(REC.MI, start = startDate, end = endDate)[,6]

stocks <- round(x = merge(TEN.MI, ENI.MI, EXO.MI, AZM.MI, REC.MI, DIA.MI ), digits = 2)
colnames(stocks) <- stockNames
tformat(stocks) <- "%d %B %y"

stocks <- na.omit(stocks)
stocks.compoundReturns <- round(Return.calculate(to.monthly(stocks, OHLC = FALSE), method = "compound"), digits=2)[-1,]


#### WEB APPLICATION ####

setwd("./webapp")
source("server.r")
source("ui.r")

shinyApp(ui, server)