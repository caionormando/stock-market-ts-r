#Libraries for this model
#install.packages(c("fpp2", "quantmod"))

library(fpp2)
library(quantmod)

#Data scrapping start date
start = "2001-01-01"
end   = Sys.Date()

#RADL3.SA is Droga Raia (Example). You can use whatever you wish (assuming they exist in the data source you specify in src parameter)
getSymbols("RADL3.SA", src = "yahoo", 
           from = start, 
           to = end,
           auto.assign=TRUE)

# Plotting

Cl(na.omit(RADL3.SA)) %>% 
  autoplot()+
  xlab("Ano")+
  ylab("Fechamento em US$")

class(RADL3.SA)

head(RADL3.SA)
tail(RADL3.SA)

# Using daily close value for creating the model

cot_mensal <- to.monthly(RADL3.SA$RADL3.SA.Close,
                         indexAt = "lastof",
                         OHLC = FALSE)

head(cot_mensal)

tail(cot_mensal)

class(cot_mensal)

cot.mensal <- na.omit(ts(cot_mensal))

ggAcf(cot.mensal)

ggPacf(cot.mensal)

ndiffs(cot.mensal)

diff.cot.mensal <- diff(cot.mensal)

library(gridExtra)

g1 <- ggAcf(diff.cot.mensal)
g2 <- ggPacf(diff.cot.mensal)

grid.arrange(g1, g2, nrow =2)

ggtsdisplay(diff.cot.mensal)


# Seasonal Arima(p, d, q) 

(model_arima <- Arima(cot.mensal, order = c(0,1,0),seasonal = TRUE))

model_auto_arima <- auto.arima(cot.mensal, stepwise = FALSE,
                    approximation = TRUE)

model

model_auto_arima$fitted

forecast <- forecast(model_arima, h = 12)
forecastAR <- forecast(model_auto_arima, h = 12)

forecast
forecastAR

autoplot(forecast(model_auto_arima),series = "Previsão")+
  autolayer(model_auto_arima$fitted, series = "Modelo", lwd =1.5)+
  autolayer(cot.mensal, series = "Cotação Real", lwd = 1.5)+
  xlab("Meses")+
  ylab("Cotação Mensal")+
  ggtitle("Droga Raia Cotação em US$ - NYSE")

accuracy(forecast)
accuracy(forecastAR)
