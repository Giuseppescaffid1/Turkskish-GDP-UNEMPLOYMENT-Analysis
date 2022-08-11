library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(ggplot2)

library(readxl)
Philippines_data <- read_excel("Desktop/Subject UNIMI/Project Macro/Philippines data.xlsx", 
                               range = "A3:E83")
View(Philippines_data)

okun <- Philippines_data
okun %>% drop_na()


#scatterplot

ggplot(data = okun)+
  geom_point(mapping = aes( x= unem, y = real_gdp_growth))

# declare our time series variables 
gdp <- ts(okun$real_gdp_growth, start = c( 1999,3), frequency = 4) #we put 4 beacuse is quarterly
unem <- ts(okun$unem, start = c( 1999,3), frequency = 4 )

#plot series 
autoplot(cbind(gdp,unem))

acf(gdp, main= "ACF Real GDP Growth",na.action = na.pass)
acf(unem, main= "ACF Unemployment",na.action = na.pass)


#finding the Optimal lags 
okun.bv <- cbind(gdp, unem)
colnames(okun.bv) <- cbind("GDP", "Unemployment")

lagselect <- VARselect(okun.bv, lag.max = 10, type = "const")


# Building VAR
ModelOkun1 <- VAR(okun.bv, p= 4, type = "const", season = NULL, exogen = NULL) 
summary(ModelOkun1)

#Diagnosis VAR
#Serial Correlation 
Serial1 <- serial.test(ModelOkun1, lags.pt = 12, type = "PT.asymptotic")
Serial1

#Heteroscedasticity
Arch1 <- arch.test(ModelOkun1, lags.multi = 12, multivariate.only = TRUE)
Arch1

#Normal Distribution of the residuals 
Norm1 <- normality.test(ModelOkun1, multivariate.only = TRUE)
Norm1

#Structural Breaks in the residuals 
Stability1 <- stability(ModelOkun1, type = "OLS-CUSUM")
plot(Stability1)

#Granger Causality 

GrangerGDP <- causality(ModelOkun1, cause = "GDP")
GrangerGDP

GrangerUnemployment <- causality(ModelOkun1, cause ='Unemployment') 
GrangerUnemployment

#IRF 
GDPirf <- irf(ModelOkun1, impulse = 'Unemployment', response = 'GDP', n.ahead = 20 , boot = TRUE) 
plot(GDPirf, ylab= 'GDP', main = 'shock from unemployment') 

unemploymentIRF <- irf(ModelOkun1, impulse = 'GDP', response = 'Unemployment', n.ahead = 20 , boot = TRUE)
plot(unemploymentIRF, ylab = 'Unemployment', main = 'shock from GDP') 

plot(unemploymentIRF, main = 'shock from GDP')

#VAR Decomp

FEVD1 <- fevd(ModelOkun1, n.ahead = 10)
plot(FEVD1)

# VAR forecasting 

forecast <- predict(ModelOkun1, n.ahead = 10, ci= 0.95)
fanchart(forecast, names = 'GDP')
fanchart(forecast, names = "Unemployment")
