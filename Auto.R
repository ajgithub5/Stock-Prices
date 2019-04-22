ts16<-readxl::read_excel("Y:/Capstonne Project/Data_Set_16.xlsx")
ts17<-readxl::read_excel("Y:/Capstonne Project/Data_Set_17.xlsx")

library('ggplot2')
library('tseries')
library('forecast')
library('lubridate')

auto16<-ts16[,c(1,3)]
auto17<-ts17[,c(1,3)]

head(auto16)

################Examine the data..................................................................
auto16$DATE<-as.character(auto16$DATE)
auto16$DATE<-as.Date(auto16$DATE,"%Y-%m-%d",tz="UTC")
auto16$Weekday<-wday(auto16$DATE,label=T)
auto17$DATE<-as.Date(auto17$DATE, "%Y-%m-%d",tz="UTC")


ggplot(auto16, aes(DATE,AUTO)) + 
  geom_line() + scale_x_date('week') +
  ylab("Auto") +
  xlab("")

auto16_ts<-ts(auto16[,c('AUTO')])

#Smoothing by Moving Average
auto16$ma<-ma(auto16$AUTO,order=7)
auto16$ma22<-ma(auto16$AUTO,order=22)

ggplot()+
  geom_line(data=auto16,aes(x=DATE,y=AUTO,colour='AUTO original')) + 
  geom_line(data=auto16,aes(x=DATE,y=ma,colour='Auto Weekly Moving Avg'))+
  geom_line(data=auto16,aes(x=DATE,y=ma22,colour='Auto Monthly Moving Avg'))+
  xlab('Year')+ylab('Auto')

#Decomposing the data............................................
automa_ts<-ts(na.omit(auto16$ma),frequency=7)

#..............Check Seasonality..................................

fit <- tbats(automa_ts)
seasonal <- !is.null(fit$seasonal)
seasonal
#..................................................................

autodecomp<-stl(automa_ts,s.window = 'periodic')
autodeseasonal<-seasadj(autodecomp)
plot(autodecomp)

#ADF test for non stationarity....................................
adf.test(automa_ts,alternative = "stationary")  #pvalue = 0.4607 Not stationary
plot.ts(automa_ts)

automa_tsd1<-diff(automa_ts,differences = 1)  
plot.ts(automa_tsdiff1) #seems to be stationary with mean at 0
adf.test(automa_tsdiff1,alternative = "stationary") #pvalue less than 0.01. stationary

#automa_tsdiff2<-diff(automa_ts,differnces = 2)
#plot.ts(automa_tsdiff2)

#automa_tsdiff3<-diff(automa_ts,differences = 3)
#plot.ts(automa_tsdiff3)

#Autocorrelations and choosing model order..................................
#ACF plots can help in determining the order of the M A (q) model. 
#Partial autocorrelation plots (PACF), as the name suggests, display 
#correlation between a variable and its lags that is not explained by 
#previous lags. PACF plots are useful when determining the order of the AR(p) model.

Acf(automa_ts,main='')
Pacf(automa_ts,main='')

autodeseasonald1<-diff(autodeseasonal,differences = 1)
plot(autodeseasonald1)
adf.test(autodeseasonald1,alternative = 'stationary')

Acf(automa_tsd1,main='ACF for Differenced 1 Series')
Pacf(automa_tsd1,main='Pacf for Differenced 1 Series')

acf(automa_tsd1, lag.max=20, na.action = na.pass, plot=FALSE) 
pacf(automa_tsd1, lag.max=20,na.action = na.pass, plot=FALSE) 


#Model 1
auto.arima(automa_ts,seasonal = FALSE)
#ARIMA(2,0,2) sigma^2 estimated as 3870.759:  log likelihood=-9604.27
#AIC=19220.54   AICc=19220.59   BIC=19253.28

autofit_1 = auto.arima(automa_tsd1, seasonal=FALSE)
tsdisplay(residuals(autofit_1), lag.max=45, main='(2,0,2) Model Residuals')

autoforecast_1 <- forecast:::forecast.Arima(autofit_1, h=365)
head(autoforecast_1)
forecast:::plot.forecast(autoforecast_1)

autoforecast_1$model
accuracy(autoforecast_1,auto17$AUTO)
(23626.7/mean(auto17$AUTO))*100   
###############################################
auto.arima(automa_ts,seasonal = FALSE)
#(2,1,2)

autofit1_1 = auto.arima(automa_ts, seasonal=FALSE)
tsdisplay(residuals(autofit1_1), lag.max=45, main='(2,1,2) Model Residuals')

autoforecast1_1 <- forecast:::forecast.Arima(autofit1_1, h=365)
head(autoforecast1_1)
forecast:::plot.forecast(autoforecast1_1)

autoforecast1_1$model
accuracy(autoforecast1_1,auto17$AUTO)
(2760.06/mean(auto17$AUTO))*100  #11.7% 

#Model 2.............................................................................
auto.arima(automa_ts,seasonal = TRUE)
#ARIMA(1,1,1)(2,0,0)[7]
#sigma^2 estimated as 2868.73:  log likelihood=-9346.26
#AIC=18704.51   AICc=18704.56   BIC=18737.25

autofit1_2 = auto.arima(automa_ts, seasonal=TRUE)
tsdisplay(residuals(autofit1_2), lag.max=45, main='(2,1,2) Model Residuals')

autoforecast1_2 <- forecast:::forecast.Arima(autofit1_2, h=365)
head(autoforecast1_2)
forecast:::plot.forecast(autoforecast1_2)

autoforecast1_2$model
accuracy(autoforecast1_2,auto17$AUTO)

#                         ME          RMSE           MAE               MPE          MAPE
#Training set    0.005647020567   53.46767725   26.11085707 -0.00004650919161  0.1977733471
#Test set     2609.965319732391 2775.37485492 2609.96531973 10.86409195814890 10.8640919581
#                     MASE            ACF1
#Training set  0.5050919038 -0.003043266611
#Test set     50.4875174690              NA


(2775.37/mean(auto17$AUTO))*100     #11.76

#MOdel 3............................................................................
# ARIMA(3,1,2) model, 
autofit1_3 <- arima(automa_ts, order=c(3,1,2))
autofit1_3$aic   #20373.33

autoforecast1_3 <- forecast:::forecast.Arima(autofit1_3, h=365)
head(autoforecast1_3)
forecast:::plot.forecast(autoforecast1_3)

autoforecast1_3$model
accuracy(autoforecast1_3,auto17$AUTO)

#                       ME         RMSE           MAE           MPE          MA
# Training set    2.268519919   63.1573881   30.64517779  0.0192563593  0.2328522081
# Test set     3537.689883129 3809.8795836 3537.68988313 14.6850822778 14.6850822778
#                   MASE            ACF1
#Training set  0.592804409 -0.001115504229
#Test set     68.433545237              NA

(3809.87/mean(auto17$AUTO))*100  #16.14%

# Holt-Winters Function
# beta = FALSE - No exponential smoothening, Gamma = FALSE - No seasonality
autofit1_6 <- HoltWinters(automa_ts, beta=FALSE, gamma=FALSE)  
plot(autofit1_6)

autoforecast1_6 = forecast:::forecast.HoltWinters(autofit1_6, h= 365)
autoforecast1_6

autoforecast1_6$model
accuracy(autoforecast1_6,auto17$AUTO)
(3890.67/mean(auto17$AUTO))*100 #16.49%

forecast:::plot.forecast(autoforecast1_6)

# beta = True - exponential smoothening, Gamma = FALSE - No seasonality
autofit1_7 <- HoltWinters(automa_ts, beta=TRUE, gamma=FALSE)  
plot(autofit1_7)

autoforecast1_7 = forecast:::forecast.HoltWinters(autofit1_7, h= 365)
autoforecast1_7

autoforecast1_7$model
accuracy(autoforecast1_7,auto17$AUTO)

#                       ME         RMSE           MAE              MPE          MAPE
#Training set     0.051059095   68.2211428   30.87680204   0.002996417781  0.2317731031
#Test set     -2637.526723587 3526.8929333 2860.73122668 -10.708820004002 11.7372734601
#                    MASE           ACF1
#Training set  0.5972849795 -0.04372771478
#Test set     55.3383666401             NA

(3526.89/mean(auto17$AUTO))*100 #14.94%

forecast:::plot.forecast(autoforecast1_7)

# beta = True - exponential smoothening, Gamma = TRUE - No seasonality
autofit1_8 <- HoltWinters(automa_ts, beta=TRUE, gamma=TRUE)  
plot(autofit1_8)

autoforecast1_8 = forecast:::forecast.HoltWinters(autofit1_8, h= 365)
autoforecast1_8

autoforecast1_8$model
accuracy(autoforecast1_8,auto17$AUTO)

#                       ME          RMSE           MAE             MPE         MAPE
#Training set    0.02975045387   77.70750853   37.18851266  0.002378257249 0.2952891244
#Test set     -729.25831214371 1496.68737077 1177.80768467 -2.852034510949 4.8762919337
#                     MASE          ACF1
#Training set  0.719379552 -0.2978656706
#Test set     22.783669042            NA

(1496.68/mean(auto17$AUTO))*100 #6.34%

forecast:::plot.forecast(autoforecast1_7)

