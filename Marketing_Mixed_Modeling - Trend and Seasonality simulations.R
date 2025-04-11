##########################################################################
##########################################################################
##########################################################################
#Conclusions:
#1) The best approach would be to include time and seasonality variables in the regression equation, e.g.:
#    y~x+t+s
#  There are a couple of ways to compute seasonality: 
#     i) Via Fourier transform (before applying Fourier transformation we need to detrend the time series)
#         The functions can be sin(freq*(2*pi)*(t+a)), cos(freq*(2*pi)*(t+a)), where freq is provided by the output of the 'periodogram' function, 
#         and a can be computed from the 'nlsLM' function.   
#     ii) Have a binary variable for each month, e.g. Feb, Mar, ..., Dec
#2) i) Decompose the time series first using y~t+s. 
#   ii) Regress the independent variables on the residuals, y.res~x+t+s. 
#3) i) Decompose the time series first using 'decompose' function in R (instead of 'stl' function). Note that this approach drops some data.
#   ii) Regress the independent variables on the residuals (random part of the decomposition), y.res~x+t+s. 
#4) i) Decompose both dependent and independent variables using any of the above two methods.
#   ii) Regress the errors of the independent variables on the errors of the dependent variables, y.res~x.res+t+s. 

# NOTE 1: R^2 can be inflated because of the t and s terms. Therefore, in order to calculate the R^2 from the 
#       independent variables only, then we should decompose the dependent variable first, and then regress the independent variables on the errors, i.e. approaches 2, 3, 4 above. 
# NOTE 2: Approach 1 and 4 should give the same coefficients for the independent variables. 
##########################################################################
##########################################################################
##########################################################################
# x = N(0,2^2)
# y = 10 + 0*x + 12*t + N(0,3^2)
t = seq(1, 1000)
x = rnorm(length(t), 0, 200)
y = 10 + 0*x + 12*t + rnorm(1000, 0, 300)
acf(y)
summary(lm(y~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

y.trend.remove = y - 12*t
summary(lm(y.trend.remove~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.remove~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
##########################################################################
# x = 3*t + N(0,2^2)
# y = 10 + 0*x + 12*t + N(0,3^2)
t = seq(1, 1000)
x = 3*t + rnorm(length(t), 0, 200)
y = 10 + 0*x + 12*t + rnorm(1000, 0, 300)
acf(y)
summary(lm(y~x)) #WRONG RELATIONSHIP, WRONG ESTIMATE
summary(lm(y~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

y.trend.remove = y - 12*t
summary(lm(y.trend.remove~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.remove~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
##########################################################################
# x = N(0,2^2)
# y = 10 + 2*x + 12*t + N(0,3^2)
t = seq(1, 1000)
x = 0*t + rnorm(length(t), 0, 200)
y = 10 + 2*x + 12*t + rnorm(1000, 0, 300)
acf(y)
summary(lm(y~x)) #CORRECT RELATIONSHIP, WRONG ESTIMATE
summary(lm(y~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

y.trend.remove = y - 12*t
summary(lm(y.trend.remove~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.remove~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
##########################################################################
# x = 3*t + N(0,2^2)
# y = 10 + 2*x + 12*t + N(0,3^2)
t = seq(1, 1000)
x = 3*t + rnorm(length(t), 0, 200)
y = 10 + 2*x + 12*t + rnorm(1000, 0, 300)
acf(y)
summary(lm(y~x)) #CORRECT RELATIONSHIP, WRONG ESTIMATE
summary(lm(y~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

y.trend.remove = y - 18*t
summary(lm(y.trend.remove~x)) #CORRECT RELATIONSHIP, WRONG ESTIMATE 
summary(lm(y.trend.remove~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

x.trend.remove = x - 3*t
summary(lm(y.trend.remove~x.trend.remove)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.remove~x.trend.remove+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

##########################################################################
##########################################################################
##########################################################################
##########################################################################
# x = N(0,2^2)
# s = sin(1/50)
# y = 10 + 0*x + 12*t + 1000*s + N(0,3^2)
t = seq(1, 1000)
s = sin(4*(2*pi/length(t))*t)
x = rnorm(length(t), 0, 200)
y = 10 + 0*x + 12*t + 1000*s + rnorm(1000, 0, 300)
acf(y)
summary(lm(y~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y~x+t)) #CORRECT RELATIONSHIP, WRONG ESTIMATE
summary(lm(y~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

y.trend.remove = y - 12*t
summary(lm(y.trend.remove~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.remove~x+t)) #WRONG RELATIONSHIP, WRONG ESTIMATE
summary(lm(y.trend.remove~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

# In order to detect the frequency of a wave function using Fourier transformation, we need to remove the trend from the time series
ts.trend <- lm(y ~ t)
plot(ts.trend, col="red")
detrended.ts <- ts.trend$residuals
plot(detrended.ts, type="l", main="detrended time series")
library(TSA)
pgram <- periodogram(detrended.ts)
pgram.data = data.frame(freq=pgram$freq, spec=pgram$spec, time_period=1/pgram$freq)
pgram.data.order = pgram.data[order(-pgram.data$spec),]
head(pgram.data.order, 5)
y.decomposed.stl = stl(ts(y, frequency=pgram.data.order$time_period[1]), "periodic")
y.trend.seasonality.remove.stl = y.decomposed.stl$time.series[,3] 
summary(lm(y.trend.seasonality.remove.stl~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.seasonality.remove.stl~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y.trend.seasonality.remove.stl~x+t+s)) #WRONG RELATIONSHIP, WRONG ESTIMATE

y.decomposed.dcmps = decompose(ts(y, frequency=pgram.data.order$time_period[1]), "additive")
y.trend.seasonality.remove.dcmps = y.decomposed.dcmps$random
summary(lm(y.trend.seasonality.remove.dcmps~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.seasonality.remove.dcmps~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y.trend.seasonality.remove.dcmps~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
##########################################################################
# x = 3*t + N(0,2^2)
# s = sin(1/50)
# y = 10 + 0*x + 12*t + 1000*s + N(0,3^2)
t = seq(1, 1000)
s = sin(4*(2*pi/length(t))*t)
x = 3*t + rnorm(length(t), 0, 200)
y = 10 + 0*x + 12*t + 1000*s + rnorm(1000, 0, 300)
acf(y)
summary(lm(y~x)) #WRONG RELATIONSHIP, WRONG ESTIMATE
summary(lm(y~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

y.trend.remove = y - 12*t
summary(lm(y.trend.remove~x)) #WRONG RELATIONSHIP, WRONG ESTIMATE 
summary(lm(y.trend.remove~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y.trend.remove~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

# In order to detect the frequency of a wave function using Fourier transformation, we need to remove the trend from the time series
ts.trend <- lm(y ~ t)
plot(ts.trend, col="red")
detrended.ts <- ts.trend$residuals
plot(detrended.ts, type="l", main="detrended time series")
library(TSA)
pgram <- periodogram(detrended.ts)
pgram.data = data.frame(freq=pgram$freq, spec=pgram$spec, time_period=1/pgram$freq)
pgram.data.order = pgram.data[order(-pgram.data$spec),]
head(pgram.data.order, 5)
y.decomposed.stl = stl(ts(y, frequency=pgram.data.order$time_period[1]), "periodic")
y.trend.seasonality.remove.stl = y.decomposed.stl$time.series[,3] 
summary(lm(y.trend.seasonality.remove.stl~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.seasonality.remove.stl~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y.trend.seasonality.remove.stl~x+t+s)) #WRONG RELATIONSHIP, WRONG ESTIMATE

y.decomposed.dcmps = decompose(ts(y, frequency=pgram.data.order$time_period[1]), "additive")
y.trend.seasonality.remove.dcmps = y.decomposed.dcmps$random
summary(lm(y.trend.seasonality.remove.dcmps~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.seasonality.remove.dcmps~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y.trend.seasonality.remove.dcmps~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
##########################################################################
# x = N(0,2^2)
# s = sin(1/50)
# y = 10 + 2*x + 12*t + 1000*s + N(0,3^2)
t = seq(1, 1000)
s = sin(4*(2*pi/length(t))*t)
x = rnorm(length(t), 0, 200)
y = 10 + 2*x + 12*t + 1000*s + rnorm(1000, 0, 300)
acf(y)
summary(lm(y~x)) #CORRECT RELATIONSHIP, WRONG ESTIMATE
summary(lm(y~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

y.trend.remove = y - 12*t
summary(lm(y.trend.remove~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.remove~x+t)) #WRONG RELATIONSHIP, WRONG ESTIMATE
summary(lm(y.trend.remove~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

# In order to detect the frequency of a wave function using Fourier transformation, we need to remove the trend from the time series
ts.trend <- lm(y ~ t)
plot(ts.trend, col="red")
detrended.ts <- ts.trend$residuals
plot(detrended.ts, type="l", main="detrended time series")
library(TSA)
pgram <- periodogram(detrended.ts)
pgram.data = data.frame(freq=pgram$freq, spec=pgram$spec, time_period=1/pgram$freq)
pgram.data.order = pgram.data[order(-pgram.data$spec),]
head(pgram.data.order, 5)
y.decomposed.stl = stl(ts(y, frequency=pgram.data.order$time_period[1]), "periodic")
y.trend.seasonality.remove.stl = y.decomposed.stl$time.series[,3] 
summary(lm(y.trend.seasonality.remove.stl~x)) #CORRECT RELATIONSHIP, WRONG ESTIMATE 
summary(lm(y.trend.seasonality.remove.stl~x+t)) #CORRECT RELATIONSHIP, WRONG ESTIMATE
summary(lm(y.trend.seasonality.remove.stl~x+t+s)) #WRONG RELATIONSHIP, WRONG ESTIMATE

y.decomposed.dcmps = decompose(ts(y, frequency=pgram.data.order$time_period[1]), "additive")
y.trend.seasonality.remove.dcmps = y.decomposed.dcmps$random
summary(lm(y.trend.seasonality.remove.dcmps~x)) #CORRECT RELATIONSHIP, WRONG ESTIMATE 
summary(lm(y.trend.seasonality.remove.dcmps~x+t)) #CORRECT RELATIONSHIP, WRONG ESTIMATE
summary(lm(y.trend.seasonality.remove.dcmps~x+t+s)) #CORRECT RELATIONSHIP, WRONG ESTIMATE
##########################################################################
# x = 3*t + N(0,2^2)
# s = sin(1/50)
# y = 10 + 2*x + 12*t +1000*s + N(0,3^2)
t = seq(1, 1000)
s = sin(4*(2*pi/length(t))*t)
x = 3*t + rnorm(length(t), 0, 200)
y = 10 + 2*x + 12*t + 1000*s + rnorm(1000, 0, 300)
acf(y)
summary(lm(y~x)) #CORRECT RELATIONSHIP, WRONG ESTIMATE
summary(lm(y~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

y.trend.remove = y - 18*t
summary(lm(y.trend.remove~x)) #WRONG RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.remove~x+t)) #WRONG RELATIONSHIP, WRONG ESTIMATE
summary(lm(y.trend.remove~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

# In order to detect the frequency of a wave function using Fourier transformation, we need to remove the trend from the time series
ts.trend <- lm(y ~ t)
plot(ts.trend, col="red")
detrended.ts <- ts.trend$residuals
plot(detrended.ts, type="l", main="detrended time series")
library(TSA)
pgram <- periodogram(detrended.ts)
pgram.data = data.frame(freq=pgram$freq, spec=pgram$spec, time_period=1/pgram$freq)
pgram.data.order = pgram.data[order(-pgram.data$spec),]
head(pgram.data.order, 5)
y.decomposed.stl = stl(ts(y, frequency=pgram.data.order$time_period[1]), "periodic")
y.trend.seasonality.remove.stl = y.decomposed.stl$time.series[,3] 
summary(lm(y.trend.seasonality.remove.stl~x)) #CORRECT RELATIONSHIP, WRONG ESTIMATE 
summary(lm(y.trend.seasonality.remove.stl~x+t)) #WRONG RELATIONSHIP, WRONG ESTIMATE
summary(lm(y.trend.seasonality.remove.stl~x+t+s)) #WRONG RELATIONSHIP, WRONG ESTIMATE

y.decomposed.dcmps = decompose(ts(y, frequency=pgram.data.order$time_period[1]), "additive")
y.trend.seasonality.remove.dcmps = y.decomposed.dcmps$random
summary(lm(y.trend.seasonality.remove.dcmps~x)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.seasonality.remove.dcmps~x+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y.trend.seasonality.remove.dcmps~x+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE

x.trend.remove = x - 3*t
summary(lm(y.trend.seasonality.remove.dcmps~x.trend.remove)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE 
summary(lm(y.trend.seasonality.remove.dcmps~x.trend.remove+t)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
summary(lm(y.trend.seasonality.remove.dcmps~x.trend.remove+t+s)) #CORRECT RELATIONSHIP, CORRECT ESTIMATE
##########################################################################

