## This example has been taken from https://github.com/joshkyh/amss/blob/master/BayesRegression_Obs.R
## The data are time series

#3) i) Decmpose the time series first using 'decompose' function in R (instead of 'stl' function). Note that this approach drops some data.
#   ii) Regress the independent variables on the residuals (random part of the decomposition), y.res~x+t+s. 

########################################################################
###### Data Pre-processing
########################################################################
# Read CSV
data <- read.csv("C:/Data_Science/Training/MarketingMixedModelingWithR/ObservedData.csv")
dim(data)
head(data)

#data <- data[, c('revenue', 'tv.spend', 'search.spend')]

# Simplify column names
#names(data) <- c('revenue', 'tv', 'search')

######### TEST FOR STATIONARITY ##############################
# 1. Check stationarity for the dependent variable
acf(data$revenue, lag.max=dim(data)[1])
# 2. Augmented Dickey-Fuller test to check stationarity: p-value>0.05 shows non-stationary  
library(tseries)
adf.test(data$revenue)
# 3. Phillips-Perron Unit Root test to check stationarity: p-value>0.05 shows non-stationary
pp.test(data$revenue)
# 4. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test to check stationarity: : p-value<0.05 shows non-stationary
kpss.test(data$revenue)
# Since at least two of the three tests show non-stationarity, then the variable will be transformed. 

######### Webel-Ollech test for seasonality - frequency is how many data points identify a year ##############################
library(seastests)
summary(wo(ts(data$revenue, frequency=52)))
# Seasonality is detected

# Autocorrelation and seasonality is observed. We will decompose the time series - this will treat the seasonality
# Plot the dependent variable - additive model is suggested
plot(x=data$time.index, y=data$revenue, type='l')
# The below methods have been taken from https://anomaly.io/seasonally-adjustement-in-r/index.html
#############################################################################################
############## REMOVE TREND AND SEASONALITY FROM DEPENDENT VARIABLE #########################
############## METHOD 1: MANUAL CALCULATIONS #########################
#############################################################################################
# Detect the trend - smooth the time series using the centered moving average
# In order to detect the frequency of a wave function using Fourier transformation, we need to remove the trend from the time series
t = seq(1,dim(data)[1])
ts.trend <- lm(data$revenue ~ t)
plot(ts.trend, col="red")
detrended.ts <- ts.trend$residuals
plot(detrended.ts, type="l", main="detrended time series")
library(TSA)
pgram <- periodogram(detrended.ts)
pgram = periodogram(data$revenue)
pgram.data = data.frame(freq=pgram$freq, spec=pgram$spec, time_period=1/pgram$freq)
pgram.data.order = pgram.data[order(-pgram.data$spec),]
head(pgram.data.order, 5)
# The Fourier time series is maximized for time period is 54. We use 52 because this indicates
# one year. Therefore, we should use moving average window of 52. 
library(forecast)
revenue.trend.manual = ma(data$revenue, order=52, centre=T)
revenue.trend.manual.remove = data$revenue - revenue.trend.manual

# Average the seasonality
s_revenue = t(matrix(data=revenue.trend.manual.remove, nrow=52))
revenue.seasonal.manual = colMeans(s_revenue, na.rm=T)
revenue.seasonal.manual = rep(revenue.seasonal.manual, dim(data)[1]/length(revenue.seasonal.manual))

# Remove seasonality and trend 
revenue.trend.seasonality.remove.manual = data$revenue - revenue.trend.manual - revenue.seasonal.manual

plot(x=data$time.index, y=data$revenue, type='l')
plot(ts(revenue.seasonal.manual, frequency=52), type='l')
plot(revenue.trend.manual, type='l')
plot(revenue.trend.seasonality.remove.manual, type='l')

# 1. Check stationarity for the dependent variable
acf(revenue.trend.seasonality.remove.manual, lag.max=100, na.action=na.pass)
# 2. Augmented Dickey-Fuller test to check stationarity: p-value>0.05 shows non-stationary  
library(tseries)
adf.test(na.omit(revenue.trend.seasonality.remove.manual))
# 3. Phillips-Perron Unit Root test to check stationarity: p-value>0.05 shows non-stationary
pp.test(na.omit(revenue.trend.seasonality.remove.manual))
# 4. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test to check stationarity: : p-value<0.05 shows non-stationary
kpss.test(revenue.trend.seasonality.remove.manual)

######### TEST FOR SEASONALITY ##############################
library(seastests)
summary(wo(ts(revenue.trend.seasonality.remove.manual, frequency=52)))
#############################################################################################
############## REMOVE TREND AND SEASONALITY FROM DEPENDENT VARIABLE #########################
############## METHOD 2: DECOMPOSE FUNCTION #########################
#############################################################################################
revenue.decomposed.dcmps = decompose(ts(data$revenue, frequency=52), "additive")
revenue.seasonal.dcmps = revenue.decomposed.dcmps$seasonal
revenue.trend.dcmps = revenue.decomposed.dcmps$trend
revenue.trend.seasonality.remove.dcmps = revenue.decomposed.dcmps$random
plot(ts(data$revenue, frequency=52))
plot(revenue.seasonal.dcmps)
plot(revenue.trend.dcmps)
plot(revenue.trend.seasonality.remove.dcmps)
plot(revenue.decomposed.dcmps)

# 1. Check stationarity for the dependent variable
acf(revenue.trend.seasonality.remove.dcmps, lag.max=100, na.action=na.pass)
# 2. Augmented Dickey-Fuller test to check stationarity: p-value>0.05 shows non-stationary  
library(tseries)
adf.test(na.omit(revenue.trend.seasonality.remove.dcmps))
# 3. Phillips-Perron Unit Root test to check stationarity: p-value>0.05 shows non-stationary
pp.test(na.omit(revenue.trend.seasonality.remove.dcmps))
# 4. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test to check stationarity: : p-value<0.05 shows non-stationary
kpss.test(revenue.trend.seasonality.remove.dcmps)

######### TEST FOR SEASONALITY ##############################
library(seastests)
summary(wo(ts(revenue.trend.seasonality.remove.dcmps, frequency=52)))
#############################################################################################
############## REMOVE TREND AND SEASONALITY FROM DEPENDENT VARIABLE #########################
############## METHOD 3: STL FUNCTION #########################
#############################################################################################
revenue.decomposed.stl = stl(ts(data$revenue, frequency=52), "periodic")
revenue.seasonal.stl =  revenue.decomposed.stl$time.series[,1]
revenue.trend.stl = revenue.decomposed.stl$time.series[,2]
revenue.trend.seasonality.remove.stl = revenue.decomposed.stl$time.series[,3] + min(revenue.trend.stl)
revenue.trend.stl = revenue.trend.stl - min(revenue.trend.stl)
plot(ts(data$revenue, frequency=52))
plot(revenue.seasonal.stl)
plot(revenue.trend.stl)
plot(revenue.trend.seasonality.remove.stl)
plot(revenue.decomposed.stl)

# 1. Check stationarity for the dependent variable
acf(revenue.trend.seasonality.remove.stl, lag.max=100, na.action=na.pass)
# 2. Augmented Dickey-Fuller test to check stationarity: p-value>0.05 shows non-stationary  
library(tseries)
adf.test(na.omit(revenue.trend.seasonality.remove.stl))
# 3. Phillips-Perron Unit Root test to check stationarity: p-value>0.05 shows non-stationary
pp.test(na.omit(revenue.trend.seasonality.remove.stl))
# 4. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test to check stationarity: : p-value<0.05 shows non-stationary
kpss.test(revenue.trend.seasonality.remove.stl)

######### TEST FOR SEASONALITY ##############################
library(seastests)
summary(wo(ts(revenue.trend.seasonality.remove.stl, frequency=52)))
#################################################################################

# Select variables and transform variables. The adstock function is taken from https://analyticsartist.wordpress.com/2014/08/17/marketing-mix-modeling-explained-with-r/
adstockTransform1 <- function(x, a_exp=-2, rate=0.25){
  return(as.numeric(stats::filter( 1/(1+exp(a_exp*x)), rate, method = "recursive")))
}
adstockTransform2 <- function(x, rate=0){
  return(as.numeric(stats::filter( x, filter=rate, method = "recursive")))
}

library(dplyr)
library(RCurl)
library(Hmisc)
library(zoo)

data$revenue.trend.seasonality.remove.stl = revenue.trend.seasonality.remove.stl
mmm.data = data %>% 
          dplyr::select(revenue.trend.seasonality.remove.stl,
                 competitor.sales,
                 search.spend,
                 search.query.volume,
                 search.matching.query.volume,
                 search.imps,
                 search.clicks,
                 tv.spend,
                 tv.volume) %>%
          mutate(
            # Create adstock 2 variables
            search.spend.adstock1=adstockTransform1(search.spend, a_exp=-2, rate=0.25), 
            tv.spend.adstock1=adstockTransform1(tv.spend, a_exp=-2, rate=0.25), 
            tv.volume.adstock1=adstockTransform1(tv.volume, a_exp=-2, rate=0.25), 
            
            # Create adstock 2 variables
            search.spend.adstock2=adstockTransform2(search.spend, 1), 
            tv.spend.adstock2=adstockTransform2(tv.spend, 1), 
            tv.volume.adstock2=adstockTransform2(tv.volume, 1), 
            
            # Create log variables
                 log.search.matching.query.volume=log(search.matching.query.volume+2), 
                 log.search.imps=log(search.imps+2), 
                 log.search.clicks=log(search.clicks+2), 
                 
            # Create lag variables
                 search.spend.adstock.lag1y=Lag(search.spend.adstock2, 52),
                 search.spend.adstock.lag3m=Lag(search.spend.adstock2, 13),
                 search.spend.adstock.lag1w=Lag(search.spend.adstock2, 1), 
                 search.spend.adstock.diff1=c(0,diff(search.spend.adstock2, 1)), 

            tv.spend.adstock.lag1y=Lag(tv.spend.adstock2, 52),
            tv.spend.adstock.lag3m=Lag(tv.spend.adstock2, 13),
            tv.spend.adstock.lag1w=Lag(tv.spend.adstock2, 1), 
            tv.spend.adstock.diff1=c(0,diff(tv.spend.adstock2, 1)), 
            
                 tv.volume.adstock.lag1y=Lag(tv.volume.adstock2, 52),
                 tv.volume.adstock.lag3m=Lag(tv.volume.adstock2, 13),
                 tv.volume.adstock.lag1w=Lag(tv.volume.adstock2, 1), 
                 tv.volume.adstock.diff1=c(0,diff(tv.volume.adstock2, 1)), 
            
            competitor.sales.lag1y=Lag(competitor.sales, 52),
            competitor.sales.lag3m=Lag(competitor.sales, 13),
            competitor.sales.lag1w=Lag(competitor.sales, 1), 
            competitor.sales.diff1=c(0,diff(competitor.sales, 1)), 
            
            # Create moving average variables
            search.spend.adstock.p3m=rollmean(search.spend.adstock2, k=13, fill=NA, align = "right"), 
            search.spend.adstock.p1y=rollmean(search.spend.adstock2, k=52, fill=NA, align = "right"), 
            
            tv.spend.adstock.p3m=rollmean(tv.spend.adstock2, k=13, fill=NA, align = "right"), 
            tv.spend.adstock.p1y=rollmean(tv.spend.adstock2, k=52, fill=NA, align = "right"), 
            
            tv.volume.adstock.p3m=rollmean(tv.volume.adstock2, k=13, fill=NA, align = "right"), 
            tv.volume.adstock.p1y=rollmean(tv.volume.adstock2, k=52, fill=NA, align = "right"), 
            
            competitor.sales.p3m=rollmean(competitor.sales, k=13, fill=NA, align = "right"), 
            competitor.sales.p1y=rollmean(competitor.sales, k=52, fill=NA, align = "right")
            
          ) %>%
        dplyr::select( -search.matching.query.volume, -search.imps, -search.clicks)

# Drop first year data
mmm.data <- mmm.data[53:nrow(mmm.data),]
head(mmm.data)

########################################################################
###### Check for collinearity and multicollinearity
########################################################################
library(corrplot)
corrplot(cor(mmm.data), method = "circle", addgrid.col="darkgray", order="hclust", addrect=13) # Display the correlation coefficient

# Remove variables that are highly correlated (abs(corr)>0.7)
mmm.data = mmm.data %>% dplyr::select(
  revenue.trend.seasonality.remove.stl,
  tv.spend.adstock2, 
  competitor.sales.diff1, 
  competitor.sales.lag3m, 
  search.spend, 
  search.spend.adstock1, 
  tv.spend.adstock1, 
  tv.spend, 
  log.search.matching.query.volume, 
  competitor.sales
#-tv.volume.adstock1, 
#-tv.volume.adstock2, 
#-tv.volume.adstock.lag1w, 
#-tv.volume.adstock.lag3m, 
#-tv.volume.adstock.lag1y, 
#-tv.volume.adstock.p3m, 
#-tv.volume.adstock.p1y, 
#-tv.volume.adstock.diff1, 

#-tv.spend.adstock.p3m, 
#-tv.spend.adstock.diff1, 

#-search.spend.adstock.lag1w, 
#-search.spend.adstock.lag1y, 
#-search.spend.adstock.diff1, 
#-search.spend.adstock.p3m, 

#-competitor.sales.lag1y, 
#-competitor.sales.p3m, 
#-competitor.sales.lag1w, 

#-search.query.volume, 

#-log.search.imps, 
#-log.search.clicks, 

#-competitor.sales.p1y, 

#-tv.spend, 
#-tv.volume, 
#-search.spend
) 
corrplot(cor(mmm.data), method = "pie", addgrid.col="darkgray", order="hclust", addrect=9)

# VIF analysis - remove variables with VIF>10
library(car)
lm.fit <- lm(revenue.trend.seasonality.remove.stl ~ ., data = mmm.data)
vif(lm.fit)

mmm.data = mmm.data %>% dplyr::select(
  -log.search.matching.query.volume
  )
lm.fit <- lm(revenue.trend.seasonality.remove.stl ~ ., data = mmm.data)
vif(lm.fit)
# The remaining variables have VIF<10
########################################################################
###### Fit a linear regression model
########################################################################
summary(lm.fit)
# Stepwise regression model
library(MASS)
step.model <- stepAIC(lm.fit, direction = "both", trace = T)
summary(step.model)
########################################################################
# Optimize rate parameters
#multivariate adstock function
AdstockRateMV <- function(Impact, Ads, maxiter = 100, start_rates = 0){
  # parameter names
  params = letters[2:(ncol(Ads)+1)]
  # ad variable names
  ads = paste0("ad_", params)
  # rate variable names
  rates = paste0("rate_", params)
  # create partial formula
  param_fm = paste(
    paste(params, "*adstockTransform2(", ads, ",", rates, ")", sep = ""),
    collapse = " + "
  )
  # create whole formula
  fm = as.formula(paste("Impact ~ a +", param_fm))
  # starting values for nls
  start = c(rep(1, length(params) + 1), rep(start_rates, length(rates)))
  names(start) = c("a", params, rates)
  # input data
  Ads_df = Ads
  names(Ads_df) = ads
  Data = cbind(Impact, Ads_df)
  # fit model
  rate_min=0
  rate_max=1
  library(minpack.lm)
  lower = c(rep(-Inf, length(params) + 1), rep(rate_min, length(rates)))
  upper = c(rep(Inf, length(params) + 1), rep(rate_max, length(rates)))
  modFit <- nlsLM(fm, data = Data, start = start,
                  lower = lower, upper = upper,
                  control = nls.lm.control(maxiter = maxiter))
  #  }
  # model coefficients
  AdstockInt = round(summary(modFit)$coefficients[1, 1])
  AdstockCoef = round(summary(modFit)$coefficients[params, 1], 2)
  AdstockRate = round(summary(modFit)$coefficients[rates, 1], 2)
  # print formula with coefficients
  param_fm_coefs = paste(
    paste(round(AdstockCoef, 2), " * adstockTransform2(", names(Ads), ", ", round(AdstockRate, 2), ")", sep = ""),
    collapse = " + "
  )
  fm_coefs = as.formula(paste("Impact ~ ", AdstockInt, " +", param_fm_coefs))
  # rename rates with original variable names
  names(AdstockRate) = paste0("rate_", names(Ads))
  # calculate percent error
  mape = mean(abs((Impact-predict(modFit))/Impact) * 100)
  # return outputs
  return(list(fm = fm_coefs, base = AdstockInt, rates = AdstockRate, mape = mape))
}

# multivariate adstock model
search.spend=data$search.spend
tv.spend=data$tv.spend
revenue.trend.seasonality.remove.stl=as.numeric(data$revenue.trend.seasonality.remove.stl)
Ads = data.frame(search.spend, tv.spend)
AdstockRateMV(revenue.trend.seasonality.remove.stl, Ads, maxiter = 100, 
              start_rates = 1)

# Grid search to optimize the remaining adstock parameters
df_optimization = data.frame(search.spend.adstock1.a_exp=NA, 
                             search.spend.adstock1.rate=NA, 
                             tv.spend.adstock1.a_exp=NA, 
                             tv.spend.adstock1.rate=NA, 
                             tv.spend.adstock2.rate=NA, 
                             search.spend.adstock2.rate=NA, 
                             AIC=NA, 
                             adj.r.squared=NA, 
                             r.squared=NA)

search.spend.adstock1.a_exp_grid = seq(from=-10, to=0, length.out=5)
search.spend.adstock1.rate_grid = seq(from=0.8, to=1, length.out=5)
tv.spend.adstock1.a_exp_grid = seq(from=-10, to=0, length.out=5)
tv.spend.adstock1.rate_grid = seq(from=0, to=0.2, length.out=5)
tv.spend.adstock2.rate_grid = 1
search.spend.adstock2.rate_grid =  1

for(search.spend.adstock1.a_exp in search.spend.adstock1.a_exp_grid){
  for(search.spend.adstock1.rate in search.spend.adstock1.rate_grid){
    for(tv.spend.adstock1.a_exp in tv.spend.adstock1.a_exp_grid){
      for(tv.spend.adstock1.rate in tv.spend.adstock1.rate_grid){
        for(tv.spend.adstock2.rate in tv.spend.adstock2.rate_grid){
          for(search.spend.adstock2.rate in search.spend.adstock2.rate_grid){
mmm.data = data %>% 
  mutate(
    # Create adstock 2 variables
    search.spend.adstock1=adstockTransform1(search.spend, a_exp=search.spend.adstock1.a_exp, rate=search.spend.adstock1.rate), 
    tv.spend.adstock1=adstockTransform1(tv.spend, a_exp=tv.spend.adstock1.a_exp, rate=tv.spend.adstock1.rate), 
    
    # Create adstock 2 variables
    tv.spend.adstock2=adstockTransform2(tv.spend, tv.spend.adstock2.rate), 
#    search.spend.adstock2=adstockTransform2(search.spend, search.spend.adstock2.rate), 
    
    # Create lag variables
#    search.spend.adstock.lag3m=Lag(search.spend.adstock2, 13),
    
#    tv.spend.adstock.lag3m=Lag(tv.spend.adstock2, 13),
#    tv.spend.adstock.lag1w=Lag(tv.spend.adstock2, 1), 
    
    competitor.sales.lag3m=Lag(competitor.sales, 13),
    competitor.sales.diff1=c(0,diff(competitor.sales, 1)) 
    
    # Create moving average variables
#    search.spend.adstock.p1y=rollmean(search.spend.adstock2, k=52, fill=NA, align = "right") 
  ) %>%
  dplyr::select(revenue.trend.seasonality.remove.stl,
              competitor.sales, 
              search.spend.adstock1, 
              tv.spend.adstock1, 
              tv.spend.adstock2, 
#              search.spend.adstock.lag3m, 
#              tv.spend.adstock.lag3m, 
#              tv.spend.adstock.lag1w, 
              competitor.sales.lag3m, 
              competitor.sales.diff1, 
#              search.spend.adstock.p1y
              search.spend, 
              tv.spend)
# Drop first year data
mmm.data <- mmm.data[53:nrow(mmm.data),]

lm.fit <- lm(revenue.trend.seasonality.remove.stl ~ ., data = mmm.data)
step.model <- stepAIC(lm.fit, direction = "both")

df_optimization[nrow(df_optimization)+1,] = data.frame(search.spend.adstock1.a_exp=search.spend.adstock1.a_exp, 
                                                       search.spend.adstock1.rate=search.spend.adstock1.rate, 
                                                       tv.spend.adstock1.a_exp=tv.spend.adstock1.a_exp, 
                                                       tv.spend.adstock1.rate=tv.spend.adstock1.rate, 
                                                       tv.spend.adstock2.rate=tv.spend.adstock2.rate, 
                                                       search.spend.adstock2.rate=search.spend.adstock2.rate, 
                                                       AIC=AIC(step.model), 
                                                       adj.r.squared=summary(step.model)$adj.r.squared, 
                                                       r.squared=summary(step.model)$r.squared)
}
}
}
}
}
}

df_optimization = df_optimization[2:nrow(df_optimization),]
df_optimization = df_optimization[order(df_optimization$AIC),]
df_optimization

# The best parameters are the following
search.spend.adstock1.a_exp_optimum = -10
search.spend.adstock1.rate_optimum = 1
tv.spend.adstock1.a_exp_optimum = -10
tv.spend.adstock1.rate_optimum = 0.15
tv.spend.adstock2.rate_optimum = 1
search.spend.adstock2.rate_optimum =  1

# Calculate the best model
mmm.data = data %>% 
  mutate(
    # Create adstock 2 variables
    search.spend.adstock1=adstockTransform1(search.spend, a_exp=search.spend.adstock1.a_exp_optimum, rate=search.spend.adstock1.rate_optimum), 
    tv.spend.adstock1=adstockTransform1(tv.spend, a_exp=tv.spend.adstock1.a_exp_optimum, rate=tv.spend.adstock1.rate_optimum), 
    
    # Create adstock 2 variables
    tv.spend.adstock2=adstockTransform2(tv.spend, tv.spend.adstock2.rate_optimum), 
    #    search.spend.adstock2=adstockTransform2(search.spend, search.spend.adstock2.rate), 
    
    # Create lag variables
    #    search.spend.adstock.lag3m=Lag(search.spend.adstock2, 13),
    
    #    tv.spend.adstock.lag3m=Lag(tv.spend.adstock2, 13),
    #    tv.spend.adstock.lag1w=Lag(tv.spend.adstock2, 1), 
    
    competitor.sales.lag3m=Lag(competitor.sales, 13),
    competitor.sales.diff1=c(0,diff(competitor.sales, 1)) 
    
    # Create moving average variables
    #    search.spend.adstock.p1y=rollmean(search.spend.adstock2, k=52, fill=NA, align = "right") 
  ) %>%
  dplyr::select(revenue.trend.seasonality.remove.stl,
                competitor.sales, 
                search.spend.adstock1, 
                tv.spend.adstock1, 
                tv.spend.adstock2, 
                #              search.spend.adstock.lag3m, 
                #              tv.spend.adstock.lag3m, 
                #              tv.spend.adstock.lag1w, 
                competitor.sales.lag3m, 
                competitor.sales.diff1, 
                #              search.spend.adstock.p1y
                search.spend, 
                tv.spend)
# Drop first year data
mmm.data <- mmm.data[53:nrow(mmm.data),]

# Check correlations
corrplot(cor(mmm.data), method = "pie", addgrid.col="darkgray", order="hclust", addrect=9)

# Remove variables that are highly correlated
mmm.data = mmm.data %>% dplyr::select(
  revenue.trend.seasonality.remove.stl,
  search.spend.adstock1, 
  competitor.sales.diff1, 
  competitor.sales.lag3m, 
  search.spend, 
  tv.spend.adstock1, 
  tv.spend, 
  competitor.sales 
)

# VIF analysis - remove variables with VIF>10
library(car)
lm.fit <- lm(revenue.trend.seasonality.remove.stl ~ ., data = mmm.data)
vif(lm.fit)

# Plot the contribution of each component
plot.series = data.frame(revenue.prediction = c(rep(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="(Intercept)")], nrow(mmm.data)), 
                                              summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend.adstock1")]*mmm.data$search.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend")]*mmm.data$search.spend, 
                                              summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend")]*mmm.data$tv.spend, 
                                              summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales")]*mmm.data$competitor.sales  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.lag3m")]*mmm.data$competitor.sales.lag3m  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1
), 
prediction.type = rep(c("4_Base","3_Search_spend","2_TV_spend","1_Competitor_sales"), each=nrow(mmm.data)),
x.axis = rep(c(1:nrow(mmm.data)), 4)
)
ggplot(plot.series, aes(x.axis, revenue.prediction)) + geom_area(aes(fill = prediction.type))

# Plot the contribution of the significant components
step.model <- stepAIC(lm.fit, direction = "both")
plot.series = data.frame(revenue.prediction = c(rep(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="(Intercept)")], nrow(mmm.data)), 
                                                summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend")]*mmm.data$tv.spend, 
                                                summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.lag3m")]*mmm.data$competitor.sales.lag3m  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1
), 
prediction.type = rep(c("3_Base","2_TV_spend","1_Competitor_sales"), each=nrow(mmm.data)),
x.axis = rep(c(1:nrow(mmm.data)), 3)
)
ggplot(plot.series, aes(x.axis, revenue.prediction)) + geom_area(aes(fill = prediction.type))



# Model evalution
# Visualize residuals
residualPlot(lm.fit)
# Residual autocorrelation of the model with multicollinearity
# 1. Check stationarity for the dependent variable
acf(residuals(lm.fit), lag.max=100, na.action=na.pass)
# 2. Augmented Dickey-Fuller test to check stationarity: p-value>0.05 shows non-stationary  
library(tseries)
adf.test(na.omit(residuals(lm.fit)))
# 3. Phillips-Perron Unit Root test to check stationarity: p-value>0.05 shows non-stationary
pp.test(na.omit(residuals(lm.fit)))
# 4. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test to check stationarity: : p-value<0.05 shows non-stationary
kpss.test(residuals(lm.fit))
# 5. Durbin-Watson test for autocorrelation: p-value<0.05 shows autocorrelated error terms
dwtest(lm.fit)
durbinWatsonTest(lm.fit)
######### TEST FOR SEASONALITY ##############################
library(seastests)
summary(wo(ts(residuals(lm.fit), frequency=52)))
# Test for heteroskedasticity
# Breush Pagan Test: p-value>0.05 shows heteroscedasticity
lmtest::bptest(lm.fit)
# NCV Test: p-value>0.05 shows heteroscedasticity
car::ncvTest(lm.fit)
# The residuals are not autocorrelated, do not have seasonality, and do not have heteroskedasticity

# Visualize residuals
residualPlot(step.model)
# Residual autocorrelation of the best model
# 1. Check stationarity for the dependent variable
acf(residuals(step.model), lag.max=100, na.action=na.pass)
# 2. Augmented Dickey-Fuller test to check stationarity: p-value>0.05 shows non-stationary  
library(tseries)
adf.test(na.omit(residuals(step.model)))
# 3. Phillips-Perron Unit Root test to check stationarity: p-value>0.05 shows non-stationary
pp.test(na.omit(residuals(step.model)))
# 4. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test to check stationarity: : p-value<0.05 shows non-stationary
kpss.test(residuals(step.model))
# 5. Durbin-Watson test for autocorrelation: p-value<0.05 shows autocorrelated error terms
dwtest(step.model)
durbinWatsonTest(step.model)
######### TEST FOR SEASONALITY ##############################
library(seastests)
summary(wo(ts(residuals(step.model), frequency=52)))
# Test for heteroskedasticity
# Breush Pagan Test: p-value>0.05 shows heteroscedasticity
lmtest::bptest(step.model)
# NCV Test: p-value>0.05 shows heteroscedasticity
car::ncvTest(step.model)
# The residuals are not autocorrelated, do not have seasonality, and do not have heteroskedasticity

