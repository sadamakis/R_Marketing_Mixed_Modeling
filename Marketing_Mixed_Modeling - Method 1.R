## This example has been taken from https://github.com/joshkyh/amss/blob/master/BayesRegression_Obs.R
## The data are time series

#1) The best approach would be to include time and seasonality variables in the regression equation, e.g.:
#    y~x+t+s
#  There are a couple of ways to compute seasonality: 
#     ii) Have a binary variable for each month, e.g. Feb, Mar, ..., Dec


########################################################################
###### Data Pre-processing
########################################################################
# Read CSV
data <- read.csv("C:/Data_Science/Training/MarketingMixedModelingWithR/ObservedData.csv")
dim(data)
head(data)
# Add month variable
a=0; data$january = ifelse(data$time.index %in% c(seq(4*a+1,length.out=4), seq(4*a+1,length.out=4)+52, seq(4*a+1,length.out=4)+52*2, seq(4*a+1,length.out=4)+52*3, seq(4*a+1,length.out=4)+52*4), 1,0)
a=1; data$february = ifelse(data$time.index %in% c(seq(4*a+1,length.out=4), seq(4*a+1,length.out=4)+52, seq(4*a+1,length.out=4)+52*2, seq(4*a+1,length.out=4)+52*3, seq(4*a+1,length.out=4)+52*4), 1,0)
a=2; data$march = ifelse(data$time.index %in% c(seq(4*a+1,length.out=5), seq(4*a+1,length.out=5)+52, seq(4*a+1,length.out=5)+52*2, seq(4*a+1,length.out=5)+52*3, seq(4*a+1,length.out=5)+52*4), 1,0)
a=3; data$april = ifelse(data$time.index %in% c(seq(4*a+2,length.out=4), seq(4*a+2,length.out=4)+52, seq(4*a+2,length.out=4)+52*2, seq(4*a+2,length.out=4)+52*3, seq(4*a+2,length.out=4)+52*4), 1,0)
a=4; data$may = ifelse(data$time.index %in% c(seq(4*a+2,length.out=4), seq(4*a+2,length.out=4)+52, seq(4*a+2,length.out=4)+52*2, seq(4*a+2,length.out=4)+52*3, seq(4*a+2,length.out=4)+52*4), 1,0)
a=5; data$june = ifelse(data$time.index %in% c(seq(4*a+2,length.out=5), seq(4*a+2,length.out=5)+52, seq(4*a+2,length.out=5)+52*2, seq(4*a+2,length.out=5)+52*3, seq(4*a+2,length.out=5)+52*4), 1,0)
a=6; data$july = ifelse(data$time.index %in% c(seq(4*a+3,length.out=4), seq(4*a+3,length.out=4)+52, seq(4*a+3,length.out=4)+52*2, seq(4*a+3,length.out=4)+52*3, seq(4*a+3,length.out=4)+52*4), 1,0)
a=7; data$august = ifelse(data$time.index %in% c(seq(4*a+3,length.out=4), seq(4*a+3,length.out=4)+52, seq(4*a+3,length.out=4)+52*2, seq(4*a+3,length.out=4)+52*3, seq(4*a+3,length.out=4)+52*4), 1,0)
a=8; data$september = ifelse(data$time.index %in% c(seq(4*a+3,length.out=5), seq(4*a+3,length.out=5)+52, seq(4*a+3,length.out=5)+52*2, seq(4*a+3,length.out=5)+52*3, seq(4*a+3,length.out=5)+52*4), 1,0)
a=9; data$october = ifelse(data$time.index %in% c(seq(4*a+4,length.out=4), seq(4*a+4,length.out=4)+52, seq(4*a+4,length.out=4)+52*2, seq(4*a+4,length.out=4)+52*3, seq(4*a+4,length.out=4)+52*4), 1,0)
a=10; data$november = ifelse(data$time.index %in% c(seq(4*a+4,length.out=4), seq(4*a+4,length.out=4)+52, seq(4*a+4,length.out=4)+52*2, seq(4*a+4,length.out=4)+52*3, seq(4*a+4,length.out=4)+52*4), 1,0)
a=11; data$december = ifelse(data$time.index %in% c(seq(4*a+4,length.out=5), seq(4*a+4,length.out=5)+52, seq(4*a+4,length.out=5)+52*2, seq(4*a+4,length.out=5)+52*3, seq(4*a+4,length.out=5)+52*4), 1,0)

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

mmm.data = data %>% 
  dplyr::select(revenue,
                competitor.sales,
                search.spend,
                search.query.volume,
                search.matching.query.volume,
                search.imps,
                search.clicks,
                tv.spend,
                tv.volume, 
                time.index, 
                february, 
                march, 
                april, 
                may, 
                june, 
                july, 
                august, 
                september, 
                october, 
                november, 
                december) %>%
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
corrplot(cor(mmm.data), method = "circle", addgrid.col="darkgray", order="hclust", addrect=26) # Display the correlation coefficient

# Remove variables that are highly correlated (abs(corr)>0.7)
mmm.data = mmm.data %>% dplyr::select(
  revenue,
  tv.spend.adstock2, 
  competitor.sales.diff1, 
  competitor.sales.lag3m, 
  search.spend, 
  search.spend.adstock1, 
  tv.spend.adstock1, 
  tv.spend, 
  log.search.matching.query.volume, 
  competitor.sales, 
  time.index, 
  february, 
  march, 
  april, 
  may, 
  june, 
  july, 
  august, 
  september, 
  october, 
  november, 
  december
) 
corrplot(cor(mmm.data), method = "pie", addgrid.col="darkgray", order="hclust", addrect=20)

# VIF analysis - remove variables with VIF>10
library(car)
lm.fit <- lm(revenue ~ ., data = mmm.data)
vif(lm.fit)

mmm.data = mmm.data %>% dplyr::select(
  -log.search.matching.query.volume, 
  -tv.spend.adstock2, 
# Remove competitor sales because it self predicts the target variable
  -competitor.sales
)
lm.fit <- lm(revenue ~ ., data = mmm.data)
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
revenue=as.numeric(data$revenue)
Ads = data.frame(search.spend, tv.spend)
AdstockRateMV(revenue, Ads, maxiter = 100, 
              start_rates = 0.5)

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
tv.spend.adstock2.rate_grid = 0.84
search.spend.adstock2.rate_grid =  0.44

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
#                tv.spend.adstock2=adstockTransform2(tv.spend, tv.spend.adstock2.rate), 
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
              dplyr::select(revenue,
#                            competitor.sales, 
                            search.spend.adstock1, 
                            tv.spend.adstock1, 
#                            tv.spend.adstock2, 
                            #              search.spend.adstock.lag3m, 
                            #              tv.spend.adstock.lag3m, 
                            #              tv.spend.adstock.lag1w, 
                            competitor.sales.lag3m, 
                            competitor.sales.diff1, 
                            #              search.spend.adstock.p1y
                            search.spend, 
                            tv.spend, 
                            time.index, 
                            february, 
                            march, 
                            april, 
                            may, 
                            june, 
                            july, 
                            august, 
                            september, 
                            october, 
                            november, 
                            december
              )
            # Drop first year data
            mmm.data <- mmm.data[53:nrow(mmm.data),]
            
            lm.fit <- lm(revenue ~ ., data = mmm.data)
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
search.spend.adstock1.a_exp_optimum = 0
search.spend.adstock1.rate_optimum = 0.95
tv.spend.adstock1.a_exp_optimum = -10
tv.spend.adstock1.rate_optimum = 0.20
tv.spend.adstock2.rate_optimum = 0.84
search.spend.adstock2.rate_optimum =  0.44

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
  dplyr::select(revenue,
#                competitor.sales, 
                search.spend.adstock1, 
                tv.spend.adstock1, 
#                tv.spend.adstock2, 
                #              search.spend.adstock.lag3m, 
                #              tv.spend.adstock.lag3m, 
                #              tv.spend.adstock.lag1w, 
                competitor.sales.lag3m, 
                competitor.sales.diff1, 
                #              search.spend.adstock.p1y
                search.spend, 
                tv.spend, 
                time.index, 
                february,   
                march,      
                april,      
                may,        
                june,      
                july,      
                august,    
                september, 
                october,   
                november,  
                december 
  )
# Drop first year data
mmm.data <- mmm.data[53:nrow(mmm.data),]

# Check correlations
corrplot(cor(mmm.data), method = "pie", addgrid.col="darkgray", order="hclust", addrect=9)

# Remove variables that are highly correlated
mmm.data = mmm.data %>% dplyr::select(
  revenue,
  search.spend.adstock1, 
  competitor.sales.diff1, 
  competitor.sales.lag3m, 
  search.spend, 
  tv.spend.adstock1, 
  tv.spend, 
#  competitor.sales, 
  time.index, 
  february,   
  march,      
  april,      
  may,        
  june,      
  july,      
  august,    
  september, 
  october,   
  november,  
  december 
)

# VIF analysis - remove variables with VIF>10
library(car)
lm.fit <- lm(revenue ~ .+tv.spend*(february+march+april+may+june+july+august+september+october+november+december)
             +search.spend*(february+march+april+may+june+july+august+september+october+november+december)+
               tv.spend.adstock1*(february+march+april+may+june+july+august+september+october+november+december)+
               search.spend.adstock1*(february+march+april+may+june+july+august+september+october+november+december), data = mmm.data)
vif(lm.fit)
summary(lm.fit)

# Plot the contribution of each component
plot.series = data.frame(revenue.prediction = c(
  rep(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="(Intercept)")], nrow(mmm.data)), 
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend.adstock1")]*mmm.data$search.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend")]*mmm.data$search.spend,
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend")]*mmm.data$tv.spend,
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.lag3m")]*mmm.data$competitor.sales.lag3m  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1,
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="february")]*mmm.data$february  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="march")]*mmm.data$march  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="april")]*mmm.data$april  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="may")]*mmm.data$may  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="june")]*mmm.data$june  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="july")]*mmm.data$july  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="august")]*mmm.data$august + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="september")]*mmm.data$september + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="october")]*mmm.data$october + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="november")]*mmm.data$november + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="december")]*mmm.data$december
), 
prediction.type = rep(c("5_Base","4_Search_spend","3_TV_spend","2_Competitor_sales", "1_Seasonality"), each=nrow(mmm.data)),
x.axis = rep(c(1:nrow(mmm.data)), 5)
)
ggplot(plot.series, aes(x.axis, revenue.prediction)) + geom_area(aes(fill = prediction.type))

# Plot the positive increments only
plot.series.positive = data.frame(revenue.prediction = c(
rep(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="(Intercept)")], nrow(mmm.data)), 
ifelse(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend.adstock1")]*mmm.data$search.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend")]*mmm.data$search.spend<=0, 0, summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend.adstock1")]*mmm.data$search.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend")]*mmm.data$search.spend),
ifelse(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend")]*mmm.data$tv.spend<=0, 0, summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend")]*mmm.data$tv.spend), 
ifelse(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.lag3m")]*mmm.data$competitor.sales.lag3m  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1<=0, 0, summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.lag3m")]*mmm.data$competitor.sales.lag3m  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1), 
ifelse(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="february")]*mmm.data$february  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="march")]*mmm.data$march  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="april")]*mmm.data$april  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="may")]*mmm.data$may  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="june")]*mmm.data$june  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="july")]*mmm.data$july  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="august")]*mmm.data$august + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="september")]*mmm.data$september + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="october")]*mmm.data$october + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="november")]*mmm.data$november + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="december")]*mmm.data$december<=0, 0, summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="february")]*mmm.data$february  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="march")]*mmm.data$march  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="april")]*mmm.data$april  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="may")]*mmm.data$may  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="june")]*mmm.data$june  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="july")]*mmm.data$july  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="august")]*mmm.data$august + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="september")]*mmm.data$september + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="october")]*mmm.data$october + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="november")]*mmm.data$november + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="december")]*mmm.data$december) 
), 
prediction.type = rep(c("5_Base","4_Search_spend","3_TV_spend","2_Competitor_sales", "1_Seasonality"), each=nrow(mmm.data)),
x.axis = rep(c(1:nrow(mmm.data)), 5)
)
ggplot(plot.series.positive, aes(x.axis, revenue.prediction)) + geom_area(aes(fill = prediction.type))

# Calculate the average contribution
plot.series %>% 
  group_by(prediction.type) %>% 
  summarise_at(vars(revenue.prediction), funs(mean, median, sum))
average_contribution = aggregate(plot.series$revenue.prediction, list(plot.series$prediction.type), FUN=c("mean"))
average_contribution = average_contribution[order(average_contribution$x, decreasing=T),]
average_contribution

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
library(lmtest)
dwtest(lm.fit)
durbinWatsonTest(lm.fit)
######### TEST FOR SEASONALITY ##############################
library(seastests)
summary(wo(ts(residuals(lm.fit), frequency=52)))
# Test for heteroscedasticity
# Breush Pagan Test: p-value>0.05 shows heteroscedasticity
lmtest::bptest(lm.fit)
# NCV Test: p-value>0.05 shows heteroscedasticity
car::ncvTest(lm.fit)
# The residuals are autocorrelated, stationary, do not have seasonality, and do not have heteroskedasticity


# Plot the contribution of the significant components
step.model <- stepAIC(lm.fit, direction = "both")
summary(step.model)
plot.series = data.frame(revenue.prediction = c(
  rep(summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="(Intercept)")], nrow(mmm.data)), 
  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="search.spend.adstock1")]*mmm.data$search.spend.adstock1,
  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1,
  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1,
  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="february")]*mmm.data$february  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="march")]*mmm.data$march  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="april")]*mmm.data$april  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="may")]*mmm.data$may  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="june")]*mmm.data$june  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="july")]*mmm.data$july  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="august")]*mmm.data$august + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="september")]*mmm.data$september + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="october")]*mmm.data$october + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="november")]*mmm.data$november + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="december")]*mmm.data$december
), 
prediction.type = rep(c("5_Base","4_Search_spend","3_TV_spend","2_Competitor_sales", "1_Seasonality"), each=nrow(mmm.data)),
x.axis = rep(c(1:nrow(mmm.data)), 5)
)
ggplot(plot.series, aes(x.axis, revenue.prediction)) + geom_area(aes(fill = prediction.type))

# Plot the positive increments only
plot.series.positive = data.frame(revenue.prediction = c(
rep(summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="(Intercept)")], nrow(mmm.data)), 
ifelse(summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="search.spend.adstock1")]*mmm.data$search.spend.adstock1<=0, 0, summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="search.spend.adstock1")]*mmm.data$search.spend.adstock1),
ifelse(summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1<=0, 0, summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1), 
ifelse(summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1<=0, 0, summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1),
ifelse(summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="february")]*mmm.data$february  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="march")]*mmm.data$march  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="april")]*mmm.data$april  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="may")]*mmm.data$may  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="june")]*mmm.data$june  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="july")]*mmm.data$july  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="august")]*mmm.data$august + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="september")]*mmm.data$september + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="october")]*mmm.data$october + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="november")]*mmm.data$november + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="december")]*mmm.data$december<=0, 0, summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="february")]*mmm.data$february  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="march")]*mmm.data$march  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="april")]*mmm.data$april  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="may")]*mmm.data$may  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="june")]*mmm.data$june  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="july")]*mmm.data$july  +  summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="august")]*mmm.data$august + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="september")]*mmm.data$september + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="october")]*mmm.data$october + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="november")]*mmm.data$november + summary(step.model)$coef[which(rownames(summary(step.model)$coef)=="december")]*mmm.data$december) 
), 
prediction.type = rep(c("5_Base","4_Search_spend","3_TV_spend","2_Competitor_sales", "1_Seasonality"), each=nrow(mmm.data)),
x.axis = rep(c(1:nrow(mmm.data)), 5)
)
ggplot(plot.series.positive, aes(x.axis, revenue.prediction)) + geom_area(aes(fill = prediction.type))

# Calculate the average contribution
plot.series %>% 
  group_by(prediction.type) %>% 
  summarise_at(vars(revenue.prediction), funs(mean, median, sum))
average_contribution = aggregate(plot.series$revenue.prediction, list(plot.series$prediction.type), FUN=c("mean"))
average_contribution = average_contribution[order(average_contribution$x, decreasing=T),]
average_contribution

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
durbinWatsonTest(step.model, max.lag=20)
######### TEST FOR SEASONALITY ##############################
library(seastests)
summary(wo(ts(residuals(step.model), frequency=52)))
# Test for heteroskedasticity
# Breush Pagan Test: p-value>0.05 shows heteroscedasticity
lmtest::bptest(step.model)
# NCV Test: p-value>0.05 shows heteroscedasticity
car::ncvTest(step.model)
# The residuals are autocorrelated, stationary, do not have seasonality, and do not have heteroskedasticity
# Since the residuals are autocorrelated, we would like to add lagged dependent variables in the modeling equation. 
# In order to understand the order of the lagged dependent variable we can check the best ARIMA model for the residuals:
library(forecast)
auto.arima(residuals(step.model))
# The ARIMA(3,0,2) model suggests that there might be up to order 3 lag of the dependent variable. We start with lag 1 (Y_t-1), check results, if needed lag 2, check results, if needed lag 3, check results, etc.  

#############################################################################
# In oder to fix the autocorrelation, we insert the dependent variale lag into our model
#############################################################################
# Calculate the best model
mmm.data.autocorrelation = data %>% 
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
    competitor.sales.diff1=c(0,diff(competitor.sales, 1)), 

    # Create lag variables for the dependent variable
    revenue.lag1w=Lag(revenue, 1),
    revenue.lag2w=Lag(revenue, 2),
    revenue.lag3w=Lag(revenue, 3) 

    # Create moving average variables
    #    search.spend.adstock.p1y=rollmean(search.spend.adstock2, k=52, fill=NA, align = "right") 
  ) %>%
  dplyr::select(revenue,
                #                competitor.sales, 
                search.spend.adstock1, 
                tv.spend.adstock1, 
                #                tv.spend.adstock2, 
                #              search.spend.adstock.lag3m, 
                #              tv.spend.adstock.lag3m, 
                #              tv.spend.adstock.lag1w, 
                competitor.sales.lag3m, 
                competitor.sales.diff1, 
                #              search.spend.adstock.p1y
                search.spend, 
                tv.spend, 
                revenue.lag1w,
                revenue.lag2w, 
                revenue.lag3w,
                time.index, 
                february,   
                march,      
                april,      
                may,        
                june,      
                july,      
                august,    
                september, 
                october,   
                november,  
                december 
  )
# Drop first year data
mmm.data.autocorrelation <- mmm.data.autocorrelation[53:nrow(mmm.data.autocorrelation),]

# Check correlations
corrplot(cor(mmm.data.autocorrelation), method = "pie", addgrid.col="darkgray", order="hclust", addrect=9)

# Remove variables that are highly correlated
mmm.data.autocorrelation = mmm.data.autocorrelation %>% dplyr::select(
  revenue,
  search.spend.adstock1, 
  competitor.sales.diff1, 
#  competitor.sales.lag3m, 
  search.spend, 
  tv.spend.adstock1, 
  tv.spend, 
  #  competitor.sales, 
  revenue.lag1w,
  revenue.lag2w,
 revenue.lag3w,
  time.index, 
  february,   
  march,      
  april,      
  may,        
  june,      
  july,      
  august,    
  september, 
  october,   
  november,  
  december 
)

# VIF analysis - remove variables with VIF>10
library(car)
lm.fit.autocorrelation <- lm(revenue ~ ., data = mmm.data.autocorrelation)
vif(lm.fit.autocorrelation)

# Calculate the best model using BIC
lm.fit.autocorrelation <- lm(revenue ~ .+tv.spend*(february+march+april+may+june+july+august+september+october+november+december)
                             +search.spend*(february+march+april+may+june+july+august+september+october+november+december)
                              + tv.spend.adstock1*(february+march+april+may+june+july+august+september+october+november+december)
                              + search.spend.adstock1*(february+march+april+may+june+july+august+september+october+november+december), data = mmm.data.autocorrelation)
summary(lm.fit.autocorrelation)

# Plot the contribution of the significant components - select the best model using BIC as this is an explanatory model
step.model.autocorrelation <- step(lm.fit.autocorrelation, direction="both", k=log(nrow(mmm.data.autocorrelation)))
summary(step.model.autocorrelation)
plot.series.autocorrelation = data.frame(revenue.prediction = c(
  rep(summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="(Intercept)")], nrow(mmm.data.autocorrelation)), 
  (summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="search.spend")] + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="search.spend:december")]*mmm.data.autocorrelation$december )*mmm.data.autocorrelation$search.spend,
  (summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="tv.spend.adstock1")] + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="tv.spend.adstock1:december")]*mmm.data.autocorrelation$december )*mmm.data.autocorrelation$tv.spend.adstock1  +  (summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="tv.spend")] )*mmm.data.autocorrelation$tv.spend,
  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="competitor.sales.diff1")]*mmm.data.autocorrelation$competitor.sales.diff1,
  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="revenue.lag1w")]*mmm.data.autocorrelation$revenue.lag1w,
  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="march")]*mmm.data.autocorrelation$march  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="april")]*mmm.data.autocorrelation$april  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="may")]*mmm.data.autocorrelation$may  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="june")]*mmm.data.autocorrelation$june  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="july")]*mmm.data.autocorrelation$july  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="august")]*mmm.data.autocorrelation$august + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="september")]*mmm.data.autocorrelation$september + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="october")]*mmm.data.autocorrelation$october + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="november")]*mmm.data.autocorrelation$november + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="december")]*mmm.data.autocorrelation$december
), 
prediction.type = rep(c("6_Base","5_Search_spend","4_TV_spend","3_Competitor_sales", "2_Revenue_lag1w", "1_Seasonality"), each=nrow(mmm.data.autocorrelation)),
x.axis = rep(c(1:nrow(mmm.data.autocorrelation)), 6)
)
ggplot(plot.series.autocorrelation, aes(x.axis, revenue.prediction)) + geom_area(aes(fill = prediction.type))

# Plot the interactions separately
plot.series.autocorrelation.interactions = data.frame(revenue.prediction = c(
  rep(summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="(Intercept)")], nrow(mmm.data.autocorrelation)), 
  (summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="search.spend")]  )*mmm.data.autocorrelation$search.spend,
  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="search.spend:december")]*mmm.data.autocorrelation$december*mmm.data.autocorrelation$search.spend,
  (summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="tv.spend.adstock1")]  )*mmm.data.autocorrelation$tv.spend.adstock1  +  (summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="tv.spend")] )*mmm.data.autocorrelation$tv.spend,
  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="tv.spend.adstock1:december")]*mmm.data.autocorrelation$december*mmm.data.autocorrelation$tv.spend.adstock1,
  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="competitor.sales.diff1")]*mmm.data.autocorrelation$competitor.sales.diff1,
  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="revenue.lag1w")]*mmm.data.autocorrelation$revenue.lag1w,
  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="march")]*mmm.data.autocorrelation$march  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="april")]*mmm.data.autocorrelation$april  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="may")]*mmm.data.autocorrelation$may  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="june")]*mmm.data.autocorrelation$june  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="july")]*mmm.data.autocorrelation$july  +  summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="august")]*mmm.data.autocorrelation$august + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="september")]*mmm.data.autocorrelation$september + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="october")]*mmm.data.autocorrelation$october + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="november")]*mmm.data.autocorrelation$november + summary(step.model.autocorrelation)$coef[which(rownames(summary(step.model.autocorrelation)$coef)=="december")]*mmm.data.autocorrelation$december
), 
prediction.type = rep(c("8_Base","7_Search_spend","6_Search_spend_december","5_TV_spend","4_TV_spend_december","3_Competitor_sales", "2_Revenue_lag1w", "1_Seasonality"), each=nrow(mmm.data.autocorrelation)),
x.axis = rep(c(1:nrow(mmm.data.autocorrelation)), 8)
)
ggplot(plot.series.autocorrelation, aes(x.axis, revenue.prediction)) + geom_area(aes(fill = prediction.type))

# Visualize residuals
residualPlot(step.model.autocorrelation)
# Residual autocorrelation of the best model
# 1. Check stationarity for the dependent variable
acf(residuals(step.model.autocorrelation), lag.max=100, na.action=na.pass)
pacf(residuals(step.model.autocorrelation), lag.max=100, na.action=na.pass)
# 2. Augmented Dickey-Fuller test to check stationarity: p-value>0.05 shows non-stationary 
library(tseries)
adf.test(na.omit(residuals(step.model.autocorrelation)))
# 3. Phillips-Perron Unit Root test to check stationarity: p-value>0.05 shows non-stationary
pp.test(na.omit(residuals(step.model.autocorrelation)))
# 4. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test to check stationarity: : p-value<0.05 shows non-stationary
kpss.test(residuals(step.model.autocorrelation))
# 5. Durbin-Watson test for autocorrelation: p-value<0.05 shows autocorrelated error terms
dwtest(step.model.autocorrelation)
durbinWatsonTest(step.model.autocorrelation, max.lag=10)
######### TEST FOR SEASONALITY ##############################
library(seastests)
summary(wo(ts(residuals(step.model.autocorrelation), frequency=52)))
# Test for heteroskedasticity
# Breush Pagan Test: p-value>0.05 shows heteroscedasticity
lmtest::bptest(step.model.autocorrelation)
# NCV Test: p-value>0.05 shows heteroscedasticity
car::ncvTest(step.model.autocorrelation)
# The residuals are not autocorrelated, do not have seasonality, and do not have heteroskedasticity
auto.arima(residuals(step.model.autocorrelation))


# Create a Year variable
plot.series.autocorrelation$year = ifelse(plot.series.autocorrelation$x.axis %in% 1:52, 'Year 1',
                                          ifelse(plot.series.autocorrelation$x.axis %in% 53:104, 'Year 2', 
                                                 ifelse(plot.series.autocorrelation$x.axis %in% 105:156, 'Year 3', 'NA')
                                          )
)
plot.series.autocorrelation.interactions$year = ifelse(plot.series.autocorrelation.interactions$x.axis %in% 1:52, 'Year 1',
                                                       ifelse(plot.series.autocorrelation.interactions$x.axis %in% 53:104, 'Year 2', 
                                                              ifelse(plot.series.autocorrelation.interactions$x.axis %in% 105:156, 'Year 3', 'NA')
                                                       )
)


# Calculate the average contribution per week, and total contribution over 3 years
aggregate(plot.series.autocorrelation$revenue.prediction, list(plot.series.autocorrelation$prediction.type), FUN=c("mean"))
average_contribution.autocorrelation = plot.series.autocorrelation %>% 
  group_by(prediction.type) %>% 
  summarise_at(vars(revenue.prediction), funs(mean, sum))
names(average_contribution.autocorrelation) = c('prediction.type', 'average.contribution', 'total.contribution')
average_contribution.autocorrelation = average_contribution.autocorrelation[order(average_contribution.autocorrelation$average.contribution, decreasing=T),]
average_contribution.autocorrelation

# Calculate the interactions separately
aggregate(plot.series.autocorrelation.interactions$revenue.prediction, list(plot.series.autocorrelation.interactions$prediction.type), FUN=c("mean"))
average_contribution.autocorrelation.interactions = plot.series.autocorrelation.interactions %>% 
  group_by(prediction.type) %>% 
  summarise_at(vars(revenue.prediction), funs(mean, sum))
names(average_contribution.autocorrelation.interactions) = c('prediction.type', 'average.contribution', 'total.contribution')
average_contribution.autocorrelation.interactions = average_contribution.autocorrelation.interactions[order(average_contribution.autocorrelation.interactions$average.contribution, decreasing=T),]
average_contribution.autocorrelation.interactions
# These results indicate that the December campaigns for search do not add value to the revenue. On the contrary they cost the company. In December, it is preferable to invest on TV advertisement.  

# Plot contribution per type
bplot = barplot(average_contribution.autocorrelation$total.contribution,  xlab='Prediction type', ylab='Total contribution', col='blue', border='red', cex.names=0.8)
text(average_contribution.autocorrelation$prediction.type,
     x = bplot,
     offset =-0.1,
     y = -200,
     cex = 0.8,
     srt = 60,
     xpd = TRUE,
     pos = 2 )


# Calculate ROI over 3 years
tv.ROI = filter(average_contribution.autocorrelation,prediction.type=='4_TV_spend')$total.contribution / sum(mmm.data.autocorrelation$tv.spend)
cat("TV ROI is", tv.ROI)
search.ROI = filter(average_contribution.autocorrelation,prediction.type=='5_Search_spend')$total.contribution / sum(mmm.data.autocorrelation$search.spend)
cat("Search ROI is", search.ROI)
# These results show that the search marketing is not profitable

# Calculate ROI over 3 years - include interactions
tv.ROI.interactions = filter(average_contribution.autocorrelation.interactions,prediction.type=='5_TV_spend' | prediction.type=='4_TV_spend_december')$total.contribution / sum(mmm.data.autocorrelation$tv.spend)
cat("TV ROI is", tv.ROI.interactions)
search.ROI.interactions = filter(average_contribution.autocorrelation.interactions,prediction.type=='7_Search_spend' | prediction.type=='6_Search_spend_december')$total.contribution / sum(mmm.data.autocorrelation$search.spend)
cat("Search ROI is", search.ROI.interactions)
# These results indicate that if December search marketing will be removed, then the search marketing will be profitable

# Year on year analysis
average_contribution.autocorrelation.year = plot.series.autocorrelation %>% 
  group_by(prediction.type, year) %>% 
  summarise_at(vars(revenue.prediction), funs(mean, sum))
names(average_contribution.autocorrelation.year) = c('prediction.type', 'year', 'average.contribution', 'total.contribution')
average_contribution.autocorrelation.year = average_contribution.autocorrelation.year[order(average_contribution.autocorrelation.year$year, -average_contribution.autocorrelation.year$average.contribution),]
average_contribution.autocorrelation.year
# Plot contribution per type per year
ggplot(data=average_contribution.autocorrelation.year, aes(x=prediction.type, y=total.contribution, fill=year)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Calculate ROI for year 1
tv.ROI.year1 = filter(average_contribution.autocorrelation.year,prediction.type=='4_TV_spend',year=='Year 1')$total.contribution / sum(mmm.data.autocorrelation$tv.spend[1:52])
cat("TV ROI on year 1 is", tv.ROI.year1)
search.ROI.year1 = filter(average_contribution.autocorrelation.year,prediction.type=='5_Search_spend',year=='Year 1')$total.contribution / sum(mmm.data.autocorrelation$search.spend[1:52])
cat("Search ROI on year 1 is", search.ROI.year1)

# Calculate ROI for year 2
tv.ROI.year2 = filter(average_contribution.autocorrelation.year,prediction.type=='4_TV_spend',year=='Year 2')$total.contribution / sum(mmm.data.autocorrelation$tv.spend[53:104])
cat("TV ROI on year 2 is", tv.ROI.year2)
search.ROI.year2 = filter(average_contribution.autocorrelation.year,prediction.type=='5_Search_spend',year=='Year 2')$total.contribution / sum(mmm.data.autocorrelation$search.spend[53:104])
cat("Search ROI on year 2 is", search.ROI.year2)

# Calculate ROI for year 3
tv.ROI.year3 = filter(average_contribution.autocorrelation.year,prediction.type=='4_TV_spend',year=='Year 3')$total.contribution / sum(mmm.data.autocorrelation$tv.spend[105:156])
cat("TV ROI on year 3 is", tv.ROI.year3)
search.ROI.year3 = filter(average_contribution.autocorrelation.year,prediction.type=='5_Search_spend',year=='Year 3')$total.contribution / sum(mmm.data.autocorrelation$search.spend[105:156])
cat("Search ROI on year 3 is", search.ROI.year3)


total.spend.df = data.frame(rep('Total spend',4),
                        c('Total', 'Year 1', 'Year 2', 'Year 3'), 
                        c(sum(data$total.spend[53:208])/3, sum(data[which(data$time.index %in% c(53:104)),]$total.spend), sum(data[which(data$time.index %in% c(105:156)),]$total.spend), sum(data[which(data$time.index %in% c(157:208)),]$total.spend)))
names(total.spend.df) = c('Type', 'Year', 'Value')
marketing.revenue.df = data.frame(rep('Revenue',4),
                        c('Total', 'Year 1', 'Year 2', 'Year 3'), 
                        c(sum(filter(average_contribution.autocorrelation,prediction.type %in% c('4_TV_spend', '5_Search_spend'))$total.contribution)/3, 
                          sum(filter(average_contribution.autocorrelation.year,prediction.type %in% c('4_TV_spend', '5_Search_spend'),year=='Year 1')$total.contribution), 
                          sum(filter(average_contribution.autocorrelation.year,prediction.type %in% c('4_TV_spend', '5_Search_spend'),year=='Year 2')$total.contribution),
                          sum(filter(average_contribution.autocorrelation.year,prediction.type %in% c('4_TV_spend', '5_Search_spend'),year=='Year 3')$total.contribution)
                        ))
names(marketing.revenue.df) = c('Type', 'Year', 'Value')
revenue.df = rbind(marketing.revenue.df,total.spend.df)
# Plot year on year revenue and spend
ggplot(data=revenue.df, aes(x=Type, y=Value, fill=Year)) + geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Paired") + theme_minimal()

ROI.df = data.frame(c(rep('TV',4), rep('Search',4)), 
                    rep(c('Total', 'Year 1', 'Year 2', 'Year 3'),2), 
                    c(tv.ROI, tv.ROI.year1, tv.ROI.year2, tv.ROI.year3, search.ROI, search.ROI.year1, search.ROI.year2, search.ROI.year3), 
                    c(sum(mmm.data.autocorrelation$tv.spend)/3, sum(mmm.data.autocorrelation[which(mmm.data.autocorrelation$time.index %in% c(53:104)),]$tv.spend), sum(mmm.data.autocorrelation[which(mmm.data.autocorrelation$time.index %in% c(105:156)),]$tv.spend), sum(mmm.data.autocorrelation[which(mmm.data.autocorrelation$time.index %in% c(157:208)),]$tv.spend), 
                      sum(mmm.data.autocorrelation$search.spend)/3, sum(mmm.data.autocorrelation[which(mmm.data.autocorrelation$time.index %in% c(53:104)),]$search.spend), sum(mmm.data.autocorrelation[which(mmm.data.autocorrelation$time.index %in% c(105:156)),]$search.spend), sum(mmm.data.autocorrelation[which(mmm.data.autocorrelation$time.index %in% c(157:208)),]$search.spend)))
names(ROI.df) = c('Type', 'Year', 'ROI', 'Spend')
ROI.df$ROI_yoy_change = ifelse(ROI.df$Year %in% c('Total','Year 1'), 0, (ROI.df$ROI-lag(ROI.df$ROI,1))/ROI.df$ROI)
ROI.df$spend_yoy_change = ifelse(ROI.df$Year %in% c('Total','Year 1'), 0, (ROI.df$Spend-lag(ROI.df$Spend,1))/ROI.df$Spend)
names(ROI.df) = c('Type', 'Year', 'ROI', 'Spend', '% ROI Year on year change', '% Spend Year on year change')
# Plot year on year ROI and spend per marketing type
ggplot(data=ROI.df, aes(x=Type, y=ROI, fill=Year)) + geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Paired") + theme_minimal()
ggplot(data=ROI.df, aes(x=Type, y=Spend, fill=Year)) + geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Paired") + theme_minimal()

# Plot year on year change for ROI
yoy.ROI.df = ROI.df %>% filter(ROI.df$Year %in% c('Year 2', 'Year 3'))
library(scales)
ggplot(data=yoy.ROI.df, aes(x=Type, y=`% ROI Year on year change`, fill=Year)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(data=yoy.ROI.df, aes(x=Type, y=`% ROI Year on year change`, 
                                 label=percent(`% ROI Year on year change`,0.01), 
                                 hjust=ifelse(sign(`% ROI Year on year change`)>0, 1, 0)), 
            position = position_dodge(width=1)) + 
  scale_y_continuous(labels = percent_format()) + 
  coord_flip()
ggplot(data=yoy.ROI.df, aes(x=Type, y=`% Spend Year on year change`, fill=Year)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(data=yoy.ROI.df, aes(x=Type, y=`% Spend Year on year change`, 
                                 label=percent(`% Spend Year on year change`,0.01), 
                                 hjust=ifelse(sign(`% Spend Year on year change`)>0, 1, 0)), 
            position = position_dodge(width=1)) + 
  scale_y_continuous(labels = percent_format()) + 
  coord_flip()
# Comments: 
# - Search: ROI increased in Year 3, while spend was increasing every year. December ROI is not profitable. 
# - TV: ROI is significantly higher than search. ROI and spend was roughly the same over the 3 years. It is worth noting that in Year 2 the spend was the lowest out of the 3 years, while the ROI was the highest out of the 3 years. 

##############################################################################
##### OPTIMIZATION ###########################################################
##############################################################################
# If the predictive model is straightforward (without recurrent characteristics), then optimization can be performed using standard linear programming techniques. For example, lpSolveAPI package in R is sufficient. See more at https://analyticsprofile.com/business-analytics/how-to-optimise-digital-marketing-spend-using-linear-programming-in-r/. 
# If the predictive model has recurrent characteristics (we are in this case since we are using adstock variables and lag_1 vaiables), then linear programming cannot be performed. We can optimize the spending using the last year as input and perform iteratively optimization using similar code to the one in C:\Data_Science\Training\MarketingMixedModelingWithR\Marketing_Mixed_Modeling - Optimization.R


