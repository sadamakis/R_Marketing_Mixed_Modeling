## This example has been taken from https://github.com/joshkyh/amss/blob/master/BayesRegression_Obs.R
## The data are time series

#2) i) Decompose the time series first using y~t+s. 
#   ii) Regress the independent variables on the residuals, y.res~x+t+s. 

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

# Save the residuals
residual.fit = lm(revenue~time.index+february+march+april+may+june+july+august+september+october+november+december, data=data)
summary(residual.fit)
data$revenue.residual = residuals(residual.fit)

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
  dplyr::select(revenue.residual,
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
  revenue.residual,
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
lm.fit <- lm(revenue.residual ~ ., data = mmm.data)
vif(lm.fit)

mmm.data = mmm.data %>% dplyr::select(
  -log.search.matching.query.volume, 
  -tv.spend.adstock2,
  # Remove competitor sales because it self predicts the target variable
  -competitor.sales
)
lm.fit <- lm(revenue.residual ~ ., data = mmm.data)
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
revenue.residual=as.numeric(data$revenue.residual)
Ads = data.frame(search.spend, tv.spend)
AdstockRateMV(revenue.residual, Ads, maxiter = 100, 
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
              dplyr::select(revenue.residual,
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
            
            lm.fit <- lm(revenue.residual ~ ., data = mmm.data)
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
search.spend.adstock1.rate_optimum = 0.80
tv.spend.adstock1.a_exp_optimum = -10
tv.spend.adstock1.rate_optimum = 0.20
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
  dplyr::select(revenue.residual,
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
  revenue.residual,
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
lm.fit <- lm(revenue.residual ~ ., data = mmm.data)
vif(lm.fit)
summary(lm.fit)

# Plot the contribution of each component
plot.series = data.frame(revenue.residual.prediction = c(
  rep(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="(Intercept)")], nrow(mmm.data)), 
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend.adstock1")]*mmm.data$search.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="search.spend")]*mmm.data$search.spend,
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend")]*mmm.data$tv.spend,
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.lag3m")]*mmm.data$competitor.sales.lag3m  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1,
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="february")]*mmm.data$february  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="march")]*mmm.data$march  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="april")]*mmm.data$april  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="may")]*mmm.data$may  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="june")]*mmm.data$june  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="july")]*mmm.data$july  +  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="august")]*mmm.data$august + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="september")]*mmm.data$september + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="october")]*mmm.data$october + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="november")]*mmm.data$november + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="december")]*mmm.data$december
), 
prediction.type = rep(c("5_Base","4_Search_spend","3_TV_spend","2_Competitor_sales", "1_Seasonality"), each=nrow(mmm.data)),
x.axis = rep(c(1:nrow(mmm.data)), 5)
)
ggplot(plot.series, aes(x.axis, revenue.residual.prediction)) + geom_area(aes(fill = prediction.type))

# Calculate the average contribution
plot.series %>% 
  group_by(prediction.type) %>% 
  summarise_at(vars(revenue.residual.prediction), funs(mean, median, sum))
average_contribution = aggregate(plot.series$revenue.residual.prediction, list(plot.series$prediction.type), FUN=c("mean"))
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
# The residuals are not autocorrelated, do not have seasonality, and do not have heteroskedasticity


# Plot the contribution of the significant components
step.model <- stepAIC(lm.fit, direction = "both")
summary(step.model)
plot.series = data.frame(revenue.residual.prediction = c(
  rep(summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="(Intercept)")], nrow(mmm.data)), 
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="tv.spend.adstock1")]*mmm.data$tv.spend.adstock1,
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="competitor.sales.diff1")]*mmm.data$competitor.sales.diff1,
  summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="february")]*mmm.data$february + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="october")]*mmm.data$october + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="november")]*mmm.data$november + summary(lm.fit)$coef[which(rownames(summary(lm.fit)$coef)=="december")]*mmm.data$december
), 
prediction.type = rep(c("5_Base","3_TV_spend","2_Competitor_sales", "1_Seasonality"), each=nrow(mmm.data)),
x.axis = rep(c(1:nrow(mmm.data)), 4)
)
ggplot(plot.series, aes(x.axis, revenue.residual.prediction)) + geom_area(aes(fill = prediction.type))

# Calculate the average contribution
plot.series %>% 
  group_by(prediction.type) %>% 
  summarise_at(vars(revenue.residual.prediction), funs(mean, median, sum))
average_contribution = aggregate(plot.series$revenue.residual.prediction, list(plot.series$prediction.type), FUN=c("mean"))
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


