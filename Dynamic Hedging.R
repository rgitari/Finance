
weather = read.csv('weather_data.csv')
summary(weather)
df <- data.frame(weather)
summary(df)

library(tidyverse)

#create a vector that stores the sum of winter HDD in each year 
a <- vector()
for (i in 1:124) { 
  a[i] <- sum(df[(i*12-11):(i*12-7), 'HDD'])
}
HDD_winter <- a
year <- seq(1895,2018,1) 
plot(year, HDD_winter, type='l', main ='Winter HDD over years') 
#We can see that the HDD is very volatile across the past 140 years 

#Step 3: building up the model 
lm0 <- lm(HDD_winter ~ year)
summary(lm0)

#detrend the residual 
residual <- HDD_winter - lm0$fitted.values 
std_noise <- sd(residual) 

#Step 4
#simulate HDD of the coming winter
set.seed(1230)
scores <- rnorm(10000, 0, std_noise) 
scores <- scores + summary(lm0)$coefficients[2,1]* 2019 + summary(lm0)$coefficients[1,1]


empDensityFunc <- ecdf(scores)
plot(empDensityFunc, xlab = 'sample quantile', ylab='cummulative prob', main='Cummulative Density Function')

#Payoff fo Basic Floor 
payoff_Basic <- vector()
for (i in 1:10000) { 
  if ( scores[i] < 4200 ){ 
     payoff_Basic[i] <- min((4200 - scores[i]) * 15000, 9000000) 
  } else {
    payoff_Basic[i] <- 0
  }
} 

payoff_Basic

?rnorm()
#Payoff to Reduced Tick Size 
payoff_Reduced <- vector() 
for (i in 1:10000) { 
  if ( scores[i] < 4200 ){
    payoff_Reduced[i] <- min((4200 - scores[i]) * 10000, 8000000) 
  } else {
    payoff_Reduced[i] <- 0
  }
}

#Payoff to Lower Stike 
payoff_Lower <- vector() 
for (i in 1:10000) { 
  if ( scores[i] < 3900 ){
    payoff_Lower[i] <- min((3900 - scores[i]) * 15000, 7000000) 
  } else {
    payoff_Lower[i] <- 0
  }
}

#plot of calculated payoff against simulated HDD
plot(scores ,payoff_Basic, xlab='HDD', ylab='payoff', main='Basic_payoff against HDD')
plot(scores ,payoff_Reduced, xlab='HDD', ylab='payoff', main='Reduced_payoff against HDD')
plot(scores ,payoff_Lower, xlab='HDD', ylab='payoff', main='Lower_payoff against HDD')


# profit of Reduced Tick Size 
mean_payoff_Reduced <- mean(payoff_Reduced)
discount_Reduced <- mean_payoff_Reduced * exp(5/12 *(-0.063))
NPV_Reduced <- discount_Reduced - 4000000

#profit of Basic Floor
mean_payoff_Basic <- mean(payoff_Basic)
discount_Basic <- mean_payoff_Basic * exp(5/12 *(-0.063))
NPV_Basic <- discount_Basic - 5500000

#profit of Lower Strike 
mean_payoff_Lower <- mean(payoff_Lower)
discount_Lower <- mean_payoff_Lower * exp(5/12 *(-0.063))
NPV_Lower <- discount_Lower - 2500000

