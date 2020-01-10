dt1
dt2
dt3

########################################################## Question 1 ##################################################################

#Subtract risk free returns from strategy returns to get portfolio returns net of Rf

excess_return_subset <- dt1[,1:5]-dt1[,10]
excess_return <- data.frame(excess_return_subset, dt1[,6:10])
excess_return

Strategy <- c(names(dt1[,c(1:5,7:9)]))

#Sharpe Ratio of all the strategies
sharpe_mk <- mean(excess_return$Market.cap.weighted)*12/(sd(dt1$Market.cap.weighted)*sqrt(12))
sharpe_ew <- mean(excess_return$Equal.weighted)*12/(sd(dt1$Equal.weighted)*sqrt(12))
sharpe_iv <- mean(excess_return$Inverse.Vol)*12/(sd(dt1$Inverse.Vol)*sqrt(12))
sharpe_mw <- mean(excess_return$MVP.weighted)*12/(sd(dt1$MVP.weighted)*sqrt(12))
sharpe_fc <- mean(excess_return$FundamentalComposite)*12/(sd(dt1$FundamentalComposite)*sqrt(12))
sharpe_sb <- mean(excess_return$SMB)*12/(sd(dt1$SMB)*sqrt(12))
sharpe_hl <- mean(excess_return$HML)*12/(sd(dt1$HML)*sqrt(12))
sharpe_mm <- mean(excess_return$MOM)*12/(sd(dt1$MOM)*sqrt(12))

sharpe <- c(sharpe_mk, sharpe_ew, sharpe_iv, sharpe_mw, sharpe_fc, sharpe_sb, sharpe_hl, sharpe_mm)

sharpe_ratios <- data.frame(Strategy, sharpe)
sharpe_ratios
ggplot(sharpe_ratios, aes(y=sharpe_ratios$sharpe, x=as.factor(sharpe_ratios$Strategy)))+ geom_point(size=3)+ theme_bw() + 
  ggtitle("Sharpe Ratios for all the strategies") + 
  ylab("Sharpe Ratio")+ 
  xlab("Strategy")

########################################################## Question 2 ##################################################################

#Regressing all the strategies with market retuns 

mktcap_ofm <- lm(excess_return$Market.cap.weighted~excess_return$Mkt.RF, data = excess_return)
eqlwt_ofm <- lm(excess_return$Equal.weighted~excess_return$Mkt.RF, data = excess_return)
invol_ofm <- lm(excess_return$Inverse.Vol~excess_return$Mkt.RF, data = excess_return)
mvpwt_ofm <- lm(excess_return$MVP.weighted~excess_return$Mkt.RF, data = excess_return)
funcom_ofm <- lm(excess_return$FundamentalComposite~excess_return$Mkt.RF, data = excess_return)
smb_ofm <- lm(excess_return$SMB ~excess_return$Mkt.RF, data = excess_return)
hml_ofm <- lm(excess_return$HML ~excess_return$Mkt.RF, data = excess_return)
mom_ofm <- lm(excess_return$MOM ~excess_return$Mkt.RF, data = excess_return)

regression <- list(mktcap_ofm, eqlwt_ofm, invol_ofm, mvpwt_ofm, funcom_ofm, smb_ofm, hml_ofm, mom_ofm)

summary(regression[[2]])$coefficients[1,1] 

alpha <- vector()
beta <- vector()
Rsquared <- vector()
tstat_alpha<- vector()
tstat_beta <- vector()
Strategy <- c(names(dt1[,c(1:5,7:9)]))

#Summarizing the regression results 

for (i in 1:length(regression)){
  alpha[i] <- summary(regression[[i]])$coefficients[1,1] * 100 
  tstat_alpha[i] <- summary(regression[[i]])$coefficients[1,3] 
  beta[i] <- summary(regression[[i]])$coefficients[2,1]
  tstat_beta[i] <- summary(regression[[i]])$coefficients[2,3] 
  Rsquared[i] <- summary(regression[[i]])$r.squared
}

results<-data.frame(Strategy, alpha, tstat_alpha, beta, tstat_beta, Rsquared) 
results$idiosyncratic_risk <- 1-results$Rsquared

results
######################################################### Question 3 ####################################################################

Old_factors <- dt1[,6:9] 
dvList_new <- names(dt1)[1:5]

dvList <- names(new_dt1)  
modelfits <- vector(length(dvList_new), mode = "list")
names(modelfits) <- dvList_new
for(i in dvList_new) {
  modelformula <- paste(i," ~ Old_factors$Mkt.RF + Old_factors$SMB + Old_factors$HML + Old_factors$MOM ")  
  modelfits[[i]] <- lm(as.formula(modelformula), data = new_dt1) 
} 

alpha <- vector()
alpha_tstat <- vector()
Market_beta = vector()
Market_tstat <- vector()
SMB_beta = vector()
SMB_tstat <- vector()
HML_beta = vector()
HML_tstat <- vector()
MOM_beta = vector()
MOM_tstat <- vector()
Rsquare <- vector() 

for (j in 1:5) {
  alpha[j] <- summary(modelfits[[j]])$coefficients[1,1] 
  alpha_tstat[j] <- summary(modelfits[[j]])$coefficients[1,3]
  Market_beta[j] <- summary(modelfits[[j]])$coefficients[2,1]
  Market_tstat[j] <- summary(modelfits[[j]])$coefficients[2,3] 
  SMB_beta[j] <- summary(modelfits[[j]])$coefficients[3,1]
  SMB_tstat[j] <- summary(modelfits[[j]])$coefficients[3,3] 
  HML_beta[j] <- summary(modelfits[[j]])$coefficients[4,1]
  HML_tstat[j] <- summary(modelfits[[j]])$coefficients[4,3] 
  MOM_beta[j] <- summary(modelfits[[j]])$coefficients[5,1]
  MOM_tstat[j] <- summary(modelfits[[j]])$coefficients[5,3] 
  Rsquare[j] <- summary(modelfits[[j]])$r.squared
} 

#tables of output
tstat_table2 <- cbind(alpha_tstat, Market_tstat, SMB_tstat, HML_tstat, MOM_tstat)
row.names(tstat_table2) <- dvList_new
tstat_table

coeff_table2 <- cbind(alpha*100, Market_beta, SMB_beta, HML_beta, MOM_beta, Rsquare)
row.names(coeff_table2) <- dvList_new
coeff_table2

########################################################## Question 4 ##################################################################


sharpe_bh <- mean(dt2$Berkshire.Hathaway.A-dt2$RF)*12/(sd(dt2$Berkshire.Hathaway.A)*sqrt(12))

#Full sample 
dt2$brka_rf <- dt2$Berkshire.Hathaway.A - dt2$RF
lm <- lm(brka_rf ~ Mkt.RF + SMB+ HML + MOM, data= dt2)
summary(lm)

#Subsample 1: April 1980 and ending December 2007 
#198004 - 200712
dt2[405,]
tail(dt2)
subsample_1 <- dt2[1:333,]
head(subsample_1)

lm1 <- lm(brka_rf ~ Mkt.RF + SMB+ HML + MOM, data= subsample_1)
summary(lm1)
#Subsample 2: January 2008 and ending December 2013
#200801 - 201312
subsample_2 <- dt2[334:405,]
head(subsample_2)

lm2 <- lm(brka_rf ~ Mkt.RF + SMB+ HML + MOM, data= subsample_2)
summary(lm2)

## Intercept (alpha)/ Cofficients 
cof <- as.matrix(lm$coefficients)
cof1 <- as.matrix(lm1$coefficients)
cof2 <- as.matrix(lm2$coefficients)

## t stats 
tstat <- summary(lm)$coefficients[,3] 
tstat1 <- summary(lm1)$coefficients[,3] 
tstat2 <- summary(lm2)$coefficients[,3] 

regresults <- as.data.frame(cbind(cof,tstat,cof1,tstat1,cof2,tstat2))
colnames <-  c("Full sample coefficients", "Full sample t stats",
               "Subsample 1 coefficients", "Subsample 1 t stat", 
               "Subsample 2 coefficients", "Subsample 2 t stat" )
names(regresults) <- colnames
final1 <- as.data.frame(regresults) #3 Need intercept as a % 
transposed <- transpose(as.matrix(final1))
regresults_2 <- paste0(round(regresults[1,] * 100,3), "%")
regresults_2
#R2 of each regression, need as a number
r2 <- summary(lm)$r.squared
r2_1 <- summary(lm1)$r.squared
r2_2 <- summary(lm2)$r.squared
r2_results <- as.data.frame(cbind(r2,r2_1,r2_2))
colnames_2 <-  c("Full sample R squared", "Subsample 1 R squared",
                 "Subsample 2 R squared") 
names(r2_results) <- colnames_2
r2_results 

#Calculate sharpe ratio for BRK-A and value weighted market index
# Annualized Mean Return = 12* Monthly Mean Return
# Annualized Std. Dev. of Returns = (√12) * Monthly Std. Dev. of Returns
## Berkshire
annmean <- mean(dt2$brka_rf) *  12
annstd <- sd(dt2$Berkshire.Hathaway.A)* sqrt(12) 
sharpe_ratio_portfolio <- (annmean/annstd) # 0.752496
sharpe_ratio_portfolio

## Market 
mean(dt2$Mkt.RF) * 12 # 0.07994074
sd(dt2$mkt) * sqrt(12) # 0.1560834
mean(dt2$Mkt.RF) * 12 / sd(dt2$mkt) * sqrt(12)
0.07994074/0.1560834
dt2$mkt <- dt2$Mkt.RF + dt2$RF
cor_2 <- cor(dt2)
cor_2

########################################################## Question 5 ##################################################################
install.packages("stringr")
install.packages("Rsolnp")
library(stringr)
library(Rsolnp)

y=dt3$BRK.A
x1=dt3$Vanguard.S.P.500.Index.Inv..VFINX.
x2=dt3$Vanguard.Small.Cap.Index.Inv..NAESX.
x3=dt3$Vanguard.Value.Index.Inv..VIVAX.

ind_sub=str_sub(rownames(dt3),-4,-1) %in% as.character(2008:2013)

#define loss function
f_loss=function(b){
  p=y
  b=b[1]+b[2]*x1+b[3]*x2+b[4]*x3
  return(sum((b-p)^2))
}

#equality constraints function
eq=function(b){
  return(b[2]+b[3]+b[4])
}

#random start points for parameter optimization
theta=c(0.1,0.5,0.25,0.25)

f=function(dt,sub=FALSE, Shorts=FALSE){
  if(sub){
    dt=dt[ind_sub,]
  }
  y=dt[,1] #BRK.A
  x1=dt[,2] #index1
  x2=dt[,3] #index2
  x3=dt[,4] #index3
  
  #define loss functino
  f_loss=function(b){
    p=y
    b=b[1]+b[2]*x1+b[3]*x2+b[4]*x3
    return(sum((b-p)^2))
  }
  if(Shorts){
    m=solnp(theta,f_loss,eqfun=eq,eqB=1,LB=c(-Inf,-Inf,-Inf,-Inf))
  }else{
    m=solnp(theta,f_loss,eqfun=eq,eqB=1,LB=c(-Inf,0,0,0))
  }
  w=m$pars
  y_b=as.matrix(dt) %*% w
  mar=mean(y-y_b)
  te=sd(y-y_b)
  return(c(w,mar,te))
}

r1 <- f(dt3, sub = FALSE, Shorts = FALSE) # Full sample
r2 <- f(dt3, sub = F, Shorts = T)   # if shorts were allowed in full sample
r3 <- f(dt3, sub = T, Shorts = F)  # sub sample 
r4 <- f(dt3, sub = T, Shorts = T) # if shorts were allowed in sub sample 

r1
r3

returns <- as.data.frame(sapply(dt3,FUN=mean))
returns$sds <- sapply(dt3,FUN=sd)
colnames(returns) <- c("Average Monthly Returns", "Standard Deviation of Monthly Returns")
res <- cor(dt3)
corr <- as.data.frame(res)
round(corr, 2)
returns
corr

########################################################## Question 1 EXTRA ##################################################################

#Annualised returns

Annualised_return <- matrix(data = seq(1,430), nrow = 43,byrow=TRUE)

for (i in 1:nrow(Annualised_return)) {
  for (j in 1:ncol(Annualised_return)) {
    Annualised_return[i,j]=sum(excess_return[(12*i-11):(12*i),j])
  }
}


colnames(Annualised_return) <-c(names(dt1)) 

Annualised_return

#Annualised standard deviation

Annualised_sd <- matrix(data = seq(1,430, by=1), nrow = 43, ncol =10)

for (i in 1:nrow(Annualised_sd)) {
  for (j in 1:ncol(Annualised_sd)) {
    Annualised_sd[i,j]=sd(dt1[(12*i-11):(12*i),j])*sqrt(12)
  }
}

colnames(Annualised_sd) <-c(names(dt1)) 

Annualised_sd
colnames(Annualised_sd) <-c(names(dt1)) 


#Annual sharpe ratios 

Annualised_sharpe <- matrix(data = seq(1,430, by=1), nrow = 43, ncol =10)
for (i in 1:nrow(Annualised_sharpe)) {
  for (j in 1:ncol(Annualised_sharpe)) {
    Annualised_sharpe[i,j]=abs(Annualised_return[i,j])/Annualised_sd[i,j]
  }
}

colnames(Annualised_sharpe) <-c(names(dt1)) 

Annualised_sharpe


year <- 1968+seq(1,43, by = 1)
results1 <- data.frame(year,Annualised_sharpe)
results1