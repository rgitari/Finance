
load('data_4.RData') 
summary(dt)
View(dt)

#question 1

#a
#Whole Sample
mean_table <- sapply(dt[,6:9], mean)
std_table <- sapply(dt[,6:9], sd)

mean_rf <- mean(dt$Tbill)
excessret_table <- mean_table - mean_rf 
sharpe_table <- excessret_table / std_table

#low inflation and low growth 
dt1 <- dt[which(dt$INF1GRW1==1),] 
mean_table1 <- sapply(dt1[,6:9], mean) 
std_table1 <- sapply(dt1[,6:9], sd)

mean_rf1 <- mean(dt1$Tbill)
excessret_table1 <- mean_table1 - mean_rf1
sharpe_table1 <- excessret_table1 / std_table1 

#high inflation and low growth 
dt2 <- dt[which(dt$INF2GRW1==1),]
mean_table2 <- sapply(dt2[,6:9], mean) 
std_table2 <- sapply(dt2[,6:9], sd)  

mean_rf2 <- mean(dt2$Tbill)
excessret_table2 <- mean_table2 - mean_rf2 
sharpe_table2 <- excessret_table2 / std_table2 

#low inflation and high growth 
dt3 <- dt[which(dt$INF1GRW2==1),]
mean_table3 <- sapply(dt3[,6:9], mean) 
std_table3 <- sapply(dt3[,6:9], sd)  

mean_rf3 <- mean(dt3$Tbill)
excessret_table3 <- mean_table3 - mean_rf3
sharpe_table3 <- excessret_table3 / std_table3

#high inflation and high growth 
dt4 <- dt[which(dt$INF2GRW2==1),]
mean_table4 <- sapply(dt4[,6:9], mean) 
std_table4 <- sapply(dt4[,6:9], sd)  

mean_rf4 <- mean(dt4$Tbill)
excessret_table4 <- mean_table4 - mean_rf4 
sharpe_table4 <- excessret_table4 / std_table4


#b
RiskyAsset <- dt[,6:9]
VCV_matrix <- matrix(c(cov(RiskyAsset)), nrow = 4, ncol = 4) 
range.names <- c('Stocks','Bonds','Gold','Commodities')
dimnames(VCV_matrix) <- list(range.names, range.names)
covmat <- VCV_matrix  

RiskyAsset1 <- dt1[,6:9]
VCV_matrix1 <- matrix(c(cov(RiskyAsset1)), nrow = 4, ncol = 4) 
range.names <- c('Stocks','Bonds','Gold','Commodities')
dimnames(VCV_matrix1) <- list(range.names, range.names)
covmat1 <- VCV_matrix1 

RiskyAsset2 <- dt2[,6:9]
VCV_matrix2 <- matrix(c(cov(RiskyAsset2)), nrow = 4, ncol = 4) 
range.names <- c('Stocks','Bonds','Gold','Commodities')
dimnames(VCV_matrix2) <- list(range.names, range.names)
covmat2 <- VCV_matrix2

RiskyAsset3 <- dt3[,6:9]
VCV_matrix3 <- matrix(c(cov(RiskyAsset3)), nrow = 4, ncol = 4) 
range.names <- c('Stocks','Bonds','Gold','Commodities')
dimnames(VCV_matrix3) <- list(range.names, range.names)
covmat3 <- VCV_matrix3

RiskyAsset4 <- dt4[,6:9]
VCV_matrix4 <- matrix(c(cov(RiskyAsset4)), nrow = 4, ncol = 4) 
range.names <- c('Stocks','Bonds','Gold','Commodities')
dimnames(VCV_matrix4) <- list(range.names, range.names)
covmat4 <- VCV_matrix4 

#c 
library(tseries)
library(propagate) 
library(IntroCompFinR)
er <- mean_table 
cov.mat <- VCV_matrix
risk.free <- mean_rf
tanPort <- tangency.portfolio(mean_table, VCV_matrix, mean_rf, shorts = TRUE) 
weight_tanPort <- tanPort$weights  

#Optimal allocation for investors with different risk preferences
#The optimal weights of risky portfolio = (E(r_p) - rf) / (A * var(r_P))
ret_tanPort<- weight_tanPort %*% er 
std_tanPort <- tanPort$sd
sharpe_tanPort <- (ret_tanPort - mean_rf) / std_tanPort 
sharpe_tanPort


tanPort1 <- tangency.portfolio(mean_table1, VCV_matrix1, mean_rf1, shorts = TRUE) 
weight_tanPort1 <- tanPort1$weights  
#Optimal allocation for investors with different risk preferences
#The optimal weights of risky portfolio = (E(r_p) - rf) / (A * var(r_P))
ret_tanPort1<- tanPort1$er
std_tanPort1 <- tanPort1$sd
sharpe_tanPort1 <- (ret_tanPort1 - mean_rf1) / std_tanPort1 
sharpe_tanPort1 


tanPort2 <- tangency.portfolio(mean_table2, VCV_matrix2, mean_rf2, shorts = TRUE) 
weight_tanPort2 <- tanPort2$weights  
#Optimal allocation for investors with different risk preferences
#The optimal weights of risky portfolio = (E(r_p) - rf) / (A * var(r_P))
ret_tanPort2<- tanPort2$er
std_tanPort2 <- tanPort2$sd 
sharpe_tanPort2 <- (ret_tanPort2 - mean_rf2) / std_tanPort2
sharpe_tanPort2

tanPort3 <- tangency.portfolio(mean_table3, VCV_matrix3, mean_rf3, shorts = TRUE) 
weight_tanPort3 <- tanPort3$weights  
#Optimal allocation for investors with different risk preferences
#The optimal weights of risky portfolio = (E(r_p) - rf) / (A * var(r_P))
ret_tanPort3<- tanPort3$er 
std_tanPort3 <- tanPort3$sd 
sharpe_tanPort3 <- (ret_tanPort3 - mean_rf3) / std_tanPort3 
sharpe_tanPort3

tanPort4 <- tangency.portfolio(mean_table4, VCV_matrix4, mean_rf4, shorts = TRUE) 
weight_tanPort4 <- tanPort4$weights  
#Optimal allocation for investors with different risk preferences
#The optimal weights of risky portfolio = (E(r_p) - rf) / (A * var(r_P))
ret_tanPort4 <- tanPort4$er 
std_tanPort4 <- tanPort4$sd 
sharpe_tanPort4 <- (ret_tanPort4 - mean_rf4) / std_tanPort4
sharpe_tanPort4


#d
#Global Minimum Variance Portfolio 
GMV <- globalMin.portfolio(mean_table, VCV_matrix)
weight_GMV <- GMV$weights
weight_GMV
ret_GMV <- GMV$er
std_GMV <- GMV$sd
sharpe_GMV <- (ret_GMV - mean_rf) / std_GMV 

GMV1 <- globalMin.portfolio(mean_table1, VCV_matrix1)
weight_GMV1 <- GMV1$weights
weight_GMV1
ret_GMV1 <- GMV1$er
std_GMV1 <- GMV1$sd
sharpe_GMV1 <- (ret_GMV1 - mean_rf1) / std_GMV1 

GMV2 <- globalMin.portfolio(mean_table2, VCV_matrix2)
weight_GMV2 <- GMV2$weights
weight_GMV2
ret_GMV2 <- GMV2$er
std_GMV2 <- GMV2$sd
sharpe_GMV2 <- (ret_GMV2 - mean_rf2) / std_GMV2

GMV3 <- globalMin.portfolio(mean_table3, VCV_matrix3)
weight_GMV3 <- GMV3$weights
weight_GMV3
ret_GMV3 <- GMV3$er
std_GMV3 <- GMV3$sd
sharpe_GMV3 <- (ret_GMV3 - mean_rf3) / std_GMV3

GMV4 <- globalMin.portfolio(mean_table4, VCV_matrix4)
weight_GMV4 <- GMV4$weights
weight_GMV4
ret_GMV4 <- GMV4$er
std_GMV4 <- GMV4$sd
sharpe_GMV4 <- (ret_GMV4 - mean_rf4) / std_GMV4


#e 
#Investors with different levels of risk aversion
riskyPortweight_1 <- (ret_tanPort - mean_rf) / (1.3 * (std_tanPort)^2)
riskyPortweight_2 <- (ret_tanPort - mean_rf) / (2.8 * (std_tanPort)^2)
riskyPortweight_3 <- (ret_tanPort - mean_rf) / (6.5 * (std_tanPort)^2)
riskyPortweight_4 <- (ret_tanPort - mean_rf) / (10.5 * (std_tanPort)^2)
riskyPortweight_5 <- (ret_tanPort - mean_rf) / (16.9 * (std_tanPort)^2) 

ret_1 <- riskyPortweight_1 * ret_tanPort + (1 - riskyPortweight_1) * mean_rf
ret_2 <- riskyPortweight_2 * ret_tanPort + (1 - riskyPortweight_2) * mean_rf
ret_3 <- riskyPortweight_3 * ret_tanPort + (1 - riskyPortweight_3) * mean_rf
ret_4 <- riskyPortweight_4 * ret_tanPort + (1 - riskyPortweight_4) * mean_rf
ret_5 <- riskyPortweight_5 * ret_tanPort + (1 - riskyPortweight_5) * mean_rf

std_port_1 <- riskyPortweight_1 * std_tanPort 
std_port_2 <- riskyPortweight_2 * std_tanPort 
std_port_3 <- riskyPortweight_3 * std_tanPort 
std_port_4 <- riskyPortweight_4 * std_tanPort 
std_port_5 <- riskyPortweight_5 * std_tanPort  

sharpe_1 <- (ret_1 - mean_rf) / std_port_1 
sharpe_2 <- (ret_2 - mean_rf) / std_port_2
sharpe_3 <- (ret_3 - mean_rf) / std_port_3 
sharpe_4 <- (ret_4 - mean_rf) / std_port_4 
sharpe_5 <- (ret_5 - mean_rf) / std_port_5 


# sample 1
riskyPort1weight_1 <- (ret_tanPort1 - mean_rf1) / (1.3 * (std_tanPort1)^2)
riskyPort1weight_2 <- (ret_tanPort1 - mean_rf1) / (2.8 * (std_tanPort1)^2)
riskyPort1weight_3 <- (ret_tanPort1 - mean_rf1) / (6.5 * (std_tanPort1)^2)
riskyPort1weight_4 <- (ret_tanPort1 - mean_rf1) / (10.5 * (std_tanPort1)^2)
riskyPort1weight_5 <- (ret_tanPort1 - mean_rf1) / (16.9 * (std_tanPort1)^2) 

ret1_1 <- riskyPort1weight_1 * ret_tanPort1 + (1 - riskyPort1weight_1) * mean_rf1
ret1_2 <- riskyPort1weight_2 * ret_tanPort1 + (1 - riskyPort1weight_2) * mean_rf1
ret1_3 <- riskyPort1weight_3 * ret_tanPort1 + (1 - riskyPort1weight_3) * mean_rf1
ret1_4 <- riskyPort1weight_4 * ret_tanPort1 + (1 - riskyPort1weight_4) * mean_rf1
ret1_5 <- riskyPort1weight_5 * ret_tanPort1 + (1 - riskyPort1weight_5) * mean_rf1

std_port1_1 <- riskyPort1weight_1 * std_tanPort1 
std_port1_2 <- riskyPort1weight_2 * std_tanPort1 
std_port1_3 <- riskyPort1weight_3 * std_tanPort1 
std_port1_4 <- riskyPort1weight_4 * std_tanPort1 
std_port1_5 <- riskyPort1weight_5 * std_tanPort1  

sharpe1_1 <- (ret1_1 - mean_rf1) / std_port1_1 
sharpe1_2 <- (ret1_2 - mean_rf1) / std_port1_2
sharpe1_3 <- (ret1_3 - mean_rf1) / std_port1_3 
sharpe1_4 <- (ret1_4 - mean_rf1) / std_port1_4 
sharpe1_5 <- (ret1_5 - mean_rf1) / std_port1_5 



#Sample 2
riskyPort2weight_1 <- (ret_tanPort2 - mean_rf2) / (1.3 * (std_tanPort2)^2)
riskyPort2weight_2 <- (ret_tanPort2 - mean_rf2) / (2.8 * (std_tanPort2)^2)
riskyPort2weight_3 <- (ret_tanPort2 - mean_rf2) / (6.5 * (std_tanPort2)^2)
riskyPort2weight_4 <- (ret_tanPort2 - mean_rf2) / (10.5 * (std_tanPort2)^2)
riskyPort2weight_5 <- (ret_tanPort2 - mean_rf2) / (16.9 * (std_tanPort2)^2) 

ret2_1 <- riskyPort2weight_1 * ret_tanPort2 + (1 - riskyPort2weight_1) * mean_rf2
ret2_2 <- riskyPort2weight_2 * ret_tanPort2 + (1 - riskyPort2weight_2) * mean_rf2
ret2_3 <- riskyPort2weight_3 * ret_tanPort2 + (1 - riskyPort2weight_3) * mean_rf2
ret2_4 <- riskyPort2weight_4 * ret_tanPort2 + (1 - riskyPort2weight_4) * mean_rf2
ret2_5 <- riskyPort2weight_5 * ret_tanPort2 + (1 - riskyPort2weight_5) * mean_rf2

std_port2_1 <- riskyPort2weight_1 * std_tanPort2
std_port2_2 <- riskyPort2weight_2 * std_tanPort2 
std_port2_3 <- riskyPort2weight_3 * std_tanPort2 
std_port2_4 <- riskyPort2weight_4 * std_tanPort2 
std_port2_5 <- riskyPort2weight_5 * std_tanPort2  

sharpe2_1 <- (ret2_1 - mean_rf2) / std_port2_1 
sharpe2_2 <- (ret2_2 - mean_rf2) / std_port2_2
sharpe2_3 <- (ret2_3 - mean_rf2) / std_port2_3 
sharpe2_4 <- (ret2_4 - mean_rf2) / std_port2_4 
sharpe2_5 <- (ret2_5 - mean_rf2) / std_port2_5 


#Sample 3
riskyPort3weight_1 <- (ret_tanPort3 - mean_rf3) / (1.3 * (std_tanPort3)^2)
riskyPort3weight_2 <- (ret_tanPort3 - mean_rf3) / (2.8 * (std_tanPort3)^2)
riskyPort3weight_3 <- (ret_tanPort3 - mean_rf3) / (6.5 * (std_tanPort3)^2)
riskyPort3weight_4 <- (ret_tanPort3 - mean_rf3) / (10.5 * (std_tanPort3)^2)
riskyPort3weight_5 <- (ret_tanPort3 - mean_rf3) / (16.9 * (std_tanPort3)^2) 

ret3_1 <- riskyPort3weight_1 * ret_tanPort3 + (1 - riskyPort3weight_1) * mean_rf3
ret3_2 <- riskyPort3weight_2 * ret_tanPort3 + (1 - riskyPort3weight_2) * mean_rf3
ret3_3 <- riskyPort3weight_3 * ret_tanPort3 + (1 - riskyPort3weight_3) * mean_rf3
ret3_4 <- riskyPort3weight_4 * ret_tanPort3 + (1 - riskyPort3weight_4) * mean_rf3
ret3_5 <- riskyPort3weight_5 * ret_tanPort3 + (1 - riskyPort3weight_5) * mean_rf3

std_port3_1 <- riskyPort3weight_1 * std_tanPort3
std_port3_2 <- riskyPort3weight_2 * std_tanPort3 
std_port3_3 <- riskyPort3weight_3 * std_tanPort3 
std_port3_4 <- riskyPort3weight_4 * std_tanPort3 
std_port3_5 <- riskyPort3weight_5 * std_tanPort3  

sharpe3_1 <- (ret3_1 - mean_rf3) / std_port3_1 
sharpe3_2 <- (ret3_2 - mean_rf3) / std_port3_2
sharpe3_3 <- (ret3_3 - mean_rf3) / std_port3_3 
sharpe3_4 <- (ret3_4 - mean_rf3) / std_port3_4 
sharpe3_5 <- (ret3_5 - mean_rf3) / std_port3_5 



#Sample 4
riskyPort4weight_1 <- (ret_tanPort4 - mean_rf4) / (1.3 * (std_tanPort4)^2)
riskyPort4weight_2 <- (ret_tanPort4 - mean_rf4) / (2.8 * (std_tanPort4)^2)
riskyPort4weight_3 <- (ret_tanPort4 - mean_rf4) / (6.5 * (std_tanPort4)^2)
riskyPort4weight_4 <- (ret_tanPort4 - mean_rf4) / (10.5 * (std_tanPort4)^2)
riskyPort4weight_5 <- (ret_tanPort4 - mean_rf4) / (16.9 * (std_tanPort4)^2) 

ret4_1 <- riskyPort4weight_1 * ret_tanPort4 + (1 - riskyPort4weight_1) * mean_rf4
ret4_2 <- riskyPort4weight_2 * ret_tanPort4 + (1 - riskyPort4weight_2) * mean_rf4
ret4_3 <- riskyPort4weight_3 * ret_tanPort4 + (1 - riskyPort4weight_3) * mean_rf4
ret4_4 <- riskyPort4weight_4 * ret_tanPort4 + (1 - riskyPort4weight_4) * mean_rf4
ret4_5 <- riskyPort4weight_5 * ret_tanPort4 + (1 - riskyPort4weight_5) * mean_rf4

std_port4_1 <- riskyPort4weight_1 * std_tanPort4
std_port4_2 <- riskyPort4weight_2 * std_tanPort4 
std_port4_3 <- riskyPort4weight_3 * std_tanPort4 
std_port4_4 <- riskyPort4weight_4 * std_tanPort4 
std_port4_5 <- riskyPort4weight_5 * std_tanPort4  

sharpe4_1 <- (ret4_1 - mean_rf4) / std_port4_1 
sharpe4_2 <- (ret4_2 - mean_rf4) / std_port4_2
sharpe4_3 <- (ret4_3 - mean_rf4) / std_port4_3 
sharpe4_4 <- (ret4_4 - mean_rf4) / std_port4_4 
sharpe4_5 <- (ret4_5 - mean_rf4) / std_port4_5 


#Q3 
#Static-EW Portfolio with equal weight to each regim
temp <- c(riskyPort1weight_3, riskyPort2weight_3, riskyPort3weight_3, riskyPort4weight_3)
temp_riskfree <- 1 - temp
Static_RiskyPort_weight <- 0.25 * temp 
Static_riskfree_weight <- 0.25 * temp_riskfree
Static_Port1_weight <- Static_RiskyPort_weight[1] * weight_tanPort1
Static_Port2_weight <- Static_RiskyPort_weight[2] * weight_tanPort2
Static_Port3_weight <- Static_RiskyPort_weight[3] * weight_tanPort3
Static_Port4_weight <- Static_RiskyPort_weight[4] * weight_tanPort4

Static_riskfree <- sum(Static_riskfree_weight)

Static_Port <- Static_Port1_weight  + Static_Port2_weight + Static_Port3_weight + Static_Port4_weight
Static_Port <- c(Static_Port, Static_riskfree)
names(Static_Port) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
Static_Port

# Tilt-INF1GRW1
Static1_RiskyPort_weight <- c(0.5*riskyPort1weight_3, 1/6*riskyPort2weight_3, 1/6*riskyPort3weight_3, 1/6*riskyPort4weight_3)
Static1_riskfree_weight <- c(0.5*(1-riskyPort1weight_3), 1/6*(1-riskyPort2weight_3), 1/6*(1-riskyPort3weight_3), 1/6*(1-riskyPort4weight_3))
Static1_Port1_weight <- Static1_RiskyPort_weight[1] * weight_tanPort1
Static1_Port2_weight <- Static1_RiskyPort_weight[2] * weight_tanPort2
Static1_Port3_weight <- Static1_RiskyPort_weight[3] * weight_tanPort3
Static1_Port4_weight <- Static1_RiskyPort_weight[4] * weight_tanPort4

Static1_riskfree <- sum(Static1_riskfree_weight)

Static1_Port <- Static1_Port1_weight  + Static1_Port2_weight + Static1_Port3_weight + Static1_Port4_weight
Static1_Port <- c(Static1_Port, Static1_riskfree)
names(Static1_Port) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
Static1_Port

# Tilt-INF2GRW1
Static2_RiskyPort_weight <- c(1/6*riskyPort1weight_3, 0.5*riskyPort2weight_3, 1/6*riskyPort3weight_3, 1/6*riskyPort4weight_3)
Static2_riskfree_weight <- c(1/6*(1-riskyPort1weight_3), 0.5*(1-riskyPort2weight_3), 1/6*(1-riskyPort3weight_3), 1/6*(1-riskyPort4weight_3))
Static2_Port1_weight <- Static2_RiskyPort_weight[1] * weight_tanPort1
Static2_Port2_weight <- Static2_RiskyPort_weight[2] * weight_tanPort2
Static2_Port3_weight <- Static2_RiskyPort_weight[3] * weight_tanPort3
Static2_Port4_weight <- Static2_RiskyPort_weight[4] * weight_tanPort4

Static2_riskfree <- sum(Static2_riskfree_weight)

Static2_Port <- Static2_Port1_weight  + Static2_Port2_weight + Static2_Port3_weight + Static2_Port4_weight
Static2_Port <- c(Static2_Port, Static2_riskfree)
names(Static2_Port) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
Static2_Port

# Tilt-INF1GRW2
Static3_RiskyPort_weight <- c(1/6*riskyPort1weight_3, 1/6*riskyPort2weight_3, 0.5*riskyPort3weight_3, 1/6*riskyPort4weight_3)
Static3_riskfree_weight <- c(1/6*(1-riskyPort1weight_3), 1/6*(1-riskyPort2weight_3), 0.5*(1-riskyPort3weight_3), 1/6*(1-riskyPort4weight_3))
Static3_Port1_weight <- Static3_RiskyPort_weight[1] * weight_tanPort1
Static3_Port2_weight <- Static3_RiskyPort_weight[2] * weight_tanPort2
Static3_Port3_weight <- Static3_RiskyPort_weight[3] * weight_tanPort3
Static3_Port4_weight <- Static3_RiskyPort_weight[4] * weight_tanPort4

Static3_riskfree <- sum(Static3_riskfree_weight)

Static3_Port <- Static3_Port1_weight  + Static3_Port2_weight + Static3_Port3_weight + Static3_Port4_weight
Static3_Port <- c(Static3_Port, Static3_riskfree)
names(Static3_Port) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
Static3_Port 

# Tilt-INF2GRW2
Static4_RiskyPort_weight <- c(1/6*riskyPort1weight_3, 1/6*riskyPort2weight_3, 1/6*riskyPort3weight_3, 0.5*riskyPort4weight_3)
Static4_riskfree_weight <- c(1/6*(1-riskyPort1weight_3), 1/6*(1-riskyPort2weight_3), 1/6*(1-riskyPort3weight_3), 0.5*(1-riskyPort4weight_3))
Static4_Port1_weight <- Static4_RiskyPort_weight[1] * weight_tanPort1
Static4_Port2_weight <- Static4_RiskyPort_weight[2] * weight_tanPort2
Static4_Port3_weight <- Static4_RiskyPort_weight[3] * weight_tanPort3
Static4_Port4_weight <- Static4_RiskyPort_weight[4] * weight_tanPort4

Static4_riskfree <- sum(Static4_riskfree_weight)

Static4_Port <- Static4_Port1_weight  + Static4_Port2_weight + Static4_Port3_weight + Static4_Port4_weight
Static4_Port <- c(Static4_Port, Static4_riskfree)
names(Static4_Port) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
Static4_Port 
Static1_Port

0.57339792 + -0.12765018  + 0.12281139 + 0.38120765  + 0.05023323 

#Q4
WholeSample <- riskyPortweight_3 %*% weight_tanPort 
WholeSample <- c(WholeSample, 1-riskyPortweight_3)
names(WholeSample) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
WholeSample

INF1GRW1_6.5 <- riskyPort1weight_3 %*% weight_tanPort1
INF1GRW1_6.5 <- c(INF1GRW1_6.5, 1 - riskyPort1weight_3)
names(INF1GRW1_6.5) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
INF1GRW1_6.5

INF2GRW1_6.5 <- riskyPort2weight_3 %*% weight_tanPort2
INF2GRW1_6.5 <- c(INF2GRW1_6.5, 1 - riskyPort2weight_3)
names(INF2GRW1_6.5) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
INF2GRW1_6.5

INF1GRW2_6.5 <- riskyPort3weight_3 %*% weight_tanPort3
INF1GRW2_6.5 <- c(INF1GRW2_6.5, 1 - riskyPort3weight_3)
names(INF1GRW2_6.5) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
INF1GRW2_6.5 

INF2GRW2_6.5 <- riskyPort4weight_3 %*% weight_tanPort4
INF2GRW2_6.5 <- c(INF2GRW2_6.5, 1 - riskyPort4weight_3)
names(INF2GRW2_6.5) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
INF2GRW2_6.5  

#weight table of 10 portfolios 
weights.tb <- rbind(WholeSample, INF1GRW1_6.5, INF2GRW1_6.5, INF1GRW2_6.5, INF2GRW2_6.5, Static_Port, Static1_Port, Static2_Port, Static3_Port, Static4_Port)
weights.tb <- as.matrix(weights.tb)

#need to get a table of mean returns for each of the 5 regimes 
mean.tb <- rbind(mean_table, mean_table1, mean_table2, mean_table3, mean_table4)
rownames(mean.tb) <- c("WholeSample","INF1GRW1_6.5","INF2GRW1_6.5","INF1GRW2_6.5","INF2GRW2_6.5")
Meanriskfree <- t(t(c(mean_rf, mean_rf1, mean_rf2, mean_rf3, mean_rf4)))
mean.tb <- cbind(mean.tb, Meanriskfree)
colnames(mean.tb) <- c("Stocks","Bonds","Gold","Commodities","RiskFree")
mean.tb <- as.matrix(mean.tb)

#VCV matrix list
RiskyAsset <- dt[,6:10]
VCV0 <- matrix(c(cov(RiskyAsset)), nrow = 5, ncol = 5) 
range.names <- c('Stocks','Bonds','Gold','Commodities','RiskFree')
dimnames(VCV0) <- list(range.names, range.names)

RiskyAsset <- dt1[,6:10]
VCV1 <- matrix(c(cov(RiskyAsset)), nrow = 5, ncol = 5) 
range.names <- c('Stocks','Bonds','Gold','Commodities','RiskFree')
dimnames(VCV1) <- list(range.names, range.names)

RiskyAsset <- dt2[,6:10]
VCV2 <- matrix(c(cov(RiskyAsset)), nrow = 5, ncol = 5) 
range.names <- c('Stocks','Bonds','Gold','Commodities','RiskFree')
dimnames(VCV2) <- list(range.names, range.names)

RiskyAsset <- dt3[,6:10]
VCV3 <- matrix(c(cov(RiskyAsset)), nrow = 5, ncol = 5) 
range.names <- c('Stocks','Bonds','Gold','Commodities','RiskFree')
dimnames(VCV3) <- list(range.names, range.names)

RiskyAsset <- dt4[,6:10]
VCV4 <- matrix(c(cov(RiskyAsset)), nrow = 5, ncol = 5) 
range.names <- c('Stocks','Bonds','Gold','Commodities','RiskFree')
dimnames(VCV4) <- list(range.names, range.names)

VCV.tb <- list(VCV0, VCV1, VCV2, VCV3, VCV4)

#calculate the sharpe ratios 
Final_table <- data.frame(as.matrix(0, nrow = 10, ncol = 5))
for (i in 1:10) {
  for (j in 1:5) {
    Final_table[i,j] <- (sum(weights.tb[i,]*mean.tb[j,])-mean.tb[j,5]) / sqrt(weights.tb[i,] %*% VCV.tb[[j]] %*% t(t(weights.tb[i,])))
  }
}

colnames(Final_table) <- c("WholeSample","INF1GRW1","INF2GRW1","INF1GRW2","INF2GRW2")
rownames(Final_table) <- c("WholeSample","INF1GRW1_6.5","INF2GRW1_6.5","INF1GRW2_6.5","INF2GRW2_6.5","Static","Tilt_INF1GRW1","tilt_INF2GRW1","tilt_INF1GRW2","tilt_INF2GRW2")
Final_table
View(Final_table)


barplot(Static1_Port, main= "Optimal Portfolio Allocation under Low Inflation & Low Growth")
barplot(Static4_Port,  main= "Optimal Portfolio Allocation under High Inflation & High Growth")


################
#Clear workspace 
rm(list = ls())
View(dt)

### Question 1 
library(tseries)
library(propagate)
library(IntroCompFinR)
library(tidyverse)
library(dplyr)

colnames(dt)
#Whole, "INF1GRW1","INF2GRW1", "INF1GRW2","INF2GRW2"

###### Whole sample 
sapply(dt, class)
colnames(dt[,6:10]) # assets 

#Averages and SD of assets returns 
means <- colMeans(dt[,6:10], na.rm = TRUE)
sds <- sapply(dt[,6:10], sd, na.rm = TRUE)

#Sharpe ratio 
sharpes <- matrix(1:5, nrow=5, byrow = TRUE )
for (i in 1:5){
  sharpes[i] <- (means[i] - means[5]) / sds[i]
}
sharpes

### 
subset1 <- dt %>%
  filter(INF1GRW1 == 1)

subset2 <- dt %>%
  filter(INF2GRW1 == 1)

subset3 <- dt %>%
  filter(INF1GRW2 == 1)

subset4 <- dt %>%
  filter(INF2GRW2 == 1)

###### 
# SUBSET 1 
#Averages and SD of assets returns 
means1 <- colMeans(subset1[,6:10], na.rm = TRUE)
sds1 <- sapply(subset1[,6:10], sd, na.rm = TRUE)

sharpes1 <- matrix(1:5, nrow=5, byrow = TRUE )
for (i in 1:5){
  sharpes1[i] <- (means1[i] - means1[5]) / sds1[i]
}
### Subset 2 
means2 <- colMeans(subset2[,6:10], na.rm = TRUE)
sds2 <- sapply(subset2[,6:10], sd, na.rm = TRUE)

sharpes2 <- matrix(1:5, nrow=5, byrow = TRUE )
for (i in 1:5){
  sharpes2[i] <- (means2[i] - means2[5]) / sds2[i]
}
#### Subset 3 
means3 <- colMeans(subset3[,6:10], na.rm = TRUE)
sds3 <- sapply(subset3[,6:10], sd, na.rm = TRUE)

sharpes3 <- matrix(1:5, nrow=5, byrow = TRUE )
for (i in 1:5){
  sharpes3[i] <- (means3[i] - means3[5]) / sds3[i]
}

#### Subset 4
means4 <- colMeans(subset4[,6:10], na.rm = TRUE)
sds4 <- sapply(subset4[,6:10], sd, na.rm = TRUE)

sharpes4 <- matrix(1:5, nrow=5, byrow = TRUE )
for (i in 1:5){
  sharpes4[i] <- (means4[i] - means4[5]) / sds4[i]
}
#Variance covariance 
matrix_full <- as.matrix(dt[,6:10])
corr_full <- cor(matrix_full)
matrix_1 <- as.matrix(subset1[,6:10])
corr_1 <- cor(matrix_1)
matrix_2 <- as.matrix(subset2[,6:10])
corr_2 <- cor(matrix_2)
matrix_3 <- as.matrix(subset3[,6:10])
corr_3 <- cor(matrix_3)
matrix_4 <- as.matrix(subset4[,6:10])
corr_4 <- cor(matrix_4)

cov_full <- cor2cov(corr_full, sds^2)
cov_1 <- cor2cov(corr_1, sds1^2)
cov_2 <- cor2cov(corr_2, sds2^2)
cov_3 <- cor2cov(corr_3, sds3^2)
cov_4 <-cor2cov(corr_4, sds4^2)

cov_1

### Part C 
## Full sample 
fullmean <- means[1:4]
fullcov <- cov_full[1:4,1:4]
efficent_full = tangency.portfolio(fullmean, fullcov, means[5], shorts=TRUE) 
plot(efficent_full)
efficent_full


#Subsample 1 
sub1mean <- means1[1:4]
sub1cov <- cov_1[1:4,1:4]
efficent_1 = tangency.portfolio(sub1mean, sub1cov, means1[5], shorts=TRUE) 
plot(efficent_1)
efficent_1

#Subsample 2
sub2mean <- means2[1:4]
sub2cov <- cov_2[1:4,1:4]
efficent_2 = tangency.portfolio(sub2mean, sub2cov, means2[5], shorts=TRUE) 
plot(efficent_2)

#Subsample 3
sub3mean <- means3[1:4]
sub3cov <- cov_3[1:4,1:4]
efficent_3 = tangency.portfolio(sub3mean, sub3cov, means3[5], shorts=TRUE) 
plot(efficent_3)

#Subsample 4
sub4mean <- means4[1:4]
sub4cov <- cov_4[1:4,1:4]
efficent_4 = tangency.portfolio(sub4mean, sub4cov, means4[5], shorts=TRUE) 
plot(efficent_4)

plot(efficent_1)
plot(efficent_2)
plot(efficent_3)
plot(efficent_4)


######## Global minimum variance  Portfolio 
min_full <- globalMin.portfolio(fullmean, fullcov, shorts=TRUE)
min_1 <- globalMin.portfolio(sub1mean, sub1cov, shorts=TRUE)
min_2 <- globalMin.portfolio(sub2mean, sub2cov, shorts=TRUE)
min_3 <- globalMin.portfolio(sub3mean, sub3cov, shorts=TRUE)
min_4 <- globalMin.portfolio(sub4mean, sub4cov, shorts=TRUE)

########### QUESTION    1e
A <- c(1.3,2.8,6.5,10.5,16.9)

weights <- (efficent_full$er - means[5])/ ((efficent_full$sd)^2 * A)
weights

## Question 2 

aversion <- 6.5 
weight_full <-  (efficent_full$er - means[5]) / ((efficent_full$sd)^2 * 6.5)
weight_1 <- (efficent_1$er - means1[5]) / ((efficent_1$sd)^2 *6.5 )
weight_2 <- (efficent_2$er - means2[5]) / ((efficent_2$sd)^2 *6.5 )
weight_3 <- (efficent_3$er - means3[5]) / ((efficent_3$sd)^2 *6.5 )
weight_4 <- (efficent_4$er - means4[5]) / ((efficent_4$sd)^2 *6.5 ) #  ??
weights <- c(weight_full,weight_1,weight_2,weight_3,weight_4)
weights

#for all assets 
means # means 1-4 , the means 
sds # sds1-4 , standard deviations 
as.vector(sharpes)[1:4] # sharpes1-4 , sharpe ratios
efficent_full$weights
efficent_full
weight_full

sapply(sharpes, class)

#full table 
table <- rbind(means[1:4], sds[1:4], as.vector(sharpes)[1:4], efficent_full$weights)
table_1 <- rbind(means1[1:4], sds1[1:4], as.vector(sharpes1)[1:4], efficent_1$weights)
table_2 <- rbind(means2[1:4], sds2[1:4], as.vector(sharpes2)[1:4], efficent_2$weights)
table_3 <- rbind(means3[1:4], sds3[1:4], as.vector(sharpes3)[1:4], efficent_3$weights)
table_4 <- rbind(means4[1:4], sds4[1:4], as.vector(sharpes4)[1:4], efficent_4$weights)

tables <- c(table,table_1, table_2, table_3, table_4)


table_1[4,]  #traditional weights 
weights[1]  # Weight in tangent 
full_weights <- table[4,]*weights[1]   
weights_1 <- table_1[4,] * weights[2]   
weights_2 <- table_2[4,] * weights[3]  
weights_3 <- table_3[4,] * weights[4]  
weights_4 <- table_4[4,] * weights[5]  
full_weights

weights
weights 
table <- rbind(table, full_weights)
table_1 <- rbind(table_1, weights_1)
table_2 <- rbind(table_2, weights_2)
table_3 <- rbind(table_3, weights_3)
table_4 <- rbind(table_4, weights_4)

rownames(table_4) <- c('Mean Monthly Return', 'Monthly Standard deviation of Returns', 
                       'Monthly Sharpe Ratio','Weights in Optimal Portfolio', 'Weights in Final Portfolio')
# repeeat for all tables ^^^^ 
weights 
table_2
table_2

write.table(table_4, 'table_4.csv', sep=',')



