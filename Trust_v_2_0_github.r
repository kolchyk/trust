library(quantmod)
library(xts)
library(zoo)
library(TTR)
library(tidyverse)
library(corrplot)
library(DSTrading)
library(dplyr)
library(chron)
library(roll)
library(caret)
library(MASS) # Boxcox function
library(car) # qqPlot function
library(h2o)
library(magrittr)
library(stats)
#### рабочий проект
setwd("C:\\Cloud\\RR")

#-------capping_outliers-------------------------------  https://www.mql5.com/ru/articles/3486
capping_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  caps <- quantile(x, probs = c(.25,.75),na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- caps[1] 
  y[x > (qnt[2] + H)] <- caps[2] 
  y
}
 
Euro <- read.csv("EURUSD_H4_200001030000_201905032000.csv", header = TRUE, sep = "")
Euro$date<-paste(Euro$X.DATE., "",Euro$X.TIME.)
Euro <- Euro[-c(1,2,3,7,8,9)]
colnames(Euro) <- c("High", "Low", "Close","date")
Euro<-Euro[,c(4, 1, 2,3)]
Euro <- Euro[-c(2,3)]
Euro<-Euro[1:30106,]
n <- length((Euro$Close))


##### days
Euro_days<-filter(Euro,grepl('00:00:00',Euro$date))
Euro_days$date <- as.character(Euro_days$date)
Euro_days$date<- as.POSIXct(as.character(Euro_days$date), format = '%Y.%m.%d %H:%M:%S') 
Euro_days<-xts(Euro_days[,2:2],order.by =as.POSIXct(Euro_days$date),format = '%Y.%m.%d %H:%M:%S')
colnames(Euro_days) <- c( "Close")

x1<-KAMA(Euro_days, nER = 2, nFast = 2, nSlow = 20,priceMethod = "Close")
Euro_days$D_KAMA_fast<-x1$KAMA

x1<-KAMA(Euro_days, nER = 10, nFast = 2, nSlow = 30,priceMethod = "Close")
Euro_days$D_KAMA_slow<-x1$KAMA

Euro_days<- data.frame(date=index(Euro_days), coredata(Euro_days))  
Euro_days$date <- as.character(Euro_days$date)

Euro_days$date<-(paste(as.character(Euro_days$date), "00:00:00"))


#########
Euro$date <- as.character(Euro$date)
Euro$date<- as.POSIXct(as.character(Euro$date), tz = "GMT", format = '%Y.%m.%d %H:%M:%S') 
Euro<-xts(Euro[,2:2],order.by =as.POSIXct(Euro$date), format = '%Y.%m.%d %H:%M:%S')
colnames(Euro) <- c( "Close")
x2<-KAMA(Euro, nER = 2, nFast = 2, nSlow = 20,priceMethod = "Close")
Euro$H_KAMA_fast<-x2$KAMA

x1<-KAMA(Euro, nER = 10, nFast = 2, nSlow = 30,priceMethod = "Close")
Euro$H_KAMA_slow<-x1$KAMA

Euro<- data.frame(date=index(Euro), coredata(Euro))  
Euro$date <- as.character(Euro$date)

###### ????????????
Euro_all <- merge(Euro_days,Euro, by= "date", all = TRUE)
rm(Euro,Euro_days,x1,x2)

colnames(Euro_all) <- c("date", "Close_day", "KAMA_day","D_KAMA_slow", "Close_4hour","KAMA_4hour","H_KAMA_slow")

Euro_all$Close_day<-na.approx(Euro_all$Close_day)
Euro_all$KAMA_day<-na.approx(Euro_all$KAMA_day)
Euro_all$D_KAMA_slow<-na.approx(Euro_all$D_KAMA_slow)


plot((Euro_all$Close_day[2000:3700]), col = "black", type = "l",lwd = 1)
lines((Euro_all$KAMA_day[2000:3700]), col = "blue", type = "l",lwd = 2)
lines((Euro_all$D_KAMA_slow[2000:3700]), col = "red", type = "l",lwd = 1)
lines((Euro_all$KAMA_4hour[2000:2700]), col = "green", type = "l",lwd = 2)

############ ??????????? ??????? ?????????? ??????? ? ????
Euro_all$D_dif_KAMA <-capping_outliers((c(0,(diff(Euro_all$KAMA_day)))))
Euro_all$H_dif_KAMA <-capping_outliers((c(0,(diff(Euro_all$KAMA_4hour)))))

Euro_all$D_dif_KAMA_slow <-capping_outliers((c(0,(diff(Euro_all$D_KAMA_slow)))))
Euro_all$H_dif_KAMA_slow <-capping_outliers((c(0,(diff(Euro_all$H_KAMA_slow)))))

Euro_all$D_dif_KAMA_lag1<-lag(Euro_all$D_dif_KAMA,1)
Euro_all$H_dif_KAMA_lag1<-lag(Euro_all$H_dif_KAMA,1)

Euro_all$D_dif_KAMA_lag2<-lag(Euro_all$D_dif_KAMA,2)
Euro_all$H_dif_KAMA_lag2<-lag(Euro_all$H_dif_KAMA,2)

Euro_all$D_dif_KAMA_lag3<-lag(Euro_all$D_dif_KAMA,3)
Euro_all$H_dif_KAMA_lag3<-lag(Euro_all$H_dif_KAMA,3)

##################### ??????????? ?????????? RSI,???????? ? ?? ????
n1<-7
n2<-14
n3<-21

Euro_all$D_ind_rsi1 <- RSI(Euro_all$KAMA_day, n1)
Euro_all$H_ind_rsi1 <- RSI(Euro_all$KAMA_4hour, n1)

Euro_all$D_moment_10 <- momentum(Euro_all$KAMA_day, n1)  
Euro_all$H_moment_10 <- momentum(Euro_all$KAMA_4hour, n1)  

Euro_all$D_moment_20 <- momentum(Euro_all$KAMA_day, n2)  
Euro_all$H_moment_20 <- momentum(Euro_all$KAMA_4hour, n2)  

Euro_all$D_moment_10_lag1<-lag(Euro_all$D_moment_10,1)
Euro_all$D_moment_10_lag2<-lag(Euro_all$D_moment_10,2)

Euro_all$H_moment_10_lag1<-lag(Euro_all$H_moment_10,1)
Euro_all$H_moment_10_lag2<-lag(Euro_all$H_moment_10,2)


#?????? ?????????? ???????
Euro_all$D_ind_sma_n1<- EMA(Euro_all$KAMA_day, n1, wilder = FALSE)
Euro_all$H_ind_sma_n1<- EMA(Euro_all$KAMA_4hour, n1, wilder = FALSE)

Euro_all$D_ind_sma_n2<- EMA(Euro_all$KAMA_day, n2, wilder = FALSE)
Euro_all$H_ind_sma_n2<- EMA(Euro_all$KAMA_4hour, n2, wilder = FALSE)

Euro_all$D_ind_sma_n3<- EMA(Euro_all$KAMA_day, n3, wilder = FALSE)
Euro_all$H_ind_sma_n3<- EMA(Euro_all$KAMA_4hour, n3, wilder = FALSE)

Euro_all$D_KAMAvsn1<-Euro_all$KAMA_day-Euro_all$D_ind_sma_n1
Euro_all$H_KAMAvsn1<-Euro_all$KAMA_4hour-Euro_all$H_ind_sma_n1

Euro_all$D_KAMAvsn2<-Euro_all$KAMA_day-Euro_all$D_ind_sma_n2
Euro_all$H_KAMAvsn2<-Euro_all$KAMA_4hour-Euro_all$H_ind_sma_n2

Euro_all$D_KAMAvsn1_lag1<-lag(Euro_all$D_KAMAvsn1,1)
Euro_all$H_KAMAvsn1_lag1<-lag(Euro_all$H_KAMAvsn1,1)

stoch<-stoch(Euro_all[,c("KAMA_day","KAMA_day","KAMA_day")], nFastK = 3, nFastD = 9, nSlowD = 2,  bounded = TRUE,
              smooth = 1) %>% as.data.frame()
Euro_all$D_Stochastik<-stoch$slowD

stoch<-stoch(Euro_all[,c("KAMA_4hour","KAMA_4hour","KAMA_4hour")], nFastK = 3, nFastD = 9, nSlowD = 2,  bounded = TRUE,
             smooth = 1) %>% as.data.frame()
Euro_all$H_Stochastik<-stoch$slowD

Euro_all$D_CCI_n1<-CCI(Euro_all[,c("KAMA_day","KAMA_day","KAMA_day")], n = n1, c = 0.015)
Euro_all$H_CCI_n1<-CCI(Euro_all[,c("KAMA_4hour","KAMA_4hour","KAMA_4hour")], n = n1, c = 0.015)

Euro_all$D_CCI_n2<-CCI(Euro_all[,c("KAMA_day","KAMA_day","KAMA_day")], n = n2, c = 0.015)
Euro_all$H_CCI_n2<-CCI(Euro_all[,c("KAMA_4hour","KAMA_4hour","KAMA_4hour")], n = n2, c = 0.015)

Euro_all$D_CCI_n1_lag1<-lag(Euro_all$D_CCI_n1,1)
Euro_all$H_CCI_n1_lag1<-lag(Euro_all$H_CCI_n1,1)

PC16<-as.data.frame(DonchianChannel(Euro_all$KAMA_day, n1, include.lag = FALSE))
Euro_all$D_PC16<-(Euro_all$KAMA_day-PC16$low)/(PC16$high-PC16$low)
 
PC16<-as.data.frame(DonchianChannel(Euro_all$KAMA_4hour, n1, include.lag = FALSE))
Euro_all$H_PC16<-(Euro_all$KAMA_4hour-PC16$low)/(PC16$high-PC16$low)


#######################
Euro_all$ind_zig_zag5<- ZigZag(Euro_all$KAMA_4hour,change = 0.003,percent = FALSE, retrace = FALSE, lastExtreme = TRUE)
#Euro_all$ind_zig_zag10<-ZigZag(Euro_all$KAMA_4hour,change = 0.01,percent = FALSE, retrace = FALSE, lastExtreme = TRUE)

Euro_all$Sign_5<-sign(Euro_all$ind_zig_zag5-lag(Euro_all$ind_zig_zag5)) %>% as.numeric()
table(Euro_all$Sign_5)
#Euro_all$Sign_10<-sign(Euro_all$ind_zig_zag10-lag(Euro_all$ind_zig_zag10)) %>% as.numeric()

Euro_all$Target<-((Euro_all$Sign_5+sign(Euro_all$D_dif_KAMA_slow))/2)
table(Euro_all$Target)

#Euro_all$Target<-(Euro_all$Sign_5+Euro_all$Sign_10)/2 
Euro_all$Target<- as.factor(Euro_all$Target)
#Euro_all$Target<-sign(Euro_all$ind_zig_zag5-lag(Euro_all$ind_zig_zag5)) %>% as.numeric()  %>%sign() %>% as.factor()
#Euro$Target<-lead(Euro$Target,0) 


##########################
Euro_all<-na.omit(Euro_all)

diff <- c(0,diff(Euro_all$Close_4hour))
diff_sign<-lag(as.numeric(as.character(Euro_all$Target)),n=1)
diff_sign[1:2] <- 0 
equity<-cumsum(diff_sign*diff)
plot(equity[23500:24000], col = "black", type = "l")
tail(na.omit(equity), n=1)
mean(diff(na.omit(equity)))

#############
par(mfrow=c(1,1))
plot((Euro_all$Close_4hour[2000:2300]), col = "black", type = "l",lwd = 1)
lines((Euro_all$KAMA_4hour[2000:2300]), col = "blue", type = "l",lwd = 2)
lines((Euro_all$ind_zig_zag5[2000:2300]), col = "red", type = "l",lwd = 2)
lines((Euro_all$ind_zig_zag10[2000:2300]), col = "green", type = "l",lwd = 2)

par(new = TRUE)
plot((as.numeric(as.character(Euro_all$Target[2000:2300]))), type = "l",col = "red",lwd = 2)
lines((Euro$run_Mediana_diff[1000:1500]), col = "red", type = "l",lwd = 2)

par(new = TRUE)
plot((Euro$PC16[2000:2300]), type = "l", col = "green",lwd = 2)
lines((Euro$fastK[2000:2300]), col = "red", type = "l",lwd = 2)

grid()

##################################
Euro_all<-na.omit(Euro_all)
str(Euro_all)

#######################
drops <- c( "D_ind_sma_n1","H_ind_sma_n1","D_ind_sma_n2","H_ind_sma_n2","D_ind_sma_n3","H_ind_sma_n3",
            "PC_16_ch_C_diff","ind_zig_zag10","ind_zig_zag5","Sign_5","Sign_10")
Euro_all<-Euro_all[ , !(names(Euro_all) %in% drops)]

rm("diff","diff_sign","equity","n","n1","n2","n3","PC16","stoch","drops","capping_outliers")

Euro_all<-na.omit(Euro_all) %>% as.data.frame()
str(Euro_all)
###########################

#preProClean <- preProcess(x = Euro,method = c("BoxCox","center","scale","corr") ) #PCA ???????????
#preProClean <- preProcess(x = Euro,method = c("BoxCox","center","scale","corr") ) #PCA ???????????
#Euro1<- predict(preProClean, Euro ) %>% na.omit
 
h2o.init(nthreads = -1,max_mem_size = "4g")

train<-as.h2o(Euro_all[1000:27000,])
test<-as.h2o(Euro_all[25001:NROW(Euro_all),])

y <- "Target"
x <- setdiff(names(train), c("date","ind_zig_zag5","Sign_5","Close_day","KAMA_day",
                             "Close_4hour","KAMA_4hour","D_KAMA_slow","H_KAMA_slow",y))
             
aml <- h2o.automl(x = x,y = y,training_frame = train,
                  max_runtime_secs = 900,
                  #balance_classes=TRUE,
                  #validation_frame = test,
                  nfolds = 5,
                  stopping_rounds=2,
                  stopping_tolerance = 0.001,
                  stopping_metric = c( "mean_per_class_error"),
                  leaderboard_frame=test,
                  sort_metric = c("mean_per_class_error"),
                  include_algos = c("GBM"),
                  verbosity="debug")

lb <- aml@leaderboard
print(lb, n = nrow(lb))   
aml@leader

imp1<-h2o.varimp(h2o.getModel(aml@leaderboard[1, 1]))
h2o.varimp_plot(h2o.getModel(aml@leaderboard[1, 1]),40)


########################### Test model ##############
pred <- h2o.predict(h2o.getModel(aml@leaderboard[1, 1]), test) %>% as.data.frame()
test0<-as.data.frame(test)
itog<-cbind(test0,pred) %>% as.data.frame()

itog$diff <- c(0,diff(itog$Close_4hour))
itog$predict_l1<- lag(as.numeric(as.character(itog$predict)),n=1)
itog$predict_l1[1] <- 0 
itog$equity<-cumsum(itog$predict_l1*itog$diff)
plot(itog$equity, col = "black", type = "l")
tail(na.omit(itog$equity), n=1)
mean(diff(na.omit(itog$equity)))

###########################
par(mfrow=c(1,1))
plot(itog$Close_4hour[1000:1200], col = "black", type = "l",lwd = 2)
lines(itog$KAMA_4hour[1000:1200], col = "red", type = "b",lwd = 2)
lines(itog$run_Mediana[1000:1200], col = "green", type = "b",lwd = 2)
lines(itog$ind_sma_n3[1000:1200], col = "blue", type = "b",lwd = 2)

par(new = TRUE)
plot((as.numeric(as.character(itog$Target)))[1000:1200], type = "b", axes = FALSE, bty = "n", xlab = "", ylab = "",col = "blue",lwd = 2)
lines(as.numeric(as.character(itog$predict))[1000:1200], col = "green", type = "b",lwd = 1)

par(new = TRUE)
plot((as.numeric(as.character(itog$equity)))[1000:1200], type = "b", axes = FALSE, bty = "n", xlab = "", ylab = "",col = "violet",lwd = 2)
grid()


#h2o.shutdown(prompt = FALSE)

imp3<-h2o.varimp(h2o.getModel(aml@leaderboard[1, 1]))
write.table(imp3, file = "imp3.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")

h2o.saveModel(aml@leader, path=getwd(), force = TRUE)
h2o.saveModel(h2o.getModel(aml@leaderboard[2, 1]), path=getwd(), force = TRUE)

library(xlsx)
write.xlsx(itog, file = "AUTO_ML.xlsx",row.names=FALSE, col.names=TRUE)


getwd()


################################
require(ggvis)

Euro_all  %>% ggvis(~CCI_n1  , fill = ~Target) %>% 
  group_by(Target) %>%  layer_densities() %>% 
  add_legend("fill", title = "factor")

table(Euro_all$Target)
str(Euro_all$CCI_n1)

###############################################
Euro_all %>%  keep(is.numeric) %>%   gather() %>%   ggplot(aes(value)) +  facet_wrap(~ key, scales = "free") +  geom_histogram()

########### ???? ?????????? ###################
Euro_all$Target<-as.numeric(as.character(Euro_all$Target))
x2<-which(sapply(Euro_all,is.numeric))
correlationMatrix <- cor(Euro_all[x2])
highlyCorrelated <- as.data.frame(findCorrelation(correlationMatrix, cutoff=0.9,verbose = TRUE, names = FALSE))
colnames(highlyCorrelated) <- c("Var")

Euro_highlyCorrelated<-Euro_all[,(highlyCorrelated$Var)]
print(highlyCorrelated)
corrplot(correlationMatrix,type = c("lower"),method = c( "number"),         addrect = 5)



hist((Euro$rollsd))
skewness(Euro$rollsd) 
kurtosis(Euro$rollsd) 
boxplot((Euro$rollsd))
qqPlot(Euro$rollsd)

