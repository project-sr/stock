#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■#
#■■■■■■■■■■■■■■■■■■■■ 0. Set environment ■■■■■■■■■■■■■■■■■■■■■#
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■#
#---- 0.1. Install packages in need 
#-- rm(list=ls()); gc(reset=T);
pckgs <- c("ggplot2","data.table", "dplyr", "rpart",'rpart.plot',
           'h2o','randomForest','tidyr','caret','installr',
           'stringr','zoo')
#install.packages(pckgs,dependencies = TRUE)
lapply(pckgs, require, character.only = TRUE)
#updateR()

#--- 0.2. Set working directory and directory path
getwd() #C:/Users/zache/Documents/Study
Dirpath_data <- file.path("C:/Users/zache/Documents/Study/Project_SR/stock/data")
Dirpath_model<- file.path("C:/Users/zache/Documents/Study/Project_SR/stock/model")

#--- 0.3. Read input data
price_00000 <- fread(file = paste0(Dirpath_data,'/ssjj_price.csv'), encoding = 'UTF-8') #testdata : ssjj price
fin.stat_00000 <- fread(file = paste0(Dirpath_data,'/samsung_elec_financial_stat.csv'), encoding = 'UTF-8') #test ssjj
c.rate_00000 <- fread(file = paste0(Dirpath_data,'/currency_rate.csv'), encoding = 'UTF-8') 


#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■#
#■■■■■■■■■■■■■■■■■■■■■■ 1. Data handle ■■■■■■■■■■■■■■■■■■■■■■■#
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■#
#--- 1.1. price data handling
class(price_00000)
price_00000 <- price_00000[,-'V1']
colnames(price_00000) <- c('date', 'volume', 'highP', 'openP', 'lowP' ,'closeP')


# create rangeV of the day (range_high.low, range_open.close)
price_00000 <- price_00000[, `:=`(range_high.low = highP - lowP,
                                  range_open.close = openP - closeP)]

# create avgP/maxP/minP/avgV/maxV/minV in the periods
cut_period <- c(20,60,120,240,720,2400) #1m,1q,2q,1y,3y,10y
#price_00000 <- price_00000[, date := as.Date(as.character(date), format="%Y%m%d")]
for(i in 1:length(cut_period)){
  price_00000 <- price_00000[, (paste0('avgP', '_', cut_period[i])) := 
                               rollapplyr(closeP, width = cut_period[i], FUN = mean, fill = NA, align = 'left')]
  price_00000 <- price_00000[, (paste0('minP', '_', cut_period[i])) := 
                               rollapplyr(closeP, width = cut_period[i], FUN = min, fill = NA, align = 'left')]
  price_00000 <- price_00000[, (paste0('maxP', '_', cut_period[i])) := 
                               rollapplyr(closeP, width = cut_period[i], FUN = max, fill = NA, align = 'left')]
  price_00000 <- price_00000[, (paste0('avgV', '_', cut_period[i])) := 
                               rollapplyr(closeP, width = cut_period[i], FUN = mean, fill = NA, align = 'left')]
  price_00000 <- price_00000[, (paste0('minV', '_', cut_period[i])) := 
                               rollapplyr(closeP, width = cut_period[i], FUN = min, fill = NA, align = 'left')]
  price_00000 <- price_00000[, (paste0('maxV', '_', cut_period[i])) := 
                               rollapplyr(closeP, width = cut_period[i], FUN = max, fill = NA, align = 'left')]
}

colnames(price_00000)


# set cols to shift (lag/lead) ; make Y
cols <- c('closeP')
lagcols <- paste("lag", cols, sep="_")
lag_period <- c(1,5) #1d,1w,1m,1q,2q,1y,3y,10y
for(i in 1:length(lag_period)){
  price_00000 <- price_00000[, (paste0(lagcols,"_",lag_period[i])) := shift(.SD, lag_period[i], NA, "lag"), .SDcols=cols]
}


#--- 1.2. currency rate data handling
class(c.rate_00000)
c.rate_00000 <- c.rate_00000[,-'V1']
colnames(c.rate_00000) <- c('date', 'USD', 'JPY', 'CNY', 'EUR')





#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■#
#■■■■■■■■■■■■■■■■■■■■■■■ 2. Model fit ■■■■■■■■■■■■■■■■■■■■■■■■#
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■#
#--- 2.0. data integrate
# data merge
class(c.rate_00000)
class(price_00000)
setkey(c.rate_00000, date)
setkey(price_00000, date)
stock_00000 <- c.rate_00000[price_00000]

# remove record having NA
nrow(stock_00000)
nrow(na.omit(stock_00000))

stock_00000_test <- na.omit(stock_00000)


# pilot model tetst (RF)

X_names <- setdiff(names(stock_00000_test), c('date', 'volume', 'highP', 'openP', 'lowP', 'closeP', 
                                         'range_high.low', 'range_open.close', 'lag_closeP_1', 'lag_closeP_5'))

stock_00000_test <- stock_00000_test[, Y5 := as.factor(as.numeric(lag_closeP_5/closeP >= 1.04))]

table(stock_00000_test$Y5)
stock_00000_test[, .(Y5, closeP, lag_closeP_5)]








#--- 2.1. fit pilot model (RF)
# test using caret modeling
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")

indexTrain <- createDataPartition(stock_00000_test$Y5, p = .9, list = F)
training <- stock_00000_test[, .SD, .SDcols=c(X_names,'Y5')][ indexTrain, ]
testing  <- stock_00000_test[, .SD, .SDcols=c(X_names,'Y5')][-indexTrain, ]

t1 <- Sys.time()
rf_fit <- train(Y5 ~ ., data = training, method = "rf", trControl = fitControl, verbose = F, tuneLength = 10)
rf_fit
t2 <- Sys.time()
cat('takes : ', difftime(t2,t1,'sec'))


varImp(rf_fit)

predict(rf_fit, newdata = testing)
predict(rf_fit, newdata = testing) %>% confusionMatrix(testing$Y5)

# install.packages("doMC")
library("doMC")







# create diffV of variables (diff_closeP, diff_volume)
price_00000 <- price_00000[, `:=`(diff_closeP = )]


price_00000$전일비 <- c(-diff(price_00000$종가),0)
price_00000$Y <- as.factor(as.numeric(price_00000$전일비>100))

table(price_00000$Y)









A <- price_00000[, .(종가, lead_종가_1)]
A$compare <- as.numeric(A$종가 > A$lead_종가_1*1.04)

cbind(-diff(price_00000$종가), price_00000$전일비)

          # Ref code of shift function
          # return a new data.table instead of updating
          # with names automatically set
          DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
          DT[, shift(.SD, 1:2, NA, "lead", TRUE), .SDcols=2:4]
          
          # lag/lead in the right order
          DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
          DT = DT[sample(nrow(DT))]
          # add lag=1 for columns 'v1,v2,v3' in increasing order of 'year'
          cols = c("v1","v2","v3")
          anscols = paste("lag", cols, sep="_")
          DT[order(year), (cols) := shift(.SD, 1, type="lag"), .SDcols=cols]
          DT[order(year)]
          
          # while grouping
          DT = data.table(year=rep(2010:2011, each=3), v1=1:6)
          DT[, c("lag1", "lag2") := shift(.SD, 1:2), by=year]
          
          # on lists
          ll = list(1:3, letters[4:1], runif(2))
          shift(ll, 1, type="lead")
          shift(ll, 1, type="lead", give.names=TRUE)
          shift(ll, 1:2, type="lead")



#--- 1.2. financial statement data handling
class(fin.stat_00000)
fin.stat_00000 <- as.matrix(fin.stat_00000)
fin.stat_00000[1,1] <- '날짜'
A <- t(fin.stat_00000[,-1])
colnames(A) <- fin.stat_00000[,1]

YYYYMMDD <- as.integer(paste0(substr(A[,1],1,4), substr(A[,1],6,7), 15))
YYYYMM <- as.integer(paste0(substr(A[,1],1,4), substr(A[,1],6,7)))
substr(price_00000$날짜,1,6) %in% YYYYMM

B <- data.table(날짜 = YYYYMM, stat_V1 = 1)


setkey(price_00000, 날짜)
setkey(B, 날짜)

C <- B[price_00000]




as.Date(YYYYMM[1], format="%Y:%m:%d")

paste0(YYYYMM[1],1:9)
str_pad(1:31, pad = 0, width = 2 , "left")


dim(fin.stat_00000)

as.matrix(fin.stat_00000)
t(as.data.frame(fin.stat_00000))
t(fin.stat_00000)



class(fin.stat_00000[2,1])
unlist(fin.stat_00000[2,2])


replace(fin.stat_00000, fin.stat_00000=='', NA)

dim(fin.stat_00000)

fin.stat_00000[15,8]==''

stock_00000

fread()




















## Ref code for create additional variables

stock_data <- list()
train <- list()
test <- list()
tree2 <- list()
tree3 <- list()
rate_pred2 <- c()
rate_pred3 <- c()

## get stock data : quantmod ##
#https://blog.naver.com/jk940/221345104532# quantmod활용
a <- getSymbols(paste0('001790',".KS"), auto.assign = F, env=NULL)
a <- as.data.table(na.omit(a))
colnames(a) <- c("date", "open", "high", "low", "close", "volume", "adj")  
a <- a[volume >0, ]
a <- a[, adj.ratio := adj/close] %>% 
      .[, `:=`(open.adj = open*adj.ratio,
               high.adj = high*adj.ratio,
               low.adj = low*adj.ratio,
               close.adj = close*adj.ratio)] %>% 
      .[,`:=`(height = high.adj - low.adj + low.adj*0.01,
              body = abs(close.adj - open.adj) + close.adj*0.01)] %>% 
      .[, `:=`(candle_ratio = height/body)] %>% 
      select(date, close.adj, candle_ratio)


a <- a[(nrow(a)-19):nrow(a), `:=`(min_1m = min(close.adj),
                                  max_1m = max(close.adj),
                                  avg_1m = mean(close.adj))]

a <- a[(nrow(a)-59):nrow(a), `:=`(min_1q = min(close.adj),
                                  max_1q = max(close.adj),
                                  avg_1q = mean(close.adj))]

a <- a[(nrow(a)-119):nrow(a), `:=`(min_2q = min(close.adj),
                                   max_2q = max(close.adj),
                                   avg_2q = mean(close.adj))]

a <- a[(nrow(a)-239):nrow(a), `:=`(min_1y = min(close.adj),
                                   max_1y = max(close.adj),
                                   avg_1y = mean(close.adj))]

a <- a[(nrow(a)-719):nrow(a), `:=`(min_3y = min(close.adj),
                                   max_3y = max(close.adj),
                                   avg_3y = mean(close.adj))]

plot(a$date, a$close.adj, type='l')
points(a$date, a$avg_3y, type='l', col='red')
plot(a$close.adj, type='l')


## crawling web : rvest ##
#https://blog.naver.com/jk940/221339612873# 크롤링
#https://blog.naver.com/jk940/221341970825# 텍스트 정규식















i <- 2




for(i in 1:length(stock_code)){
  tryCatch({
    stock_data[[i]] <- getSymbols(paste0(stock_code[i],".KS"), auto.assign = F)
    stock_data[[i]] <- as.data.table(na.omit(stock_data[[i]]))
    names(stock_data)[i] <- paste0(stock_code[i],".KS")
    colnames(stock_data[[i]]) <- c("date", "open", "high", "low", "close", "volume", "adj")  
    n_row <- nrow(stock_data[[i]])
    
    max_open_close <- apply(stock_data[[i]][-n_row, .(open, close)], 1, max)
    min_open_close <- apply(stock_data[[i]][-n_row, .(open, close)], 1, min)
  
    train[[i]] <- stock_data[[i]][, Y1 := c(diff(close),NA)] %>% 
                                .[-n_row, `:=`(tail_up = high - max_open_close,
                                               tail_down = low - min_open_close,
                                               candle_box = close - open,
                                               Y2 = Y1/close,
                                               Y3 = ifelse(Y1>0,1,0)
                                                )] %>% 
                                .[-c(n_row-1, n_row)]
    
    test[[i]] <- stock_data[[i]][, Y1 := c(diff(close),NA)] %>% 
                               .[-n_row, `:=`(tail_up = high - max_open_close,
                                              tail_down = low - min_open_close,
                                              candle_box = close - open,
                                              Y2 = Y1/close,
                                              Y3 = ifelse(Y1>0,1,0)
                                                )] %>% 
                               .[c(n_row-1, n_row)]
    
    tree2[[i]] <- rpart(Y2 ~ tail_up + tail_down + candle_box + volume, data=train[[i]], cp=0.005)
    tree3[[i]] <- rpart(Y3 ~ tail_up + tail_down + candle_box + volume, data=train[[i]], cp=0.005)
    rate_pred2[i] <- rpart.predict(tree2[[i]], test[[i]][1,])
    rate_pred3[i] <- rpart.predict(tree3[[i]], test[[i]][1,])
  }, error = function(e){})  
    
  cat(paste0(i, "th is done \n"))
}


for(i in 1:length(stock_code)){
tree3[[i]] <- rpart(Y3 ~ tail_up + tail_down + candle_box + volume, data=train[[i]], cp=0.005)
rate_pred3[i] <- rpart.predict(tree3[[i]], test[[i]][1,])
}


rate_pred2 <- rate_pred[!is.na(rate_pred2)]
rate_pred3 <- rate_pred[!is.na(rate_pred3)]
ind2_top10 <- order(rate_pred2, decreasing = T)[1:10]
ind3_top10 <- order(rate_pred3, decreasing = T)[1:10]

real_Y3 <- c()
for(i in 1:10){
  real_Y3[i] <- test[[ind3_top10[i]]][1,Y2]*100
}

cbind(names(stock_data)[ind3_top10],
      round(rate_pred3[ind3_top10],4)*100,
      real_Y3
      )

test[[400]]






rpart.plot(tree[[which(rate_pred==max(rate_pred))]])

train[[i]][tail_down < -525 & tail_up >= 250 & volume >= 2600000 & volume < 3000000,]






#윗꼬리길이, 아랫꼬리길이, 캔들몸통길이, 거래량  *  최근 5일거래 -> 다음하루 상승여부 예측


i <- 865

stock_data[[i]] <- getSymbols(paste0(stock_code[i],".KS"), env=NULL)
stock_data[[i]] <- as.data.table(na.omit(stock_data[[i]]))
names(stock_data)[i] <- paste0(stock_code[i],".KS")
colnames(stock_data[[i]]) <- c("date", "open", "high", "low", "close", "volume", "adj")  
n_row <- nrow(stock_data[[i]])

max_open_close <- apply(stock_data[[i]][-n_row, .(open, close)], 1, max)
min_open_close <- apply(stock_data[[i]][-n_row, .(open, close)], 1, min)

train[[i]] <- stock_data[[i]][, Y1 := c(diff(close),NA)] %>% 
  .[-n_row, `:=`(tail_up = high - max_open_close,
                 tail_down = low - min_open_close,
                 candle_box = close - open,
                 Y2 = Y1/close,
                 Y3 = ifelse(Y1>0,1,0)
  )] %>% 
  .[-c(n_row-1, n_row)]

test[[i]] <- stock_data[[i]][, Y1 := c(diff(close),NA)] %>% 
  .[-n_row, `:=`(tail_up = high - max_open_close,
                 tail_down = low - min_open_close,
                 candle_box = close - open,
                 Y2 = Y1/close,
                 Y3 = ifelse(Y1>0,1,0)
  )] %>% 
  .[c(n_row-1, n_row)]

tree[[i]] <- rpart(Y2 ~ tail_up + tail_down + candle_box + volume, data=train[[i]], cp=0.005)
rate_pred[i] <- rpart.predict(tree[[i]], test[[i]][1,])


