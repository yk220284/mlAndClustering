library(tidyverse)
library(lubridate)
library(pa)
library(keras)

inputTable <- function() {read.csv(file.choose(), stringsAsFactors = TRUE)}

#Basic data prepration
Account_Data <- inputTable()
Account_Data$Valid_User <- as.factor(Account_Data$Valid_User)
Account_Data$Sex <- as.factor(Account_Data$Sex)
Account_Data$DoB <- as.Date(Account_Data$DoB)
Account_Data <- mutate(Account_Data, Ri = 0 , Rl = 0 , Rs = 0 , Ra = 0)

#Brinson Modelling prepration
benchmark <- inputTable()
benchmark$Security_Code <- str_c(str_dup("0",6-str_count(benchmark$Security_Code)),benchmark$Security_Code)
benchmark$Security_Code <- as.factor(benchmark$Security_Code)
benchmark$date <- ymd(benchmark$date)

Account_Holding <- inputTable()
Account_Holding$Security_Code <- str_c(str_dup("0",6-str_count(Account_Holding$Security_Code)),Account_Holding$Security_Code)
Account_Holding$Security_Code <- as.factor(Account_Holding$Security_Code)
Acc_unique <- unique(Account_Holding$Account)

#Generating Ri\Rl\Rs\Ra
for(i in 1:length(Acc_unique)){
  Account_Holding_Sub <- Account_Holding[Account_Holding$Account==Acc_unique[i],]
  Account_Holding_Sub <- mutate(Account_Holding_Sub, portfolio = Security_Holding/sum(Security_Holding))
  Account_Holding_Sub_Portfolio <- select(Account_Holding_Sub,Security_Code,portfolio)
  data_brinson <- left_join(benchmark,Account_Holding_Sub_Portfolio, by = "Security_Code")
  data_brinson$portfolio <- replace_na(data_brinson$portfolio,0)
  data_brinson$Security_Code <- as.factor(data_brinson$Security_Code)
  
  br <- brinson(x=data_brinson , date.var = "date" , cat.var = "sector" , bench.weight = "benchmark" , portfolio.weight = "portfolio" , ret.var = "return")
  Account_Data$Rl[Account_Data$Account==Acc_unique[i]] <- returns(br)$Aggregate[1]
  Account_Data$Rs[Account_Data$Account==Acc_unique[i]] <- returns(br)$Aggregate[2]
  Account_Data$Ri[Account_Data$Account==Acc_unique[i]] <- returns(br)$Aggregate[3]
  Account_Data$Ra[Account_Data$Account==Acc_unique[i]] <- returns(br)$Aggregate[4]
}


#Generating recent mean&frq
Recent_activity <- read.csv(choose.files(),stringsAsFactors = FALSE)

Recent_activity$Date <- ymd(Recent_activity$Date)
Recent_activity_by_account <- group_by(Recent_activity,Account)
Recent_activity_mean <- summarise(Recent_activity_by_account, amount_mean = mean(Amount), Recent_frq = n()/60)

Recent_activity_month <- Recent_activity_by_account[Recent_activity_by_account$Date>="2018-6-9",]
Recent_activity_mean_month <- summarise(Recent_activity_month, amount_mean_month = mean(Amount), Recent_frq_month = n()/30)

Recent_activity_week <- Recent_activity_by_account[Recent_activity_by_account$Date>="2018-7-3",]
Recent_activity_mean_week <- summarise(Recent_activity_week, amount_mean_week = mean(Amount), Recent_frq_week = n()/7)

data_ready <- left_join(Account_Data,Recent_activity_mean , by = "Account")
data_ready <- left_join(data_ready,Recent_activity_mean_month, by ="Account")
data_ready <- left_join(data_ready,Recent_activity_mean_week,by = "Account")

#Target selection
Target_xianjinbao <- inputTable()
Target_rzrq <- inputTable()

data_ready_target <- left_join(data_ready,Target_rzrq, by = "Account")
data_ready_target <- left_join(data_ready_target,Target_xianjinbao , by = "Account")


#NA remove
sapply(data_ready_target,function(x) sum(is.na(x)))
na_replace_list <- list(amount_mean=0,Recent_frq=0,amount_mean_month=0,amount_mean_week=0,Recent_frq_month=0,Recent_frq_week=0,Target_rzrq=0,Target_xianjinbao=0)
data_ready_target <- replace_na(data_ready_target,na_replace_list)

#ouput data_ready_target
write.csv(data_ready_target,"data_ready.csv")


#Training & testing data set
data_ready_target <- as.matrix(data_ready_target)
dimnames(data_ready_target) <- NULL
data_ready_target[,1:24] <- normalize(data_ready_target[,1:24])
data_ready_target[,25] <- data_ready_target[,25]

x_train <- data_ready_target[,1:24]
y_train <- data_ready_target[,25]

y_train <- to_categorical(y_train, 2)


##model fitting
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(94)) %>% 
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

#compile the model with appropriate loss function, optimizer, and metrics:
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

#Training and Evaluation

history <- model %>% fit(
  x_train, 
  y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
#Use the fit() function to train the model for 30 epochs using batches of 128 images:



model %>% evaluate(x_test, y_test)

pred <- model %>% predict_classes(x_train)

data_ready_target <- mutate(data_ready_target,Asset1=if_else(Asset_Total==0,1,0))
data_ready_target <- mutate(data_ready_target,Asset2=if_else(Asset_Total>0&Asset_Total<=100,1,0))
data_ready_target <- mutate(data_ready_target,Asset3=if_else(Asset_Total>100&Asset_Total<=10000,1,0))
data_ready_target <- mutate(data_ready_target,Asset4=if_else(Asset_Total>10000&Asset_Total<=100000,1,0))
data_ready_target <- mutate(data_ready_target,Asset5=if_else(Asset_Total>100000,1,0))

y_train <- data_ready_target$Target_xianjinbao
x_train <- select(data_ready_target,-Target_xianjinbao)

x_train <- as.matrix(x_train)
dimnames(x_train) <- NULL
y_train <- as.matrix(y_train)
x_train <- normalize(x_train)


#test random data
random_index <- (1:300000)
random_index = sample(size = 100000,random_index)
y_test <- y_train[random_index]
x_test <- data_ready[random_index,]
x_test <- select(x_test,-Target_xianjinbao)
x_test <- as.matrix(x_test)
x_test <- x_test[,-1]
dimnames(x_test) <- NULL
x_test <- normalize(x_test)





model %>% evaluate(x_test, y_test)
pred <- model %>% predict_classes(x_test)


