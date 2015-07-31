### Kaggle Scripts: Ponpare Coupon Purchase Prediction ###


#read in all the input data
cpdtr <- read.csv("coupon_purchased_train_en.csv")
cpltr <- read.csv("coupon_list_train_en.csv")
cplte <- read.csv("coupon_list_test_en.csv")
ulist <- read.csv("user_list_en.csv")

cpdtr=cpdtr[,-1]
cpltr=cpltr[,-1]
cplte=cplte[,-1]
ulist=ulist[,-1]



#making of the train set
train <- merge(cpdtr,cpltr,by="COUPON_ID_hash")

train <- train[,c("COUPON_ID_hash","USER_ID_hash","ITEM_COUNT",
                  "CAPSULE_NAME","GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE", "VALIDPERIOD",
                  "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                  "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                  "USABLE_DATE_BEFORE_HOLIDAY","ken_name","small_area_name")]
#combine the test set with the train
cplte$USER_ID_hash <- "dummyuser"

#Replicate rows based on item_count
train=replicate(train)
train=train[,-3]

cpchar <- cplte[,c("COUPON_ID_hash","USER_ID_hash",
                   "CAPSULE_NAME","GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE", "VALIDPERIOD",
                   "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                   "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                   "USABLE_DATE_BEFORE_HOLIDAY","ken_name","small_area_name")]

train <- rbind(train,cpchar)
#NA imputation
train[is.na(train)] <- 1

#feature engineering
train$DISCOUNT_PRICE <- 1/ log10(train$DISCOUNT_PRICE) #(train$DISCOUNT_PRICE - min(train$DISCOUNT_PRICE))/(max(train$DISCOUNT_PRICE) - min(train$DISCOUNT_PRICE))
train$PRICE_RATE <- (train$PRICE_RATE - min(train$PRICE_RATE))/(max(train$PRICE_RATE) - min(train$PRICE_RATE))
train$VALIDPERIOD = (train$VALIDPERIOD - min(train$VALIDPERIOD))/(max(train$VALIDPERIOD) - min(train$VALIDPERIOD))

#convert the factors to columns of 0's and 1's
train <- cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                           contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], contrasts, contrasts=FALSE)))

#separate the test from train
test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
train <- train[train$USER_ID_hash!="dummyuser",]

#data frame of user characteristics
uchar <- aggregate(.~USER_ID_hash, data=train[,-1],FUN=mean)

#W <- as.matrix(Diagonal(x=c(rep(2,13), rep(1,1), rep(1,1), rep(1,1), rep(0,9), rep(1,47), rep(0.25,55))))
#funcCosineW(W)



--------------------------
#Weight Matrix: CAPSULE_NAME GENRE_NAME DISCOUNT_PRICE PRICE_RATE VALIDPERIOD USABLE_DATE_ ken_name small_area_name  
W <- as.matrix(Diagonal(x=c(rep(0,24), rep(2,13), rep(1,1), rep(1,1), rep(1,1), rep(0,9), rep(1,47), rep(0.25,55))))

funcCosineW(W)

#Function with input W or wieghtMAtrix / output the users and 10 coupons

funcCosineW = function(weightMat){

  require(Matrix)
  W=as.matrix(weightMat)
  
  #calculation of cosine similairties of users and coupons
  score = as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))
  #order the list of coupons according to similairties and take only first 10 coupons
  uchar$PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
    purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
    return(purchased_cp)
  }))
  
  
  #make submission
  submission <- merge(ulist, uchar, all.x=TRUE)
  submission <- submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
  uchar = uchar[,-153]
  return(submission)
  #write.csv(submission, file="cosine_sim.csv", row.names=FALSE)

}


-------------------------
Function replicate Input a datframe / output dataframe with lines replicated depending on ITEM_COUNT column

replicate = function(dat){
  data.frame(dat[rep(seq_len(dim(dat)[1]), dat$ITEM_COUNT), , drop = FALSE], row.names=NULL)
}
