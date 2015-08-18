### Prepare the features of the users and the test coupons
###
### edit: dir, names of data frames, column index of small_area_name
###
###

# Load all user and coupon data
dir = "data/"
load(paste0(dir, "coupon_purchased_train.RData"))
#cpdtr <- purchases_train    # splits
cpdtr <- coupon_purchased_train    # original
load(paste0(dir, "coupon_list_train.RData"))
#cpltr <- coupons_train    # splits
cpltr <- couple_list_train    # original
#load(paste0(dir, "coupon_list_test.RData"))    # splits
#cplte <- coupons_test
cplte <- read.csv("data/coupon_list_test_en.csv")    # original
#cplte <- coupon_list_test

# Incorporate training coupon features into purchase records
# and select features
train <- merge(cpdtr,cpltr,by="COUPON_ID_hash")
#colnames(train)[6] <- "small_area_name"   # for split1/2/3
colnames(train)[7] <- "small_area_name"   # for original
train <- train[,c("COUPON_ID_hash","USER_ID_hash","ITEM_COUNT",
                  "CAPSULE_NAME","GENRE_NAME","DISCOUNT_PRICE",
                  "PRICE_RATE", "VALIDPERIOD",
                  "USABLE_DATE_MON","USABLE_DATE_TUE",
                  "USABLE_DATE_WED","USABLE_DATE_THU",
                  "USABLE_DATE_FRI","USABLE_DATE_SAT",
                  "USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                  "USABLE_DATE_BEFORE_HOLIDAY","ken_name",
                  "small_area_name")]

# Replicate each record as many times as ITEM_COUNT
replicate = function(dat){
  data.frame(dat[rep(seq_len(dim(dat)[1]), dat$ITEM_COUNT), ,
                 drop = FALSE],
             row.names=NULL)
}
train=replicate(train)
train=train[,-3]

# Attach testing coupon features (for some common feature
# transformations)
cplte$USER_ID_hash <- "dummyuser"
cpchar <- cplte[,c("COUPON_ID_hash","USER_ID_hash",
                   "CAPSULE_NAME","GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE", "VALIDPERIOD",
                   "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                   "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                   "USABLE_DATE_BEFORE_HOLIDAY","ken_name","small_area_name")]
train <- rbind(train,cpchar)

# Missing value imputation
train[is.na(train)] <- 1

# Feature engineering
train$DISCOUNT_PRICE <- 1/ log10(train$DISCOUNT_PRICE)
#(train$DISCOUNT_PRICE - min(train$DISCOUNT_PRICE))/(max(train$DISCOUNT_PRICE) - min(train$DISCOUNT_PRICE))
train$PRICE_RATE <- 1/ log10(train$PRICE_RATE)
#train$PRICE_RATE <- (train$PRICE_RATE - min(train$PRICE_RATE)) / 
#  (max(train$PRICE_RATE) - min(train$PRICE_RATE))
train$VALIDPERIOD <- (train$VALIDPERIOD - min(train$VALIDPERIOD)) / 
  (max(train$VALIDPERIOD) - min(train$VALIDPERIOD))

# Convert categorical variables to 0's and 1's
train <- cbind(train[,c(1,2)],
               model.matrix(~ -1 + .,train[,-c(1,2)],
                            contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], contrasts, contrasts=FALSE)))

# Detach testing coupon features
test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
train <- train[train$USER_ID_hash!="dummyuser",]

# Summarize for each user the features of purchased coupons
# (by taking their mean)
uchar <- aggregate(.~USER_ID_hash, data=train[,-1], FUN=mean)

# Save the features of the users and the test coupons
save(uchar, file=paste0(dir, "user_features_2.RData"))
save(test, file=paste0(dir, "testcoupon_features_2.RData"))
