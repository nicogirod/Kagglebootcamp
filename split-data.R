# NOTE: adjust CV_START and dir
# CV_START = 2012-06-17, dir = split1
# CV_START = 2012-06-10, dir = split2
# CV_START = 2012-06-03, dir = split3



# Load all the date specific data.

load("data/coupon_list_train_en_all.RData")
load("data/coupon_visit_train.RData")
load("data/coupon_purchased_train.RData")



# Set period for validation.

week <- 60 * 60 * 24 * 7
CV_START <- as.POSIXlt("2012-06-17")
CV_END <- CV_START + week - 1



# Split training data into a smaller training set
# and a validation set.

dir = "data/??/"

i.train <- (couple_list_train$DISPFROM < CV_START)
i.test <- ((couple_list_train$DISPFROM >= CV_START) &
             (couple_list_train$DISPFROM <= CV_END))
coupons_train <- couple_list_train[i.train,-1]
coupons_test <- couple_list_train[i.test,-1]
save(coupons_train, file=paste0(dir, "coupon_list_train.RData"))
save(coupons_test, file=paste0(dir, "coupon_list_test.RData"))

i.train <- (coupon_purchased_train$I_DATE < CV_START)
i.test <- ((coupon_purchased_train$I_DATE >= CV_START) &
             (coupon_purchased_train$I_DATE <= CV_END))
purchases_train <- coupon_purchased_train[i.train,-1]
purchases_test <- coupon_purchased_train[i.test,-1]
save(purchases_train, file=paste0(dir, "coupon_purchased_train.RData"))
save(purchases_test, file=paste0(dir, "coupon_purchased_test.RData"))

i.train <- (coupon_visit_train$I_DATE < CV_START)
visits_train <- coupon_visit_train[i.train,-1]
save(visits_train, file=paste0(dir, "coupon_visit_train.RData"))

