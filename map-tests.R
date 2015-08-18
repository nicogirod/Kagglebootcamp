source("map.R")

load("data/split1/coupon_purchased_test.RData")

load("data/user_list_en_all.RData")
users <- as.character(user_list$USER_ID_hash)
n.users <- length(users)

load("data/split1/coupon_list_test.RData")
coupons <- coupons_test$COUPON_ID_hash



# Test: randomly generate 10 coupons for each user

set.seed(10)
c <- as.character(coupons[1])
predict <- cbind(users, matrix(c, n.users, 10))
colnames(predict) <- c("USER_ID_hash", 
                       paste0("C", as.character(1:10)))
for (i in 1:n.users){
  predict[i,2:11] <- as.vector(sample(coupons, 10, replace=F))
}
predict <- as.data.frame(predict)

MAP(predict, purchases_test)   # 0.00064



# Test: choose a purchased coupon (if any) for each user

predict2 <- cbind(users, matrix(c, n.users, 1))
colnames(predict2) <- c("USER_ID_hash", "C1")
for (i in 1:n.users){
  cc <- with(purchases_test, COUPON_ID_hash[USER_ID_hash == users[i]])
  if (length(cc) > 0){
    predict2[i,2] <- as.character(cc[1])
  }
}
predict2 <- as.data.frame(predict2)

MAP(predict2, purchases_test)   # 0.116
