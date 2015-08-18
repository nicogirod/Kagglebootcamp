##### Produce submissions
#####
##### edit: wts, fname
#####

fname = "submissions/submission13a.csv"
wts <- c(0, 2, 64, 32, 1, 0, 0, 8, 1)
  # CAPSULE_NAME, GENRE_NAME, DISCOUNT_PRICE
  # PRICE_RATE, VALIDPERIOD, USABLE_DATE_(weekday)
  # USABLE_DATE_(weekend), ken_name, small_area_name
  # Nico: 0, 2, 1, 1, 1, 0, 0, 1, 0.25


# Load the prepared user and test coupon features, and the script
# for making recommendations

load(paste0("data/user_features_2.RData"))
load(paste0("data/testcoupon_features_2.RData"))
load("data/user_list_en_all.RData")
users <- as.character(user_list$USER_ID_hash)
source("cos-sim.R")

# Implement recommendation model

W <- as.matrix(Diagonal(x=rep(wts, c(24, 13, 1, 1, 1, 4, 5, 47, 55))))
rec <- funcCosineW(W)

rec.u <- rec[,1]
rec.c <- rec[,-1]
missed <- users[!(users %in% rec.u)]
top.c <- names(sort(table(as.vector(rec.c)), decreasing = TRUE)[1:10])
rec2 <- cbind(missed, t(matrix(rep(top.c, 91), ncol=91)))

rec <- rbind(rec, rec2)

sub <- sapply(1:dim(rec)[1],
               FUN = function(i){
                 paste(rec[i,-1], collapse=" ")
               })
sub <- cbind(rec[,1], sub)
colnames(sub) <- c("USER_ID_hash", "PURCHASED_COUPONS")


# Save the parameter values and corresponding MAPs

write.csv(sub, fname, row.names=F)


