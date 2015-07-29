# This function computes the mean average precision (MAP) 
# of the recommendations for all users.
#
# [predict]
# -- data frame
# -- first column: contains all users, named "USER_ID_hash"
# -- other columns: recommended coupons, in order
#
# [actual]
# -- data frame
# -- contains all purchase records in the testing period, including
#    a column of users named "USER_ID_hash" and a column of
#    purchased coupons named "COUPON_ID_hash"



MAP <- function(predict, actual){
  
  n.user <- dim(predict)[1]       # number of users
  n.rec <- dim(predict)[2] - 1    # number of recommendations
  AP.sum <- 0
  
  # This section computes the average precision (AP) for
  # each user and accumulate.
  
  for (u in predict$USER_ID_hash){
    u.actual <-  with(actual, COUPON_ID_hash[USER_ID_hash == u])
                                  # a user's purchased coupons
    n.pur <- length(u.actual)     # number of purchased coupons
    if (n.pur == 0) {
      AP <- 0                     # AP = 0 in absence of purchases
    } else {
      u.predict <- predict[(predict$USER_ID_hash == u),
                           (names(predict) != "USER_ID_hash")]
                                  # recommended coupons for a user
      u.predict <- as.matrix(u.predict)
      correct <- (u.predict %in% u.actual) * 1
      AP <- sum(cumsum(correct)*correct/(1:n.rec)) / min(n.rec, n.pur)
                                  # AP of recommendations
    }
    AP.sum <- AP.sum + AP   
  }

  return(AP.sum / n.user)         # MAP of all recommendations
}

