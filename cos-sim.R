# This function accepts a weight matrix for the coupon features
# and returns coupon recommendations for all users

library(Matrix)

funcCosineW = function(weightMat){
  
  # Calculate cosine similairties between users and testing coupons
  score = as.matrix(uchar[,2:ncol(uchar)]) %*% weightMat %*% 
    t(as.matrix(test[,2:ncol(test)]))
  
  # Find for each user the top 10 coupons
  submission <- 
    do.call(rbind, 
            lapply(1:nrow(uchar),
                   FUN = function(i){
                     pos <- which(score[i,] == max(score[i,]))
                     while (length(pos) < 10){
                       pos <- c(pos, which(score[i,] == max(score[i,-pos])))
                     }
                     pos <- pos[1:10]
                     return(as.character(test$COUPON_ID_hash[pos]))
                     #purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10], collapse=" ")
                     #return(purchased_cp)
                   }
            ))
  
  # Collect all recommendations
  submission <- cbind(as.character(uchar$USER_ID_hash), submission)
  return(submission)
  #write.csv(submission, file="cosine_sim.csv", row.names=FALSE)
  
}

