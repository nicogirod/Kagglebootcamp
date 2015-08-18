##### This script implements our recommendation model for various
##### parameter combinations, and records their scores (MAPs).
#####
##### edit: dir, reportfile, wts
#####
#####

dir = "data/split1/"
reportfile = "map-test??"
wts <- list(0, 2, 32, 32, 1, 0, 0, c(8, 16, 32), 1)
  # CAPSULE_NAME, GENRE_NAME, DISCOUNT_PRICE
  # PRICE_RATE, VALIDPERIOD, USABLE_DATE_(weekday)
  # USABLE_DATE_(weekend), ken_name, small_area_name


# Load the prepared user and test coupon features, the script
# for making recommendations, and the script for computing MAPs

load(paste0(dir, "user_features_2.RData"))
load(paste0(dir, "testcoupon_features_2.RData"))
load(paste0(dir, "coupon_purchased_test.RData"))
source("cos-sim.R")
source("map.R")

# Implement recommendation model and record its MAP over
# a grid of parameter values

report <- NULL
pos <- rep(1, 9)
done <- FALSE

while (!done) {
  w <- do.call(c, lapply(1:9, 
                         FUN=function(i){ return(wts[[i]][pos[i]]) }))
  print(w)
  W <- as.matrix(Diagonal(x=rep(w, c(24, 13, 1, 1, 1, 4, 5, 47, 55))))
  rec <- funcCosineW(W)
  score <- round(MAP(rec, purchases_test), 6)
  report <- rbind(report, c(w, score))
  
  i <- 1
  while (pos[i] == length(wts[[i]])) {
    pos[i] <- 1
    i <- i + 1
    if (i > 9) break
  }
  if (i > 9) { done <- TRUE }
  else { pos[i] <- pos[i] + 1 }
  
}

# Save the parameter values and corresponding MAPs

write.csv(report, paste0(dir, reportfile, ".csv"), row.names=F)



