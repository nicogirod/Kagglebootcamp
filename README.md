### NYC Data Science Bootcamp: Kaggle project

The original description of the project can be found
[here](https://www.kaggle.com/c/coupon-purchase-prediction).

Below is an overview of the included R scripts.

**1. pre-processing**

+ *split-data.R* --- splits the original training data into
training-validation sets

+ *make-features.R* --- pre-processes the features of the users 
and the test coupons

**2. tuning parameters** (feature weights)

+ *run-tests.R* --- implements our recommendation model on
a validation set using various weight combinations for
the coupon features; records resulting scores (MAPs)

+ *cos-sim.R* --- a function that accepts a weight combination
for the coupon features and returns coupon recommendations for
all users

+ *map.R* --- a function that computes the mean average precision
(MAP) of the recommendations for all users in a validation set

+ *summ-tests.R* --- combines the test scores (MAPs) from all
validation sets

**3. producing a submission**

+ *make-submission.R* --- produces coupon recommendations for
all users in the test set
