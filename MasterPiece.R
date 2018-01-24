cost_tau <- rep(NA,length(seq(min(yhat.bst),max(yhat.bst),by=0.01)))

for(i in seq(min(yhat.bst),max(yhat.bst),by=0.01)){
  tau <- i
  prediction <- factor(ifelse(yhat.bst>=tau,1,0))
  levels(prediction) <- c("no","yes")
  result <- confusionMatrix(prediction,test$return_customer)
  j <- which(seq(min(yhat.bst),max(yhat.bst),by=0.01)==i)
  naive <- sum((test$return_customer == "yes")*(-10) + (test$return_customer == "no")*(3))
  N <- nrow(test)
  cost_tau[j] <- ((result$table[[1]]*3 - result$table[[3]]*10) - naive )/N
}

which(cost_tau==max(cost_tau))
best_cutoff <- seq(min(yhat.bst),max(yhat.bst),by=0.01)[which(cost_tau==max(cost_tau))]
best_cutoff

profit <- cost_tau[which(cost_tau==max(cost_tau))]
profit









if(!require("xxx")) install.packages("xxx"); library("xxx")
#################################
#######     Packages    #########
#################################

if(!require("tabplot")) install.packages("tabplot"); library("tabplot")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("gdata")) install.packages("gdata"); library("gdata")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("e1071")) install.packages("e1071"); library("e1071")
if(!require("readr")) install.packages("readr"); library("readr")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("gbm")) install.packages("gbm"); library("gbm")
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("GGally")) install.packages("GGally"); library("GGally")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("rpart")) install.packages("rpart"); library("rpart")
if(!require("rpart.plot")) install.packages("rpart.plot"); library("rpart.plot")
if(!require("kernlab")) install.packages("kernlab"); library("kernlab")
if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("pROC")) install.packages("pROC"); library("pROC")
if(!require("ggm")) install.packages("ggm"); library("ggm")
if(!require("DMwR")) install.packages("DMwR"); library("DMwR")
if(!require("ROSE")) install.packages("ROSE"); library("ROSE")
if(!require("purrr")) install.packages("purrr"); library("purrr")
if(!require("plyr")) install.packages("plyr"); library("plyr")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("adabag")) install.packages("adabag"); library("adabag")
if(!require("h2o")) install.packages("h2o"); library("h2o")
if(!require("klaR")) install.packages("klaR"); library("klaR") 
if(!require("PRROC")) install.packages("PRROC"); library("PRROC")
if(!require("ROCR")) install.packages("ROCR"); library("ROCR")
########################################
##########     First Steps      ########
########################################

# Read "known" Dataset
known <- read.csv("assignment_BADS_WS1617_known.csv", sep = ",", na.strings=c("","NA"))

# Check class of all variables 
sapply(known,class)

# Transform wrongly classified variables via as.factor to factor variables.
known[,c("ID", "title", "newsletter", "model", 
         "delivery", "postcode_invoice", "coupon", 
         "giftwrapping", "referrer", "cost_shipping",
         "return_customer", "goods_value")] <- lapply(known[,c("ID", "title", "newsletter", "model", 
                                                               "delivery", "postcode_invoice",  "coupon", 
                                                               "giftwrapping", "referrer", "cost_shipping",
                                                               "return_customer", "goods_value")] , factor)

# Transform date variables via as.date to class "Date". Default format is %Y%m%d.
class(known$order_date)
known[,c("order_date", "account_creation_date", "deliverydate_estimated", 
         "deliverydate_actual")] <- lapply(known[,c("order_date",
                                                    "account_creation_date", 
                                                    "deliverydate_estimated", 
                                                    "deliverydate_actual")], as.Date)

# Create a backup for the original dataset.
known_original <- known


############################################
#####  Erease insignificant variables  #####
############################################

table.payment <- table(known$delivery, known$payment)
summary(table.payment)
# p-value = 0 for the test of independecne, so delivery and payment are dependent. 
table.payment
# Since "payment" ist directly correlated with "Cash Payment" in the variable "delivery" we can erase "delivery".
known <- subset(known, select = -delivery) 

length(known$order_date == known$account_creation_date) 
# This shows that it is always the same date so we can erase the variable "account_creation_date".
known <- subset(known, select = -account_creation_date)

summary(known$points_redeemed)
# Since "points_redeemed" just contains values of "0" we can erase it.
known <- subset(known, select = -points_redeemed)

summary(known$title)
# Since just 345 out of 51884 observations have the value 1, we can erase the variable "title".
known <- subset(known, select = -title)

summary(as.factor(known$film_count))
# This shows that 50898 out of 51884 observations the variabe "film_count" has the value 0. 
# So the number of other values is insignificantly low, therefore we erase this variable. 
known <- subset(known, select = -film_count) 

summary(known$musical_count)
summary(as.factor(known$musical_count))
# This shows that 51179 out of 51884 observations the variabe "musical_count" has the value 0. 
# So the number of other values is insignificantly low, therefore we erase this variable. 
known <- subset(known, select = -musical_count)

summary(known$hardware_count)
summary(as.factor(known$hardware_count))
# This shows that just 14 out of 51884 observations have he value 1 in the variable "hardware_count". 
# Due to this result we erase this variable. 
known <- subset(known, select = -hardware_count)


##############################################
#####    Missing Values & Outliers    ########
##############################################


#################################
###########   Weight   ##########
#################################

summary(known$weight)
# Hence, we decided to replace all 3947 NA?s in the variable "weight" with the mean = 636.7.
known$weight <- NAToUnknown(x=known$weight, unknown=636.7, force=TRUE)

# Check for outliers in "weight".
boxplot(known$weight)
ggplot(data = known, aes(x = 0, y = known$weight)) + geom_boxplot(outlier.colour = "red")

# Adjustment of outliers in "weight". 
lower.quartile <- as.numeric(summary(known$weight)[2])
upper.quartile <- as.numeric(summary(known$weight)[5])
IQR <- upper.quartile - lower.quartile
upper.bound <- upper.quartile + 1.5*IQR
known$weight[ known$weight > upper.bound ] <- upper.bound
boxplot(known$weight)
# The outliers have been successfully adjusted.
summary(known$weight)

# Check correlation between "weight" and "return_customer":
aov(formula = weight ~ return_customer, data = known)
summary(aov(formula = weight ~ return_customer, data = known))
# This shows a significant dependence of the two variables. 
pcor(c("weight", "return_customer"), var(known))
# The partial correlation of "weight" and "return_customer" is 0.04218683.                           

#################################
###    Form of Address     ######
#################################

summary(known$form_of_address)
# Shows that there is a large number of NA´s. 
# As it is difficult to replace the NA´s by the other levels, we decided to introduce a new level named "Empty". 
known$form_of_address <- NAToUnknown(x=known$form_of_address, unknown="Empty")
summary(known$form_of_address)


#################################
###    Advertising Code    ######
#################################

summary(known$advertising_code)
# Shows that 41593 values out of 51884 are missing.
# Therefore we decided to transform the variable into a binary variable that is 1 if an advertising code 
# was given in the data, and that is 0 if there was no advertising code given.
known$advertising_code <- NAToUnknown(x=known$advertising_code, unknown= "0")
known$advertising_code <- ifelse(known$advertising_code==0,0,1)
known$advertising_code <- as.factor(known$advertising_code)

#################################
#######    item sum      ########
#################################

# The sum of items is unequal to "item_count".
# Check for how many observations our statement above holds. 
length(known$item_count[known$item_count != known$canceled_items
                        +known$used_items+known$book_count+known$paperback_count
                        +known$schoolbook_count+known$ebook_count+known$audiobook_download_count
                        +known$audiobook_count+known$imported_count+known$other_count])

# Creating a new variable "item_sum".
item_sum <- known$canceled_items+known$used_items+known$book_count+known$paperback_count+known$schoolbook_count+
  known$ebook_count+known$audiobook_download_count+known$audiobook_count+known$imported_count+known$other_count
known<- cbind(known, item_sum)
# Replacing the observations where "item_count" is unequal to "item_sum" with values of new variable "item_sum".
known$item_count <- ifelse(known$item_count == known$item_sum, known$item_count, known$item_sum) 
length(known$item_count[known$item_count == known$item_sum])
summary(known$item_sum)

# Adjustment of outliers:
lower.quartile.is <- as.numeric(summary(known$item_sum)[2])
upper.quartile.is <- as.numeric(summary(known$item_sum)[5])
IQR.is <- upper.quartile.is - lower.quartile.is
upper.bound.is <- upper.quartile.is + 1.5*IQR.is
known$item_sum[ known$item_sum > upper.bound.is ] <- upper.bound.is
summary(known$item_sum)
# All outliers have been successfully replaced.

#################################
###       book count       ######
#################################

# There are several outliers that should be adjusted. 
lower.quartile.bc <- as.numeric(summary(known$book_count)[2])
upper.quartile.bc <- as.numeric(summary(known$book_count)[5])
IQR.bc <- upper.quartile.bc - lower.quartile.bc
upper.bound.bc <- upper.quartile.bc + 1.5*IQR.bc
known$book_count[ known$book_count > upper.bound.bc ] <- upper.bound.bc
summary(known$book_count)
# All outliers have been successfully adjusted.

#################################
###    postcode_delivery    #####
#################################

# Set up of a new variable "postcode_delivery2" that is calles "Yes" if a value for "postcode_delivery" was given 
# and that is called "No" if no value was available in the data. 

postcode_delivery2 <- known$postcode_delivery
postcode_delivery2 <- as.character(postcode_delivery2)

postcode_delivery2 <- NAToUnknown(x=postcode_delivery2, unknown="No")
postcode_delivery2[postcode_delivery2 != "No"] <- "Yes"
summary(postcode_delivery2)
# Here we can see that the transformation worked well. 
postcode_delivery2 <- as.factor(postcode_delivery2)
known <- cbind(known, postcode_delivery2)
summary(known$postcode_delivery2)

#################################
###    postcode_invoice    ######
#################################

# How to categorize postcode_invoice to regional zip-codes (Bundeslaender or 1-10)?
class(known$postcode_invoice)
postcode_numeric <- as.numeric(known$postcode_invoice)

# Cutting the observations wherever postcode_numeric is between 0 and 9, 9 and 19 and so on.
# Assign labels to those detached observations outcome saved in a new variable "ZIP_codes".
ZIP_codes <- cut(postcode_numeric, breaks=c(0,9,19,29,39,49,59,69,79,89,99),
                 labels=c("Region_0", "Region_1", "Region_2", "Region_3", "Region_4", "Region_5", "Region_6","Region_7", "Region_8", "Region_9"))

class(ZIP_codes)
summary(ZIP_codes)
plot(ZIP_codes)
# Shows that the majority of orders came from region 4. 
known <- cbind(known, ZIP_codes)

# Check the relation between the new variable "ZIP_codes" and "return_customer". 
tab_ZIPS <- table(known$ZIP_codes, known$return_customer)
summary(tab_ZIPS)
# The Chi-Squared test shows statistical dependence beween the "ZIP_codes" and "return_customer".
prop_ZIPS <- prop.table(tab_ZIPS,1)
prop_ZIPS
plot(prop_ZIPS[,2], type="l")
sort(prop_ZIPS[,2], decreasing = TRUE)
# Differences between the regions are quite small, regarding a returning customer.

# Check whether there is a difference between postcodes of urban and rural areas.
# How do postcodes of cities with more than 500,000 inhabitants differ from more rural areas?
# Postcodes of cities with more than 500,000 inhabitants:
# 10-13 (Berlin)
# 20&22 (Hamurg)
# 80-81 (Muenchen)
# 50-51 (Koeln)
# 60&61 (Frankfurt)
# 70    (Stuttgart)
# 40    (Duesseldorf)
# 44    (Dortmund)
# 45    (Essen)
# 04    (Leipzig)
# 28    (Bremen)
# 01    (Dresden)
# 30    (Hannover)
# 90    (Nuernberg)

# Take those ZIP_codes and assign the description "city" to them.
area <- as.numeric(known$postcode_invoice)
area[area == 10&11&12&13&20&22&80&81&50&51&60&61&70&40&44&45
     &4&28&1&30&90] <- "city"
# For the remaining postcodes we assign "rural".
area[area != "city"] <- "rural"
class(area)
# Change the class of "area" to a factor variable. 
area <- as.factor(area)
known <- cbind(known, area)
chisq.test(known$area, known$return_customer)
summary(known$area)

#################################
####   Christmas shopping  ######
#################################

# Check for christmas shopping by using the "month" variable from "inv_date2" (that is based on "order_date"). 
inv_date2 <- separate(known,order_date, c("Year", "Month", "Day"), sep = "-") 
# We introduce a new binary variable "christmas" that is 1 if the month for ordering is october (10th month)), and 0 if not. 
christmas <- ifelse(inv_date2$Month=="10",1,0)
# This variable should be a factor variable. 
christmas <- as.factor(christmas)
# We fit the new variable "christmas" to our data frame "known". 
known <- cbind(known, christmas)

#################################
###  deliverydate_estimated  ####
#################################

# Since we assume our observation dates should be between 04/2013 and 03/2014, we check for dates outside 
# of this time period. 
# So let´s find rows for the invalid date value within the variable "deliverydate_estimated". 
summary(known$deliverydate_estimated)

# Split "deliverydate_estimated" into year, month and dy column.
inv_date <- separate(known,deliverydate_estimated, c("Year", "Month", "Day"), sep = "-")
# Replace values > 2014 in the "deliverydate_estimated" column. 
inv_date$Year[inv_date$Year > "2014" & inv_date$deliverydate_actual < "2014-01-01"] <- "2013"
# Replace values < 2013 in the "deliverydate_estimated" column.
inv_date$Year[inv_date$Year < "2013" & inv_date$deliverydate_actual > "2013-12-31"] <- "2014"
# Now get the three colums back together to the original variable "deliverydate_estimated".
inv_date <- unite(inv_date, "deliverydate_estimated", c(Year, Month, Day), sep = "-")
# Replace the dummy dataset with the original name "known" again.
known <- inv_date
# Check for class "deliverydate_estimated".
class(known$deliverydate_estimated)
# Change class of "deliverydate_estimated" to "Date" again.
known[,"deliverydate_estimated"] <- as.Date(known[,"deliverydate_estimated"])
# Check for class again. 
class(known$deliverydate_estimated)
# Now "deliverydate_estimated" is of the right class again. 


####  deliverydate_actual  ####
# Check wheter there are invalid date value for the variable "deliverydate_actual. 
summary(known$deliverydate_actual)
# Here we can see that several orders were just delivered by the end of 2014, so with a high delivery delay.
# So let´s create a varable "deivery_delay".


##################################
######    Delivery delay    ######
##################################

# Calculate new Variable "delivery_delay" from the difference of "deliverydate_actual" and "deliverydate_estimated". 
delivery_delay <- known$deliverydate_actual - known$deliverydate_estimated
head(delivery_delay)
summary(delivery_delay)
# The variable is of class "difftime", we change it to integer to get a better summary.
delivery_delay <- as.integer(delivery_delay)

summary(delivery_delay)
# Unfortunatey there are 8708 NA´s, that come from the NA´s in the variable "deiverydate_actual". 
# Even though we want to check for statistical dependence of "delivery_delay" w.r.t. "return_customer". 
# But since chisq.test can't handle NA´s, we check for the chisq.test without NA´s in "delivery_delay". 
chisq.test(known$return_customer, !is.na(delivery_delay))
# The p-value = 0.0001032, therefore we conclude significant dependence. 

# Combine the new variable with our data frame "known".
known <- cbind(known, delivery_delay)
summary(known$delivery_delay)

# Even if there are 8708 missing values in the variable "delivery_delay" we decided to 
# keep the variable for further test and replace the missing values with the modal value. 
mode.delivery_delay <- function(delivery_delay) {
  uniqw <- unique(known$delivery_delay)
  uniqw[which.max(tabulate(match(known$delivery_delay, uniqw)))]
}
mode.delivery_delay() 
# The modal value and also the median is -1.
known$delivery_delay <- NAToUnknown(x=known$delivery_delay, unknown=-1, force=TRUE)


lower.quartile <- as.numeric(summary(known$delivery_delay)[2])
upper.quartile <- as.numeric(summary(known$delivery_delay)[5])
IQR <- upper.quartile - lower.quartile

# Calculate upper.bound and lower.bound value. 
upper.bound <- upper.quartile + 1.5*IQR
lower.bound <- lower.quartile - 1.5*IQR
# Replace values where the value of "delivery_delay" is larger than the "upper.bound".
known$delivery_delay[ known$delivery_delay > upper.bound ] <- upper.bound
# Replace values where the value of "delivery_delay" is lower than the "lower.bound".
known$delivery_delay[ known$delivery_delay < lower.bound ] <- lower.bound
summary(known$delivery_delay)

ggplot(data = known, aes(x = 0, y = known$delivery_delay)) + geom_boxplot(outlier.colour = "red")

summary(known$delivery_delay)
# Check whether there is a dependence between the different delivery delays and the variable "return_customer".
tab_delay <- table(known$delivery_delay, known$return_customer)
tab_delay
summary(tab_delay) 
# Chi-Squared states significant dependence between the variables. 
# So now we can erase the variables "deliverydate_estimated" and "deliverydate_actual",
# as we have replaced them by "delivery_delay".
known <- subset(known, select = -c(deliverydate_estimated, deliverydate_actual))

##############################################
########         Weight of Evidence      #####
########    for categorial variables     #####
##############################################

# We use full insample data for woe 
# We apply this for postcode_invoice and email_domain
woe.object <- woe(return_customer ~ postcode_invoice + email_domain, data = known, zeroadj = 0.5)

woe.object$woe

plot(woe.object) # Rule of thumb: 0.02 - 0.1 = weak predictive power. This stats that postcode_invoice has weak, email_domain no prdictive power !!!
summary(woe.object$xnew)
woe.object$xnew
woe.both <- woe.object$xnew
woe.both$woe.postcode_invoice
#Combine transformed variables into the dataframe
known[, "woe.postcode_invoice"] <- woe.both$woe.postcode_invoice
known[,"postcode_invoice"] <- NULL
known[,"email_domain"] <- NULL

##############################################
########   Multiple Chi-Squard Tests     #####
########    tor categorial variables     #####
##############################################
str(known)
chi_test_multiple <- mapply(function(x, y) chisq.test(x, y)$p.value, known[,c("ID", "form_of_address", "newsletter", "model", "postcode_delivery",  
                                                                              "postcode_delivery2", "coupon", "payment", "ZIP_codes",
                                                                              "referrer", "cost_shipping")], MoreArgs=list(known[,"return_customer"])) 
chi_test_multiple 
# Evaluation: yes = there is a significant dependence between the return_customer and some variables.
ifelse(chi_test_multiple<0.05,"sig. dependent","independent") 
# This shows which variables have a significant correlation with return_customer and which not. 

# Erease insignificant (independent) variables.
known <- subset(known, select = -c(postcode_delivery))
str(known)



##############################################
####   Training and Test Set (Daniel)   ######
##############################################
#With this code the sample is drawn with respect to the target variable. 
#This guarantees that the distribution from the target variable is identical within the train and test set. 
# When just doing a "sample()" this is not necessarily the case. 

known2 <- known

###############################################
#####     Standardize/Normalization    ########
###############################################
str(known)
num <- sapply(known2,is.numeric)
summary(num)
fac <- sapply(known2,is.factor)

known2[,num]
known3 <- scale(known2[,num]) # Generation Z-Scores, Mean=0, Variance=1
summary(known2[,num])
summary(known2)
summary(known3)
known4 <- cbind(known2[,fac],known2$order_date, known3)
names(known4)[names(known4)=="known2$order_date"] <- "order_date"

str(known4)

#################################################################
# Create the train size 

train.size <- createDataPartition(known4$return_customer, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train.known <- known4[train.size,-1] # Exclude ID 
test.known <- known4[-train.size,-1]

##################################################################
# Rename and assign levels (X1...Xn), This makes it easier for R to handle the different levels-

feature.names=names(train.known)

for (f in feature.names) {
  if (class(train.known[[f]])=="factor") {
    levels <- unique(c(train.known[[f]]))
    train.known[[f]] <- factor(train.known[[f]],
                               labels=make.names(levels))
  }
}

feature.names=names(test.known)
for (f in feature.names) {
  if (class(test.known[[f]])=="factor") {
    levels <- unique(c(test.known[[f]]))
    test.known[[f]] <- factor(test.known[[f]],
                              labels=make.names(levels))
  }
}



str(train.known)
str(test.known)



##############################################
##############   Logit       #################
##############################################

## First try:
lr <- glm(train.known$return_customer ~., data = train.known, family = binomial(link="logit"))
summary(lr)
# We can also have a look at the confidence intervals. Zero should not be included in the confidence
# interval of  significant variable. 
confint(lr)
coef(summary(lr))
# Check for which variables the p-value is smaller than 0.05.
significance_lr <- coef(summary(lr))[,4]
ifelse(significance_lr < 0.05, "sig. dependent", "independent") 
# There are some levels in "email_domain", "goods_value" and "ZIP_codes" not significant, but as other
# levels of these variables are statistically dependent on "return_customer" we leave them in the model. 
# Further, "postcode_delivery2", giftwrapping", "weight", "book_count", "paperback_count", 
# "imported_count", "item_sum" and "delivery_delay" do not show a significant dependence 
# so we will erase these variables from the model to improve it. 

# But first we check how well the model predicts. 
# Therefore we now use the test dataset "test.known" within the predict function and include 
# "type = response" to ensure that the result is fit to the same line (ID) as before. 
pred.lr <- predict(lr, newdata = test.known, type = "response")
summary(pred.lr)
head(predict(lr, newdata=test.known))

##Area under the Curve 
roc(test.known$return_customer,pred.lr)

# Now we check how well the model predicts that a customer is not returning if "return_customer" == 0,
# or that a customer is returning if "return_customer" == 1.
# We chose the default threshold of 0.5.
class.lr <- ifelse(pred.lr > 0.5, "X2", "X1")
accuracy.lr <- sum(class.lr == test.known$return_customer) / length(class.lr) 
accuracy.lr 
# The result is 0, even if we use other default thresholds. 

# To check how well our model performs we use the Brier Score. 
# The Brier score is the mean square error (MSE) between the actual values of "return_customer" 
# (1/0) and the predicted class probabilities "pred.lr".
# Therefore we first ceate a zero-one coded target variable.
y <- as.numeric(test.known$return_customer)-1  ####### actual values of test or train data????
brier.lr <- sum((y- pred.lr)^2) / length(y)
brier.lr
sprintf("The logit model has a Brier Score of %.5f (lower is better)", brier.lr)

# Is this a good score? We create a benchmark to compare the Brier Scores. 
# The most frequent outcome in the variable "return_customer"  is 0, which is now used as benchmark. 
class.benchmark <- rep("X1", nrow(train.known))
accuracy.benchmark <- sum(train.known$return_customer == class.benchmark) / length(y) 
# Check the value of the accuracy.benchmark.
accuracy.benchmark 
# The accuracy.benchmark has the value zero.
pred.benchmark <- rep(sum(test.known$return_customer == "X2")/nrow(test.known), nrow(test.known))
brier.benchmark <- sum((y- pred.benchmark)^2) / length(y)
brier.benchmark 
sprintf("The brier score benchmark is %.5f (lower is better)", brier.benchmark)
# So at least the brier score of our predicted ligistic model is better than the benchmark 
# (as lower is better). 


## We also create a logit model with the h2o package and check whether we get a better output.
set.seed(123)
h2o.init(nthreads = -1, max_mem_size="8g")
# We transform the data sets to h2o data sets "train.known.h2o" and "test.known.h2o". 
train.known.h2o <- as.h2o(train.known)
test.known.h2o <- as.h2o(test.known)

# Column 11 is return_customer in the test and training data frames.
# We transform the target variable to NA.
test.known.h2o
test.rc<- test.known.h2o[11]
test.known.h2o[11] <- NA

# As dependent variable we choose "return_customer" (column 11).
rc <- c(11)
# As independent variables we choose all other variables in the data.frame "train.known".
rest <- c(1:10, 12:28) 

# Now we use these arguments in the logistic regression:
lr.h2o <- h2o.glm(y = rc, x = rest, training_frame = train.known.h2o, family = "binomial") 
summary(lr.h2o)
# Let?s have a look at the coefficients and the p-values of the data frame.
coef(summary(lr.h2o))
# To get a quick overview over the performance of the model:
h2o.performance(lr.h2o)
# This gives us the output of the logistic regression of the h2o model. 
predict.lr.h2o <- as.data.frame(h2o.predict(lr.h2o, test.known.h2o))
summary(predict.lr.h2o)
# Fit the predicted otcome to the right line in the test data set "test.known.h2o".
known.h2o <- data.frame(predict.lr.h2o$h2o.predict, test.known.h2o$test.rc)

a <- sum(known.h2o$predict.lr.h2o_h2o.predict==1 & known.h2o$test.known.h2o_test.rc==1)
b <- sum(known.h2o$predict.lr.h2o_h2o.predict==0 & known.h2o$test.known.h2o_test.rc==1)
c <- sum(known.h2o$predict.lr.h2o_h2o.predict==1 & known.h2o$test.known.h2o_test.rc==0)
d <- sum(known.h2o$predict.lr.h2o_h2o.predict==0 & known.h2o$test.known.h2o_test.rc==0)
#e <- 3*d-10*b### 0 -> does not make sence? revenue from the logisitic regression model lr.h2o ????
a
b
c
d
# Due to the very imbalanced data set, the output is always zero, which is the most frequent value
# of the dependent variable. 
# Unfortunately this result is not quite good. 


## Second try:
# Now we take insignificant variables out and create a new logistic model. imported_count???
train.known <- subset(train.known, select = -c(postcode_delivery2, delivery_delay, item_sum,
                                               giftwrapping, weight, book_count, paperback_count)) 
str(train.known)

test.known <- subset(test.known, select = -c(postcode_delivery2, delivery_delay, item_sum,
                                             giftwrapping, weight, book_count, paperback_count)) 
str(test.known)
lr <- glm(train.known$return_customer ~., data = train.known, family = binomial(link="logit"))
summary(lr)
confint(lr)
coef(summary(lr))
# Check for which variables the p-value is smaller than 0.05.
significance_lr <- coef(summary(lr))[,4]
ifelse(significance_lr < 0.05, "sig. dependent", "independent") 
# We can see that still not all levels in all variables are significantly denpendent on "return_customer"
# but in all variables there are levels where the p-value is smaller than 0.05. 

# Now we check again how well the model predicts.
pred.lr <- predict(lr, newdata = test.known, type = "response")
head(predict(lr, newdata=test.known))

# Let?s check again how well the model predicts that a customer is not returning if "return_customer" == 0,
# or that a customer is returnin if "return_customer" == 1.
class.lr <- ifelse(pred.lr > 0.5, "X2", "X1")
accuracy.lr <- sum(class.lr == test.known$return_customer) / length(class.lr) 
accuracy.lr 
# As before, the result is zero even if we vary the default threshold. 

# Let?s check the Brier Score again:
y <- as.numeric(train.known$return_customer)-1
brier.lr <- sum((y- pred.lr)^2) / length(y)
brier.lr 
# The Brier Score of our model decreased from 0.1587279 to 0.1587025, which is a slight improvement.
sprintf("The logit model has a Brier Score of %.5f (lower is better)", brier.lr)
# "The logit model has a Brier Score of 0.16047 (lower is better)".
brier.benchmark
# As before, we compare the new result to our brier.benchmark, which is 0.1883811. 
# So our new model?s brier score is better that the benchmark, but worse than our first model.
# This output is very bad as erasing insignificant variables did not improve our model. 

# Last but not least, we can try to improve the model by erasing all variables that include
# levels that show insignificance. In our last model this was the case for "ZIP_codes" 
# and "email_domain". 


## Third try:
# We erast the variables "ZIP_codes" and "email_domain" from our model.  
train.known <- subset(train.known, select = -c(ZIP_codes, email_domain)) 
str(train.known)
test.known <- subset(test.known, select = -c(ZIP_codes, email_domain)) 
str(test.known)
# All varibles are significant at the 0.05 level. 
lr <- glm(train.known$return_customer ~., data = train.known, family = binomial(link="logit"))
summary(lr)
confint(lr)
coef(summary(lr))
# Check for which variables the p-value is smaller than 0.001.
significance_lr <- coef(summary(lr))[,4]
ifelse(significance_lr < 0.01, "sig. dependent", "independent") 
# Just a view variables stay that are not significant at the 0.01 level. 
# This is the case for "schoolbook_count","model", "canceled_items", and one level of "goods_value". 

# We check again how well the model predicts.
pred.lr <- predict(lr, newdata = test.known, type = "response")
class.lr <- ifelse(pred.lr > 0.5, 1, 0)
accuracy.lr <- sum(class.lr == test.known$return_customer) / length(class.lr) 
accuracy.lr 
# Again, the result is zero even if we vary the default threshold. 

# Let?s check the Brier Score again:
y <- as.numeric(train.known$return_customer)-1
brier.lr <- sum((y- pred.lr)^2) / length(y)
brier.lr 
# The Brier Score of our model decreased from 0.1587279 to 0.1587025, which is a slight improvement.
sprintf("The logit model has a Brier Score of %.5f (lower is better)", brier.lr)
# "The logit model has a Brier Score of 0.16006 (lower is better)"
brier.benchmark
# Again, our new model is better than the benchmark, and now we can also observe a slight impovement
# compared to the second one. Even though the result is still worse than the first model.




##############################################
########   Random Forest (Daniel)   ##########
##############################################
# Use known2 as dataset ( 31 Variables) - Run Rf.known2, predicts a AUC of 0.6271
# The difference between known2 and train.known is that the known2 dataset is not standardized and not transformed into X1....Xn factor levels



summary(known$return_customer)
known2 <- subset(known, select = -c(postcode_invoice,order_date,advertising_code,title))
summary(known2)
str(known2)
str(train.known)
#1. Create a trainings set:
train.size <- sample(1:nrow(known2), nrow(known2)*0.8)
#2. Run randomForest
RF.known <- randomForest(return_customer~.,data=train.known,importance=T)
RF.known

Rf.known2 <- randomForest(return_customer~.,data=known2[,-1],subset = train.size,importance=T)
Rf.known2






#3. Evaluate performace on the test set

yhat.bag <- predict(RF.known,newdata=test.known, type="prob")[,2]
pred_RF2 <- predict(Rf.known2,newdata=known2[-train.size,-1],type="prob")[,2]


#### Plotting Curves ######
##Create a test set (the remaining data without the training)
known.test <-known2[-train.size,"return_customer"]
known.test

roc_rf1 <- roc(test.known$return_customer,yhat.bag,plot=T, ci = T) # Better results

str(roc_rf1)
roc(known.test,pred_RF2, plot=T)
plot(roc_rf1, main="ROC Curve for Random Forest", col="blue", print.auc=T)



#### ggplot
library(ROCR)
roc2 <- cbind(roc_rf1$sensitivities,roc_rf1$specificities)
roc2 <- as.data.frame(roc2)
roc2
ggplot(roc2,aes(1-V2,V1))+geom_line(size = 2, alpha = 0.7)+
  labs(title= "ROC Curve Random Forest", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  geom_line(colour="red", size=1) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 15)
# To add another ROC data: +geom_line(data=roc3)


### Package ROCR
pred1 <- prediction(pred_RF2,known.test)
PR.perf <- performance(pred1, "prec", "rec")
per2 <- performance(pred1, measure = "tpr", x.measure = "fpr")
ROC.perf <- performance(pred1, "tpr", "fpr")
plot(per2, col=rainbow(10))
plot(PR.perf)

##MSE or brier.score funktioniert hier wohl nicht, da output binär ist. Was soll das Ergbenis sein wenn es nur 0 oder 1 gibt? 

##Check variable importance 
varImpPlot(RF.known,type=1, main = "Variable Importance")
varImpPlot(Rf.known2,type=1, main = "Variable Importance")
abline(v=c(12,27,49))
print(importance(RF.known))



######### Tune RF ##########
str(train.known)
tuneRF <- tuneRF(train.known[,-11,],train.known[,11],importance=T,stepFactor = 0.5)


##############################################
########   BOOSTING      (Daniel)   ##########
##############################################


####

str(train.known)

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objControl
objModel <- train(train.known[,-11], train.known[,"return_customer"], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC")


plot(objModel)
summary(objModel)
print(objModel)


predictions <- predict(object=objModel, test.known[,-11], type='raw')
gbm.tab <- table(predictions,test.known$return_customer)
gbm.tab
confusionMatrix(gbm.tab)
mean(predictions==return_customer)
head(predictions)
print(postResample(pred=predictions, obs=as.factor(test.known[,"return_customer"])))



### Second Model with more tuning parameter

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

objControl <- trainControl(method='repeatedcv', number=5, repeats = 3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel2 <- train(train.known[,-11], train.known[,"return_customer"], 
                   method='gbm', 
                   trControl=objControl, 
                   tuneGrid = gbmGrid,
                   metric = "ROC")

##Best Parameters (with ROSE):  ntrees=300, interaction.depth=5
summary(objModel2)
print(objModel2)

#############
# Random approach 
summary(test.known$return_customer)
predictions
random <- sample(c("X1","X2"),20753,prob=c(0.8,0.2), replace = T)
random
table(random,test.known$return_customer)
mean(random==return_customer)



#### Third model with respect to impalanced data 
###

##Original Model 
gbmGrid <-data.frame(interaction.depth = 5, 
                     n.trees = 300, 
                     shrinkage = 0.1,
                     n.minobsinnode = 20)


objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel_original <- train(train.known[,-11], train.known[,"return_customer"], 
                           method='gbm', 
                           trControl=objControl,  
                           metric = "ROC", 
                           tuneGrid = gbmGrid)



# Create model weights (they sum to one)

model_weights <- ifelse(train.known$return_customer == "X1",
                        (1/table(train.known$return_customer)[1]) * 0.4,
                        (1/table(train.known$return_customer)[2]) * 0.6)



objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel_weighted <- train(train.known[,-11], train.known[,"return_customer"], 
                           method='gbm', 
                           trControl=objControl,  
                           metric = "ROC",
                           weights = model_weights,
                           tuneGrid = gbmGrid)

print(objModel_weighted)

### Use up parameter 
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE, sampling = "up")
objModel_up <- train(train.known[,-11], train.known[,"return_customer"], 
                     method='gbm', 
                     trControl=objControl,  
                     metric = "ROC",
                     tuneGrid = gbmGrid)


### Use down parameter

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE, sampling = "down")
objModel_down <- train(train.known[,-11], train.known[,"return_customer"], 
                       method='gbm', 
                       trControl=objControl,  
                       metric = "ROC",
                       tuneGrid = gbmGrid)


### Use smote parameter             

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE, sampling = "smote")
objModel_smote <- train(train.known[,-11], train.known[,"return_customer"], 
                        method='gbm', 
                        trControl=objControl,  
                        metric = "ROC",
                        tuneGrid = gbmGrid)


### Use rose parameter with two different models . 

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE, sampling = "rose")
for(methods_b in c('gbm','AdaBoost.M1')) {
  objModel_rose <- train(train.known[,-11], train.known[,"return_customer"], 
                         method=methods_b, 
                         trControl=objControl,  
                         metric = "ROC")
  print(objModel_rose)                 
}






plot(objModel)
summary(objModel)
print(objModel_rose)

# Examine results for test sets

test_roc <- function(model, data) {
  
  roc(data$return_customer,
      predict(model, data, type = "prob")[, "X2"])}

model_list <- list(original = objModel_original,
                   weighted = objModel_weighted,
                   down = objModel_down,
                   up = objModel_up,
                   SMOTE = objModel_smote,
                   ROSE = objModel_rose)

model_list_roc <- model_list %>%
  map(test_roc, data = test.known)

model_list_roc %>%
  map(auc)

##Plot the curves 
results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 6 models



ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



##############################################
########  Support Vector Machine (Daniel)  ###
##############################################
summary(known)
install.packages("e1071")
library(e1071)
known5 <- subset(known4, select = -c(giftwrapping,
                                     used_items,musical_count,
                                     film_count,audiobook_count))

train.size3 <- createDataPartition(known5$return_customer, p = .8, 
                                   list = FALSE, 
                                   times = 1)

train.known3 <- known5[train.size,-1]
test.known3 <- known5[-train.size,-1]


feature.names=names(train.known3)

for (f in feature.names) {
  if (class(train.known3[[f]])=="factor") {
    levels <- unique(c(train.known3[[f]]))
    train.known3[[f]] <- factor(train.known3[[f]],
                                labels=make.names(levels))
  }
}

feature.names=names(test.known3)
for (f in feature.names) {
  if (class(test.known3[[f]])=="factor") {
    levels <- unique(c(test.known3[[f]]))
    test.known3[[f]] <- factor(test.known3[[f]],
                               labels=make.names(levels))
  }
}



summary(known3)
str(known3)
str(train.known3)



# response variable (target) needs to be of form "factor"
class(known2$return_customer)
svmfit=svm(return_customer~., data=train.known[-30], kernel="linear", cost=1,scale=FALSE,cross=0)
summary(svmfit)

#### Kernal trick ####
kernal <- rbfdot(sigma=0.01)
svmfit_kernal=svm(return_customer~., data=train.known3[,-1], kernel="radial", cost=1,scale=FALSE, cross=4)
summary(svmfit_kernal)

##Check for the best cost factor
tune.out=tune(svm,return_customer~.,data=train.known3[,-1],kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)

##Store the model with the best cost factor
bestmod=tune.out$best.model 
summary(bestmod)

## Make predictions
summary(test.known)
##linear kernal
ypred=predict(svmfit ,newdata=test.known[,-30])
confusionMatrix(test.known$return_customer,ypred)
roc(test.known$return_customer,ypred)

## radial kernal 
ypred=predict(svmfit_kernal,newdata=test.known3[,-1])

svm_table <- table(predict=ypred, truth=test.known3$return_customer)               
svm_table
attach(test.known3)
mean(ypred==return_customer)
diag(svm_table)

############################ SVM with caret   ####################

objControl_svm <- trainControl(method='repeatedcv', repeats =3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

svmTune <- train(return_customer~.,data=train.known[,-30], 
                 method="svmRadial",
                 tuneLength=9,
                 preProc=c("center","scale"),
                 metric="ROC",
                 trControl=objControl_svm)

pred_svm=predict(svmTune ,newdata=test.known[,-30]) #Even if the target variable is included, it should not be a problem since the model searches for the right colums to use
roc(test.known$return_customer,pred_svm)

##############################################
#########  Neural Networks (Daniel)  #########
##############################################

train.size <- createDataPartition(known4$return_customer, p = .6, 
                                  list = FALSE, 
                                  times = 1)

train.known <- known4[train.size,-1]
test.known <- known4[-train.size,-1]
str(train.known)
net <- nnet(return_customer~.,data=train.known,size=20, maxit=150)
net
net.pred <- predict(net,test.known, type="class")
tab.net <- table(net.pred,test.known$return_customer)
mean(net.pred==test.known$return_customer)
confusionMatrix(tab.net)

##############################################
#########  Neural Networks (Marius)  #########
##############################################

#using functions of caret-package for a NN-regression (wrapping extensive list of model functions
#into one coherent framework)
#creation of object "model.control" which contains the set up for our model estimation framework
#using trainControl to set our control parameters for a k-fold-cross-validation


#################################
##                             ##
## neural networks with caret  ##
##                             ##
#################################
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark")

nrOfCores <- detectCores()
cl <- makeCluster(max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))

#1st: Training- and test-data

train.size <- createDataPartition(known4$return_customer, p = .6, 
                                  list = FALSE, 
                                  times = 1)

train.known <- known4[train.size,-1]
test.known <- known4[-train.size,-1]


feature.names=names(train.known)

for (f in feature.names) {
  if (class(train.known[[f]])=="factor") {
    levels <- unique(c(train.known[[f]]))
    train.known[[f]] <- factor(train.known[[f]],
                               labels=make.names(levels))
  }
}

feature.names=names(test.known)
for (f in feature.names) {
  if (class(test.known[[f]])=="factor") {
    levels <- unique(c(test.known[[f]]))
    test.known[[f]] <- factor(test.known[[f]],
                              labels=make.names(levels))
  }
}

#########subsampling for class imbalances#########

#####down-sampling######

#using functions of caret-package for a NN-regression
#creation of object "model.control" which contains the set up for our model estimation framework
#using trainControl to set our control parameters for a k-fold-cross-validation
model.control.down <- trainControl(
  method = "cv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "down",
  allowParallel = TRUE,
  returnData = FALSE)

#defining a search grid for values to test
nn.grid <- expand.grid(decay = c(0, 10^seq(-3,0,1)), size = 3)

#train a neural network, using pre-defined "model.control" and "nn.parms"
nn.down <- train(return_customer~., data = train.known, method = "nnet", maxit = 100, trace = TRUE,
                 tuneGrid = nn.grid, metric = "ROC", trControl = model.control.down)

stopCluster(cl)

nn.down
summary(nn.down)
print(nn.down)

yhat.nn.down <- predict(nn.down, newdata = test.known, type = "prob") [,2]
nn.down.roc <- roc(test.known$return_customer, yhat.nn.down)
auc(nn.down.roc)
plot.roc(nn.down.roc)


#####up-sampling######

#using functions of caret-package for a NN-regression
#creation of object "model.control" which contains the set up for our model estimation framework
#using trainControl to set our control parameters for a k-fold-cross-validation
model.control.up <- trainControl(
  method = "cv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "up",
  allowParallel = TRUE,
  returnData = FALSE)

#defining a search grid for values to test
nn.grid <- expand.grid(decay = c(0, 10^seq(-3,0,1)), size = 3)

#train a neural network, using pre-defined "model.control" and "nn.parms"
nn.up <- train(return_customer~., data = train.known, method = "nnet", maxit = 100, trace = TRUE,
               tuneGrid = nn.grid, metric = "ROC", trControl = model.control.up)

stopCluster(cl)

nn.up
summary(nn.up)
print(nn.up)

yhat.nn.up <- predict(nn.up, newdata = test.known, type = "prob") [,2]
nn.up.roc <- roc(test.known$return_customer, yhat.nn.up)
auc(nn.up.roc)
plot.roc(nn.up.roc)


#####SMOTE######

#using functions of caret-package for a NN-regression
#creation of object "model.control" which contains the set up for our model estimation framework
#using trainControl to set our control parameters for a k-fold-cross-validation
model.control.smote <- trainControl(
  method = "cv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "smote",
  allowParallel = TRUE,
  returnData = FALSE)

#defining a search grid for values to test
nn.grid <- expand.grid(decay = c(0, 10^seq(-3,0,1)), size = 3)

#train a neural network, using pre-defined "model.control" and "nn.parms"
nn.smote <- train(return_customer~., data = train.known, method = "nnet", maxit = 100, trace = TRUE,
                  tuneGrid = nn.grid, metric = "ROC", trControl = model.control.smote)

stopCluster(cl)

nn.smote
summary(nn.smote)
print(nn.smote)

yhat.nn.smote <- predict(nn.smote, newdata = test.known, type = "prob") [,2]
nn.smote.roc <- roc(test.known$return_customer, yhat.nn.smote)
auc(nn.smote.roc)
plot.roc(nn.smote.roc)


#####original######

#using functions of caret-package for a NN-regression
#creation of object "model.control" which contains the set up for our model estimation framework
#using trainControl to set our control parameters for a k-fold-cross-validation
model.control.orig <- trainControl(
  method = "cv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  returnData = FALSE)

#defining a search grid for values to test
nn.grid <- expand.grid(decay = c(0, 10^seq(-3,0,1)), size = 3)

#train a neural network, using pre-defined "model.control" and "nn.parms"
nn.orig <- train(return_customer~., data = train.known, method = "nnet", maxit = 100, trace = TRUE,
                 tuneGrid = nn.grid, metric = "ROC", trControl = model.control.orig)

stopCluster(cl)

plot(nn.orig)
nn.orig
summary(nn.orig)
print(nn.orig)

yhat.nn.orig <- predict(nn.orig, newdata = test.known, type = "prob") [,2]
nn.orig.roc <- roc(test.known$return_customer, yhat.nn.orig)
auc(nn.orig.roc)
#area under the curve: 0.5644
plot.roc(nn.orig.roc)


#####ROSE######

#using functions of caret-package for a NN-regression
#creation of object "model.control" which contains the set up for our model estimation framework
#using trainControl to set our control parameters for a k-fold-cross-validation
model.control.rose <- trainControl(
  method = "cv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "rose",
  allowParallel = TRUE,
  returnData = FALSE)

#defining a search grid for values to test
nn.grid <- expand.grid(decay = c(0, 10^seq(-3,0,1)), size = 3)

#train a neural network, using pre-defined "model.control" and "nn.parms"
nn.rose <- train(return_customer~., data = train.known, method = "nnet", maxit = 100, trace = TRUE,
                 tuneGrid = nn.grid, metric = "ROC", trControl = model.control.rose)

stopCluster(cl)

plot(nn.rose)
nn.rose
summary(nn.rose)
print(nn.rose)

yhat.nn.rose <- predict(nn.rose, newdata = test.known, type = "prob") [,2]
nn.rose.roc <- roc(test.known$return_customer, yhat.nn.rose)
auc(nn.rose.roc)
plot.roc(nn.rose.roc)


#######with more tuning parameters########

#ten-folds stratified repeated cross-validation
folds <- 10
cvIndex <- createFolds(factor(train.known$return_customer), folds, returnTrain = T)

model.control.rose <- trainControl(
  index = cvIndex,
  method = "repeatedcv",
  number = folds,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "rose",
  allowParallel = TRUE,
  returnData = FALSE)

#defining a search grid for values to test
nn.grid <- expand.grid(decay = c(0,0.2,0.4,0.6,0.8,1), size = seq(3,9,2))

#train a neural network, using pre-defined "model.control" and "nn.parms"
nn.rose <- train(return_customer~., data = train.known, method = "nnet", maxit = 100, trace = TRUE,
                 tuneGrid = nn.grid, metric = "ROC", trControl = model.control.rose)

stopCluster(cl)

plot(nn.rose)
nn.rose
summary(nn.rose)
print(nn.rose)

yhat.nn.rose <- predict(nn.rose, newdata = test.known, type = "prob") [,2]
nn.rose.roc <- roc(test.known$return_customer, yhat.nn.rose)
auc(nn.rose.roc)
#0.5606
plot.roc(nn.rose.roc)


####### now with more repititions of the stratified repeated ######
####### cross-validation and a higher number of iterations   ######
####### and a different set of weight decays & hidden nodes, ######
####### also I use cross-validation instead of repeated cv   ######

folds <- 10
cvIndex <- createFolds(factor(train.known$return_customer), folds, returnTrain = T)

model.control.rose2 <- trainControl(
  index = cvIndex,
  method = "cv",
  number = folds,
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "rose",
  allowParallel = TRUE,
  returnData = FALSE)

#defining a search grid for values to test
nn.grid2 <- expand.grid(decay = c(0.6,0.7,0.8,0.9,1), size = c(3,4,5,6))

#train a neural network, using pre-defined "model.control" and "nn.parms"
nn.rose2 <- train(return_customer~., data = train.known, method = "nnet", maxit = 500, trace = TRUE,
                  tuneGrid = nn.grid2, metric = "ROC", trControl = model.control.rose2)

stopCluster(cl)

plot(nn.rose2)
nn.rose2
summary(nn.rose2)
print(nn.rose2)

yhat.nn.rose2 <- predict(nn.rose2, newdata = test.known, type = "prob") [,2]
nn.rose2.roc <- roc(test.known$return_customer, yhat.nn.rose2)
auc(nn.rose2.roc)
#0.5579
plot.roc(nn.rose2.roc)

#######how about variable importance?########

#######now considering the variable importance with less variables########

##############################################
########  Visualization ROC ...  (Marius)  ###
##############################################


#plot ROC-curves for all regressions in the same chart
predictions.ROC <- data.frame(LR = logit.probs, SVM = ypred, RF = yhat.bag, DT = pred.dt, LG = glm.pred, BO = predictions)

#to draw the ROC curve of all regressions in one plot we use "hmeasure"
#it computes the ROC curve and related scalar performance metrics, including
#the AUC (area under curve)
true.class <- as.numeric(test.known$return_customer)-1
true.class
h <- HMeasure(true.class, as.numeric(predictions))
plotROC(h, which = 1)
#now we would like to extract the result of the AUC calculation
h$metrics["AUC"]

yhat.nn
as.numeric(predictions)-1
logit.probs
str(test.known)
gbm.tab