#Import file, with check names off so I can personally modify the variable names

#setwd("Data Exercise_Analytics")

cd <- read.csv("Data Exercise_Analytics\\Campaign Data.csv", sep = ";", quote = "", check.names = F)

# Cleaning variables and variable names

nam <- names(cd)

nam <- as.character(gsub("\"","",nam))

names(cd) <- unname(nam)

age <- cd$age

age <- as.numeric(substr(as.character(age),2,4))

cd$age <- age

cd$job <- as.factor(gsub("\"","",cd$job))

cd$marital <- as.factor(gsub("\"","",cd$marital))

cd$education <- as.factor(gsub("\"","",cd$education))

cd$default <- as.factor(gsub("\"","",cd$default))

cd$housing <- as.factor(gsub("\"","",cd$housing))

cd$loan <- as.factor(gsub("\"","",cd$loan))

cd$contact <- as.factor(gsub("\"","",cd$contact))

cd$month <- as.factor(gsub("\"","",cd$month))

cd$day_of_week <- as.factor(gsub("\"","",cd$day_of_week))

cd$poutcome <- as.factor(gsub("\"","",cd$poutcome))

cd$response <- as.factor(gsub("\"","",cd$response))

levels(cd$response) <- c("No","Yes")

rm(age)

rm(nam)

# Converting education to a numeric level, for comparative purposes

cd$edulevel[cd$education == "unknown"] <- -1
cd$edulevel[cd$education == "illiterate"] <- 0
cd$edulevel[cd$education == "basic.4y"] <- 1
cd$edulevel[cd$education == "basic.6y"] <- 2
cd$edulevel[cd$education == "basic.9y"] <- 3
cd$edulevel[cd$education == "high.school"] <- 4
cd$edulevel[cd$education == "professional.course"] <- 5
cd$edulevel[cd$education == "university.degree"] <- 6

cd$season[cd$month == "apr"] <- "spring"
cd$season[cd$month == "may"] <- "spring"
cd$season[cd$month == "mar"] <- "spring"
cd$season[cd$month == "jun"] <- "summer"
cd$season[cd$month == "jul"] <- "summer"
cd$season[cd$month == "aug"] <- "summer"
cd$season[cd$month == "sep"] <- "winter"
cd$season[cd$month == "oct"] <- "winter"
cd$season[cd$month == "nov"] <- "winter"
cd$season[cd$month == "dec"] <- "winter"

# plotting various variables in the x, against cd$response

z <- function(x) {plot(cd$response, x)}

# Ran each variable in z to see if any correlation is apparent

# Also run major variables against one another to test for correlation amongst predictors

# Results examined:

# outcome of previous marketing campaign - if yes, then they should have subscribed
# yet 35% of all successes have not subscribed
# is it a time factor? compare median of success-subscribed to success-not

psucc <- cd[cd$poutcome == "success",]

succyes <- psucc[psucc$response == "yes",]
summary(succyes$pdays)

# about 6 days

succno <- psucc[psucc$response == "no",]
summary(succno$pdays)

# about 5 days - not enough to explain the discrepancy

par(mfrow = c(1,2))

par(mar=c(9,6,4,6))

plot(psucc$response, main = "Term Deposits Subscribed amongst Clients \n marked as 'Successful' in a Previous Campaign", col = "blue", xlab = "Term Deposits Subscribed", yaxt = 'n')

plot(psucc$response, psucc$pdays, col="skyblue", main = "Median Number of Days since Last Contact \n for Clients marked as 'Successful' \n by Previous Campaign", xlab = "Term Deposits Subscribed")

# Investigating times of below median employment

cdempllow <- cd[cd$nr.employed<5191,]

table(cdempllow$response)

# Much higher win ratio than normal

# Times of below first quartile employment

cdempvlow <- cd[cd$nr.employed<5099,]

table(cdempvlow$response)

# Times of at and above median employment

cdemphigh <- cd[cd$nr.employed>=5191,]

table(cdemphigh$response)

par(mfrow=c(1,3))

plot(cdemphigh$response, col="blue", main = "Term Deposits Subscribed during \n Above Median Employment", xlab = "Term Deposits Subscribed", yaxt = 'n')

plot(cdempllow$response, col="yellow", main = "Term Deposits Subscribed during \n Below Median Employment", xlab = "Term Deposits Subscribed", yaxt = 'n')

plot(cdempvlow$response, col="red", main = "Term Deposits Subscribed during \n Below First Quartile Employment", xlab = "Term Deposits Subscribed", yaxt = 'n')

#plot(cd$response, col="blue", main = "Term Deposits Subscribed Overall", xlab = "Term Deposits Subscribed", yaxt = 'n')

# Check statistical significance

lowemp <- table(cdempvlow$response)

regular <- table(cd$response)

testemp <- as.data.frame(rbind(lowemp,regular))

chisq.test(testemp)

# Check the education level of the savers, more educated save more in times of low employment

par(mfrow=c(1,2))

par(mar=c(6,9,3,4))

plot(cdemphigh$response, cdemphigh$edulevel, col="blue", main = "Term Deposits Subscribed during \n High Employment by Education Level", xlab = "Term Deposits Subscribed", yaxt = 'n')

axis(2,labels=c("Unknown","Illiterate","Basic 4","Basic 6","Basic 9","High School","Professional Course","University Degree"), at=-1:6, las=1)

plot(cdempllow$response, cdempllow$edulevel, col="yellow", main = "Term Deposits Subscribed during \n Low Employment by Education Level", xlab = "Term Deposits Subscribed", yaxt = 'n')

axis(2,labels=c("Unknown","Illiterate","Basic 4","Basic 6","Basic 9","High School","Professional Course","University Degree"), at=-1:6, las=1)


# Compare win ratios across age

par(mfrow=c(1,4))

plot(cd$response, main = "Term Deposits Subscribed \n Overall", xlab = "Term Deposits Subscribed", yaxt = 'n', col = "brown1")

cdold <- cd[cd$age>50,]

plot(cdold$response, main = "Term Deposits Subscribed by \n People over 50", xlab = "Term Deposits Subscribed", yaxt = 'n', col = "thistle")

cdold <- cd[cd$age>55,]

plot(cdold$response, main = "Term Deposits Subscribed by \n People over 55", xlab = "Term Deposits Subscribed", yaxt = 'n', col = "tan2")

cdold <- cd[cd$age>60,]

plot(cdold$response, main = "Term Deposits Subscribed by \n People over 60", xlab = "Term Deposits Subscribed", yaxt = 'n', col = "wheat3")

# Testing to see if the difference in subscription rate in people over 60 is statistically significant

old <- table(cdold$response)

regular <- table(cd$response)

testage <- as.data.frame(rbind(old,regular))

chisq.test(testage)


# Trying random forests to see variable importance

library(randomForest)

fit <- randomForest(response ~ month + contact + day_of_week + age + job + marital + edulevel + default + campaign + previous + cons.conf.idx + cons.price.idx + emp.var.rate + nr.employed + euribor3m, data=cd, importance = T)

varImpPlot(fit, main = "Overall")

#fit2 <- cforest(response ~ month + contact + age + job + marital + education + default + housing + loan + cons.conf.idx, data=cd, controls=cforest_unbiased(ntree=500,mtry=3))

# Check to see if there's any important variables in the split between cellular and telephone in the contact variable

#cell <- cd[cd$contact == "\"\"cellular\"\"",]

#phone <- cd[cd$contact == "\"\"telephone\"\"",]

#fitcell <- randomForest(response ~ month + age + job + marital + edulevel + default + housing + loan + cons.conf.idx, data=cell, importance = T)

#varImpPlot(fitcell, main = "Cellular")

#fitphone <- randomForest(response ~ month + age + job + marital + edulevel + default + housing + loan + cons.conf.idx, data=phone, importance = T)

#varImpPlot(fitphone, main = "Telephone")




# Not turning job into char as it's more useful as a factor

#job <- as.character(cd$X..job..)

#job <- sapply(job, function(x) {strsplit(x, "\"")[[1]][3]})

#cd$X..job.. <- job

