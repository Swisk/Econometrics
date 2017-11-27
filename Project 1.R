
library(car)
library(MASS)
library(caret)

# read in data
df <- read.csv("C:/Users/Shane/Documents/NUS Documents/NUS Year 4/Project1Data.csv")

# data cleaning
# transform year and month into numeric form for use in regression
df$time <- df$year + df$month/12


# test and training set
# we could use k fold cross validation here, but because the data is so small, a simple train test split should be more than sufficient.
set.seed(123)
sample <- sample(c(TRUE,FALSE),nrow(df),prob = c(0.75,0.25),replace = TRUE)

data.train <- df[sample, ]
data.test<- df[!sample,]


# Naive Model
# demand model
naive <- lm(qu ~ cprice + tprice + oprice + incom + q1 + q2 + q3 + time, data = data.train)

summary(naive)

# test generalizability of model to test data
naive.test <- predict(naive, newdata = data.test)
naive.test.residuals <- data.test$qu - naive.test
sqrt(mean(naive.test.residuals^2))


# 2 STAGE LINEAR REGRESSION
# first stage

# VARIABLE SELECTION METHOD
# test correlation
scatterplotMatrix(df)
cor(df[c(4:8, 13:15)])

# we can see that a large number of variables (such as income and oprice) correlate almost perfectly.
# as such, it makes sense to only consider a smaller number of these variables.
# for the sake of the later endogeneity, we will use only the supply side variables.
# these two variable have the lowest correlation with each other, combined have a very high correlation with all the other exogeneous factors
# log price for nonnegativity

price.model <- lm(log(cprice) ~ bprice + wprice + q1 + q2 + q3, data = data.train)

# view performance of the model
summary(price.model)

# view plots to test assumptions of model validity
plot(price.model)


## from the distribution of the residuals, we can observe that perhaps a linear model is not the most appropriate.
# test distribution of variables
scatterplotMatrix(df[c(5,13:15)])

## We can see that there appears to be a non linear relationship with cprice and wprice, but mostly linear 
## with bprice and time.

# Hence, try non linear model with variables earlier identified
price.log.model <- lm(log(cprice) ~ bprice + wprice + I(wprice^2) + q1 + q2 + q3, data = data.train)

# view model performance again
summary(price.log.model)
plot(price.log.model)


# improve model
# we use stepAIC here to automatically find the best model from the provided variables
stepped.model <- stepAIC(price.log.model, direction="both")
summary(stepped.model)

plot(stepped.model)

# test generalizability to test data
stepped.test <- predict(stepped.model, newdata = data.test)
stepped.test.residuals <- log(data.test$cprice) - stepped.test
sqrt(mean(stepped.test.residuals^2))

## mse about the same, generalizes well
## we see that the simple linear model here works best.



# BETTENDORF method
# linear price equation
price.model1 <- lm(log(cprice/oprice) ~ I(bprice/oprice) + I(wprice/oprice) + I(incom/oprice) + I(tprice/oprice) + q1 + q2 + q3, data = data.train)

# view performance of model
summary(price.model1)

# automatically step through variable selection
step1 <- stepAIC(price.model1, direction="both")
summary(step1)


# select final price model for later use
price.final <- stepped.model



# Second stage

# predict the price and add back to dataframe
df$predprice <- exp(predict(price.final, newdata = df))

# recreate splits using same partition (to add predicted price)
data.train <- df[sample, ]
data.test<- df[!sample,]


# predict demand using predicted price

# naive linear function
demand1 <- lm(qu ~ predprice + tprice + oprice + incom + q1 + q2 + q3 + time, data = data.train)

# like earlier, we have seen that the instrumental variables tprice, oprice and income are highly correlated. 
# we can likely keep only one and remove the rest
# this corresponds to our stepAIC result.
summary(demand1)
plot(demand1)

stepped.demand <- stepAIC(demand1, direction="both")
summary(stepped.demand)
plot(stepped.demand)



# it makes sense that the interaction between the price of coffee and its substitue is important for prediction, so
# we include this here.
# Add interaction terms
demand2 <- lm(qu ~ predprice*tprice + q1 + q2 + q3 + time, data = data.train)
summary(demand2)

stepped.demand2 <- stepAIC(demand2, direction="both")
summary(stepped.demand2)
# but selection shows that it is not valuable as a predictor


# we can test if perhaps there is an exponential relationship to consider
# add quadratic terms
demand3 <- lm(qu ~ predprice + tprice + q1 + q2 + q3 + time + I(predprice)^2 + I(tprice)^2 + q1 + q2 + q3 + I(time)^2, data = data.train)
summary(demand3)

stepped.demand3 <- stepAIC(demand3, direction="both")
summary(stepped.demand3)
## but we converge to the simple linear model.



# bettendorf paper demand function
demand4 <- lm(qu ~ I(predprice/oprice) + I(tprice/oprice) + q1 + q2 + q3 + I(incom/oprice), data = data.train)
summary(demand4)

stepped.demand4 <- stepAIC(demand4, direction="both")
summary(stepped.demand4)


# Select our final demand model
demand.final <- stepped.demand4


# test instruments
# this will vary depending on which price model is selected. 

# isolate residuals for the whole data frame

demand.residuals <- df$qu - predict(demand.final, newdata = df)

test <- lm(demand.residuals ~ bprice + wprice, data = df)
summary(test)

## low r squared is good

hausmann <- pchisq(-0.0006868 * 1, 1, lower.tail = FALSE)


# We can see that since the Adjusted R-squared is negative, without even calculating the Hausmann statistic
# we can reject the null hypothesis.