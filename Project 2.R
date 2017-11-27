library(MatchIt)
library(lfe)
library(ROCR)
library(pROC)
library(ggplot2)
library(caret)
library(plm)
library(data.table)

df <- read.csv('C:/Users/Shane/Documents/NUS Documents/NUS Year 4/DSC5101/DiD_data.csv')

summary(df)

# clean bank codes as factor rather than numeric
df$rssd9001 <- as.factor(df$rssd9001)
df$treat_3_b_avg <- as.factor(df$treat_3_b_avg)


# we attempt to try and create a baseline model


##
## PART 1
## 
##

# propensity score matching


# remove rows with na's from dataframe for matching
df.nomiss <- na.omit(df)

# select only the first quarter
df.nomiss <- df.nomiss[df.nomiss$rssd9999 == '20040930',]

# predictive modelling convention
# split into train, validation and test
# can be used to select best model for logistic regression part

set.seed(2134)

sample <- sample(c(0,1,2),nrow(df.nomiss),prob = c(0.6,0.2,0.2),replace = TRUE)

data.train <- df.nomiss[sample == 0,]
data.validation <- df.nomiss[sample == 1,]
data.test <- df.nomiss[sample == 2,]


# we don't use average trading ratio as a predictor for obvious reasons
# we also don't use bankcode
propensity <- glm(treat_3_b_avg ~ rssd9999 + after_DFA_1 + dep_roa1 
                  + dep_leverage + dep_lnassets + dep_creditrisk_total3 + dep_cir 
                  + dep_depositratio + dep_loans_REratio + dep_liquidity 
                  + dep_cpp_bankquarter, family = binomial, data = data.train)

# see performance of model
summary(propensity)

data.validation$pred <- predict(propensity, newdata = data.validation, type = "response")
perf <- prediction(data.validation$pred, data.validation$treat_3_b_avg)

# plot ROC curve
curve <- performance(perf, "prec", "rec")
plot(curve)
auc.perf <- performance(perf, measure = 'auc')
auc.perf

# auc is PERFECT

# try another model for comparison
propensity1 <- glm(treat_3_b_avg ~ rssd9999 + after_DFA_1 + dep_roa1*dep_lnassets*dep_creditrisk_total3 
                  + dep_leverage + dep_cir 
                  + dep_depositratio + dep_loans_REratio + dep_liquidity 
                  + dep_cpp_bankquarter, family = binomial, data = data.train)

# see performance of model
summary(propensity1)

data.validation$pred <- predict(propensity1, newdata = data.validation, type = "response")
perf <- prediction(data.validation$pred, data.validation$treat_3_b_avg)

# plot ROC curve
curve <- performance(perf, "prec", "rec")
plot(curve)
auc.perf <- performance(perf, measure = 'auc')
auc.perf

# the precision recall curve is pretty weirdly shaped, but the auc is okay

# try another model (again) for comparison
propensity2 <- glm(treat_3_b_avg ~ after_DFA_1 + dep_lnassets + dep_lnassets*dep_creditrisk_total3 
                   + dep_cir 
                   + dep_depositratio + dep_loans_REratio 
                   + dep_cpp_bankquarter, family = binomial, data = data.train)

# see performance of model
summary(propensity2)

data.validation$pred <- predict(propensity2, newdata = data.validation, type = "response")
perf <- prediction(data.validation$pred, data.validation$treat_3_b_avg)

# plot ROC curve
curve <- performance(perf, "prec", "rec")
plot(curve)
auc.perf <- performance(perf, measure = 'auc')
auc.perf

# once again the auc is perfect
# has a lower AIC so this is a better model though

# show performance just for completeness

data.test$pred <- predict(propensity2, newdata = data.test, type = "response")
perf <- prediction(data.test$pred, data.test$treat_3_b_avg)

# plot ROC curve
curve <- performance(perf, "prec", "rec")
plot(curve)
auc.perf <- performance(perf, measure = 'auc')
auc.perf


# use same model in matching
matched <- matchit(treat_3_b_avg ~ after_DFA_1 + dep_lnassets + dep_lnassets*dep_creditrisk_total3 
                   + dep_cir 
                   + dep_depositratio + dep_loans_REratio 
                   + dep_cpp_bankquarter, method = "nearest", data = df.nomiss, ratio = 3)

summary(matched)

df.matched <- match.data(matched)




##
##
## PART 2 - We try to replicate the baseline models as shown in the paper. 
##
##

# run linear regression to see effect of treatment
model <- lm(bhc_avgtradingratio ~ treat_3_b_avg + after_DFA_1 + after_DFA_1:treat_3_b_avg, data = df.matched)

# it is interesting to note that using a dummy for treatment vs control actually does not show a relationship in the
# direction we expect. This is the opposite of the findings in the paper.

# This is probably due to the fact that too much variation is suppressed by using a 
# 3% cut off.
# this also implies that the assumption that the baseline bias is constant is probably wrong.


# with controls
model1 <- lm(bhc_avgtradingratio ~ treat_3_b_avg + after_DFA_1 + after_DFA_1:treat_3_b_avg + dep_roa1 
             + dep_leverage + dep_lnassets + dep_creditrisk_total3 + dep_cir 
             + dep_depositratio + dep_loans_REratio + dep_liquidity 
             + dep_cpp_bankquarter , data = df.matched)

# with fixed effects accounted for
model2 <- felm(bhc_avgtradingratio ~ treat_3_b_avg*after_DFA_1 + dep_roa1 
               + dep_leverage + dep_lnassets + dep_creditrisk_total3 + dep_cir 
               + dep_depositratio + dep_loans_REratio + dep_liquidity 
               + dep_cpp_bankquarter| rssd9001 + rssd9999  , data = df.matched)

summary(model)

summary(model1)
# we are interested in treat_3_b_avg:after_DFA_1
# but it's not significant AND not correct sign

summary(model2)

# notice that if we include fixed effects for all time and banks, the variables for post/pre treatment and
# for treatment control become meaningless, because we are already controlling for each individual time period
# and bank effect. 
# so we only keep the interaction term

# significant and correct sign, so better model


##
##
## PART 3 - VERIFYING THE BASELINE BIAS ASSUMPTION
##
##

# verify the predictive power of covariates in controlling baseline bias

# select only the controls
df.subset <- df[df$treat_3_b_avg == 0,]

# encode time as factors for fixed effects
# cannot include bank fixed effects as we are separating by bank
df.subset$rssd9001 <- as.character(df.subset$rssd9001)
df.subset$rssd9999 <- as.factor(df.subset$rssd9999)

# split controls into group 1 and 2 randomly by bank
# we assume bank fixed effects cancel out by random assignment


sample <- sample(unique(df.subset$rssd9001))

df.control1 <- df.subset[df.subset$rssd9001 %in% sample[1:length(sample)/2],]
df.control2 <- df.subset[!df.subset$rssd9001 %in% sample[1:length(sample)/2],]


# we manually code in fixed effects here because plm or felm doesn't allow us to make predictions easily
# this model could of course be improved but that is not that important - important is how 
# generalizable it is to the other controls
# overfitting would be counterproductive
control1predict <- lm(bhc_avgtradingratio ~ after_DFA_1 + dep_roa1 
                            + dep_leverage + dep_lnassets + dep_creditrisk_total3 + dep_cir 
                            + dep_depositratio + dep_loans_REratio + dep_liquidity 
                            + dep_cpp_bankquarter + rssd9999, data = df.control1)

summary(control1predict)
control1predict$xlevels

# adjusting levels because model is dumb at automatically detecting these
control1predict$xlevels <- union(control1predict$xlevels, levels(df.control2$rsd9999))


post_predict <- predict(control1predict, newdata = df.control2)
newdataset <- data.frame(obs = df.control2$bhc_avgtradingratio, pred = post_predict)
defaultSummary(newdataset)

# relatively comparable r squared yay
# both are really bad models though lul


##
##
## PART 4 - CONTINUOUS VARIABLES
##
##


# data preparation
# create variable for degree of excess trading ratio
# average exceed for period before rule implementation

dt <- df[df$after_DFA_1 == 0,]
dt <- data.table(dt)
exceed <- dt[,list(bhc_exceed = mean(bhc_avgtradingratio, na.rm = TRUE)), by = rssd9001]
exceed[,2] <- exceed[,2] - 0.03

exceed <- as.data.frame(as.matrix(exceed))
exceed$bhc_exceed <- sapply(exceed$bhc_exceed, max, 0)
exceed$bhc_exceed <- as.numeric(exceed$bhc_exceed)

df <- merge(df, exceed, by = "rssd9001")


# create time since announcement variable
df$rssd9999_working <- as.numeric(as.Date(as.character(df$rssd9999), format = "%Y%m%d"))

# takes bloody long to run for some reason 
# df$time_announcement <- as.numeric(lapply(df$rssd9999, difftime, "2010-09-30"))

# find number of days since annoucement as variable
df$time_announcement <- as.numeric(lapply(df$rssd9999_working - as.numeric(as.Date("2010-09-30")),max, 0))


# with fixed effects accounted for
model.continuous <- felm(bhc_avgtradingratio ~ bhc_exceed*time_announcement + dep_roa1 
               + dep_leverage + dep_lnassets + dep_creditrisk_total3 + dep_cir 
               + dep_depositratio + dep_loans_REratio + dep_liquidity 
               + dep_cpp_bankquarter| rssd9001 + rssd9999  , data = df)

summary(model.continuous)

# we notice that for some reason bhc_exceed and time announcement become na
# this could be due to a multicollinearity issue, which makes sense because bhc exceed and time announcement are
# derivatives of the bank fixed effect and the time fixed effect. 
# so, we only look at the interaction term. 

