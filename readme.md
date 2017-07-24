---
title: "Creative Analytics Sample"
author: "Neal Xie"
date: "Feburary 13, 2017"
output:
  html_document: default
  pdf_document: default
---

<style type="text/css">

body{ /* Normal  */
   font-size: 12px;
}
td {  /* Table  */
   font-size: 8px;
}
h1 { /* Header 1 */
 font-size: 24px;
 color: Black;
}
h2 { /* Header 2 */
 font-size: 18px;
 color: Black;
}
h3 { /* Header 3 */
 font-size: 14px;
 color: Black;
}
code.r{ /* Code block */
  font-size: 10px;
}
pre { /* Code block */
  font-size: 10px
}
</style>

# Background and Introduction

This is an analytics product sample for one of my internal teams which is dealing a Hospitality client, they have launched a new campaign with various creatives and tried to find out what are key properties to drive their website sales. Basically they want me to do the predictions on their website sales based on their historical creative data. They will use the prediction to guide their creative target in the future campaign.

Below is a **quick demo & model porotype** that I built for them based on **4 months'** historical data.

## Data Dictionary

**Total Conversions:** This is the target variable. Number corresponds to the number of creatives that drove audience to purchase on website.   
**Date:** This is a time variable with format like "2016-10-01".  
**Creative:** Unique creative name used to identify each creative look.  
**Creative Pixel Size:** Size of the creative.  
**Creative Field 4:** This is a field included some property information for creative.  
**Platform Type:** The platform the creative shows.  
**State/Region:** The state the creative shows.  
**Impressions:** Meeting of one message with one consumer, number of creative expressed to audience.

From the data dictionary that we could know we need to predict **Total Conversions** based on other variables.

Required Packages:

```r
require(readxl)    # For reading specific sheet from an excel file 
require(lubridate) # For making dealing with date variables
require(caTools)   # For stratify sampling
require(dplyr)     # For data wrangling
require(xgboost)   # For XGBoosting model
require(Matrix)    # For construting binary variables from catrgorical variables (One-hot encoding)
require(h2o)       # For Neural Network deep learning model
require(AER)       # For dispersion Test in Poisson GLMs
require(ggplot2)   # For data visualization
```


# Algorithm Selection
The time period is not long enough, and each day record has multiple obversions break down to different dimensions such as size, platform, language, etc. So it is not available to run time series analysis.

Dependent variable **Total Conversions** is a discrate variable, so it is a regression question.

The majority of independent variables are categorical variables, only **Impressions** is numeric count variable. So even we create dummy variables to repsent those categorical variables, the **Pure linear regression** may hard to handle this dataset. It is better to use **Regularized Regression**, **Regression Tree** or **Neural Network**. Tree or network based model also pretty robust to multicollinearity.

In order to improve the accuracy and prediction power of the model, I would like to use ensemble method, specifically, **XGBoost** and **H2O Deep Learning**, since it's high performance and speed. I will also use **Regulatized Poisson Regression** to create a base line model and to interpret the relationships between variables, since it's easy to explain not like other two models. They all have kind of "Black Box" systems.

# Data Preparation

```r
# Read sample data from target sheet of excel file
Disney_SP = read_excel("C:/_Project/Segmentation Product/Disney_SP.xlsx", sheet = "Merge")

# Look at the size of the data and data type of each variable
str(Disney_SP)

# Take a look at typical value of each variable
summary(Disney_SP)

# Data cleaning
Disney_SP$Size = Disney_SP$`Creative Pixel Size`
Disney_SP$Region = Disney_SP$`State/Region`
Disney_SP$Platform = Disney_SP$`Platform Type`
Disney_SP$Conversions = Disney_SP$`Total Conversions`
Disney_SP$Field = Disney_SP$`Creative Field 4`
```

# Feature Engineering
Since **XGBoost** only take **numerical variables**, there are lots of categorical variables in the dataset, so I need to do some feature transformations here:  
1. For categorical variables with less levels, I will convert them into binary dummy variables;  
2. For categorical variables with more levels, I will convert them into lower scales dummy variables depends on the distributions of Conversions;  
3. For date variable, I will create some new variables to catch seasonality.  

**H2O Deep Learning** and **Regulatized Poisson Regression** can catch **factor type** variables, so I will convet those categorical variables to **factor type**.

## Create new variables

```r
# Time related new variables
Disney_SP$Weekday = weekdays(Disney_SP$Date)
Disney_SP$Month = months(Disney_SP$Date)
Disney_SP$WeekofMonth = ifelse(ceiling(mday(Disney_SP$Date)/7)==5,4,ceiling(mday(Disney_SP$Date)/7))

# New language category
Disney_SP$Language = "ENG"
Disney_SP[grep("SPAN", Disney_SP$Field,fixed = TRUE),]["Language"] = "SPAN"

# New price symbol category
Disney_SP$PriceSymbol = "No"
Disney_SP[grep("$", Disney_SP$Field, fixed = TRUE),]["PriceSymbol"] = "Yes"

# New percentage symbol category
Disney_SP$PercentageSymbol = "No"
Disney_SP[grep("%", Disney_SP$Field, fixed = TRUE),]["PercentageSymbol"] = "Yes"

# New product symbol category
Disney_SP$ProductSymbol = "No"
Disney_SP[grep("Resort", Disney_SP$Field, fixed = TRUE),]["ProductSymbol"] = "Yes"
Disney_SP[grep("Hotel", Disney_SP$Field, fixed = TRUE),]["ProductSymbol"] = "Yes"

# New region category (CA takes the majority of population, so we only consider CA or Not CA)
Disney_SP$RegionCA = "No"
Disney_SP[Disney_SP$Region == "CA",]["RegionCA"] = "Yes"

# New creative type category
Disney_SP$CreativeType = "Image"
Disney_SP[grep("html", Disney_SP$Creative, fixed = TRUE),]["CreativeType"] = "HTMLBanner"
```

## Delete useless variables

```r
# Drop meaningless (in terms of modeling) variables from dataset
names(Disney_SP)
drops <- c("Date", "Creative", "Creative Pixel Size", "Creative Field 4", "Platform Type", "State/Region", 
           "Total Conversions", "Field", "Region")

Disney_SP <- Disney_SP[, !(names(Disney_SP) %in% drops)]
```

## Create dummy variables

```r
# Convert all categorical variables to factor type
Disney_SP$Month = as.factor(Disney_SP$Month)
Disney_SP$Weekday = as.factor(Disney_SP$Weekday)
Disney_SP$WeekofMonth = as.factor(Disney_SP$WeekofMonth)
Disney_SP$Size = as.factor(Disney_SP$Size)
Disney_SP$Platform = as.factor(Disney_SP$Platform)
Disney_SP$Language = as.factor(Disney_SP$Language)
Disney_SP$PriceSymbol = as.factor(Disney_SP$PriceSymbol)
Disney_SP$PercentageSymbol = as.factor(Disney_SP$PercentageSymbol)
Disney_SP$ProductSymbol = as.factor(Disney_SP$ProductSymbol)
Disney_SP$RegionCA = as.factor(Disney_SP$RegionCA)
Disney_SP$CreativeType = as.factor(Disney_SP$CreativeType)
```

## Sampling
Before we build model, we need to split the dataset into train (80%) and test(20%) first. We can use test data to do model evaluation after modeling.

```r
# Stratify sampling based on time (month)
train_rows = sample.split(Disney_SP$Month, SplitRatio = 0.80)
train = na.omit(Disney_SP[train_rows,])
test = na.omit(Disney_SP[!train_rows,])
```

## Exploratory Analysis
Let's describe our final data first

```r
# Describe train data
str(train)

summary(train)
```
Each variable has 5638 valid observations and their distributions seem quite reasonable. Our model assumes that these values, conditioned on the predictor variables, will be equal (or at least roughly so). We can display the summary statistics by using lookup tables by categorical variables.

```r
# Make lookup tables for categorical variables
Size_Impressions_lookup = train %>% group_by(Size) %>% summarise(Impressions_avg = mean(Impressions))
Size_Conversions_lookup = train %>% group_by(Size) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(Size_Impressions_lookup,Size_Conversions_lookup)

Language_Impressions_lookup = train %>% group_by(Language) %>% summarise(Impressions_avg = mean(Impressions))
Language_Conversions_lookup = train %>% group_by(Language) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(Language_Impressions_lookup,Language_Conversions_lookup)

PriceSymbol_Impressions_lookup = train %>% group_by(PriceSymbol) %>% summarise(Impressions_avg = mean(Impressions))
PriceSymbol_Conversions_lookup = train %>% group_by(PriceSymbol) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(PriceSymbol_Impressions_lookup,PriceSymbol_Conversions_lookup)

PercentageSymbol_Impressions_lookup = train %>% group_by(PercentageSymbol) %>% summarise(Impressions_avg = mean(Impressions))
PercentageSymbol_Conversions_lookup = train %>% group_by(PercentageSymbol) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(PercentageSymbol_Impressions_lookup,PercentageSymbol_Conversions_lookup)

ProductSymbol_Impressions_lookup = train %>% group_by(ProductSymbol) %>% summarise(Impressions_avg = mean(Impressions))
ProductSymbol_Conversions_lookup = train %>% group_by(ProductSymbol) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(ProductSymbol_Impressions_lookup,ProductSymbol_Conversions_lookup)

RegionCA_Impressions_lookup = train %>% group_by(RegionCA) %>% summarise(Impressions_avg = mean(Impressions))
RegionCA_Conversions_lookup = train %>% group_by(RegionCA) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(RegionCA_Impressions_lookup,RegionCA_Conversions_lookup)

CreativeType_Impressions_lookup = train %>% group_by(CreativeType) %>% summarise(Impressions_avg = mean(Impressions))
CreativeType_Conversions_lookup = train %>% group_by(CreativeType) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(CreativeType_Impressions_lookup,CreativeType_Conversions_lookup)

Platform_Impressions_lookup = train %>% group_by(Platform) %>% summarise(Impressions_avg = mean(Impressions))
Platform_Conversions_lookup = train %>% group_by(Platform) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(Platform_Impressions_lookup,Platform_Conversions_lookup)

# Make lookup tables for time variables (Seasonality)
Month_Impressions_lookup = train %>% group_by(Month) %>% summarise(Impressions_avg = mean(Impressions))
Month_Conversions_lookup = train %>% group_by(Month) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(Month_Impressions_lookup,Month_Conversions_lookup)

Weekday_Impressions_lookup = train %>% group_by(Weekday) %>% summarise(Impressions_avg = mean(Impressions))
Weekday_Conversions_lookup = train %>% group_by(Weekday) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(Weekday_Impressions_lookup,Weekday_Conversions_lookup)

WeekofMonth_Impressions_lookup = train %>% group_by(WeekofMonth) %>% summarise(Impressions_avg = mean(Impressions))
WeekofMonth_Conversions_lookup = train %>% group_by(WeekofMonth) %>% summarise(Conversions_avg = mean(Conversions))
inner_join(WeekofMonth_Impressions_lookup,WeekofMonth_Conversions_lookup)
```
It looks like conversions do have some sensitive to month, weekday and week of month, so those three variables should be able to catch this relationship.

```r
# Distribtion of conversions
ggplot(train, aes(Conversions)) + geom_histogram(binwidth=4)
```

![plot of chunk Distribution of conversions](figure/Distribution of conversions-1.png)

We can see from graph above, the target variable falls into a **Poisson Distribution**. Now let's build the model.

## Build the model
### Regulatized Poisson Regression

```r
# Fit a GLM model with poisson option
pr.fit = glm(Conversions~., data = train, family = poisson)

# Summarise the model
summary(pr.fit)

# Run a dispersion test for this model
dispersiontest(pr.fit, trafo = 1)
```
As we can see from model above, the overdispersion corresponds to *alpha > 0*. So we need to use **Quasi Poisson Regression** to analyze.

```r
# Fit a Quasi poisson regression model
qpr.fit = glm(Conversions~., data = train, family = quasipoisson)

# Summarise the quasi poisson regression model
summary(qpr.fit)

# Predict on test data
qpr.fit_pred = predict(qpr.fit, newdata = test, family = quasipoisson)

# Round the conversions prediction into integer
qpr.fit_pred = round(qpr.fit_pred)

# Predicted conversions should not less than 0.
qpr.fit_pred <- ifelse(qpr.fit_pred > 0, qpr.fit_pred ,0)

# MSE
sum((qpr.fit_pred-test$Conversions)^2)/nrow(test)

# Pseudo R2
1-(qpr.fit$deviance/qpr.fit$null.deviance)
```
### XGBoost

```r
# Create the sparse matrix for both train and test datasets
train_sparse_matrix = sparse.model.matrix(Conversions~.-1, data = train)
test_sparse_matrix = sparse.model.matrix(Conversions~.-1, data = test)

# Create the dependent vector
train_output_vector = as.matrix(train[, "Conversions"])
test_output_vector = as.matrix(test[, "Conversions"])

# Set parameters
param = list("booster" = "gblinear",
             "objective" = "reg:linear",
             "nthread" = 4,
             "eta" = 0.1,
             "subsample" = 0.8,
             "min_child_weight" = 2,
             "max_depth" = 15
             )

# Construct a watch list, use test error to measure the model quality to avoid overfitting during model training (similar to cross validation)
dtrain <- xgb.DMatrix(data = train_sparse_matrix, label=train_output_vector)
dtest <- xgb.DMatrix(data = test_sparse_matrix, label=test_output_vector)
watchlist <- list(train=dtrain, test=dtest)

# Fit a xgboost model
bst.fit = xgb.train(params = param, data = dtrain, nround = 75, watchlist = watchlist)
```

```
## [1]	train-rmse:7.830991	test-rmse:7.769921 
## [2]	train-rmse:7.203764	test-rmse:7.270766 
## [3]	train-rmse:6.795120	test-rmse:6.973509 
## [4]	train-rmse:6.480366	test-rmse:6.757858 
## [5]	train-rmse:6.220663	test-rmse:6.588202 
## [6]	train-rmse:6.004142	test-rmse:6.452991 
## [7]	train-rmse:5.822218	test-rmse:6.344804 
## [8]	train-rmse:5.669683	test-rmse:6.258747 
## [9]	train-rmse:5.541645	test-rmse:6.190506 
## [10]	train-rmse:5.434024	test-rmse:6.136639 
## [11]	train-rmse:5.343580	test-rmse:6.094400 
## [12]	train-rmse:5.267695	test-rmse:6.061541 
## [13]	train-rmse:5.203964	test-rmse:6.036156 
## [14]	train-rmse:5.150528	test-rmse:6.016798 
## [15]	train-rmse:5.105343	test-rmse:6.002092 
## [16]	train-rmse:5.067226	test-rmse:5.991120 
## [17]	train-rmse:5.034877	test-rmse:5.983097 
## [18]	train-rmse:5.007409	test-rmse:5.977389 
## [19]	train-rmse:4.983972	test-rmse:5.973464 
## [20]	train-rmse:4.963944	test-rmse:5.970909 
## [21]	train-rmse:4.946689	test-rmse:5.969412 
## [22]	train-rmse:4.931780	test-rmse:5.968738 
## [23]	train-rmse:4.918876	test-rmse:5.968706 
## [24]	train-rmse:4.907623	test-rmse:5.969148 
## [25]	train-rmse:4.897778	test-rmse:5.969944 
## [26]	train-rmse:4.889130	test-rmse:5.970968 
## [27]	train-rmse:4.881479	test-rmse:5.972169 
## [28]	train-rmse:4.874710	test-rmse:5.973509 
## [29]	train-rmse:4.868690	test-rmse:5.974897 
## [30]	train-rmse:4.863284	test-rmse:5.976346 
## [31]	train-rmse:4.858429	test-rmse:5.977812 
## [32]	train-rmse:4.854056	test-rmse:5.979277 
## [33]	train-rmse:4.850098	test-rmse:5.980723 
## [34]	train-rmse:4.846511	test-rmse:5.982127 
## [35]	train-rmse:4.843247	test-rmse:5.983497 
## [36]	train-rmse:4.840257	test-rmse:5.984828 
## [37]	train-rmse:4.837516	test-rmse:5.986119 
## [38]	train-rmse:4.834983	test-rmse:5.987390 
## [39]	train-rmse:4.832656	test-rmse:5.988596 
## [40]	train-rmse:4.830496	test-rmse:5.989783 
## [41]	train-rmse:4.828488	test-rmse:5.990941 
## [42]	train-rmse:4.826639	test-rmse:5.992033 
## [43]	train-rmse:4.824912	test-rmse:5.993093 
## [44]	train-rmse:4.823305	test-rmse:5.994109 
## [45]	train-rmse:4.821805	test-rmse:5.995094 
## [46]	train-rmse:4.820405	test-rmse:5.996039 
## [47]	train-rmse:4.819099	test-rmse:5.996930 
## [48]	train-rmse:4.817876	test-rmse:5.997793 
## [49]	train-rmse:4.816728	test-rmse:5.998638 
## [50]	train-rmse:4.815641	test-rmse:5.999471 
## [51]	train-rmse:4.814617	test-rmse:6.000294 
## [52]	train-rmse:4.813662	test-rmse:6.001062 
## [53]	train-rmse:4.812767	test-rmse:6.001794 
## [54]	train-rmse:4.811918	test-rmse:6.002521 
## [55]	train-rmse:4.811123	test-rmse:6.003201 
## [56]	train-rmse:4.810368	test-rmse:6.003886 
## [57]	train-rmse:4.809657	test-rmse:6.004538 
## [58]	train-rmse:4.808985	test-rmse:6.005178 
## [59]	train-rmse:4.808347	test-rmse:6.005818 
## [60]	train-rmse:4.807749	test-rmse:6.006411 
## [61]	train-rmse:4.807181	test-rmse:6.006997 
## [62]	train-rmse:4.806645	test-rmse:6.007561 
## [63]	train-rmse:4.806137	test-rmse:6.008115 
## [64]	train-rmse:4.805655	test-rmse:6.008655 
## [65]	train-rmse:4.805198	test-rmse:6.009188 
## [66]	train-rmse:4.804763	test-rmse:6.009725 
## [67]	train-rmse:4.804354	test-rmse:6.010221 
## [68]	train-rmse:4.803967	test-rmse:6.010692 
## [69]	train-rmse:4.803600	test-rmse:6.011158 
## [70]	train-rmse:4.803250	test-rmse:6.011618 
## [71]	train-rmse:4.802918	test-rmse:6.012067 
## [72]	train-rmse:4.802605	test-rmse:6.012494 
## [73]	train-rmse:4.802305	test-rmse:6.012933 
## [74]	train-rmse:4.802020	test-rmse:6.013361 
## [75]	train-rmse:4.801750	test-rmse:6.013767
```

```r
# Predict on test data
bst.fit_pred = predict(bst.fit, test_sparse_matrix)

# Round the conversions prediction into integer
bst.fit_pred = round(bst.fit_pred)

# Predicted conversions should not less than 0.
bst.fit_pred <- ifelse(bst.fit_pred > 0, bst.fit_pred ,0)

# MSE
sum((bst.fit_pred-test$Conversions)^2)/nrow(test)
```

```
## [1] 36.11214
```

```r
r2 = sum((bst.fit_pred-mean(test$Conversions))^2)/sum((test$Conversions-mean(test$Conversions))^2)
print(paste("R square is ", r2))
```

```
## [1] "R square is  0.586958245924861"
```

```r
# Find what the actual tree looks like
model = xgb.dump(bst.fit, with_stats = T)

# Print top 10 nodes of the model
model[1:10]
```

```
##  [1] "booster[0]"  "bias:"       "0.70208"     "weight:"     "0.000500502"
##  [6] "-0.00133521" "0.370598"    "-0.0918794"  "0.474507"    "-2.36395"
```

```r
# Get the feature real names
names = dimnames(train_sparse_matrix)[[2]]

# Compute feature importance matrix
importance_matrix = xgb.importance(names, model=bst.fit)

# Plot features
xgb.plot.importance(importance_matrix, rel_to_first = F, main = "Relative importance")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)



