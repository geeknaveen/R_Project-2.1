############# Project-2.1 ############## 

# Problem Statement 
# Imagine that the CEO of a DVD player sales company approaches you in order to predict the 
# sale of DVDs. He also provides you the data such as the advertising budget (in thousands), sales 
# (in thousands), number of times the song is played on the radio channel, Radio Mirchi per week 
# and the attractiveness of the brand (rated on a scale of 1 to 10 by an independent agency). 

# Objective 
# Create a Linear Regression Model for DVD sales data set. The data set contains the following 
# details: 
# Advertising: The budget spent on advertising. 
# Sales: Number of copies sold 
# Plays: Number of plays on Radio Mirchi  
# Attractiveness: Attractiveness of the brand (rating scale from 1 to 10; 1 being the worst 
#                                              and 10 being the best) 

# getwd()
# setwd ("F:/ACADGILD/Business Analytics With R/ASSIGNMENTS/_Project-2")

# install.packages("readxl")
library("readxl")
sales_DVD <- read_excel("F:/ACADGILD/Business Analytics With R/ASSIGNMENTS/_Project-2/Sales_dataset.xlsx")
View(sales_DVD)

# install.packages("corrplot")
library(corrplot)

# Calculating Correlation
# names(sales_DVD)
cor_relation <- cor(sales_DVD)
head(round(cor_relation,2))
corrplot(cor_relation, method= "number")

#### Developing Regression Model #####
 
# Splitting the sales_DVD into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(sales_DVD$sales, SplitRatio = 2/3)
# training_set = subset(sales_DVD, split == TRUE)
# test_set = subset(sales_DVD, split == FALSE)

# Regressorting Linear Regression to the Training set
regressor = lm(formula = sales ~ advertise + plays,
               data = sales_DVD)
par(mfrow=c(2,2))
plot(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = sales_DVD)
head(y_pred)

# Plotting Predicted VS Actual values
plot(y_pred, sales_DVD$sales,xlab="Predicted",ylab="Actual")
title(main="Predicted vs Actual Values", col.main="Blue", font.main=4)
abline(a=0,b=1,col = "red")

# Extracting Coefficients
summary(regressor)$coeff
anova(regressor)

#### Variable Selection Methods ####
#Stepwise Selection based on AIC
# install.packages('MASS')
library(MASS)
step <- stepAIC(regressor, direction="both")
summary(step)

#Backward Selection based on AIC
step <- stepAIC(regressor, direction="backward")
summary(step)

#Forward Selection based on AIC
step <- stepAIC(regressor, direction="forward")
summary(step)






