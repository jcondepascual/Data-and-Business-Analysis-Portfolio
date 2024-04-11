## Sample 4 - Sales research with R Studio

###############################################################################

## Scenario

## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# EDA using R-----------------------------------------------------------

# 1. Load and explore the data.

# Install and import Tidyverse.

install.packages('tidyverse')
library(tidyverse)

# Import the data set.
turtle_sales <- read.csv(file.choose(), header=T)

# Print the data frame.
turtle_sales
summary (turtle_sales)
str(turtle_sales)

View(turtle_sales)

# Explore NA and duplicates.

is.na(turtle_sales)
apply(is.na(turtle_sales), 2, which)

# Only two NA in 'Year' column, rows 180 and 258. Remove these columns in next 
# step so no action taken.

duplicated(turtle_sales)

# No duplicated observations.

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns (Ranking, Year, Genre, Publisher). 

turtle_sales2 <- subset(turtle_sales, select=-c(Ranking, Year, Genre, Publisher))

# View the data frame and structure.

turtle_sales2
str(turtle_sales2)

# View the descriptive statistics.

summary(turtle_sales2)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots.
# Create scatterplots.

qplot(Global_Sales, EU_Sales, data=turtle_sales2,
      main='Scatterplot global sales vs EU sales')

qplot(Global_Sales, NA_Sales, data=turtle_sales2,
      main='Scatterplot global sales vs NA sales')

qplot(EU_Sales, NA_Sales, data=turtle_sales2,
      main='Scatterplot EU sales vs NA sales')

qplot(Global_Sales, Product, data=turtle_sales2,
      main='Scatterplot gobal sales by product')

## 2b) Histograms.
# Create histograms.

qplot(Global_Sales, bins=25, data=turtle_sales2, main='Histogram global sales')
qplot(EU_Sales, bins=25, data=turtle_sales2, main='Histogram EU sales')
qplot(NA_Sales, bins=25, data=turtle_sales2, main='Histogram NA sales')
qplot(Product, bins=25, data=turtle_sales2, main='Histogram product by ID')

## 2c) Boxplots.
# Create boxplots.

qplot(Global_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot global sales', geom='boxplot')

qplot(EU_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot EU sales', geom='boxplot')

qplot(NA_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot NA sales', geom='boxplot')

qplot(Product, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot Product ID sales', geom='boxplot')



###############################################################################

# 3. Observations and insights.

# Relationship between partial sales and global sales.
# Skewness:

# Install the moments package and load the library.
install.packages('moments') 
library(moments)

skewness(turtle_sales2$Global_Sales)
skewness(turtle_sales2$EU_Sales)
skewness(turtle_sales2$NA_Sales)

# All skewed to the right or positively skewed.

# Some outliers to watch (on recommendations).


###############################################################################
###############################################################################


# Week 5: Manipulating data using R -------------------------------

# 1. Load and explore the data.

# View data frame created in Week 4.

turtle_sales2

# View head qnd structure.

head(turtle_sales2)
str(turtle_sales2)


# Check output: Determine the min, max, and mean values.
.
min(turtle_sales2$Global_Sales)
min(turtle_sales2$EU_Sales)
min(turtle_sales2$NA_Sales)

max(turtle_sales2$Global_Sales)
max(turtle_sales2$EU_Sales)
max(turtle_sales2$NA_Sales)

mean(turtle_sales2$Global_Sales)
mean(turtle_sales2$EU_Sales)
mean(turtle_sales2$NA_Sales)


# View the descriptive statistics.

summary(turtle_sales2)


###############################################################################

# 2. Determine the impact on sales per product_id.

# 2a) Group data based on Product and determine the sum per Product.

turtle_sales_product <- turtle_sales %>% group_by(Product) %>%
  summarise(across(.cols = c('NA_Sales', 'EU_Sales', 'Global_Sales'), ~sum(.)))

# View the data frame.

as_tibble(turtle_sales_product)

# Summary and View of the new data frame.

summary(turtle_sales_product)

View(turtle_sales_product)

# 2b) Create scatterplots, histograms and boxplots to gain insights into 
# the sales data.

# Scatterplots.

qplot(Global_Sales, EU_Sales, data=turtle_sales_product,
      main='Scatterplot global sales vs EU sales grouped by product')

qplot(Global_Sales, NA_Sales, data=turtle_sales_product,
      main='Scatterplot global sales vs NA sales grouped by product')

qplot(EU_Sales, NA_Sales, data=turtle_sales_product,
      main='Scatterplot EU sales vs NA sales grouped by product')


# Histograms.

qplot(Global_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')
qplot(EU_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')
qplot(NA_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')

# Boxplots.

qplot(Global_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot global sales grouped by product', geom='boxplot')

qplot(EU_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot EU sales grouped by product', geom='boxplot')

qplot(NA_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot NA sales grouped by product', geom='boxplot')


# 3. Determine the normality of the data set.

# 3a) Create Q-Q Plots.
# Create Q-Q Plots.

qqnorm(turtle_sales_product$Global_Sales)
# Add a reference line:
qqline(turtle_sales_product$Global_Sales, col='blue')

qqnorm(turtle_sales_product$EU_Sales)
# Add a reference line:
qqline(turtle_sales_product$EU_Sales, col='blue')

qqnorm(turtle_sales_product$NA_Sales)
# Add a reference line:
qqline(turtle_sales_product$NA_Sales, col='blue')



# 3b) Perform Shapiro-Wilk test.
# Install and import Moments.

install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk test.

shapiro.test((turtle_sales_product$Global_Sales))
shapiro.test((turtle_sales_product$EU_Sales))
shapiro.test((turtle_sales_product$NA_Sales))

# Our p-values are way below 0.05 (Global 2.2e-16, EU 2.987e-16, NA 2.2e-16),
# so the data provided is not normally distributed.

# 3c) Determine Skewness and Kurtosis.

# Skewness and Kurtosis.

skewness(turtle_sales_product$Global_Sales)
skewness(turtle_sales_product$EU_Sales)
skewness(turtle_sales_product$NA_Sales)

kurtosis(turtle_sales_product$Global_Sales)
kurtosis(turtle_sales_product2$EU_Sales)
kurtosis(turtle_sales_product2$NA_Sales)


# 3d) Correlation between the sales data columns.

# Correlation between the sales data columns.

round(cor(turtle_sales_product), digits=2)

# Strong correlation between Global_sales and NA_sales (0.92), Global_sales and
# EU_sales (0.85). Less good correlation between EU_sales and NA_sales.


###############################################################################

# 3. Plot the data.
# Create plots to gain insights into data.

# Scatterplot global sales vs NA sales.

ggplot(data=turtle_sales_product,mapping=aes(x=Global_Sales, y=NA_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("Global sales") +
  scale_y_continuous("NA sales") +
  labs(title="Turtle Games global sales vs NA sales (Million GBP)")


# Scatterplot global sales vs Europe sales.

ggplot(data=turtle_sales_product,mapping=aes(x=Global_Sales, y=EU_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("Global sales") +
  scale_y_continuous("EU sales") +
  labs(title="Turtle Games global sales vs EU sales(Million GBP)")

# Scatterplot global sales vs Europe sales.

ggplot(data=turtle_sales_product,mapping=aes(x=EU_Sales, y=NA_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("EU sales") +
  scale_y_continuous("NA sales") +
  labs(title="Turtle Games EU sales vs NA sales(Million GBP)")


# Scatterplot Product vs global sales (linear).

ggplot(data=turtle_sales_product,mapping=aes(x=Product, y=Global_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("GProduct ID") +
  scale_y_continuous("Global sales") +
  labs(title="Turtle Games product global sales (Million GBP)")

# Scatterplot Product vs global sales (non linear).

ggplot(data=turtle_sales_product,mapping=aes(x=Product, y=Global_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(color='orange') +
  scale_x_continuous("GProduct ID") +
  scale_y_continuous("Global sales") +
  labs(title="Turtle Games product global sales (Million GBP)")

# Non linear seems to be the best for product/global sales.

###############################################################################

# 4. Observations and insights.

# Scatterplots very useful to understand sales variables relationships.
# Strong correlation between Global_sales and NA_sales (0.92), Global_sales and
# EU_sales (0.85). Less good correlation between EU_sales and NA_sales.
.


###############################################################################
###############################################################################

# Week 6: Making recommendations to the business using R------------------------

# 1. Load and explore the data.
# View data frame created in Week 5.

View(turtle_sales_product)

# Determine a summary of the data frame.
summary(turtle_sales_product)

###############################################################################

# 2. Create a simple linear regression model.
# 2a) Determine the correlation between columns.

cor(turtle_sales_product)

# Create a linear regression model.

model1 <- lm(NA_Sales ~ Global_Sales, data=turtle_sales_product)
model2 <- lm(EU_Sales ~ Global_Sales, data=turtle_sales_product)
model3 <- lm(EU_Sales ~ NA_Sales, data=turtle_sales_product)
model4 <- lm(NA_Sales ~ EU_Sales, data=turtle_sales_product)

# View the model.
model1
summary(model1)
model2
summary(model2)
model3
summary(model3)
model4
summary(model4)


# 2b) Create a plot (simple linear regression).
# Basic visualisation.

plot(turtle_sales_product$NA_Sales, turtle_sales_product$Global_Sales)
abline(coefficients(model1))

plot(turtle_sales_product$EU_Sales, turtle_sales_product$Global_Sales)
abline(coefficients(model2))

plot(turtle_sales_product$EU_Sales, turtle_sales_product$NA_Sales)
abline(coefficients(model3))

plot(turtle_sales_product$NA_Sales, turtle_sales_product$EU_Sales)
abline(coefficients(model4))

###############################################################################

# 3. Create a multiple linear regression model.

# Select only numeric columns.

names(turtle_sales_product)
turtle_sales_noproduct <- subset(turtle_sales_product, select=-c(Product))

str(turtle_sales_noproduct)
summary(turtle_sales_noproduct)

# Determine the correlation between the sales columns.

round(cor(turtle_sales_noproduct), digits = 2)


# Multiple linear regression model.

modelA = lm(Global_Sales~NA_Sales+EU_Sales, data=turtle_sales_noproduct)
summary(modelA)


# If we include the variable "Product'.

modelB = lm(Global_Sales~NA_Sales+EU_Sales+Product, data=turtle_sales_product)
summary(modelB)

# Model B including the variable "Product' is slightly more robust
# (AdjR2=0.97 vs 0.96 Model A).


###############################################################################

# 4. Predictions based on given values.
# Compare with observed values for a number of records.

View(turtle_sales_product)

# We use model A as we are given just two independent variables data.

# A. NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.

NA_Sales <- c(34.02)
EU_Sales <- c(23.80)

sales1 <- data.frame(NA_Sales, EU_Sales)

# Predicted Global_Sales value.
predict(modelA, newdata = sales1)

# Predicted value 68.056 vs observation value 67.85: good .

# B. NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
# Values not on provided data set.
# Most similar 3.94/1.28 with observed Global_sales value 8.36.

NA_Sales <- c(3.94)
EU_Sales <- c(1.28)

sales2 <- data.frame(NA_Sales, EU_Sales)

# Predicted Global_Sales value.
predict(modelA, newdata = sales2)

# Predicted value 7.03 vs observation value 8.36: average.

# C. NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65, observed value 4.32.

NA_Sales <- c(2.73)
EU_Sales <- c(0.65)

sales3 <- data.frame(NA_Sales, EU_Sales)

# Predicted Global_Sales value.
predict(modelA, newdata = sales3)

# Predicted value 4.90 vs observation value 4.32: good.

# D. NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
# Values not on provided data set.
# Most similar 2.27/2.30 with observed Global_sales value 5.60.

NA_Sales <- c(2.27)
EU_Sales <- c(2.30)

sales4 <- data.frame(NA_Sales, EU_Sales)

# Predicted Global_Sales value.
predict(modelA, newdata = sales4)

# Predicted value 6.36 vs observation value 5.60: average.

# E. NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52, Global sales 23.21.

NA_Sales <- c(22.08)
EU_Sales <- c(0.52)

sales <- data.frame(NA_Sales, EU_Sales)

# Predicted Global_Sales value.
predict(modelA, newdata = sales)

# Predicted value 26.62 vs observation value 23.21: average.

###############################################################################

# 5. Observations and insights.

# Please check the attached report for insights, conclusions and 
# company recommendations.

# Thank you for reading.

###############################################################################
###############################################################################




