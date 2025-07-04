#load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
library(lubridate)
library(broom)
library(ggplot2)
library(forecast)
library(data.table)
library(scales)
library(caret)
library(randomForest)
library(gbm)
library(glmnet)
library(xgboost)
library(forecast)

# Load CDC chronic diseases dataset into df 
df <- read.csv("C:/Users/19258/Downloads/Chronic Obstructive Pulmonary Disease_data.csv")


# filter data by 'stratificationcategory1 = gender' and 'stratificationcategory1 = race/ethnicity'
# did not include 'stratificationcategory1 = overall' since this would double count the number of deaths from obstructive pulmonary disease and not a specific enough predictor
# filtering by gender and race/ethnicity sets stratification1 to male, female, hispanic, asian, etc.
# by using both gender and race/ethnicity, may be double counting number of deaths as well (something to keep in mind)
# filter by Question = Mortality with chronic obstructive pulmonary disease as underlying or contributing cause among adults aged >= 45 years
# select relevant columns only and rename them (decided to leave out question col)


filtered_df <- df %>% 
  select(c('YearStart', 'LocationDesc','Topic', 'DataValue',"StratificationCategory1","Stratification1")) %>%
  filter(StratificationCategory1 == "Gender" | StratificationCategory1 == "Race/Ethnicity") %>% 
  rename(
    Year = YearStart,
    State = LocationDesc,
    Disease = Topic,
    Number_Diagnosed = DataValue
  )%>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year, State) %>%
  mutate(Number_Diagnosed = as.numeric(Number_Diagnosed))

# Remove rows with na in 'number_diagnosed' 
filtered_df <- filtered_df %>% filter(!is.na(Number_Diagnosed))

dim(filtered_df)


# Load Medicaid expenditure into df1
df1 <- read.csv('C:/Users/19258/Downloads/MEDICAID_AGGREGATE20.csv')

# Select relevant columns only and rename them 
# Select years in common with filtered_df 
filtered_df1 <- df1 %>% 
  select(State_Name, Y2008, Y2009,Y2010,Y2011,Y2012,Y2013,Y2014,Y2015,Y2016,Y2017,Y2018,Y2019,Y2020) %>% 
  rename(State = State_Name, '2008' = Y2008, '2009' = Y2009, '2010' = Y2010, '2011' = Y2011, '2012' = Y2012, '2013'=Y2013, '2014'=Y2014, '2015'=Y2015, '2016'=Y2016, '2017'=Y2017, '2018'=Y2018, '2019'=Y2019, '2020'=Y2020)

# convert empty strings to NA so you can treat them as missing
filtered_df1$State[filtered_df1$State == ""] <- NA
sum(is.na(filtered_df1$State))  # now should count these as missing

# Remove rows with na 
medicaid_df <- filtered_df1 %>% drop_na()

# Reshape medicaid_df to long format and group by state then sum all medicaid expenses 
medicaid_df <- medicaid_df %>% 
  pivot_longer(cols = starts_with('20'), names_to = "Year", values_to="Expenses") %>%
  group_by(State, Year) %>% 
  summarise(Medicaid_Expenses = sum(Expenses), .groups='drop') %>% 
  mutate(Year = as.integer(Year))


# Combine cdc and medicaid dfs by state and year
combined_df <- filtered_df %>% left_join(medicaid_df, by = c("State", "Year"))

# Load median income df 
df2 <- read.csv('C:/Users/19258/Downloads/state_median_income.csv')

# Delete second row 
df2 <- df2[-2,]

# Set the first row as the column names 
colnames(df2) <- as.character(unlist(df2[1,]))

# Remove the first row (which is now the header)
df2 <- df2[-1,]

#Select columns with data from 2008 through 2020 

filtered_df2 <- df2 %>% 
  select(State, '2008', '2009 (36)', '2010 (37)', '2011', '2012', '2013 (39)', '2014', '2015', '2016', '2017 (40)', '2018', '2019', '2020 (41)') %>%
  rename(
    '2009' = '2009 (36)', 
    '2010' = '2010 (37)', 
    '2013' = '2013 (39)', 
    '2017' = '2017 (40)', 
    '2020' = '2020 (41)') %>% 
  filter((State!='United States')&(State!='District of Columbia'))

# Noticed there are two Wisconsin median income rows per year so decided to keep 
# the slighly larger values 
filtered_df2 <- filtered_df2[-49,]

# Reshape filtered_df2 to long format and group by state 
median_df <- filtered_df2 %>% 
  pivot_longer(cols = starts_with('20'), names_to = "Year", values_to="Median_Income") %>% 
  mutate(Year = as.integer(Year))

#Convert the Median_Income data type and clean the values 
median_df$Median_Income <- gsub(",", "", median_df$Median_Income)
median_df$Median_Income <- as.numeric(median_df$Median_Income)

# Join median df with previously combined df (medicaid expense and cdc data) group by state and year 
final_df <- combined_df %>% left_join(median_df, by=c("State", "Year"))

# Check the na values per column
colSums(is.na(final_df))

# na values accounts for less than 10% for two columns(Medicaid exp and median income)
# Remove rows with na 
final_df <- final_df %>% drop_na()

# Make sure no na values in the df
colSums(is.na(final_df))


# Exploratory data analysis 

summary(Filter(is.numeric, final_df))

#Drop the rows that number diagnosed = 0 and confirm 
final_df <- final_df %>% 
  filter(Number_Diagnosed != 0)
nrow(final_df[final_df$Number_Diagnosed==0,])

# Average of number_diagnosed by race and gender

final_df %>% 
  group_by(Stratification1) %>% 
  summarise(average=mean(Number_Diagnosed, na.rm=TRUE)) %>%
  arrange(desc(average)) 

### Insights ###
#1. White, non-Hispanic individuals have the highest average diagnosis count (3779), 
#which may reflect both population size and potential disparities in diagnosis/reporting.
#2. Gender differences:
#Females (2513) have a higher average diagnosis count than males (2231).
#This might reflect higher screening rates, healthcare-seeking behavior, or disease prevalence among women.
#3. Racial disparities are evident:
#After White, non-Hispanic, the next highest racial groups are:
#Black, non-Hispanic (843)
#Hispanic (414)
#American Indian or Alaska Native (193)
#Asian or Pacific Islander (167)
#These lower averages may indicate healthcare access gaps, underdiagnosis, or smaller representation in the dataset.

# Mean and median for medicaid expense, median income and number of diagnosed for state
mean_median_state <- final_df %>% group_by(State) %>% summarise(across(c(Medicaid_Expenses, Median_Income, Number_Diagnosed), list(mean=mean, median=median)))
mean_median_state <- mean_median_state %>% mutate(ratio = Medicaid_Expenses_median/Median_Income_median)%>%
  arrange(desc(ratio))
mean_median_state

# medicaid expenses frequency distribution
ggplot(final_df, aes(x = Medicaid_Expenses)) +
  geom_histogram(binwidth = 10000, fill = "cadetblue", color = "black") +
  labs(title = "Histogram of Medicaid Expenses", x = "Medicaid Expenses (in millions)", y = "Frequency")

# median income frequency distribution
ggplot(final_df, aes(x = Median_Income)) +
  geom_histogram(binwidth = 1000, fill = "cadetblue3", color = "black") +
  labs(title = "Histogram of Median Income", x = "Median Income (in thousands)", y = "Frequency")

# filter to gender 
gender <- final_df %>% filter(StratificationCategory1 == "Gender")

#filter to race/ethnicity 
race <- final_df %>% filter(StratificationCategory1 == 'Race/Ethnicity')

# number diagnosed by gender
ggplot(gender, aes(x = Stratification1, y = Number_Diagnosed)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot by Gender", x = "Gender", y = "Number Diagnosed")

### Insights ###
#1. Right-skewed Distribution:
#Both gender groups (Female and Male) have a large number of extreme outliers at the top end.
#Most of the data points are clustered near 0, 
#meaning that in most counties (or units of measurement), the number diagnosed is relatively low.
#2. Similar Central Tendencies:
#The median line (inside the boxes) for both genders appears to be very close to the bottom, 
#suggesting similar central tendencies (e.g., median values) for diagnosis counts in both groups.
#3. Presence of High Outliers:
#The presence of many high outliers in both genders may indicate a small number of areas with very high diagnosis counts, 
#possibly corresponding to densely populated counties or high-incidence regions.

ggplot(gender, aes(x = Stratification1, y = log1p(Number_Diagnosed))) +
  geom_boxplot(fill = "orange") +
  labs(title = "Log-Transformed Boxplot by Gender", x = "Gender", y = "Log(Number Diagnosed + 1)")

### Updated Insights ###
#1. Comparable Distributions:
#Both Female and Male groups show very similar distributions after log transformation.
#The median values (central horizontal lines in the boxes) and interquartile ranges (IQRs) are nearly identical.
#2. Presence of Outliers:
#Even after log transformation, both genders still show outliers above the upper whisker, 
#indicating that a few areas still have relatively high diagnosis counts.
#However, the scale now reveals these outliers more proportionately.
#3. Skewness Reduced:
#The transformation reduces the right skew, 
#making it easier to compare gender groups without distortion from extreme values.
#4. No Substantial Gender Gap:
#There is no significant visual difference in the number diagnosed between males and females based on this visualization.

# number diagnosed by race/ethnicity
ggplot(race, aes(x = Stratification1, y = Number_Diagnosed)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot by Race/Ethnicity", x = "Race/Ethnicity", y = "Number Diagnosed")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Insights ###
#1. White, non-Hispanic stands out:
#This group has significantly higher counts and more extreme outliers compared to all other racial/ethnic groups.
#This may reflect population distribution (i.e., there are simply more White, non-Hispanic individuals in many regions) or other structural factors.
#2. Other groups have low medians:
#Most of the other race/ethnicity categories show low medians and IQRs, 
#with boxplots nearly flattened at the bottom of the y-axis, making them hard to interpret due to scale compression.
#3. Right-skewed distribution across all groups:
#Similar to the gender plot, the raw scale makes the plot less informative due to extreme values dominating the view.

ggplot(race, aes(x = Stratification1, y = log1p(Number_Diagnosed))) +
  geom_boxplot(fill = "orange") +
  labs(title = "Log-Transformed Boxplot by Race/Ethnicity", 
       x = "Race/Ethnicity", y = "Log(Number Diagnosed + 1)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Updated Insights ###
#1. More balanced visualization:
#Log transformation successfully reduces the skew and compresses extreme values, 
#allowing for a clearer comparison between racial/ethnic groups.
#2. White, non-Hispanic still has the highest values:
#This group retains higher median and upper whisker values, 
#indicating consistently higher diagnosis counts even after adjusting for skew.
#Suggests either larger population size or potentially higher prevalence/reporting rates in this group.
#3. Other groups show similar central tendencies:
#Groups like Asian or Pacific Islander, Hispanic, and Other, non-Hispanic display similar medians and interquartile ranges, mostly centered around log(4) to log(5)

# scatterplot showing number diagnosed against median income
ggplot(final_df, aes(x = Median_Income, y = Number_Diagnosed)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot for Number Diagnosed vs. Median Income", x = "Median Income (in thousands)", y = "Number Diagnosed")

### Insights ###
#1. No clear linear trend:
#The data appears widely scattered with no obvious upward or downward slope, 
#suggesting little to no linear correlation between median income and number diagnosed.
#2. High variability at mid-income levels:
#The most variability in diagnosis counts occurs in the $40,000-$70,000 range.
#This may reflect areas with larger populations-income alone doesn't explain the variance.
#3. Fewer high-count outliers at higher incomes:
#As median income increases (especially above ~$75,000), 
#we see fewer extreme diagnosis counts.
#This could hint at lower diagnosis rates or smaller populations in higher-income areas.

# scatterplot showing number diagnosed against medicaid expenses
ggplot(final_df, aes(x = Medicaid_Expenses, y = Number_Diagnosed)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot for Number Diagnosed vs. Medicaid Expenses", x = "Medicaid Expenses (in millions)", y = "Number Diagnosed")

#1. No strong linear relationship:
#Like the previous scatter plot, this one shows no clear linear trend. 
#The points are widely spread across the range of Medicaid expenses.
#2. High concentration at lower expenses:
#A dense cluster of data points lies below $50,000 million in Medicaid expenses. 
#Many of these still show relatively high diagnosis counts.
#This suggests that higher diagnosis numbers don't necessarily require the highest Medicaid expenditures, 
#possibly due to population size effects.
#3. Sparse data at high expense levels:
#As Medicaid expenses exceed $100,000 million, the number of points decreases and diagnosis counts tend to be lower, or highly variable. 
#These may correspond to states with large Medicaid budgets but low per-case reporting or diagnoses.

# correlation analysis
cor_matrix <- final_df %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

corrplot::corrplot(cor_matrix, method = "circle")

#Drop the column Disease, which contains only one value 
final_df <- final_df %>% select(-Disease)

# only factor categorical columns used in modeling
modified_df <- final_df %>%
  mutate(State=as.factor(State),
         Stratification1=as.factor(Stratification1))

# Split data into training and testing sets 
# training data uses data from 2008-2017 and 
# testing data uses data from 2018-2020

set.seed(123)
train_df <- modified_df %>% filter(Year >= 2008 & Year <= 2017)
test_df <- modified_df %>% filter(Year >= 2018 & Year <= 2020)

# Preprocessing 

# Check the null values in train and test sets 
colSums(is.na(train_df))
colSums(is.na(test_df))

# Model training
# linear regression
lm_model <- lm(Number_Diagnosed ~ State + Year + Stratification1 + Medicaid_Expenses + Median_Income, data = train_df)

# random forest regression
rf_model <- randomForest(Number_Diagnosed ~ State + Year + Stratification1 + Medicaid_Expenses + Median_Income, data = train_df, ntree = 100)

# gradient boosting regression
gbm_model <- gbm(Number_Diagnosed ~ State + Year + Stratification1 + Medicaid_Expenses + Median_Income, data = train_df, distribution = "gaussian", n.trees = 100, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5)

#Xgboost model using trees

X <- train_df %>%
  select(c(State,Year,Stratification1,Medicaid_Expenses,Median_Income,Number_Diagnosed))

X$State <- as.numeric(X$State)  # Convert State to numeric
X$Stratification1 <- as.numeric(X$Stratification1)

y <- X %>%
  select(c(Number_Diagnosed))

X <- X %>%
  select(-c(Number_Diagnosed))

# Convert data to DMatrix (optimized format for XGBoost)
dtrain <- xgb.DMatrix(data = as.matrix(X), label = as.matrix(y))

# Define model parameters
params <- list(
  objective = "reg:squarederror",  # Regression objective
  booster = "gbtree",  # Tree-based model
  eta = 0.1,  # Learning rate
  max_depth = 6,  # Depth of trees
  subsample = 0.8,  # Subsampling ratio
  colsample_bytree = 0.8  # Feature selection ratio
)

# Train XGBoost model
xgb_model <- xgboost(
  data = dtrain,
  params = params,
  nrounds = 100,  # Number of boosting rounds
  verbose = 0  # Suppress training logs
)

# predictions
test_df$lm_pred <- predict(lm_model, newdata = test_df)
test_df$rf_pred <- predict(rf_model, newdata = test_df)
test_df$gbm_pred <- predict(gbm_model, newdata = test_df, n.trees = 100)

# predictions for xgboost

X_test <- test_df %>%
  select(c(State,Year,Stratification1,Medicaid_Expenses,Median_Income))

X_test$State <- as.numeric(X_test$State)  # Convert State to numeric
X_test$Stratification1 <- as.numeric(X_test$Stratification1)

test_df$xgboost_pred <- predict(xgb_model, as.matrix(X_test), nrounds = 100)

### Evaluate models 
# calculate mae
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# calculate mse
mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# calculate rmse
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# calculate r-squared
r_squared <- function(actual, predicted) {
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  1 - (ss_residual / ss_total)
}

# metrics for linear regression model
lm_mae <- mae(test_df$Number_Diagnosed, test_df$lm_pred)
lm_mse <- mse(test_df$Number_Diagnosed, test_df$lm_pred)
lm_rmse <- rmse(test_df$Number_Diagnosed, test_df$lm_pred)
lm_r2 <- r_squared(test_df$Number_Diagnosed, test_df$lm_pred)

# metrics for random forest model
rf_mae <- mae(test_df$Number_Diagnosed, test_df$rf_pred)
rf_mse <- mse(test_df$Number_Diagnosed, test_df$rf_pred)
rf_rmse <- rmse(test_df$Number_Diagnosed, test_df$rf_pred)
rf_r2 <- r_squared(test_df$Number_Diagnosed, test_df$rf_pred)

# metric for gradient boosting
gbm_mae <- mae(test_df$Number_Diagnosed, test_df$gbm_pred)
gbm_mse <- mse(test_df$Number_Diagnosed, test_df$gbm_pred)
gbm_rmse <- rmse(test_df$Number_Diagnosed, test_df$gbm_pred)
gbm_r2 <- r_squared(test_df$Number_Diagnosed, test_df$gbm_pred)


# metric for gradient boosting
xgboost_mae <- mae(test_df$Number_Diagnosed, test_df$xgboost_pred)
xgboost_mse <- mse(test_df$Number_Diagnosed, test_df$xgboost_pred)
xgboost_rmse <- rmse(test_df$Number_Diagnosed, test_df$xgboost_pred)
xgboost_r2 <- r_squared(test_df$Number_Diagnosed, test_df$xgboost_pred)


# results
cat("Linear Regression Metrics:")
cat("MAE:", lm_mae) # 
cat("MSE:", lm_mse) # 
cat("RMSE:", lm_rmse) # 
cat("R-squared:", lm_r2) # 

cat("Random Forest Metrics:")
cat("MAE:", rf_mae) # 
cat("MSE:", rf_mse) # 
cat("RMSE:", rf_rmse) # 
cat("R-squared:", rf_r2) # 

cat("Gradient Boosting Metrics:")
cat("MAE:", gbm_mae) # 
cat("MSE:", gbm_mse) # 
cat("RMSE:", gbm_rmse) # 
cat("R-squared:", gbm_r2) # 


cat("XGBoost Metrics:")
cat("MAE:", xgboost_mae) # 
cat("MSE:", xgboost_mse) # 
cat("RMSE:", xgboost_rmse) # 
cat("R-squared:", xgboost_r2) # 

### Insights ###
#1. All models perform similarly, with small differences in error metrics and R� values.
#2. Gradient Boosting achieved the lowest MAE (3049.17), 
#suggesting it predicts closer to the actual values on average than the others.
#3. Random Forest has the lowest RMSE (10520.94) and the highest R� (0.0412), 
#indicating it's slightly better at capturing variance in the data.
#4. Linear Regression performed the worst overall:
#It has the highest MAE and RMSE.
#The lowest R� (0.0254), indicating it explains only ~2.5% of the variance - meaning the relationship between predictors and the target is likely nonlinear or involves complex interactions.
#5. All R� values are low (< 0.05):
#This suggests that your features - even with ensemble models - are not strong predictors of Number_Diagnosed.
#There may be missing explanatory variables (e.g., population size, disease prevalence, healthcare access).

#Forecasting
# Step 1: Create list of years, states, and stratification values
future_years <- 2021:2050
states <- unique(train_df$State)
strat_vals <- unique(train_df$Stratification1)

# Step 2: Fit Random Forest models for Medicaid_Expenses and Median_Income by state
train_df <- train_df %>%
  mutate(Year = as.numeric(Year))

# Fit models for each state
medicaid_models <- train_df %>%
  group_by(State, Stratification1) %>%
  summarise(model = list(randomForest(Medicaid_Expenses ~ Year, data = cur_data_all())))

income_models <- train_df %>%
  group_by(State, Stratification1) %>%
  summarise(model = list(randomForest(Median_Income ~ Year, data = cur_data_all())))

# Step 3: Create future data combinations (State x Year x Stratification)

future_data <- expand.grid(State=states, Year = future_years, Stratification1 = strat_vals, stringsAsFactors = FALSE)

# Step 4: Add Predicted Medicaid expense and median income 
# Define helper functions 
predict_by_state <- function(state_name, strat_value, year_value, model_list){
  model_row <- model_list %>% filter(State == state_name, Stratification1 == strat_value)
  if (nrow(model_row)==0) return(NA)
  predict(model_row$model[[1]], newdata = data.frame(Year=year_value))
}

# Apply predictions row by row 
future_data <- future_data %>% 
  rowwise() %>% 
  mutate(
    Medicaid_Expenses = predict_by_state(State, Stratification1, Year, medicaid_models), 
    Median_Income = predict_by_state(State, Stratification1, Year, income_models)) %>% 
  ungroup() 

# Make predictions with random forest models
future_data$Predicted_Number_Diagnosed <- predict(rf_model, future_data)

# Export to Excel
# -------------------------------
write.xlsx(
  future_data,
  file = "pulmonary_predictions_dual_output_2021_2050.xlsx",
  sheetName = "Predictions",
  rowNames = FALSE)