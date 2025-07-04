#load libraries
install.packages("zoo")
install.packages("lifecycle", type = "binary")
install.packages("dplyr")
install.packages("rlang", type = "binary")
install.packages("ggplot2", dependencies = TRUE, type = "binary")
install.packages("xgboost")
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
library(lubridate)
library(broom)
library(ggplot2)
library(forecast)
library(DataExplorer)
library(data.table)
library(scales)
library(caret)
library(randomForest)
library(gbm)
library(glmnet)
library(xgboost)
library(forecast)



df <- read.csv("C:/Users/19258/Downloads/diabetes_data.csv")

# Load CDC chronic diseases dataset into df 
# Filter data to show only rows 
# where topic = diabetes, stratification = 'gender' or 'race/ethnicity'

filtered_df <- df %>% 
  select(c('YearStart', 'LocationDesc','Topic', 'DataValue',"StratificationCategory1","Stratification1")) %>%
  filter(!(StratificationCategory1=='Overall')) %>% 
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

filtered_df %>% distinct(Year)

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

# Average of number_diagnosed by race and gender

final_df %>% 
  group_by(Stratification1) %>% 
  summarise(average=mean(Number_Diagnosed, na.rm=TRUE)) %>%
  arrange(desc(average)) 

### Insights ###
#1.White, non-Hispanic individuals have the highest average number diagnosed (1181), 
#significantly higher than all other groups.
#2. Gender comparison:
#Males (992) and females (978) have relatively similar averages, 
#suggesting no major gender disparity in diagnosis counts overall.
#3. Racial and ethnic disparities:
#Black, non-Hispanic (611) and Hispanic (476) groups show lower average diagnoses than White, non-Hispanic.
#4. Asian or Pacific Islander (345) and American Indian or Alaska Native (274) are diagnosed even less on average.

# Mean and median for medicaid expense, median income and number of diagnosed for state
mean_median_state <- final_df %>% group_by(State) %>% summarise(across(c(Medicaid_Expenses, Median_Income, Number_Diagnosed), list(mean=mean, median=median)))
mean_median_state <- mean_median_state %>% mutate(ratio = Medicaid_Expenses_median/Median_Income_median)%>%
  arrange(desc(ratio))

###Insights###
#1. Top States by Medicaid Burden (High ratio)
#These states have Medicaid median expenses that are a large proportion of median income:
#State	Ratio	Interpretation
#- California	2.28	Median Medicaid expenses exceed median income, 
#suggesting high healthcare burden and/or high public spending.
#- New York	1.93	Similar pattern-extremely high Medicaid expenses relative to income.
#- Texas	1.15	Though lower than CA/NY, still over 100% of income.
#- Florida	0.75	Expenses remain high relative to income.
#Interpretation: These states may:
#Serve large, vulnerable populations.
#Have expansive Medicaid coverage.
#Face higher healthcare costs or cost-of-living.

#2. States with High Number of Diagnosed Cases (but Low ratio)
#Some states show low Medicaid burden but high diagnosis counts:
#State	Diagnosed (Mean)	Ratio	Note
#Florida	4,893.87	0.75	High diagnosis, but moderate Medicaid burden.
#Michigan	2,626.01	0.50	Above-average diagnoses, relatively efficient spending.
#North Carolina	2,025.56	0.46	Similar pattern to Michigan.
#Interpretation: Possibly better cost containment, lower per-patient spending, or underfunding concerns.

#3. States with Low Medicaid Burden and Low Diagnoses
#Likely healthier or underfunded systems:
#State	Ratio	Diagnosed (Mean)
#Wyoming	0.019	65.94
#South Dakota	0.028	294.53
#North Dakota	0.038	76.01
#Interpretation: Could indicate:
#Small populations
#Lower disease prevalence
#Potential underreporting or lack of access

# medicaid expenses frequency distribution
ggplot(final_df, aes(x = Medicaid_Expenses)) +
  geom_histogram(binwidth = 10000, fill = "cadetblue", color = "black") +
  labs(title = "Histogram of Medicaid Expenses", x = "Medicaid Expenses (in millions)", y = "Frequency")

###Insights###
#1. Right-Skewed Distribution (Positive Skew)
#Most Medicaid expenses are concentrated at the lower end, particularly below $50,000 million.
#There are fewer but significant outliers with very high expenses (up to ~170,000 million), causing a long tail on the right.
#2. Majority of Observations are Low to Moderate
#A large portion of the data falls between 0 and 30,000 million, 
#suggesting that many regions (or time periods) have relatively modest Medicaid spending.
#3. Potential Outliers / High-Cost Areas
#The sparsely populated bars beyond 100,000 million likely represent states or timeframes with unusually high costs, possibly due to:
#Larger populations
#Expanded Medicaid programs
#High-cost procedures or demographics


# median income frequency distribution
ggplot(final_df, aes(x = Median_Income)) +
  geom_histogram(binwidth = 1000, fill = "cadetblue3", color = "black") +
  labs(title = "Histogram of Median Income", x = "Median Income (in thousands)", y = "Frequency")

#1. Approximately Normal Distribution with Slight Right Skew
#The distribution appears roughly bell-shaped, 
#peaking between $50,000 and $60,000.
#There's a slight skew to the right, indicating some regions have notably higher median incomes (above $80,000), but they are less common.
#2. Most Common Range
#The majority of observations fall between $45,000 and $70,000, 
#indicating that most regions have moderate median incomes.
#3. Fewer Regions with Very Low or Very High Incomes
#Few regions fall below $40,000 or above $85,000, showing that extremely low or high-income areas are rare.

# filter to gender 
gender <- final_df %>% filter(StratificationCategory1 == "Gender")

#filter to race/ethnicity 
race <- final_df %>% filter(StratificationCategory1 == 'Race/Ethnicity')

# number diagnosed by gender
ggplot(gender, aes(x = Stratification1, y = Number_Diagnosed)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot by Gender", x = "Gender", y = "Number Diagnosed")

###Insights###
#1. Distributions Are Similar Across Genders
#The overall shape and spread of the number diagnosed appear very similar for both females and males.
#This suggests that the distribution of diagnoses is not heavily gender-skewed.

# number diagnosed by race/ethnicity
ggplot(race, aes(x = Stratification1, y = Number_Diagnosed)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot by Race/Ethnicity", x = "Race/Ethnicity", y = "Number Diagnosed")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Insights###
#1. White, non-Hispanic group dominates in volume
#This group has the highest number of diagnoses overall.
#The distribution shows a wide range, including many high-value outliers, 
#some approaching or exceeding 400,000.
#2. High variability in larger populations
#Black, non-Hispanic and Hispanic groups also show a substantial range in diagnosed numbers, 
#with numerous outliers indicating large counts in some instances.
#Hispanic group has some extreme outliers (e.g., near or above 200,000), 
#suggesting high numbers in certain areas or categories.
#3. Smaller groups have lower diagnosed counts
#American Indian or Alaska Native, Asian or Pacific Islander, Multiracial non-Hispanic, and Other non-Hispanic groups show lower overall counts with fewer extreme values.

# scatterplot showing number diagnosed against median income
ggplot(final_df, aes(x = Median_Income, y = Number_Diagnosed)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot for Number Diagnosed vs. Median Income", x = "Median Income (in thousands)", y = "Number Diagnosed")

###Insights###

#1. Inverse Relationship Observed
#There appears to be a negative correlation between Median Income and Number Diagnosed.
#Higher diagnosis counts are more common in areas with lower median incomes (under $60,000).
#As income increases, the number of diagnosed cases tends to decrease, though not in a perfectly linear fashion.
#2. High Concentration of Diagnoses in Lower-Income Areas
#The densest cluster of red dots (points) lies between $35,000 and $60,000 in median income and up to 100,000 diagnoses.
#Several extreme outliers (200,000-400,000 diagnoses) also occur within this low-to-middle income range.
#3. Fewer Diagnosed Cases in High-Income Areas
#For median incomes above $80,000, there are significantly fewer cases, and diagnosis counts are mostly below 50,000.

# scatterplot showing number diagnosed against medicaid expenses
ggplot(final_df, aes(x = Medicaid_Expenses, y = Number_Diagnosed)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot for Number Diagnosed vs. Medicaid Expenses", x = "Medicaid Expenses (in millions)", y = "Number Diagnosed")

###Insights###
#1. Nonlinear Relationship with a Clustered Pattern
#There's a dense cluster of points with low Medicaid expenses (< $50,000 million) and 
#high diagnosis counts, especially between $0 and $40,000 million.
#Beyond that, the relationship becomes much more scattered and unclear, 
#suggesting other factors may influence diagnosis counts at higher expense levels.
#2. Diminishing Returns Beyond a Certain Expense Level
#For Medicaid expenses above ~$50,000 million, the number of diagnosed cases tends to drop sharply or become inconsistent.
#This may indicate that beyond a certain level of Medicaid spending, 
#other factors-like efficiency of healthcare delivery or population size-become more important than raw expense.
#3. High Diagnoses at Moderate Spending
#Most of the very high diagnosis counts (200,000-400,000) occur within low-to-moderate Medicaid expense ranges (e.g., $10,000 to $50,000 million).

# correlation analysis
cor_matrix <- final_df %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

corrplot::corrplot(cor_matrix, method = "circle")

###INSIGHTS###

#The strongest correlation is between Medicaid_Expenses and Year - Medicaid spending is growing.
#The number of diagnosed cases does not show strong linear relationships 
#with either Medicaid_Expenses or Median_Income.
#Income levels and diagnoses appear statistically independent, at least linearly.

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
test_df$lm_pred <- predict(lm_model, test_df)
test_df$rf_pred <- predict(rf_model, test_df)
test_df$gbm_pred <- predict(gbm_model, test_df, n.trees = 100)

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
cat("MAE:", lm_mae) # 1515.079
cat("MSE:", lm_mse) # 51536666
cat("RMSE:", lm_rmse) # 7178.904
cat("R-squared:", lm_r2) # -0.002486961

cat("Random Forest Metrics:")
cat("MAE:", rf_mae) # 1245.151
cat("MSE:", rf_mse) # 50878515
cat("RMSE:", rf_rmse) # 7132.918
cat("R-squared:", rf_r2) # 0.01031535

cat("Gradient Boosting Metrics:")
cat("MAE:", gbm_mae) # 1249.424
cat("MSE:", gbm_mse) # 51088011
cat("RMSE:", gbm_rmse) # 7147.588
cat("R-squared:", gbm_r2) # 0.006240241



cat("XGBoost Metrics:")
cat("MAE:", xgboost_mae) # 1311.481
cat("MSE:", xgboost_mse) # 51034484
cat("RMSE:", xgboost_rmse) # 7143.842
cat("R-squared:", xgboost_r2) # 0.007281439


#Interpretation
#1.Linear regression performs the worst, even worse than just guessing the mean (R² < 0).
#2.Random Forest performs the best overall:
#Lowest MAE (best short-term error minimization).
#Lowest RMSE (best long-term error minimization).
#Highest R² (explains ~1% of variance - small but better than others).
#3.Gradient Boosting and XGBoost are close but slightly underperform compared to Random Forest in this case.

#Takeaways:
#1.You correctly compared different regression models using consistent metrics.
#2.However, all models perform poorly in terms of R². An R² near zero suggests:
#The features don't explain much of the variation in Number_Diagnosed.
#Important variables may be missing or not captured well.
#The data may have high noise or low signal.

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
  file = "diabetes_predictions_dual_output_2021_2030.xlsx",
  sheetName = "Predictions",
  rowNames = FALSE)