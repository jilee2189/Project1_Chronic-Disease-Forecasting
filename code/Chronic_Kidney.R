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
df <- read.csv("C:/Users/19258/Downloads/Chronic Kidney Disease_data.csv")

# filter data by 'stratificationcategory1 = gender' and 'stratificationcategory1 = race/ethnicity', 
# did not include 'stratificationcategory1 = overall' since this would double count the number of deaths from ckd and not a specific enough predictor
# filtering by gender and race/ethnicity sets stratification1 to male, female, hispanic, asian, etc.
# by using both gender and race/ethnicity, may be double counting number of deaths as well (something to keep in mind)
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

#1. White, non-Hispanic individuals have the highest average number diagnosed - significantly higher than any other group.
#This may reflect:
#-Higher population size
#-Greater access to diagnostic services
#-Potential disparities in disease burden or healthcare usage
#2. Gender gap is modest:
#-Males: 992
#-Females: 978
#-The average number diagnosed is slightly higher for males, but the difference is small.
#3. Black, Hispanic, and Asian populations show lower average diagnosis numbers
#This could suggest:
#-Underdiagnosis
#-Healthcare access barriers
#-Demographic differences in disease prevalence

# Mean and median for medicaid expense, median income and number of diagnosed for state
mean_median_state <- final_df %>% group_by(State) %>% summarise(across(c(Medicaid_Expenses, Median_Income, Number_Diagnosed), list(mean=mean, median=median)))
mean_median_state <- mean_median_state %>% mutate(ratio = Medicaid_Expenses_median/Median_Income_median)%>%
  arrange(desc(ratio))
mean_median_state

### Insights ###
#1. California and New York stand out dramatically with ratios above 1, 
#meaning median Medicaid expenses exceed median income - 
#suggesting either a concentration of Medicaid-supported populations or 
#large-scale healthcare infrastructure spending.
#2. Texas and Florida also rank highly despite lower per capita income, 
#which may reflect demographic or policy-related factors.
#3. High-spending states often have large populations (e.g., California, New York).
#4. States with strong income levels (e.g., Massachusetts, New Jersey) tend to have lower ratios, 
#suggesting they spend less Medicaid per dollar of resident income.

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

#1. Most values are very low:
#The bulk of the data is concentrated near zero for both genders.
#This is evident because the box (interquartile range) is compressed at the bottom, 
#indicating that most counties/zip codes/areas have relatively low diagnosis counts.
#2. Presence of extreme outliers:
#Both Female and Male groups have many extreme outliers - points far above the box and whiskers.
#These represent areas with unusually high numbers of diagnoses, 
#possibly densely populated or high-incidence regions.
#3. No major difference between genders:
#The shape, median, and spread of the boxplots for Female and Male appear very similar.
#This suggests there may be no strong gender-related disparity 
#in the number of diagnoses based on this boxplot alone.

# number diagnosed by race/ethnicity
ggplot(race, aes(x = Stratification1, y = Number_Diagnosed)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot by Race/Ethnicity", x = "Race/Ethnicity", y = "Number Diagnosed")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Insights ###

#1. Most groups have low diagnosis counts overall:
#Like the gender plot, the majority of values across all race/ethnicity groups are concentrated near zero.
#This is indicated by the compressed box and whiskers at the bottom of each category.
#2. White, non-Hispanic group shows the highest spread and outliers:
#This group has the widest range of diagnosis counts and the most extreme outliers (up to 400,000+), suggesting:
#Larger population representation
#Possibly more regions or counties with high case counts
#Or greater variation within the group
#3. Hispanic and Black, non-Hispanic groups also show relatively high diagnosis counts:
#These two groups have longer whiskers and more upper outliers compared to others, 
#though less than the White, non-Hispanic group.
#This could reflect higher burden or concentration in certain areas.
#4. Other racial/ethnic groups show more tightly clustered counts:
#Groups like Asian or Pacific Islander, American Indian or Alaska Native, Multiracial non-Hispanic, and Other non-Hispanic have relatively lower and tighter diagnosis counts.
#This might reflect smaller populations, underreporting, or lower incidence.

# scatterplot showing number diagnosed against median income
ggplot(final_df, aes(x = Median_Income, y = Number_Diagnosed)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot for Number Diagnosed vs. Median Income", x = "Median Income (in thousands)", y = "Number Diagnosed")

#1. Negative correlation trend:
#There is a visible downward trend: as median income increases, 
#the number of diagnosed cases tends to decrease.
#This suggests that lower-income areas may be experiencing higher diagnosis counts.
#2. High variability at low-income levels:
#At income levels around $30,000-$60,000, there is wide spread in the number diagnosed (ranging from near 0 to over 300,000).
#This indicates that while low-income areas tend to have high counts, 
#some still have low counts - suggesting other influencing factors.
#3. Upper-income areas show fewer diagnoses:
#For median incomes above $80,000, diagnosis counts are consistently low, with fewer and smaller outliers.
#This supports the idea that wealthier communities may have fewer diagnoses, 
#possibly due to better access to care, preventive measures, or lower disease prevalence.

# scatterplot showing number diagnosed against medicaid expenses
ggplot(final_df, aes(x = Medicaid_Expenses, y = Number_Diagnosed)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot for Number Diagnosed vs. Medicaid Expenses", x = "Medicaid Expenses (in millions)", y = "Number Diagnosed")

#1. Curved cluster of points on the left (0-50,000):
#Most data points lie in the lower Medicaid expenses range (up to 50,000), and within this range, the number diagnosed increases with spending.
#This suggests a positive relationship between Medicaid spending and diagnosis count in lower-spending areas, possibly because:
#Areas with more diagnosed cases receive more funding, or
#Spending helps uncover and treat more diagnoses.
#2. Flat cluster at high Medicaid spending levels:
#Beyond $75,000+ in expenses, the number diagnosed becomes flat or scattered and lower overall.
#Some high-spending areas show surprisingly low diagnosis counts, which could indicate:
#3. Non-linear relationship:
#The plot does not show a consistent linear pattern across the full range.
#There's a rising trend early on, 
#but it plateaus or declines in higher ranges - 
#suggesting diminishing returns or varying context for high Medicaid expenses.

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
cat("MAE:", lm_mae) # 1515.079
cat("MSE:", lm_mse) # 51536666
cat("RMSE:", lm_rmse) # 7178.904
cat("R-squared:", lm_r2) # -0.002486961

cat("Random Forest Metrics:")
cat("MAE:", rf_mae) # 1215.113
cat("MSE:", rf_mse) # 50796368
cat("RMSE:", rf_rmse) # 7127.157
cat("R-squared:", rf_r2) # 0.01191326

cat("Gradient Boosting Metrics:")
cat("MAE:", gbm_mae) # 1249.807
cat("MSE:", gbm_mse) # 51088406
cat("RMSE:", gbm_rmse) # 7147.615
cat("R-squared:", gbm_r2) # 0.006232553


cat("XGBoost Metrics:")
cat("MAE:", xgboost_mae) # 1325.899
cat("MSE:", xgboost_mse) # 51096569
cat("RMSE:", xgboost_rmse) # 7148.186
cat("R-squared:", xgboost_r2) # 0.00607377

### Insights ###

#Random Forest performs best:
#1. Lowest MAE (most accurate on average).
#Highest R² (although still very low at ~1%).
#2. All models struggle overall:
#Very low R² values (close to 0) suggest that the features used (State, Year, Stratification1, Medicaid_Expenses, Median_Income) do not capture enough information to predict Number_Diagnosed effectively.

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
  file = "kidney_predictions_dual_output_2021_2050.xlsx",
  sheetName = "Predictions",
  rowNames = FALSE)