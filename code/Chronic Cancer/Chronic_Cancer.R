
install.packages("cli")
install.packages("recipes")
install.packages("lifecycle", dependencies = TRUE)# Load libraries
install.packages("corrplot")
library(corrplot)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(gbm)
library(openxlsx)
library(purrr)

# Load CDC chronic diseases dataset into df 
# Filter data to show only rows where DataValueType ='Number', topic = cancer, stratification = 'gender' or 'race/ethnicity'
df <- read.csv("C:/Users/19258/Downloads/cancer_data.csv")
filtered_df <- df %>%
  filter(Topic == 'Cancer') %>%
  filter(StratificationCategory1 == "Gender" | StratificationCategory1 == "Race/Ethnicity") %>%
  select(YearStart, LocationDesc, Topic, Question, DataValue, StratificationCategory1, Stratification1) %>%
  rename(
    Year = YearStart,
    State = LocationDesc,
    Disease = Topic,
    Type = Question,
    Number_Diagnosed = DataValue
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year, State) %>%
  mutate(Number_Diagnosed = as.numeric(Number_Diagnosed))

nrow(filtered_df)


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

head(filtered_df1,5)

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

# STOP HERE 

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
  pivot_longer(cols = starts_with('20'), names_to = "Year", values_to="Median Income") %>% 
  mutate(Year = as.integer(Year))


# Join median df with previously combined df (medicaid expense and cdc data) group by state and year 
final_df <- combined_df %>% left_join(median_df, by=c("State", "Year"))

# Check the na values per column
colSums(is.na(final_df))

# na values accounts for less than 5% for two columns(Medicaid exp and median income)
# Remove rows with na 
final_df <- final_df %>% drop_na()

# Make sure no na values in the df
colSums(is.na(final_df))

# Exploratory data analysis 

summary(Filter(is.numeric, final_df))

# Average of number_diagnosed by cancer type 

final_df %>% 
  group_by(Type) %>% 
  summarise(average=mean(Number_Diagnosed, na.rm=TRUE)) %>%
  arrange(desc(average))

### Insights ### 
# Among specific types, the top three are:
#1. Breast cancer (~613 cases)
#2. Prostate cancer (~530)
#3. Lung and bronchus cancer (~475)

# Top specific cancers by mortality:
# 1. Lung and bronchus (~353 deaths/year)
# 2. Female breast (~138)
# 3. Colorectal cancer (~132)
# 4. Prostate (~109)

#Lung cancer shows a high death burden relative to its incidence, 
#which may point to late diagnoses or limited treatment effectiveness.

# Average of number_diagnosed by race and gender 

final_df %>% 
  group_by(Stratification1) %>%
  summarise(average= mean(Number_Diagnosed, na.rm=TRUE)) %>% 
  arrange(desc(average))

### Insights ### 
#1. Cancer Burden is Highest Among White, Non-Hispanic Individuals
#With an average of ~1,058 cases, this group has the highest recorded average. This may reflect:
  
#-Population size (they could be the largest group in the dataset),
#-Higher rates of diagnosis due to better access to healthcare or screening,
#-Actual differences in cancer incidence.

#Suggests the importance of continued screening and prevention efforts in this population.

#2. Gender Differences in Cancer Incidence
#Males have a higher average (~1,009) than females (~834).
#This aligns with known trends where certain male-specific cancers (like prostate) contribute to higher incidence.
#It may also suggest men are more likely to develop or be diagnosed with cancers.
#Can inform gender-specific awareness campaigns and prevention strategies.

#3. Disparities Among Racial/Ethnic Minorities
#Black, non-Hispanic: 221
#Hispanic: 158
#Asian or Pacific Islander: ~81
#American Indian or Alaska Native: ~33
#These groups show lower average diagnosis counts, but that doesn’t necessarily mean lower cancer risk — it could reflect:
#Underdiagnosis or limited access to healthcare,
#Barriers to screening, language, or cultural mistrust,
#Smaller population sizes in the dataset.

# Clean median_income and convert to numeric 
final_df <- final_df %>% mutate(Median_Income=as.numeric(gsub(",","",`Median Income`)))
final_df <- final_df %>%
  select(-`Median Income`)

# Mean and median for medicaid expense, median income and number of diagnosed for state
mean_median_state <- final_df %>% group_by(State) %>% summarise(across(c(Medicaid_Expenses, Median_Income, Number_Diagnosed), list(mean=mean, median=median)))
mean_median_state <- mean_median_state %>% mutate(ratio = Medicaid_Expenses_median/Median_Income_median)

### Insights ### 

#Medicaid Burden vs. Wealth
#Insight: States like New York (1.96) and California (1.78) have Medicaid expenses that exceed median incomes. 
#This suggests either:
#A large Medicaid population with high needs, or
#High per-person Medicaid spending.
#Contrast: States like Wyoming (0.02) and North Dakota (0.03) spend very little on Medicaid relative to income—this could indicate low program reach, lower health needs, or policy differences.

# medicaid expenses frequency distribution
ggplot(final_df, aes(x = Medicaid_Expenses)) +
  geom_histogram(binwidth = 10000, fill = "cadetblue", color = "black") +
  labs(title = "Histogram of Medicaid Expenses", x = "Medicaid Expenses (in millions)", y = "Frequency")

### Insights ###

#1. Right-skewed distribution 
#This means:
#Most Medicaid expenses are relatively low.
#A small number of observations have extremely high expenses, 
#creating a long tail.
#2. Majority of States/Populations Fall Below ~$25,000M
#The mode (most frequent range) of Medicaid expenses is likely in the $0–$25,000 million range.
#Suggests that many states or regions spend moderately, 
#while only a few states have very high Medicaid costs.

# median income frequency distribution
ggplot(final_df, aes(x = Median_Income)) +
  geom_histogram(binwidth = 1000, fill = "cadetblue3", color = "black") +
  labs(title = "Histogram of Median Income", x = "Median Income (in thousands)", y = "Frequency")

### Insights ###

#1. Skewness of Distribution:
#The histogram is right-skewed (positively skewed), 
#meaning most of the data is concentrated on the lower income range (e.g., $40k–$60k), with a long tail toward the higher end.
#This suggests fewer high-income observations, and many moderate- or low-income ones.
#2. Mode and Central Tendency:
#The peak (mode) appears to be around $45,000 to $50,000. 
#This is where the most frequent income values lie.

# Plot distribution by cancer type
library(ggplot2)
library(dplyr)
library(stringr)

# Wrap labels to avoid overlap
final_df$Type <- str_wrap(final_df$Type, width = 30)

# Calculate top 10 cancer types by median number diagnosed
top_types <- final_df %>%
  group_by(Type) %>%
  summarise(median_cases = median(Number_Diagnosed, na.rm = TRUE)) %>%
  arrange(desc(median_cases)) %>%
  slice_head(n = 10) %>%
  pull(Type)

# Filter original dataset
filtered_df <- final_df %>% filter(Type %in% top_types)

# Reorder for visual clarity
filtered_df$Type <- reorder(filtered_df$Type, filtered_df$Number_Diagnosed, FUN = median)

# Plot
ggplot(filtered_df, aes(x = Type, y = Number_Diagnosed)) +
  geom_boxplot(fill = "lightcoral") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Distribution of Diagnosed Cases (Top 10 Cancer Types)",
    x = "Cancer Type",
    y = "Number Diagnosed"
  )

# filter to gender 
gender <- final_df %>% filter(StratificationCategory1 == "Gender")

#filter to race/ethnicity 
race <- final_df %>% filter(StratificationCategory1 == 'Race/Ethnicity')



# Get top 10 cancer types by total diagnosed cases
top_types <- gender %>%
  group_by(Type) %>%
  summarise(Total = sum(Number_Diagnosed, na.rm = TRUE)) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10) %>%
  pull(Type)

# Step 2: Filter the main dataset
filtered_gender <- gender %>%
  filter(Type %in% top_types)

# Step 3: Optional: wrap long labels for readability
filtered_gender$Type <- str_wrap(filtered_gender$Type, width = 25)

# Step 4: Create the boxplot
ggplot(filtered_gender, aes(x = Stratification1, y = Number_Diagnosed)) + 
  geom_boxplot(fill = 'orange', outlier.size = 0.5) + 
  facet_wrap(~Type, scales = 'free_y') + 
  labs(
    title = 'Number Diagnosed by Gender (Top 10 Cancer Types)',
    x = 'Gender',
    y = 'Number Diagnosed'
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# number diagnosed by race/ethnicity

# Step 1: Get top 10 cancer types by number diagnosed
top_types <- race %>%
  group_by(Type) %>%
  summarise(Total = sum(Number_Diagnosed, na.rm = TRUE)) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10) %>%
  pull(Type)

# Step 2: Filter and wrap long labels
filtered_race <- race %>%
  filter(Type %in% top_types) %>%
  mutate(Type = str_wrap(Type, width = 25))

# Step 3: Create clearer boxplot
ggplot(filtered_race, aes(x = Stratification1, y = Number_Diagnosed, fill = Stratification1)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~Type, scales = "free_y", ncol = 2) +
  labs(
    title = "Number Diagnosed by Race/Ethnicity (Top 10 Cancer Types)",
    x = "Race/Ethnicity",
    y = "Number Diagnosed"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

### Insights ### 

#1. Non-Hispanic White and Non-Hispanic Black populations generally show higher median counts, 
#suggesting higher reported diagnoses.
#2. Lung and Bronchus Cancer (Incidence & Mortality):
#Mortality and incidence are both notably high for Non-Hispanic Black and White groups.
#3. Female Breast Cancer (Incidence & Mortality):
#High incidence among Non-Hispanic White and Black women.
#However, mortality appears disproportionally higher for Non-Hispanic Black women, 
#suggesting disparities in treatment access or diagnosis timing.
#4. Prostate Cancer (Incidence):
#Extremely high median diagnoses for Non-Hispanic Black males compared to other races.

# correlation analysis
install.packages("corrplot")
library(corrplot)

cor_matrix <- final_df %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

corrplot::corrplot(cor_matrix, method = "circle")

### Insights ### 

#Strong Positive Correlations:
  
#1. Number_Diagnosed vs. Medicaid_Expenses:
#strong positive correlation (dark blue, close to 1) indicates that 
#as the number of diagnosed cases increases, Medicaid expenses also rise.
#Suggests that disease burden is a key driver of healthcare costs.

#2. Medicaid_Expenses vs. Year:
#moderate to strong positive correlation suggests 
#Medicaid expenses are increasing over time, 
#possibly due to rising treatment costs or broader coverage.

#3. Median_Income vs. Year:
# positive correlation indicates median income has increased over the years (likely due to inflation and economic growth).

#Weak/Negligible Correlations:
  
#1. Number_Diagnosed vs. Median_Income:
#most no correlation — indicates that 
#income levels are not strongly related to the number of diagnosed cancer cases in the data.

#2. Year vs. Number_Diagnosed:
#very weak — implies the number of diagnoses may not be significantly increasing or decreasing over time (could be stable or vary by type).

#Drop the column Disease, which contains only one value 
final_df <- final_df %>% select(-Disease)

# only factor categorical columns used in modeling
modified_df <- final_df %>%
  mutate(across(c(State, StratificationCategory1, Stratification1, Type), as.factor))

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

train_df <- train_df %>%
  mutate(
    State = as.factor(State),
    StratificationCategory1 = as.factor(StratificationCategory1),
    Stratification1 = as.factor(Stratification1),
    Type = as.factor(Type),
    Medicaid_Expenses = as.numeric(Medicaid_Expenses),
    Median_Income = as.numeric(Median_Income)
  )

test_df <- test_df %>%
  mutate(
    State = as.factor(State),
    StratificationCategory1 = as.factor(StratificationCategory1),
    Stratification1 = as.factor(Stratification1),
    Type = as.factor(Type),
    Medicaid_Expenses = as.numeric(Medicaid_Expenses),
    Median_Income = as.numeric(Median_Income)
  )

# Model training
# REGRESSION: Predict Number_Diagnosed 
# linear regression 

lm_model <- lm(Number_Diagnosed ~ ., data =train_df)
summary(lm_model)

# random forest 
library(randomForest)
rf_model <- randomForest(
  Number_Diagnosed ~ State + Stratification1 + Medicaid_Expenses + Median_Income,         # Target ~ Predictors
  data = train_df,              # Your training data
  ntree = 15,                  # Number of trees
  importance = TRUE,            # To get variable importance
)



# Neural network 
library(nnet)
nnet_model <- nnet(Number_Diagnosed ~ State + Stratification1 + Medicaid_Expenses + Median_Income, data = train_df, size = 5, linout = TRUE, trace = FALSE, maxit = 100)


print(nnet_model)
summary(nnet_model)
# CLASSIFICATION: Predict Type 

unique(train_df$Type)

rf_class_model <- randomForest(Type ~ State + Stratification1 + Medicaid_Expenses + Median_Income, data = train_df, ntree=15)

#Check if there are missing values in test set 

na_counts <- colSums(is.na(test_df))
na_counts[na_counts > 0]



# PREDICTIONS ON TEST SET 

# Regression prediction
test_df$rf_pred_Number_Diagnosed <- predict(rf_model, test_df)
test_df$lm_pred_Number_Diagnosed <- predict(lm_model, test_df)
test_df$nnet_pred_Number_Diagnosed <- predict(nnet_model, test_df)


# Classification prediction 

test_df$rf_pred_Type <- predict(rf_class_model, test_df, type = "response")

# Evaluation Metrics - only if test data has actual values
if("Number_Diagnosed" %in% names(test_df) && "Type" %in% names(test_df)) {
  rf_reg_mae <- mean(abs(test_df$Number_Diagnosed - test_df$rf_pred_Number_Diagnosed), na.rm = TRUE)
  rf_reg_rmse <- sqrt(mean((test_df$Number_Diagnosed - test_df$rf_pred_Number_Diagnosed)^2, na.rm = TRUE))
  rf_class_acc <- mean(test_df$rf_pred_Type == test_df$Type, na.rm = TRUE)
  lm_reg_mae <- mean(abs(test_df$Number_Diagnosed - test_df$lm_pred_Number_Diagnosed), na.rm = TRUE)
  lm_reg_rmse <- sqrt(mean((test_df$Number_Diagnosed - test_df$lm_pred_Number_Diagnosed)^2, na.rm = TRUE))
  nnet_reg_mae <- mean(abs(test_df$Number_Diagnosed - test_df$nnet_pred_Number_Diagnosed), na.rm = TRUE)
  nnet_reg_rmse <- sqrt(mean((test_df$Number_Diagnosed - test_df$nnet_pred_Number_Diagnosed)^2, na.rm = TRUE))

  cat("Random Forest Regression:\nMAE:", rf_reg_mae, "\nRMSE:", rf_reg_rmse, "\n\n")
  cat("Linear Regression:\nMAE:", lm_reg_mae, "\nRMSE:", lm_reg_rmse, "\n\n")
  cat("neural network Regression:\nMAE:", nnet_reg_mae, "\nRMSE:", nnet_reg_rmse, "\n\n")
  cat("Random Forest Classification Accuracy:", rf_class_acc, "\n")
}

# CONCLUSION 

#Random Forest Regression:
#MAE: 29.9798 
#RMSE: 59.35068 

#Linear Regression:
#MAE: 554.988 
#RMSE: 697.4181 

#neural network Regression:
#MAE: 490.8242 
#RMSE: 490.9091 

#Random Forest Classification Accuracy: 0.5672447 

#FUTURE PREDICTIONS (2021-2050)

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
future_data$Predicted_Cancer_Type <- predict(rf_class_model, future_data)

# Export to Excel
# -------------------------------
write.xlsx(
  future_data,
  file = "cancer_predictions_dual_output_2021_2030.xlsx",
  sheetName = "Predictions",
  rowNames = FALSE
)