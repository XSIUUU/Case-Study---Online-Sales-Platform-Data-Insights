####################
# Data Engineering #
# Group6           #
####################

# Load Wehkamp data ----
# Download the Wehkamp data directly from the server ----
## Load relevant libraries ----
library(rstudioapi)
library(RPostgres)

## Build date variable in wehkamp data ----
wehkamp_data$last_session_dtime <- as.Date(wehkamp_data$last_session_dtime)

# Download COVID data from the web ----
Covid_cases <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

## Only focus on data from the Netherlands ----
Covid_cases_Reduced <- Covid_cases[Covid_cases$Country.Region == "Netherlands",]

## Only focus on data from the main country ----
Covid_cases_Reduced_Further <- Covid_cases_Reduced[Covid_cases_Reduced$Province.State == "",]

## Transpose the dataframe and exclude the first four entries ----
Covid_cases_df <- data.frame(t(Covid_cases_Reduced_Further[,-c(1:4)]))

## Extract a date variable from the row names ----
Covid_cases_df$Date <- as.Date(row.names(Covid_cases_df),"X%m.%d.%y")

## Focus on 2021 data(due to building difference, extract 1 more day and left join later) ----
Covid_cases_2021_22_df <- Covid_cases_df[Covid_cases_df$Date >= "2020-12-31",]

## Rename focal variable ----
names(Covid_cases_2021_22_df)[names(Covid_cases_2021_22_df)=="X201"] <- "CovidCases"

## Make a plot of the focal variable ----
plot(Covid_cases_2021_22_df$Date,Covid_cases_2021_22_df$CovidCases,type="l")

## Create a new variable, consisting of new cases ----
Covid_cases_2021_22_df$NewCovidCases <- c(NA,diff(Covid_cases_2021_22_df$CovidCases,1))

## Make a plot of the new variable ----
plot(Covid_cases_2021_22_df$Date,Covid_cases_2021_22_df$NewCovidCases,type="l")

## Replace one very extreme outlier ----
Covid_cases_2021_22_df$NewCovidCases[Covid_cases_2021_22_df$NewCovidCases > 150000] <- max(Covid_cases_2021_22_df$NewCovidCases[Covid_cases_2021_22_df$NewCovidCases < 150000], na.rm = TRUE)

## Make a plot of the new variable again - does this look better? ----
plot(Covid_cases_2021_22_df$Date,Covid_cases_2021_22_df$NewCovidCases,type="l")

## Removing first row of 2020
Covid_cases_2021_22_df <- Covid_cases_2021_22_df[-1,]

## Add a week variable ----
library(lubridate)
Covid_cases_2021_22_df$Week <- week(Covid_cases_2021_22_df$Date)


## Calculate weekly averages ----
Weeks <- unique(Covid_cases_2021_22_df$Week)
for (iWeek in Weeks) {
  Covid_cases_2021_22_df$WeekAverage[Covid_cases_2021_22_df$Week == iWeek] <- mean(Covid_cases_2021_22_df$NewCovidCases[Covid_cases_2021_22_df$Week == iWeek],na.rm = TRUE)  
}

## Make a plot to check whether we created the weekly average correctly----
plot(Covid_cases_2021_22_df$Date,Covid_cases_2021_22_df$NewCovidCases,type="l")
lines(Covid_cases_2021_22_df$Date,Covid_cases_2021_22_df$WeekAverage,type="l", col="Red")

# Clean up after getting external data ----
rm(Covid_cases, Covid_cases_df,Covid_cases_Reduced,Covid_cases_Reduced_Further,iWeek,Weeks) 

# Combine Wehkamp data and external data ----
install.packages("dplyr")  
library(dplyr)
wehkamp_covid <- left_join(wehkamp_data, Covid_cases_2021_22_df, by = c("last_session_dtime" = "Date"))

# Rename and remove unnecessary variable of weather data----
weather.data <- weather.data %>% rename(date=YYYYMMDD,mean_temp=Daily.mean.temperature,sunshine_duration=Sunshine.duration,precipitation_duration=Precipitation.duration,precipitation_amount=Daily.precipitation.amount,cloud_cover=Mean.daily.cloud.cover) %>% select(,-Percentage.of.maximum.potential.sunshine.duration)
weather.data <- weather.data %>% select(-STN)
weather.data$date <- as_date(as.character(weather.data$date), format = "%Y%m%d")

# left join of weather data----
wehkamp_covid_weather <- left_join(wehkamp_covid, weather.data, by=c("last_session_dtime" = "date"))

# load the trend data ----
trend <- `Wehkamp.interest.(google.trends)`

# Convert ‘Week’ column to Date type----
library(lubridate)
trend$Week <- as.Date(trend$Week, format = "%Y/%m/%d")

# Expand with trend to daily date----
library(dplyr)
trend_expanded <- trend %>%
  rowwise() %>%
  do(data.frame(Week = .$Week, 
                weekly_trend = .$Wehkamp, 
                Date = seq(.$Week, by = "day", length.out = 7))) %>% select(,-Week)

# merge google trends data with the rest----
wehkamp_covid_weather_googletrend <- left_join(wehkamp_covid_weather, trend_expanded, by=c("last_session_dtime" = "Date"))
View(wehkamp_covid_weather_googletrend)

## export csv----
write.csv(wehkamp_covid_weather_googletrend, file = "wehkamp_covid_weather_googletrend.csv", row.names = FALSE)
getwd()

#####
# DATA MERGING DONE----
#####

#####
# data type conversion start----
#####
# Transform gender,device code ----
merged_data <- wehkamp_covid_weather_googletrend %>%
  mutate(gender = case_when(
    gender == "MkoiQQyrlgoFB0aa4gKu4zm73_w" ~ 1,       #male
    gender == "mutKSpyMJM_ZQCATL6NlLjlnF3Y" ~ 2,       #female
    gender == "L8ptf3Nk2ejeZuxuRSeUD2EHDT4" ~ 0,       #other
    TRUE ~ as.numeric(gender)  
  ))%>%
  mutate(device = case_when(
    device == "mobile" ~ 1,       #mobile
    device == "tablet" ~ 2,       #tablet
    device == "desktop" ~ 3,      #desktop
    TRUE ~ as.numeric(device)  
  ))

# Transform chr to num----
merged_data <- merged_data %>%
  mutate(
    clothing_budget = as.numeric(gsub("\\..*", "", clothing_budget)),
    clothing_fashionable = as.numeric(gsub("\\..*", "", clothing_fashionable)),
    clothing_segment = as.numeric(gsub("\\..*", "", clothing_segment)),
    consumption_frequency = as.numeric(gsub("\\..*", "", consumption_frequency)),
    education = as.numeric(gsub("\\..*", "", education)),
    age = as.numeric(gsub("\\..*", "", age)),
    household_composition = as.numeric(gsub("\\..*", "", household_composition)),
    income = as.numeric(gsub("\\..*", "", income)),
    newspaper_type = as.numeric(gsub("\\..*", "", newspaper_type)),
    urbanisation = as.numeric(gsub("\\..*", "", urbanisation))
  )

# Transform Interger64 to num----
library(bit64)
merged_data <- merged_data %>% 
  mutate(across(c(n_product_view, n_purchase, n_add, n_remove), as.numeric))

## Turning 0 value to na (meeting the requirement to treat "other" as missing data)----
library(dplyr)

merged_data <- merged_data %>%
  mutate(household_composition = ifelse(household_composition == 0, NA, household_composition))%>%
  mutate(age = ifelse(age == 0, NA, age))%>%
  mutate(education = ifelse(education == 0, NA, education))%>%
  mutate(newspaper_type = ifelse(newspaper_type == 0, NA, newspaper_type))%>%
  mutate(consumption_frequency = ifelse(consumption_frequency == 0, NA, consumption_frequency))%>%
  mutate(clothing_budget = ifelse(clothing_budget == 0, NA, clothing_budget))%>%
  mutate(clothing_fashionable = ifelse(clothing_fashionable == 0, NA, clothing_fashionable))%>%
  mutate(clothing_segment = ifelse(clothing_segment == 0, NA, clothing_segment))%>%
  mutate(income = ifelse(income == 0, NA, income))%>%
  mutate(gender = ifelse(gender == 0, NA, gender))

summary(merged_data)

## export csv
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)
getwd()

#####
# data type conversion done----
#####

################

# DATA CLEANING----

################
# Extract main variables (conversion and all the variables with missing data)----
main_data.mis <- subset(merged_data, select = c(session_conversion, clothing_budget,clothing_fashionable,clothing_segment,
                                                consumption_frequency, education, age, household_composition, 
                                                income, newspaper_type, urbanisation, gender, device
)) 
cor(main_data.mis[, c('household_composition', 'clothing_budget', 'clothing_fashionable',
                      "clothing_segment","consumption_frequency","education","age","income","newspaper_type",
                      "urbanisation","gender","device")], use = "pairwise.complete.obs")

# listwise deletion model----
ListwiseDeletionModel <- lm(session_conversion~
                              clothing_budget+clothing_fashionable+clothing_segment+
                              consumption_frequency+education+age+household_composition+
                              income+newspaper_type+urbanisation+gender+device,data=main_data.mis)
summary(ListwiseDeletionModel)

# Load MICE library ----
library(mice)

## Inspect pattern of missings ----
md.pattern(main_data.mis)

## Indicate which variables can be used for predicting missings ----

PredictorMatrix <-
  #1  2  3  4  5  6  7  8  9  10 11 12 13
  matrix(c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,         #1 conversion rate
           1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0,         #2 clothing_budget
           0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0,         #3 clothing_fashionable
           0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0,         #4clothing_segment
           1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0,         #5   consumption_frequency
           0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0,         #6 education
           0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0,         #7   age
           1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0,         #8 household_composition
           1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0,         #9  income
           0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,         #10  newspaper_type
           0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0,         #11  urbanisation
           0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,         #12 gender
           0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0           #13 device
           
  ), nrow = 13, ncol = 13, byrow = TRUE
  )

## Impute data ----
MiceImputedData <- mice(main_data.mis, m=5, maxit = 50, method = 'pmm', predictorMatrix = PredictorMatrix, seed = 500)
summary(MiceImputedData)

## Build predictive model for all generated imputed data sets ----
MiceAllModels <- with(data = MiceImputedData, exp = lm(session_conversion~
                                                         clothing_budget+clothing_fashionable+clothing_segment+
                                                         consumption_frequency+education+age+household_composition+
                                                         income+newspaper_type+urbanisation+gender+device
)) 

## Combine results of all 5 models ----
MiceCombineModels <- pool(MiceAllModels)
summary(MiceCombineModels)

# Extract the first data from combination data and merged to other variables that had no NA----
## Step 1: Extract the dataset after the first imputation----
first_imputed_data <- complete(MiceImputedData, 1)
summary(first_imputed_data)

## Step 2: Extract other variables from the original dataset that need to be merged----
other_variables <- subset(merged_data, select = c(internet_session_id, most_customer_id, last_session_dtime,n_product_view, n_add, WeekAverage, 
                                                  mean_temp, sunshine_duration, precipitation_duration, precipitation_amount, cloud_cover, 
                                                  weekly_trend))

## Step 3: Combine the imputed data set with the other variables by row----
completed_data <- cbind(other_variables, first_imputed_data)
summary(completed_data)

## export csv
write.csv(completed_data, file = "wehkamp_after_inputed.csv", row.names = FALSE)
getwd()

######

# Detect outliers using the Mahalonobis Distance

######

# Data loading----
temperal_data <- wehkamp_after_inputed
w_numeric <- wehkamp_after_inputed[,4:25]

# Calculate variance matrix ----
w_cov <- cov(w_numeric)

# Calculate Mahalanobis distance ----
w_mahal <- mahalanobis(x = w_numeric, center = colMeans(w_numeric), cov = w_cov)
w_mahal
sorted_mahal <- sort(w_mahal)

# Plot Mahalanobis distance for each point ----
plot(density(w_mahal, bw = 0.5),
     main="Squared Mahalanobis distances, n=536765, p=22",lwd=2) ; rug(w_mahal)

# Plot Chi-square distribution with df = 22 ----
xrange <- seq(0,12,by=.001)
lines(xrange,dchisq(xrange,22),col="dodgerblue",lwd=2)

# Make Q-Q plot to assess Mahalanobis distance ----
qqplot(qchisq(ppoints(100), df = 22), w_mahal,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[22]^2))
abline(0, 1, col = 'gray')


# scatterplot pot for checking outliers and deviation----
plot(1:length(sorted_mahal), sorted_mahal, 
     main = "Ordered Mahalanobis Distances",
     xlab = "Observation Index (Sorted)", 
     ylab = "Mahalanobis Distance",
     pch = 19, 
     col = "blue")
abline(h = qchisq(p=0.95, df = ncol(w_numeric)), col ="red", lty = 2)  #check the outlier within 2 standard deviations
abline(h = qchisq(p=0.9938 , df = ncol(w_numeric)), col ="green", lty = 2)  #check the outlier within 2.5 standard deviations
abline(h = qchisq(p=0.9973 , df = ncol(w_numeric)), col ="purple", lty = 2)  #check the outlier within 3 standard deviations

# Set a threshold for outliers as 3 standard deviation----
threshold <- qchisq(p = 0.9973, df = ncol(w_numeric))

# Identify outliers (TRUE if Mahalanobis distance exceeds the threshold)----
w_outliers <- w_mahal > threshold

# Add a column in the original data frame to mark outliers----
temperal_data$outlier <- ifelse(w_mahal > threshold, TRUE, FALSE)
temperal_data

#  outlier numbers----
sum(temperal_data$outlier)
#  outlier percentage----
(sum(temperal_data$outlier) / length(temperal_data$outlier)) * 100

# Remove all outlier----
Final_wehkamp <- temperal_data[temperal_data[, 26] == FALSE,c(1:25)]

summary(Final_wehkamp)

## export csv
write.csv(Final_wehkamp, file = "Final_wehkamp_data.csv", row.names = FALSE)
getwd()


####################
#data analysis----
####################
# Load packages ----
library(dplyr)
library(ggplot2)
library(car)
install.packages("gmodels")
library(gmodels)
install.packages("nortest")
library(nortest)

# Read in the data, and provide overview of the variables ----
str(Final_wehkamp)
dim (Final_wehkamp)
summary(Final_wehkamp)

# Hypothesis Testing - WHO ----

# Gender ---- 
## CHI2-test----
gender_test <- chisq.test(Final_wehkamp$gender, Final_wehkamp$session_conversion)
gender_test
## Overview of CHI2 test----
gender_test$observed
gender_test$expected
## Explore the relationship between two categorical variables----
CrossTable(Final_wehkamp$gender, Final_wehkamp$session_conversion)
## visualization of relationship between gender and conversion with barplot----
gender_conversion <- Final_wehkamp[,c("gender","session_conversion")]
gender_purchase_summary <- gender_conversion %>%
  filter(session_conversion == 1) %>% 
  group_by(gender) %>%
  summarise(total_purchases = n()) 
ggplot(gender_purchase_summary, aes(x = gender, y = total_purchases)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total conversion by gender group", x = "Gender Group", y = "Total conversion") +
  theme_minimal()

# Age ----
## CHI2-test----
age_test <- chisq.test(Final_wehkamp$age, Final_wehkamp$session_conversion)
age_test
## Overview of CHI2 test----
age_test$observed
age_test$expected
## Explore the relationship between two categorical variables----
CrossTable(Final_wehkamp$age, Final_wehkamp$session_conversion)
## visualization of relationship between age and conversion with barplot----
age_conversion <- Final_wehkamp[,c("age","session_conversion")]
age_purchase_summary <- age_conversion %>%
  filter(session_conversion == 1) %>% 
  group_by(age) %>%
  summarise(total_purchases = n()) 
ggplot(age_purchase_summary, aes(x = age, y = total_purchases)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total conversion by age group", x = "Age Group", y = "Total conversion") +
  theme_minimal()

# Education ----
## CHI2-test
education_test <- chisq.test(Final_wehkamp$education, Final_wehkamp$session_conversion)
education_test
## Overview of CHI2 test----
education_test$observed
education_test$expected
## Explore the relationship between two categorical variables----
CrossTable(Final_wehkamp$education, Final_wehkamp$session_conversion)
## visualization of relationship between education and conversion with barplot----
education_conversion <- Final_wehkamp[,c("education","session_conversion")]
education_purchase_summary <- education_conversion %>%
  filter(session_conversion == 1) %>% 
  group_by(education) %>%
  summarise(total_purchases = n()) 
ggplot(education_purchase_summary, aes(x = education, y = total_purchases)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total conversion by education group", x = "Education Group", y = "Total conversion") +
  theme_minimal()


# Incomce ---- 
##Creating a sample----
wehkampsample <- Final_wehkamp[sample(1:nrow(Final_wehkamp), 250, replace=FALSE),]
## visualization of income by scatter plot with trendline----
library(ggplot2)
ggplot(wehkampsample, aes(x = income, y = session_conversion)) +
  geom_jitter(height = 0.05, alpha = 0.5) +  # Increase jitter and avoid data overlap
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +  # # Fitting curves using logistic regression
  labs(
    title = "Scatterplot -  income vs conversion",
    x = "income  (Independent Variable)",
    y = "Session Conversion (Dependent Variable)"
  ) +
  theme_minimal()
##Logistic regression of income----
glm_income <- glm(session_conversion ~ income, family = "binomial", data = Final_wehkamp)
summary(glm_income)


# Household composition ----
## CHI2-test
household_composition_test <- chisq.test(Final_wehkamp$household_composition, Final_wehkamp$session_conversion)
household_composition_test
## Overview of CHI2 test----
household_composition_test$observed
household_composition_test$expected
## Explore the relationship between two categorical variables----
CrossTable(Final_wehkamp$household_composition, Final_wehkamp$session_conversion)
## visualization of relationship between household composition and conversion with barplot----
household_composition_conversion <- Final_wehkamp[,c("household_composition","session_conversion")]
household_composition_purchase_summary <- household_composition_conversion %>%
  filter(session_conversion == 1) %>% 
  group_by(household_composition) %>%
  summarise(total_purchases = n()) 
ggplot(household_composition_purchase_summary, aes(x = household_composition, y = total_purchases)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total conversion by Household Composition group", x = "Household Composition Group", y = "Total conversion") +
  theme_minimal()


# Newspaper type ----
## CHI2-test
newspaper_type_test <- chisq.test(Final_wehkamp$newspaper_type, Final_wehkamp$session_conversion)
newspaper_type_test
## Overview of CHI2 test----
newspaper_type_test$observed
newspaper_type_test$expected
## Explore the relationship between two categorical variables----
CrossTable(Final_wehkamp$newspaper_type, Final_wehkamp$session_conversion)
## visualization of relationship between newspaper type and conversion with barplot----
newspaper_type_conversion <- Final_wehkamp[,c("newspaper_type","session_conversion")]
newspaper_type_purchase_summary <- newspaper_type_conversion %>%
  filter(session_conversion == 1) %>% 
  group_by(newspaper_type) %>%
  summarise(total_purchases = n()) 
ggplot(newspaper_type_purchase_summary, aes(x = newspaper_type, y = total_purchases)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total conversion by newspaper type group", x = "Newspaper type group", y = "Total conversion") +
  theme_minimal()


# Consumption frequency ----
## CHI2-test
consumption_frequency_test <- chisq.test(Final_wehkamp$consumption_frequency, Final_wehkamp$session_conversion)
consumption_frequency_test
## Overview of CHI2 test----
consumption_frequency_test$observed
consumption_frequency_test$expected
## Explore the relationship between two categorical variables----
CrossTable(Final_wehkamp$consumption_frequency, Final_wehkamp$session_conversion)
## visualization of relationship between consumption frequency and conversion with barplot----
consumption_frequency_conversion <- Final_wehkamp[,c("consumption_frequency","session_conversion")]
consumption_frequency_purchase_summary <- consumption_frequency_conversion %>%
  filter(session_conversion == 1) %>% 
  group_by(consumption_frequency) %>%
  summarise(total_purchases = n()) 
ggplot(consumption_frequency_purchase_summary, aes(x = consumption_frequency, y = total_purchases)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total conversion by consumption frequency group", x = "Consumption frequency group", y = "Total conversion") +
  theme_minimal()


# Hypothesis Testing - WHAT ----

# Clothing budget ----
## visualization of clothing_budget by scatter plot----
plot(wehkampsample$clothing_budget, wehkampsample$session_conversion, main="clothing_budget  vs session_conversion", xlab='clothing_budget', ylab='churn (0=no conversion, 1 =conversion)')
plot(wehkampsample$clothing_budget,jitter(wehkampsample$session_conversion), main="clothing_budget  vs session_conversion", xlab='clothing_budget', ylab='churn (0=no conversion, 1 = conversion)')
## visualization of clothing_budget by scatter plot with trendline----
library(ggplot2)
ggplot(wehkampsample, aes(x = clothing_budget, y = session_conversion)) +
  geom_jitter(height = 0.05, alpha = 0.5) +  # Increase jitter and avoid data overlap
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +  # # Fitting curves using logistic regression
  labs(
    title = "Scatterplot - Clothing budget vs conversion",
    x = "Clothing budget (Independent Variable)",
    y = "Session Conversion (Dependent Variable)"
  ) +
  theme_minimal()
##Logistic regression of clothing budget----
glm_cb <- glm(session_conversion ~ clothing_budget, family = "binomial", data = Final_wehkamp_data)
summary(glm_cb)

# Clothing fashionable ----
## visualization of clothing_fashionable by scatter plot----
plot(wehkampsample$clothing_fashionable, wehkampsample$session_conversion, main="clothing_fashionable  vs session_conversion", xlab='clothing_fashionable', ylab='churn (0=no conversion, 1 =conversion)')
plot(wehkampsample$clothing_fashionable,jitter(wehkampsample$session_conversion), main="clothing_fashionable  vs session_conversion", xlab='clothing_fashionable', ylab='churn (0=no conversion, 1 = conversion)')
## visualization of clothing_fashionable by scatter plot with trendline----
library(ggplot2)
ggplot(wehkampsample, aes(x = clothing_fashionable, y = session_conversion)) +
  geom_jitter(height = 0.05, alpha = 0.5) +  # Increase jitter and avoid data overlap
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +  # # Fitting curves using logistic regression
  labs(
    title = "Scatterplot - Clothing fashionable vs conversion",
    x = "Clothing Fashionable (Independent Variable)",
    y = "Session Conversion (Dependent Variable)"
  ) +
  theme_minimal()
###Logistic regression of clothing_fashionable
glm_cf <- glm(session_conversion ~ clothing_fashionable, family = "binomial", data = Final_wehkamp_data)
summary(glm_cf)


# Clothing Segment (CHI2-test) ----
## visualization of clothing_segment by mosaicplot----
## CHi2 test for clothing_segment
install.packages("gmodels")
library(gmodels)
We_test <- chisq.test(Final_wehkamp_data$session_conversion, Final_wehkamp_data$clothing_segment)
We_test
We_test$observed
We_test$expected
CrossTable(Final_wehkamp_data$session_conversion, Final_wehkamp_data$clothing_segment)

## mosaicplot plot for clothing_segment ----
### Convert variables to tabular format ----
table_data <- table(Final_wehkamp_data$session_conversion, Final_wehkamp_data$clothing_segment)
### Set up a colour scheme----
color_scheme <- c("lightsteelblue", "palegreen", "thistle", "peachpuff", "rosybrown")
### Visualising the relationship between clothing_segment and transformation through Mosaic Plot----
mosaicplot(table_data, main = "Mosaic Plot - Conversion vs Clothing Segment",
           xlab = "Session Conversion", ylab = "Clothing Segment", col = color_scheme, border = "gray40")

# Device use ----
## visualization of device by mosaicplot----
## CHi2 test for device
We_test <- chisq.test(Final_wehkamp_data$session_conversion, Final_wehkamp_data$device)
We_test
We_test$observed
We_test$expected
CrossTable(Final_wehkamp_data$session_conversion, Final_wehkamp_data$device)

## mosaicplot plot for clothing_segment ----
### Convert variables to tabular format ----
table_data <- table(Final_wehkamp_data$session_conversion, Final_wehkamp_data$device)
### Set up a colour scheme----
color_scheme <- c("lightsteelblue", "palegreen", "thistle", "peachpuff", "rosybrown")
### Visualising the relationship between clothing_segment and transformation through Mosaic Plot----
mosaicplot(table_data, main = "Mosaic Plot - Conversion vs device",
           xlab = "Session Conversion", ylab = "device", col = color_scheme, border = "gray40")

# Hypothesis Testing - WHEN ----

# product views —---
## H:More product views lead to more conversion. - n_product_view (numerical)----
## visualization of n_product_view by scatter plot with trendline----
wehkampsample <- Final_wehkamp[sample(1:nrow(Final_wehkamp), 250, replace=FALSE),]
ggplot(wehkampsample, aes(x = n_product_view, y = session_conversion)) +
  geom_point(alpha = 0.6) +  
  geom_jitter(height = 0.05, alpha = 0.5) +  # Increase jitter and avoid data overlap
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Conversions vs Number of Product Views",
       x = "Number of Product Views",
       y = "Conversions") +
  theme_minimal()
## visualization of n_product_view by scatter plot----
plot(wehkampsample$n_product_view, wehkampsample$session_conversion, main="conversions vs. number of product views", xlab='Conversion', ylab='Number of product views')
plot(wehkampsample$n_product_view, jitter(wehkampsample$session_conversion), main="conversions vs. number of product views", xlab='Conversion', ylab='Number of product views')
## logit regression (categorical vs numerical)----
npv1 <- glm(formula = n_product_view ~ session_conversion, family = "binomial", data = Final_wehkamp)
summary(npv1) # significant  at p = 0.001, the number of product views correlates negatively with conversion


# trends ----
## The more Wehkamp is trending, the more conversion they have. - weekly_trend (categorical vs. numerical)----
## visualization of n_product_view by scatter plot with trendline----
ggplot(wehkampsample, aes(x = weekly_trend, y = session_conversion)) +
  geom_point(alpha = 0.6) +  
  geom_jitter(height = 0.05, alpha = 0.5) +  # Increase jitter and avoid data overlap
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Conversion vs Wehkamp Trend",
       x = "Wehkamp Trend",
       y = "Conversion Rate") +
  theme_minimal()
## visualization of trend by scatter plot----
plot(weather_sample$weekly_trend, weather_sample$session_conversion, main="conversions vs. trend", xlab='Conversion rate', ylab='Weekly trend')
plot(weather_sample$weekly_trend, jitter(weather_sample$session_conversion), main="conversions vs. trend", xlab='Conversion rate', ylab='Weekly trend')
## logit regression (categorical vs numerical)----
trend1 <- glm(formula = weekly_trend ~ session_conversion, family = "binomial", data = Final_wehkamp)
summary(trend1) # no significant effect of trend on conversion (only on 10 % significance level)


################################################################################
### Do the analysis of weather data on a daily level with a regression analysis----
###############################################################################

# change all the data to daily as aggregation level (in order to study the relationship between daily weather and daily customer actions)----
daily_data <- Final_wehkamp %>%
  group_by(last_session_dtime) %>%
  summarise(
    mean_temp = max(mean_temp),
    sunshine_duration=max(sunshine_duration),
    precipitation_duration=max(precipitation_duration),
    precipitation_amount=max(precipitation_amount),
    cloud_cover=max(cloud_cover),
    daily_sessions = n(),  # Total number of sessions per day
    daily_conversion = sum(session_conversion==1),  # Number of purchases per day
    daily_conversion_rate = daily_conversion / daily_sessions  # Calculate Daily conversion rate
  )
# order data by date
daily_data <- daily_data[order(daily_data$last_session_dtime), ] # order by date
# change date
daily_data$last_session_dtime <- as.Date(daily_data$last_session_dtime)



# for mean_temp ----
## Visualisation of the trend between average daily temperature and daily conversion rate through a bipartite line graph----
### Creating the main image--------
plot(daily_data$last_session_dtime, daily_data$daily_conversion_rate,
     type = "l",  # Set to line
     col = "blue",  
     lwd = 2,  # line width
     ylim = c(0, max(daily_data$daily_conversion_rate, na.rm = TRUE)),  # y-axis range
     xlab = "Date",  # x-axis labels
     ylab = "Daily conversion rate",  # y-axis labels
     main = "Daily conversion rate and Mean Temperature"  # title
)
### Add a second y-axis lable--------
par(new = TRUE)  # Allows drawing on the same graph
plot(daily_data$last_session_dtime, daily_data$mean_temp,
     type = "l",  
     col = "red", 
     lwd = 2,  
     axes = FALSE,  # No y-axis is plotted
     xlab = "",  # No x-axis labels are drawn
     ylab = "",  # No y-axis labels are drawn
     ylim = c(0, max(daily_data$mean_temp, na.rm = TRUE))  # Setting the y-axis range
)
### Add a second y-axis lable---- lable----
axis(side = 4, at = pretty(range(daily_data$mean_temp, na.rm = TRUE)))  # The y-axis is on the right
mtext("Daily Mean Temperature", side = 4, line = 3)  # Add right-hand y-axis labels
### Add explanation--------
legend("topright", legend = c("Daily conversion rate", "Daily Mean Temperature"),
       col = c("blue", "red"),lwd = 1)

## Scatterplot and trendline of daily average temperature and conversion rate----
ggplot(daily_data, aes(x = mean_temp, y = daily_conversion_rate)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Daily Conversion Rate vs Mean Temperature",
       x = "Mean Temperature",
       y = "Daily Conversion Rate") +
  theme_minimal()

## Regression model for mean temperature (numerical vs numerical)----
temp1 <- lm(formula = daily_conversion_rate ~ mean_temp, data = daily_data)
summary(temp1) # significant at p=0.001, daily_mean_temp correlates positively to daily_conversion_rate



# for sunshine_duration ----
## Visualisation of the trend between sunshine_duration and daily conversion rate through a bipartite line graph----
### Creating the main image--------
plot(daily_data$last_session_dtime, daily_data$daily_conversion_rate,
     type = "l",  # Set to line
     col = "blue",  
     lwd = 2,  # line width
     ylim = c(0, max(daily_data$daily_conversion_rate, na.rm = TRUE)),  # y-axis range
     xlab = "Date",  # x-axis labels
     ylab = "Daily conversion rate",  # y-axis labels
     main = "Daily conversion rate and Sunshine Duration"  # title
)
### Add a second y-axis lable--------
par(new = TRUE)  # Allows drawing on the same graph
plot(daily_data$last_session_dtime, daily_data$sunshine_duration,
     type = "l",  
     col = "red", 
     lwd = 2,  
     axes = FALSE,  # No y-axis is plotted
     xlab = "",  # No x-axis labels are drawn
     ylab = "",  # No y-axis labels are drawn
     ylim = c(0, max(daily_data$sunshine_duration, na.rm = TRUE))  # Setting the y-axis range
)
### Add a second y-axis lable---- lable----
axis(side = 4, at = pretty(range(daily_data$sunshine_duration, na.rm = TRUE)))  # The y-axis is on the right
mtext("Daily Sunshine Duration", side = 4, line = 3)  # Add right-hand y-axis labels
### Add explanation----
legend("topright", legend = c("Daily conversion rate", "Daily Sunshine Duration"),
       col = c("blue", "red"),lwd = 1)

## Scatterplot and trendline of sunshine_duration and conversion rate----
ggplot(daily_data, aes(x = sunshine_duration, y = daily_conversion_rate)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Daily Conversion Rate vs Sunshine Duration",
       x = "Sunshine Duration",
       y = "Daily Conversion Rate") +
  theme_minimal()

## Regression model for sunshine_duration (numerical vs numerical)----
sun1 <- lm(formula = daily_conversion_rate ~ sunshine_duration, data = daily_data)
summary(sun1) # significant at p=0.001, daily sunshine_duration correlates positively to daily_conversion_rate


# for precipitation_duration ----
## Visualisation of the trend between precipitation_duration and daily conversion rate through a bipartite line graph----
### Creating the main image--------
plot(daily_data$last_session_dtime, daily_data$daily_conversion_rate,
     type = "l",  # Set to line
     col = "blue",  
     lwd = 2,  # line width
     ylim = c(0, max(daily_data$daily_conversion_rate, na.rm = TRUE)),  # y-axis range
     xlab = "Date",  # x-axis labels
     ylab = "Daily conversion rate",  # y-axis labels
     main = "Daily conversion rate and Precipitation Duration"  # title
)
### Add a second y-axis lable----
par(new = TRUE)  # Allows drawing on the same graph
plot(daily_data$last_session_dtime, daily_data$precipitation_duration,
     type = "l",  
     col = "red", 
     lwd = 2,  
     axes = FALSE,  # No y-axis is plotted
     xlab = "",  # No x-axis labels are drawn
     ylab = "",  # No y-axis labels are drawn
     ylim = c(0, max(daily_data$precipitation_duration, na.rm = TRUE))  # Setting the y-axis range
)
### Add a second y-axis lable----
axis(side = 4, at = pretty(range(daily_data$precipitation_duration, na.rm = TRUE)))  # The y-axis is on the right
mtext("Daily Precipitation Duration", side = 4, line = 3)  # Add right-hand y-axis labels
### Add explanation----
legend("topright", legend = c("Daily conversion rate", "Daily Precipitation Duration"),
       col = c("blue", "red"),lwd = 1)

## Scatterplot and trendline of precipitation_duration and conversion rate----
ggplot(daily_data, aes(x = precipitation_duration, y = daily_conversion_rate)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Daily Conversion Rate vs Precipitation Duration",
       x = "Precipitation Duration",
       y = "Daily Conversion Rate") +
  theme_minimal()

## Regression model for precipitation_duration (numerical vs numerical)----
pred1 <- lm(formula = daily_conversion_rate ~ precipitation_duration, data = daily_data)
summary(pred1) # significant at p=0.001, daily precipitation_duration correlates negatively to daily_conversion_rate


# for precipitation_amount ----
## Visualisation of the trend between precipitation_amount and daily conversion rate through a bipartite line graph----
### Creating the main image----
plot(daily_data$last_session_dtime, daily_data$daily_conversion_rate,
     type = "l",  # Set to line
     col = "blue",  
     lwd = 2,  # line width
     ylim = c(0, max(daily_data$daily_conversion_rate, na.rm = TRUE)),  # y-axis range
     xlab = "Date",  # x-axis labels
     ylab = "Daily conversion rate",  # y-axis labels
     main = "Daily conversion rate and Precipitation Amount"  # title
)
### Add a second y-axis lable----
par(new = TRUE)  # Allows drawing on the same graph
plot(daily_data$last_session_dtime, daily_data$precipitation_amount,
     type = "l",  
     col = "red", 
     lwd = 2,  
     axes = FALSE,  # No y-axis is plotted
     xlab = "",  # No x-axis labels are drawn
     ylab = "",  # No y-axis labels are drawn
     ylim = c(0, max(daily_data$precipitation_amount, na.rm = TRUE))  # Setting the y-axis range
)
### Add a second y-axis lable---- lable
axis(side = 4, at = pretty(range(daily_data$precipitation_amount, na.rm = TRUE)))  # The y-axis is on the right
mtext("Daily Precipitation Amount", side = 4, line = 3)  # Add right-hand y-axis labels
### Add explanation----
legend("topright", legend = c("Daily conversion rate", "Daily Precipitation Amount"),
       col = c("blue", "red"),lwd = 1)

## Scatterplot and trendline of precipitation_amount and conversion rate----
ggplot(daily_data, aes(x = precipitation_amount, y = daily_conversion_rate)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Daily Conversion Rate vs Precipitation Amount",
       x = "Precipitation Amount",
       y = "Daily Conversion Rate") +
  theme_minimal()

## Regression model for precipitation_amount (numerical vs numerical)----
prea1 <- lm(formula = daily_conversion_rate ~ precipitation_amount, data = daily_data)
summary(prea1) # significant at p=0.001, daily_precipitation_amount correlates negatively to daily_conversion_rate


# for cloud_cover ----
### Creating the main image----
plot(daily_data$last_session_dtime, daily_data$daily_conversion_rate,
     type = "l",  # Set to line
     col = "blue",  
     lwd = 2,  # line width
     ylim = c(0, max(daily_data$daily_conversion_rate, na.rm = TRUE)),  # y-axis range
     xlab = "Date",  # x-axis labels
     ylab = "Daily conversion rate",  # y-axis labels
     main = "Daily conversion rate and Cloud Cover"  # title
)
### Add a second y-axis lable----
par(new = TRUE)  # Allows drawing on the same graph
plot(daily_data$last_session_dtime, daily_data$cloud_cover,
     type = "l",  
     col = "red", 
     lwd = 2,  
     axes = FALSE,  # No y-axis is plotted
     xlab = "",  # No x-axis labels are drawn
     ylab = "",  # No y-axis labels are drawn
     ylim = c(0, max(daily_data$cloud_cover, na.rm = TRUE))  # Setting the y-axis range
)
### Add a second y-axis lable---- lable
axis(side = 4, at = pretty(range(daily_data$cloud_cover, na.rm = TRUE)))  # The y-axis is on the right
mtext("Daily Cloud Cover", side = 4, line = 3)  # Add right-hand y-axis labels
### Add explanation----
legend("topright", legend = c("Daily conversion rate", "Daily Cloud Cover"),
       col = c("blue", "red"),lwd = 1)

## Scatterplot and trendline of cloud_cover and conversion rate----
ggplot(daily_data, aes(x = cloud_cover, y = daily_conversion_rate)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Daily Conversion Rate vs Cloud Coverage",
       x = "Cloud Coverage",
       y = "Daily Conversion Rate") +
  theme_minimal()

## Regression model for cloud_cover (numerical vs numerical)----
cloud1 <- lm(formula = daily_conversion_rate ~ cloud_cover, data = daily_data)
summary(cloud1) # significant at p=0.01, daily_cloud_cover correlates negatively to daily_conversion_rate


# test for multicollinearity among the daily weather variables ----
## logical regression of all the weather related variables----
## better weather leads to a higher conversion rate----
weather_complete <- lm(formula = precipitation_duration ~ mean_temp + sunshine_duration + precipitation_amount + cloud_cover, data = daily_data)
summary(weather_complete)

vif_values <- vif(weather_complete)
print(vif_values)
## look at correlation matrix----
## Create a subset with only the continuous predictor variables----
predictors <- Final_wehkamp[, c("mean_temp", "sunshine_duration", "precipitation_duration", "precipitation_amount", "cloud_cover")]
## Calculate the correlation matrix----
cor_matrix <- cor(predictors, use = "complete.obs")
## Display the correlation matrix----
print(cor_matrix)


# Hypothesis Testing - WHERE ----

# Higher level of urbanisation leads to a higher conversion rate----
## visualization of relationship between income and conversion with scartter plot----
plot(wehkampsample$urbanisation, wehkampsample$session_conversion, main="conversion vs urbanisation", xlab='Relationship length (in years)', ylab='churn (1=no churn, 2 = churn)')
plot(wehkampsample$urbanisation, jitter(wehkampsample$session_conversion), main="conversion vs urbanisation", xlab='Relationship length (in years)', ylab='churn (1=no churn, 2 = churn)')
## Logit regression of urbanisation(categorical vs numerical(likert scale))----
Where1 <- glm(formula = session_conversion ~ urbanisation, family = "binomial", data = Final_wehkamp_data)
summary(Where1)


## Higher average of Average new covid cases per week lead to higher conversion rate
## visualization of relationship between Average new covid cases per week and conversion with scartter plot----
plot(wehkampsample$WeekAverage, wehkampsample$session_conversion, main="conversion vs Average new covid cases per week", xlab='Conversion rate', ylab='Average new covid cases per week')
plot(wehkampsample$WeekAverage, jitter(wehkampsample$session_conversion), main="conversion vs Average new covid cases per week", xlab='Conversion rate', ylab='Average new covid cases per week')
## Logit regression of covid (categorical vs numerical(likert scale))----
Covid1 <- glm(formula = session_conversion ~ WeekAverage, family = "binomial", data = Final_wehkamp_data)
summary(Covid1)

###############################
#Construct a logistic regression model with all except the nominal variables----
################################
Final_model <- Final_wehkamp
summary(Final_model)

# Standardize the Variables (We chose only continuous and ordered variables, because nominal variables are automatically converted to dummy variables and there is no way to compare the weights.)----
Final_model$n_product_view <- scale(Final_model$n_product_view)
Final_model$WeekAverage <- scale(Final_model$WeekAverage)
Final_model$mean_temp <- scale(Final_model$mean_temp)
Final_model$weekly_trend <- scale(Final_model$weekly_trend)
Final_model$urbanisation <- scale(Final_model$urbanisation)
Final_model$clothing_budget <- scale(Final_model$clothing_budget)
Final_model$clothing_fashionable <- scale(Final_model$clothing_fashionable)
Final_model$consumption_frequency <- scale(Final_model$consumption_frequency)
Final_model$age <- scale(Final_model$age)
Final_model$income <- scale(Final_model$income)

# Fit the Logistic Regression Model----
logit_model <- glm(session_conversion ~ n_product_view + WeekAverage + mean_temp + weekly_trend + 
                     urbanisation + clothing_budget + clothing_fashionable + consumption_frequency + 
                     age + income, 
                   data = Final_model, family = binomial)

# View the model summary----
summary(logit_model)
# test the models robustness----
vif_values <- vif(logit_model)
print(vif_values)
# pseudo R2(Model fit testing)----
install.packages("pscl")
library(pscl)
pR2(logit_model)