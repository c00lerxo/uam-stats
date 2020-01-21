install.packages("magrittr", repos = "http://cran.us.r-project.org")
install.packages("moments")

library(magrittr)
library(moments)


# Task 2.1
###
# The variable result in the file survey.txt describes the results of
# the survey of the activities of the president of a certain city.
# 100 city residents were randomly selected, and they were asked the following question:
# How would you rate the activities of the city president?
# The following answers were available: definitely good (a), good (b), wrong (c),
# definitely wrong (d), I have no opinion (e).
# Which type is this variable? What are its possible values?

# This is a qualitative (categorical) random variable, as there exists finite set of
# its categories and it's not numerical. There exists some order of categories,
# but category e (i don't have opinion) cannot be ordered.
# Possible categories  are a, b, c, d, e.
  

# Import the data from the file survey.txt into the variable survey.

survey <- read.table("~/uam/stats/survey.txt", header = TRUE)

# Present the empirical distribution of the result variable in the form of a table.

SurveyEmpiricalDistribution <- function(survey) {
  results <- survey$result
  data.frame(cbind(results = table(results),
                   frequency = prop.table(table(results))))
}

survey_empirical_distribution <- SurveyEmpiricalDistribution(survey)

survey_empirical_distribution %>%
  print()

# Present the empirical distribution of result variable
# only for persons with primary education in the form of a table.

FilterByPrimaryEducation <- function(survey) {
  education <- survey$education
  survey[survey$education == 'p',]
}

survey_empirical_distribution_only_primary <- survey %>%
  FilterByPrimaryEducation() %>%
  SurveyEmpiricalDistribution()

print(survey_empirical_distribution_only_primary)

# Illustrate the survey results using a bar chart and pie chart.

GetRandomColorsVector <- function(color_count) {
  sample(colours(), color_count)
}

BarChartForEmpiricalDistribution <- function(empirical_distribution, y_label, data_modifier = 1) {
  barplot(empirical_distribution$results / data_modifier,
          names.arg = row.names(empirical_distribution),
          xlab = "Answer",
          ylab = y_label,
          main = "Empirical distribution of answers",
          col = GetRandomColorsVector(length(empirical_distribution$results)))
}

PieChartForEmpiricalDistribution <- function(empirical_distribution) {
  slices <- empirical_distribution$results
  labels <- row.names(survey_empirical_distribution)
  
  pie(slices, labels, col = GetRandomColorsVector(length(empirical_distribution$results)))
}

survey_empirical_distribution %>%
  BarChartForEmpiricalDistribution(y_label = "Frequency")

survey_empirical_distribution %>%
  BarChartForEmpiricalDistribution(y_label = "Probability",
                                   data_modifier = nrow(survey))

survey_empirical_distribution %>%
  PieChartForEmpiricalDistribution()

# Illustrate the results of the survey using a bar chart, broken down by women and men.

FilterByGender <- function(survey, gender) {
  survey[survey$sex == gender,]
}

BarChartBrokenByWomenAndMen <- function(men_distribution, women_distribution) {
  men_frequency <- men_distribution$results
  women_frequency <- women_distribution$results
  
  barplot(cbind(men_frequency, women_frequency),
          names.arg = c("m", "f"),
          xlab = "Answer",
          ylab = "Frequency",
          beside = TRUE,
          main = "Empirical distribution of answers broken by men and women",
          col = GetRandomColorsVector(length(men_frequency)))
}

men_empirical_distribution <- survey %>%
  FilterByGender('m') %>%
  SurveyEmpiricalDistribution()

women_empirical_distribution <- survey %>%
  FilterByGender('f') %>%
  SurveyEmpiricalDistribution()

print(men_empirical_distribution)
print(women_empirical_distribution)

BarChartBrokenByWomenAndMen(men_empirical_distribution, women_empirical_distribution)

# Interpret the above tabular and graphical results.

# The data sample consists of 100 answers.
# The most common answer in the sample among both genders was answer C (wrong), with 29 answers.
# Also, there is almost equal amount of negative and positive answers (as there were 21 'i have no opinion'),
# but more people think that activities of the city president are negative (46 vs 43).
# Looking on empirical distribution broken by men and women, we can conclude that
# survey's participants were mainly women (68 vs 32).
# Men seem to be more satisfied with president's activity, and also less often gave 'i have no opinion' answer.
# (needs to be checked, as men sample size is much smaller than women sample size)


# Task 2.1
###
# 200 randomly selected 5-second time periods of work of atelephone exchange
# were examined. The number of calls was recorded. The results are contained
# in the file telephone_exchange.RData.
# Which type is this variable? What are its possible values?

# This variable is a quantitative, discrete random variable.
# Set of values is {0, 1, 2, 3, 4, 5}

# Import the data from the file telephone_exchange.RData.

load('~/uam/stats/telephone_exchange.rdata')

# Present the empirical distribution of the number of calls in the form of a table.

NumberOfCallsEmpiricalDistribution <- function() {
  data.frame(cbind(number_of_calls = table(telephone_exchange$number),
                   frequency = prop.table(table(telephone_exchange$number))))
}

NumberOfCallsEmpiricalDistribution() %>%
  print()

# Illustrate the empirical distribution of the number of calls using a bar chart and pie chart.

BarChartForNumberOfCalls <- function(empirical_distribution, probability = FALSE) {
  if (probability == TRUE) {
    barplot(empirical_distribution$number_of_calls,
            names.arg = row.names(empirical_distribution),
            xlab = "Number of calls",
            ylab = "Probability",
            main = "Empirical distribution of number of calls",
            col = GetRandomColorsVector(length(empirical_distribution$frequency)))
  }
  else {
    barplot(empirical_distribution$frequency,
            names.arg = row.names(empirical_distribution),
            xlab = "Number of calls",
            ylab = "Probability",
            main = "Empirical distribution of number of calls",
            col = GetRandomColorsVector(length(empirical_distribution$frequency)))
  }
}

PieChartForNumberOfCalls <- function(empirical_distribution) {
  slices <- empirical_distribution$number_of_calls
  labels <- row.names(empirical_distribution)
  
  pie(slices, labels, col = GetRandomColorsVector(length(empirical_distribution$frequency)))
}

NumberOfCallsEmpiricalDistribution() %>%
  BarChartForNumberOfCalls()

NumberOfCallsEmpiricalDistribution() %>%
  BarChartForNumberOfCalls(probability = TRUE)

NumberOfCallsEmpiricalDistribution() %>%
  PieChartForNumberOfCalls

# Calculate the mean of the number of calls,
# the median of the number of calls,
# the standard deviation of the number of calls,
# and the coefficient of variation of the number of calls.

# Mean
mean <- mean(telephone_exchange$number) %>%
  print()

# Median
median(telephone_exchange$number) %>%
  print()

# Standard deviation
sd <-sd(telephone_exchange$number) %>%
  print()

cv <- function(sd, mean) {
  (sd / mean) * 100
}

# Coefficient of variation
cv(sd, mean) %>%
  print()


# Interpret the above tabular, graphical and numerical results.

# In the sample, most often there was only 1 call in time period of 5 seconds (33.5%),
# although it's not a majority.
# Median (2) and average (1.74) are quite similar, which suggests that a central tendency
# in the sample is 2 calls per 5 seconds.
# Standard deviation (1.28086) seems to be quite small at the first glance,
# but actually it's not, as our data values are in the set {0, 1, 2, 3, 4, 5}.
# It suggests that our data is dispersed.
# Also, the coefficient of variation (73%) has a big value, which also gives us
# a hint about data being widely spread-out.


