install.packages("magrittr", repos = "http://cran.us.r-project.org")
library(magrittr)


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

