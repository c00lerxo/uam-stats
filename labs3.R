install.packages("EnvStats")
install.packages("magrittr", repos = "http://cran.us.r-project.org")

library(magrittr)
library(EnvStats)


# Task 3.1
###
# For the braking distance data in Example 2 of the lecture taking into account
# the theoretical distribution, calculate the values of the three estimators for the standard deviation.
# Illustrate the three theoretical density functions on the histogram.

brakes <- read.table('~/uam/stats/brakes.txt', dec = ',')

mvue_mu_est <- mean(brakes$V1)
mvue_sigma_est <- sd(brakes$V1)

# or

mvue_est <- enorm(brakes$V1, method = "mvue") %>%
  print()

mle_est <- enorm(brakes$V1, method = "mle") %>%
  print()

# Third estimator?? Not sure, what it should be.
# enorm() can estimate using only "mle" and "mvue" methods
# ls.home.amu.edu.pl website also suggests that third method for sd estimation doesn't exist
# (see example estimators for normal distribution)

HistogramForBrakesWithEstimatedDensities <- function(brakes, mvue_est, mle_est) {
  data <- brakes$V1
  hist(brakes$V1, probability = TRUE, main = "Histogram of brakes distance", xlab = "Brakes distance intervals")
  # Draw empirical distribution density
  lines(density(data), col = "red", lwd = 2)
  # Draw density estimated by MVUE
  mvue_mu_est <- mvue_est$parameters["mean"] 
  mvue_sigma_est <- mvue_est$parameters["sd"]
  curve(dnorm(x, mean = mvue_mu_est, sd = mvue_sigma_est), add = TRUE, col = "green", lwd = 2)
  # Draw density estimated by MLE
  mle_mu_est <- mle_est$parameters["mean"]
  mle_sigma_est <- mle_est$parameters["sd"]
  curve(dnorm(x, mean = mle_mu_est, sd = mle_sigma_est), add = TRUE, col = "blue", lwd = 2)
  # Add legend
  legend("topleft", legend = c("Empirical", "MVUE", "MLE"), col = c("red", "green", "blue"), lwd = 2)
}

brakes %>%
  HistogramForBrakesWithEstimatedDensities(mvue_est, mle_est)


# Task 3.2
###
# 200 randomly selected 5-second time periods of work of a telephone exchange were examined.
# The number of calls was recorded. The results are contained in the file telephone_exchange.RData.

load('~/uam/stats/telephone_exchange.RData')

# Suggest the theoretical distribution of the examined variable.

# Theoretical distribution of the examined variable is the Poisson distribution
# (our variable is number of events which happen in given time interval)

# Calculate the values of the estimator of the model parameter.

lambda <- epois(telephone_exchange$number)$parameters["lambda"] %>%
  print()

# Compare the empirical probabilities of occurrence of individual values of
# the number of calls in the sample with theoretical values obtained on the basis
# of the theoretical distribution.

CompareEmpiricalWithTheoreticalDistribution <- function(telephone_exchange, lambda) {
  data <- telephone_exchange$number
  # Beware the sorting!!!
  theoretical_probs <- unique(dpois(sort(data), lambda = lambda))
  empirical_probs <- prop.table(table(data))
  
  # For the sake of task requirements
  theoretical_probs %>%
    sum() %>%
    print()
  
  matrix(c(empirical_probs, theoretical_probs),
         nrow = 2, 
         byrow = TRUE,
         dimnames = list(c("empirical", "theoretical"), sort(unique(data))))
}

PlotBarplotComparison <- function(comparison) {
  empirical_probs <- comparison[1,]
  theoretical_probs <- comparison[2,]
  
  barplot(rbind(empirical_probs, theoretical_probs),
          beside = TRUE,
          col = c("red", "blue"),
          main = "Empirical and theoretical distribution of number of calls",
          ylab = "Probability")
  legend("topright", legend = c("Empirical", "Theoretical"), col = c("red", "blue"), lwd = 2 )
}


comparison <- telephone_exchange %>%
  CompareEmpiricalWithTheoreticalDistribution(lambda = lambda) %>%
  print()


PlotBarplotComparison(comparison)


# Check the goodness-of-fit of the theoretical distribution based on the Q-Q plot.

qqPlot(telephone_exchange$number,
       distribution = "pois",
       param.list = list(lambda = lambda),
       add.line = TRUE,
       main = "Q-Q plot for number of calls vs theoretical distribution",
       xlab = "Theoretical quantiles",
       ylab = "Empirical quantiles")

# Based on the above considerations, does the theoretical distribution seem to be appropriate?

# It seems to be appropriate, as most of the data lies roughly on the straight line. 
# But I have some doubts, as there are some 'anomalies' (although they have some symmetry)
# What's worth noting, there are 5 points where x = y (and also 5 quantiles)
# - Why is there 6th quantile on x axis? We have only 5 possibilities, data is discrete!
# ?? NOT SURE!


# Calculate the empirical and theoretical probability that the number of calls is smaller than 4.

# Empirical

length(telephone_exchange[telephone_exchange$number < 4,]) / length(telephone_exchange$number) %>%
  print()

# Theoretical

ppois(3, lambda) %>%
  print()

# Calculate the confidence intervals for the model parameter based on three methods.

# Exact

epois(telephone_exchange$number, ci = TRUE, ci.method = "exact")$interval$limits %>%
  print()

# Pearson-Hartley approximation

epois(telephone_exchange$number, ci = TRUE, ci.method = "pearson.hartley.approx")$interval$limits %>%
  print()

# Normal approximation

epois(telephone_exchange$number, ci = TRUE, ci.method = "normal.approx")$interval$limits %>%
  print()


# Task 3.3
###
# The variable in the file failures.txt describes the results of 50 measurements 
# of failure-free operation time of a given device (in hours).

failures <- read.table("~/uam/stats/failures.txt")

# Suggest the theoretical distribution of the examined variable.

# The theoretical distribution of the examined variable is exponential distribution,
# as we measure time periods between certain reoccuring events.

# Calculate the values of the MLE of the model parameter.

rate <- eexp(failures$V1)$parameters["rate"]

# Compare the empirical distribution of occurrence of individual values
# of the failure-free operation time in the sample with theoretical distribution.

Compare <- function(failures, rate) {
  data <- failures$V1
  breaks <- hist(failures$V1, plot = FALSE)$breaks
  theoretical_breaks <- hist(dexp(data), plot = FALSE)$breaks
  # Beware the sorting!!!
  theoretical_probs <- unique(dexp(sort(data), rate = rate))
  empirical_probs <- prop.table(table(cut(failures$V1, breaks = breaks)))
  
  matrix(c(empirical_probs, theoretical_probs),
         nrow = 2, 
         byrow = TRUE,
         dimnames = list(c("empirical", "theoretical"), sort(unique(data))))
}

failures %>%
  Compare(rate)

PlotComparison <- function(failures, rate) {
  hist(failures$V1, probability = TRUE, 
       col = "light green",
       main = "Histogram of failures free operation time",
       xlab = "Failure-free operation time intervals")
  lines(density(failures$V1), col = "red", lwd = 2)
  curve(dexp(x, rate = rate), col = "blue", lwd = 2, add = TRUE)
}

failures %>%
  PlotComparison(rate = rate)

# Check the goodness-of-fit of the theoretical distribution based on the Q-Q plot.

qqPlot(failures$V1,
       distribution = "exp",
       param.list = list(rate = rate),
       add.line = TRUE,
       main = "Q-Q plot for failure-free operation time vs theoretical distribution",
       xlab = "Theoretical quantiles",
       ylab = "Empirical quantiles")

# Based on the above considerations, does the theoretical distribution seem to be appropriate?

# Certainly yes - the distributions are almost identical, as most of quantiles lie on the straight line.

# Calculate the empirical and theoretical probability that the failure-free operation time
# is contained in the interval [1000, 1500].

# Empirical

length(failures[failures$V1 >= 1000 & failures$V1 <= 1500,]) / length(failures$V1) %>%
  print()

# Theoretical

pexp(1500, rate = rate) - pexp(1000, rate = rate)

# Calculate the confidence interval for the model parameter.

eexp(failures$V1, ci = TRUE)$interval$limits %>%
  print()

# Calculate the maximum likelihood estimator values and the confidence interval limits
# for the expected value and variance of the theoretical distribution.

# Expected value

1 / rate %>%
  print()

# Variance

1 / rate^2 %>%
  print()

# Interval limits
# Doesn't work lol, but it's  necessary to do something like this
simdata <- rexp(50, rate = rate)
matrixdata =  matrix(simdata, nrow = 1000, ncol = 4)
means.exp = apply(matrixdata,1,mean)
se = sd(means.exp)/sqrt(50)
lower = mean(means.exp) - 1.96 * se
upper = mean(means.exp) + 1.96 * se
c(lower, upper)
