### TASK 1 ###
# The table below presents the number of cases of respiratory tuberculosis in 1995-2002. 
# The number of cases was given per 100,000 population. Assuming a linear relationship between 
# the year and the number of cases, make a comprehensive regression analysis.
df <- data.frame(
                  year = c(1995:2002),
                  number_of_cases = c(39.7, 38.2, 34.7, 33.1, 30.1, 28.4, 26.3, 24.7)
                )

## TASK 1.1 ##
# Present the data on the scatter diagram. Does the linear regression model seem reasonable?
plot(df$year, df$number_of_cases,
      ylab = "Number of cases",
      xlab = "Year",
      las = 1
     )

## TASK 1.2 ##
# Fit the linear regression model to this data. What are the values of the regression coefficient 
# estimators and confidence intervals? Draw the obtained regression line on the scatter diagram.
# fit the model
number_of_cases <- df$number_of_cases
year <- df$year
model <- lm(number_of_cases ~ year)
# calculate regression coefficient estimators and confidence intervals
coef(model)
confint(model)
# draw regression line on the scatter plot
abline(model, col = "red", lwd = 2)

## TASK 1.3 ##
# Which coefficients are statistically significant in the constructed model? What is the model fit?
summary(model)

## TASK 1.4 ##
# Calculate fitted values by the model as well as residual values.
fitted(model) # fitted values
residuals(model) # residual values

## TASK 1.5 ## 
# Present on the scatter diagram the limits of the 95% prediction interval.
pred.int <- predict(model, df, interval = "prediction")
# lwr and upr: the lower and the upper confidence limits for the expected values.
# By default the function produces the 95% prediction limits.
lines(year, pred.int[, 2], lty = 2, col = "red") # lwr
lines(year, pred.int[, 3], lty = 2, col = "red") # upr

## TASK 1.6 ## 
# Predict the number of cases of respiratory tuberculosis for 2003-2007. Illustrate the results on 
# scatter diagram.
new_data <- data.frame(
                        year = c(2003:2007)
                      )
new_pred <- predict(model, new_data, interval = "prediction")
new_data <- cbind(new_data, number_of_cases = round(new_pred[, 1], 1))
plot(rbind(df, new_data),
       ylab = "Number of cases",
       xlab = "Year",
       las = 1
     )
lines(year, pred.int[, 2], lty = 2, col = "red") # lwr
lines(year, pred.int[, 3], lty = 2, col = "red") # upr
lines(new_data$year, new_pred[, 2], lty = 2, col = "red") # lwr
lines(new_data$year, new_pred[, 3], lty = 2, col = "red") # upr
abline(model, col = "red", lwd = 2)


## TASK 1.7 ## !!!
# Would it make sense to remove the intercept from the model? If so, follow the instructions above 
# for a no-intercept linear regression model.



### TASK 2 ### MODEL FOR FULL DATASET
# The data set in the file braking.RData contains information on the braking distance at a given speed 
# of a certain car model. There is an outlier in this data set. Identify it using a scatter diagram. 
# Using a linear regression model, describe the relationship between braking distance and speed using 
# full data and data without outlier. What are the results for both models? Which model is better? 
# More precisely, follow the same points 2-7 as in Task 1 for each model separately. In point 6, 
# predict braking distance for speed 30, 31, …, 50.
getwd() # check current directory
setwd("C:\\Users\\Weronika\\Desktop\\studia\\III year, I semester\\Elements of Statistics")
load("braking.RData")
# draw scatter plot
plot(braking, 
      main = "Scatter diagram", 
      ylab = "Distance", 
      xlab = "Speed",
      las = 1
    )
# identify outlier
# outlier - any point that doesn’t appear to belong with the vast majority of the other points.
outlier <- boxplot(braking)$out
outlier_indices <- which(braking$distance %in% outlier)

## TASK 2.2 ##
# Fit the linear regression model to this data. What are the values of the regression coefficient 
# estimators and confidence intervals? Draw the obtained regression line on the scatter diagram.
# fit the model
distance <- braking$distance
speed <- braking$speed
model <- lm(distance ~ speed)
# calculate regression coefficient estimators and confidence intervals
coef(model)
confint(model)
# draw regression line on the scatter plot
abline(model, col = "red", lwd = 2)

## TASK 2.3 ##
# Which coefficients are statistically significant in the constructed model? What is the model fit?
summary(model)

## TASK 2.4 ##
# Calculate fitted values by the model as well as residual values.
fitted(model) # fitted values
residuals(model) # residual values

## TASK 2.5 ##
# Present on the scatter diagram the limits of the 95% prediction interval.
pred.int <- predict(model, braking, interval = "prediction")
# lwr and upr: the lower and the upper confidence limits for the expected values.
# By default the function produces the 95% prediction limits.
lines(speed, pred.int[, 2], lty = 2, col = "red") # lwr
lines(speed, pred.int[, 3], lty = 2, col = "red") # upr

## TASK 2.6 ##
# Predict braking distance for speed 30, 31, …, 50. Illustrate the results on scatter diagram.
new_data <- data.frame(
                        speed = c(30:50)
                      )
new_pred <- predict(model, new_data, interval = "prediction")
new_data <- cbind(new_data, distance = round(new_pred[, 1], 1))
plot(rbind(braking, new_data),
     ylab = "Number of cases",
     xlab = "Year",
     las = 1
)
lines(speed, pred.int[, 2], lty = 2, col = "red") # lwr
lines(speed, pred.int[, 3], lty = 2, col = "red") # upr
lines(new_data$speed, new_pred[, 2], lty = 2, col = "red") # lwr
lines(new_data$speed, new_pred[, 3], lty = 2, col = "red") # upr
abline(model, col = "red", lwd = 2)

## TASK 2.7 ## !!!
# Would it make sense to remove the intercept from the model? If so, follow the instructions above 
# for a no-intercept linear regression model.



### TASK 2 ### WITHOUT OUTLIER
# identify outlier
# outlier - any point that doesn’t appear to belong with the vast majority of the other points.
outlier <- boxplot(braking)$out
outlier_indices <- which(braking$distance %in% outlier)
braking_1 <- braking[-c(outlier_indices), ]

## TASK 2.2 ##
# Fit the linear regression model to this data. What are the values of the regression coefficient 
# estimators and confidence intervals? Draw the obtained regression line on the scatter diagram.
# draw scatter plot
plot(braking_1, 
     main = "Scatter diagram", 
     ylab = "Distance", 
     xlab = "Speed",
     las = 1
)
# fit the model
distance <- braking_1$distance
speed <- braking_1$speed
model <- lm(distance ~ speed)
# calculate regression coefficient estimators and confidence intervals
coef(model)
confint(model)
# draw regression line on the scatter plot
abline(model, col = "green", lwd = 2)

## TASK 2.3 ##
# Which coefficients are statistically significant in the constructed model? What is the model fit?
summary(model)

## TASK 2.4 ##
# Calculate fitted values by the model as well as residual values.
fitted(model) # fitted values
residuals(model) # residual values

## TASK 2.5 ##
# Present on the scatter diagram the limits of the  95% prediction interval.
pred.int <- predict(model, braking_1, interval = "prediction")
# lwr and upr: the lower and the upper confidence limits for the expected values.
# By default the function produces the 95% prediction limits.
lines(speed, pred.int[, 2], lty = 2, col = "green") # lwr
lines(speed, pred.int[, 3], lty = 2, col = "green") # upr

## TASK 2.6 ##
# Predict braking distance for speed 30, 31, …, 50. Illustrate the results on scatter diagram.
new_data <- data.frame(
                        speed = c(30:50)
                      )
new_pred <- predict(model, new_data, interval = "prediction")
new_data <- cbind(new_data, distance = round(new_pred[, 1], 1))
plot(rbind(braking_1, new_data),
     ylab = "Number of cases",
     xlab = "Year",
     las = 1
)
lines(speed, pred.int[, 2], lty = 2, col = "green") # lwr
lines(speed, pred.int[, 3], lty = 2, col = "green") # upr
lines(new_data$speed, new_pred[, 2], lty = 2, col = "blue") # lwr
lines(new_data$speed, new_pred[, 3], lty = 2, col = "blue") # upr
abline(model, col = "green", lwd = 2)

## TASK 2.7 ## !!!
# Would it make sense to remove the intercept from the model? If so, follow the instructions above 
# for a no-intercept linear regression model.
